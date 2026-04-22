// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright 2024 AffineScript Contributors

//! Workspace Support
//!
//! Manages monorepos with multiple packages.

use crate::manifest::Manifest;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// A workspace containing multiple packages
#[derive(Debug)]
pub struct Workspace {
    /// Root directory
    pub root: PathBuf,

    /// Root manifest
    pub manifest: Manifest,

    /// Member packages (path -> manifest)
    pub members: HashMap<PathBuf, Manifest>,
}

impl Workspace {
    /// Find and load a workspace
    pub fn find(start_dir: impl AsRef<Path>) -> anyhow::Result<Option<Self>> {
        let start = start_dir.as_ref().canonicalize()?;

        // Walk up looking for workspace root
        let mut current = start.as_path();
        loop {
            let manifest_path = current.join("affine.toml");
            if manifest_path.exists() {
                let manifest = Manifest::load(&manifest_path)?;
                if manifest.workspace.is_some() {
                    return Ok(Some(Self::load(current)?));
                }
            }

            match current.parent() {
                Some(parent) => current = parent,
                None => break,
            }
        }

        Ok(None)
    }

    /// Load a workspace from its root
    pub fn load(root: impl AsRef<Path>) -> anyhow::Result<Self> {
        let root = root.as_ref().to_path_buf();
        let manifest = Manifest::load(root.join("affine.toml"))?;

        let mut members = HashMap::new();

        if let Some(ws) = &manifest.workspace {
            for pattern in &ws.members {
                // Expand glob patterns
                for entry in glob::glob(&root.join(pattern).to_string_lossy())? {
                    let path = entry?;
                    if path.is_dir() {
                        let member_manifest_path = path.join("affine.toml");
                        if member_manifest_path.exists() {
                            let member_manifest = Manifest::load(&member_manifest_path)?;
                            members.insert(path, member_manifest);
                        }
                    }
                }
            }
        }

        Ok(Workspace {
            root,
            manifest,
            members,
        })
    }

    /// Get all packages in the workspace (including root if it's a package)
    pub fn packages(&self) -> impl Iterator<Item = (&PathBuf, &Manifest)> {
        let root_iter = if self.manifest.package.name.is_empty() {
            None
        } else {
            Some((&self.root, &self.manifest))
        };

        root_iter.into_iter().chain(self.members.iter())
    }

    /// Find a package by name
    pub fn find_package(&self, name: &str) -> Option<(&PathBuf, &Manifest)> {
        self.packages().find(|(_, m)| m.package.name == name)
    }

    /// Get dependency graph between workspace members
    pub fn dependency_graph(&self) -> HashMap<String, Vec<String>> {
        let mut graph = HashMap::new();

        for (_, manifest) in self.packages() {
            let mut deps = Vec::new();

            for dep_name in manifest.dependencies.keys() {
                // Check if this is a workspace member
                if self.find_package(dep_name).is_some() {
                    deps.push(dep_name.clone());
                }
            }

            graph.insert(manifest.package.name.clone(), deps);
        }

        graph
    }

    /// Topological sort of packages (dependencies first)
    pub fn sorted_packages(&self) -> anyhow::Result<Vec<String>> {
        let graph = self.dependency_graph();
        let mut result = Vec::new();
        let mut visited = std::collections::HashSet::new();
        let mut visiting = std::collections::HashSet::new();

        fn visit(
            name: &str,
            graph: &HashMap<String, Vec<String>>,
            visited: &mut std::collections::HashSet<String>,
            visiting: &mut std::collections::HashSet<String>,
            result: &mut Vec<String>,
        ) -> anyhow::Result<()> {
            if visited.contains(name) {
                return Ok(());
            }
            if visiting.contains(name) {
                anyhow::bail!("Cyclic dependency detected involving: {}", name);
            }

            visiting.insert(name.to_string());

            if let Some(deps) = graph.get(name) {
                for dep in deps {
                    visit(dep, graph, visited, visiting, result)?;
                }
            }

            visiting.remove(name);
            visited.insert(name.to_string());
            result.push(name.to_string());

            Ok(())
        }

        for name in graph.keys() {
            visit(name, &graph, &mut visited, &mut visiting, &mut result)?;
        }

        Ok(result)
    }
}

// TODO: Phase 8 implementation
// - [ ] Add workspace inheritance for dependencies
// - [ ] Add workspace-level features
// - [ ] Add parallel builds across workspace
// - [ ] Add affected package detection for CI
// - [ ] Add workspace-wide linting/formatting
