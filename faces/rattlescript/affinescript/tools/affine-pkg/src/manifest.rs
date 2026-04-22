// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright 2024 AffineScript Contributors

//! Package Manifest (affine.toml)
//!
//! Defines the package metadata and dependencies.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;

/// Package manifest
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Manifest {
    /// Package information
    pub package: Package,

    /// Dependencies
    #[serde(default)]
    pub dependencies: HashMap<String, Dependency>,

    /// Dev dependencies
    #[serde(default, rename = "dev-dependencies")]
    pub dev_dependencies: HashMap<String, Dependency>,

    /// Build dependencies
    #[serde(default, rename = "build-dependencies")]
    pub build_dependencies: HashMap<String, Dependency>,

    /// Feature flags
    #[serde(default)]
    pub features: HashMap<String, Vec<String>>,

    /// Binary targets
    #[serde(default, rename = "bin")]
    pub binaries: Vec<BinaryTarget>,

    /// Library target
    pub lib: Option<LibraryTarget>,

    /// Workspace configuration
    pub workspace: Option<Workspace>,
}

/// Package metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Package {
    /// Package name
    pub name: String,

    /// Package version
    pub version: String,

    /// AffineScript edition
    #[serde(default = "default_edition")]
    pub edition: String,

    /// Package authors
    #[serde(default)]
    pub authors: Vec<String>,

    /// Package description
    pub description: Option<String>,

    /// Documentation URL
    pub documentation: Option<String>,

    /// Homepage URL
    pub homepage: Option<String>,

    /// Repository URL
    pub repository: Option<String>,

    /// License identifier (SPDX)
    pub license: Option<String>,

    /// License file path
    #[serde(rename = "license-file")]
    pub license_file: Option<String>,

    /// Keywords for search
    #[serde(default)]
    pub keywords: Vec<String>,

    /// Categories for classification
    #[serde(default)]
    pub categories: Vec<String>,

    /// Build script path
    pub build: Option<String>,
}

fn default_edition() -> String {
    "2024".to_string()
}

/// Dependency specification
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Dependency {
    /// Simple version string
    Version(String),

    /// Detailed dependency specification
    Detailed(DependencyDetail),
}

/// Detailed dependency specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DependencyDetail {
    /// Version requirement
    pub version: Option<String>,

    /// Git repository URL
    pub git: Option<String>,

    /// Git branch
    pub branch: Option<String>,

    /// Git tag
    pub tag: Option<String>,

    /// Git commit
    pub rev: Option<String>,

    /// Local path
    pub path: Option<String>,

    /// Package name in registry (if different)
    pub package: Option<String>,

    /// Required features
    #[serde(default)]
    pub features: Vec<String>,

    /// Whether this is optional
    #[serde(default)]
    pub optional: bool,

    /// Default features
    #[serde(default = "default_true", rename = "default-features")]
    pub default_features: bool,

    /// Registry URL
    pub registry: Option<String>,
}

fn default_true() -> bool {
    true
}

/// Binary target
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BinaryTarget {
    /// Binary name
    pub name: String,

    /// Source file path
    pub path: Option<String>,

    /// Required features
    #[serde(default)]
    #[serde(rename = "required-features")]
    pub required_features: Vec<String>,
}

/// Library target
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LibraryTarget {
    /// Library name
    pub name: Option<String>,

    /// Source file path
    pub path: Option<String>,

    /// Crate types to generate
    #[serde(default, rename = "crate-type")]
    pub crate_type: Vec<String>,
}

/// Workspace configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Workspace {
    /// Member packages
    pub members: Vec<String>,

    /// Excluded paths
    #[serde(default)]
    pub exclude: Vec<String>,

    /// Shared dependencies
    pub dependencies: Option<HashMap<String, Dependency>>,
}

impl Manifest {
    /// Load manifest from file
    pub fn load(path: impl AsRef<Path>) -> anyhow::Result<Self> {
        let content = std::fs::read_to_string(path)?;
        let manifest: Manifest = toml::from_str(&content)?;
        Ok(manifest)
    }

    /// Save manifest to file
    pub fn save(&self, path: impl AsRef<Path>) -> anyhow::Result<()> {
        let content = toml::to_string_pretty(self)?;
        std::fs::write(path, content)?;
        Ok(())
    }

    /// Get all dependencies (including dev and build)
    pub fn all_dependencies(&self) -> impl Iterator<Item = (&String, &Dependency)> {
        self.dependencies
            .iter()
            .chain(self.dev_dependencies.iter())
            .chain(self.build_dependencies.iter())
    }
}

// TODO: Phase 8 implementation
// - [ ] Add manifest validation
// - [ ] Add version validation (semver)
// - [ ] Add license validation (SPDX)
// - [ ] Add workspace resolution
// - [ ] Add feature resolution
