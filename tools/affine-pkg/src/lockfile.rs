// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright 2024 AffineScript Contributors

//! Lock File (affine.lock)
//!
//! Records the exact versions of all dependencies for reproducibility.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;

/// Lock file format
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Lockfile {
    /// Lock file version
    pub version: u32,

    /// Locked packages
    pub packages: Vec<LockedPackage>,

    /// Metadata (checksums, etc.)
    #[serde(default)]
    pub metadata: HashMap<String, String>,
}

/// A locked package
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockedPackage {
    /// Package name
    pub name: String,

    /// Exact version
    pub version: String,

    /// Source (registry, git, path)
    pub source: PackageSource,

    /// Content hash (SHA-256)
    pub checksum: Option<String>,

    /// Dependencies of this package
    #[serde(default)]
    pub dependencies: Vec<LockedDependency>,
}

/// Package source
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum PackageSource {
    /// From registry
    #[serde(rename = "registry")]
    Registry {
        /// Registry URL
        url: String,
    },

    /// From git
    #[serde(rename = "git")]
    Git {
        /// Repository URL
        url: String,
        /// Commit hash
        commit: String,
    },

    /// Local path
    #[serde(rename = "path")]
    Path {
        /// Relative path
        path: String,
    },
}

/// A locked dependency reference
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockedDependency {
    /// Package name
    pub name: String,

    /// Version
    pub version: String,

    /// Source identifier
    pub source: Option<String>,
}

impl Lockfile {
    /// Current lock file version
    pub const VERSION: u32 = 1;

    /// Create empty lockfile
    pub fn new() -> Self {
        Lockfile {
            version: Self::VERSION,
            packages: Vec::new(),
            metadata: HashMap::new(),
        }
    }

    /// Load lockfile from path
    pub fn load(path: impl AsRef<Path>) -> anyhow::Result<Self> {
        let content = std::fs::read_to_string(path)?;
        let lockfile: Lockfile = toml::from_str(&content)?;
        Ok(lockfile)
    }

    /// Save lockfile to path
    pub fn save(&self, path: impl AsRef<Path>) -> anyhow::Result<()> {
        let content = toml::to_string_pretty(self)?;
        std::fs::write(path, content)?;
        Ok(())
    }

    /// Find a locked package by name and version
    pub fn find(&self, name: &str, version: &str) -> Option<&LockedPackage> {
        self.packages
            .iter()
            .find(|p| p.name == name && p.version == version)
    }

    /// Add or update a package
    pub fn insert(&mut self, package: LockedPackage) {
        // Remove existing entry if present
        self.packages
            .retain(|p| !(p.name == package.name && p.version == package.version));

        self.packages.push(package);
    }

    /// Sort packages for deterministic output
    pub fn sort(&mut self) {
        self.packages.sort_by(|a, b| {
            a.name.cmp(&b.name).then_with(|| a.version.cmp(&b.version))
        });
    }
}

impl Default for Lockfile {
    fn default() -> Self {
        Self::new()
    }
}

// TODO: Phase 8 implementation
// - [ ] Add checksum verification
// - [ ] Add lockfile merging (for conflicts)
// - [ ] Add lockfile diffing
// - [ ] Add source canonicalization
