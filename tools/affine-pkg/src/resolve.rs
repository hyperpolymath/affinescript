// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright 2024 AffineScript Contributors

//! Dependency Resolution
//!
//! Resolves version constraints to concrete versions.
//! Uses PubGrub algorithm for efficient conflict detection.

use crate::manifest::Dependency;
use semver::{Version, VersionReq};
use std::collections::{HashMap, HashSet};

/// A resolved dependency graph
#[derive(Debug, Clone)]
pub struct ResolvedGraph {
    /// Packages in topological order
    pub packages: Vec<ResolvedPackage>,

    /// Dependency edges
    pub edges: HashMap<PackageId, Vec<PackageId>>,
}

/// A resolved package
#[derive(Debug, Clone)]
pub struct ResolvedPackage {
    /// Package identifier
    pub id: PackageId,

    /// Features enabled
    pub features: HashSet<String>,

    /// Source URL/path
    pub source: String,
}

/// Package identifier (name + version)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackageId {
    /// Package name
    pub name: String,

    /// Exact version
    pub version: Version,
}

impl std::fmt::Display for PackageId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}@{}", self.name, self.version)
    }
}

/// Resolution error
#[derive(Debug, thiserror::Error)]
pub enum ResolveError {
    /// Version conflict
    #[error("version conflict for {package}: {required} conflicts with {available}")]
    Conflict {
        package: String,
        required: String,
        available: String,
    },

    /// Package not found
    #[error("package not found: {0}")]
    NotFound(String),

    /// Cyclic dependency
    #[error("cyclic dependency detected: {0}")]
    Cycle(String),

    /// Feature not found
    #[error("feature {feature} not found in package {package}")]
    FeatureNotFound { package: String, feature: String },
}

/// Resolver state
pub struct Resolver {
    /// Registry client
    // registry: RegistryClient,

    /// Cache of available versions
    version_cache: HashMap<String, Vec<Version>>,

    /// Cache of package metadata
    metadata_cache: HashMap<PackageId, PackageMetadata>,
}

/// Package metadata from registry
#[derive(Debug, Clone)]
pub struct PackageMetadata {
    /// Package name
    pub name: String,

    /// Version
    pub version: Version,

    /// Dependencies
    pub dependencies: HashMap<String, VersionReq>,

    /// Features
    pub features: HashMap<String, Vec<String>>,

    /// Default features
    pub default_features: Vec<String>,
}

impl Resolver {
    /// Create a new resolver
    pub fn new() -> Self {
        Resolver {
            version_cache: HashMap::new(),
            metadata_cache: HashMap::new(),
        }
    }

    /// Resolve dependencies
    pub fn resolve(
        &mut self,
        _root_deps: &HashMap<String, Dependency>,
    ) -> Result<ResolvedGraph, ResolveError> {
        // TODO: Phase 8 implementation using PubGrub algorithm
        // Reference: https://nex3.medium.com/pubgrub-2fb6470504f
        //
        // 1. Start with root package requirements
        // 2. For each unsatisfied requirement:
        //    a. Find compatible versions
        //    b. Pick best version (highest satisfying)
        //    c. Add package's dependencies to requirements
        // 3. If conflict detected:
        //    a. Analyze conflict
        //    b. Derive resolution (incompatibility)
        //    c. Backtrack and try alternative
        // 4. Continue until all satisfied or proven unsatisfiable

        Ok(ResolvedGraph {
            packages: Vec::new(),
            edges: HashMap::new(),
        })
    }

    /// Check if a version satisfies a requirement
    fn satisfies(&self, version: &Version, req: &VersionReq) -> bool {
        req.matches(version)
    }

    /// Get available versions for a package
    fn get_versions(&mut self, _name: &str) -> Result<&[Version], ResolveError> {
        // TODO: Phase 8 implementation
        // - [ ] Check cache
        // - [ ] Query registry
        // - [ ] Parse and cache versions

        Ok(&[])
    }

    /// Get package metadata
    fn get_metadata(&mut self, _id: &PackageId) -> Result<&PackageMetadata, ResolveError> {
        // TODO: Phase 8 implementation
        // - [ ] Check cache
        // - [ ] Query registry
        // - [ ] Parse and cache metadata

        Err(ResolveError::NotFound("not implemented".into()))
    }
}

impl Default for Resolver {
    fn default() -> Self {
        Self::new()
    }
}

/// Parse a version requirement string
pub fn parse_requirement(s: &str) -> Result<VersionReq, semver::Error> {
    // Handle common shortcuts
    let normalized = match s {
        // Exact version
        s if !s.contains(['>', '<', '=', '^', '~', '*']) => format!("^{}", s),
        // Already has operator
        s => s.to_string(),
    };

    VersionReq::parse(&normalized)
}

// TODO: Phase 8 implementation
// - [ ] Implement full PubGrub algorithm
// - [ ] Add version preference (prefer newer, prefer locked)
// - [ ] Add feature unification
// - [ ] Add optional dependency handling
// - [ ] Add workspace dependency resolution
// - [ ] Add parallel version fetching
