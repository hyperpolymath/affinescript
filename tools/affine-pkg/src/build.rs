// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright 2024 AffineScript Contributors

//! Build System
//!
//! Orchestrates compilation of AffineScript packages.

use std::path::{Path, PathBuf};
use std::process::Command;

/// Build configuration
#[derive(Debug, Clone)]
pub struct BuildConfig {
    /// Release or debug mode
    pub release: bool,

    /// Target directory
    pub target_dir: PathBuf,

    /// Number of parallel jobs
    pub jobs: Option<usize>,

    /// Extra compiler flags
    pub flags: Vec<String>,

    /// Features to enable
    pub features: Vec<String>,

    /// Disable default features
    pub no_default_features: bool,
}

impl Default for BuildConfig {
    fn default() -> Self {
        BuildConfig {
            release: false,
            target_dir: PathBuf::from("target"),
            jobs: None,
            flags: Vec::new(),
            features: Vec::new(),
            no_default_features: false,
        }
    }
}

/// Build result
#[derive(Debug)]
pub struct BuildResult {
    /// Output artifacts
    pub artifacts: Vec<Artifact>,

    /// Compilation time in milliseconds
    pub duration_ms: u64,

    /// Compiler warnings
    pub warnings: Vec<String>,
}

/// Build artifact
#[derive(Debug)]
pub struct Artifact {
    /// Artifact kind
    pub kind: ArtifactKind,

    /// Output path
    pub path: PathBuf,

    /// Package name
    pub package: String,

    /// Size in bytes
    pub size: u64,
}

/// Artifact kind
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArtifactKind {
    /// WebAssembly binary
    Wasm,
    /// WebAssembly + JavaScript wrapper
    WasmJs,
    /// Library
    Library,
    /// Documentation
    Doc,
}

/// Build orchestrator
pub struct Builder {
    config: BuildConfig,
}

impl Builder {
    /// Create a new builder
    pub fn new(config: BuildConfig) -> Self {
        Builder { config }
    }

    /// Build a package
    pub fn build(&self, package_dir: impl AsRef<Path>) -> anyhow::Result<BuildResult> {
        let _package_dir = package_dir.as_ref();
        let start = std::time::Instant::now();

        // TODO: Phase 8 implementation
        // - [ ] Load manifest
        // - [ ] Resolve dependencies
        // - [ ] Build dependencies first
        // - [ ] Compile source files
        // - [ ] Link into output

        let duration_ms = start.elapsed().as_millis() as u64;

        Ok(BuildResult {
            artifacts: vec![],
            duration_ms,
            warnings: vec![],
        })
    }

    /// Check (type check without code generation)
    pub fn check(&self, package_dir: impl AsRef<Path>) -> anyhow::Result<Vec<String>> {
        let _package_dir = package_dir.as_ref();

        // TODO: Phase 8 implementation
        // - [ ] Load manifest
        // - [ ] Parse all source files
        // - [ ] Type check
        // - [ ] Borrow check
        // - [ ] Effect check
        // - [ ] Return diagnostics

        Ok(vec![])
    }

    /// Run tests
    pub fn test(
        &self,
        package_dir: impl AsRef<Path>,
        filter: Option<&str>,
    ) -> anyhow::Result<TestResult> {
        let _package_dir = package_dir.as_ref();
        let _filter = filter;

        // TODO: Phase 8 implementation
        // - [ ] Find test functions (annotated with #[test])
        // - [ ] Build test binary
        // - [ ] Run tests
        // - [ ] Collect results

        Ok(TestResult {
            passed: 0,
            failed: 0,
            skipped: 0,
            duration_ms: 0,
            failures: vec![],
        })
    }

    /// Generate documentation
    pub fn doc(&self, package_dir: impl AsRef<Path>) -> anyhow::Result<PathBuf> {
        let package_dir = package_dir.as_ref();
        let output_dir = self.config.target_dir.join("doc");

        // TODO: Phase 8 implementation
        // - [ ] Parse source files
        // - [ ] Extract doc comments
        // - [ ] Generate HTML

        let _ = package_dir;
        Ok(output_dir)
    }

    /// Clean build artifacts
    pub fn clean(&self, package_dir: impl AsRef<Path>) -> anyhow::Result<()> {
        let target_dir = package_dir.as_ref().join(&self.config.target_dir);
        if target_dir.exists() {
            std::fs::remove_dir_all(target_dir)?;
        }
        Ok(())
    }

    /// Run the AffineScript compiler
    fn run_compiler(&self, args: &[&str]) -> anyhow::Result<std::process::Output> {
        let output = Command::new("affinescript")
            .args(args)
            .output()?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            anyhow::bail!("Compiler failed: {}", stderr);
        }

        Ok(output)
    }
}

/// Test result
#[derive(Debug)]
pub struct TestResult {
    /// Number of passing tests
    pub passed: usize,

    /// Number of failing tests
    pub failed: usize,

    /// Number of skipped tests
    pub skipped: usize,

    /// Total duration in milliseconds
    pub duration_ms: u64,

    /// Details of failures
    pub failures: Vec<TestFailure>,
}

/// Test failure details
#[derive(Debug)]
pub struct TestFailure {
    /// Test name
    pub name: String,

    /// Failure message
    pub message: String,

    /// Source location
    pub location: Option<String>,
}

// TODO: Phase 8 implementation
// - [ ] Implement incremental compilation
// - [ ] Add dependency tracking
// - [ ] Add parallel compilation
// - [ ] Add build caching
// - [ ] Add build scripts support
// - [ ] Add custom compiler invocation
