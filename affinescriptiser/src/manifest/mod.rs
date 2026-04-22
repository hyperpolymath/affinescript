// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// Manifest module for affinescriptiser — Parses and validates the affinescriptiser.toml manifest,
// which declares source files, tracked resources (with affinity discipline), and WASM compilation
// settings. The manifest drives the entire pipeline: source analysis, AffineScript codegen, and
// WASM output constraints.

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::path::Path;

use crate::abi::{AffineResource, AffinityKind, WasmConfig};

/// Top-level manifest structure, corresponding to the `affinescriptiser.toml` file.
/// Contains the project identity, source file declarations, resource tracking rules,
/// and WASM compilation configuration.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Manifest {
    /// Project-level metadata (name, version).
    pub project: ProjectConfig,
    /// Source files to analyse for resource usage patterns.
    #[serde(default, rename = "sources")]
    pub sources: Vec<SourceEntry>,
    /// Resources to track with affine or linear type discipline.
    #[serde(default, rename = "resources")]
    pub resources: Vec<ResourceEntry>,
    /// WASM compilation configuration (target, optimisation, size limit).
    #[serde(default)]
    pub wasm: WasmConfig,
}

/// Project identity — the name that appears in generated AffineScript modules and WASM output.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectConfig {
    /// Project name, used to namespace generated AffineScript modules.
    pub name: String,
}

/// A source file entry declaring a file to analyse for resource allocation/deallocation patterns.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SourceEntry {
    /// Logical name for this source unit (used in generated wrapper module names).
    pub name: String,
    /// Relative path to the source file (e.g. "src/core.rs").
    pub path: String,
    /// Source language: "rust", "c", or "zig".
    pub language: String,
}

/// A resource entry declaring an allocator/deallocator pair to be tracked by the type system.
/// Each resource gets an AffineScript wrapper type enforcing its affinity discipline.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceEntry {
    /// Resource type name (e.g. "GpuBuffer", "SessionToken").
    pub name: String,
    /// Function name that creates/allocates this resource.
    pub allocator: String,
    /// Function name that destroys/deallocates this resource.
    pub deallocator: String,
    /// Affinity discipline: "affine" (at-most-once) or "linear" (exactly-once).
    pub affinity: AffinityKind,
}

impl ResourceEntry {
    /// Convert this manifest entry into the ABI-level AffineResource representation
    /// used by the codegen pipeline.
    pub fn to_abi_resource(&self) -> AffineResource {
        AffineResource {
            name: self.name.clone(),
            allocator: self.allocator.clone(),
            deallocator: self.deallocator.clone(),
            affinity: self.affinity,
        }
    }
}

/// Supported source languages for analysis. The parser dispatches on this
/// to select the appropriate allocation-pattern recognition strategy.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SourceLanguage {
    Rust,
    C,
    Zig,
}

impl SourceLanguage {
    /// Parse a language string from the manifest into the enum.
    /// Returns None for unrecognised language identifiers.
    #[allow(clippy::should_implement_trait)]
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "rust" | "rs" => Some(SourceLanguage::Rust),
            "c" => Some(SourceLanguage::C),
            "zig" => Some(SourceLanguage::Zig),
            _ => None,
        }
    }
}

/// Load and deserialise an affinescriptiser.toml manifest from the given file path.
/// Returns an error if the file cannot be read or the TOML structure is invalid.
pub fn load_manifest(path: &str) -> Result<Manifest> {
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Failed to read manifest: {}", path))?;
    toml::from_str(&content).with_context(|| format!("Failed to parse manifest: {}", path))
}

/// Validate a parsed manifest for structural correctness:
/// - Project name must be non-empty.
/// - At least one source file must be declared.
/// - At least one resource must be declared.
/// - All source languages must be recognised (rust, c, zig).
/// - All resource names, allocators, and deallocators must be non-empty.
/// - Resource names must be unique.
/// - WASM size limit, if set, must be positive.
pub fn validate(manifest: &Manifest) -> Result<()> {
    if manifest.project.name.trim().is_empty() {
        anyhow::bail!("project.name must not be empty");
    }
    if manifest.sources.is_empty() {
        anyhow::bail!("At least one [[sources]] entry is required");
    }
    if manifest.resources.is_empty() {
        anyhow::bail!("At least one [[resources]] entry is required");
    }

    // Validate each source entry.
    for (i, src) in manifest.sources.iter().enumerate() {
        if src.name.trim().is_empty() {
            anyhow::bail!("sources[{}].name must not be empty", i);
        }
        if src.path.trim().is_empty() {
            anyhow::bail!("sources[{}].path must not be empty", i);
        }
        if SourceLanguage::from_str(&src.language).is_none() {
            anyhow::bail!(
                "sources[{}].language '{}' is not supported (use: rust, c, zig)",
                i,
                src.language
            );
        }
    }

    // Validate each resource entry and check for duplicates.
    let mut seen_names = std::collections::HashSet::new();
    for (i, res) in manifest.resources.iter().enumerate() {
        if res.name.trim().is_empty() {
            anyhow::bail!("resources[{}].name must not be empty", i);
        }
        if res.allocator.trim().is_empty() {
            anyhow::bail!("resources[{}].allocator must not be empty", i);
        }
        if res.deallocator.trim().is_empty() {
            anyhow::bail!("resources[{}].deallocator must not be empty", i);
        }
        if !seen_names.insert(&res.name) {
            anyhow::bail!("Duplicate resource name: '{}'", res.name);
        }
    }

    // Validate WASM size limit.
    if let Some(limit) = manifest.wasm.size_limit_kb
        && limit == 0
    {
        anyhow::bail!("wasm.size-limit-kb must be greater than 0");
    }

    Ok(())
}

/// Write a template affinescriptiser.toml manifest to the given directory. Fails if the
/// manifest file already exists to prevent accidental overwrites.
pub fn init_manifest(path: &str) -> Result<()> {
    let p = Path::new(path).join("affinescriptiser.toml");
    if p.exists() {
        anyhow::bail!("affinescriptiser.toml already exists at {}", p.display());
    }
    let template = r#"# affinescriptiser manifest — Wrap code in affine + dependent types targeting WASM
# SPDX-License-Identifier: PMPL-1.0-or-later

[project]
name = "wasm-safe"

[[sources]]
name = "core-lib"
path = "src/core.rs"
language = "rust"

[[resources]]
name = "GpuBuffer"
allocator = "create_buffer"
deallocator = "release_buffer"
affinity = "affine"              # affine (at-most-once) | linear (exactly-once)

[[resources]]
name = "SessionToken"
allocator = "create_session"
deallocator = "revoke_session"
affinity = "linear"

[wasm]
target = "wasm32-unknown-unknown"
optimize = true
size-limit-kb = 512              # max WASM binary size
"#;
    std::fs::write(&p, template)?;
    println!("Created {}", p.display());
    Ok(())
}

/// Print a human-readable summary of the manifest contents to stdout.
pub fn print_info(m: &Manifest) {
    println!("=== {} ===", m.project.name);
    println!("Sources: {}", m.sources.len());
    for src in &m.sources {
        println!("  - {} ({}) @ {}", src.name, src.language, src.path);
    }
    println!("Resources: {}", m.resources.len());
    for res in &m.resources {
        println!(
            "  - {} [{}] alloc={} dealloc={}",
            res.name, res.affinity, res.allocator, res.deallocator
        );
    }
    println!("WASM target: {}", m.wasm.target);
    println!("Optimize: {}", m.wasm.optimize);
    if let Some(limit) = m.wasm.size_limit_kb {
        println!("Size limit: {}KB", limit);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper to create a valid manifest for testing.
    fn sample_manifest() -> Manifest {
        Manifest {
            project: ProjectConfig {
                name: "test-project".into(),
            },
            sources: vec![SourceEntry {
                name: "core".into(),
                path: "src/core.rs".into(),
                language: "rust".into(),
            }],
            resources: vec![ResourceEntry {
                name: "GpuBuffer".into(),
                allocator: "create_buffer".into(),
                deallocator: "release_buffer".into(),
                affinity: AffinityKind::Affine,
            }],
            wasm: WasmConfig::default(),
        }
    }

    #[test]
    fn test_valid_manifest_passes_validation() {
        let m = sample_manifest();
        assert!(validate(&m).is_ok());
    }

    #[test]
    fn test_empty_project_name_fails() {
        let mut m = sample_manifest();
        m.project.name = "".into();
        assert!(validate(&m).is_err());
    }

    #[test]
    fn test_no_sources_fails() {
        let mut m = sample_manifest();
        m.sources.clear();
        assert!(validate(&m).is_err());
    }

    #[test]
    fn test_no_resources_fails() {
        let mut m = sample_manifest();
        m.resources.clear();
        assert!(validate(&m).is_err());
    }

    #[test]
    fn test_unsupported_language_fails() {
        let mut m = sample_manifest();
        m.sources[0].language = "python".into();
        assert!(validate(&m).is_err());
    }

    #[test]
    fn test_duplicate_resource_name_fails() {
        let mut m = sample_manifest();
        m.resources.push(ResourceEntry {
            name: "GpuBuffer".into(),
            allocator: "other_alloc".into(),
            deallocator: "other_dealloc".into(),
            affinity: AffinityKind::Linear,
        });
        assert!(validate(&m).is_err());
    }

    #[test]
    fn test_zero_size_limit_fails() {
        let mut m = sample_manifest();
        m.wasm.size_limit_kb = Some(0);
        assert!(validate(&m).is_err());
    }

    #[test]
    fn test_resource_to_abi_conversion() {
        let entry = ResourceEntry {
            name: "Token".into(),
            allocator: "mk".into(),
            deallocator: "rm".into(),
            affinity: AffinityKind::Linear,
        };
        let abi = entry.to_abi_resource();
        assert_eq!(abi.name, "Token");
        assert_eq!(abi.affinity, AffinityKind::Linear);
    }

    #[test]
    fn test_toml_roundtrip() {
        let toml_str = r#"
[project]
name = "roundtrip"

[[sources]]
name = "main"
path = "src/main.rs"
language = "rust"

[[resources]]
name = "Handle"
allocator = "open"
deallocator = "close"
affinity = "affine"

[wasm]
target = "wasm32-unknown-unknown"
optimize = true
size-limit-kb = 256
"#;
        let m: Manifest = toml::from_str(toml_str).expect("Should parse");
        assert_eq!(m.project.name, "roundtrip");
        assert_eq!(m.resources[0].affinity, AffinityKind::Affine);
        assert_eq!(m.wasm.size_limit_kb, Some(256));
    }

    #[test]
    fn test_source_language_parsing() {
        assert_eq!(SourceLanguage::from_str("rust"), Some(SourceLanguage::Rust));
        assert_eq!(SourceLanguage::from_str("rs"), Some(SourceLanguage::Rust));
        assert_eq!(SourceLanguage::from_str("c"), Some(SourceLanguage::C));
        assert_eq!(SourceLanguage::from_str("zig"), Some(SourceLanguage::Zig));
        assert_eq!(SourceLanguage::from_str("python"), None);
    }
}
