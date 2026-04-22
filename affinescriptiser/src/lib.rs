#![forbid(unsafe_code)]
#![allow(
    dead_code,
    clippy::too_many_arguments,
    clippy::manual_strip,
    clippy::if_same_then_else,
    clippy::vec_init_then_push
)]
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// affinescriptiser library — Public API for embedding affinescriptiser's analysis and
// codegen pipeline into other tools. Re-exports the core modules (ABI types, manifest
// parsing, and code generation) for programmatic use.

pub mod abi;
pub mod codegen;
pub mod manifest;

pub use manifest::{Manifest, load_manifest, validate};

/// Run the full affinescriptiser pipeline: load manifest, validate, and generate all artefacts.
/// This is the primary entry point for library consumers.
///
/// # Arguments
/// * `manifest_path` - Path to the affinescriptiser.toml manifest file.
/// * `output_dir` - Directory where generated AffineScript and WASM artefacts will be written.
///
/// # Errors
/// Returns an error if the manifest cannot be read, fails validation, or if code generation
/// encounters a filesystem error.
pub fn generate(manifest_path: &str, output_dir: &str) -> anyhow::Result<()> {
    let m = load_manifest(manifest_path)?;
    validate(&m)?;
    codegen::generate_all(&m, output_dir)
}
