// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// Codegen orchestrator for affinescriptiser — Coordinates the three-phase code generation
// pipeline: (1) parse source files to find resource sites, (2) generate AffineScript type
// wrappers enforcing affine/linear discipline, (3) produce WASM build config and entry points.

pub mod affine_gen;
pub mod parser;
pub mod wasm_gen;

use anyhow::{Context, Result};
use std::fs;
use std::path::Path;

use crate::manifest::Manifest;

/// Run the full code generation pipeline:
/// 1. Parse all declared source files to identify resource allocation/deallocation sites.
/// 2. Generate an AffineScript module with type wrappers for each declared resource.
/// 3. Generate WASM build configuration and entry point declarations.
/// 4. Write all generated artefacts to the output directory.
/// 5. Report any detected violations (leaks, double-frees, use-after-free, size exceeded).
pub fn generate_all(manifest: &Manifest, output_dir: &str) -> Result<()> {
    let out = Path::new(output_dir);
    fs::create_dir_all(out).context("Failed to create output directory")?;

    // Phase 1: Parse sources to find resource usage sites.
    let sites = parser::parse_sources(manifest);
    println!(
        "  [parse] Found {} resource sites across {} source files",
        sites.len(),
        manifest.sources.len()
    );

    // Phase 2: Generate AffineScript type wrapper module.
    let abi_resources: Vec<_> = manifest
        .resources
        .iter()
        .map(|r| r.to_abi_resource())
        .collect();
    let affine_module =
        affine_gen::generate_affine_module(&manifest.project.name, &abi_resources, &sites);

    let affine_path = out.join(format!("{}.afs", manifest.project.name));
    fs::write(&affine_path, &affine_module.source).with_context(|| {
        format!(
            "Failed to write AffineScript module to {}",
            affine_path.display()
        )
    })?;
    println!(
        "  [affine] Generated AffineScript module: {}",
        affine_path.display()
    );

    // Phase 3: Generate WASM build config and entry point.
    let wasm_output = wasm_gen::generate_wasm_output(&affine_module, &manifest.wasm);

    let config_path = out.join("wasm-build.toml");
    fs::write(&config_path, &wasm_output.build_config)
        .with_context(|| format!("Failed to write WASM config to {}", config_path.display()))?;
    println!(
        "  [wasm]   Generated build config: {}",
        config_path.display()
    );

    let entry_path = out.join("entry.afs");
    fs::write(&entry_path, &wasm_output.entry_point).with_context(|| {
        format!(
            "Failed to write WASM entry point to {}",
            entry_path.display()
        )
    })?;
    println!("  [wasm]   Generated entry point: {}", entry_path.display());

    // Report violations.
    let all_violations: Vec<_> = affine_module
        .violations
        .iter()
        .chain(wasm_output.violations.iter())
        .collect();

    if all_violations.is_empty() {
        println!("  [check]  No violations detected");
    } else {
        println!("  [check]  {} violation(s) detected:", all_violations.len());
        for v in &all_violations {
            println!("    [{}] {}", v.severity, v.violation);
            println!("           Suggestion: {}", v.suggestion);
        }
    }

    Ok(())
}

/// Build the generated artefacts. In Phase 1, this reports the build command that would
/// be invoked by the AffineScript compiler targeting WASM. Full build integration is
/// planned for Phase 2.
pub fn build(manifest: &Manifest, release: bool) -> Result<()> {
    let mode = if release { "release" } else { "debug" };
    println!(
        "Building affinescriptiser project '{}' in {} mode",
        manifest.project.name, mode
    );
    println!("  Target: {}", manifest.wasm.target);
    println!("  Optimize: {}", manifest.wasm.optimize);
    if let Some(limit) = manifest.wasm.size_limit_kb {
        println!("  Size limit: {}KB", limit);
    }
    println!("  (AffineScript compiler integration pending — Phase 2)");
    Ok(())
}

/// Run the generated WASM module. In Phase 1, this reports the run command that would
/// be invoked. Full runtime integration is planned for Phase 2.
pub fn run(manifest: &Manifest, args: &[String]) -> Result<()> {
    println!(
        "Running affinescriptiser project '{}' with {} args",
        manifest.project.name,
        args.len()
    );
    println!("  (WASM runtime integration pending — Phase 2)");
    Ok(())
}
