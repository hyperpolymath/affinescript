// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// Integration tests for affinescriptiser — Validates the full pipeline from manifest parsing
// through source analysis, AffineScript codegen, and WASM output generation.

use affinescriptiser::abi::{AffineResource, AffinityKind, WasmConfig, WasmTarget};
use affinescriptiser::codegen::affine_gen::generate_affine_module;
use affinescriptiser::codegen::parser::parse_source_string;
use affinescriptiser::codegen::wasm_gen::{check_size_limit, generate_wasm_output};
use affinescriptiser::manifest::{Manifest, ProjectConfig, ResourceEntry, SourceEntry, validate};

/// Helper: construct a valid manifest programmatically.
fn make_manifest(
    name: &str,
    sources: Vec<SourceEntry>,
    resources: Vec<ResourceEntry>,
    wasm: WasmConfig,
) -> Manifest {
    Manifest {
        project: ProjectConfig {
            name: name.to_string(),
        },
        sources,
        resources,
        wasm,
    }
}

// ---------------------------------------------------------------------------
// Test 1: Full pipeline — manifest -> parse -> affine gen -> wasm gen
// ---------------------------------------------------------------------------
#[test]
fn test_full_pipeline_generates_valid_output() {
    let resources = vec![
        ResourceEntry {
            name: "GpuBuffer".into(),
            allocator: "create_buffer".into(),
            deallocator: "release_buffer".into(),
            affinity: AffinityKind::Affine,
        },
        ResourceEntry {
            name: "SessionToken".into(),
            allocator: "create_session".into(),
            deallocator: "revoke_session".into(),
            affinity: AffinityKind::Linear,
        },
    ];

    let source_code = r#"
fn main() {
    let buf = create_buffer(1024);
    release_buffer(buf);
    let tok = create_session("alice");
    revoke_session(tok);
}
"#;

    // Parse source code against declared resources.
    let sites = parse_source_string(source_code, "test.rs", &resources);
    assert_eq!(sites.len(), 4, "Should find 2 allocs + 2 deallocs");

    // Generate AffineScript module.
    let abi_resources: Vec<_> = resources.iter().map(|r| r.to_abi_resource()).collect();
    let module = generate_affine_module("wasm_safe", &abi_resources, &sites);

    // No violations — all resources properly allocated and deallocated.
    assert!(
        module.violations.is_empty(),
        "Clean code should produce no violations, got: {:?}",
        module.violations
    );

    // Module source should contain wrappers for both resources.
    assert!(module.source.contains("type GpuBuffer<Affine>"));
    assert!(module.source.contains("type SessionToken<Linear>"));
    assert!(module.source.contains("safe_create_gpu_buffer"));
    assert!(module.source.contains("safe_destroy_session_token"));
    assert!(module.source.contains("#[must_consume"));

    // Generate WASM output.
    let wasm_config = WasmConfig {
        target: WasmTarget::Wasm32Unknown,
        optimize: true,
        size_limit_kb: Some(512),
    };
    let wasm_output = generate_wasm_output(&module, &wasm_config);

    // Build config should reference the correct target.
    assert!(wasm_output.build_config.contains("wasm32-unknown-unknown"));
    assert!(wasm_output.build_config.contains("optimize = true"));

    // Entry point should export safe wrappers.
    assert!(wasm_output.entry_point.contains("@wasm_export"));
    assert!(wasm_output.entry_point.contains("affinescriptiser_health"));
}

// ---------------------------------------------------------------------------
// Test 2: Double-free detection across the pipeline
// ---------------------------------------------------------------------------
#[test]
fn test_double_free_detected_in_pipeline() {
    let resources = vec![ResourceEntry {
        name: "FileHandle".into(),
        allocator: "open_file".into(),
        deallocator: "close_file".into(),
        affinity: AffinityKind::Affine,
    }];

    let buggy_code = r#"
let f = open_file("data.txt");
close_file(f);
close_file(f);  // BUG: double free
"#;

    let sites = parse_source_string(buggy_code, "buggy.rs", &resources);
    assert_eq!(sites.len(), 3, "1 alloc + 2 deallocs");

    let abi_resources: Vec<_> = resources.iter().map(|r| r.to_abi_resource()).collect();
    let module = generate_affine_module("test", &abi_resources, &sites);

    assert_eq!(
        module.violations.len(),
        1,
        "Should detect exactly one double-free"
    );
    assert!(
        module.violations[0]
            .violation
            .to_string()
            .contains("DOUBLE-FREE"),
        "Violation should be a double-free"
    );
    assert_eq!(module.violations[0].severity, "error");
}

// ---------------------------------------------------------------------------
// Test 3: Linear resource leak detection
// ---------------------------------------------------------------------------
#[test]
fn test_linear_leak_detected_in_pipeline() {
    let resources = vec![ResourceEntry {
        name: "DbConnection".into(),
        allocator: "open_db".into(),
        deallocator: "close_db".into(),
        affinity: AffinityKind::Linear,
    }];

    let leaky_code = r#"
let conn = open_db("postgres://localhost");
// Oops — forgot to deallocate the connection
"#;

    let sites = parse_source_string(leaky_code, "leak.rs", &resources);
    assert_eq!(sites.len(), 1, "1 alloc, 0 deallocs");

    let abi_resources: Vec<_> = resources.iter().map(|r| r.to_abi_resource()).collect();
    let module = generate_affine_module("test", &abi_resources, &sites);

    assert_eq!(module.violations.len(), 1, "Should detect exactly one leak");
    assert!(
        module.violations[0].violation.to_string().contains("LEAK"),
        "Violation should be a leak"
    );
}

// ---------------------------------------------------------------------------
// Test 4: Affine resources do NOT report leaks
// ---------------------------------------------------------------------------
#[test]
fn test_affine_resource_leak_is_not_violation() {
    let resources = vec![ResourceEntry {
        name: "TempBuffer".into(),
        allocator: "alloc_temp".into(),
        deallocator: "free_temp".into(),
        affinity: AffinityKind::Affine,
    }];

    let code = r#"
let tmp = alloc_temp(256);
// Not freed — but this is affine, so dropping is OK.
"#;

    let sites = parse_source_string(code, "ok.rs", &resources);
    let abi_resources: Vec<_> = resources.iter().map(|r| r.to_abi_resource()).collect();
    let module = generate_affine_module("test", &abi_resources, &sites);

    assert!(
        module.violations.is_empty(),
        "Affine resource leak should NOT be a violation"
    );
}

// ---------------------------------------------------------------------------
// Test 5: WASM size limit enforcement
// ---------------------------------------------------------------------------
#[test]
fn test_wasm_size_limit_enforcement() {
    let config = WasmConfig {
        target: WasmTarget::Wasm32Unknown,
        optimize: true,
        size_limit_kb: Some(256),
    };

    // Within limit.
    assert!(
        check_size_limit(200 * 1024, &config).is_none(),
        "200KB should be within 256KB limit"
    );

    // Exceeds limit.
    let violation = check_size_limit(300 * 1024, &config);
    assert!(violation.is_some(), "300KB should exceed 256KB limit");
    let v = violation.unwrap();
    assert_eq!(v.severity, "warning");
    assert!(v.violation.to_string().contains("SIZE-EXCEEDED"));
}

// ---------------------------------------------------------------------------
// Test 6: Manifest TOML parsing and validation round-trip
// ---------------------------------------------------------------------------
#[test]
fn test_manifest_toml_parsing_and_validation() {
    let toml_content = r#"
[project]
name = "integration-test"

[[sources]]
name = "main"
path = "src/main.rs"
language = "rust"

[[sources]]
name = "utils"
path = "src/utils.c"
language = "c"

[[resources]]
name = "Mutex"
allocator = "lock_mutex"
deallocator = "unlock_mutex"
affinity = "linear"

[[resources]]
name = "Cache"
allocator = "create_cache"
deallocator = "destroy_cache"
affinity = "affine"

[wasm]
target = "wasm32-wasi"
optimize = false
"#;

    let manifest: Manifest = toml::from_str(toml_content).expect("Should parse valid TOML");

    // Validate structure.
    assert!(validate(&manifest).is_ok());
    assert_eq!(manifest.project.name, "integration-test");
    assert_eq!(manifest.sources.len(), 2);
    assert_eq!(manifest.resources.len(), 2);
    assert_eq!(manifest.resources[0].affinity, AffinityKind::Linear);
    assert_eq!(manifest.resources[1].affinity, AffinityKind::Affine);
    assert_eq!(manifest.wasm.target, WasmTarget::Wasm32Wasi);
    assert!(!manifest.wasm.optimize);
    assert!(manifest.wasm.size_limit_kb.is_none());
}

// ---------------------------------------------------------------------------
// Test 7: Manifest validation rejects invalid input
// ---------------------------------------------------------------------------
#[test]
fn test_manifest_validation_rejects_invalid() {
    // Empty project name.
    let m = make_manifest(
        "",
        vec![SourceEntry {
            name: "x".into(),
            path: "x.rs".into(),
            language: "rust".into(),
        }],
        vec![ResourceEntry {
            name: "R".into(),
            allocator: "a".into(),
            deallocator: "d".into(),
            affinity: AffinityKind::Affine,
        }],
        WasmConfig::default(),
    );
    assert!(validate(&m).is_err(), "Empty project name should fail");

    // No sources.
    let m = make_manifest(
        "ok",
        vec![],
        vec![ResourceEntry {
            name: "R".into(),
            allocator: "a".into(),
            deallocator: "d".into(),
            affinity: AffinityKind::Affine,
        }],
        WasmConfig::default(),
    );
    assert!(validate(&m).is_err(), "No sources should fail");

    // Unsupported language.
    let m = make_manifest(
        "ok",
        vec![SourceEntry {
            name: "x".into(),
            path: "x.py".into(),
            language: "python".into(),
        }],
        vec![ResourceEntry {
            name: "R".into(),
            allocator: "a".into(),
            deallocator: "d".into(),
            affinity: AffinityKind::Affine,
        }],
        WasmConfig::default(),
    );
    assert!(validate(&m).is_err(), "Unsupported language should fail");
}

// ---------------------------------------------------------------------------
// Test 8: Generate and write to temp directory
// ---------------------------------------------------------------------------
#[test]
fn test_generate_writes_files_to_output_dir() {
    let dir = tempfile::tempdir().expect("Should create temp dir");
    let output_dir = dir.path().join("out");

    let resources = vec![AffineResource {
        name: "Buffer".into(),
        allocator: "alloc".into(),
        deallocator: "free".into(),
        affinity: AffinityKind::Affine,
    }];

    let module = generate_affine_module("test_project", &resources, &[]);
    let wasm_output = generate_wasm_output(&module, &WasmConfig::default());

    // Write files manually (simulating what generate_all does).
    std::fs::create_dir_all(&output_dir).unwrap();
    std::fs::write(output_dir.join("test_project.afs"), &module.source).unwrap();
    std::fs::write(
        output_dir.join("wasm-build.toml"),
        &wasm_output.build_config,
    )
    .unwrap();
    std::fs::write(output_dir.join("entry.afs"), &wasm_output.entry_point).unwrap();

    // Verify files exist and have content.
    assert!(output_dir.join("test_project.afs").exists());
    assert!(output_dir.join("wasm-build.toml").exists());
    assert!(output_dir.join("entry.afs").exists());

    let afs_content = std::fs::read_to_string(output_dir.join("test_project.afs")).unwrap();
    assert!(afs_content.contains("module test_project"));
    assert!(afs_content.contains("type Buffer<Affine>"));
}

// ---------------------------------------------------------------------------
// Test 9: Multi-resource interaction — no false positives
// ---------------------------------------------------------------------------
#[test]
fn test_multi_resource_no_false_positives() {
    let resources = vec![
        ResourceEntry {
            name: "GpuBuffer".into(),
            allocator: "create_buffer".into(),
            deallocator: "release_buffer".into(),
            affinity: AffinityKind::Affine,
        },
        ResourceEntry {
            name: "SessionToken".into(),
            allocator: "create_session".into(),
            deallocator: "revoke_session".into(),
            affinity: AffinityKind::Linear,
        },
        ResourceEntry {
            name: "FileHandle".into(),
            allocator: "open_file".into(),
            deallocator: "close_file".into(),
            affinity: AffinityKind::Linear,
        },
    ];

    let clean_code = r#"
let buf = create_buffer(1024);
let tok = create_session("bob");
let fh = open_file("data.bin");
release_buffer(buf);
revoke_session(tok);
close_file(fh);
"#;

    let sites = parse_source_string(clean_code, "multi.rs", &resources);
    assert_eq!(sites.len(), 6, "3 allocs + 3 deallocs");

    let abi_resources: Vec<_> = resources.iter().map(|r| r.to_abi_resource()).collect();
    let module = generate_affine_module("multi", &abi_resources, &sites);

    assert!(
        module.violations.is_empty(),
        "Clean multi-resource code should produce no violations, got: {:?}",
        module.violations
    );
}
