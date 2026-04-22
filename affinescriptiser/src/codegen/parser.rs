// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// Source parser for affinescriptiser — Scans source files (Rust, C, Zig) to identify call sites
// where tracked resources are allocated or deallocated. Produces a list of ResourceSite records
// that the affine_gen module uses to generate AffineScript type wrappers.

use crate::manifest::{Manifest, ResourceEntry, SourceLanguage};

/// A detected resource usage site in the source code.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResourceSite {
    /// Name of the resource type (matches ResourceEntry.name from the manifest).
    pub resource_name: String,
    /// The function call detected at this site (allocator or deallocator name).
    pub function: String,
    /// Whether this site is an allocation or a deallocation.
    pub kind: SiteKind,
    /// Source file path where this call was found.
    pub file: String,
    /// Line number (1-based) within the source file.
    pub line: usize,
    /// The full line of source code at this location, trimmed.
    pub context: String,
}

/// Whether a resource site is an allocation (creation) or deallocation (destruction).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SiteKind {
    /// The resource is being created/allocated at this site.
    Allocation,
    /// The resource is being destroyed/deallocated at this site.
    Deallocation,
}

impl std::fmt::Display for SiteKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SiteKind::Allocation => write!(f, "alloc"),
            SiteKind::Deallocation => write!(f, "dealloc"),
        }
    }
}

/// Parse all source files declared in the manifest, scanning for allocation and deallocation
/// call sites matching the declared resources. Returns a list of all detected sites across
/// all source files.
///
/// This performs a line-by-line text scan (not a full AST parse) looking for function call
/// patterns. For each declared resource, we search for its allocator and deallocator function
/// names followed by an opening parenthesis. This heuristic works well for common C, Rust,
/// and Zig calling conventions.
pub fn parse_sources(manifest: &Manifest) -> Vec<ResourceSite> {
    let mut sites = Vec::new();

    for source in &manifest.sources {
        // Unknown language — skip (validation should have caught this).
        let Some(lang) = SourceLanguage::from_str(&source.language) else {
            continue;
        };

        // Read the source file contents; if unavailable, skip gracefully.
        let content = match std::fs::read_to_string(&source.path) {
            Ok(c) => c,
            Err(_) => continue,
        };

        for (line_idx, line) in content.lines().enumerate() {
            let line_number = line_idx + 1;
            let trimmed = line.trim();

            // Skip comments (basic heuristic per language).
            if is_comment(trimmed, lang) {
                continue;
            }

            // Check each declared resource's allocator and deallocator.
            for resource in &manifest.resources {
                if contains_call(trimmed, &resource.allocator) {
                    sites.push(ResourceSite {
                        resource_name: resource.name.clone(),
                        function: resource.allocator.clone(),
                        kind: SiteKind::Allocation,
                        file: source.path.clone(),
                        line: line_number,
                        context: trimmed.to_string(),
                    });
                }
                if contains_call(trimmed, &resource.deallocator) {
                    sites.push(ResourceSite {
                        resource_name: resource.name.clone(),
                        function: resource.deallocator.clone(),
                        kind: SiteKind::Deallocation,
                        file: source.path.clone(),
                        line: line_number,
                        context: trimmed.to_string(),
                    });
                }
            }
        }
    }

    sites
}

/// Parse resource sites from a string of source code directly (useful for testing without
/// needing to create files on disk). Scans the given content against the provided resource
/// entries using the same heuristic as parse_sources.
pub fn parse_source_string(
    content: &str,
    file_name: &str,
    resources: &[ResourceEntry],
) -> Vec<ResourceSite> {
    let mut sites = Vec::new();

    for (line_idx, line) in content.lines().enumerate() {
        let line_number = line_idx + 1;
        let trimmed = line.trim();

        for resource in resources {
            if contains_call(trimmed, &resource.allocator) {
                sites.push(ResourceSite {
                    resource_name: resource.name.clone(),
                    function: resource.allocator.clone(),
                    kind: SiteKind::Allocation,
                    file: file_name.to_string(),
                    line: line_number,
                    context: trimmed.to_string(),
                });
            }
            if contains_call(trimmed, &resource.deallocator) {
                sites.push(ResourceSite {
                    resource_name: resource.name.clone(),
                    function: resource.deallocator.clone(),
                    kind: SiteKind::Deallocation,
                    file: file_name.to_string(),
                    line: line_number,
                    context: trimmed.to_string(),
                });
            }
        }
    }

    sites
}

/// Check if a source line contains a function call matching the given function name.
/// Looks for the pattern `function_name(` which covers standard call syntax in
/// Rust, C, and Zig. Also matches method-call syntax like `.function_name(` and
/// namespace-qualified calls like `module::function_name(`.
fn contains_call(line: &str, function_name: &str) -> bool {
    // Find all occurrences of the function name in the line.
    let mut search_from = 0;
    while let Some(pos) = line[search_from..].find(function_name) {
        let abs_pos = search_from + pos;
        let after = abs_pos + function_name.len();

        // The character immediately after the function name should be '(' or whitespace
        // followed by '(' to count as a call.
        if after < line.len() {
            let rest = &line[after..];
            let rest_trimmed = rest.trim_start();
            if rest_trimmed.starts_with('(') {
                // Verify the character before the function name is not alphanumeric
                // (to avoid matching substrings like "xcreate_buffer").
                if abs_pos == 0 {
                    return true;
                }
                let before = line.as_bytes()[abs_pos - 1];
                if !before.is_ascii_alphanumeric() && before != b'_' {
                    return true;
                }
            }
        }

        search_from = abs_pos + 1;
    }
    false
}

/// Basic comment detection heuristic per language. Returns true if the line is a
/// single-line comment.
fn is_comment(line: &str, lang: SourceLanguage) -> bool {
    match lang {
        SourceLanguage::Rust => line.starts_with("//"),
        SourceLanguage::C => line.starts_with("//") || line.starts_with("/*"),
        SourceLanguage::Zig => line.starts_with("//"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::abi::AffinityKind;

    fn gpu_resource() -> ResourceEntry {
        ResourceEntry {
            name: "GpuBuffer".into(),
            allocator: "create_buffer".into(),
            deallocator: "release_buffer".into(),
            affinity: AffinityKind::Affine,
        }
    }

    #[test]
    fn test_contains_call_basic() {
        assert!(contains_call(
            "let buf = create_buffer(size);",
            "create_buffer"
        ));
        assert!(contains_call("  release_buffer(buf);", "release_buffer"));
    }

    #[test]
    fn test_contains_call_method_syntax() {
        assert!(contains_call("gpu.create_buffer(1024)", "create_buffer"));
    }

    #[test]
    fn test_contains_call_rejects_substring() {
        // "xcreate_buffer" should NOT match "create_buffer".
        assert!(!contains_call("xcreate_buffer(x)", "create_buffer"));
    }

    #[test]
    fn test_contains_call_namespace_qualified() {
        assert!(contains_call("gpu::create_buffer(size)", "create_buffer"));
    }

    #[test]
    fn test_parse_source_string_finds_sites() {
        let code = r#"
fn main() {
    let buf = create_buffer(1024);
    process(buf);
    release_buffer(buf);
}
"#;
        let sites = parse_source_string(code, "test.rs", &[gpu_resource()]);
        assert_eq!(sites.len(), 2);
        assert_eq!(sites[0].kind, SiteKind::Allocation);
        assert_eq!(sites[0].line, 3);
        assert_eq!(sites[1].kind, SiteKind::Deallocation);
        assert_eq!(sites[1].line, 5);
    }

    #[test]
    fn test_parse_source_string_double_free() {
        let code = r#"
let buf = create_buffer(64);
release_buffer(buf);
release_buffer(buf);
"#;
        let sites = parse_source_string(code, "test.rs", &[gpu_resource()]);
        let deallocs: Vec<_> = sites
            .iter()
            .filter(|s| s.kind == SiteKind::Deallocation)
            .collect();
        assert_eq!(deallocs.len(), 2, "Should detect both deallocation sites");
    }

    #[test]
    fn test_is_comment() {
        assert!(is_comment("// this is a comment", SourceLanguage::Rust));
        assert!(!is_comment("let x = 1;", SourceLanguage::Rust));
        assert!(is_comment("/* block comment */", SourceLanguage::C));
    }
}
