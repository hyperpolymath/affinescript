// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// ABI module for affinescriptiser — Core type definitions for affine/linear resource tracking,
// WASM compilation targets, and violation reporting. These types model the AffineScript type
// system concepts: affine types (at-most-once usage), linear types (exactly-once usage),
// resource lifecycle tracking, and WASM binary constraints.

use serde::{Deserialize, Serialize};
use std::fmt;

/// Describes whether a resource follows affine or linear typing discipline.
///
/// - **Affine**: The resource may be used at most once. Dropping without deallocation is permitted.
/// - **Linear**: The resource must be used exactly once. Both leaking and double-free are violations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum AffinityKind {
    /// At-most-once usage — dropping is safe, double-use is not.
    Affine,
    /// Exactly-once usage — must be consumed, neither leaked nor double-freed.
    Linear,
}

impl fmt::Display for AffinityKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AffinityKind::Affine => write!(f, "affine"),
            AffinityKind::Linear => write!(f, "linear"),
        }
    }
}

impl AffinityKind {
    /// Returns true if leaking (not deallocating) this resource is a violation.
    /// Affine resources can be safely leaked; linear resources cannot.
    pub fn leak_is_violation(&self) -> bool {
        matches!(self, AffinityKind::Linear)
    }
}

/// A resource tracked by the affine/linear type system. Represents a handle (e.g. GpuBuffer,
/// file descriptor, session token) with a known allocator, deallocator, and affinity discipline.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AffineResource {
    /// Human-readable resource name (e.g. "GpuBuffer", "SessionToken").
    pub name: String,
    /// Function or method that allocates this resource (e.g. "create_buffer").
    pub allocator: String,
    /// Function or method that deallocates this resource (e.g. "release_buffer").
    pub deallocator: String,
    /// Whether this resource is affine (at-most-once) or linear (exactly-once).
    pub affinity: AffinityKind,
}

impl fmt::Display for AffineResource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}({}) [alloc={}, dealloc={}]",
            self.name, self.affinity, self.allocator, self.deallocator
        )
    }
}

/// WASM compilation target triple.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Default)]
pub enum WasmTarget {
    /// Standard WASM target without WASI — runs in browser or embedded runtimes.
    #[serde(rename = "wasm32-unknown-unknown")]
    #[default]
    Wasm32Unknown,
    /// WASI-enabled target — can access filesystem, environment, etc.
    #[serde(rename = "wasm32-wasi")]
    Wasm32Wasi,
}

impl fmt::Display for WasmTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WasmTarget::Wasm32Unknown => write!(f, "wasm32-unknown-unknown"),
            WasmTarget::Wasm32Wasi => write!(f, "wasm32-wasi"),
        }
    }
}

/// Configuration for the WASM compilation output — target triple, optimisation toggle,
/// and optional binary size limit to enforce deployment constraints.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct WasmConfig {
    /// The WASM target triple to compile for.
    #[serde(default)]
    pub target: WasmTarget,
    /// Whether to apply wasm-opt or equivalent optimisation passes.
    #[serde(default)]
    pub optimize: bool,
    /// Maximum allowed WASM binary size in kilobytes. None means no limit.
    #[serde(rename = "size-limit-kb", default)]
    pub size_limit_kb: Option<u64>,
}

/// Categories of affine/linear type violations that the analyser can detect.
/// Each variant corresponds to a distinct failure mode in resource lifecycle management.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ViolationType {
    /// A linear resource was allocated but never deallocated — resource leak.
    /// Only applies to linear resources; affine resources may be safely dropped.
    Leaked {
        /// Name of the leaked resource.
        resource: String,
        /// Source location (file:line) where the resource was allocated.
        allocated_at: String,
    },
    /// A resource was deallocated more than once — double-free bug.
    /// Applies to both affine and linear resources.
    DoubleFree {
        /// Name of the double-freed resource.
        resource: String,
        /// Source location of the first deallocation.
        first_free_at: String,
        /// Source location of the second (erroneous) deallocation.
        second_free_at: String,
    },
    /// A resource was used after being deallocated — use-after-free bug.
    /// Applies to both affine and linear resources.
    UseAfterFree {
        /// Name of the resource used after deallocation.
        resource: String,
        /// Source location where the resource was freed.
        freed_at: String,
        /// Source location where the freed resource was erroneously accessed.
        used_at: String,
    },
    /// The compiled WASM binary exceeds the configured size limit.
    SizeExceeded {
        /// Actual binary size in kilobytes.
        actual_kb: u64,
        /// Configured maximum size in kilobytes.
        limit_kb: u64,
    },
}

impl fmt::Display for ViolationType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ViolationType::Leaked {
                resource,
                allocated_at,
            } => {
                write!(
                    f,
                    "LEAK: linear resource '{}' allocated at {} was never deallocated",
                    resource, allocated_at
                )
            }
            ViolationType::DoubleFree {
                resource,
                first_free_at,
                second_free_at,
            } => {
                write!(
                    f,
                    "DOUBLE-FREE: resource '{}' freed at {} and again at {}",
                    resource, first_free_at, second_free_at
                )
            }
            ViolationType::UseAfterFree {
                resource,
                freed_at,
                used_at,
            } => {
                write!(
                    f,
                    "USE-AFTER-FREE: resource '{}' freed at {} but used at {}",
                    resource, freed_at, used_at
                )
            }
            ViolationType::SizeExceeded {
                actual_kb,
                limit_kb,
            } => {
                write!(
                    f,
                    "SIZE-EXCEEDED: WASM binary is {}KB but limit is {}KB",
                    actual_kb, limit_kb
                )
            }
        }
    }
}

/// A complete violation report combining the violation type with contextual metadata.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Violation {
    /// The specific type of violation detected.
    pub violation: ViolationType,
    /// Severity level: "error" for type-system violations, "warning" for size constraints.
    pub severity: String,
    /// Human-readable suggestion for fixing the violation.
    pub suggestion: String,
}

impl Violation {
    /// Constructs a new violation report from a violation type. Automatically determines
    /// severity (errors for type violations, warnings for size constraints) and generates
    /// an appropriate fix suggestion.
    pub fn new(violation: ViolationType) -> Self {
        let (severity, suggestion) = match &violation {
            ViolationType::Leaked { resource, .. } => (
                "error".to_string(),
                format!(
                    "Ensure '{}' is consumed or explicitly deallocated before scope exit",
                    resource
                ),
            ),
            ViolationType::DoubleFree { resource, .. } => (
                "error".to_string(),
                format!(
                    "Remove the second deallocation of '{}', or guard with ownership check",
                    resource
                ),
            ),
            ViolationType::UseAfterFree { resource, .. } => (
                "error".to_string(),
                format!(
                    "Do not access '{}' after deallocation — restructure control flow",
                    resource
                ),
            ),
            ViolationType::SizeExceeded { .. } => (
                "warning".to_string(),
                "Enable wasm-opt, strip debug symbols, or increase size-limit-kb".to_string(),
            ),
        };
        Self {
            violation,
            severity,
            suggestion,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_affinity_kind_display() {
        assert_eq!(AffinityKind::Affine.to_string(), "affine");
        assert_eq!(AffinityKind::Linear.to_string(), "linear");
    }

    #[test]
    fn test_affinity_leak_semantics() {
        assert!(!AffinityKind::Affine.leak_is_violation());
        assert!(AffinityKind::Linear.leak_is_violation());
    }

    #[test]
    fn test_wasm_target_display() {
        assert_eq!(
            WasmTarget::Wasm32Unknown.to_string(),
            "wasm32-unknown-unknown"
        );
        assert_eq!(WasmTarget::Wasm32Wasi.to_string(), "wasm32-wasi");
    }

    #[test]
    fn test_violation_display() {
        let v = ViolationType::Leaked {
            resource: "GpuBuffer".to_string(),
            allocated_at: "src/core.rs:42".to_string(),
        };
        assert!(v.to_string().contains("LEAK"));
        assert!(v.to_string().contains("GpuBuffer"));
    }

    #[test]
    fn test_violation_report_severity() {
        let leak = Violation::new(ViolationType::Leaked {
            resource: "X".into(),
            allocated_at: "a.rs:1".into(),
        });
        assert_eq!(leak.severity, "error");

        let size = Violation::new(ViolationType::SizeExceeded {
            actual_kb: 1024,
            limit_kb: 512,
        });
        assert_eq!(size.severity, "warning");
    }

    #[test]
    fn test_affine_resource_display() {
        let r = AffineResource {
            name: "GpuBuffer".into(),
            allocator: "create_buffer".into(),
            deallocator: "release_buffer".into(),
            affinity: AffinityKind::Affine,
        };
        let s = r.to_string();
        assert!(s.contains("GpuBuffer"));
        assert!(s.contains("affine"));
    }
}
