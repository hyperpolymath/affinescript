// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2024-2025 hyperpolymath

//! AffineScript Runtime Library
//!
//! This crate provides the runtime support for AffineScript programs
//! compiled to WebAssembly.
//!
//! # Features
//!
//! - `std`: Enable standard library support (default)
//! - `gc`: Enable garbage collector for cyclic data
//! - `wasi`: Enable WASI support for CLI programs
//!
//! # Modules
//!
//! - [`alloc`]: Memory allocation
//! - [`effects`]: Effect handling runtime
//! - [`panic`]: Panic handling
//! - [`ffi`]: Foreign function interface

#![cfg_attr(not(feature = "std"), no_std)]
#![warn(missing_docs)]

#[cfg(not(feature = "std"))]
extern crate alloc as std_alloc;

pub mod alloc;
pub mod effects;
pub mod panic;
pub mod ffi;

#[cfg(feature = "gc")]
pub mod gc;

/// Re-exports for generated code
pub mod prelude {
    pub use crate::effects::{Evidence, Handler, resume};
    pub use crate::alloc::{allocate, deallocate};

    #[cfg(feature = "gc")]
    pub use crate::gc::{Gc, GcCell};
}

/// Runtime initialization
///
/// Called at the start of every AffineScript program.
#[no_mangle]
pub extern "C" fn __affinescript_init() {
    // Initialize allocator
    alloc::init();

    // Initialize panic handler
    panic::init();

    // Initialize effect system
    effects::init();
}

/// Runtime cleanup
///
/// Called at the end of every AffineScript program.
#[no_mangle]
pub extern "C" fn __affinescript_cleanup() {
    #[cfg(feature = "gc")]
    gc::collect();
}

// TODO: Phase 6 implementation
// - [ ] Memory allocator optimized for linear values
// - [ ] Effect evidence passing runtime
// - [ ] Handler frame management
// - [ ] Continuation allocation/deallocation
// - [ ] WASI integration
// - [ ] JavaScript interop via wasm-bindgen
