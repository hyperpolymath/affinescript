// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright 2024 AffineScript Contributors

//! Foreign Function Interface for AffineScript Runtime
//!
//! This module provides the FFI layer between AffineScript and host environments.
//! It handles:
//! - Type conversions between AffineScript and host types
//! - String interop
//! - Callback management
//! - Host function imports
//!
//! # Supported Hosts
//!
//! - **JavaScript**: Via wasm-bindgen or direct imports
//! - **WASI**: Standard WASI interfaces
//! - **Native**: For testing and tooling

#[cfg(not(feature = "std"))]
use core::slice;

#[cfg(feature = "std")]
use std::slice;

/// String representation for FFI
///
/// AffineScript strings are UTF-8, length-prefixed.
#[repr(C)]
pub struct FfiString {
    /// Pointer to string data
    pub ptr: *const u8,
    /// Length in bytes
    pub len: usize,
}

impl FfiString {
    /// Create from raw parts
    pub const fn new(ptr: *const u8, len: usize) -> Self {
        FfiString { ptr, len }
    }

    /// Get as byte slice
    pub unsafe fn as_bytes(&self) -> &[u8] {
        slice::from_raw_parts(self.ptr, self.len)
    }

    /// Get as str (unchecked)
    pub unsafe fn as_str(&self) -> &str {
        core::str::from_utf8_unchecked(self.as_bytes())
    }
}

/// Array representation for FFI
#[repr(C)]
pub struct FfiArray<T> {
    /// Pointer to array data
    pub ptr: *const T,
    /// Number of elements
    pub len: usize,
}

/// Result type for FFI operations
#[repr(C)]
pub struct FfiResult<T> {
    /// Success value (if is_ok is true)
    pub value: T,
    /// Error message (if is_ok is false)
    pub error: FfiString,
    /// Whether operation succeeded
    pub is_ok: bool,
}

/// Callback type for host functions
pub type HostCallback = extern "C" fn(*const (), *mut ()) -> *mut ();

/// Host function registry
struct HostRegistry {
    /// Registered callbacks
    callbacks: [Option<HostCallback>; 256],
    /// Number of registered callbacks
    count: usize,
}

static mut HOST_REGISTRY: HostRegistry = HostRegistry {
    callbacks: [None; 256],
    count: 0,
};

/// Initialize FFI layer
pub fn init() {
    // TODO: Phase 6 implementation
    // - [ ] Set up host function table
    // - [ ] Initialize string interop
    // - [ ] Register built-in imports
}

/// Register a host callback
///
/// # Arguments
///
/// * `id` - Unique identifier for the callback
/// * `callback` - The callback function
///
/// # Returns
///
/// True if registration succeeded
#[no_mangle]
pub extern "C" fn register_host_callback(id: u32, callback: HostCallback) -> bool {
    unsafe {
        if (id as usize) < HOST_REGISTRY.callbacks.len() {
            HOST_REGISTRY.callbacks[id as usize] = Some(callback);
            HOST_REGISTRY.count += 1;
            true
        } else {
            false
        }
    }
}

/// Call a host function
///
/// # Arguments
///
/// * `id` - Callback identifier
/// * `arg` - Argument to pass
///
/// # Returns
///
/// Result from host function
#[no_mangle]
pub extern "C" fn call_host(id: u32, arg: *const ()) -> *mut () {
    unsafe {
        if let Some(callback) = HOST_REGISTRY.callbacks.get(id as usize).and_then(|c| *c) {
            callback(arg, core::ptr::null_mut())
        } else {
            core::ptr::null_mut()
        }
    }
}

// ============================================================================
// String operations
// ============================================================================

/// Allocate a string buffer
#[no_mangle]
pub extern "C" fn string_alloc(len: usize) -> *mut u8 {
    crate::alloc::allocate(len, 1)
}

/// Free a string buffer
#[no_mangle]
pub unsafe extern "C" fn string_free(ptr: *mut u8, len: usize) {
    crate::alloc::deallocate(ptr, len, 1)
}

/// Copy string to host
#[no_mangle]
pub extern "C" fn string_to_host(s: FfiString) -> *const u8 {
    s.ptr
}

// ============================================================================
// JavaScript interop (wasm-bindgen compatible)
// ============================================================================

#[cfg(target_arch = "wasm32")]
mod js {
    use super::*;

    extern "C" {
        // These would be provided by wasm-bindgen or manual JS glue

        /// Log to console
        #[link_name = "__affinescript_console_log"]
        pub fn console_log(msg: *const u8, len: usize);

        /// Get current time in milliseconds
        #[link_name = "__affinescript_now"]
        pub fn now() -> f64;

        /// Call JavaScript function
        #[link_name = "__affinescript_js_call"]
        pub fn js_call(func_id: u32, arg: *const (), arg_len: usize) -> *mut ();
    }

    /// Print to console
    #[no_mangle]
    pub extern "C" fn print(s: FfiString) {
        unsafe {
            console_log(s.ptr, s.len);
        }
    }
}

// ============================================================================
// WASI support
// ============================================================================

#[cfg(feature = "wasi")]
mod wasi_ffi {
    use super::*;

    /// Write to stdout
    #[no_mangle]
    pub extern "C" fn wasi_print(s: FfiString) {
        // TODO: Phase 6 implementation
        // - [ ] Use fd_write to stdout
    }

    /// Read from stdin
    #[no_mangle]
    pub extern "C" fn wasi_read_line() -> FfiString {
        // TODO: Phase 6 implementation
        // - [ ] Use fd_read from stdin
        FfiString::new(core::ptr::null(), 0)
    }

    /// Get environment variable
    #[no_mangle]
    pub extern "C" fn wasi_getenv(name: FfiString) -> FfiString {
        // TODO: Phase 6 implementation
        // - [ ] Use environ_get
        FfiString::new(core::ptr::null(), 0)
    }

    /// Get command line arguments
    #[no_mangle]
    pub extern "C" fn wasi_args() -> FfiArray<FfiString> {
        // TODO: Phase 6 implementation
        // - [ ] Use args_get
        FfiArray {
            ptr: core::ptr::null(),
            len: 0,
        }
    }
}

// ============================================================================
// Type conversions
// ============================================================================

/// Convert AffineScript Int to host i64
#[no_mangle]
pub extern "C" fn int_to_i64(value: *const ()) -> i64 {
    // TODO: Phase 6 implementation
    // AffineScript Ints may be arbitrary precision
    0
}

/// Convert host i64 to AffineScript Int
#[no_mangle]
pub extern "C" fn i64_to_int(value: i64) -> *mut () {
    // TODO: Phase 6 implementation
    core::ptr::null_mut()
}

/// Convert AffineScript Float to host f64
#[no_mangle]
pub extern "C" fn float_to_f64(value: *const ()) -> f64 {
    // TODO: Phase 6 implementation
    0.0
}

/// Convert host f64 to AffineScript Float
#[no_mangle]
pub extern "C" fn f64_to_float(value: f64) -> *mut () {
    // TODO: Phase 6 implementation
    core::ptr::null_mut()
}

// TODO: Phase 6 implementation
// - [ ] Implement wasm-bindgen integration
// - [ ] Add JSON serialization for complex types
// - [ ] Implement async callback support
// - [ ] Add TypeScript type generation
// - [ ] Implement Component Model interface types (future)
