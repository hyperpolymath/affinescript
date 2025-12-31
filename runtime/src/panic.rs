// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright 2024 AffineScript Contributors

//! Panic Handling for AffineScript Runtime
//!
//! This module provides panic and error handling infrastructure.
//! Since AffineScript compiles to WASM, panics need special handling.
//!
//! # Panic Strategy
//!
//! 1. **Abort**: Immediately terminate execution (default for WASM)
//! 2. **Trap**: Trigger WASM trap instruction
//! 3. **Unwind**: Stack unwinding (requires exception handling proposal)
//!
//! # Error Reporting
//!
//! Errors are reported to the host via:
//! - WASI stderr (if available)
//! - Host-provided callback
//! - WASM trap with error code

#[cfg(not(feature = "std"))]
use core::fmt::{self, Write};

#[cfg(feature = "std")]
use std::fmt::{self, Write};

/// Panic information
#[repr(C)]
pub struct PanicInfo {
    /// Error message
    pub message: *const u8,
    /// Message length
    pub message_len: usize,
    /// Source file
    pub file: *const u8,
    /// File name length
    pub file_len: usize,
    /// Line number
    pub line: u32,
    /// Column number
    pub column: u32,
}

/// Panic hook type
pub type PanicHook = extern "C" fn(*const PanicInfo);

/// Global panic hook
static mut PANIC_HOOK: Option<PanicHook> = None;

/// Error codes for WASM traps
#[repr(u32)]
pub enum ErrorCode {
    /// General panic
    Panic = 1,
    /// Out of memory
    OutOfMemory = 2,
    /// Stack overflow
    StackOverflow = 3,
    /// Integer overflow
    IntegerOverflow = 4,
    /// Division by zero
    DivisionByZero = 5,
    /// Array index out of bounds
    IndexOutOfBounds = 6,
    /// Use after move (linear type violation)
    UseAfterMove = 7,
    /// Borrow checker violation
    BorrowViolation = 8,
    /// Unhandled effect
    UnhandledEffect = 9,
    /// Assertion failure
    AssertionFailed = 10,
    /// Unreachable code
    Unreachable = 11,
}

/// Initialize panic handling
pub fn init() {
    // TODO: Phase 6 implementation
    // - [ ] Set up default panic hook
    // - [ ] Register with host for error reporting
    // - [ ] Initialize stack canaries (if enabled)
}

/// Set custom panic hook
///
/// # Arguments
///
/// * `hook` - Function to call on panic
#[no_mangle]
pub extern "C" fn set_panic_hook(hook: PanicHook) {
    unsafe {
        PANIC_HOOK = Some(hook);
    }
}

/// Panic with message
///
/// # Arguments
///
/// * `message` - Error message
/// * `file` - Source file name
/// * `line` - Line number
/// * `column` - Column number
#[no_mangle]
pub extern "C" fn panic(
    message: *const u8,
    message_len: usize,
    file: *const u8,
    file_len: usize,
    line: u32,
    column: u32,
) -> ! {
    let info = PanicInfo {
        message,
        message_len,
        file,
        file_len,
        line,
        column,
    };

    unsafe {
        if let Some(hook) = PANIC_HOOK {
            hook(&info);
        }
    }

    // Abort execution
    abort()
}

/// Panic with error code
///
/// # Arguments
///
/// * `code` - Error code indicating the type of error
#[no_mangle]
pub extern "C" fn panic_code(code: ErrorCode) -> ! {
    // TODO: Phase 6 implementation
    // - [ ] Convert code to message
    // - [ ] Call panic hook
    // - [ ] Trap with code

    abort()
}

/// Abort execution
#[no_mangle]
pub extern "C" fn abort() -> ! {
    #[cfg(target_arch = "wasm32")]
    {
        core::arch::wasm32::unreachable()
    }

    #[cfg(not(target_arch = "wasm32"))]
    {
        // For non-WASM targets (testing)
        #[cfg(feature = "std")]
        std::process::abort();

        #[cfg(not(feature = "std"))]
        loop {}
    }
}

/// Assert condition
///
/// # Arguments
///
/// * `condition` - Condition to check
/// * `message` - Error message if condition is false
#[no_mangle]
pub extern "C" fn assert(
    condition: bool,
    message: *const u8,
    message_len: usize,
    file: *const u8,
    file_len: usize,
    line: u32,
    column: u32,
) {
    if !condition {
        panic(message, message_len, file, file_len, line, column)
    }
}

/// Debug assertion (only in debug builds)
#[no_mangle]
pub extern "C" fn debug_assert(
    condition: bool,
    message: *const u8,
    message_len: usize,
    file: *const u8,
    file_len: usize,
    line: u32,
    column: u32,
) {
    #[cfg(debug_assertions)]
    if !condition {
        panic(message, message_len, file, file_len, line, column)
    }
}

/// Report use-after-move error
#[no_mangle]
pub extern "C" fn use_after_move(
    var_name: *const u8,
    var_name_len: usize,
    file: *const u8,
    file_len: usize,
    line: u32,
    column: u32,
) -> ! {
    // TODO: Phase 6 implementation
    // - [ ] Format error message
    // - [ ] Include variable name
    // - [ ] Call panic

    panic_code(ErrorCode::UseAfterMove)
}

/// Report borrow violation error
#[no_mangle]
pub extern "C" fn borrow_violation(
    message: *const u8,
    message_len: usize,
    file: *const u8,
    file_len: usize,
    line: u32,
    column: u32,
) -> ! {
    panic(message, message_len, file, file_len, line, column)
}

// TODO: Phase 6 implementation
// - [ ] Implement WASI error output
// - [ ] Add stack trace collection (if debug info available)
// - [ ] Add error code documentation
// - [ ] Implement exception handling proposal support (optional)
// - [ ] Add runtime assertions for invariants
