// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2024-2025 hyperpolymath

//! Memory Allocator for AffineScript Runtime
//!
//! This module provides memory allocation optimized for linear/affine values.
//! Since most values are used exactly once, we can use a simple bump allocator
//! with explicit deallocation.
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────┐
//! │                    WASM Linear Memory                       │
//! ├─────────────────────────────────────────────────────────────┤
//! │   Stack   │   Heap (bump)   │   Free List   │   Reserved   │
//! └─────────────────────────────────────────────────────────────┘
//! ```
//!
//! # Features
//!
//! - **Bump allocation**: Fast O(1) allocation for short-lived values
//! - **Free list**: Reuse deallocated blocks for longer-lived values
//! - **Size classes**: Segregated free lists for common sizes
//! - **Linear optimization**: Skip reference counting for linear values

#[cfg(not(feature = "std"))]
use core::{alloc::Layout, ptr::NonNull};

#[cfg(feature = "std")]
use std::{alloc::Layout, ptr::NonNull};

/// Memory block header
#[repr(C)]
struct BlockHeader {
    /// Size of the block (excluding header)
    size: usize,
    /// Flags (in_use, is_linear, etc.)
    flags: u32,
}

/// Size classes for segregated free lists
const SIZE_CLASSES: [usize; 8] = [16, 32, 64, 128, 256, 512, 1024, 2048];

/// Global allocator state
struct AllocatorState {
    /// Start of heap
    heap_start: usize,
    /// Current bump pointer
    bump_ptr: usize,
    /// End of available heap
    heap_end: usize,
    /// Free lists by size class
    free_lists: [Option<NonNull<BlockHeader>>; 8],
    /// Total allocated bytes
    allocated: usize,
    /// Total freed bytes
    freed: usize,
}

static mut ALLOCATOR: AllocatorState = AllocatorState {
    heap_start: 0,
    bump_ptr: 0,
    heap_end: 0,
    free_lists: [None; 8],
    allocated: 0,
    freed: 0,
};

/// Initialize the allocator
///
/// Called once at program startup.
pub fn init() {
    // TODO: Phase 6 implementation
    // - [ ] Query WASM memory size
    // - [ ] Set up heap region
    // - [ ] Initialize free lists
    // - [ ] Set up guard pages (if available)
}

/// Allocate memory
///
/// # Arguments
///
/// * `size` - Number of bytes to allocate
/// * `align` - Required alignment (must be power of 2)
///
/// # Returns
///
/// Pointer to allocated memory, or null on failure
#[no_mangle]
pub extern "C" fn allocate(size: usize, align: usize) -> *mut u8 {
    // TODO: Phase 6 implementation
    // - [ ] Check free list for matching size class
    // - [ ] Fall back to bump allocation
    // - [ ] Handle out-of-memory (grow memory or fail)
    // - [ ] Track allocation statistics

    core::ptr::null_mut()
}

/// Deallocate memory
///
/// # Arguments
///
/// * `ptr` - Pointer previously returned by `allocate`
/// * `size` - Size of the allocation
/// * `align` - Alignment of the allocation
///
/// # Safety
///
/// The pointer must have been allocated by this allocator and not yet freed.
#[no_mangle]
pub unsafe extern "C" fn deallocate(ptr: *mut u8, size: usize, align: usize) {
    // TODO: Phase 6 implementation
    // - [ ] Validate pointer is in heap range
    // - [ ] Add to appropriate free list
    // - [ ] Coalesce adjacent free blocks (optional)
    // - [ ] Track deallocation statistics
}

/// Reallocate memory
///
/// # Arguments
///
/// * `ptr` - Pointer previously returned by `allocate`
/// * `old_size` - Current size of the allocation
/// * `new_size` - Desired new size
/// * `align` - Alignment requirement
///
/// # Returns
///
/// Pointer to reallocated memory (may be different from input)
#[no_mangle]
pub unsafe extern "C" fn reallocate(
    ptr: *mut u8,
    old_size: usize,
    new_size: usize,
    align: usize,
) -> *mut u8 {
    // TODO: Phase 6 implementation
    // - [ ] If shrinking, just update size
    // - [ ] If growing and space available, extend in place
    // - [ ] Otherwise allocate new block and copy

    core::ptr::null_mut()
}

/// Get allocation statistics
#[no_mangle]
pub extern "C" fn alloc_stats() -> (usize, usize, usize) {
    unsafe {
        (ALLOCATOR.allocated, ALLOCATOR.freed, ALLOCATOR.allocated - ALLOCATOR.freed)
    }
}

// TODO: Phase 6 implementation
// - [ ] Implement size class selection
// - [ ] Implement free list management
// - [ ] Add memory growth support
// - [ ] Add allocation tracking for debugging
// - [ ] Optimize for common patterns (small allocations, LIFO)
