// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright 2024 AffineScript Contributors

//! Optional Garbage Collector for AffineScript Runtime
//!
//! This module provides a simple mark-sweep garbage collector for
//! cyclic data structures with ω (unrestricted) quantity.
//!
//! # When GC is Needed
//!
//! Most AffineScript data is linear or affine and doesn't need GC.
//! However, ω-quantity data (unrestricted/shareable) can form cycles:
//!
//! ```affinescript
//! type Node = {
//!     value: Int,
//!     ω next: Option[Node]  // Can be shared, may form cycles
//! }
//! ```
//!
//! # Algorithm
//!
//! Simple mark-sweep:
//! 1. Mark: Trace from roots, mark reachable objects
//! 2. Sweep: Free unmarked objects
//!
//! # Performance
//!
//! - Collection is triggered when:
//!   - Allocation fails and memory is low
//!   - Explicit `gc::collect()` call
//!   - Threshold of ω allocations reached
//!
//! - This GC is intentionally simple; most memory is managed
//!   via ownership and doesn't need GC.

#[cfg(not(feature = "std"))]
use core::ptr::NonNull;

#[cfg(feature = "std")]
use std::ptr::NonNull;

/// GC object header
///
/// Prepended to all GC-managed allocations.
#[repr(C)]
pub struct GcHeader {
    /// Mark bit (set during marking phase)
    pub marked: bool,
    /// Object size (excluding header)
    pub size: u32,
    /// Type tag (for tracing)
    pub type_tag: u32,
    /// Next object in allocation list
    pub next: *mut GcHeader,
}

/// GC-managed pointer
///
/// Smart pointer for garbage-collected values.
#[repr(transparent)]
pub struct Gc<T: ?Sized> {
    ptr: NonNull<GcHeader>,
    _marker: core::marker::PhantomData<T>,
}

impl<T> Gc<T> {
    /// Allocate a new GC-managed value
    pub fn new(value: T) -> Self {
        // TODO: Phase 6 implementation
        // - [ ] Allocate space for header + value
        // - [ ] Initialize header
        // - [ ] Register in allocation list
        // - [ ] Store value

        unimplemented!("GC allocation not yet implemented")
    }
}

/// Mutable GC cell (interior mutability)
pub struct GcCell<T: ?Sized> {
    inner: Gc<core::cell::UnsafeCell<T>>,
}

/// GC statistics
#[derive(Default)]
pub struct GcStats {
    /// Total collections performed
    pub collections: usize,
    /// Objects currently alive
    pub live_objects: usize,
    /// Bytes currently allocated
    pub live_bytes: usize,
    /// Objects freed in last collection
    pub last_freed: usize,
}

/// Global GC state
struct GcState {
    /// Head of allocation list
    alloc_list: *mut GcHeader,
    /// Number of allocations since last GC
    alloc_count: usize,
    /// Threshold to trigger collection
    threshold: usize,
    /// Statistics
    stats: GcStats,
    /// Root set
    roots: [*mut GcHeader; 256],
    /// Number of roots
    root_count: usize,
}

static mut GC_STATE: GcState = GcState {
    alloc_list: core::ptr::null_mut(),
    alloc_count: 0,
    threshold: 1000,
    stats: GcStats {
        collections: 0,
        live_objects: 0,
        live_bytes: 0,
        last_freed: 0,
    },
    roots: [core::ptr::null_mut(); 256],
    root_count: 0,
};

/// Initialize the garbage collector
pub fn init() {
    // TODO: Phase 6 implementation
    // - [ ] Set up allocation list
    // - [ ] Initialize root set
    // - [ ] Set threshold based on available memory
}

/// Perform garbage collection
#[no_mangle]
pub extern "C" fn collect() {
    unsafe {
        GC_STATE.stats.collections += 1;

        // Mark phase
        mark_from_roots();

        // Sweep phase
        sweep();

        // Reset allocation counter
        GC_STATE.alloc_count = 0;
    }
}

/// Mark phase: trace from roots
unsafe fn mark_from_roots() {
    // TODO: Phase 6 implementation
    // - [ ] Mark all roots
    // - [ ] Recursively mark reachable objects
    // - [ ] Handle cycles (already marked = skip)
}

/// Sweep phase: free unmarked objects
unsafe fn sweep() {
    // TODO: Phase 6 implementation
    // - [ ] Walk allocation list
    // - [ ] Free unmarked objects
    // - [ ] Clear marks on surviving objects
    // - [ ] Update statistics
}

/// Register a root
///
/// Roots are objects that should not be collected even if
/// not reachable from other GC objects.
#[no_mangle]
pub extern "C" fn gc_add_root(obj: *mut GcHeader) {
    unsafe {
        if GC_STATE.root_count < GC_STATE.roots.len() {
            GC_STATE.roots[GC_STATE.root_count] = obj;
            GC_STATE.root_count += 1;
        }
    }
}

/// Unregister a root
#[no_mangle]
pub extern "C" fn gc_remove_root(obj: *mut GcHeader) {
    unsafe {
        for i in 0..GC_STATE.root_count {
            if GC_STATE.roots[i] == obj {
                // Swap with last and decrease count
                GC_STATE.roots[i] = GC_STATE.roots[GC_STATE.root_count - 1];
                GC_STATE.root_count -= 1;
                break;
            }
        }
    }
}

/// Allocate GC-managed memory
#[no_mangle]
pub extern "C" fn gc_alloc(size: usize, type_tag: u32) -> *mut GcHeader {
    // Check if we should collect first
    unsafe {
        if GC_STATE.alloc_count >= GC_STATE.threshold {
            collect();
        }
    }

    // Allocate header + data (with overflow check)
    let total_size = match size.checked_add(core::mem::size_of::<GcHeader>()) {
        Some(s) => s,
        None => return core::ptr::null_mut(), // Size overflow
    };
    let ptr = crate::alloc::allocate(total_size, core::mem::align_of::<GcHeader>());

    if ptr.is_null() {
        // Try collecting and retry
        collect();
        let ptr = crate::alloc::allocate(total_size, core::mem::align_of::<GcHeader>());
        if ptr.is_null() {
            return core::ptr::null_mut();
        }
    }

    // Initialize header
    let header = ptr as *mut GcHeader;
    unsafe {
        (*header).marked = false;
        (*header).size = size as u32;
        (*header).type_tag = type_tag;

        // Add to allocation list
        (*header).next = GC_STATE.alloc_list;
        GC_STATE.alloc_list = header;
        GC_STATE.alloc_count += 1;
        GC_STATE.stats.live_objects += 1;
        GC_STATE.stats.live_bytes += total_size;
    }

    header
}

/// Get data pointer from header
#[no_mangle]
pub extern "C" fn gc_data(header: *mut GcHeader) -> *mut u8 {
    if header.is_null() {
        return core::ptr::null_mut();
    }
    unsafe { header.add(1) as *mut u8 }
}

/// Get GC statistics
#[no_mangle]
pub extern "C" fn gc_stats() -> GcStats {
    unsafe { GC_STATE.stats }
}

/// Force a collection if above threshold
#[no_mangle]
pub extern "C" fn gc_maybe_collect() {
    unsafe {
        if GC_STATE.alloc_count >= GC_STATE.threshold {
            collect();
        }
    }
}

/// Set collection threshold
#[no_mangle]
pub extern "C" fn gc_set_threshold(threshold: usize) {
    unsafe {
        GC_STATE.threshold = threshold;
    }
}

// TODO: Phase 6 implementation
// - [ ] Implement proper mark phase with type-based tracing
// - [ ] Implement sweep with proper memory deallocation
// - [ ] Add write barrier for generational GC (optional)
// - [ ] Add incremental collection (optional)
// - [ ] Add finalization support
// - [ ] Integrate with effect system for GC-safe points
