// SPDX-License-Identifier: PMPL-1.0-or-later
// Example source file for affinescriptiser — demonstrates resource allocation patterns
// that the parser will detect and the affine type system will wrap.

/// Simulate creating a GPU buffer (allocation site).
fn create_buffer(size: usize) -> *mut u8 {
    // In real code this would call a GPU API.
    std::ptr::null_mut()
}

/// Simulate releasing a GPU buffer (deallocation site).
fn release_buffer(buf: *mut u8) {
    // In real code this would free GPU memory.
}

/// Simulate creating a session token (allocation site).
fn create_session(user: &str) -> u64 {
    // In real code this would create an auth token.
    42
}

/// Simulate revoking a session token (deallocation site).
fn revoke_session(token: u64) {
    // In real code this would invalidate the session.
}

fn main() {
    // Correct usage: allocate then deallocate.
    let buf = create_buffer(1024);
    release_buffer(buf);

    // Correct usage: create then revoke session.
    let token = create_session("alice");
    revoke_session(token);
}
