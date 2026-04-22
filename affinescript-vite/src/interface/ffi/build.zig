// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// Template FFI Build Configuration (Zig 0.15.2+)
// Note: This is a minimal build file that demonstrates Zig integration

const std = @import("std");

pub fn build(b: *std.Build) void {
    _ = b.standardTargetOptions(.{});
    _ = b.standardOptimizeOption(.{});

    // In Zig 0.15+, tests are run directly with:
    //   zig build-exe -ftest-runner src/main.zig
    //   zig build-exe -ftest-runner test/integration_test.zig
    //
    // This minimal build file provides scaffolding for future expansion.
    // Tests can be invoked via command line without explicit build.zig configuration.
}
