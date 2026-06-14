// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for VmState.affine (idaptik flat-memory addressing core;
// scalar i32 ABI). The oracle re-derives the 256-cell band, range addressing,
// stack depth, and unread-port count in plain JS straight from the
// VmState.res introspection semantics, so a codegen regression surfaces as a
// differential mismatch.

// In-bounds memory cell: 0..255.
const validAddr = (a) => a >= 0 && a <= 255;

export default {
  affine: "VmState.affine",
  cases: [
    { name: "memory_size()", export: "memory_size", args: [], oracle: () => 256 },
    { name: "max_address()", export: "max_address", args: [], oracle: () => 255 },
    {
      name: "is_valid_address over [-3..260]",
      export: "is_valid_address",
      args: [[-3, 260]],
      oracle: (a) => (validAddr(a) ? 1 : 0),
    },
    {
      name: "clamp_address over [-3..260]",
      export: "clamp_address",
      args: [[-3, 260]],
      oracle: (a) => (validAddr(a) ? a : -1),
    },
    {
      name: "range_addr(start, i) = start + i",
      export: "range_addr",
      args: [[-2, 260], [-2, 18]],
      oracle: (start, i) => (start + i) | 0,
    },
    {
      name: "range_addr_in_bounds(start, length, i)",
      export: "range_addr_in_bounds",
      args: [[-2, 258], [0, 8], [-2, 10]],
      oracle: (start, length, i) => {
        if (i < 0) return 0;
        if (i > length - 1) return 0;
        return validAddr((start + i) | 0) ? 1 : 0;
      },
    },
    {
      name: "stack_depth(sp) = max(sp, 0)",
      export: "stack_depth",
      args: [[-5, 300]],
      oracle: (sp) => (sp < 0 ? 0 : sp),
    },
    {
      name: "port_input_remaining(count, ptr) = max(count - ptr, 0)",
      export: "port_input_remaining",
      args: [[0, 40], [0, 40]],
      oracle: (count, ptr) => (count - ptr < 0 ? 0 : count - ptr),
    },
  ],
};
