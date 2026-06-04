// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for Kernel_IO.affine (idaptik pure-integer device
// sandbox check).
//
// Kernel_IO's interesting export, `in_sandbox(data: [Int], root: [Int])`, takes
// two Int-array parameters -- the array ABI ([len:i32 LE][utf8/bytes] + an
// exported __affine_alloc) which THIS runner does not yet marshal (it handles
// the scalar i32 ABI only; see the ABI SCOPE note in parity.mjs). So we cannot
// sweep in_sandbox from here.
//
// What we CAN do, staying inside the scalar ABI, is exercise the module's own
// self-checking entry point `main()`. It runs six inlined sandbox scenarios
// (exact root IN, root+"/file" IN, other-device OUT, /etc/passwd OUT, empty IN,
// "/sandbox/dev10" prefix-without-separator OUT) and returns the count that
// passed -- 6 when all green. A nullary export, so the oracle is the constant 6.
//
// This proves the harness drives a nullary i32-returning export and that the
// re-decomposed array logic (inlined in main) computes correctly end to end.

export default {
  affine: "/tmp/idaptik-migration/Kernel_IO.affine",
  cases: [
    {
      name: "main() self-check returns 6",
      export: "main",
      args: [], // nullary
      oracle: () => 6,
    },
  ],
};
