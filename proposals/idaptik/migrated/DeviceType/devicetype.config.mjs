// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for DeviceType.affine (idaptik device-type taxonomy;
// scalar i32 ABI). The oracle re-derives the 0..11 closed band in plain JS so a
// codegen regression surfaces as a differential mismatch.
const valid = (c) => c >= 0 && c <= 11;
export default {
  affine: "DeviceType.affine",
  cases: [
    { name: "device_type_count()", export: "device_type_count", args: [], oracle: () => 12 },
    {
      name: "is_valid_device_type over [-3..15]",
      export: "is_valid_device_type",
      args: [[-3, 15]],
      oracle: (c) => (valid(c) ? 1 : 0),
    },
    {
      name: "clamp_device_type over [-3..15]",
      export: "clamp_device_type",
      args: [[-3, 15]],
      oracle: (c) => (valid(c) ? c : -1),
    },
  ],
};
