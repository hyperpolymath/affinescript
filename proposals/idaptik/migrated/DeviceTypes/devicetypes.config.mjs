// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for DeviceTypes.affine (idaptik device/security colour
// lookup; scalar i32 ABI). The oracle re-derives getDeviceColor / getSecurityColor
// from DeviceTypes.res INDEPENDENTLY in plain JS, using the HEX literals exactly
// as the ReScript source writes them (not the .affine's decimal encoding), so a
// transcription error in the .affine decimals -- or a codegen regression --
// surfaces as a differential mismatch.

// getDeviceColor: 8-kind table in variant-declaration order, -1 out of band.
const deviceColour = (c) => {
  switch (c) {
    case 0: return 0x2196F3; // Laptop
    case 1: return 0xFF9800; // Router
    case 2: return 0x9C27B0; // Server
    case 3: return 0xF44336; // IotCamera
    case 4: return 0x4CAF50; // Terminal
    case 5: return 0xFFEB3B; // PowerStation
    case 6: return 0x795548; // UPS
    case 7: return 0xE53935; // Firewall
    default: return -1;
  }
};

// getSecurityColor: 4-level table in variant order, -1 out of band.
const securityColour = (c) => {
  switch (c) {
    case 0: return 0x00ff00; // Open
    case 1: return 0xffff00; // Weak
    case 2: return 0xff9800; // Medium
    case 3: return 0xff0000; // Strong
    default: return -1;
  }
};

export default {
  affine: "DeviceTypes.affine",
  cases: [
    { name: "device_type_count()", export: "device_type_count", args: [], oracle: () => 8 },
    { name: "security_level_count()", export: "security_level_count", args: [], oracle: () => 4 },
    {
      name: "get_device_colour over [-2..10]",
      export: "get_device_colour",
      args: [[-2, 10]],
      oracle: (c) => deviceColour(c),
    },
    {
      name: "get_security_colour over [-2..6]",
      export: "get_security_colour",
      args: [[-2, 6]],
      oracle: (c) => securityColour(c),
    },
  ],
};
