// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for DeviceCaps.affine. Oracle re-derives the two colour
// tables from the original DeviceTypes.res getDeviceColor and getSecurityColor
// switch semantics:
//
// getDeviceColor:
//   0 Laptop->0x2196F3, 1 Router->0xFF9800, 2 Server->0x9C27B0,
//   3 IotCamera->0xF44336, 4 Terminal->0x4CAF50, 5 PowerStation->0xFFEB3B,
//   6 UPS->0x795548, 7 Firewall->0xE53935; out-of-band -> 0x888888
//
// getSecurityColor:
//   0 Open->0x00ff00, 1 Weak->0xffff00, 2 Medium->0xff9800,
//   3 Strong->0xff0000; out-of-band -> 0x888888

function oracleDeviceColor(dt) {
  if (dt === 0) return 0x2196F3;
  if (dt === 1) return 0xFF9800;
  if (dt === 2) return 0x9C27B0;
  if (dt === 3) return 0xF44336;
  if (dt === 4) return 0x4CAF50;
  if (dt === 5) return 0xFFEB3B;
  if (dt === 6) return 0x795548;
  if (dt === 7) return 0xE53935;
  return 0x888888;
}

function oracleSecurityColor(level) {
  if (level === 0) return 0x00ff00;
  if (level === 1) return 0xffff00;
  if (level === 2) return 0xff9800;
  if (level === 3) return 0xff0000;
  return 0x888888;
}

export default {
  affine: "DeviceCaps.affine",
  cases: [
    {
      name: "device_color over dt [-1..9] (all 8 valid types + OOB boundaries)",
      export: "device_color",
      args: [{ values: [-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9] }],
      oracle: (dt) => oracleDeviceColor(dt) | 0,
    },
    {
      name: "security_color over level [-1..5] (all 4 valid levels + OOB boundaries)",
      export: "security_color",
      args: [{ values: [-1, 0, 1, 2, 3, 4, 5] }],
      oracle: (level) => oracleSecurityColor(level) | 0,
    },
  ],
};
