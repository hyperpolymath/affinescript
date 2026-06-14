// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for PortScanner.affine (idaptik port-scan classifier;
// scalar i32 ABI). The oracles re-derive the four scan figures independently
// from PortScanner.res in plain JS, so a codegen regression surfaces as a
// differential mismatch.
//
// Device-type ids are PortScannerCoprocessorBridge.js DEVICE_TYPE order:
//   0 Laptop 1 Router 2 Server 3 IotCamera 4 Terminal 5 PowerStation 6 UPS 7 Firewall
// The open-port counts are the lengths of getOpenPorts' filtered arrays in the
// .res: Server/Firewall map [ssh,http,https] (3); Router [ssh,http] (2);
// IotCamera [http,rtsp] (2); PowerStation/UPS [snmp,http] (2); Laptop/Terminal
// [ssh] (1). Anything off-band -> 0 (host saw nothing open).
//
// Security-level ids are DeviceTypes.securityLevel order (= bridge order):
//   0 Open 1 Weak 2 Medium 3 Strong
// filtered: 0/100/500/2000; delay(ms): 50/200/500/1500. Off-band -> 0.

// Independent reimplementation: open ports per bridge device id (NOT the kernel).
function openPorts(dev) {
  switch (dev) {
    case 0: return 1; // Laptop
    case 1: return 2; // Router
    case 2: return 3; // Server
    case 3: return 2; // IotCamera (http + rtsp)
    case 4: return 1; // Terminal
    case 5: return 2; // PowerStation (snmp + http)
    case 6: return 2; // UPS (snmp + http)
    case 7: return 3; // Firewall
    default: return 0;
  }
}
function closedPorts(dev) {
  return 65535 - openPorts(dev);
}
function filteredPorts(sec) {
  switch (sec) {
    case 0: return 0;    // Open
    case 1: return 100;  // Weak
    case 2: return 500;  // Medium
    case 3: return 2000; // Strong
    default: return 0;
  }
}
function scanDelayMs(sec) {
  // .res Floats 50.0/200.0/500.0/1500.0 -> whole ms integers.
  switch (sec) {
    case 0: return 50;
    case 1: return 200;
    case 2: return 500;
    case 3: return 1500;
    default: return 0;
  }
}

export default {
  affine: "PortScanner.affine",
  cases: [
    {
      name: "open_port_count over device ids [-2..10]",
      export: "open_port_count",
      args: [[-2, 10]],
      oracle: (dev) => openPorts(dev),
    },
    {
      name: "closed_port_count over device ids [-2..10]",
      export: "closed_port_count",
      args: [[-2, 10]],
      oracle: (dev) => closedPorts(dev),
    },
    {
      name: "filtered_port_count over security ids [-2..6]",
      export: "filtered_port_count",
      args: [[-2, 6]],
      oracle: (sec) => filteredPorts(sec),
    },
    {
      name: "scan_delay_ms over security ids [-2..6]",
      export: "scan_delay_ms",
      args: [[-2, 6]],
      oracle: (sec) => scanDelayMs(sec),
    },
  ],
};
