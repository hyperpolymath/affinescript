// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for LocationData.affine (idaptik world-map attribute
// brain; scalar i32 ABI). The oracle re-derives each function from the ORIGINAL
// LocationData.res allLocations table (NOT from the .affine) so a codegen
// regression surfaces as a differential mismatch.
//
// The oracle encodes the .res allLocations rows directly: each row's
// `environment` string -> the binding's ENVIRONMENT ordinal (FIELD=0, CITY=1,
// DATACENTER=2, LAB=3) and each row's `devicePositions` array -> its length.
// id->index is the host's job (it owns the id strings), so the brain's domain
// is the 0..12 row band.

// The 13 rows of LocationData.res allLocations, in declared order:
//   id, environment string, devicePositions length.
const ENV = { field: 0, city: 1, datacenter: 2, lab: 3 };
const rows = [
  ["field", "field", 2],
  ["city", "city", 8],
  ["dmz", "datacenter", 4],
  ["security", "datacenter", 4],
  ["scada", "datacenter", 3],
  ["lab", "lab", 4],
  ["atlas", "datacenter", 3],
  ["nexus", "datacenter", 3],
  ["devhub", "datacenter", 2],
  ["rural-isp", "datacenter", 1],
  ["business-isp", "datacenter", 1],
  ["regional-isp", "datacenter", 1],
  ["backbone", "datacenter", 1],
];

const locationValid = (i) => (i >= 0 && i < rows.length ? 1 : 0);
const environmentKind = (i) => (locationValid(i) ? ENV[rows[i][1]] : -1);
const deviceCount = (i) => (locationValid(i) ? rows[i][2] : -1);

export default {
  affine: "LocationData.affine",
  cases: [
    {
      name: "location_count()",
      export: "location_count",
      args: [],
      oracle: () => rows.length,
    },
    {
      name: "location_valid over idx[-3..16]",
      export: "location_valid",
      args: [[-3, 16]],
      oracle: (i) => locationValid(i),
    },
    {
      name: "environment_kind over idx[-3..16]",
      export: "environment_kind",
      args: [[-3, 16]],
      oracle: (i) => environmentKind(i),
    },
    {
      name: "device_count over idx[-3..16]",
      export: "device_count",
      args: [[-3, 16]],
      oracle: (i) => deviceCount(i),
    },
  ],
};
