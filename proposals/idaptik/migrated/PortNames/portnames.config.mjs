// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for PortNames.affine (idaptik coprocessor-domain port
// taxonomy = portnames.wasm; scalar i32 ABI). Oracle re-derives the 0..9 band
// the PortNamesCoprocessor.js bridge contract expects.
const valid = (id) => id >= 0 && id <= 9;
export default {
  affine: "PortNames.affine",
  cases: [
    { name: "coprocessor_domain_count()", export: "coprocessor_domain_count", args: [], oracle: () => 10 },
    { name: "is_coprocessor_port over [-3..13]", export: "is_coprocessor_port", args: [[-3, 13]], oracle: (id) => (valid(id) ? 1 : 0) },
    { name: "domain_index_of_port over [-3..13]", export: "domain_index_of_port", args: [[-3, 13]], oracle: (id) => (valid(id) ? id : -1) },
    { name: "port_of_domain_index over [-3..13]", export: "port_of_domain_index", args: [[-3, 13]], oracle: (idx) => (valid(idx) ? idx : -1) },
  ],
};
