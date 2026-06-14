// SPDX-License-Identifier: MPL-2.0
export default {
  affine: "string_float_demo.affine",
  cases: [
    { export: "classify", args: [{ strings: ["scan", "exit", "foo", "sca", "scann", ""] }],
      oracle: (s) => (s === "scan" ? 1 : s === "exit" ? 2 : 0) },
    { export: "str_cmp", args: [{ strings: ["a", "ab", "b", ""] }, { strings: ["a", "ab", "b", ""] }],
      oracle: (a, b) => (a < b ? -1 : a === b ? 0 : 1) },
    { export: "band_of", args: [{ values: [0.0, 5.5, 9.999, 10.0, 50.0, 99.9, 100.0, 250.0] }],
      oracle: (x) => (x < 10 ? 0 : x < 100 ? 1 : 2) },
  ],
};
