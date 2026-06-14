// SPDX-License-Identifier: MPL-2.0
export default {
  affine: "string_float_demo.affine",
  cases: [
    { export: "classify", args: [{ strings: ["scan", "exit", "foo", "sca", "scann", ""] }],
      oracle: (s) => (s === "scan" ? 1 : s === "exit" ? 2 : 0) },
    { export: "str_cmp", args: [{ strings: ["a", "ab", "b", ""] }, { strings: ["a", "ab", "b", ""] }],
      oracle: (a, b) => (a < b ? -1 : a === b ? 0 : 1) },
  ],
};
