// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for Coprocessor_IO.affine (idaptik virtual-filesystem
// index-and-byte core; scalar i32 ABI). Every oracle is an INDEPENDENT JS
// reimplementation derived from src/shared/Coprocessor_IO.res semantics (the
// 0<b<128 path-byte filter, the b&0x7F encode mask, the frame split, the
// big-endian stat pair, the integer compute metrics) -- NOT a copy of the .affine
// logic -- so the differential sweep is a genuine cross-check. The .res frames
// these inside async file-buffer handlers; here only the scalar result crosses.

const oPathByteValid = (b) => (b > 0 && b < 128 ? 1 : 0);
const oMaskAscii = (b) => (b & 127) | 0;
const oContentLen = (frameLen, nullIx) => {
  const n = frameLen - (nullIx + 1);
  return n < 0 ? 0 : n;
};
const oStatHi = (size) => ((size >> 8) & 255) | 0;
const oStatLo = (size) => (size & 255) | 0;
const oWriteCompute = (cl) => 50 + cl;
const oReadCompute = (cl) => 30 + cl;
const oListCompute = (n) => 20 + n * 5;

export default {
  affine: "Coprocessor_IO.affine",
  cases: [
    {
      name: "path_byte_valid over [-5..200]",
      export: "path_byte_valid",
      args: [[-5, 200]],
      oracle: oPathByteValid,
    },
    {
      name: "mask_ascii over [0..300]",
      export: "mask_ascii",
      args: [[0, 300]],
      oracle: oMaskAscii,
    },
    {
      name: "content_len over frame_len in {0,1,10,64,256}, null_ix in {-1,0,5,9,63,255}",
      export: "content_len",
      args: [{ values: [0, 1, 10, 64, 256] }, { values: [-1, 0, 5, 9, 63, 255] }],
      oracle: oContentLen,
    },
    {
      name: "stat_hi over size in [0..70000] sampled by range",
      export: "stat_hi",
      args: [{ values: [0, 1, 255, 256, 257, 4096, 65535, 65536, 70000] }],
      oracle: oStatHi,
    },
    {
      name: "stat_lo over size in [0..70000] sampled",
      export: "stat_lo",
      args: [{ values: [0, 1, 255, 256, 257, 4096, 65535, 65536, 70000] }],
      oracle: oStatLo,
    },
    {
      name: "write_compute over content_len in [0..500]",
      export: "write_compute",
      args: [[0, 500]],
      oracle: oWriteCompute,
    },
    {
      name: "read_compute over content_len in [0..500]",
      export: "read_compute",
      args: [[0, 500]],
      oracle: oReadCompute,
    },
    {
      name: "list_compute over live_path_count in [0..200]",
      export: "list_compute",
      args: [[0, 200]],
      oracle: oListCompute,
    },
  ],
};
