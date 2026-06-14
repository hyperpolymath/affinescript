// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for InstructionCoprocessor.affine (idaptik VM opcode
// arity-and-reversibility taxonomy; scalar i32 ABI). The oracle independently
// re-derives the register-arithmetic-first 0..21 opcode band, the per-opcode
// operand arity, the tier classifier, the self-inverse predicate, and the
// structural inverse pairing in plain JS so a codegen regression surfaces as a
// differential mismatch. This re-implementation is deliberately NOT a call into
// the wasm -- it mirrors InstructionCoprocessorBridge.js's OPCODE table by hand.

const validOp = (op) => op >= 0 && op <= 21;

// Per-opcode operand counts, indexed by opcode 0..21 (the register-arith-first
// encoding: ADD=0 .. RECV=21). Independent transcription of the header table.
const ARITY = [
  2, // 0  ADD
  2, // 1  SUB
  1, // 2  NEGATE
  3, // 3  MUL
  3, // 4  AND
  3, // 5  OR
  2, // 6  XOR
  1, // 7  FLIP
  2, // 8  SWAP
  1, // 9  ROL
  1, // 10 ROR
  0, // 11 NOOP
  4, // 12 DIV
  3, // 13 IF_ZERO
  3, // 14 IF_POS
  1, // 15 PUSH
  1, // 16 POP
  2, // 17 LOAD
  2, // 18 STORE
  1, // 19 CALL
  2, // 20 SEND
  2, // 21 RECV
];

const tierOf = (op) => {
  if (op < 0) return -1;
  if (op <= 12) return 0;
  if (op <= 14) return 1;
  if (op <= 18) return 2;
  if (op === 19) return 3;
  if (op <= 21) return 4;
  return -1;
};

// NEGATE(2), XOR(6), FLIP(7), SWAP(8), NOOP(11) are their own inverse.
const selfInverseSet = new Set([2, 6, 7, 8, 11]);
const isSelfInverse = (op) => (!validOp(op) ? -1 : (selfInverseSet.has(op) ? 1 : 0));

const invertOpcode = (op) => {
  if (!validOp(op)) return -1;
  if (op === 0) return 1; // ADD -> SUB
  if (op === 1) return 0; // SUB -> ADD
  return op;
};

export default {
  affine: "InstructionCoprocessor.affine",
  cases: [
    {
      name: "opcode_count()",
      export: "opcode_count",
      args: [],
      oracle: () => 22,
    },
    {
      name: "is_valid_opcode over [-3..25]",
      export: "is_valid_opcode",
      args: [[-3, 25]],
      oracle: (op) => (validOp(op) ? 1 : 0),
    },
    {
      name: "opcode_arity over [-3..25]",
      export: "opcode_arity",
      args: [[-3, 25]],
      oracle: (op) => (validOp(op) ? ARITY[op] : -1),
    },
    {
      name: "opcode_tier over [-3..25]",
      export: "opcode_tier",
      args: [[-3, 25]],
      oracle: (op) => tierOf(op),
    },
    {
      name: "is_self_inverse over [-3..25]",
      export: "is_self_inverse",
      args: [[-3, 25]],
      oracle: (op) => isSelfInverse(op),
    },
    {
      name: "invert_opcode over [-3..25]",
      export: "invert_opcode",
      args: [[-3, 25]],
      oracle: (op) => invertOpcode(op),
    },
    {
      name: "invert_round_trip over [-3..25]",
      export: "invert_round_trip",
      args: [[-3, 25]],
      oracle: (op) => (validOp(op) ? invertOpcode(invertOpcode(op)) : -1),
    },
  ],
};
