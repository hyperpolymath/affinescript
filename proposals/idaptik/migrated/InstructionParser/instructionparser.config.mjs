// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for InstructionParser.affine (idaptik VM opcode-arity
// validation kernel; scalar i32 ABI). The oracle re-derives the per-opcode
// operand-count rules straight from InstructionParser.res's `if argCount == N`
// / `if argCount >= 3` branches in plain JS, against the canonical 0..22
// VmInstruction opcode band. A codegen regression surfaces as a differential
// mismatch.
//
// Oracle re-derivation, opcode -> (.res arity rule), independent of the .affine:
//   0 NOOP    == 0      (res:70)        13 IF_ZERO >= 3     (res:125)
//   1 ADD     == 2      (res:34)        14 IF_POS  >= 3     (res:138)
//   2 SUB     == 2      (res:40)        15 LOOP    (none -- programmatic only)
//   3 NEGATE  == 1      (res:58)        16 PUSH    == 1     (res:153)
//   4 SWAP    == 2      (res:46)        17 POP     == 1     (res:159)
//   5 FLIP    == 1      (res:64)        18 LOAD    == 2     (res:165)
//   6 ROL     1 or 2    (res:76,78)     19 STORE   == 2     (res:174)
//   7 ROR     1 or 2    (res:87,89)     20 CALL    == 1     (res:185)
//   8 AND     == 3      (res:98)        21 SEND    == 2     (res:202)
//   9 OR      == 3      (res:104)       22 RECV    == 2     (res:207)
//  10 XOR     == 2      (res:52)
//  11 MUL     == 3      (res:110)
//  12 DIV     == 4      (res:116)
// LOOP (15) and anything outside 0..22: no text command yields it (res header +
// res:214 "Unknown instruction").

// Fixed required count per opcode, or null where the op is variable/unparseable.
// (Keyed by the canonical VmInstruction opcode integer.)
const FIXED = {
  0: 0, 1: 2, 2: 2, 3: 1, 4: 2, 5: 1, 8: 3, 9: 3, 10: 2, 11: 3, 12: 4,
  16: 1, 17: 1, 18: 2, 19: 2, 20: 1, 21: 2, 22: 2,
};
const validOp = (op) => Number.isInteger(op) && op >= 0 && op <= 22;

const minArgCount = (op) => {
  if (!validOp(op)) return -1;
  if (op === 6 || op === 7) return 1; // ROL/ROR: optional bits
  if (op === 13 || op === 14) return 3; // IF_ZERO/IF_POS: >= 3
  if (op === 15) return -1; // LOOP: not text-parseable
  return op in FIXED ? FIXED[op] : -1;
};
const maxArgCount = (op) => {
  if (op === 6 || op === 7) return 2; // ROL/ROR cap at 2
  if (op === 13 || op === 14) return -1; // IF_ZERO/IF_POS: unbounded body
  return minArgCount(op); // fixed (or -1 for LOOP / out-of-band)
};
const acceptsArgCount = (op, n) => {
  if (n < 0) return 0;
  const lo = minArgCount(op);
  if (lo < 0) return 0;
  const hi = maxArgCount(op);
  if (hi < 0) return n >= lo ? 1 : 0; // unbounded tail
  return n >= lo && n <= hi ? 1 : 0;
};
const isParseableOpcode = (op) => (minArgCount(op) < 0 ? 0 : 1);

// Sweep the full opcode band plus a couple out-of-band, and arg counts -1..6
// (so the >= 3 / fixed / ROL-ROR-range / negative-count cases all fire).
const OPCODE = [-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 25];

export default {
  affine: "InstructionParser.affine",
  cases: [
    {
      name: "min_arg_count over opcode band + out-of-band",
      export: "min_arg_count",
      args: [{ values: OPCODE }],
      oracle: minArgCount,
    },
    {
      name: "max_arg_count over opcode band + out-of-band",
      export: "max_arg_count",
      args: [{ values: OPCODE }],
      oracle: maxArgCount,
    },
    {
      name: "is_parseable_opcode over opcode band + out-of-band",
      export: "is_parseable_opcode",
      args: [{ values: OPCODE }],
      oracle: isParseableOpcode,
    },
    {
      name: "accepts_arg_count over opcode band x argCount [-1..6]",
      export: "accepts_arg_count",
      args: [{ values: OPCODE }, [-1, 6]],
      oracle: acceptsArgCount,
    },
  ],
};
