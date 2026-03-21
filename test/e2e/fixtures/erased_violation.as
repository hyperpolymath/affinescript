// SPDX-License-Identifier: PMPL-1.0-or-later
// End-to-end test: function using variable (for erased check)
// Quantity checker should flag this if variable is erased (QZero).
// Since quantity annotations aren't in the lexer, we test the quantity
// checker programmatically.

fn use_var(x: Int) -> Int = x + 1;
