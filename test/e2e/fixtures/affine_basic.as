// SPDX-License-Identifier: PMPL-1.0-or-later
// End-to-end test: basic functions for quantity checking
// Tests: functions that can be quantity-checked after parsing
// Note: quantity annotations (0/1/omega) are not yet in the lexer,
// so we test the quantity checker at the OCaml API level separately.

fn use_once(x: Int) -> Int = x + 1;

fn use_many(x: Int) -> Int = x + x;

fn no_use(x: Int) -> Int = 42;
