// SPDX-License-Identifier: PMPL-1.0-or-later
// End-to-end test: arithmetic expressions
// Tests: parsing, constant folding, WASM codegen, Julia codegen

fn add(a: Int, b: Int) -> Int = a + b;

fn mul(x: Int, y: Int) -> Int = x * y;

fn constant_fold() -> Int = 2 + 3 * 4;

fn nested_arith(a: Int, b: Int, c: Int) -> Int = (a + b) * c - a / b;

fn boolean_logic(x: Bool, y: Bool) -> Bool = x && y || !x;

fn comparisons(a: Int, b: Int) -> Bool = a < b && b >= a;
