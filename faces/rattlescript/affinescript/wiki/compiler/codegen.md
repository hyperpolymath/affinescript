# Code Generation

The AffineScript code generator emits WebAssembly from the typed and verified AST.

## Overview

**File**: `lib/codegen.ml` (planned)
**Target**: WebAssembly (WASM)

## Pipeline

```
Typed AST
    │
    ▼
┌─────────────┐
│ Monomorphize│  Specialize generics
└──────┬──────┘
       │
       ▼
┌─────────────┐
│   Lower     │  Convert to IR (ANF/CPS)
└──────┬──────┘
       │
       ▼
┌─────────────┐
│  Closures   │  Convert closures to structs
└──────┬──────┘
       │
       ▼
┌─────────────┐
│  Effects    │  Compile effect handlers
└──────┬──────┘
       │
       ▼
┌─────────────┐
│  Optimize   │  IR-level optimizations
└──────┬──────┘
       │
       ▼
┌─────────────┐
│  Emit WASM  │  Generate WebAssembly
└──────┬──────┘
       │
       ▼
   .wasm file
```

## Intermediate Representation

### ANF (A-Normal Form)

All intermediate results are named:

```ocaml
type ir_expr =
  | IR_Lit of literal
  | IR_Var of var
  | IR_Let of var * ir_val * ir_expr
  | IR_LetRec of (var * ir_val) list * ir_expr
  | IR_App of var * var list
  | IR_If of var * ir_expr * ir_expr
  | IR_Switch of var * (pattern * ir_expr) list
  | IR_Return of var
  | IR_Halt

and ir_val =
  | IV_Lit of literal
  | IV_Var of var
  | IV_Prim of prim_op * var list
  | IV_Closure of var list * ir_expr  (* free vars, body *)
  | IV_Record of (string * var) list
  | IV_Project of var * string
  | IV_Construct of ctor * var list
```

### Conversion to ANF

```ocaml
let rec to_anf (expr : typed_expr) (k : var -> ir_expr) : ir_expr =
  match expr.kind with
  | TE_Lit lit ->
      let v = fresh_var () in
      IR_Let (v, IV_Lit lit, k v)

  | TE_Var x ->
      k x

  | TE_Binary (e1, op, e2) ->
      to_anf e1 (fun v1 ->
        to_anf e2 (fun v2 ->
          let v = fresh_var () in
          IR_Let (v, IV_Prim (op, [v1; v2]), k v)))

  | TE_App (fn, args) ->
      to_anf fn (fun fn_v ->
        to_anf_list args (fun arg_vs ->
          let v = fresh_var () in
          IR_Let (v, IR_App (fn_v, arg_vs), k v)))

  | TE_Lambda (params, body) ->
      let free = free_vars expr in
      let body_anf = to_anf body (fun v -> IR_Return v) in
      let v = fresh_var () in
      IR_Let (v, IV_Closure (free, body_anf), k v)

  | TE_If (cond, then_, else_) ->
      to_anf cond (fun cond_v ->
        let result = fresh_var () in
        let then_anf = to_anf then_ (fun v -> IR_Let (result, IV_Var v, k result)) in
        let else_anf = to_anf else_ (fun v -> IR_Let (result, IV_Var v, k result)) in
        IR_If (cond_v, then_anf, else_anf))

  | _ -> (* more cases *)
```

## Closure Conversion

Closures become structs containing captured variables:

```ocaml
(* Before closure conversion *)
let add = |x| |y| x + y
let add5 = add(5)
add5(3)  // 8

(* After closure conversion *)
struct Closure_add {
  code: fn(env: &Self, y: Int) -> Int,
}

struct Closure_add_inner {
  x: Int,
  code: fn(env: &Self, y: Int) -> Int,
}

fn add_outer(env: &Closure_add, x: Int) -> Closure_add_inner {
  Closure_add_inner { x, code: add_inner }
}

fn add_inner(env: &Closure_add_inner, y: Int) -> Int {
  env.x + y
}
```

Implementation:

```ocaml
let convert_closure (free_vars : var list) (params : var list) (body : ir_expr) : ir_val * func_def =
  let env_type = make_struct_type free_vars in
  let env_param = fresh_var "env" in
  let body' = substitute_free_vars body env_param free_vars in
  let func = {
    name = fresh_func_name ();
    params = env_param :: params;
    body = body';
  } in
  let closure_val = IV_Record [
    ("code", func.name);
    (* captured variables... *)
  ] @ List.map (fun v -> (var_name v, v)) free_vars in
  (closure_val, func)
```

## Effect Compilation

Effects are compiled using CPS (Continuation-Passing Style) or evidence passing:

### CPS Transformation

```ocaml
(* Before *)
fn get_and_double() -{State[Int]}-> Int {
  let x = get()
  x * 2
}

(* After CPS *)
fn get_and_double(cont: (Int) -> Result, handlers: Handlers) -> Result {
  handlers.state.get(|x| {
    cont(x * 2)
  })
}
```

### Evidence Passing

```ocaml
(* Before *)
fn program() -{IO, State[Int]}-> Unit {
  let x = get()
  print(show(x))
}

(* After evidence passing *)
fn program(io_impl: IO_Impl, state_impl: State_Impl[Int]) -> Unit {
  let x = state_impl.get()
  io_impl.print(show(x))
}
```

## WebAssembly Emission

### Type Mapping

| AffineScript | WASM |
|--------------|------|
| `Int` | `i32` or `i64` |
| `Float64` | `f64` |
| `Bool` | `i32` (0 or 1) |
| `Unit` | (none) |
| `String` | `i32` (pointer) |
| `Record` | `i32` (pointer) |
| `Closure` | `i32` (pointer) |

### WASM Generation

```ocaml
type wasm_instr =
  | I32_const of int32
  | I64_const of int64
  | F64_const of float
  | Local_get of int
  | Local_set of int
  | Global_get of int
  | Global_set of int
  | I32_add | I32_sub | I32_mul | I32_div_s
  | I64_add | I64_sub | I64_mul | I64_div_s
  | F64_add | F64_sub | F64_mul | F64_div
  | I32_eq | I32_ne | I32_lt_s | I32_gt_s | I32_le_s | I32_ge_s
  | If of block_type * wasm_instr list * wasm_instr list
  | Block of block_type * wasm_instr list
  | Loop of block_type * wasm_instr list
  | Br of int
  | Br_if of int
  | Call of func_idx
  | Call_indirect of type_idx
  | Return
  | Drop
  | Memory_grow
  | I32_load | I32_store
  | I64_load | I64_store
  | F64_load | F64_store

let rec emit_expr (expr : ir_expr) : wasm_instr list =
  match expr with
  | IR_Lit (Lit_Int n) ->
      [I32_const (Int32.of_int n)]

  | IR_Lit (Lit_Float f) ->
      [F64_const f]

  | IR_Var v ->
      [Local_get (var_index v)]

  | IR_Let (v, IV_Prim (Op_Add, [v1; v2]), body) ->
      [Local_get (var_index v1);
       Local_get (var_index v2);
       I32_add;
       Local_set (var_index v)] @
      emit_expr body

  | IR_If (cond, then_, else_) ->
      [Local_get (var_index cond);
       If (BlockType_Val I32,
           emit_expr then_,
           emit_expr else_)]

  | IR_App (fn, args) ->
      List.concat_map (fun a -> [Local_get (var_index a)]) args @
      [Call (func_index fn)]

  | IR_Return v ->
      [Local_get (var_index v); Return]

  | _ -> (* more cases *)
```

### Memory Layout

```
┌─────────────────────────────────────────────────┐
│ WASM Linear Memory                              │
├─────────────────────────────────────────────────┤
│ 0x00000000: Stack (grows down)                  │
│                                                 │
│ 0x00010000: Heap start                          │
│             ┌─────────────────────┐             │
│             │ Allocation header   │             │
│             │ - size: u32         │             │
│             │ - type tag: u32     │             │
│             ├─────────────────────┤             │
│             │ Object data         │             │
│             └─────────────────────┘             │
│                                                 │
│ 0xXXXXXXXX: Static data (strings, etc.)        │
└─────────────────────────────────────────────────┘
```

### Simple Allocator

```ocaml
let emit_alloc (size : int) : wasm_instr list =
  [
    (* Get heap pointer *)
    Global_get heap_ptr_idx;
    (* Save for return *)
    Local_tee temp_idx;
    (* Bump heap pointer *)
    I32_const (Int32.of_int size);
    I32_add;
    Global_set heap_ptr_idx;
    (* Return old pointer *)
    Local_get temp_idx;
  ]
```

## Optimizations

### Constant Folding

```ocaml
let rec fold_constants (expr : ir_expr) : ir_expr =
  match expr with
  | IR_Let (v, IV_Prim (Op_Add, [v1; v2]), body) ->
      (match (lookup v1, lookup v2) with
       | (Some (IV_Lit (Lit_Int n1)), Some (IV_Lit (Lit_Int n2))) ->
           IR_Let (v, IV_Lit (Lit_Int (n1 + n2)), fold_constants body)
       | _ -> IR_Let (v, IV_Prim (Op_Add, [v1; v2]), fold_constants body))
  | _ -> (* recurse *)
```

### Dead Code Elimination

```ocaml
let eliminate_dead_code (funcs : func_def list) : func_def list =
  let used = compute_used_vars funcs in
  List.map (fun f ->
    { f with body = filter_dead f.body used }
  ) funcs
```

### Inlining

```ocaml
let should_inline (func : func_def) : bool =
  func.size < inline_threshold && not func.is_recursive

let inline_call (call_site : ir_expr) (func : func_def) : ir_expr =
  substitute func.body func.params (get_args call_site)
```

### Tail Call Optimization

```ocaml
let rec is_tail_call (expr : ir_expr) (func_name : string) : bool =
  match expr with
  | IR_Return (IR_App (fn, _)) when fn = func_name -> true
  | IR_If (_, t, e) -> is_tail_call t func_name && is_tail_call e func_name
  | IR_Let (_, _, body) -> is_tail_call body func_name
  | _ -> false

let optimize_tail_calls (func : func_def) : func_def =
  if is_tail_recursive func then
    convert_to_loop func
  else
    func
```

## Output Formats

### WASM Text Format (.wat)

```wat
(module
  (memory 1)
  (global $heap_ptr (mut i32) (i32.const 65536))

  (func $add (param $x i32) (param $y i32) (result i32)
    local.get $x
    local.get $y
    i32.add
  )

  (func $main (result i32)
    i32.const 1
    i32.const 2
    call $add
  )

  (export "main" (func $main))
)
```

### WASM Binary Format (.wasm)

```ocaml
let emit_wasm_binary (module_ : wasm_module) : bytes =
  let buf = Buffer.create 1024 in

  (* Magic number *)
  Buffer.add_string buf "\x00asm";

  (* Version *)
  emit_u32_leb128 buf 1;

  (* Type section *)
  emit_section buf 1 (emit_types module_.types);

  (* Function section *)
  emit_section buf 3 (emit_func_types module_.funcs);

  (* Memory section *)
  emit_section buf 5 (emit_memory module_.memory);

  (* Export section *)
  emit_section buf 7 (emit_exports module_.exports);

  (* Code section *)
  emit_section buf 10 (emit_code module_.funcs);

  Buffer.to_bytes buf
```

---

## See Also

- [Architecture](architecture.md) - Compiler overview
- [Borrow Checker](borrow-checker.md) - Previous phase
- [WASM Spec](https://webassembly.github.io/spec/) - WebAssembly specification
