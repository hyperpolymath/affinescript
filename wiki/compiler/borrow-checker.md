# Borrow Checker

The AffineScript borrow checker verifies ownership and borrowing rules at compile time.

## Overview

**File**: `lib/borrow.ml` (planned)
**Algorithm**: Dataflow analysis with non-lexical lifetimes

## Goals

1. **No use-after-move**: Cannot use values after ownership transfer
2. **No data races**: No simultaneous mutable access
3. **No dangling references**: References always point to valid data
4. **Linearity enforcement**: Linear types used exactly once

## Core Concepts

### Places

A **place** represents a memory location:

```ocaml
type place = {
  base: local_var;           (* The root variable *)
  projections: projection list;  (* Path to the location *)
}

type projection =
  | Proj_Field of string     (* .field *)
  | Proj_Index of int        (* [index] *)
  | Proj_Deref                (* * (dereference) *)
```

Examples:
- `x` → `{ base = x; projections = [] }`
- `x.field` → `{ base = x; projections = [Proj_Field "field"] }`
- `(*p).data[0]` → `{ base = p; projections = [Proj_Deref; Proj_Field "data"; Proj_Index 0] }`

### Loans

A **loan** is an active borrow:

```ocaml
type loan_kind = Shared | Mutable

type loan = {
  id: loan_id;
  place: place;              (* What's borrowed *)
  kind: loan_kind;           (* Shared or mutable *)
  region: region;            (* How long it's valid *)
}
```

### Regions (Lifetimes)

A **region** is a span of code where a reference is valid:

```ocaml
type region =
  | Region_Var of int        (* Named lifetime 'a *)
  | Region_Static            (* 'static *)
  | Region_Scope of scope_id (* Lexical scope *)
```

## Analysis Phases

### Phase 1: Build Control Flow Graph

```ocaml
type cfg_node =
  | CFG_Statement of stmt
  | CFG_Branch of { cond: expr; true_: cfg_node; false_: cfg_node }
  | CFG_Loop of { body: cfg_node }
  | CFG_Return of expr option
  | CFG_Drop of place

type cfg = {
  entry: cfg_node;
  nodes: cfg_node list;
  edges: (cfg_node * cfg_node) list;
}
```

### Phase 2: Compute Liveness

Determine where each variable is "live" (might be used later):

```ocaml
type liveness = {
  live_in: PlaceSet.t;   (* Live at entry *)
  live_out: PlaceSet.t;  (* Live at exit *)
}

let compute_liveness (cfg : cfg) : (cfg_node, liveness) Map.t =
  (* Backward dataflow analysis *)
  let rec iterate worklist state =
    match worklist with
    | [] -> state
    | node :: rest ->
        let successors = get_successors cfg node in
        let live_out = union_map (fun s -> (Map.find s state).live_in) successors in
        let gen = uses_of node in
        let kill = defs_of node in
        let live_in = PlaceSet.union gen (PlaceSet.diff live_out kill) in
        let old = Map.find node state in
        if PlaceSet.equal live_in old.live_in then
          iterate rest state
        else
          let state' = Map.add node { live_in; live_out } state in
          let preds = get_predecessors cfg node in
          iterate (preds @ rest) state'
  in
  iterate (all_nodes cfg) initial_state
```

### Phase 3: Track Ownership

```ocaml
type ownership_state = {
  owned: PlaceSet.t;         (* Currently owned *)
  moved: PlaceSet.t;         (* Has been moved *)
  borrowed: loan list;       (* Active borrows *)
}

let rec check_ownership (state : ownership_state) (node : cfg_node) : ownership_state =
  match node with
  | CFG_Statement stmt ->
      check_stmt state stmt

  | CFG_Branch { cond; true_; false_ } ->
      let state' = check_expr state cond in
      let state_t = check_ownership state' true_ in
      let state_f = check_ownership state' false_ in
      merge_states state_t state_f

  | CFG_Return (Some expr) ->
      check_expr_moved state expr

  | CFG_Drop place ->
      if PlaceSet.mem place state.moved then
        ()  (* Already moved, nothing to drop *)
      else
        check_can_drop state place;
      { state with owned = PlaceSet.remove place state.owned }

  | _ -> state
```

### Phase 4: Verify Borrows

```ocaml
let check_borrow (state : ownership_state) (place : place) (kind : loan_kind) : loan =
  (* Check place is not moved *)
  if PlaceSet.mem place state.moved then
    error (Borrow_of_moved place);

  (* Check no conflicting borrows *)
  List.iter (fun loan ->
    if places_conflict place loan.place then
      match (kind, loan.kind) with
      | (Mutable, _) | (_, Mutable) ->
          error (Conflicting_borrow (place, loan))
      | (Shared, Shared) ->
          ()  (* OK: multiple shared borrows *)
  ) state.borrowed;

  (* Create new loan *)
  let loan = { id = fresh_id (); place; kind; region = current_region () } in
  loan

let check_use (state : ownership_state) (place : place) : unit =
  (* Check place is not moved *)
  if PlaceSet.mem place state.moved then
    error (Use_after_move place);

  (* Check any loans to this place are still valid *)
  List.iter (fun loan ->
    if places_conflict place loan.place && not (region_valid loan.region) then
      error (Dangling_reference loan)
  ) state.borrowed
```

### Phase 5: Insert Drops

```ocaml
let insert_drops (cfg : cfg) (liveness : liveness_info) : cfg =
  let transform_node node =
    let live_before = liveness_at node in
    let live_after = liveness_after node in
    let to_drop = PlaceSet.diff live_before live_after in
    let drops = PlaceSet.fold (fun p acc ->
      CFG_Drop p :: acc
    ) to_drop [] in
    sequence_nodes (node :: drops)
  in
  map_cfg transform_node cfg
```

## Non-Lexical Lifetimes (NLL)

Traditional borrow checkers use lexical scopes:

```affine
let mut x = 5
let r = &x      // Borrow starts
println(r)
x = 6           // ERROR in lexical: r still in scope

// With NLL:
let mut x = 5
let r = &x      // Borrow starts
println(r)      // Last use of r
x = 6           // OK: r's region ended at last use
```

Implementation:

```ocaml
(* Compute region constraints from usage *)
let compute_regions (cfg : cfg) (loans : loan list) : region_constraints =
  let constraints = ref [] in

  (* For each loan, find its last use *)
  List.iter (fun loan ->
    let last_use = find_last_use cfg loan in
    constraints := (loan.region, Region_Point last_use) :: !constraints
  ) loans;

  (* Add outlives constraints from function signatures *)
  add_signature_constraints cfg constraints;

  !constraints

(* Solve region constraints *)
let solve_regions (constraints : region_constraints) : region_solution =
  (* Fixed-point iteration *)
  let rec iterate solution =
    let changed = ref false in
    List.iter (fun (r1, r2) ->
      if not (region_outlives solution r1 r2) then begin
        extend_region solution r1 r2;
        changed := true
      end
    ) constraints;
    if !changed then iterate solution else solution
  in
  iterate empty_solution
```

## Linearity Checking

For linear types (used exactly once):

```ocaml
let check_linearity (ctx : context) (expr : typed_expr) : unit =
  let linear_vars = find_linear_vars ctx expr in

  List.iter (fun var ->
    let uses = count_uses expr var in
    match uses with
    | 0 -> error (Linear_unused var)
    | 1 -> ()
    | n -> error (Linear_multiple_use (var, n))
  ) linear_vars

let count_uses (expr : typed_expr) (var : var) : int =
  let count = ref 0 in
  visit_expr (fun e ->
    match e.kind with
    | TE_Var v when v = var -> incr count
    | _ -> ()
  ) expr;
  !count
```

## Error Messages

```ocaml
let format_borrow_error (err : borrow_error) : diagnostic =
  match err with
  | Use_after_move place ->
      {
        code = E0500;
        message = sprintf "use of moved value: `%s`" (show_place place);
        labels = [
          (place.span, "value used here after move");
          (move_location place, "value moved here");
        ];
        notes = [];
        help = Some "consider using `clone()` to copy the value";
      }

  | Conflicting_borrow (place, loan) ->
      {
        code = E0502;
        message = sprintf "cannot borrow `%s` as mutable because it is already borrowed"
          (show_place place);
        labels = [
          (place.span, "mutable borrow occurs here");
          (loan.span, sprintf "%s borrow occurs here"
            (if loan.kind = Shared then "immutable" else "mutable"));
        ];
        notes = [];
        help = None;
      }

  | Dangling_reference loan ->
      {
        code = E0597;
        message = sprintf "`%s` does not live long enough" (show_place loan.place);
        labels = [
          (loan.span, "borrowed value does not live long enough");
          (scope_end loan.region, "borrowed value dropped here");
        ];
        notes = [];
        help = None;
      }

  | Linear_unused var ->
      {
        code = E0550;
        message = sprintf "linear value `%s` not used" var;
        labels = [(var_span var, "linear value declared here but never used")];
        notes = ["linear types must be used exactly once"];
        help = Some "explicitly drop with `drop(value)` if unused";
      }
```

## Testing

```ocaml
(* Should compile *)
let test_valid_borrow () =
  check {|
    let x = String::from("hello")
    let r = &x
    println(r)
  |}

(* Should error: use after move *)
let test_use_after_move () =
  expect_error E0500 {|
    let x = String::from("hello")
    let y = x       // Move
    println(x)      // Error!
  |}

(* Should error: conflicting borrow *)
let test_conflicting_borrow () =
  expect_error E0502 {|
    let mut x = 5
    let r1 = &x
    let r2 = &mut x  // Error!
    println(r1)
  |}

(* Should compile with NLL *)
let test_nll () =
  check {|
    let mut x = 5
    let r = &x
    println(r)      // Last use of r
    x = 6           // OK with NLL
  |}
```

---

## See Also

- [Architecture](architecture.md) - Compiler overview
- [Type Checker](type-checker.md) - Previous phase
- [Code Generation](codegen.md) - Next phase
- [Ownership](../language-reference/ownership.md) - Language reference
