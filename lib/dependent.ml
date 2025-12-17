(** Dependent types for AffineScript - length-indexed vectors and refinements *)

open Types

(** Nat-level expressions for type indices *)
type nat_expr =
  | NLit of int                      (** Literal natural number *)
  | NVar of string                   (** Nat variable *)
  | NAdd of nat_expr * nat_expr      (** n + m *)
  | NSub of nat_expr * nat_expr      (** n - m (saturating at 0) *)
  | NMul of nat_expr * nat_expr      (** n * m *)
  | NMax of nat_expr * nat_expr      (** max(n, m) *)
  | NMin of nat_expr * nat_expr      (** min(n, m) *)
  | NLen of string                   (** Length of array variable *)
  | NSizeof of ty                    (** Size of type in bytes *)
[@@deriving show, eq]

(** Nat constraints *)
type nat_constraint =
  | NCEq of nat_expr * nat_expr      (** n = m *)
  | NCLt of nat_expr * nat_expr      (** n < m *)
  | NCLe of nat_expr * nat_expr      (** n <= m *)
  | NCAnd of nat_constraint * nat_constraint
  | NCOr of nat_constraint * nat_constraint
  | NCNot of nat_constraint
  | NCTrue
  | NCFalse
[@@deriving show, eq]

(** Dependent type context *)
type dep_context = {
  dc_nat_vars: (string, nat_expr option) Hashtbl.t;  (** name -> concrete value if known *)
  dc_constraints: nat_constraint list;
  dc_array_lengths: (string, nat_expr) Hashtbl.t;    (** array var -> length *)
}

let create_dep_context () = {
  dc_nat_vars = Hashtbl.create 16;
  dc_constraints = [];
  dc_array_lengths = Hashtbl.create 16;
}

(** Simplify a nat expression *)
let rec simplify_nat = function
  | NLit n -> NLit n
  | NVar v -> NVar v
  | NAdd (n1, n2) ->
      let n1' = simplify_nat n1 in
      let n2' = simplify_nat n2 in
      (match n1', n2' with
       | NLit a, NLit b -> NLit (a + b)
       | NLit 0, n | n, NLit 0 -> n
       | _ -> NAdd (n1', n2'))
  | NSub (n1, n2) ->
      let n1' = simplify_nat n1 in
      let n2' = simplify_nat n2 in
      (match n1', n2' with
       | NLit a, NLit b -> NLit (max 0 (a - b))
       | n, NLit 0 -> n
       | _ -> NSub (n1', n2'))
  | NMul (n1, n2) ->
      let n1' = simplify_nat n1 in
      let n2' = simplify_nat n2 in
      (match n1', n2' with
       | NLit a, NLit b -> NLit (a * b)
       | NLit 0, _ | _, NLit 0 -> NLit 0
       | NLit 1, n | n, NLit 1 -> n
       | _ -> NMul (n1', n2'))
  | NMax (n1, n2) ->
      let n1' = simplify_nat n1 in
      let n2' = simplify_nat n2 in
      (match n1', n2' with
       | NLit a, NLit b -> NLit (max a b)
       | _ -> NMax (n1', n2'))
  | NMin (n1, n2) ->
      let n1' = simplify_nat n1 in
      let n2' = simplify_nat n2 in
      (match n1', n2' with
       | NLit a, NLit b -> NLit (min a b)
       | _ -> NMin (n1', n2'))
  | NLen v -> NLen v
  | NSizeof t -> NSizeof t

(** Substitute nat variables *)
let rec subst_nat_var name value expr =
  match expr with
  | NLit n -> NLit n
  | NVar v when v = name -> value
  | NVar v -> NVar v
  | NAdd (n1, n2) -> NAdd (subst_nat_var name value n1, subst_nat_var name value n2)
  | NSub (n1, n2) -> NSub (subst_nat_var name value n1, subst_nat_var name value n2)
  | NMul (n1, n2) -> NMul (subst_nat_var name value n1, subst_nat_var name value n2)
  | NMax (n1, n2) -> NMax (subst_nat_var name value n1, subst_nat_var name value n2)
  | NMin (n1, n2) -> NMin (subst_nat_var name value n1, subst_nat_var name value n2)
  | NLen v -> NLen v
  | NSizeof t -> NSizeof t

(** Check if a nat expression is concrete (no variables) *)
let rec is_concrete = function
  | NLit _ -> true
  | NVar _ -> false
  | NAdd (n1, n2) | NSub (n1, n2) | NMul (n1, n2) | NMax (n1, n2) | NMin (n1, n2) ->
      is_concrete n1 && is_concrete n2
  | NLen _ -> false
  | NSizeof _ -> true

(** Evaluate concrete nat expression *)
let rec eval_nat = function
  | NLit n -> Some n
  | NVar _ -> None
  | NAdd (n1, n2) ->
      Option.bind (eval_nat n1) (fun a ->
        Option.map (fun b -> a + b) (eval_nat n2))
  | NSub (n1, n2) ->
      Option.bind (eval_nat n1) (fun a ->
        Option.map (fun b -> max 0 (a - b)) (eval_nat n2))
  | NMul (n1, n2) ->
      Option.bind (eval_nat n1) (fun a ->
        Option.map (fun b -> a * b) (eval_nat n2))
  | NMax (n1, n2) ->
      Option.bind (eval_nat n1) (fun a ->
        Option.map (fun b -> max a b) (eval_nat n2))
  | NMin (n1, n2) ->
      Option.bind (eval_nat n1) (fun a ->
        Option.map (fun b -> min a b) (eval_nat n2))
  | NLen _ -> None
  | NSizeof _ -> Some 4  (* Default size *)

(** Check nat constraint (when possible) *)
let rec check_constraint = function
  | NCTrue -> Some true
  | NCFalse -> Some false
  | NCEq (n1, n2) ->
      Option.bind (eval_nat n1) (fun a ->
        Option.map (fun b -> a = b) (eval_nat n2))
  | NCLt (n1, n2) ->
      Option.bind (eval_nat n1) (fun a ->
        Option.map (fun b -> a < b) (eval_nat n2))
  | NCLe (n1, n2) ->
      Option.bind (eval_nat n1) (fun a ->
        Option.map (fun b -> a <= b) (eval_nat n2))
  | NCAnd (c1, c2) ->
      Option.bind (check_constraint c1) (fun a ->
        Option.map (fun b -> a && b) (check_constraint c2))
  | NCOr (c1, c2) ->
      Option.bind (check_constraint c1) (fun a ->
        Option.map (fun b -> a || b) (check_constraint c2))
  | NCNot c ->
      Option.map not (check_constraint c)

(** Dependent vector type: Vec[n, T] *)
type dep_vec = {
  dv_len: nat_expr;
  dv_elem: ty;
}
[@@deriving show]

(** Create a dependent vector type *)
let vec_type len elem =
  TApp ("Vec", [TRigid (Printf.sprintf "%s" (show_nat_expr len)); elem])

(** Extract length from Vec type if possible *)
let vec_length = function
  | TApp ("Vec", [len_ty; _]) ->
      (match len_ty with
       | TRigid s ->
           (* Try to parse as nat literal *)
           (try Some (NLit (int_of_string s))
            with _ -> Some (NVar s))
       | _ -> None)
  | _ -> None

(** Dependent function type: (n: Nat) -> Vec[n, T] -> Vec[n, T] *)
type dep_arrow = {
  da_nat_params: string list;        (** Nat parameters *)
  da_type_params: string list;       (** Type parameters *)
  da_param_ty: ty;                   (** Parameter type *)
  da_return_ty: ty;                  (** Return type *)
  da_constraints: nat_constraint list;  (** Constraints on nat params *)
}
[@@deriving show]

(** Matrix type: Matrix[m, n, T] *)
let matrix_type m n elem =
  TApp ("Matrix", [
    TRigid (show_nat_expr m);
    TRigid (show_nat_expr n);
    elem
  ])

(** Refinement type: { x: Int | x > 0 } *)
type refined_type = {
  rt_base: ty;
  rt_var: string;
  rt_pred: nat_constraint;
}
[@@deriving show]

(** Create positive integer type *)
let pos_int_type =
  TRefined (TInt, RTrue)  (* Simplified - would need proper predicate *)

(** Create bounded integer type: { x: Int | lo <= x && x < hi } *)
let bounded_int lo hi =
  let constraint_ = NCAnd (
    NCLe (NLit lo, NVar "x"),
    NCLt (NVar "x", NLit hi)
  ) in
  { rt_base = TInt; rt_var = "x"; rt_pred = constraint_ }

(** Index type for arrays: { i: Nat | i < len(arr) } *)
let index_type arr_name =
  let constraint_ = NCLt (NVar "i", NLen arr_name) in
  { rt_base = TNat; rt_var = "i"; rt_pred = constraint_ }

(** Dependent type errors *)
type dep_error =
  | NatMismatch of nat_expr * nat_expr
  | ConstraintViolation of nat_constraint
  | IndexOutOfBounds of nat_expr * nat_expr  (** index, length *)
  | LengthMismatch of string * nat_expr * nat_expr
  | UnsolvedConstraint of nat_constraint
[@@deriving show]

exception Dep_error of dep_error

(** Check array index bounds *)
let check_bounds ctx index_expr len_expr =
  let constraint_ = NCLt (index_expr, len_expr) in
  match check_constraint constraint_ with
  | Some true -> Ok ()
  | Some false -> Error (IndexOutOfBounds (index_expr, len_expr))
  | None ->
      (* Add to constraints for later solving *)
      ctx.dc_constraints <- constraint_ :: ctx.dc_constraints;
      Ok ()

(** Unify two nat expressions *)
let rec unify_nat n1 n2 =
  let n1' = simplify_nat n1 in
  let n2' = simplify_nat n2 in
  if equal_nat_expr n1' n2' then
    Ok []
  else match n1', n2' with
    | NVar v, n | n, NVar v ->
        Ok [(v, n)]
    | NAdd (a1, a2), NAdd (b1, b2) ->
        Result.bind (unify_nat a1 b1) (fun s1 ->
          Result.map (fun s2 -> s1 @ s2) (unify_nat a2 b2))
    | NLit a, NLit b when a = b -> Ok []
    | _ -> Error (NatMismatch (n1', n2'))

(** Check that a dependent function call is valid *)
let check_dep_call ctx dep_fn arg_nats =
  (* Substitute nat arguments into constraints and check *)
  let subst_constraints = List.fold_left2 (fun constrs param_name arg ->
    List.map (fun c ->
      let rec subst_in_constraint = function
        | NCEq (n1, n2) -> NCEq (subst_nat_var param_name arg n1, subst_nat_var param_name arg n2)
        | NCLt (n1, n2) -> NCLt (subst_nat_var param_name arg n1, subst_nat_var param_name arg n2)
        | NCLe (n1, n2) -> NCLe (subst_nat_var param_name arg n1, subst_nat_var param_name arg n2)
        | NCAnd (c1, c2) -> NCAnd (subst_in_constraint c1, subst_in_constraint c2)
        | NCOr (c1, c2) -> NCOr (subst_in_constraint c1, subst_in_constraint c2)
        | NCNot c -> NCNot (subst_in_constraint c)
        | NCTrue -> NCTrue
        | NCFalse -> NCFalse
      in
      subst_in_constraint c
    ) constrs
  ) dep_fn.da_constraints dep_fn.da_nat_params arg_nats in
  (* Check all constraints *)
  List.iter (fun c ->
    match check_constraint c with
    | Some false -> raise (Dep_error (ConstraintViolation c))
    | Some true -> ()
    | None -> ctx.dc_constraints <- c :: ctx.dc_constraints
  ) subst_constraints

(** Common dependent type signatures *)

(** append: Vec[n, T] -> Vec[m, T] -> Vec[n + m, T] *)
let vec_append_sig =
  let n = NVar "n" in
  let m = NVar "m" in
  {
    da_nat_params = ["n"; "m"];
    da_type_params = ["T"];
    da_param_ty = TTuple [vec_type n (TRigid "T"); vec_type m (TRigid "T")];
    da_return_ty = vec_type (NAdd (n, m)) (TRigid "T");
    da_constraints = [];
  }

(** head: Vec[n + 1, T] -> T *)
let vec_head_sig =
  let n = NVar "n" in
  {
    da_nat_params = ["n"];
    da_type_params = ["T"];
    da_param_ty = vec_type (NAdd (n, NLit 1)) (TRigid "T");
    da_return_ty = TRigid "T";
    da_constraints = [];
  }

(** tail: Vec[n + 1, T] -> Vec[n, T] *)
let vec_tail_sig =
  let n = NVar "n" in
  {
    da_nat_params = ["n"];
    da_type_params = ["T"];
    da_param_ty = vec_type (NAdd (n, NLit 1)) (TRigid "T");
    da_return_ty = vec_type n (TRigid "T");
    da_constraints = [];
  }

(** zip: Vec[n, A] -> Vec[n, B] -> Vec[n, (A, B)] *)
let vec_zip_sig =
  let n = NVar "n" in
  {
    da_nat_params = ["n"];
    da_type_params = ["A"; "B"];
    da_param_ty = TTuple [vec_type n (TRigid "A"); vec_type n (TRigid "B")];
    da_return_ty = vec_type n (TTuple [TRigid "A"; TRigid "B"]);
    da_constraints = [];
  }

(** transpose: Matrix[m, n, T] -> Matrix[n, m, T] *)
let matrix_transpose_sig =
  let m = NVar "m" in
  let n = NVar "n" in
  {
    da_nat_params = ["m"; "n"];
    da_type_params = ["T"];
    da_param_ty = matrix_type m n (TRigid "T");
    da_return_ty = matrix_type n m (TRigid "T");
    da_constraints = [];
  }

(** matmul: Matrix[m, n, T] -> Matrix[n, p, T] -> Matrix[m, p, T] *)
let matrix_mul_sig =
  let m = NVar "m" in
  let n = NVar "n" in
  let p = NVar "p" in
  {
    da_nat_params = ["m"; "n"; "p"];
    da_type_params = ["T"];
    da_param_ty = TTuple [matrix_type m n (TRigid "T"); matrix_type n p (TRigid "T")];
    da_return_ty = matrix_type m p (TRigid "T");
    da_constraints = [];
  }
