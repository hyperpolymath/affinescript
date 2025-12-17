(** Internal type representation for type checking *)

(** Type variable ID *)
type tyvar_id = int
[@@deriving show, eq, ord]

let next_tyvar = ref 0
let fresh_tyvar () =
  let id = !next_tyvar in
  incr next_tyvar;
  id

(** Row variable ID *)
type rowvar_id = int
[@@deriving show, eq, ord]

let next_rowvar = ref 0
let fresh_rowvar () =
  let id = !next_rowvar in
  incr next_rowvar;
  id

(** Effect variable ID *)
type effvar_id = int
[@@deriving show, eq, ord]

let next_effvar = ref 0
let fresh_effvar () =
  let id = !next_effvar in
  incr next_effvar;
  id

(** Quantity (QTT) *)
type quantity =
  | QZero   (** Erased - compile time only *)
  | QOne    (** Linear - exactly once *)
  | QOmega  (** Unrestricted *)
[@@deriving show, eq]

(** Multiply quantities (semiring) *)
let mult_quantity q1 q2 =
  match q1, q2 with
  | QZero, _ | _, QZero -> QZero
  | QOne, QOne -> QOne
  | QOmega, _ | _, QOmega -> QOmega

(** Add quantities (join in semiring) *)
let add_quantity q1 q2 =
  match q1, q2 with
  | QZero, q | q, QZero -> q
  | QOne, QOne -> QOmega  (* Used twice = unrestricted *)
  | QOmega, _ | _, QOmega -> QOmega

(** Internal type representation *)
type ty =
  | TUnit
  | TBool
  | TInt
  | TNat
  | TFloat
  | TChar
  | TString
  | TNever                              (** Bottom type *)
  | TVar of tyvar_id                    (** Unification variable *)
  | TRigid of string                    (** Rigid type variable (skolem) *)
  | TApp of string * ty list            (** Type constructor application *)
  | TArrow of ty * ty * effect          (** Function type: T -> U / E *)
  | TForall of string * kind * ty       (** Polymorphic type *)
  | TTuple of ty list                   (** Tuple type *)
  | TRecord of row                      (** Record type with row *)
  | TRef of ty                          (** Reference type (immutable borrow) *)
  | TMut of ty                          (** Mutable reference *)
  | TOwn of ty                          (** Owned type *)
  | TRefined of ty * refinement         (** Refinement type *)
  | TQuantified of quantity * ty        (** Quantity-annotated type *)

(** Row type for records *)
and row =
  | REmpty                              (** {} *)
  | RVar of rowvar_id                   (** Row variable *)
  | RExtend of string * ty * row        (** {l: T | r} *)

(** Effect type *)
and effect =
  | EEmpty                              (** Pure *)
  | EVar of effvar_id                   (** Effect variable *)
  | ECon of string * ty list            (** Effect constructor *)
  | EUnion of effect * effect           (** Effect union *)

(** Refinement predicate (simplified) *)
and refinement =
  | RTrue
  | RFalse
  | REq of string * int                 (** x = n *)
  | RLt of string * int                 (** x < n *)
  | RGt of string * int                 (** x > n *)
  | RAnd of refinement * refinement
  | ROr of refinement * refinement
  | RNot of refinement

(** Kind *)
and kind =
  | KType                               (** Type *)
  | KNat                                (** Natural number *)
  | KRow                                (** Row *)
  | KEffect                             (** Effect *)
  | KArrow of kind * kind               (** κ → κ *)
[@@deriving show, eq]

(** Type scheme - polymorphic type *)
type scheme = {
  sc_tyvars: (string * kind) list;     (** Bound type variables *)
  sc_rowvars: string list;              (** Bound row variables *)
  sc_effvars: string list;              (** Bound effect variables *)
  sc_type: ty;                          (** The type *)
}
[@@deriving show]

(** Create a monomorphic scheme *)
let mono ty = {
  sc_tyvars = [];
  sc_rowvars = [];
  sc_effvars = [];
  sc_type = ty;
}

(** Substitution for type variables *)
type subst = {
  ty_subst: (tyvar_id, ty) Hashtbl.t;
  row_subst: (rowvar_id, row) Hashtbl.t;
  eff_subst: (effvar_id, effect) Hashtbl.t;
}

let empty_subst () = {
  ty_subst = Hashtbl.create 16;
  row_subst = Hashtbl.create 8;
  eff_subst = Hashtbl.create 8;
}

(** Apply substitution to type *)
let rec apply_subst subst ty =
  match ty with
  | TUnit | TBool | TInt | TNat | TFloat | TChar | TString | TNever | TRigid _ -> ty
  | TVar id ->
      (match Hashtbl.find_opt subst.ty_subst id with
       | Some t -> apply_subst subst t
       | None -> ty)
  | TApp (name, args) ->
      TApp (name, List.map (apply_subst subst) args)
  | TArrow (t1, t2, eff) ->
      TArrow (apply_subst subst t1, apply_subst subst t2, apply_eff_subst subst eff)
  | TForall (v, k, t) ->
      TForall (v, k, apply_subst subst t)
  | TTuple ts ->
      TTuple (List.map (apply_subst subst) ts)
  | TRecord row ->
      TRecord (apply_row_subst subst row)
  | TRef t -> TRef (apply_subst subst t)
  | TMut t -> TMut (apply_subst subst t)
  | TOwn t -> TOwn (apply_subst subst t)
  | TRefined (t, r) -> TRefined (apply_subst subst t, r)
  | TQuantified (q, t) -> TQuantified (q, apply_subst subst t)

and apply_row_subst subst = function
  | REmpty -> REmpty
  | RVar id ->
      (match Hashtbl.find_opt subst.row_subst id with
       | Some r -> apply_row_subst subst r
       | None -> RVar id)
  | RExtend (l, t, r) ->
      RExtend (l, apply_subst subst t, apply_row_subst subst r)

and apply_eff_subst subst = function
  | EEmpty -> EEmpty
  | EVar id ->
      (match Hashtbl.find_opt subst.eff_subst id with
       | Some e -> apply_eff_subst subst e
       | None -> EVar id)
  | ECon (name, args) ->
      ECon (name, List.map (apply_subst subst) args)
  | EUnion (e1, e2) ->
      EUnion (apply_eff_subst subst e1, apply_eff_subst subst e2)

(** Extend substitution *)
let extend_ty_subst subst id ty =
  Hashtbl.replace subst.ty_subst id ty

let extend_row_subst subst id row =
  Hashtbl.replace subst.row_subst id row

let extend_eff_subst subst id eff =
  Hashtbl.replace subst.eff_subst id eff

(** Free type variables in a type *)
let rec free_tyvars ty =
  match ty with
  | TUnit | TBool | TInt | TNat | TFloat | TChar | TString | TNever | TRigid _ -> []
  | TVar id -> [id]
  | TApp (_, args) -> List.concat_map free_tyvars args
  | TArrow (t1, t2, _) -> free_tyvars t1 @ free_tyvars t2
  | TForall (_, _, t) -> free_tyvars t
  | TTuple ts -> List.concat_map free_tyvars ts
  | TRecord row -> free_tyvars_row row
  | TRef t | TMut t | TOwn t -> free_tyvars t
  | TRefined (t, _) -> free_tyvars t
  | TQuantified (_, t) -> free_tyvars t

and free_tyvars_row = function
  | REmpty -> []
  | RVar _ -> []
  | RExtend (_, t, r) -> free_tyvars t @ free_tyvars_row r

(** Occurs check - prevent infinite types *)
let rec occurs id ty =
  match ty with
  | TVar id' -> id = id'
  | TApp (_, args) -> List.exists (occurs id) args
  | TArrow (t1, t2, _) -> occurs id t1 || occurs id t2
  | TForall (_, _, t) -> occurs id t
  | TTuple ts -> List.exists (occurs id) ts
  | TRecord row -> occurs_row id row
  | TRef t | TMut t | TOwn t -> occurs id t
  | TRefined (t, _) -> occurs id t
  | TQuantified (_, t) -> occurs id t
  | _ -> false

and occurs_row id = function
  | REmpty | RVar _ -> false
  | RExtend (_, t, r) -> occurs id t || occurs_row id r

(** Pretty print type *)
let rec pp_ty fmt ty =
  match ty with
  | TUnit -> Format.fprintf fmt "()"
  | TBool -> Format.fprintf fmt "Bool"
  | TInt -> Format.fprintf fmt "Int"
  | TNat -> Format.fprintf fmt "Nat"
  | TFloat -> Format.fprintf fmt "Float"
  | TChar -> Format.fprintf fmt "Char"
  | TString -> Format.fprintf fmt "String"
  | TNever -> Format.fprintf fmt "Never"
  | TVar id -> Format.fprintf fmt "?%d" id
  | TRigid name -> Format.fprintf fmt "%s" name
  | TApp (name, []) -> Format.fprintf fmt "%s" name
  | TApp (name, args) ->
      Format.fprintf fmt "%s[%a]" name
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_ty) args
  | TArrow (t1, t2, EEmpty) ->
      Format.fprintf fmt "(%a -> %a)" pp_ty t1 pp_ty t2
  | TArrow (t1, t2, eff) ->
      Format.fprintf fmt "(%a -> %a / %a)" pp_ty t1 pp_ty t2 pp_eff eff
  | TForall (v, _, t) ->
      Format.fprintf fmt "(forall %s. %a)" v pp_ty t
  | TTuple ts ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_ty) ts
  | TRecord row ->
      Format.fprintf fmt "{%a}" pp_row row
  | TRef t -> Format.fprintf fmt "ref %a" pp_ty t
  | TMut t -> Format.fprintf fmt "mut %a" pp_ty t
  | TOwn t -> Format.fprintf fmt "own %a" pp_ty t
  | TRefined (t, _) -> Format.fprintf fmt "%a where (...)" pp_ty t
  | TQuantified (q, t) ->
      let q_str = match q with QZero -> "0" | QOne -> "1" | QOmega -> "ω" in
      Format.fprintf fmt "%s %a" q_str pp_ty t

and pp_row fmt = function
  | REmpty -> ()
  | RVar id -> Format.fprintf fmt "..%d" id
  | RExtend (l, t, REmpty) -> Format.fprintf fmt "%s: %a" l pp_ty t
  | RExtend (l, t, r) -> Format.fprintf fmt "%s: %a, %a" l pp_ty t pp_row r

and pp_eff fmt = function
  | EEmpty -> Format.fprintf fmt "Pure"
  | EVar id -> Format.fprintf fmt "?e%d" id
  | ECon (name, []) -> Format.fprintf fmt "%s" name
  | ECon (name, args) ->
      Format.fprintf fmt "%s[%a]" name
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_ty) args
  | EUnion (e1, e2) -> Format.fprintf fmt "%a + %a" pp_eff e1 pp_eff e2

let show_ty ty =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  pp_ty fmt ty;
  Format.pp_print_flush fmt ();
  Buffer.contents buf
