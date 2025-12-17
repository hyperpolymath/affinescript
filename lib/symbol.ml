(** Symbol table types for AffineScript name resolution *)

(** Unique identifier for symbols *)
type symbol_id = int
[@@deriving show, eq, ord]

let next_id = ref 0
let fresh_id () =
  let id = !next_id in
  incr next_id;
  id

(** Symbol kinds *)
type symbol_kind =
  | SKVariable        (** Local/global variable *)
  | SKFunction        (** Function definition *)
  | SKParameter       (** Function parameter *)
  | SKTypeVar         (** Type variable *)
  | SKType            (** Type definition (struct, enum, alias) *)
  | SKTypeParam       (** Type parameter in generic *)
  | SKTrait           (** Trait definition *)
  | SKEffect          (** Effect definition *)
  | SKEffectOp        (** Effect operation *)
  | SKVariant         (** Enum variant constructor *)
  | SKField           (** Struct/record field *)
  | SKModule          (** Module *)
  | SKBuiltin         (** Built-in symbol *)
[@@deriving show, eq]

(** Visibility of a symbol *)
type visibility =
  | VisPrivate
  | VisPublic
  | VisPubCrate
  | VisPubSuper
  | VisPubIn of string list  (** pub(in path) *)
[@@deriving show, eq]

(** Symbol information *)
type symbol = {
  sym_id: symbol_id;
  sym_name: string;
  sym_kind: symbol_kind;
  sym_span: Span.t option;
  sym_visibility: visibility;
  sym_mutable: bool;
  sym_type: Ast.type_expr option;  (** Type annotation if available *)
  sym_quantity: Ast.quantity option;  (** Quantity annotation (0, 1, ω) *)
}
[@@deriving show]

(** Create a new symbol *)
let make_symbol ~name ~kind ?span ?(vis=VisPrivate) ?(mutable_=false) ?ty ?quantity () = {
  sym_id = fresh_id ();
  sym_name = name;
  sym_kind = kind;
  sym_span = span;
  sym_visibility = vis;
  sym_mutable = mutable_;
  sym_type = ty;
  sym_quantity = quantity;
}

(** Scope - a mapping from names to symbols *)
type scope = {
  scope_id: int;
  scope_parent: scope option;
  scope_symbols: (string, symbol) Hashtbl.t;
  scope_kind: scope_kind;
}

and scope_kind =
  | ScopeGlobal        (** Top-level scope *)
  | ScopeModule        (** Module scope *)
  | ScopeFunction      (** Function body *)
  | ScopeBlock         (** Block expression *)
  | ScopeLoop          (** Loop body (for break/continue) *)
  | ScopeMatch         (** Match arm *)
  | ScopeImpl          (** Impl block *)
  | ScopeTrait         (** Trait definition *)
[@@deriving show]

let scope_counter = ref 0
let fresh_scope_id () =
  let id = !scope_counter in
  incr scope_counter;
  id

(** Create a new scope *)
let make_scope ?parent kind = {
  scope_id = fresh_scope_id ();
  scope_parent = parent;
  scope_symbols = Hashtbl.create 16;
  scope_kind = kind;
}

(** Add a symbol to scope *)
let add_symbol scope symbol =
  Hashtbl.replace scope.scope_symbols symbol.sym_name symbol

(** Look up a symbol in current scope only *)
let find_local scope name =
  Hashtbl.find_opt scope.scope_symbols name

(** Look up a symbol in scope chain *)
let rec find_symbol scope name =
  match Hashtbl.find_opt scope.scope_symbols name with
  | Some sym -> Some sym
  | None ->
      match scope.scope_parent with
      | Some parent -> find_symbol parent name
      | None -> None

(** Check if we're inside a loop (for break/continue) *)
let rec in_loop scope =
  match scope.scope_kind with
  | ScopeLoop -> true
  | _ ->
      match scope.scope_parent with
      | Some parent -> in_loop parent
      | None -> false

(** Check if we're inside a function (for return) *)
let rec in_function scope =
  match scope.scope_kind with
  | ScopeFunction -> true
  | _ ->
      match scope.scope_parent with
      | Some parent -> in_function parent
      | None -> false

(** Get all symbols in current scope *)
let symbols_in_scope scope =
  Hashtbl.fold (fun _ sym acc -> sym :: acc) scope.scope_symbols []

(** Module path *)
type module_path = string list
[@@deriving show, eq]

(** Module definition *)
type module_def = {
  mod_path: module_path;
  mod_scope: scope;
  mod_exports: (string, symbol) Hashtbl.t;
}

(** Create a new module *)
let make_module path parent_scope = {
  mod_path = path;
  mod_scope = make_scope ~parent:parent_scope ScopeModule;
  mod_exports = Hashtbl.create 16;
}

(** Export a symbol from module *)
let export_symbol mod_def symbol =
  if symbol.sym_visibility = VisPublic then
    Hashtbl.replace mod_def.mod_exports symbol.sym_name symbol

(** Symbol table - global registry of all symbols *)
type symbol_table = {
  st_symbols: (symbol_id, symbol) Hashtbl.t;
  st_modules: (module_path, module_def) Hashtbl.t;
  st_global_scope: scope;
  mutable st_current_scope: scope;
}

(** Create a new symbol table *)
let create () =
  let global = make_scope ScopeGlobal in
  {
    st_symbols = Hashtbl.create 256;
    st_modules = Hashtbl.create 16;
    st_global_scope = global;
    st_current_scope = global;
  }

(** Register a symbol in the table *)
let register st symbol =
  Hashtbl.replace st.st_symbols symbol.sym_id symbol;
  add_symbol st.st_current_scope symbol;
  symbol

(** Enter a new scope *)
let enter_scope st kind =
  let new_scope = make_scope ~parent:st.st_current_scope kind in
  st.st_current_scope <- new_scope;
  new_scope

(** Exit current scope *)
let exit_scope st =
  match st.st_current_scope.scope_parent with
  | Some parent -> st.st_current_scope <- parent
  | None -> failwith "Cannot exit global scope"

(** Lookup symbol by ID *)
let lookup_id st id =
  Hashtbl.find_opt st.st_symbols id

(** Lookup symbol by name in current scope chain *)
let lookup st name =
  find_symbol st.st_current_scope name

(** Check if name is defined in current scope only *)
let is_defined_local st name =
  Option.is_some (find_local st.st_current_scope name)

(** Get current scope *)
let current_scope st = st.st_current_scope

(** Get global scope *)
let global_scope st = st.st_global_scope

(** Pretty print symbol for debugging *)
let pp_symbol_short fmt sym =
  Format.fprintf fmt "%s#%d (%a)" sym.sym_name sym.sym_id pp_symbol_kind sym.sym_kind
