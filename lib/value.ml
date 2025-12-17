(** Runtime values for AffineScript interpreter *)

(** Runtime values *)
type t =
  | VUnit
  | VBool of bool
  | VInt of int
  | VFloat of float
  | VChar of char
  | VString of string
  | VTuple of t list
  | VArray of t array
  | VRecord of (string * t) list
  | VVariant of string * string * t list  (** type, variant, args *)
  | VClosure of {
      params: Ast.param list;
      body: Ast.expr;
      env: env;
    }
  | VBuiltin of string * (t list -> t)
  | VRef of t ref  (** Mutable reference *)

(** Environment - variable bindings *)
and env = {
  bindings: (string, binding) Hashtbl.t;
  parent: env option;
}

and binding = {
  value: t;
  mutable_: bool;
  linear: bool;  (** Linear values can only be used once *)
  mutable consumed: bool;  (** Track if linear value was consumed *)
}

(** Create a new empty environment *)
let empty_env () = {
  bindings = Hashtbl.create 32;
  parent = None;
}

(** Create a child environment *)
let child_env parent = {
  bindings = Hashtbl.create 16;
  parent = Some parent;
}

(** Bind a variable in the current scope *)
let bind env name value ~mutable_ ~linear =
  Hashtbl.replace env.bindings name { value; mutable_; linear; consumed = false }

(** Look up a variable *)
let rec lookup env name =
  match Hashtbl.find_opt env.bindings name with
  | Some binding -> Some binding
  | None -> Option.bind env.parent (fun p -> lookup p name)

(** Update a mutable variable *)
let rec update env name new_value =
  match Hashtbl.find_opt env.bindings name with
  | Some binding when binding.mutable_ ->
      Hashtbl.replace env.bindings name { binding with value = new_value };
      true
  | Some _ -> false  (* Not mutable *)
  | None ->
      match env.parent with
      | Some parent -> update parent name new_value
      | None -> false

(** Mark a linear binding as consumed *)
let consume env name =
  match Hashtbl.find_opt env.bindings name with
  | Some binding when binding.linear ->
      if binding.consumed then
        Error (Printf.sprintf "Linear value '%s' already consumed" name)
      else begin
        binding.consumed <- true;
        Ok binding.value
      end
  | Some binding -> Ok binding.value
  | None ->
      match env.parent with
      | Some parent ->
          (match lookup parent name with
           | Some binding when binding.linear ->
               if binding.consumed then
                 Error (Printf.sprintf "Linear value '%s' already consumed" name)
               else begin
                 binding.consumed <- true;
                 Ok binding.value
               end
           | Some binding -> Ok binding.value
           | None -> Error (Printf.sprintf "Unbound variable: %s" name))
      | None -> Error (Printf.sprintf "Unbound variable: %s" name)

(** Pretty print a value *)
let rec pp fmt = function
  | VUnit -> Format.fprintf fmt "()"
  | VBool b -> Format.fprintf fmt "%b" b
  | VInt i -> Format.fprintf fmt "%d" i
  | VFloat f -> Format.fprintf fmt "%g" f
  | VChar c -> Format.fprintf fmt "'%c'" c
  | VString s -> Format.fprintf fmt "\"%s\"" (String.escaped s)
  | VTuple vs ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp) vs
  | VArray vs ->
      Format.fprintf fmt "[%a]"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp)
        (Array.to_list vs)
  | VRecord fields ->
      Format.fprintf fmt "{%a}"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           (fun fmt (k, v) -> Format.fprintf fmt "%s: %a" k pp v))
        fields
  | VVariant (_, variant, []) ->
      Format.fprintf fmt "%s" variant
  | VVariant (_, variant, args) ->
      Format.fprintf fmt "%s(%a)" variant
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp)
        args
  | VClosure _ -> Format.fprintf fmt "<closure>"
  | VBuiltin (name, _) -> Format.fprintf fmt "<builtin:%s>" name
  | VRef r -> Format.fprintf fmt "ref(%a)" pp !r

let show v =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  pp fmt v;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

(** Check value equality *)
let rec equal v1 v2 =
  match v1, v2 with
  | VUnit, VUnit -> true
  | VBool b1, VBool b2 -> b1 = b2
  | VInt i1, VInt i2 -> i1 = i2
  | VFloat f1, VFloat f2 -> f1 = f2
  | VChar c1, VChar c2 -> c1 = c2
  | VString s1, VString s2 -> s1 = s2
  | VTuple vs1, VTuple vs2 ->
      List.length vs1 = List.length vs2 &&
      List.for_all2 equal vs1 vs2
  | VArray a1, VArray a2 ->
      Array.length a1 = Array.length a2 &&
      Array.for_all2 equal a1 a2
  | VRecord f1, VRecord f2 ->
      List.length f1 = List.length f2 &&
      List.for_all2 (fun (k1, v1) (k2, v2) -> k1 = k2 && equal v1 v2) f1 f2
  | VVariant (t1, v1, a1), VVariant (t2, v2, a2) ->
      t1 = t2 && v1 = v2 && List.length a1 = List.length a2 &&
      List.for_all2 equal a1 a2
  | _ -> false

(** Coerce to bool for conditionals *)
let to_bool = function
  | VBool b -> Ok b
  | v -> Error (Printf.sprintf "Expected Bool, got %s" (show v))

(** Coerce to int *)
let to_int = function
  | VInt i -> Ok i
  | v -> Error (Printf.sprintf "Expected Int, got %s" (show v))

(** Coerce to float *)
let to_float = function
  | VFloat f -> Ok f
  | VInt i -> Ok (Float.of_int i)
  | v -> Error (Printf.sprintf "Expected Float, got %s" (show v))
