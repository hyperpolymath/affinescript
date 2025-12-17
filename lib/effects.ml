(** Algebraic effects and handlers for AffineScript *)

open Ast
open Value

(** Effect operation *)
type effect_op = {
  eo_name: string;
  eo_effect: string;
  eo_params: Types.ty list;
  eo_result: Types.ty;
}
[@@deriving show]

(** Effect definition *)
type effect_def = {
  ef_name: string;
  ef_type_params: string list;
  ef_ops: effect_op list;
}
[@@deriving show]

(** Effect handler clause *)
type handler_clause =
  | HReturn of string * expr      (** return x -> e *)
  | HOp of string * string list * string * expr  (** op(args) k -> e *)
[@@deriving show]

(** Effect handler *)
type handler = {
  h_effect: string;
  h_clauses: handler_clause list;
}
[@@deriving show]

(** Continuation representation *)
type continuation = {
  k_env: env;
  k_expr: expr;
  k_hole: string;  (** Variable to substitute result into *)
}

(** Effect stack frame *)
type effect_frame = {
  ef_handler: handler;
  ef_cont: continuation;
}

(** Effect runtime state *)
type effect_state = {
  es_stack: effect_frame list;
  es_suspended: (string * t * continuation) list;  (** Suspended computations *)
}

let create_effect_state () = {
  es_stack = [];
  es_suspended = [];
}

(** Built-in effects *)

(** State effect *)
let state_effect = {
  ef_name = "State";
  ef_type_params = ["S"];
  ef_ops = [
    { eo_name = "get"; eo_effect = "State"; eo_params = []; eo_result = Types.TRigid "S" };
    { eo_name = "put"; eo_effect = "State"; eo_params = [Types.TRigid "S"]; eo_result = Types.TUnit };
  ];
}

(** Exception effect *)
let exn_effect = {
  ef_name = "Exn";
  ef_type_params = ["E"];
  ef_ops = [
    { eo_name = "throw"; eo_effect = "Exn"; eo_params = [Types.TRigid "E"]; eo_result = Types.TNever };
  ];
}

(** Async effect *)
let async_effect = {
  ef_name = "Async";
  ef_type_params = [];
  ef_ops = [
    { eo_name = "await"; eo_effect = "Async"; eo_params = [Types.TApp ("Future", [Types.TRigid "T"])]; eo_result = Types.TRigid "T" };
    { eo_name = "spawn"; eo_effect = "Async"; eo_params = [Types.TArrow (Types.TUnit, Types.TRigid "T", Types.EEmpty)]; eo_result = Types.TApp ("Future", [Types.TRigid "T"]) };
    { eo_name = "yield_"; eo_effect = "Async"; eo_params = []; eo_result = Types.TUnit };
  ];
}

(** Reader effect *)
let reader_effect = {
  ef_name = "Reader";
  ef_type_params = ["R"];
  ef_ops = [
    { eo_name = "ask"; eo_effect = "Reader"; eo_params = []; eo_result = Types.TRigid "R" };
    { eo_name = "local"; eo_effect = "Reader";
      eo_params = [Types.TArrow (Types.TRigid "R", Types.TRigid "R", Types.EEmpty); Types.TArrow (Types.TUnit, Types.TRigid "A", Types.ECon ("Reader", []))];
      eo_result = Types.TRigid "A" };
  ];
}

(** Writer effect *)
let writer_effect = {
  ef_name = "Writer";
  ef_type_params = ["W"];
  ef_ops = [
    { eo_name = "tell"; eo_effect = "Writer"; eo_params = [Types.TRigid "W"]; eo_result = Types.TUnit };
  ];
}

(** Choice/NonDet effect *)
let choice_effect = {
  ef_name = "Choice";
  ef_type_params = [];
  ef_ops = [
    { eo_name = "choose"; eo_effect = "Choice"; eo_params = []; eo_result = Types.TBool };
    { eo_name = "fail"; eo_effect = "Choice"; eo_params = []; eo_result = Types.TNever };
  ];
}

(** Console IO effect *)
let console_effect = {
  ef_name = "Console";
  ef_type_params = [];
  ef_ops = [
    { eo_name = "print"; eo_effect = "Console"; eo_params = [Types.TString]; eo_result = Types.TUnit };
    { eo_name = "read_line"; eo_effect = "Console"; eo_params = []; eo_result = Types.TString };
  ];
}

(** All built-in effects *)
let builtin_effects = [
  state_effect;
  exn_effect;
  async_effect;
  reader_effect;
  writer_effect;
  choice_effect;
  console_effect;
]

(** Effect operation result *)
type op_result =
  | OpReturn of t                      (** Operation completed with value *)
  | OpSuspend of string * t list * continuation  (** Operation suspended *)
  | OpAbort of t                       (** Handler aborted with value *)

(** Find handler for effect operation *)
let find_handler state effect_name op_name =
  List.find_opt (fun frame ->
    frame.ef_handler.h_effect = effect_name &&
    List.exists (function
      | HOp (name, _, _, _) -> name = op_name
      | _ -> false
    ) frame.ef_handler.h_clauses
  ) state.es_stack

(** Perform an effect operation *)
let perform state env effect_name op_name args cont =
  match find_handler state effect_name op_name with
  | None ->
      (* No handler - this is an error *)
      failwith (Printf.sprintf "Unhandled effect operation: %s.%s" effect_name op_name)
  | Some frame ->
      (* Find the operation clause *)
      let clause = List.find_map (function
        | HOp (name, params, k_name, body) when name = op_name ->
            Some (params, k_name, body)
        | _ -> None
      ) frame.ef_handler.h_clauses in
      match clause with
      | None ->
          failwith (Printf.sprintf "Missing clause for operation: %s" op_name)
      | Some (params, k_name, body) ->
          (* Bind parameters and continuation *)
          let handler_env = child_env env in
          List.iter2 (fun param arg ->
            bind handler_env param arg ~mutable_:false ~linear:false
          ) params args;
          (* Bind continuation as a function *)
          let k_value = VBuiltin ("continuation", fun resume_args ->
            match resume_args with
            | [result] ->
                (* Resume the suspended computation *)
                let resume_env = child_env cont.k_env in
                bind resume_env cont.k_hole result ~mutable_:false ~linear:false;
                Eval.eval resume_env cont.k_expr
            | _ -> failwith "Continuation expects exactly one argument"
          ) in
          bind handler_env k_name k_value ~mutable_:false ~linear:false;
          (* Evaluate handler body *)
          OpReturn (Eval.eval handler_env body)

(** Install a handler and run computation *)
let handle state handler computation env =
  let frame = {
    ef_handler = handler;
    ef_cont = { k_env = env; k_expr = computation; k_hole = "_result" };
  } in
  let state' = { state with es_stack = frame :: state.es_stack } in
  try
    let result = Eval.eval env computation in
    (* Computation completed - run return clause *)
    let return_clause = List.find_map (function
      | HReturn (x, body) -> Some (x, body)
      | _ -> None
    ) handler.h_clauses in
    match return_clause with
    | Some (x, body) ->
        let return_env = child_env env in
        bind return_env x result ~mutable_:false ~linear:false;
        Eval.eval return_env body
    | None -> result
  with
  | Eval.Runtime_error _ as e ->
      (* Pop handler and re-raise *)
      raise e

(** State effect handler implementation *)
let state_handler init_state =
  let state_ref = ref init_state in
  {
    h_effect = "State";
    h_clauses = [
      HReturn ("x", ExprVar { name = "x"; span = Span.dummy });
      (* get() k -> k(!state_ref) *)
      (* put(s) k -> state_ref := s; k(()) *)
    ];
  }

(** Exception handler implementation *)
let exn_handler on_throw =
  {
    h_effect = "Exn";
    h_clauses = [
      HReturn ("x", ExprLit (LitUnit Span.dummy));  (* wrap in Ok *)
      (* throw(e) k -> on_throw(e) *)
    ];
  }

(** Reader handler implementation *)
let reader_handler env_value =
  {
    h_effect = "Reader";
    h_clauses = [
      HReturn ("x", ExprVar { name = "x"; span = Span.dummy });
      (* ask() k -> k(env_value) *)
      (* local(f, m) k -> handle[Reader(f(env_value))] { m() } |> k *)
    ];
  }

(** Writer handler implementation *)
let writer_handler =
  {
    h_effect = "Writer";
    h_clauses = [
      HReturn ("x", ExprVar { name = "x"; span = Span.dummy });  (* (x, []) *)
      (* tell(w) k -> let (a, ws) = k(()) in (a, w :: ws) *)
    ];
  }

(** Choice handler implementation - returns list of all results *)
let choice_list_handler =
  {
    h_effect = "Choice";
    h_clauses = [
      HReturn ("x", ExprVar { name = "x"; span = Span.dummy });  (* [x] *)
      (* choose() k -> k(true) @ k(false) *)
      (* fail() k -> [] *)
    ];
  }

(** Async runtime state *)
type async_state = {
  mutable as_ready: (int * (unit -> t)) list;  (** Ready tasks *)
  mutable as_waiting: (int * (t -> unit -> t)) list;  (** Waiting for value *)
  mutable as_next_id: int;
}

let create_async_state () = {
  as_ready = [];
  as_waiting = [];
  as_next_id = 0;
}

(** Future value *)
type future_state =
  | Pending
  | Resolved of t

type future = {
  f_id: int;
  mutable f_state: future_state;
  mutable f_waiters: (t -> unit) list;
}

(** Async effect interpreter *)
let run_async computation env =
  let async_state = create_async_state () in
  let futures : (int, future) Hashtbl.t = Hashtbl.create 16 in

  let make_future () =
    let id = async_state.as_next_id in
    async_state.as_next_id <- id + 1;
    let f = { f_id = id; f_state = Pending; f_waiters = [] } in
    Hashtbl.replace futures id f;
    f
  in

  let resolve_future f value =
    f.f_state <- Resolved value;
    List.iter (fun waiter -> waiter value) f.f_waiters;
    f.f_waiters <- []
  in

  (* Main scheduler loop *)
  let rec run_scheduler () =
    match async_state.as_ready with
    | [] -> ()  (* All done *)
    | (id, task) :: rest ->
        async_state.as_ready <- rest;
        let _ = task () in
        run_scheduler ()
  in

  (* Start main computation *)
  async_state.as_ready <- [(0, fun () -> Eval.eval env computation)];
  run_scheduler ();
  VUnit

(** Effect type checking *)

(** Effect row - set of effects *)
type effect_row =
  | EffPure                            (** No effects *)
  | EffVar of string                   (** Effect variable *)
  | EffCons of string * effect_row     (** E | r *)
[@@deriving show]

(** Check if effect is in row *)
let rec effect_in_row eff = function
  | EffPure -> false
  | EffVar _ -> true  (* Unknown row might contain it *)
  | EffCons (e, rest) -> e = eff || effect_in_row eff rest

(** Subtract effect from row *)
let rec subtract_effect eff = function
  | EffPure -> EffPure
  | EffVar v -> EffVar v  (* Can't remove from unknown row *)
  | EffCons (e, rest) ->
      if e = eff then rest
      else EffCons (e, subtract_effect eff rest)

(** Unify effect rows *)
let rec unify_effects r1 r2 =
  match r1, r2 with
  | EffPure, EffPure -> Ok []
  | EffVar v, r | r, EffVar v -> Ok [(v, r)]
  | EffCons (e1, rest1), EffCons (e2, rest2) when e1 = e2 ->
      unify_effects rest1 rest2
  | EffCons (e1, rest1), r2 ->
      if effect_in_row e1 r2 then
        unify_effects rest1 (subtract_effect e1 r2)
      else
        Error (Printf.sprintf "Effect %s not in row" e1)
  | _ -> Error "Cannot unify effect rows"

(** Check that computation's effects are handled *)
let check_handled handlers comp_effects =
  let handled_effects = List.map (fun h -> h.h_effect) handlers in
  let rec check = function
    | EffPure -> true
    | EffVar _ -> true  (* Polymorphic - assumed ok *)
    | EffCons (e, rest) ->
        List.mem e handled_effects && check rest
  in
  check comp_effects
