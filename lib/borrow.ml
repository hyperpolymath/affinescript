(** Borrow checker for AffineScript - enforces affine type rules *)

open Ast

(** Usage state for a binding *)
type usage_state =
  | Unused              (** Never used *)
  | Used of Span.t      (** Used exactly once at this location *)
  | Moved of Span.t     (** Ownership transferred (moved) *)
  | Borrowed of Span.t  (** Borrowed (referenced) *)
  | MutBorrowed of Span.t  (** Mutably borrowed *)
  | MultipleUses        (** Used more than once (error for linear) *)
[@@deriving show]

(** Binding information *)
type binding_info = {
  bi_name: string;
  bi_span: Span.t option;
  bi_quantity: Types.quantity;
  bi_mutable: bool;
  bi_ownership: ownership option;
  mutable bi_state: usage_state;
}

(** Borrow errors *)
type error =
  | UseAfterMove of string * Span.t * Span.t  (** name, use location, move location *)
  | LinearUsedMultipleTimes of string * Span.t * Span.t
  | LinearNotUsed of string * Span.t option
  | CannotMoveFromBorrow of string * Span.t
  | CannotMutateBorrowed of string * Span.t
  | DoubleMutableBorrow of string * Span.t * Span.t
  | BorrowedWhileMutablyBorrowed of string * Span.t * Span.t
  | EscapingBorrow of string * Span.t
  | UninitializedUse of string * Span.t

exception Borrow_error of error

let error_to_string = function
  | UseAfterMove (name, _, _) ->
      Printf.sprintf "Use of moved value: %s" name
  | LinearUsedMultipleTimes (name, _, _) ->
      Printf.sprintf "Linear value '%s' used multiple times" name
  | LinearNotUsed (name, _) ->
      Printf.sprintf "Linear value '%s' not used" name
  | CannotMoveFromBorrow (name, _) ->
      Printf.sprintf "Cannot move out of borrowed value: %s" name
  | CannotMutateBorrowed (name, _) ->
      Printf.sprintf "Cannot mutate borrowed value: %s" name
  | DoubleMutableBorrow (name, _, _) ->
      Printf.sprintf "Cannot borrow '%s' mutably more than once" name
  | BorrowedWhileMutablyBorrowed (name, _, _) ->
      Printf.sprintf "Cannot borrow '%s' while mutably borrowed" name
  | EscapingBorrow (name, _) ->
      Printf.sprintf "Borrowed value '%s' escapes its scope" name
  | UninitializedUse (name, _) ->
      Printf.sprintf "Use of uninitialized value: %s" name

(** Borrow checker context *)
type context = {
  ctx_bindings: (string, binding_info) Hashtbl.t;
  ctx_parent: context option;
  ctx_borrows: (string, Span.t * bool) Hashtbl.t;  (** name -> (span, is_mutable) *)
  ctx_errors: error list ref;
  ctx_in_loop: bool;
}

let create_context () = {
  ctx_bindings = Hashtbl.create 32;
  ctx_parent = None;
  ctx_borrows = Hashtbl.create 16;
  ctx_errors = ref [];
  ctx_in_loop = false;
}

let child_context parent = {
  ctx_bindings = Hashtbl.create 16;
  ctx_parent = Some parent;
  ctx_borrows = Hashtbl.create 8;
  ctx_errors = parent.ctx_errors;
  ctx_in_loop = parent.ctx_in_loop;
}

let loop_context parent = {
  ctx_bindings = Hashtbl.create 16;
  ctx_parent = Some parent;
  ctx_borrows = Hashtbl.create 8;
  ctx_errors = parent.ctx_errors;
  ctx_in_loop = true;
}

let add_error ctx err =
  ctx.ctx_errors := err :: !(ctx.ctx_errors)

(** Add a binding to context *)
let add_binding ctx name ~span ~quantity ~mutable_ ~ownership =
  let info = {
    bi_name = name;
    bi_span = span;
    bi_quantity = quantity;
    bi_mutable = mutable_;
    bi_ownership = ownership;
    bi_state = Unused;
  } in
  Hashtbl.replace ctx.ctx_bindings name info

(** Look up a binding *)
let rec lookup_binding ctx name =
  match Hashtbl.find_opt ctx.ctx_bindings name with
  | Some info -> Some info
  | None -> Option.bind ctx.ctx_parent (fun p -> lookup_binding p name)

(** Record a use of a binding *)
let record_use ctx name span =
  match lookup_binding ctx name with
  | None -> ()  (* Unknown binding - skip *)
  | Some info ->
      match info.bi_state, info.bi_quantity with
      | Moved move_span, _ ->
          add_error ctx (UseAfterMove (name, span, move_span))
      | Used use_span, Types.QOne ->
          add_error ctx (LinearUsedMultipleTimes (name, span, use_span));
          info.bi_state <- MultipleUses
      | _, _ ->
          info.bi_state <- Used span

(** Record a move of a binding *)
let record_move ctx name span =
  match lookup_binding ctx name with
  | None -> ()
  | Some info ->
      match info.bi_state with
      | Moved move_span ->
          add_error ctx (UseAfterMove (name, span, move_span))
      | Borrowed borrow_span ->
          add_error ctx (CannotMoveFromBorrow (name, borrow_span))
      | MutBorrowed borrow_span ->
          add_error ctx (CannotMoveFromBorrow (name, borrow_span))
      | _ ->
          info.bi_state <- Moved span

(** Record a borrow *)
let record_borrow ctx name span ~mutable_ =
  match lookup_binding ctx name with
  | None -> ()
  | Some info ->
      (* Check existing borrows *)
      (match Hashtbl.find_opt ctx.ctx_borrows name with
       | Some (existing_span, true) when mutable_ ->
           add_error ctx (DoubleMutableBorrow (name, span, existing_span))
       | Some (existing_span, true) ->
           add_error ctx (BorrowedWhileMutablyBorrowed (name, span, existing_span))
       | Some (existing_span, false) when mutable_ ->
           add_error ctx (BorrowedWhileMutablyBorrowed (name, span, existing_span))
       | _ -> ());
      Hashtbl.replace ctx.ctx_borrows name (span, mutable_);
      info.bi_state <- if mutable_ then MutBorrowed span else Borrowed span

(** Check that all linear bindings are used at scope exit *)
let check_linear_used ctx =
  Hashtbl.iter (fun name info ->
    if info.bi_quantity = Types.QOne then
      match info.bi_state with
      | Unused -> add_error ctx (LinearNotUsed (name, info.bi_span))
      | _ -> ()
  ) ctx.ctx_bindings

(** Get span from expression *)
let expr_span = function
  | ExprSpan (_, span) -> Some span
  | ExprVar id -> Some id.span
  | ExprLit (LitInt (_, span)) -> Some span
  | ExprLit (LitFloat (_, span)) -> Some span
  | ExprLit (LitBool (_, span)) -> Some span
  | ExprLit (LitChar (_, span)) -> Some span
  | ExprLit (LitString (_, span)) -> Some span
  | ExprLit (LitUnit span) -> Some span
  | _ -> None

(** Convert AST quantity to internal quantity *)
let convert_quantity = function
  | Some Ast.QZero -> Types.QZero
  | Some Ast.QOne -> Types.QOne
  | Some Ast.QOmega -> Types.QOmega
  | None -> Types.QOmega  (* Default to unrestricted *)

(** Check an expression *)
let rec check_expr ctx expr =
  match expr with
  | ExprSpan (e, _) -> check_expr ctx e

  | ExprLit _ -> ()

  | ExprVar id ->
      record_use ctx id.name id.span

  | ExprLet { el_mut; el_pat; el_value; el_body; _ } ->
      check_expr ctx el_value;
      add_pattern_bindings ctx el_pat ~mutable_:el_mut;
      Option.iter (check_expr ctx) el_body

  | ExprIf { ei_cond; ei_then; ei_else } ->
      check_expr ctx ei_cond;
      let then_ctx = child_context ctx in
      check_expr then_ctx ei_then;
      check_linear_used then_ctx;
      Option.iter (fun e ->
        let else_ctx = child_context ctx in
        check_expr else_ctx e;
        check_linear_used else_ctx
      ) ei_else

  | ExprMatch { em_scrutinee; em_arms } ->
      check_expr ctx em_scrutinee;
      List.iter (fun arm ->
        let arm_ctx = child_context ctx in
        add_pattern_bindings arm_ctx arm.ma_pat ~mutable_:false;
        Option.iter (check_expr arm_ctx) arm.ma_guard;
        check_expr arm_ctx arm.ma_body;
        check_linear_used arm_ctx
      ) em_arms

  | ExprLambda { elam_params; elam_body; _ } ->
      let fn_ctx = child_context ctx in
      List.iter (fun p ->
        add_binding fn_ctx p.p_name.name
          ~span:(Some p.p_name.span)
          ~quantity:(convert_quantity p.p_quantity)
          ~mutable_:false
          ~ownership:p.p_ownership
      ) elam_params;
      check_expr fn_ctx elam_body;
      check_linear_used fn_ctx

  | ExprApp (func, args) ->
      check_expr ctx func;
      (* Arguments may be moved to the function *)
      List.iter (fun arg ->
        check_expr ctx arg;
        (* If arg is a variable, consider it moved (conservative) *)
        match arg with
        | ExprVar id -> record_move ctx id.name id.span
        | _ -> ()
      ) args

  | ExprField (e, _) ->
      check_expr ctx e

  | ExprTupleIndex (e, _) ->
      check_expr ctx e

  | ExprIndex (e1, e2) ->
      check_expr ctx e1;
      check_expr ctx e2

  | ExprTuple exprs ->
      List.iter (check_expr ctx) exprs

  | ExprArray exprs ->
      List.iter (check_expr ctx) exprs

  | ExprRecord { er_fields; er_spread } ->
      List.iter (fun (id, expr_opt) ->
        match expr_opt with
        | Some e -> check_expr ctx e
        | None -> record_use ctx id.name id.span
      ) er_fields;
      Option.iter (check_expr ctx) er_spread

  | ExprRowRestrict (e, _) ->
      check_expr ctx e

  | ExprBinary (e1, _, e2) ->
      check_expr ctx e1;
      check_expr ctx e2

  | ExprUnary (op, e) ->
      (match op with
       | OpRef ->
           (* Creating a reference borrows the value *)
           (match e with
            | ExprVar id ->
                let span = match expr_span expr with Some s -> s | None -> id.span in
                record_borrow ctx id.name span ~mutable_:false
            | _ -> check_expr ctx e)
       | OpDeref -> check_expr ctx e
       | _ -> check_expr ctx e)

  | ExprBlock blk ->
      check_block ctx blk

  | ExprReturn e_opt ->
      Option.iter (check_expr ctx) e_opt

  | ExprTry { et_body; et_catch; et_finally } ->
      check_block ctx et_body;
      Option.iter (fun arms ->
        List.iter (fun arm ->
          let arm_ctx = child_context ctx in
          add_pattern_bindings arm_ctx arm.ma_pat ~mutable_:false;
          check_expr arm_ctx arm.ma_body;
          check_linear_used arm_ctx
        ) arms
      ) et_catch;
      Option.iter (check_block ctx) et_finally

  | ExprHandle { eh_body; eh_handlers } ->
      check_expr ctx eh_body;
      List.iter (fun h ->
        match h with
        | HandlerReturn (pat, e) ->
            let h_ctx = child_context ctx in
            add_pattern_bindings h_ctx pat ~mutable_:false;
            check_expr h_ctx e;
            check_linear_used h_ctx
        | HandlerOp (_, pats, e) ->
            let h_ctx = child_context ctx in
            List.iter (fun p -> add_pattern_bindings h_ctx p ~mutable_:false) pats;
            check_expr h_ctx e;
            check_linear_used h_ctx
      ) eh_handlers

  | ExprResume e_opt ->
      Option.iter (check_expr ctx) e_opt

  | ExprUnsafe ops ->
      List.iter (fun op ->
        match op with
        | UnsafeRead e | UnsafeForget e -> check_expr ctx e
        | UnsafeWrite (e1, e2) | UnsafeOffset (e1, e2) ->
            check_expr ctx e1;
            check_expr ctx e2
        | UnsafeTransmute (_, _, e) -> check_expr ctx e
        | UnsafeAssume _ -> ()
      ) ops

  | ExprVariant _ -> ()

and check_block ctx { blk_stmts; blk_expr } =
  let block_ctx = child_context ctx in
  List.iter (check_stmt block_ctx) blk_stmts;
  Option.iter (check_expr block_ctx) blk_expr;
  check_linear_used block_ctx

and check_stmt ctx = function
  | StmtLet { sl_mut; sl_pat; sl_value; _ } ->
      check_expr ctx sl_value;
      add_pattern_bindings ctx sl_pat ~mutable_:sl_mut

  | StmtExpr e ->
      check_expr ctx e

  | StmtAssign (target, _, value) ->
      check_expr ctx value;
      (* Check target is mutable *)
      (match target with
       | ExprVar id ->
           (match lookup_binding ctx id.name with
            | Some info when not info.bi_mutable ->
                add_error ctx (CannotMutateBorrowed (id.name, id.span))
            | _ -> ())
       | _ -> check_expr ctx target)

  | StmtWhile (cond, body) ->
      (* In a loop, variables might be used multiple times *)
      let loop_ctx = loop_context ctx in
      check_expr loop_ctx cond;
      check_block loop_ctx body

  | StmtFor (pat, iter, body) ->
      check_expr ctx iter;
      let loop_ctx = loop_context ctx in
      add_pattern_bindings loop_ctx pat ~mutable_:false;
      check_block loop_ctx body

and add_pattern_bindings ctx pat ~mutable_ =
  match pat with
  | PatWildcard _ -> ()
  | PatVar id ->
      add_binding ctx id.name
        ~span:(Some id.span)
        ~quantity:Types.QOmega
        ~mutable_
        ~ownership:None
  | PatLit _ -> ()
  | PatCon (_, pats) ->
      List.iter (fun p -> add_pattern_bindings ctx p ~mutable_) pats
  | PatTuple pats ->
      List.iter (fun p -> add_pattern_bindings ctx p ~mutable_) pats
  | PatRecord (fields, _) ->
      List.iter (fun (id, pat_opt) ->
        match pat_opt with
        | Some p -> add_pattern_bindings ctx p ~mutable_
        | None ->
            add_binding ctx id.name
              ~span:(Some id.span)
              ~quantity:Types.QOmega
              ~mutable_
              ~ownership:None
      ) fields
  | PatOr (p1, p2) ->
      add_pattern_bindings ctx p1 ~mutable_;
      add_pattern_bindings ctx p2 ~mutable_
  | PatAs (id, p) ->
      add_binding ctx id.name
        ~span:(Some id.span)
        ~quantity:Types.QOmega
        ~mutable_
        ~ownership:None;
      add_pattern_bindings ctx p ~mutable_

(** Check a function declaration *)
let check_fn_decl ctx (decl: fn_decl) =
  let fn_ctx = child_context ctx in
  (* Add parameters *)
  List.iter (fun p ->
    add_binding fn_ctx p.p_name.name
      ~span:(Some p.p_name.span)
      ~quantity:(convert_quantity p.p_quantity)
      ~mutable_:false
      ~ownership:p.p_ownership
  ) decl.fd_params;
  (* Check body *)
  (match decl.fd_body with
   | FnBlock blk -> check_block fn_ctx blk
   | FnExpr e -> check_expr fn_ctx e);
  check_linear_used fn_ctx

(** Check a program *)
let check_program (prog: program) =
  let ctx = create_context () in
  (* Check all function declarations *)
  List.iter (fun decl ->
    match decl with
    | TopFn fd -> check_fn_decl ctx fd
    | TopConst { tc_value; _ } -> check_expr ctx tc_value
    | _ -> ()
  ) prog.prog_decls;
  List.rev !(ctx.ctx_errors)
