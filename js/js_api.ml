(* SPDX-License-Identifier: MIT OR AGPL-3.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** JavaScript API for AffineScript interpreter.
    This module provides a JavaScript-compatible interface
    for running the interpreter in the browser via js_of_ocaml.
*)

open Js_of_ocaml

(** JavaScript-compatible result type *)
class type eval_result = object
  method success : bool Js.readonly_prop
  method value : Js.js_string Js.t Js.optdef Js.readonly_prop
  method error : Js.js_string Js.t Js.optdef Js.readonly_prop
  method type_ : Js.js_string Js.t Js.optdef Js.readonly_prop
end

(** Create a success result *)
let success_result value type_str : eval_result Js.t =
  object%js
    val success = true
    val value = Js.def (Js.string value)
    val error = Js.undefined
    val type_ = Js.def (Js.string type_str)
  end

(** Create an error result *)
let error_result error_msg : eval_result Js.t =
  object%js
    val success = false
    val value = Js.undefined
    val error = Js.def (Js.string error_msg)
    val type_ = Js.undefined
  end

(** Evaluate an expression string *)
let eval_expr_string (code : string) : eval_result Js.t =
  try
    (* Parse the expression *)
    let expr = Affinescript.Parse_driver.parse_expr ~file:"<playground>" code in

    (* Create initial environment *)
    let env = Affinescript.Interp.create_initial_env () in

    (* Evaluate *)
    match Affinescript.Interp.eval env expr with
    | Ok value ->
      let value_str = Affinescript.Value.show_value value in
      (* Try to get type if possible - for now just return "Any" *)
      success_result value_str "Any"
    | Error err ->
      error_result (Affinescript.Value.show_eval_error err)
  with
  | Affinescript.Parse_driver.Parse_error (msg, _span) ->
    error_result ("Parse error: " ^ msg)
  | Affinescript.Lexer.Lexer_error (msg, _span) ->
    error_result ("Lexer error: " ^ msg)
  | exn ->
    error_result ("Unexpected error: " ^ Printexc.to_string exn)

(** Evaluate a full program *)
let eval_program_string (code : string) : eval_result Js.t =
  try
    (* Parse the program *)
    let prog = Affinescript.Parse_driver.parse_string ~file:"<playground>" code in

    (* Evaluate declarations *)
    match Affinescript.Interp.eval_program prog with
    | Ok env ->
      (* Return the environment as a string *)
      let env_str =
        List.map (fun (name, value) ->
          Printf.sprintf "%s = %s" name (Affinescript.Value.show_value value)
        ) env
        |> String.concat "\n"
      in
      success_result env_str "Environment"
    | Error err ->
      error_result (Affinescript.Value.show_eval_error err)
  with
  | Affinescript.Parse_driver.Parse_error (msg, _span) ->
    error_result ("Parse error: " ^ msg)
  | Affinescript.Lexer.Lexer_error (msg, _span) ->
    error_result ("Lexer error: " ^ msg)
  | exn ->
    error_result ("Unexpected error: " ^ Printexc.to_string exn)

(** Export JavaScript API *)
let () =
  Js.export "AffineScript"
    (object%js
      method eval code =
        eval_expr_string (Js.to_string code)

      method evalProgram code =
        eval_program_string (Js.to_string code)

      method version =
        Js.string "0.1.0"
    end)
