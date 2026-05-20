(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** Entry point for JavaScript playground build.
    This loads the JS API module by referencing it.
*)

(* Reference the Js_api module to ensure it's linked *)
let () = ()

(* The JS API is automatically registered via Js.export in Js_api module *)
include Js_api
