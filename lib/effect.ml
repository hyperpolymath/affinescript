(* SPDX-License-Identifier: MIT OR AGPL-3.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** Effect system operations *)

open Types

(** Combine two effects into a union *)
let union_eff (e1 : eff) (e2 : eff) : eff =
  let e1 = repr_eff e1 in
  let e2 = repr_eff e2 in
  match (e1, e2) with
  | (EPure, e) | (e, EPure) -> e
  | (e1', e2') when e1' = e2' -> e1'
  | (EUnion es1, EUnion es2) ->
    (* Flatten and deduplicate *)
    let all_effs = es1 @ es2 in
    let unique = List.sort_uniq compare all_effs in
    if List.length unique = 1 then List.hd unique
    else EUnion unique
  | (EUnion es, e) | (e, EUnion es) ->
    if List.mem e es then EUnion es
    else EUnion (List.sort_uniq compare (e :: es))
  | (e1', e2') ->
    EUnion (List.sort_uniq compare [e1'; e2'])

(** Union a list of effects *)
let union_effs (effs : eff list) : eff =
  List.fold_left union_eff EPure effs

(** Check if an effect is pure *)
let is_pure (e : eff) : bool =
  match repr_eff e with
  | EPure -> true
  | _ -> false

(** Effect subtyping: e1 <: e2 means e1 can be used where e2 is expected
    - Pure can be used anywhere (Pure <: any effect)
    - IO context can use Pure or IO (both Pure <: IO and IO <: IO)
    - Pure context can only use Pure (IO is NOT <: Pure) *)
let rec eff_subsumes (e1 : eff) (e2 : eff) : bool =
  let e1 = repr_eff e1 in
  let e2 = repr_eff e2 in
  match (e1, e2) with
  (* Any effect can accommodate Pure *)
  | (_, EPure) -> true
  (* Pure cannot accommodate other effects *)
  | (EPure, _) -> false
  (* Same effect *)
  | (e1', e2') when e1' = e2' -> true
  (* e1 is a union, e1 subsumes e2 if any effect in e1 subsumes e2 *)
  | (EUnion es, e2') ->
    List.exists (fun e -> eff_subsumes e e2') es
  (* e2 is a union, e1 must subsume all effects in e2 *)
  | (e1', EUnion es) ->
    List.for_all (fun e -> eff_subsumes e1' e) es
  (* Otherwise no subtyping relationship *)
  | _ -> false

(** Check if effect e1 is a subset of e2 (all effects in e1 are in e2) *)
let rec eff_subset (e1 : eff) (e2 : eff) : bool =
  let e1 = repr_eff e1 in
  let e2 = repr_eff e2 in
  match (e1, e2) with
  | (EPure, _) -> true
  | (_, EPure) -> false
  | (ESingleton s1, ESingleton s2) -> s1 = s2
  | (ESingleton s, EUnion es) ->
    List.exists (fun e ->
      match repr_eff e with
      | ESingleton s' -> s = s'
      | _ -> false
    ) es
  | (EUnion es1, EUnion es2) ->
    List.for_all (fun e1' ->
      List.exists (fun e2' ->
        match (repr_eff e1', repr_eff e2') with
        | (ESingleton s1, ESingleton s2) -> s1 = s2
        | _ -> e1' = e2'
      ) es2
    ) es1
  | (EUnion es, e2') ->
    List.for_all (fun e -> eff_subset e e2') es
  | (e1', e2') -> e1' = e2'

(** Normalize an effect (flatten unions, remove duplicates, sort) *)
let normalize_eff (e : eff) : eff =
  let rec collect_singletons (e : eff) : string list =
    match repr_eff e with
    | EPure -> []
    | ESingleton s -> [s]
    | EUnion es -> List.concat_map collect_singletons es
    | EVar _ -> []  (* Variables can't be normalized *)
  in

  match repr_eff e with
  | EPure -> EPure
  | ESingleton _ as e' -> e'
  | EVar _ as e' -> e'
  | EUnion _ as e' ->
    let singletons = collect_singletons e' in
    let unique = List.sort_uniq String.compare singletons in
    match unique with
    | [] -> EPure
    | [s] -> ESingleton s
    | ss -> EUnion (List.map (fun s -> ESingleton s) ss)

(** Pretty print effect *)
let string_of_eff (e : eff) : string =
  let rec aux (e : eff) : string =
    match repr_eff e with
    | EPure -> "pure"
    | ESingleton s -> s
    | EUnion es ->
      String.concat " | " (List.map aux es)
    | EVar r ->
      begin match !r with
        | EUnbound (v, _) -> Printf.sprintf "'e%d" v
        | ELink e' -> aux e'
      end
  in
  aux (normalize_eff e)
