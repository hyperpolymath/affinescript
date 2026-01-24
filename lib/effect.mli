(* SPDX-License-Identifier: MIT OR AGPL-3.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** Effect system operations *)

open Types

(** Combine two effects into a union *)
val union_eff : eff -> eff -> eff

(** Union a list of effects *)
val union_effs : eff list -> eff

(** Check if an effect is pure *)
val is_pure : eff -> bool

(** Effect subtyping: e1 <: e2 means e1 can be used where e2 is expected
    Pure is a subtype of any effect *)
val eff_subsumes : eff -> eff -> bool

(** Check if effect e1 is a subset of e2 *)
val eff_subset : eff -> eff -> bool

(** Normalize an effect (flatten unions, remove duplicates, sort) *)
val normalize_eff : eff -> eff

(** Pretty print effect *)
val string_of_eff : eff -> string
