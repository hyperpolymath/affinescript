(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell (hyperpolymath) *)

(** Shared exceptions for parser semantic actions.

    This module exists because exceptions defined in the [%{ %}] prologue
    of [parser.mly] are not exported through [parser.mli], so consumers
    (notably [parse_driver.ml]) cannot pattern-match on them. Defining the
    exception here lets both [parser.mly] and [parse_driver.ml] reference
    it without a circular dependency. *)

(** Raised by a parser semantic action when a syntactically valid form
    carries a value the action rejects (e.g. a quantity literal that is
    neither 0 nor 1, an unknown [@]-attribute name).

    Caught by [Parse_driver] and translated to its [Parse_error]
    exception with proper [Span.t] resolution. *)
exception Parse_action_error of string * Lexing.position * Lexing.position
