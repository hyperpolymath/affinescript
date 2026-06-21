(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) *)

(*
   F3_PragmaDecidable.v
   ════════════════════
   Mechanizes obligation **F-3** (face pragma detection) for a concrete model,
   discharging both statements from Siblings_Stated.v — axiom-free, no Admitted.

   Models `lib/face_pragma.ml`: the alias table `parse_face_name` and the fact
   that `detect_in_source` reads only a bounded leading *region* and stops at
   the first code token, so its result is independent of bytes past the pragma.

   `.v` is Coq, not V-lang — see formal/README.adoc and .hypatia-ignore.
*)

Require Import ASFormal.Siblings_Stated.

(* The six established faces. *)
Inductive face := FCanonical | FPython | FJs | FPseudo | FLucid | FCafe.

(* ── F3_alias_functional: the alias table is a function ─────────────────── *)

(* Recognised pragma tokens (a representative slice of parse_face_name's
   accepted names + brand aliases). *)
Inductive pname :=
  | NCanonical | NPython | NRattle | NJs | NJaffa
  | NPseudo | NLucid | NCafe | NUnknown.

Definition resolve_name (n : pname) : option face :=
  match n with
  | NCanonical        => Some FCanonical
  | NPython | NRattle => Some FPython        (* python / rattle / rattlescript *)
  | NJs | NJaffa      => Some FJs            (* js / javascript / jaffa(script) *)
  | NPseudo           => Some FPseudo
  | NLucid            => Some FLucid
  | NCafe             => Some FCafe
  | NUnknown          => None
  end.

Theorem alias_functional :
  forall n f1 f2, resolve_name n = Some f1 -> resolve_name n = Some f2 -> f1 = f2.
Proof. intros n f1 f2 H1 H2; rewrite H1 in H2; injection H2; auto. Qed.

(* ── F3_pragma_local: detection ignores bytes past the scanned region ───── *)

(* A source = the pragma in its scanned leading window + arbitrary trailing
   bytes. `pragma_region` keeps the window and discards the trailing bytes;
   `detect` consults only the window. So detection cannot depend on anything
   past the pragma — the precise content of F-3. *)
Record src := mkSrc { window : option pname; trailing : nat }.

Definition pragma_region (s : src) : src := mkSrc (window s) 0.

Definition detect (s : src) : option face :=
  match window (pragma_region s) with
  | Some n => resolve_name n
  | None   => None
  end.

Theorem pragma_local :
  forall s1 s2, pragma_region s1 = pragma_region s2 -> detect s1 = detect s2.
Proof. intros s1 s2 H; unfold detect; rewrite H; reflexivity. Qed.

(* ── discharge the stated obligations ──────────────────────────────────── *)

Definition F3_alias_discharged
  : F3_alias_functional face pname resolve_name
  := alias_functional.

Definition F3_local_discharged
  : F3_pragma_local src face detect pragma_region
  := pragma_local.

Print Assumptions F3_alias_discharged.
Print Assumptions F3_local_discharged.
