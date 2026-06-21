(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) *)

(*
   F5_RenderFaithful.v
   ═══════════════════
   F-5 (docs/PROOF-NEEDS.adoc): `render_ty` faithfulness / non-collision.

   lib/face.ml's `render_ty` rewrites a small fixed vocabulary of canonical type
   names per face — Unit, Bool, and Option[_] — leaving everything else
   untouched. F-5 asks: is that renaming injective (it never collapses two
   distinct canonical types onto one displayed string), even where face lexemes
   overlap — Js renders Unit as "null" and Option[T] as "T | null", so both
   mention "null"?

   This models the canonical-type vocabulary render_ty branches on (`cty`), the
   displayed-name lexemes (`name` — distinct constructors model distinct on-screen
   tokens), and the rendered form (`rty`), then mirrors render_ty as
   `render : face → cty → rty`. Two fidelity points carried over from the OCaml:

   * Unit/Bool are special-cased only at the WHOLE-type level (`if s = "Unit"`),
     never on a nested occurrence.
   * Option's inner is rendered *canonically* — the OCaml `global_replace`
     captures the canonical substring `\1` — so Js `Option[Option[Int]]` ⇒
     "Option[Int] | null" (inner Option intact), exactly the greedy-regex result.

   Theorems: `canon_inj`; `render_inj` (every face's renaming is injective — the
   non-collapse guarantee); and the pointed `js_no_collapse` / `cafe_no_collapse`
   / `option_never_atom` (Unit and Option[_] never read as the same type despite
   the shared "null"/"?" lexeme). Axiom-free, no Admitted.

   Scope (honest): models the displayed-name vocabulary, not raw OCaml strings;
   assumes well-formed type strings (no adversarial text around `Option[...]`).

   `.v` is Coq, not V-lang — see formal/README.adoc and .hypatia-ignore.
*)

Require Import PeanoNat.

(* The six faces (lib/face.ml `face`). *)
Inductive face := Canonical | Python | Js | Pseudocode | Lucid | Cafe.

(* Displayed atom lexemes. Distinct constructors ⇒ distinct on-screen names —
   faithful, because the real renamings (None/null/nothing/bool/boolean/Boolean)
   are genuinely distinct tokens and no canonical base name equals any of them. *)
Inductive name :=
| NUnit | NBool
| NNone | NNothing | NNull
| Nbool | NBoolean | NbooleanJs
| NBase (n : nat).

(* Rendered type forms. *)
Inductive rty :=
| RNm (x : name)
| ROptBr (r : rty)    (* Option[ r ] *)
| ROrNull (r : rty)   (* r | null    *)
| RMaybe (r : rty)    (* Maybe r     *)
| RQ (r : rty).       (* r ?         *)

(* Canonical types: the vocabulary render_ty distinguishes. *)
Inductive cty :=
| CUnit | CBool | CBase (n : nat) | COption (a : cty).

(* Canonical rendering — Option's inner stays canonical (matches the regex \1). *)
Fixpoint canon (t : cty) : rty :=
  match t with
  | CUnit     => RNm NUnit
  | CBool     => RNm NBool
  | CBase n   => RNm (NBase n)
  | COption a => ROptBr (canon a)
  end.

(* Per-face rendering, mirroring lib/face.ml render_ty: top-level special-case
   for Unit/Bool; per-face Option wrapper with a canonical inner. *)
Definition render (f : face) (t : cty) : rty :=
  match t with
  | CUnit =>
    match f with
    | Canonical | Lucid => RNm NUnit
    | Python            => RNm NNone
    | Js | Cafe         => RNm NNull
    | Pseudocode        => RNm NNothing
    end
  | CBool =>
    match f with
    | Canonical                 => RNm NBool
    | Python                    => RNm Nbool
    | Js                        => RNm NbooleanJs
    | Pseudocode | Lucid | Cafe => RNm NBoolean
    end
  | CBase n => RNm (NBase n)
  | COption a =>
    match f with
    | Canonical | Python | Pseudocode => ROptBr (canon a)
    | Js                              => ROrNull (canon a)
    | Lucid                           => RMaybe  (canon a)
    | Cafe                            => RQ      (canon a)
    end
  end.

(* ── canonical rendering is injective ──────────────────────────────────── *)
Lemma canon_inj : forall t1 t2, canon t1 = canon t2 -> t1 = t2.
Proof.
  induction t1; destruct t2; simpl; intro H;
    try discriminate; try reflexivity.
  - inversion H; reflexivity.                       (* CBase = CBase *)
  - inversion H; f_equal; apply IHt1; assumption.   (* COption = COption *)
Qed.

(* ── every face's renaming is injective: the non-collapse guarantee ─────── *)
Theorem render_inj : forall f t1 t2, render f t1 = render f t2 -> t1 = t2.
Proof.
  intros f t1 t2 H; destruct f, t1, t2; cbn in H;
    solve [ discriminate
          | reflexivity
          | inversion H; subst; reflexivity
          | inversion H; f_equal; apply canon_inj; assumption ].
Qed.

(* ── the pointed overlaps don't collapse ───────────────────────────────────
   Js renders Unit as "null" and Option[T] as "T | null"; Cafe as "null"/"T?".
   The shared lexeme never makes Unit and an Option read as the same type. *)
Corollary js_no_collapse   : forall a, render Js   CUnit <> render Js   (COption a).
Proof. intros a H; discriminate. Qed.

Corollary cafe_no_collapse : forall a, render Cafe CUnit <> render Cafe (COption a).
Proof. intros a H; discriminate. Qed.

(* No Option ever collides with a base/Unit/Bool atom (different rendered shape). *)
Corollary option_never_atom : forall f a x, render f (COption a) <> RNm x.
Proof. intros f a x H; destruct f; cbn in H; discriminate. Qed.

Print Assumptions render_inj.
Print Assumptions js_no_collapse.
Print Assumptions option_never_atom.
