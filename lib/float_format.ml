(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 hyperpolymath *)

(** [float_to_string] text, shared by every backend (#643).

    The format is ECMAScript [Number.prototype.toString], which is what
    Bun's [String(x)] gives: the fewest significant digits that read back
    to the same double (the closest such digits, an exact tie going to
    the even digit), laid out as "1e+21", "100", "0.000001", "1e-7",
    "NaN", "-Infinity". Both zeros print as "0". The core-WASM printer
    ({!Wasm_dtoa}) produces the same text. *)

(* Shortest digits: the first precision whose correctly rounded %e form
   reads back to [x]. If any p-digit decimal round-trips, the correctly
   rounded one does, since it is the closest. *)
let shortest_digits (x : float) : string * int =
  let rec go p =
    let s = Printf.sprintf "%.*e" (p - 1) x in
    if p >= 17 || float_of_string s = x then s else go (p + 1)
  in
  let s = go 1 in
  let e_at = String.index s 'e' in
  let mant = String.sub s 0 e_at in
  let exp10 = int_of_string (String.sub s (e_at + 1) (String.length s - e_at - 1)) in
  let digits = String.concat "" (String.split_on_char '.' mant) in
  (* drop trailing zeros (%.16e of 0.5 gives no extras, but be safe) *)
  let n = ref (String.length digits) in
  while !n > 1 && digits.[!n - 1] = '0' do decr n done;
  (String.sub digits 0 !n, exp10 + 1)

let to_string (x : float) : string =
  if Float.is_nan x then "NaN"
  else if x = Float.infinity then "Infinity"
  else if x = Float.neg_infinity then "-Infinity"
  else if x = 0.0 then "0"
  else
    let sign = if x < 0.0 then "-" else "" in
    let (ds, n) = shortest_digits (Float.abs x) in
    let k = String.length ds in
    let body =
      if k <= n && n <= 21 then ds ^ String.make (n - k) '0'
      else if 0 < n && n <= 21 then
        String.sub ds 0 n ^ "." ^ String.sub ds n (k - n)
      else if -6 < n && n <= 0 then "0." ^ String.make (-n) '0' ^ ds
      else
        let e = n - 1 in
        let m = if k = 1 then ds else String.sub ds 0 1 ^ "." ^ String.sub ds 1 (k - 1) in
        m ^ "e" ^ (if e < 0 then "-" else "+") ^ string_of_int (abs e)
    in
    sign ^ body
