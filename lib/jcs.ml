(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) *)

(* Wired into lib/dune and used by lib/json_output.ml for every machine
   document (--json reports and the LSP payloads).

   Compilation status: built and vector-checked as a standalone dune + yojson
   project (the vectors are in test/test_jcs.ml, all six byte-identical to an
   independent JCS implementation). The *repository* build could not complete
   in that environment: sedlex's build downloads UnicodeData.txt from
   unicode.org, which the sandbox blocks — see
   tools/bootstrap-ocaml-toolchain.sh. The first full `dune build` is the
   remaining check. *)

(** Canonical JSON: I-JSON (RFC 7493) validation + JCS (RFC 8785)
    canonicalisation.

    Every JSON document this compiler writes for machines — [--json]
    diagnostics, LSP hover/goto-def/completion payloads — goes through this
    module, so two runs that agree on the values also agree on the bytes.

    Estate standard: {b hyperpolymath/ijson-jcs} (D306) — I-JSON validation
    and JCS canonicalisation, with [check]/[fix]/[canon] in its CLI. That
    repository ships a Rust library; this file is a port of the two RFCs it
    implements, because the compiler's toolchain is hermetic (no Cargo, no
    crates.io) and shelling out to the CLI would make [--json] output depend
    on what is installed on the machine. The CLI remains the out-of-band
    conformance oracle: {b tools/run_jcs_conformance.sh} runs it over the
    golden corpus when it is on [PATH] and says so loudly when it is not.

    What "canonical" means here, in the order the RFCs state it:

    + I-JSON (RFC 7493): object keys are unique and case-sensitively compared,
      strings are valid Unicode (no lone surrogates, no overlong encodings),
      and numbers are finite (no [NaN], no [+/-Infinity]).
    + JCS (RFC 8785): object keys sorted by UTF-16 code units (so the order is
      the one ECMAScript's [Array.prototype.sort] gives, {e not} UTF-8 byte
      order or locale order), strings escaped exactly as
      [JSON.stringify] escapes them, numbers formatted with the ECMAScript
      [Number::toString] algorithm, and no insignificant whitespace.

    The serialiser is written against [Yojson.Basic.t] — the seven constructs
    the compiler's own documents use — not [Yojson.Safe.t]; there is no
    [`Intlit`/`Tuple`/`Variant` here because [Basic] has none.

    Two consequences worth stating, because they are the ones that bite:

    * UTF-16 ordering and UTF-8 byte ordering disagree above the BMP: a
      surrogate code unit (U+D800..U+DFFF) sorts {e before} U+E000..U+FFFF in
      UTF-16, so a key containing an emoji precedes a key containing a
      private-use character. [compare_keys] implements the UTF-16 order.
    * JCS numbers are ES6 numbers, not C's [%g]: [1e21] is ["1e+21"],
      [1e20] is ["100000000000000000000"], and [-0] is ["0"]. [es6_number]
      implements that algorithm (shortest round-tripping digits, then the
      ES6 placement rule), including the exponent-form switch at 1e21/1e-7. *)

(** {1 Errors} *)

(** Why a value cannot be canonicalised. Each case is an I-JSON violation,
    i.e. something that would make the document unsafe for the consumers that
    rely on it (content addressing, signed payloads, diffing). *)
type error =
  | NaN_or_infinity of string
      (** A non-finite number, at the given JSON path. *)
  | Duplicate_key of string * string
      (** Two members with the same key, at the object path. *)
  | Lone_surrogate of string
      (** A string that is not valid Unicode (lone surrogate or bad UTF-8),
          at the given JSON path. *)

(** Render an error for a diagnostic line. *)
let error_to_string = function
  | NaN_or_infinity path ->
    Printf.sprintf "%s: NaN and Infinity have no I-JSON representation" path
  | Duplicate_key (path, key) ->
    Printf.sprintf "%s: duplicate object key %S (I-JSON requires unique keys)"
      path key
  | Lone_surrogate path ->
    Printf.sprintf "%s: string is not valid Unicode (lone surrogate or bad UTF-8)"
      path

(** {1 UTF-16 code units} *)

(** Decode UTF-8 into UTF-16 code units. Raises [Invalid_argument] on a lone
    surrogate, an overlong form, a truncated sequence, or a code point above
    U+10FFFF — every one of which RFC 7493 forbids. *)
let utf16_units (s : string) : int array =
  let n = String.length s in
  let units = ref [] in
  let push u = units := u :: !units in
  let malformed i why =
    invalid_arg (Printf.sprintf "invalid UTF-8 at byte %d: %s" i why)
  in
  let i = ref 0 in
  while !i < n do
    let c0 = Char.code s.[!i] in
    if c0 < 0x80 then begin
      push c0;
      incr i
    end else if c0 < 0xC2 then malformed !i "continuation byte or overlong"
    else if c0 < 0xE0 then begin
      if !i + 1 >= n then malformed !i "truncated 2-byte sequence";
      let c1 = Char.code s.[!i + 1] in
      if c1 land 0xC0 <> 0x80 then malformed (!i + 1) "expected continuation";
      push (((c0 land 0x1F) lsl 6) lor (c1 land 0x3F));
      i := !i + 2
    end else if c0 < 0xF0 then begin
      if !i + 2 >= n then malformed !i "truncated 3-byte sequence";
      let c1 = Char.code s.[!i + 1] and c2 = Char.code s.[!i + 2] in
      if c1 land 0xC0 <> 0x80 then malformed (!i + 1) "expected continuation";
      if c2 land 0xC0 <> 0x80 then malformed (!i + 2) "expected continuation";
      let cp = ((c0 land 0x0F) lsl 12) lor ((c1 land 0x3F) lsl 6)
               lor (c2 land 0x3F) in
      if cp < 0x800 then malformed !i "overlong 3-byte sequence";
      if cp >= 0xD800 && cp <= 0xDFFF then malformed !i "lone surrogate";
      push cp;
      i := !i + 3
    end else if c0 < 0xF5 then begin
      if !i + 3 >= n then malformed !i "truncated 4-byte sequence";
      let c1 = Char.code s.[!i + 1]
      and c2 = Char.code s.[!i + 2]
      and c3 = Char.code s.[!i + 3] in
      if c1 land 0xC0 <> 0x80 then malformed (!i + 1) "expected continuation";
      if c2 land 0xC0 <> 0x80 then malformed (!i + 2) "expected continuation";
      if c3 land 0xC0 <> 0x80 then malformed (!i + 3) "expected continuation";
      let cp = ((c0 land 0x07) lsl 18) lor ((c1 land 0x3F) lsl 12)
               lor ((c2 land 0x3F) lsl 6) lor (c3 land 0x3F) in
      if cp < 0x10000 then malformed !i "overlong 4-byte sequence";
      if cp > 0x10FFFF then malformed !i "code point above U+10FFFF";
      let v = cp - 0x10000 in
      push (0xD800 lor (v lsr 10));
      push (0xDC00 lor (v land 0x3FF));
      i := !i + 4
    end else malformed !i "invalid leading byte"
  done;
  Array.of_list (List.rev !units)

(** JCS key order: lexicographic over UTF-16 code units. *)
let compare_keys (a : string) (b : string) : int =
  let ua = utf16_units a and ub = utf16_units b in
  let la = Array.length ua and lb = Array.length ub in
  let rec go i =
    if i >= la && i >= lb then 0
    else if i >= la then -1
    else if i >= lb then 1
    else
      let d = compare ua.(i) ub.(i) in
      if d <> 0 then d else go (i + 1)
  in
  go 0

(** {1 Strings} *)

(** Escape one string exactly as ECMAScript's [JSON.stringify] does: the two
    mandatory escapes, the five short control escapes, [\u00xx] (lower-case
    hex) for the rest of C0, and every other code point verbatim. *)
let escape_string (s : string) : string =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  String.iter
    (fun ch ->
      match ch with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\b' -> Buffer.add_string buf "\\b"
      | '\012' -> Buffer.add_string buf "\\f"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when Char.code c < 0x20 ->
        Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"';
  Buffer.contents buf

(** {1 Numbers} *)

(** C's [%g] exponent form, or OCaml's shortest round-tripping digits? We ask
    for [%.*g] with increasing precision and keep the first spelling that
    parses back to the same float. That yields the shortest digit string,
    which is what ECMAScript starts from. *)
let shortest_g (f : float) : string =
  let rec go p =
    let s = Printf.sprintf "%.*g" p f in
    if float_of_string s = f || p >= 17 then s else go (p + 1)
  in
  go 1

(** Split a [%g] spelling into (negative, significant digits, decimal
    exponent of the leading digit). *)
let split_decimal (s : string) : bool * string * int =
  let neg, body =
    if String.length s > 0 && s.[0] = '-' then
      (true, String.sub s 1 (String.length s - 1))
    else (false, s)
  in
  let mantissa, exp_part =
    match String.index_opt body 'e' with
    | None -> (body, 0)
    | Some i ->
      ( String.sub body 0 i,
        int_of_string (String.sub body (i + 1) (String.length body - i - 1)) )
  in
  let int_part, frac_part =
    match String.index_opt mantissa '.' with
    | None -> (mantissa, "")
    | Some i ->
      ( String.sub mantissa 0 i,
        String.sub mantissa (i + 1) (String.length mantissa - i - 1) )
  in
  let digits = int_part ^ frac_part in
  (* Exponent of the leading digit: the int part contributes (len-1), the
     fraction shifts it down by its length once leading zeros are stripped. *)
  let stripped = ref 0 in
  let first_nonzero = ref (-1) in
  String.iteri
    (fun i c ->
      if !first_nonzero < 0 && c <> '0' then first_nonzero := i;
      if !first_nonzero < 0 then incr stripped)
    digits;
  if !first_nonzero < 0 then (neg, "0", 0)
  else
    let significant = String.sub digits !first_nonzero
        (String.length digits - !first_nonzero) in
    (* drop trailing zeros: they carry no information *)
    let len = ref (String.length significant) in
    while !len > 1 && significant.[!len - 1] = '0' do decr len done;
    let significant = String.sub significant 0 !len in
    let exp10 =
      (String.length int_part - 1 - !stripped) + exp_part
    in
    (neg, significant, exp10)

(** The ECMAScript [Number::toString] algorithm (ES2015 §7.1.12.1), applied to
    a non-zero finite double: shortest round-tripping digits, then plain
    notation while the exponent is in [-6, 21), exponent form outside it. *)
let es6_number (f : float) : string =
  if f = 0.0 then "0" (* also -0.0, which JCS renders "0" *)
  else
    let neg, digits, exp10 = split_decimal (shortest_g f) in
    let n = String.length digits in
    let body =
      if exp10 >= -6 && exp10 < 21 then
        if exp10 >= n - 1 then
          (* integer: digits followed by (exp10 - (n-1)) zeros *)
          digits ^ String.make (exp10 - (n - 1)) '0'
        else if exp10 >= 0 then
          String.sub digits 0 (exp10 + 1)
          ^ "."
          ^ String.sub digits (exp10 + 1) (n - exp10 - 1)
        else
          (* 0.000ddd *)
          "0." ^ String.make (-exp10 - 1) '0' ^ digits
      else
        let mant =
          if n = 1 then digits
          else String.sub digits 0 1 ^ "." ^ String.sub digits 1 (n - 1)
        in
        Printf.sprintf "%se%s%d" mant (if exp10 >= 0 then "+" else "-")
          (abs exp10)
    in
    (if neg then "-" else "") ^ body

(** {1 Canonicalisation} *)

let to_string (json : Yojson.Basic.t) : (string, error) result =
  let buf = Buffer.create 256 in
  let path_of rev_path =
    if rev_path = [] then "$"
    else "$" ^ String.concat "" (List.rev rev_path)
  in
  let rec go path (v : Yojson.Basic.t) : (unit, error) result =
    match v with
    | `Null -> Buffer.add_string buf "null"; Ok ()
    | `Bool b -> Buffer.add_string buf (if b then "true" else "false"); Ok ()
    | `Int i -> Buffer.add_string buf (string_of_int i); Ok ()
    | `Float f ->
      if Float.is_nan f || Float.is_infinite f then
        Error (NaN_or_infinity (path_of path))
      else begin
        Buffer.add_string buf (es6_number f);
        Ok ()
      end
    | `String s -> (
      (* eager validation: utf16_units raises on malformed Unicode *)
      match utf16_units s with
      | _ ->
        Buffer.add_string buf (escape_string s);
        Ok ()
      | exception Invalid_argument _ -> Error (Lone_surrogate (path_of path)))
    | `List items ->
      Buffer.add_char buf '[';
      let rec walk i = function
        | [] -> Ok ()
        | item :: rest ->
          if i > 0 then Buffer.add_char buf ',';
          (match go (Printf.sprintf "[%d]" i :: path) item with
           | Ok () -> walk (i + 1) rest
           | Error e -> Error e)
      in
      (match walk 0 items with
       | Ok () -> Buffer.add_char buf ']'; Ok ()
       | Error e -> Error e)
    | `Assoc members ->
      let keys = List.map fst members in
      let sorted = List.sort_uniq compare keys in
      if List.length sorted <> List.length keys then begin
        let seen = Hashtbl.create 16 in
        let dup = ref None in
        List.iter
          (fun k ->
            if Hashtbl.mem seen k && !dup = None then dup := Some k
            else Hashtbl.replace seen k ())
          keys;
        Error (Duplicate_key (path_of path, match !dup with Some k -> k | None -> ""))
      end else begin
        let ordered = List.sort (fun (a, _) (b, _) -> compare_keys a b) members in
        Buffer.add_char buf '{';
        let rec walk i = function
          | [] -> Ok ()
          | (k, value) :: rest ->
            if i > 0 then Buffer.add_char buf ',';
            (match utf16_units k with
             | _ ->
               Buffer.add_string buf (escape_string k);
               Buffer.add_char buf ':';
               (match go (Printf.sprintf ".%s" k :: path) value with
                | Ok () -> walk (i + 1) rest
                | Error e -> Error e)
             | exception Invalid_argument _ -> Error (Lone_surrogate (path_of path)))
        in
        (match walk 0 ordered with
         | Ok () -> Buffer.add_char buf '}'; Ok ()
         | Error e -> Error e)
      end
  in
  match go [] json with
  | Ok () -> Ok (Buffer.contents buf)
  | Error e -> Error e

(** Canonicalise, treating an I-JSON violation as a compiler bug: the compiler
    builds every document it emits, so a violation here means it produced
    something it promised never to produce. Failing loudly beats shipping
    non-canonical bytes that a content-addressed consumer would silently
    accept as different. *)
let to_string_exn (json : Yojson.Basic.t) : string =
  match to_string json with
  | Ok s -> s
  | Error e -> invalid_arg ("Jcs.to_string_exn: " ^ error_to_string e)
