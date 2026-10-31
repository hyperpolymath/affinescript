(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) *)

(** JCS canonical JSON: the vectors that pin {!Jcs}.

    The expectations are the output of an independent JCS implementation
    (node's [JSON.stringify] for ES6 string escaping and [Number::toString],
    default [Array.prototype.sort] for UTF-16 key order), which makes them a
    spec oracle rather than a snapshot of whatever this code happened to do.
    They cover the three places JCS and "just JSON" disagree: key order above
    the BMP, number spelling at both ES6 switch points, and control-character
    escaping. See docs/reference/JSON-AND-YAML.adoc. *)

let open Yojson.Basic

(* U+20AC, U+00E9, U+1F600, U+E000, U+FFFD *)
let euro = "\226\130\172"
let eacute = "\195\169"
let emoji = "\240\159\152\128"
let pua = "\238\128\128"
let replacement = "\239\191\189"

let canonical_inputs : (string * Yojson.Basic.t * string) list = [
  ( "RFC 8785 numbers + string escaping"
  , `Assoc
      [ ("literals", `List [ `Null; `Bool true; `Bool false ])
      ; ("numbers",
         `List
           [ `Float 333333333.33333329
           ; `Float 1e30
           ; `Float 4.5
           ; `Float 2e-3
           ; `Float 1e-27
           ])
      ; ("string", `String (euro ^ "$\015\nA'B\"\\\\\"/"))
      ]
  , "{\"literals\":[null,true,false],\"numbers\":[333333333.3333333,1e+30,4.5,0.002,1e-27],\"string\":\"\226\130\172$\\u000f\\nA'B\\\"\\\\\\\\\\\"/\"}" )
  ( "keys sort by UTF-16 code units, not by insertion order"
  , `Assoc [ ("b", `Int 1); ("a", `Int 2); ("A", `Int 3); ("aa", `Int 4); ("", `Int 5) ]
  , "{\"\":5,\"A\":3,\"a\":2,\"aa\":4,\"b\":1}" )
  ( "nested objects and non-ASCII keys pass through verbatim"
  , `Assoc
      [ ("z", `List [ `Int 1; `Int 2; `Assoc [ ("y", `Null) ] ])
      ; (eacute, `Int 1)
      ; ("e", `Int 2)
      ]
  , "{\"e\":2,\"z\":[1,2,{\"y\":null}],\"\195\169\":1}" )
  ( "an astral key precedes U+E000 (the case UTF-8 byte order gets wrong)"
  , `Assoc
      [ ("astral", `Int 1); (emoji, `Int 2); (replacement, `Int 3); (pua, `Int 4) ]
  , "{\"astral\":1,\"\240\159\152\128\":2,\"\238\128\128\":4,\"\239\191\189\":3}" )
  ( "ES6 number placement: 1e21 vs 1e20, 1e-7 vs 1e-6, subnormals, -0"
  , `List
      [ `Int 0
      ; `Float (-0.)
      ; `Float 1e21
      ; `Float 1e20
      ; `Float 1e-7
      ; `Float 1e-6
      ; `Float 5e-324
      ; `Float 1.7976931348623157e308
      ; `Float 123456789012345680000.
      ; `Float (-2.5)
      ; `Float 0.1
      ]
  , "[0,0,1e+21,100000000000000000000,1e-7,0.000001,5e-324,1.7976931348623157e+308,123456789012345680000,-2.5,0.1]" )
  ( "nested reshaping keeps canonical order at every level"
  , `Assoc
      [ ( "nested",
          `Assoc
            [ ("k2", `List [ `Bool true; `String "x" ])
            ; ("k1", `Assoc [ ("z", `Int 1); ("y", `List [ `Null ]) ])
            ] )
      ]
  , "{\"nested\":{\"k1\":{\"y\":[null],\"z\":1},\"k2\":[true,\"x\"]}}" )
  ]

(** I-JSON rejections: the document must never be emitted. *)
let rejected : (string * Yojson.Basic.t) list =
  [ ("NaN", `Float nan)
  ; ("infinity", `Float infinity)
  ; ("duplicate keys", `Assoc [ ("a", `Int 1); ("a", `Int 2) ])
  ; ("lone surrogate", `String "\237\160\128")
  ; ("truncated UTF-8", `String "\226\130")
  ]

let test_canonical (name, input, expected) () =
  match Jcs.to_string input with
  | Ok actual -> Alcotest.(check string) name expected actual
  | Error e -> Alcotest.failf "%s: canonicalisation failed: %s" name (Jcs.error_to_string e)

let test_rejected (name, input) () =
  match Jcs.to_string input with
  | Ok actual -> Alcotest.failf "%s must be rejected by I-JSON but was emitted as %s" name actual
  | Error _ -> ()

let tests =
  [ ( "canonical serialisation",
      List.map (fun v -> Alcotest.test_case (let n, _, _ = v in n) `Quick (test_canonical v))
        canonical_inputs )
  ; ( "I-JSON rejections",
      List.map (fun v -> Alcotest.test_case (let n, _ = v in n) `Quick (test_rejected v)) rejected )
  ]
