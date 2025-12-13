(** Source location tracking for error messages *)

type pos = {
  line : int;
  col : int;
  offset : int;
}
[@@deriving show, eq, ord]

type t = {
  start_pos : pos;
  end_pos : pos;
  file : string;
}
[@@deriving show, eq, ord]

let dummy = {
  start_pos = { line = 0; col = 0; offset = 0 };
  end_pos = { line = 0; col = 0; offset = 0 };
  file = "<dummy>";
}

let make ~file ~start_pos ~end_pos = { start_pos; end_pos; file }

let merge s1 s2 =
  if s1.file <> s2.file then
    invalid_arg "Span.merge: different files"
  else
    { start_pos = s1.start_pos; end_pos = s2.end_pos; file = s1.file }

let pp_short fmt span =
  Format.fprintf fmt "%s:%d:%d" span.file span.start_pos.line span.start_pos.col
