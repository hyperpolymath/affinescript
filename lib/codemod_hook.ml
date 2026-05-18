(* CODEMOD INSTRUMENTATION — branch stage-c/codemod only, never merged
   (affinescript#218). The instrumented parser calls [note] with the
   byte position of every record-literal LBRACE it matches; the #{
   migration codemod reads [brace_offsets] to insert '#' there. *)

let brace_offsets : int list ref = ref []
let note (p : Lexing.position) =
  brace_offsets := p.Lexing.pos_cnum :: !brace_offsets
