(* SPDX-License-Identifier: MIT OR AGPL-3.0-or-later *)
(* Single source of truth for the compiler version string.
 *
 * Replaces the five hardcoded "0.1.0" sites that used to drift behind
 * the release tag (issue #297):
 *
 *   bin/main.ml         — `affinescript --version`
 *   lib/repl.ml         — REPL banner
 *   lib/lsp_server.ml   — LSP `initialize` response
 *   lib/onnx_codegen.ml — ONNX `producer_version` field on emitted models
 *
 * The value below is baked at *build* time by the release workflow:
 * `.github/workflows/release.yml` sed-substitutes this line (and the
 * `(version …)` field in `dune-project`) to match `${GITHUB_REF_NAME}`
 * before running `dune build --release`.  When building from a non-
 * release commit, this reflects whatever was last committed to main,
 * matching `dune-project`'s declared version. *)

let value = "0.1.1"
