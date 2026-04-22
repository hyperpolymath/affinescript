# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) {{CURRENT_YEAR}} hyperpolymath (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# K9 Validator: Methodology Guard
# Checks that agent work respects methodology constraints declared in
# agent_instructions/methodology.a2ml.
#
# Usage: k9 validate methodology-guard

let methodology_guard = {
  name = "methodology-guard",
  version = "1.0.0",
  description = "Validates that agent work respects declared methodology constraints",

  checks = {
    divergent_invariant_language = {
      description = "No files in languages violating the divergent language invariant",
      severity = "error",
      # When methodology.divergent-invariants.language-invariant is set,
      # check that no new files introduce a different language for that purpose.
      # Example: if language-invariant = "idris2", reject new .lean or .v files
      # in the proof directories.
      check_type = "file-extension-guard",
      scope = "src/",
    },

    believe_me_ceiling = {
      description = "believe_me count must not exceed declared ceiling",
      severity = "error",
      pattern = "believe_me",
      ceiling_key = "methodology.divergent-invariants.believe-me-ceiling",
      default_ceiling = 0,
    },

    assert_total_ceiling = {
      description = "assert_total count must not exceed declared ceiling",
      severity = "error",
      pattern = "assert_total",
      ceiling_key = "methodology.divergent-invariants.assert-total-ceiling",
      default_ceiling = 0,
    },

    state_not_template = {
      description = "STATE.a2ml must not contain template placeholders",
      severity = "warning",
      file = ".machine_readable/6a2/STATE.a2ml",
      reject_patterns = ["{{PLACEHOLDER}}", "{{PROJECT}}", "rsr-template-repo"],
    },

    coverage_updated = {
      description = "coverage.a2ml should be updated within 30 days",
      severity = "info",
      file = ".machine_readable/agent_instructions/coverage.a2ml",
      staleness_days = 30,
    },
  },
}
in methodology_guard
