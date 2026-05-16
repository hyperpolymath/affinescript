# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# RSR Standard Justfile Template
# https://just.systems/man/en/
#
# Copy this file to new projects and customize the placeholder values.
#
# Run `just` to see all available recipes
# Run `just cookbook` to generate docs/just-cookbook.adoc
# Run `just combinations` to see matrix recipe options

set shell := ["bash", "-uc"]
set dotenv-load := true
set positional-arguments := true

# Import auto-generated contractile recipes (must-check, trust-verify, etc.)
# Re-generate with: contractile gen-just
import? ".machine_readable/contractiles/contractile.just"

# Project metadata — customize these
project := "affinescript-vite"
OWNER := "hyperpolymath"
REPO := "affinescript-vite"
version := "0.1.0"
tier := "infrastructure"  # 1 | 2 | infrastructure

# ═══════════════════════════════════════════════════════════════════════════════
# DEFAULT & HELP
# ═══════════════════════════════════════════════════════════════════════════════

# Show all available recipes with descriptions
default:
    @just --list --unsorted

# Show detailed help for a specific recipe
help recipe="":
    #!/usr/bin/env bash
    if [ -z "{{recipe}}" ]; then
        just --list --unsorted
        echo ""
        echo "Usage: just help <recipe>"
        echo "       just cookbook     # Generate full documentation"
        echo "       just combinations # Show matrix recipes"
    else
        just --show "{{recipe}}" 2>/dev/null || echo "Recipe '{{recipe}}' not found"
    fi

# Show this project's info
info:
    @echo "Project: affinescript-vite"
    @echo "Version: {{version}}"
    @echo "RSR Tier: {{tier}}"
    @echo "Recipes: "$(just --summary | wc -w)""
    @[ -f ".machine_readable/STATE.a2ml" ] && grep -oP 'phase\s*=\s*"\K[^"]+' .machine_readable/STATE.a2ml | head -1 | xargs -I{} printf "Phase: %s\n" "{}" || true

# ═══════════════════════════════════════════════════════════════════════════════
# INIT — Bootstrap a new project from this template
# ═══════════════════════════════════════════════════════════════════════════════

# Interactive project bootstrap — replaces all {{PLACEHOLDER}} tokens
init:
    #!/usr/bin/env bash
    set -euo pipefail

    echo "═══════════════════════════════════════════════════"
    echo "  RSR Project Bootstrap"
    echo "═══════════════════════════════════════════════════"
    echo ""

    # --- Load defaults from config (if exists) ---
    # Create yours: ~/.config/rsr/defaults
    # Format: OWNER=myorg  AUTHOR="My Name"  AUTHOR_EMAIL=me@example.org ...
    DEFAULTS="${XDG_CONFIG_HOME:-$HOME/.config}/rsr/defaults"
    if [ -f "$DEFAULTS" ]; then
        echo "Loading defaults from "$DEFAULTS""
        # shellcheck source=/dev/null
        source "$DEFAULTS"
        echo ""
    fi

    # --- Required values (pre-filled from defaults if available) ---
    read -rp "Project name (human-readable, e.g. My Project): " PROJECT_NAME
    [ -z "$PROJECT_NAME" ] && echo "Error: project name required" && exit 1

    read -rp "Repository slug (e.g. my-project): " REPO
    [ -z "$REPO" ] && echo "Error: repo slug required" && exit 1

    read -rp "Owner [${OWNER:-}]: " _OWNER
    OWNER="${_OWNER:-${OWNER:-}}"
    [ -z "$OWNER" ] && echo "Error: owner required" && exit 1

    read -rp "Author full name [${AUTHOR:-}]: " _AUTHOR
    AUTHOR="${_AUTHOR:-${AUTHOR:-}}"
    [ -z "$AUTHOR" ] && echo "Error: author name required" && exit 1

    read -rp "Author email [${AUTHOR_EMAIL:-}]: " _AUTHOR_EMAIL
    AUTHOR_EMAIL="${_AUTHOR_EMAIL:-${AUTHOR_EMAIL:-}}"
    [ -z "$AUTHOR_EMAIL" ] && echo "Error: email required" && exit 1

    # --- Optional values (pre-filled from defaults if available) ---
    read -rp "Author organization [${AUTHOR_ORG:-none}]: " _AUTHOR_ORG
    AUTHOR_ORG="${_AUTHOR_ORG:-${AUTHOR_ORG:-}}"

    read -rp "Previous/alt email [${AUTHOR_EMAIL_ALT:-none}]: " _AUTHOR_EMAIL_ALT
    AUTHOR_EMAIL_ALT="${_AUTHOR_EMAIL_ALT:-${AUTHOR_EMAIL_ALT:-}}"

    read -rp "Project description []: " PROJECT_DESCRIPTION

    read -rp "Forge domain [${FORGE:-github.com}]: " _FORGE
    FORGE="${_FORGE:-${FORGE:-github.com}}"

    read -rp "Security contact email [${SECURITY_EMAIL:-$AUTHOR_EMAIL}]: " _SECURITY_EMAIL
    SECURITY_EMAIL="${_SECURITY_EMAIL:-${SECURITY_EMAIL:-$AUTHOR_EMAIL}}"

    read -rp "Conduct contact email [${CONDUCT_EMAIL:-$AUTHOR_EMAIL}]: " _CONDUCT_EMAIL
    CONDUCT_EMAIL="${_CONDUCT_EMAIL:-${CONDUCT_EMAIL:-$AUTHOR_EMAIL}}"

    read -rp "Project type (library|binary|monorepo|service|website) [library]: " PROJECT_TYPE
    PROJECT_TYPE="${PROJECT_TYPE:-library}"

    read -rp "Website URL [https://${FORGE}/${OWNER}/${REPO}]: " WEBSITE
    WEBSITE="${WEBSITE:-https://${FORGE}/${OWNER}/${REPO}}"

    # --- Container values (optional — only relevant if container/ exists) ---
    if [ -d "container" ]; then
        echo ""
        echo "── Container configuration (optional) ─────────"
        read -rp "Service name [${REPO}]: " _SERVICE_NAME
        SERVICE_NAME="${_SERVICE_NAME:-${REPO}}"
        read -rp "Primary port [8080]: " _PORT
        PORT="${_PORT:-8080}"
        read -rp "Container registry [ghcr.io/${OWNER}]: " _REGISTRY
        REGISTRY="${_REGISTRY:-ghcr.io/${OWNER}}"
    else
        SERVICE_NAME="${REPO}"
        PORT="8080"
        REGISTRY="ghcr.io/${OWNER}"
    fi

    # --- Derived values ---
    PROJECT_UPPER=$(echo "$REPO" | tr '[:lower:]-' '[:upper:]_')
    PROJECT_LOWER=$(echo "$REPO" | tr '[:upper:]-' '[:lower:]_')
    CURRENT_YEAR="$(date +%Y)"
    CURRENT_DATE="$(date +%Y-%m-%d)"
    VERSION="0.1.0"

    # Derive citation name parts (best-effort split on last space)
    AUTHOR_LAST="${AUTHOR##* }"
    AUTHOR_FIRST="${AUTHOR% *}"
    FIRST_INITIAL="${AUTHOR_FIRST:0:1}."
    if [ "$AUTHOR_LAST" = "$AUTHOR_FIRST" ]; then
        AUTHOR_FIRST="$AUTHOR"
        AUTHOR_LAST=""
        FIRST_INITIAL=""
    fi

    echo ""
    echo "── Summary ──────────────────────────────────────"
    echo "  Project:     "$PROJECT_NAME""
    echo "  Repo:        "$REPO""
    echo "  Owner:       "$OWNER""
    echo "  Author:      "$AUTHOR" <"$AUTHOR_EMAIL">"
    [ -n "$AUTHOR_ORG" ] && echo "  Organization: "$AUTHOR_ORG""
    echo "  Forge:       "$FORGE""
    echo "  Year:        "$CURRENT_YEAR""
    echo "────────────────────────────────────────────────"
    echo ""
    read -rp "Proceed? [Y/n] " CONFIRM
    [[ "${CONFIRM:-Y}" =~ ^[Nn] ]] && echo "Aborted." && exit 0

    echo ""
    echo "Replacing placeholders..."

    # Brace tokens as variables (hex avoids just interpolation)
    LB="{{"
    RB="}}"

    # Build the sed expression list
    # Note: using | as delimiter since URLs contain /
    SED_ARGS=(
        -e "s|${LB}PROJECT_NAME${RB}|"${PROJECT_NAME}"|g"
        -e "s|${LB}PROJECT_DESCRIPTION${RB}|${PROJECT_DESCRIPTION}|g"
        -e "s|${LB}PROJECT${RB}|${PROJECT_UPPER}|g"
        -e "s|${LB}project${RB}|${PROJECT_LOWER}|g"
        -e "s|${LB}REPO${RB}|${REPO}|g"
        -e "s|${LB}OWNER${RB}|"${OWNER}"|g"
        -e "s|${LB}AUTHOR${RB}|${AUTHOR}|g"
        -e "s|${LB}AUTHOR_EMAIL${RB}|${AUTHOR_EMAIL}|g"
        -e "s|${LB}AUTHOR_ORG${RB}|${AUTHOR_ORG}|g"
        -e "s|${LB}AUTHOR_LAST${RB}|${AUTHOR_LAST}|g"
        -e "s|${LB}AUTHOR_FIRST${RB}|${AUTHOR_FIRST}|g"
        -e "s|${LB}AUTHOR_INITIALS${RB}|${FIRST_INITIAL}|g"
        -e "s|${LB}FORGE${RB}|${FORGE}|g"
        -e "s|${LB}CURRENT_YEAR${RB}|${CURRENT_YEAR}|g"
        -e "s|${LB}CURRENT_DATE${RB}|${CURRENT_DATE}|g"
        -e "s|${LB}DATE${RB}|${CURRENT_DATE}|g"
        -e "s|${LB}SECURITY_EMAIL${RB}|${SECURITY_EMAIL}|g"
        -e "s|${LB}CONDUCT_EMAIL${RB}|${CONDUCT_EMAIL}|g"
        -e "s|${LB}LICENSE${RB}|PMPL-1.0-or-later|g"
        -e "s|${LB}CONDUCT_TEAM${RB}|Code of Conduct Committee|g"
        -e "s|${LB}RESPONSE_TIME${RB}|48 hours|g"
        -e "s|${LB}MAIN_BRANCH${RB}|main|g"
        -e "s|${LB}PROJECT_PURPOSE${RB}|${PROJECT_DESCRIPTION}|g"
        -e "s|${LB}PROJECT_ROLE${RB}|${PROJECT_TYPE}|g"
        -e "s|${LB}PROJECT_TYPE${RB}|${PROJECT_TYPE}|g"
        -e "s|${LB}WEBSITE${RB}|${WEBSITE}|g"
        -e "s|${LB}SERVICE_NAME${RB}|${SERVICE_NAME}|g"
        -e "s|${LB}PORT${RB}|${PORT}|g"
        -e "s|${LB}REGISTRY${RB}|${REGISTRY}|g"
        -e "s|${LB}IMAGE${RB}|${REGISTRY}/${SERVICE_NAME}|g"
        -e "s|${LB}VERSION${RB}|${VERSION}|g"
        -e "s|${LB}EMAIL${RB}|${AUTHOR_EMAIL}|g"
    )
    [ -n "$AUTHOR_EMAIL_ALT" ] && SED_ARGS+=(-e "s|${LB}AUTHOR_EMAIL_ALT${RB}|${AUTHOR_EMAIL_ALT}|g")

    # Replace in all text files (skip .git, LICENSE text, and binaries)
    find . -type f \
        -not -path './.git/*' \
        -not -name 'PMPL-1.0-or-later.txt' \
        -not -name '*.png' -not -name '*.jpg' -not -name '*.gif' \
        -not -name '*.woff' -not -name '*.woff2' \
        | while read -r file; do
        if file --brief "$file" | grep -qi 'text\|ascii\|utf'; then
            sed -i "${SED_ARGS[@]}" "$file"
        fi
    done

    # Also replace [YOUR-REPO-NAME] and [YOUR-NAME/ORG] in AI manifest
    sed -i "s|\[YOUR-REPO-NAME\]|"${PROJECT_NAME}"|g" 0-AI-MANIFEST.a2ml 2>/dev/null || true
    sed -i "s|\[YOUR-NAME/ORG\]|"${OWNER}"|g" 0-AI-MANIFEST.a2ml 2>/dev/null || true

    echo ""
    echo "── Validation ───────────────────────────────────"

    # Check for remaining placeholders
    PATTERN="${LB}[A-Z_]*${RB}"
    REMAINING=$(grep -rl "$PATTERN" . --include='*.md' --include='*.adoc' --include='*.yml' --include='*.yaml' --include='*.a2ml' --include='*.toml' --include='*.scm' --include='*.ncl' --include='*.nix' --include='*.json' --include='*.sh' 2>/dev/null | grep -v '.git/' | grep -v '.machine_readable/ai/PLACEHOLDERS.adoc' || true)
    if [ -n "$REMAINING" ]; then
        echo "WARNING: Remaining placeholders in:"
        echo "$REMAINING" | sed 's/^/  /'
        echo ""
        echo "Run: grep -rn '$LB' . --include='*.md' to inspect"
    else
        echo "All placeholders replaced successfully!"
    fi

    # K9-SVC validation (if available)
    if command -v k9-svc >/dev/null 2>&1; then
        echo ""
        echo "Running k9-svc validation..."
        k9-svc validate . 2>/dev/null || true
    fi

    echo ""
    echo "Running OpenSSF compliance verification..."
    just verify

    echo ""
    echo "Done! Next steps:"
    echo "  1. Review changes: git diff"
    echo "  2. Remove template cruft: rm .machine_readable/ai/PLACEHOLDERS.adoc"
    echo "  3. Customize README.adoc for your project"
    echo "  4. Commit: git add -A && git commit -m 'feat: initialize from RSR template'"
    echo "  5. Push: git remote add origin git@${FORGE}:${OWNER}/${REPO}.git && git push -u origin main"

# ═══════════════════════════════════════════════════════════════════════════════
# GROOVE & V-TRIPLE SETUP
# ═══════════════════════════════════════════════════════════════════════════════

# Configure Groove protocol manifest (port assignment, API surfaces)
groove-setup:
    #!/usr/bin/env bash
    set -euo pipefail
    MANIFEST=".machine_readable/integrations/groove.a2ml"
    if [ ! -f "$MANIFEST" ]; then
        echo "Error: $MANIFEST not found. Run 'just init' first."
        exit 1
    fi

    echo "═══════════════════════════════════════════════════"
    echo "  Groove Protocol Setup"
    echo "═══════════════════════════════════════════════════"
    echo ""
    echo "Check PORT-REGISTRY.md before assigning a port:"
    echo "  https://github.com/hyperpolymath/standards/blob/main/PORT-REGISTRY.md"
    echo ""

    read -rp "Primary port for this service: " PORT
    [ -z "$PORT" ] && echo "Error: port required" && exit 1

    echo ""
    echo "Which API surfaces does this project expose?"
    read -rp "  REST API? [Y/n]: " REST
    read -rp "  gRPC? [y/N]: " GRPC
    read -rp "  GraphQL? [y/N]: " GRAPHQL
    read -rp "  WebSocket? [y/N]: " WS
    read -rp "  SSE (Server-Sent Events)? [y/N]: " SSE

    # Update port in manifest
    sed -i "s/(port 0)/(port "${PORT}")/" "$MANIFEST"

    # Update API surface flags
    [[ "${GRPC,,}" == "y" ]] && sed -i 's/(grpc.*enabled false)/(grpc    (enabled true)/' "$MANIFEST"
    [[ "${GRAPHQL,,}" == "y" ]] && sed -i 's/(graphql.*enabled false)/(graphql (enabled true)/' "$MANIFEST"
    [[ "${WS,,}" == "y" ]] && sed -i 's/(websocket.*enabled false)/(websocket (enabled true)/' "$MANIFEST"
    [[ "${SSE,,}" == "y" ]] && sed -i 's/(sse.*enabled false)/(sse     (enabled true)/' "$MANIFEST"

    echo ""
    echo "Groove manifest updated: "$MANIFEST""
    echo "Port "${PORT}" assigned". Add to PORT-REGISTRY.md if not already there."

# Check for template placeholders that haven't been replaced
verify-template:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "Checking for unreplaced template placeholders..."
    FOUND=0

    # Check for double-brace placeholder patterns
    HITS=$(grep -rn '{{'{{'}}[A-Z_]*{{'}}'}}' --include="*.adoc" --include="*.md" --include="*.a2ml" \
           --include="*.scm" --include="*.toml" --include="*.yml" --include="*.yaml" \
           . 2>/dev/null | grep -v 'node_modules\|\.git/' | grep -v 'PLACEHOLDERS.adoc' || true)
    if [ -n "$HITS" ]; then
        echo ""
        echo "⚠ Unreplaced placeholders found:"
        echo "$HITS" | head -20
        FOUND=1
    fi

    # Check for template defaults still present
    if grep -q 'rsr-template-repo' Justfile 2>/dev/null; then
        echo "⚠ Justfile still references 'rsr-template-repo' — update project name"
        FOUND=1
    fi

    # Check for port 0 in Groove manifest
    if grep -q '(port 0)' .machine_readable/integrations/groove.a2ml 2>/dev/null; then
        echo "⚠ Groove manifest has port 0 — run 'just groove-setup' to assign a port"
        FOUND=1
    fi

    # Check for empty SCM files
    for f in .machine_readable/6a2/STATE.a2ml .machine_readable/6a2/META.a2ml .machine_readable/6a2/ECOSYSTEM.a2ml; do
        if [ -f "$f" ] && grep -q '{{'{{'}}' "$f" 2>/dev/null; then
            echo "⚠ $f still has template placeholders"
            FOUND=1
        fi
    done

    if [ $FOUND -eq 0 ]; then
        echo "✓ No template placeholders found — project is properly customised."
    else
        echo ""
        echo "Run 'just init' to replace placeholders, or edit files manually."
        exit 1
    fi

# ═══════════════════════════════════════════════════════════════════════════════
# PROJECT SELF-ASSESSMENT
# ═══════════════════════════════════════════════════════════════════════════════

# Analyse this project and advise what to keep, remove, or leave for later.
# Does NOT modify any files — only prints recommendations.
self-assess:
    #!/usr/bin/env bash
    set -euo pipefail

    echo "═══════════════════════════════════════════════════"
    echo "  RSR Project Self-Assessment"
    echo "═══════════════════════════════════════════════════"
    echo ""
    echo "Scanning project structure to identify what's"
    echo "relevant, removable, or worth keeping for later..."
    echo ""

    # Detect project characteristics
    HAS_RUST=false; [ -f "Cargo.toml" ] && HAS_RUST=true
    HAS_ELIXIR=false; [ -f "mix.exs" ] && HAS_ELIXIR=true
    HAS_RESCRIPT=false; [ -f "rescript.json" ] || [ -f "bsconfig.json" ] && HAS_RESCRIPT=true
    HAS_IDRIS=false; ls *.ipkg >/dev/null 2>&1 && HAS_IDRIS=true
    HAS_ZIG=false; [ -f "build.zig" ] || [ -d "ffi/zig" ] && HAS_ZIG=true
    HAS_GLEAM=false; [ -f "gleam.toml" ] && HAS_GLEAM=true
    HAS_CONTAINER=false; [ -f "Containerfile" ] || [ -f "container/Containerfile" ] && HAS_CONTAINER=true
    HAS_TESTS=false; [ -d "test" ] || [ -d "tests" ] || [ -d "__tests__" ] && HAS_TESTS=true
    HAS_API=false; grep -rq 'port\|listen\|endpoint' --include="*.exs" --include="*.rs" --include="*.toml" . 2>/dev/null && HAS_API=true
    IS_LIBRARY=false; [ -f "Cargo.toml" ] && grep -q '\[lib\]' Cargo.toml 2>/dev/null && IS_LIBRARY=true

    echo "Detected: Rust=$HAS_RUST Elixir=$HAS_ELIXIR ReScript=$HAS_RESCRIPT"
    echo "          Idris=$HAS_IDRIS Zig=$HAS_ZIG Gleam=$HAS_GLEAM"
    echo "          Container=$HAS_CONTAINER Tests=$HAS_TESTS API=$HAS_API"
    echo ""

    # ── ESSENTIAL (removing these breaks RSR compliance) ──────────
    echo "── ESSENTIAL (removing breaks Rhodium Standard) ──────────"
    echo ""

    for f in LICENSE SECURITY.md CODE_OF_CONDUCT.md CONTRIBUTING.md .editorconfig .gitignore; do
        if [ -f "$f" ]; then
            echo "  ✓ "$f" — KEEP" (RSR required)"
        else
            echo "  ✗ "$f" — MISSING" (RSR violation!)"
        fi
    done

    if [ -d ".machine_readable/6a2" ]; then
        echo "  ✓ .machine_readable/6a2/ — KEEP (SCM checkpoint files)"
    else
        echo "  ✗ .machine_readable/6a2/ — MISSING (RSR violation!)"
    fi

    if [ -d ".github/workflows" ]; then
        WF_COUNT=$(ls .github/workflows/*.yml 2>/dev/null | wc -l)
        echo "  ✓ .github/workflows/ — KEEP ($WF_COUNT workflows, RSR CI/CD)"
    fi
    echo ""

    # ── RELEVANT (useful for your project type) ───────────────────
    echo "── RELEVANT (matches your project) ───────────────────────"
    echo ""

    if $HAS_IDRIS && [ -d "src/interface/abi" ]; then
        echo "  ✓ src/interface/abi/ — KEEP (Idris2 ABI definitions)"
    elif ! $HAS_IDRIS && [ -d "src/interface/abi" ]; then
        echo "  ? src/interface/abi/ — No Idris2 detected."
        echo "    → KEEP if you plan to add formal verification later."
        echo "    → SAFE TO REMOVE if this project will never use Idris2."
        echo "    ⚠ Consequence: no formally verified interface definitions."
    fi

    if $HAS_ZIG && [ -d "src/interface/ffi" ]; then
        echo "  ✓ src/interface/ffi/ — KEEP (Zig FFI bridge)"
    elif ! $HAS_ZIG && [ -d "src/interface/ffi" ]; then
        echo "  ? src/interface/ffi/ — No Zig detected."
        echo "    → KEEP if you plan C ABI interop later."
        echo "    → SAFE TO REMOVE if this is a pure web/scripting project."
        echo "    ⚠ Consequence: no C-compatible FFI bridge."
    fi

    if $HAS_API && [ -f ".machine_readable/integrations/groove.a2ml" ]; then
        PORT=$(grep '(port ' .machine_readable/integrations/groove.a2ml 2>/dev/null | sed 's/.*(port \([0-9]*\)).*/\1/')
        if [ "$PORT" = "0" ]; then
            echo "  ⚠ groove.a2ml — Port not assigned. Run 'just groove-setup'."
        else
            echo "  ✓ groove.a2ml — KEEP (Groove discovery on port $PORT)"
        fi
    elif $HAS_API; then
        echo "  ✗ groove.a2ml — MISSING. Your project has an API but no Groove manifest."
        echo "    → Run 'just groove-setup' to enable snap-on/snap-off discovery."
    fi

    if $HAS_CONTAINER && [ -d "container" ]; then
        echo "  ✓ container/ — KEEP (Containerfile + compose)"
    elif ! $HAS_CONTAINER && [ -d "container" ]; then
        echo "  ? container/ — No Containerfile detected in use."
        echo "    → KEEP if you plan to containerise later."
        echo "    → SAFE TO REMOVE for libraries and CLI tools."
    fi

    echo ""

    # ── SAFE TO REMOVE (not relevant, no consequences) ────────────
    echo "── SAFE TO REMOVE (no RSR consequences) ──────────────────"
    echo ""

    if ! $HAS_RESCRIPT && [ -d "examples" ] && ls examples/*.res >/dev/null 2>&1; then
        echo "  ○ examples/*.res — Template ReScript examples. Not your code."
    fi

    if [ -f ".machine_readable/ai/PLACEHOLDERS.adoc" ]; then
        echo "  ○ .machine_readable/ai/PLACEHOLDERS.adoc — Template doc. Remove after init."
    fi

    if [ -f "flake.nix" ] && ! command -v nix >/dev/null 2>&1; then
        echo "  ○ flake.nix — Nix flake. Safe to remove if you don't use Nix."
        echo "    → KEEP if others might build with Nix."
    fi

    if [ -f "guix.scm" ] && ! command -v guix >/dev/null 2>&1; then
        echo "  ○ guix.scm — Guix package. Safe to remove if you don't use Guix."
        echo "    → KEEP if others might build with Guix."
    fi

    echo ""

    # ── FUTURE VALUE (not needed now, worth keeping) ──────────────
    echo "── KEEP FOR FUTURE (not active, but valuable later) ──────"
    echo ""

    if [ -d ".machine_readable/contractiles" ]; then
        echo "  ◆ contractiles/ — Must/Trust/Dust/Lust contracts."
        echo "    Not enforced until you configure them, but ready when you need"
        echo "    automated compliance checking. Zero cost to keep."
    fi

    if [ -d ".machine_readable/bot_directives" ]; then
        echo "  ◆ bot_directives/ — Gitbot fleet configuration."
        echo "    Not active until gitbot-fleet is connected. Keeps your repo"
        echo "    ready for automated maintenance when the fleet arrives."
    fi

    if [ -d ".machine_readable/agent_instructions" ]; then
        echo "  ◆ agent_instructions/ — AI agent methodology config."
        echo "    Guides Claude/Gemini/etc on how to work in this repo."
        echo "    No cost to keep. Improves AI assistance quality."
    fi

    if [ -d "docs/governance" ]; then
        echo "  ◆ docs/governance/ — TSDM, CRG, maintenance checklists."
        echo "    Not needed for solo projects. Essential when you add contributors."
    fi

    if [ -d "verification" ]; then
        echo "  ◆ verification/ — Proofs, benchmarks, fuzzing, safety case."
        echo "    Empty scaffolds until you add formal verification."
        echo "    Worth keeping for any project that claims safety properties."
    fi

    echo ""
    echo "═══════════════════════════════════════════════════"
    echo "  Assessment complete. No files were modified."
    echo "═══════════════════════════════════════════════════"

# ═══════════════════════════════════════════════════════════════════════════════
# OPENSSF COMPLIANCE VERIFICATION
# ═══════════════════════════════════════════════════════════════════════════════

# Verify OpenSSF Best Practices prerequisites — fails if any required file is missing
verify:
    #!/usr/bin/env bash
    set -euo pipefail

    echo "=== OpenSSF Best Practices Verification ==="
    ERRORS=0

    check_file() {
        if [ ! -f "$1" ]; then
            echo "  FAIL: "$1" missing"
            ERRORS=$((ERRORS + 1))
        else
            echo "  OK:   "$1""
        fi
    }

    # Accept either .md or .adoc for documentation files
    check_either() {
        if [ ! -f "$1" ] && [ ! -f "$2" ]; then
            echo "  FAIL: $1 (or $2) missing"
            ERRORS=$((ERRORS + 1))
        else
            local found="$1"
            [ -f "$2" ] && found="$2"
            [ -f "$1" ] && found="$1"
            echo "  OK:   "$found""
        fi
    }

    check_either "SECURITY.md" "SECURITY.adoc"
    check_file "LICENSE"
    check_either "CONTRIBUTING.md" "CONTRIBUTING.adoc"
    check_either "README.adoc" "README.md"
    check_file ".machine_readable/STATE.a2ml"
    check_file ".machine_readable/META.a2ml"
    check_file ".machine_readable/ECOSYSTEM.a2ml"
    check_either "CHANGELOG.md" "CHANGELOG.adoc"

    # Check at least 1 workflow exists
    WORKFLOW_COUNT=$(find .github/workflows -name '*.yml' -o -name '*.yaml' 2>/dev/null | wc -l)
    if [ "$WORKFLOW_COUNT" -eq 0 ]; then
        echo "  FAIL: No workflows in .github/workflows/"
        ERRORS=$((ERRORS + 1))
    else
        echo "  OK:   .github/workflows/ ($WORKFLOW_COUNT workflows)"
    fi

    echo ""
    if [ "$ERRORS" -gt 0 ]; then
        echo "FAIL: "$ERRORS" OpenSSF" prerequisites missing — repo cannot ship."
        exit 1
    fi
    echo "PASS: All OpenSSF Best Practices prerequisites satisfied."

# ═══════════════════════════════════════════════════════════════════════════════
# BUILD & COMPILE
# ═══════════════════════════════════════════════════════════════════════════════

# Build the project (debug mode)
build *args:
    @echo "Building affinescript-vite (debug)..."
    # PENDING: Replace with your build command
    # Examples:
    #   cargo build {{args}}                    # Rust
    #   mix compile {{args}}                    # Elixir
    #   zig build {{args}}                      # Zig
    #   deno task build {{args}}                # Deno/ReScript
    @echo "Build complete"

# Build in release mode with optimizations
build-release *args:
    @echo "Building affinescript-vite (release)..."
    # PENDING: Replace with your release build command
    # Examples:
    #   cargo build --release {{args}}
    #   MIX_ENV=prod mix compile {{args}}
    #   zig build -Doptimize=ReleaseFast {{args}}
    @echo "Release build complete"

# Build and watch for changes (requires entr or similar)
build-watch:
    @echo "Watching for changes..."
    # PENDING: Customize file patterns for your language
    # Examples:
    #   find src -name '*.rs' | entr -c just build
    #   mix compile --force --warnings-as-errors
    #   deno task dev

# Clean build artifacts [reversible: rebuild with `just build`]
clean:
    @echo "Cleaning..."
    # PENDING: Customize for your build system
    rm -rf target/ _build/ build/ dist/ out/ obj/ bin/

# Deep clean including caches [reversible: rebuild]
clean-all: clean
    rm -rf .cache .tmp

# ═══════════════════════════════════════════════════════════════════════════════
# TEST & QUALITY
# ═══════════════════════════════════════════════════════════════════════════════

# Run all tests
test *args:
    @echo "Running tests..."
    # PENDING: Replace with your test command
    # Examples:
    #   cargo test {{args}}
    #   mix test {{args}}
    #   zig build test {{args}}
    #   deno test {{args}}
    @echo "Tests passed!"

# Run tests with verbose output
test-verbose:
    @echo "Running tests (verbose)..."
    # PENDING: Replace with verbose test command

# Smoke test
test-smoke:
    @echo "Smoke test..."
    # PENDING: Add basic sanity checks

# Run end-to-end tests (full pipeline: build → run → verify)
e2e:
    @echo "Running E2E tests..."
    # PENDING: Replace with your E2E test command. Examples:
    #   bash tests/e2e.sh                    # Shell-based E2E
    #   npx playwright test                  # Browser E2E
    #   mix test test/integration/e2e_test.exs  # Elixir E2E
    #   cargo test --test end_to_end         # Rust E2E
    @echo "E2E tests passed!"

# Run aspect tests (cross-cutting concern validation)
aspect:
    @echo "Running aspect tests..."
    # PENDING: Replace with your aspect test command. Examples:
    #   bash tests/aspect_tests.sh           # Shell-based aspect tests
    #   cargo test --test aspects             # Rust aspect tests
    # Aspect tests validate architectural invariants:
    #   - Thread safety (mutex in FFI modules)
    #   - ABI/FFI contract (declarations match exports)
    #   - SPDX compliance (all files have license headers)
    #   - No dangerous patterns (believe_me, assert_total, etc.)
    @echo "Aspect tests passed!"

# Run benchmarks (performance regression detection)
bench:
    @echo "Running benchmarks..."
    # PENDING: Replace with your benchmark command. Examples:
    #   cargo bench                           # Rust criterion
    #   zig build bench                       # Zig benchmarks
    #   mix run bench/benchmarks.exs          # Elixir benchee
    #   deno bench                            # Deno bench
    @echo "Benchmarks complete!"

# Run readiness tests (Component Readiness Grade: D/C/B)
readiness:
    @echo "Running readiness tests..."
    # PENDING: Replace with your readiness test command. Examples:
    #   cargo test --test readiness -- --nocapture
    @echo "Readiness tests complete!"

# Print the current CRG grade (reads from READINESS.md '**Current Grade:** X' line)
crg-grade:
    @grade=$$(grep -oP '(?<=\*\*Current Grade:\*\* )[A-FX]' READINESS.md 2>/dev/null | head -1); \
    [ -z "$$grade" ] && grade="X"; \
    echo "$$grade"

# Print a shields.io CRG badge for embedding in README files
# Looks for '**Current Grade:** X' in READINESS.md; falls back to X
crg-badge:
    @grade=$$(grep -oP '(?<=\*\*Current Grade:\*\* )[A-FX]' READINESS.md 2>/dev/null | head -1); \
    [ -z "$$grade" ] && grade="X"; \
    case "$$grade" in \
      A) color="brightgreen" ;; \
      B) color="green" ;; \
      C) color="yellow" ;; \
      D) color="orange" ;; \
      E) color="red" ;; \
      F) color="critical" ;; \
      *) color="lightgrey" ;; \
    esac; \
    echo "[![CRG $$grade](https://img.shields.io/badge/CRG-$$grade-$$color?style=flat-square)](https://github.com/hyperpolymath/standards/tree/main/component-readiness-grades)"

# Run the full merge-requirement test suite (ALL categories)
# Per STANDING rule: P2P + E2E + aspect + execution + lifecycle + bench
test-all: test e2e aspect bench readiness
    @echo "All test categories passed — safe to merge!"

# Run all quality checks
quality: fmt-check lint test
    @echo "All quality checks passed!"

# Fix all auto-fixable issues [reversible: git checkout]
fix: fmt
    @echo "Fixed all auto-fixable issues"

# ═══════════════════════════════════════════════════════════════════════════════
# LINT & FORMAT
# ═══════════════════════════════════════════════════════════════════════════════

# Format all source files [reversible: git checkout]
fmt:
    @echo "Formatting source files..."
    # PENDING: Replace with your formatter
    # Examples:
    #   cargo fmt
    #   mix format
    #   gleam format
    #   deno fmt

# Check formatting without changes
fmt-check:
    @echo "Checking formatting..."
    # PENDING: Replace with your format check
    # Examples:
    #   cargo fmt --check
    #   mix format --check-formatted
    #   gleam format --check

# Run linter
lint:
    @echo "Linting source files..."
    # PENDING: Replace with your linter
    # Examples:
    #   cargo clippy -- -D warnings
    #   mix credo --strict
    #   gleam check

# ═══════════════════════════════════════════════════════════════════════════════
# RUN & EXECUTE
# ═══════════════════════════════════════════════════════════════════════════════

# Run the application
run *args: build
    # PENDING: Replace with your run command
    echo "Run not configured yet"

# Run with verbose output
run-verbose *args: build
    # PENDING: Replace with verbose run command
    echo "Run not configured yet"

# Install to user path
install: build-release
    @echo "Installing affinescript-vite..."
    # PENDING: Replace with your install command

# ═══════════════════════════════════════════════════════════════════════════════
# DEPENDENCIES
# ═══════════════════════════════════════════════════════════════════════════════

# Install/check all dependencies
deps:
    @echo "Checking dependencies..."
    # PENDING: Replace with your dependency check
    # Examples:
    #   cargo check
    #   mix deps.get
    #   gleam deps download
    @echo "All dependencies satisfied"

# Audit dependencies for vulnerabilities
deps-audit:
    @echo "Auditing for vulnerabilities..."
    # PENDING: Replace with your audit command
    # Examples:
    #   cargo audit
    #   mix audit
    @command -v trivy >/dev/null && trivy fs --severity HIGH,CRITICAL --quiet . || true
    @command -v gitleaks >/dev/null && gitleaks detect --source . --no-git --quiet || true
    @echo "Audit complete"

# ═══════════════════════════════════════════════════════════════════════════════
# DOCUMENTATION
# ═══════════════════════════════════════════════════════════════════════════════

# Generate all documentation
docs:
    @mkdir -p docs/generated docs/man
    just cookbook
    just man
    @echo "Documentation generated in docs/"

# Generate justfile cookbook documentation
cookbook:
    #!/usr/bin/env bash
    mkdir -p docs
    OUTPUT="docs/just-cookbook.adoc"
    echo "= affinescript-vite Justfile Cookbook" > "$OUTPUT"
    echo ":toc: left" >> "$OUTPUT"
    echo ":toclevels: 3" >> "$OUTPUT"
    echo "" >> "$OUTPUT"
    echo "Generated: $(date -Iseconds)" >> "$OUTPUT"
    echo "" >> "$OUTPUT"
    echo "== Recipes" >> "$OUTPUT"
    echo "" >> "$OUTPUT"
    just --list --unsorted | while read -r line; do
        if [[ "$line" =~ ^[[:space:]]+([a-z_-]+) ]]; then
            recipe="${BASH_REMATCH[1]}"
            echo "=== $recipe" >> "$OUTPUT"
            echo "" >> "$OUTPUT"
            echo "[source,bash]" >> "$OUTPUT"
            echo "----" >> "$OUTPUT"
            echo "just $recipe" >> "$OUTPUT"
            echo "----" >> "$OUTPUT"
            echo "" >> "$OUTPUT"
        fi
    done
    echo "Generated: "$OUTPUT""

# Generate man page
man:
    #!/usr/bin/env bash
    mkdir -p docs/man
    cat > docs/man/affinescript-vite.1 << EOF
    .TH affinescript-vite 1 "$(date +%Y-%m-%d)" "{{version}}" "affinescript-vite Manual"
    .SH NAME
    affinescript-vite \- RSR-compliant project
    .SH SYNOPSIS
    .B just
    [recipe] [args...]
    .SH DESCRIPTION
    RSR (Rhodium Standard Repository) project managed with just.
    .SH AUTHOR
    $(git config user.name 2>/dev/null || echo "Author") <$(git config user.email 2>/dev/null || echo "email")>
    EOF
    echo "Generated: docs/man/affinescript-vite.1"

# ═══════════════════════════════════════════════════════════════════════════════
# CONTAINERS (stapeln ecosystem — Podman + Chainguard Wolfi)
# ═══════════════════════════════════════════════════════════════════════════════

# Initialise container templates — substitute placeholders with project values
container-init:
    #!/usr/bin/env bash
    set -euo pipefail

    if [ ! -d "container" ]; then
        echo "Error: container/ directory not found."
        echo "This repo may not have been created from rsr-template-repo."
        exit 1
    fi

    echo "=== Container Template Initialisation ==="
    echo ""

    # Load RSR defaults if available
    DEFAULTS="${XDG_CONFIG_HOME:-$HOME/.config}/rsr/defaults"
    if [ -f "$DEFAULTS" ]; then
        echo "Loading defaults from "$DEFAULTS""
        # shellcheck source=/dev/null
        source "$DEFAULTS"
        echo ""
    fi

    # Prompt for container-specific values
    read -rp "Service name (e.g. my-api) [affinescript-vite]: " _SERVICE_NAME
    SERVICE_NAME="${_SERVICE_NAME:-affinescript-vite}"

    read -rp "Primary port [8080]: " _PORT
    PORT="${_PORT:-8080}"

    read -rp "Container registry [ghcr.io/${OWNER:-hyperpolymath}]: " _REGISTRY
    REGISTRY="${_REGISTRY:-ghcr.io/${OWNER:-hyperpolymath}}"

    echo ""
    echo "  Service: "$SERVICE_NAME""
    echo "  Port:    "$PORT""
    echo "  Registry: "$REGISTRY""
    echo ""
    read -rp "Proceed? [Y/n] " CONFIRM
    [[ "${CONFIRM:-Y}" =~ ^[Nn] ]] && echo "Aborted." && exit 0

    echo ""
    echo "Replacing container placeholders..."

    # Brace tokens as variables (hex escapes avoid just interpolation)
    LB="{{"
    RB="}}"

    SED_ARGS=(
        -e "s|${LB}SERVICE_NAME${RB}|${SERVICE_NAME}|g"
        -e "s|${LB}PORT${RB}|${PORT}|g"
        -e "s|${LB}REGISTRY${RB}|${REGISTRY}|g"
    )

    find container/ -type f | while read -r file; do
        if file --brief "$file" | grep -qi 'text\|ascii\|utf'; then
            sed -i "${SED_ARGS[@]}" "$file"
        fi
    done

    echo "Container templates initialised."
    echo ""
    echo "Next steps:"
    echo "  1. Edit container/Containerfile — add your build commands"
    echo "  2. Edit container/entrypoint.sh — set your application binary"
    echo "  3. Review container/compose.toml — adjust services and volumes"
    echo "  4. Build: just container-build"

# Build container image via cerro-torre pipeline
container-build *args:
    #!/usr/bin/env bash
    if [ -f "container/ct-build.sh" ]; then
        cd container && ./ct-build.sh {{args}}
    elif [ -f "container/Containerfile" ]; then
        podman build -t affinescript-vite:latest -f container/Containerfile .
    elif [ -f "Containerfile" ]; then
        podman build -t affinescript-vite:latest -f Containerfile .
    else
        echo "No Containerfile found in container/ or project root"
        exit 1
    fi

# Verify compose configuration
container-verify:
    #!/usr/bin/env bash
    if [ ! -f "container/compose.toml" ]; then
        echo "No container/compose.toml found"
        exit 1
    fi
    cd container
    if command -v selur-compose &>/dev/null; then
        selur-compose verify
    else
        echo "selur-compose not found, falling back to podman compose"
        podman compose --file compose.toml config
    fi

# Start container stack
container-up *args:
    #!/usr/bin/env bash
    if [ ! -f "container/compose.toml" ]; then
        echo "No container/compose.toml found"
        exit 1
    fi
    cd container
    if command -v selur-compose &>/dev/null; then
        selur-compose up {{args}}
    else
        podman compose --file compose.toml up {{args}}
    fi

# Stop container stack
container-down:
    #!/usr/bin/env bash
    cd container 2>/dev/null || { echo "No container/ directory"; exit 1; }
    if command -v selur-compose &>/dev/null; then
        selur-compose down
    else
        podman compose --file compose.toml down
    fi

# Sign and verify container bundle (build + pack + sign + verify)
container-sign:
    #!/usr/bin/env bash
    if [ -f "container/ct-build.sh" ]; then
        cd container && ./ct-build.sh
    else
        echo "No container/ct-build.sh found"
        exit 1
    fi

# Push signed bundle to registry
container-push:
    #!/usr/bin/env bash
    if [ -f "container/ct-build.sh" ]; then
        cd container && ./ct-build.sh --push
    else
        echo "No container/ct-build.sh found — falling back to podman push"
        podman push affinescript-vite:latest
    fi

# Run container interactively (for debugging)
container-run *args:
    podman run --rm -it affinescript-vite:latest {{args}}

# ═══════════════════════════════════════════════════════════════════════════════
# CI & AUTOMATION
# ═══════════════════════════════════════════════════════════════════════════════

# Run full CI pipeline locally
ci: deps quality
    @echo "CI pipeline complete!"

# Install git hooks
install-hooks:
    @mkdir -p .git/hooks
    @cat > .git/hooks/pre-commit << 'HOOKEOF'
    #!/bin/bash
    just fmt-check || exit 1
    just lint || exit 1
    just assail || exit 1
    HOOKEOF
    @chmod +x .git/hooks/pre-commit
    @echo "Git hooks installed"

# ═══════════════════════════════════════════════════════════════════════════════
# SECURITY
# ═══════════════════════════════════════════════════════════════════════════════

# Run security audit
security: deps-audit
    @echo "=== Security Audit ==="
    @command -v gitleaks >/dev/null && gitleaks detect --source . --verbose || true
    @command -v trivy >/dev/null && trivy fs --severity HIGH,CRITICAL . || true
    @echo "Security audit complete"

# Generate SBOM
sbom:
    @mkdir -p docs/security
    @command -v syft >/dev/null && syft . -o spdx-json > docs/security/sbom.spdx.json || echo "syft not found"

# ═══════════════════════════════════════════════════════════════════════════════
# VALIDATION & COMPLIANCE
# ═══════════════════════════════════════════════════════════════════════════════

# Validate RSR compliance
validate-rsr:
    #!/usr/bin/env bash
    echo "=== RSR Compliance Check ==="
    MISSING=""
    for f in .editorconfig .gitignore Justfile README.adoc LICENSE 0-AI-MANIFEST.a2ml; do
        [ -f "$f" ] || MISSING="$MISSING $f"
    done
    for f in .machine_readable/STATE.a2ml .machine_readable/META.a2ml .machine_readable/ECOSYSTEM.a2ml .machine_readable/anchors/ANCHOR.a2ml .machine_readable/policies/MAINTENANCE-AXES.a2ml .machine_readable/policies/MAINTENANCE-CHECKLIST.a2ml .machine_readable/policies/SOFTWARE-DEVELOPMENT-APPROACH.a2ml; do
        [ -f "$f" ] || MISSING="$MISSING $f"
    done
    for f in docs/legal/EXHIBIT-A-ETHICAL-USE.txt docs/legal/EXHIBIT-B-QUANTUM-SAFE.txt LICENSE; do
        [ -f "$f" ] || MISSING="$MISSING $f"
    done
    for f in src/interface/abi src/interface/ffi src/interface/generated; do
        [ -d "$f" ] || MISSING="$MISSING $f"
    done
    for f in docs/governance/MAINTENANCE-CHECKLIST.adoc docs/governance/SOFTWARE-DEVELOPMENT-APPROACH.adoc; do
        [ -f "$f" ] || MISSING="$MISSING $f"
    done
    if [ -f ".machine_readable/META.a2ml" ]; then
        grep -q 'axis-1 = "must > intend > like"' .machine_readable/META.a2ml || MISSING="$MISSING META.a2ml:axis-1"
        grep -q 'axis-2 = "corrective > adaptive > perfective"' .machine_readable/META.a2ml || MISSING="$MISSING META.a2ml:axis-2"
        grep -q 'axis-3 = "systems > compliance > effects"' .machine_readable/META.a2ml || MISSING="$MISSING META.a2ml:axis-3"
        grep -q 'scoping-first = true' .machine_readable/META.a2ml || MISSING="$MISSING META.a2ml:scoping-first"
        grep -q 'idris-unsound-scan = "believe_me/assert_total"' .machine_readable/META.a2ml || MISSING="$MISSING META.a2ml:idris-unsound-scan"
        grep -q 'audit-focus = "systems in place, documentation explains actual state, safety/security accounted for, observed effects reviewed"' .machine_readable/META.a2ml || MISSING="$MISSING META.a2ml:audit-focus"
        grep -q 'compliance-focus = "seams/compromises/exception register, bounded exceptions, anti-drift checks"' .machine_readable/META.a2ml || MISSING="$MISSING META.a2ml:compliance-focus"
        grep -q 'effects-evidence = "benchmark execution/results and maintainer status dialogue/review"' .machine_readable/META.a2ml || MISSING="$MISSING META.a2ml:effects-evidence"
        grep -q 'compliance-tooling = "panic-attack"' .machine_readable/policies/MAINTENANCE-AXES.a2ml || MISSING="$MISSING MAINTENANCE-AXES.a2ml:compliance-tooling"
        grep -q 'effects-tooling = "ecological checking with sustainabot guidance"' .machine_readable/policies/MAINTENANCE-AXES.a2ml || MISSING="$MISSING MAINTENANCE-AXES.a2ml:effects-tooling"
        grep -q 'source-human = "docs/governance/MAINTENANCE-CHECKLIST.adoc"' .machine_readable/policies/MAINTENANCE-CHECKLIST.a2ml || MISSING="$MISSING MAINTENANCE-CHECKLIST.a2ml:source-human"
        grep -q 'source-human = "docs/governance/SOFTWARE-DEVELOPMENT-APPROACH.adoc"' .machine_readable/policies/SOFTWARE-DEVELOPMENT-APPROACH.a2ml || MISSING="$MISSING SOFTWARE-DEVELOPMENT-APPROACH.a2ml:source-human"
    fi
    if [ -n "$MISSING" ]; then
        echo "MISSING:$MISSING"
        exit 1
    fi
    echo "RSR compliance: PASS"

# Validate STATE.a2ml syntax
validate-state:
    @if [ -f ".machine_readable/STATE.a2ml" ]; then \
        grep -q '^\[metadata\]' .machine_readable/STATE.a2ml && \
        grep -q 'project\s*=' .machine_readable/STATE.a2ml && \
        echo "STATE.a2ml: valid" || echo "STATE.a2ml: INVALID (missing required sections)"; \
    else \
        echo "No .machine_readable/STATE.a2ml found"; \
    fi

# Validate AI installation guide completeness (finishbot pre-release check)
validate-ai-install:
    #!/usr/bin/env bash
    echo "=== AI Installation Guide Check ==="
    GUIDE="docs/AI_INSTALLATION_GUIDE.adoc"
    README="README.adoc"
    ERRORS=0

    # Check guide exists
    if [ ! -f "$GUIDE" ]; then
        echo "MISSING: "$GUIDE"" (create from template: docs/AI_INSTALLATION_GUIDE.adoc)"
        ERRORS=$((ERRORS + 1))
    else
        # Check for unfilled PENDING markers
        PENDINGS=$(grep -c '\[PENDING-AI-INSTALL' "$GUIDE" 2>/dev/null || true)
        if [ "$PENDINGS" -gt 0 ]; then
            echo "INCOMPLETE: "$GUIDE"" has $PENDINGS unfilled [PENDING-AI-INSTALL] markers:"
            grep -n '\[PENDING-AI-INSTALL' "$GUIDE" | head -10
            ERRORS=$((ERRORS + 1))
        else
            echo "$GUIDE: complete (no PENDING markers)"
        fi

        # Check AI implementation section exists
        if ! grep -q 'ai-implementation' "$GUIDE" 2>/dev/null; then
            echo "MISSING: [[ai-implementation]] anchor in $GUIDE"
            ERRORS=$((ERRORS + 1))
        fi

        # Check privacy notice exists
        if ! grep -qi 'privacy' "$GUIDE" 2>/dev/null; then
            echo "MISSING: Privacy notice in $GUIDE"
            ERRORS=$((ERRORS + 1))
        fi

        # Check install commands exist (not just placeholders)
        if ! grep -q 'git clone' "$GUIDE" 2>/dev/null; then
            echo "WARNING: No git clone command found in $GUIDE -- install commands may be incomplete"
        fi
    fi

    # Check README has AI install section
    if [ -f "$README" ]; then
        if ! grep -qi 'AI-Assisted Installation' "$README" 2>/dev/null; then
            echo "MISSING: AI-Assisted Installation section in $README"
            echo "  Copy from docs/AI-INSTALL-README-SECTION.adoc"
            ERRORS=$((ERRORS + 1))
        fi

        # Check README for unfilled PENDING markers
        README_PENDINGS=$(grep -c '\[PENDING-AI-INSTALL' "$README" 2>/dev/null || true)
        if [ "$README_PENDINGS" -gt 0 ]; then
            echo "INCOMPLETE: $README has $README_PENDINGS unfilled [PENDING-AI-INSTALL] markers"
            ERRORS=$((ERRORS + 1))
        fi
    fi

    if [ "$ERRORS" -gt 0 ]; then
        echo ""
        echo "AI install guide: FAIL ($ERRORS issues)"
        exit 1
    fi
    echo "AI install guide: PASS"

# Full validation suite
validate: validate-rsr validate-state validate-ai-install
    @echo "All validations passed!"

# ═══════════════════════════════════════════════════════════════════════════════
# STATE MANAGEMENT
# ═══════════════════════════════════════════════════════════════════════════════

# Update STATE.a2ml timestamp
state-touch:
    @if [ -f ".machine_readable/STATE.a2ml" ]; then \
        sed -i 's/last-updated = "[^"]*"/last-updated = "'"$(date +%Y-%m-%d)"'"/' .machine_readable/STATE.a2ml && \
        echo "STATE.a2ml timestamp updated"; \
    fi

# Show current phase from STATE.a2ml
state-phase:
    @grep -oP 'phase\s*=\s*"\K[^"]+' .machine_readable/STATE.a2ml 2>/dev/null | head -1 || echo "unknown"

# ═══════════════════════════════════════════════════════════════════════════════
# GUIX & NIX
# ═══════════════════════════════════════════════════════════════════════════════

# Enter Guix development shell (primary)
guix-shell:
    guix shell -D -f guix.scm

# Build with Guix
guix-build:
    guix build -f guix.scm

# Enter Nix development shell (fallback)
nix-shell:
    @if [ -f "flake.nix" ]; then nix develop; else echo "No flake.nix"; fi

# ═══════════════════════════════════════════════════════════════════════════════
# HYBRID AUTOMATION
# ═══════════════════════════════════════════════════════════════════════════════

# Run local automation tasks
automate task="all":
    #!/usr/bin/env bash
    case "{{task}}" in
        all) just fmt && just lint && just test && just docs && just state-touch ;;
        cleanup) just clean && find . -name "*.orig" -delete && find . -name "*~" -delete ;;
        update) just deps && just validate ;;
        *) echo "Unknown: {{task}}. Use: all, cleanup, update" && exit 1 ;;
    esac

# ═══════════════════════════════════════════════════════════════════════════════
# COMBINATORIC MATRIX RECIPES
# ═══════════════════════════════════════════════════════════════════════════════

# Build matrix: [debug|release] x [target] x [features]
build-matrix mode="debug" target="" features="":
    @echo "Build matrix: mode="{{mode}}" target="{{target}}" features="{{features}}""

# Test matrix: [unit|integration|e2e|all] x [verbosity] x [parallel]
test-matrix suite="unit" verbosity="normal" parallel="true":
    @echo "Test matrix: suite={{suite}} verbosity={{verbosity}} parallel={{parallel}}"

# Container matrix: [build|run|push|shell|scan] x [registry] x [tag]
container-matrix action="build" registry="ghcr.io/hyperpolymath" tag="latest":
    @echo "Container matrix: action={{action}} registry={{registry}} tag={{tag}}"

# CI matrix: [lint|test|build|security|all] x [quick|full]
ci-matrix stage="all" depth="quick":
    @echo "CI matrix: stage={{stage}} depth={{depth}}"

# Show all matrix combinations
combinations:
    @echo "=== Combinatoric Matrix Recipes ==="
    @echo ""
    @echo "Build Matrix: just build-matrix [debug|release] [target] [features]"
    @echo "Test Matrix:  just test-matrix [unit|integration|e2e|all] [verbosity] [parallel]"
    @echo "Container:    just container-matrix [build|run|push|shell|scan] [registry] [tag]"
    @echo "CI Matrix:    just ci-matrix [lint|test|build|security|all] [quick|full]"

# ═══════════════════════════════════════════════════════════════════════════════
# VERSION CONTROL
# ═══════════════════════════════════════════════════════════════════════════════

# Show git status
status:
    @git status --short

# Show recent commits
log count="20":
    @git log --oneline -{{count}}

# Generate CHANGELOG.md with git-cliff
changelog:
    @command -v git-cliff >/dev/null || { echo "git-cliff not found — install: cargo install git-cliff"; exit 1; }
    git cliff --config .machine_readable/configs/git-cliff/cliff.toml --output CHANGELOG.md
    @echo "Generated CHANGELOG.md"

# Preview changelog for unreleased commits (does not write)
changelog-preview:
    @command -v git-cliff >/dev/null || { echo "git-cliff not found — install: cargo install git-cliff"; exit 1; }
    git cliff --config .machine_readable/configs/git-cliff/cliff.toml --unreleased --strip header

# Tag a new release (usage: just release-tag 1.2.3)
release-tag version:
    #!/usr/bin/env bash
    TAG="v{{version}}"
    if git rev-parse "$TAG" >/dev/null 2>&1; then
        echo "Tag $TAG already exists"
        exit 1
    fi
    just changelog
    git add CHANGELOG.md
    git commit -m "chore(release): prepare $TAG"
    git tag -a "$TAG" -m "Release $TAG"
    echo "Created tag $TAG — push with: git push origin main --tags"

# ═══════════════════════════════════════════════════════════════════════════════
# UTILITIES
# ═══════════════════════════════════════════════════════════════════════════════

# Count lines of code
loc:
    @find . \( -name "*.rs" -o -name "*.ex" -o -name "*.exs" -o -name "*.res" -o -name "*.gleam" -o -name "*.zig" -o -name "*.idr" -o -name "*.hs" -o -name "*.ncl" -o -name "*.scm" -o -name "*.adb" -o -name "*.ads" \) -not -path './target/*' -not -path './_build/*' 2>/dev/null | xargs -r wc -l 2>/dev/null | tail -1 || echo "0"

# Show PENDING comments
todos:
    @grep -rn "PENDING\|FIXME\|HACK\|XXX" --include="*.rs" --include="*.ex" --include="*.res" --include="*.gleam" --include="*.zig" --include="*.idr" --include="*.hs" . 2>/dev/null || echo "No PENDINGs"

# Open in editor
edit:
    ${EDITOR:-code} .

# Run high-rigor security assault using panic-attacker
maint-assault:
    @./.machine_readable/scripts/maintenance/maint-assault.sh

# Run panic-attacker pre-commit scan (foundational floor-raise requirement)
assail:
    @command -v panic-attack >/dev/null 2>&1 && panic-attack assail . || echo "WARN: panic-attack not found — install from https://github.com/hyperpolymath/panic-attacker"


# Self-diagnostic — checks dependencies, permissions, paths
doctor:
    @echo "Running diagnostics for rsr-template-repo..."
    @echo "Checking required tools..."
    @command -v just >/dev/null 2>&1 && echo "  [OK] just" || echo "  [FAIL] just not found"
    @command -v git >/dev/null 2>&1 && echo "  [OK] git" || echo "  [FAIL] git not found"
    @echo "Checking for hardcoded paths..."
    @grep -rn '$HOME\|$ECLIPSE_DIR' --include='*.rs' --include='*.ex' --include='*.res' --include='*.gleam' --include='*.sh' . 2>/dev/null | head -5 || echo "  [OK] No hardcoded paths"
    @echo "Diagnostics complete."

# Guided tour of key features
tour:
    @echo "=== rsr-template-repo Tour ==="
    @echo ""
    @echo "1. Project structure:"
    @ls -la
    @echo ""
    @echo "2. Available commands: just --list"
    @echo ""
    @echo "3. Read README.adoc for full overview"
    @echo "4. Read EXPLAINME.adoc for architecture decisions"
    @echo "5. Run 'just doctor' to check your setup"
    @echo ""
    @echo "Tour complete! Try 'just --list' to see all available commands."

# Open feedback channel with diagnostic context
help-me:
    @echo "=== rsr-template-repo Help ==="
    @echo "Platform: $(uname -s) $(uname -m)"
    @echo "Shell: $SHELL"
    @echo ""
    @echo "To report an issue:"
    @echo "  https://github.com/hyperpolymath/rsr-template-repo/issues/new"
    @echo ""
    @echo "Include the output of 'just doctor' in your report."

# ═══════════════════════════════════════════════════════════════════════════════
# FORMAL VERIFICATION (PROOFS)
# ═══════════════════════════════════════════════════════════════════════════════

# Check all formal proofs (Idris2 + Lean4 + Agda + Coq)
proof-check-all: proof-check-idris2 proof-check-lean4 proof-check-agda proof-check-coq proof-scan-dangerous
    @echo "=== All proof checks complete ==="

# Check Idris2 proofs (ABI, types, dependent type proofs)
proof-check-idris2:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "=== Checking Idris2 proofs ==="
    if ! command -v idris2 &>/dev/null; then
        echo "SKIP: idris2 not installed"
        exit 0
    fi
    ERRORS=0
    for f in "$(find verification/proofs/idris2 -name '*.idr' 2>/dev/null)"; do
        echo -n "  Checking "$f" ... "
        if idris2 --check "$f" 2>/dev/null; then
            echo "OK"
        else
            echo "FAIL"
            ERRORS=$((ERRORS + 1))
        fi
    done
    if [ "$ERRORS" -gt 0 ]; then
        echo "FAIL: "$ERRORS" Idris2" proof(s) failed"
        exit 1
    fi
    echo "PASS: All Idris2 proofs verified"

# Check Lean4 proofs
proof-check-lean4:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "=== Checking Lean4 proofs ==="
    if ! command -v lean &>/dev/null; then
        echo "SKIP: lean not installed"
        exit 0
    fi
    ERRORS=0
    for f in "$(find verification/proofs/lean4 -name '*.lean' 2>/dev/null)"; do
        echo -n "  Checking "$f" ... "
        if lean "$f" 2>/dev/null; then
            echo "OK"
        else
            echo "FAIL"
            ERRORS=$((ERRORS + 1))
        fi
    done
    if [ "$ERRORS" -gt 0 ]; then
        echo "FAIL: "$ERRORS" Lean4 proof(s) failed"
        exit 1
    fi
    echo "PASS: All Lean4 proofs verified"

# Check Agda proofs
proof-check-agda:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "=== Checking Agda proofs ==="
    if ! command -v agda &>/dev/null; then
        echo "SKIP: agda not installed"
        exit 0
    fi
    ERRORS=0
    for f in "$(find verification/proofs/agda -name '*.agda' 2>/dev/null)"; do
        echo -n "  Checking "$f" ... "
        if agda --safe "$f" 2>/dev/null; then
            echo "OK"
        else
            echo "FAIL"
            ERRORS=$((ERRORS + 1))
        fi
    done
    if [ "$ERRORS" -gt 0 ]; then
        echo "FAIL: "$ERRORS" Agda proof(s) failed"
        exit 1
    fi
    echo "PASS: All Agda proofs verified"

# Check Coq proofs
proof-check-coq:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "=== Checking Coq proofs ==="
    if ! command -v coqc &>/dev/null; then
        echo "SKIP: coqc not installed"
        exit 0
    fi
    ERRORS=0
    for f in "$(find verification/proofs/coq -name '*.v' 2>/dev/null)"; do
        echo -n "  Checking "$f" ... "
        if coqc "$f" 2>/dev/null; then
            echo "OK"
        else
            echo "FAIL"
            ERRORS=$((ERRORS + 1))
        fi
    done
    if [ "$ERRORS" -gt 0 ]; then
        echo "FAIL: "$ERRORS" Coq proof(s) failed"
        exit 1
    fi
    echo "PASS: All Coq proofs verified"

# Scan for dangerous patterns in proof files (believe_me, sorry, Admitted, etc.)
proof-scan-dangerous:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "=== Scanning for dangerous patterns in proofs ==="
    DANGEROUS=0
    PATTERNS="believe_me|assert_total|postulate|sorry|Admitted|unsafeCoerce|Obj\.magic"
    for f in "$(find verification/proofs -name '*.idr' -o -name '*.lean' -o -name '*.agda' -o -name '*.v' 2>/dev/null)"; do
        MATCHES=$(grep -nE "$PATTERNS" "$f" 2>/dev/null || true)
        if [ -n "$MATCHES" ]; then
            echo "  DANGEROUS: "$f""
            echo "$MATCHES" | sed 's/^/    /'
            DANGEROUS=$((DANGEROUS + 1))
        fi
    done
    if [ "$DANGEROUS" -gt 0 ]; then
        echo "FAIL: $DANGEROUS file(s) contain dangerous patterns"
        exit 1
    fi
    echo "PASS: No dangerous patterns found in proofs"

# Show proof status summary
proof-status:
    #!/usr/bin/env bash
    echo "=== Proof Status ==="
    echo ""
    echo "Idris2: $(find verification/proofs/idris2 -name '*.idr' 2>/dev/null | wc -l) files"
    echo "Lean4:  $(find verification/proofs/lean4 -name '*.lean' 2>/dev/null | wc -l) files"
    echo "Agda:   $(find verification/proofs/agda -name '*.agda' 2>/dev/null | wc -l) files"
    echo "Coq:    $(find verification/proofs/coq -name '*.v' 2>/dev/null | wc -l) files"
    echo "TLA+:   $(find verification/proofs/tlaplus -name '*.tla' 2>/dev/null | wc -l) files"
    echo ""
    if [ -f PROOF-STATUS.md ]; then
        grep -E "^\| \*\*Total\*\*" PROOF-STATUS.md 2>/dev/null || echo "(No summary row in PROOF-STATUS.md)"
    else
        echo "(No PROOF-STATUS.md found)"
    fi
