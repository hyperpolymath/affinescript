#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# ssg-bootstrap.sh — Universal SSG Initialisation Helper
#
# Provides a starting hand for creating a documentation site or blog.
# Options 1-2 are hyperpolymath-maintained SSGs; options 3-5 are popular
# third-party choices. Use whichever fits your project.

set -euo pipefail

DEST="${1:-docs/site}"

echo "═══════════════════════════════════════════════════"
echo "  SSG BOOTSTRAP HELPER"
echo "  Target directory: $DEST"
echo "═══════════════════════════════════════════════════"
echo ""
echo "Select an SSG to initialize in this project:"
echo "  [1] Casket-SSG (Haskell) — hyperpolymath, pretty-formal"
echo "  [2] Ddraig-SSG (Idris2)  — hyperpolymath, dependent-type proofed"
echo "  [3] Serum (Elixir)       — BEAM-based, concurrent"
echo "  [4] Zola (Rust)          — Fast, standalone, standard"
echo "  [5] Custom Git URL       — Any SSG from a git repository"
echo ""

read -rp "Enter choice [1-5]: " choice

case "$choice" in
    1)
        echo "Selected: Casket-SSG"
        echo "Run: git clone https://github.com/hyperpolymath/casket-ssg $DEST"
        ;;
    2)
        echo "Selected: Ddraig-SSG"
        echo "Run: git clone https://github.com/hyperpolymath/ddraig-ssg $DEST"
        ;;
    3)
        echo "Selected: Serum"
        echo "Run: mix serum.new $DEST"
        ;;
    4)
        echo "Selected: Zola"
        echo "Run: zola init $DEST"
        ;;
    5)
        read -rp "Git URL: " custom_url
        echo "Run: git clone $custom_url $DEST"
        ;;
    *)
        echo "Invalid selection. Aborting."
        exit 1
        ;;
esac
