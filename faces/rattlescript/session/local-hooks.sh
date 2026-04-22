#!/usr/bin/env bash
set -euo pipefail

verb="${1:-}"
object="${2:-}"
repo_path="${3:-.}"

session_dir="$repo_path/.session"
mkdir -p "$session_dir"
log_file="$session_dir/local-hooks.log"

echo "$(date -u +%Y-%m-%dT%H:%M:%SZ) hook: $verb $object $repo_path" >> "$log_file"

case "$verb $object" in
  "verify release")
    echo "release hook: ensure AUDIT.adoc and session reports are reviewed" >> "$log_file"
    ;;
  "close urgent")
    echo "urgent hook: prioritize EMERGENCY-CHECKPOINT.md generation" >> "$log_file"
    ;;
esac
