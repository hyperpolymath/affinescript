#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -lt 3 ]; then
  cat >&2 <<'USAGE'
Usage: ./session/dispatch.sh <verb> <object> <repo-path>

Canonical commands:
  intake repo <path>
  checkpoint change <path>
  verify maintenance <path>
  verify substantial <path>
  verify release <path>
  close planned <path>
  close urgent <path>
  recover repo <path>
  handover full <path>
  handover split <path>
  handover model <path>
  handover human <path>
USAGE
  exit 2
fi

verb="$1"
object="$2"
repo_path="$3"
cmd_pair="$verb $object"

if [ ! -d "$repo_path" ]; then
  echo "error: repository path '$repo_path' does not exist" >&2
  exit 2
fi

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/.." && pwd)"

standards_dir="${SESSION_STANDARDS_DIR:-}"
if [ -z "$standards_dir" ]; then
  if [ -d "$repo_root/../standards/session-management-standards" ]; then
    standards_dir="$repo_root/../standards/session-management-standards"
  elif [ -d "$repo_root/standards/session-management-standards" ]; then
    standards_dir="$repo_root/standards/session-management-standards"
  fi
fi

case "$cmd_pair" in
  "intake repo")
    protocol_rel="continuity/repo-intake"
    ;;
  "checkpoint change")
    protocol_rel="continuity/checkpoint-before-major-change"
    ;;
  "verify maintenance")
    protocol_rel="verify/maintenance-sweep"
    ;;
  "verify substantial")
    protocol_rel="verify/substantial-completion"
    ;;
  "verify release")
    protocol_rel="verify/release-audit"
    ;;
  "close planned")
    protocol_rel="continuity/planned-session-close"
    ;;
  "close urgent")
    protocol_rel="continuity/emergency-termination"
    ;;
  "recover repo")
    protocol_rel="continuity/recovery-operation"
    ;;
  "handover full")
    protocol_rel="handover/full-transfer"
    ;;
  "handover split")
    protocol_rel="handover/collaborative-transfer"
    ;;
  "handover model")
    protocol_rel="handover/model-transfer"
    ;;
  "handover human")
    protocol_rel="handover/human-transfer"
    ;;
  *)
    echo "error: unsupported canonical command '$cmd_pair'" >&2
    exit 2
    ;;
esac

session_dir="$repo_path/.session"
mkdir -p "$session_dir"

command_record="$session_dir/LAST-CANONICAL-COMMAND.md"

cat > "$command_record" <<RECORD
# Last Canonical Session Command

- Command: $cmd_pair $repo_path
- Timestamp (UTC): $(date -u +%Y-%m-%dT%H:%M:%SZ)
- Repo path: $repo_path
- Protocol path: $protocol_rel
- Standards dir: ${standards_dir:-UNRESOLVED}

## Continuity Core (update while executing)

- Goal:
- Current task:
- Last completed action:
- Next intended action:
- Repository:
- Branch:
- HEAD commit:
- Files of interest:
- Known blockers:
- Residual risks:
- Recommended next protocol:
RECORD

if [ -n "$standards_dir" ] && [ -d "$standards_dir/$protocol_rel" ]; then
  echo "canonical: $cmd_pair $repo_path"
  echo "standard: $standards_dir/$protocol_rel"
  echo "checklist: $standards_dir/$protocol_rel/CHECKLIST.adoc"
  echo "protocol: $standards_dir/$protocol_rel/PROTOCOL.k9"
  echo "state template: $standards_dir/$protocol_rel/STATE-template.a2ml"
else
  echo "warning: could not resolve central standards directory."
  echo "Set SESSION_STANDARDS_DIR to standards/session-management-standards." >&2
  echo "canonical: $cmd_pair $repo_path"
  echo "mapped protocol: $protocol_rel"
fi

hooks="$script_dir/local-hooks.sh"
if [ -x "$hooks" ]; then
  "$hooks" "$verb" "$object" "$repo_path"
fi

echo "recorded: $command_record"
