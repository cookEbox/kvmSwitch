#!/usr/bin/env bash
set -euo pipefail

NEW=$(sed -nE 's#^(/nix/store/\S+)#\1#p' /tmp/kvm.path | head -n1)
if [[ -z "${NEW:-}" ]]; then
  echo "No valid store path in /tmp/kvm.path" >&2
  exit 1
fi

# Try to remove existing kvmSwitch from the profile
OLD=$(nix profile list \
  | grep -F kvmSwitch \
  | sed -nE 's#.*(/nix/store/\S*)#\1#p' \
  | head -n1 || true)

if [[ -n "${OLD:-}" ]]; then
  nix profile remove "$OLD" || true
fi

nix profile add "$NEW"
echo "Installed: $NEW"

