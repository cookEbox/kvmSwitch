{ pkgs }:
let
  app = pkgs.writeShellApplication {
    name = "copy-to-pi";
    runtimeInputs = [
      pkgs.cachix
      pkgs.openssh
      pkgs.coreutils
      pkgs.gawk
    ];
    text = ''
      set -euo pipefail
      CACHE="''${CACHE:-nixcooke}"
      HOST="''${HOST:-nick@192.168.1.116}"
      ATTR="''${ATTR:-packages.x86_64-linux.kvmSwitch-aarch64}"  # cross-built aarch64 output
      BIN_NAME="''${BIN_NAME:-kvmSwitch}"
      RESTART_CMD="''${RESTART_CMD:-}"  # e.g. "systemctl --user restart kvmSwitch.service"

      echo "▶︎ Building $ATTR …"
      nix build ".#''${ATTR}"
      OUT="$(readlink -f result)"
      echo "• store path: $OUT"

      echo "▶︎ Pushing to Cachix ($CACHE) …"
      cachix push "$CACHE" "$OUT"

      echo "▶︎ Removing old $BIN_NAME from $HOST profile (if any) …"
      ssh "$HOST" '
        set -e
        idxs=$(nix profile list | awk "/'"$BIN_NAME"'/ {print \$1}")
        if [ -n "$idxs" ]; then
          nix profile remove $idxs
          echo "• removed indices: $idxs"
        else
          echo "• none to remove"
        fi
      '

      echo "▶︎ Installing new $BIN_NAME on $HOST …"
      ssh "$HOST" "nix profile add '$OUT' && echo '✓ installed'; command -v '$BIN_NAME' >/dev/null && $BIN_NAME --version || true"

      if [ -n "$RESTART_CMD" ]; then
        echo "▶︎ Restarting service on $HOST: $RESTART_CMD"
        ssh "$HOST" "$RESTART_CMD" || echo "• restart command failed (non-fatal)"
      fi

      echo "✅ Done."
    '';
  };
in
  app

