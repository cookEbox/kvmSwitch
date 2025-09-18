{ pkgs
, defaultCache     ? "nixcooke"
, defaultHost      ? "nick@192.168.1.116"
, defaultAttr      ? "packages.x86_64-linux.kvmSwitch-aarch64"
, defaultOut       ? "/tmp/kvm.path"
, defaultInstaller ? "/usr/local/bin/kvm-install.sh"
, defaultSSHOpts   ? "-o StrictHostKeyChecking=accept-new"
}:
let
  # keep your installer in repo at nix/install.sh
  installSrc = ./install.sh;

  app = pkgs.writeShellApplication {
    name = "copy-to-pi";
    runtimeInputs = [
      pkgs.nix
      pkgs.cachix
      pkgs.openssh
      pkgs.coreutils
    ];
    text = ''
      set -euo pipefail

      CACHE="''${CACHE:-${defaultCache}}"
      HOST="''${HOST:-${defaultHost}}"
      ATTR="''${ATTR:-${defaultAttr}}"
      PATH_FILE="''${PATH_FILE:-${defaultOut}}"
      INSTALLER="''${INSTALLER:-${defaultInstaller}}"
      SSH_OPTS="''${SSH_OPTS:-${defaultSSHOpts}}"

      echo "Building .#''${ATTR} ..."
      OUT="$(nix build ".#''${ATTR}" --print-out-paths)"
      echo "Output path: ''${OUT}"

      echo "Pushing to Cachix (''${CACHE}) ..."
      cachix push "''${CACHE}" "''${OUT}"

      echo "Copying store path to Pi via nix copy ..."
      nix copy --to "ssh://''${HOST}" "''${OUT}"

      echo "Writing path on Pi: ''${PATH_FILE}"
      ssh $SSH_OPTS "''${HOST}" "printf '%s\n' \"''${OUT}\" | sudo tee \"''${PATH_FILE}\" >/dev/null"

      echo "Ensuring installer on Pi at ''${INSTALLER} ..."
      if ! ssh $SSH_OPTS "''${HOST}" "test -x \"''${INSTALLER}\""; then
        echo "Installer not found; uploading…"
        scp $SSH_OPTS ${installSrc} "''${HOST}:/tmp/kvm-install.sh"
        ssh $SSH_OPTS "''${HOST}" "sudo mv /tmp/kvm-install.sh \"''${INSTALLER}\" && sudo chmod +x \"''${INSTALLER}\""
      fi

      echo "Running installer on Pi …"
      ssh $SSH_OPTS "''${HOST}" "sudo \"''${INSTALLER}\""

      echo "Done. Installed: ''${OUT}"
    '';
  };
in
  app

