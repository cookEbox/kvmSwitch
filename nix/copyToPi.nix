{ pkgs
, defaultCache ? "nixcooke"
, defaultHost  ? "nick@192.168.1.116"
, defaultAttr  ? "packages.x86_64-linux.kvmSwitch-aarch64"
, defaultOut   ? "/tmp/kvm.path"
}:
let
  installSrc = ./install.sh;
  app = pkgs.writeShellApplication {
    name = "copy-to-pi";
    runtimeInputs = [
      pkgs.nix         # nix build
      pkgs.cachix      # cachix push
      pkgs.openssh     # scp
      pkgs.coreutils   # readlink
    ];
    text = ''
      set -euo pipefail

      CACHE="''${CACHE:-${defaultCache}}"
      HOST="''${HOST:-${defaultHost}}"
      ATTR="''${ATTR:-${defaultAttr}}"
      PATH_FILE="''${PATH_FILE:-${defaultOut}}"

      echo "Building ''${ATTR} ..."
      nix build ".#''${ATTR}"
      OUT="$(readlink -f result)"
      echo "Output path: ''${OUT}"

      echo "Pushing to Cachix (''${CACHE}) ..."
      cachix push "''${CACHE}" "''${OUT}"

      echo "Writing local path file: ''${PATH_FILE}"
      printf '%s\n' "''${OUT}" > "''${PATH_FILE}"

      echo "Copying path file to Pi: ''${HOST}:''${PATH_FILE}"
      scp "''${PATH_FILE}" "''${HOST}:''${PATH_FILE}"

      echo "Removing existing installer on Pi (if present)..."
      ssh "''${HOST}" "rm -f /tmp/kvm-install.sh"

      echo "Copying installer to Pi and running it..."
      scp ${installSrc} "''${HOST}:/tmp/kvm-install.sh"
      ssh "''${HOST}" "chmod +x /tmp/kvm-install.sh && /tmp/kvm-install.sh"

      echo "Done."
    '';
  };
in
  app

