{
  description = "kvmSwitch (libgpiod v1)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    # Include both native dev and the Pi targets
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" "armv7l-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        lib  = pkgs.lib;

        # Cross sets for when host != target
        pkgsAarch64 = pkgs.pkgsCross.aarch64-multiplatform;
        pkgsArmv7l  = pkgs.pkgsCross.armv7l-hf-multiplatform;

        packageName = "kvmSwitch";

        # Prefer libgpiod v1. Some nixpkgs have libgpiod1, some libgpiod_1.
        gpiodV1 = (pkgs.libgpiod1 or pkgs.libgpiod_1);

        # Helper to instantiate from the generated nix file
        mkFromGenerated = pkgsU:
          let
            # the generated file from `cabal2nix . > nix/kvmSwitch.nix`
            drv0 = pkgsU.callPackage ./nix/kvmSwitch.nix {
              # force pkg-config to pick v1 of libgpiod
              libgpiod = (pkgsU.libgpiod1 or pkgsU.libgpiod_1);
            };

            # nudge Haskell lib if needed
            drv1 = pkgsU.haskell.lib.markUnbroken
                     (pkgsU.haskell.lib.doJailbreak drv0);

            pcPath = pkgs.lib.makeSearchPath "lib/pkgconfig" [
              (pkgsU.libgpiod1 or pkgsU.libgpiod_1)
            ];
          in
          drv1.overrideAttrs (old: {
            nativeBuildInputs = (old.nativeBuildInputs or []) ++ [ pkgs.pkg-config ];
            buildInputs       = (old.buildInputs       or []) ++ [ (pkgsU.libgpiod1 or pkgsU.libgpiod_1) ];
            # Ensure *only* v1 is seen by pkg-config
            PKG_CONFIG_PATH   = pcPath;
            PKG_CONFIG_LIBDIR = pcPath;
          });

        # Per-target derivations
        kvmNative =
          mkFromGenerated pkgs;

        kvmAarch64 =
          if pkgs.stdenv.hostPlatform.system == "aarch64-linux"
          then mkFromGenerated pkgs
          else mkFromGenerated pkgsAarch64;

        kvmArmv7l =
          if pkgs.stdenv.hostPlatform.system == "armv7l-linux"
          then mkFromGenerated pkgs
          else mkFromGenerated pkgsArmv7l;
      in
      {
        packages.${packageName}           = kvmNative;
        packages.default                  = kvmNative;
        packages."${packageName}-aarch64" = kvmAarch64;
        packages."${packageName}-armv7l"  = kvmArmv7l;

        devShells.default = pkgs.mkShell {
          inputsFrom = lib.optional (kvmNative ? env) kvmNative.env;
          buildInputs = [
            pkgs.haskellPackages.haskell-language-server
            pkgs.haskellPackages.hoogle
            pkgs.cabal-install
            pkgs.ghcid
            pkgs.lsof
            pkgs.xdotool
            pkgs.pkg-config
            pkgs.cachix
          ] ++ lib.optionals pkgs.stdenv.isLinux [
            (pkgs.libgpiod1 or pkgs.libgpiod_1)
          ];

          shellHook = lib.optionalString pkgs.stdenv.isLinux ''
            export LD_LIBRARY_PATH=${lib.makeLibraryPath [ (pkgs.libgpiod1 or pkgs.libgpiod_1) ]}:$LD_LIBRARY_PATH
          '';
        };

        # For older tools that expect devShell
        devShell = self.devShells.${system}.default;
      });
}

