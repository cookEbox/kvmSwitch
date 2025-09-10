{
  description = "kvmSwitch (libgpiod v1)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    # Host(s) you actually develop on:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        lib  = pkgs.lib;

        # Cross targets (Pi)
        pkgsAarch64 = pkgs.pkgsCross.aarch64-multiplatform;
        pkgsArmv7l  = pkgs.pkgsCross.armv7l-hf-multiplatform;

        packageName = "kvmSwitch";

        jailbreakUnbreak = pkgsU: pkg:
          let hl = pkgsU.haskell.lib; in hl.markUnbroken (hl.doJailbreak pkg);

        withOverrides = pkgsU:
          pkgsU.haskellPackages.override {
            overrides = self': super: { };
          };

        
        mkKvmWith = pkgsU:
          let
            hp0  = pkgsU.haskellPackages;
            gpiodV1 = (pkgsU.libgpiod1 or pkgsU.libgpiod_1);
            drv0 = hp0.callCabal2nix packageName self {
              libgpiod = gpiodV1;  # for pkg-config-depends: libgpiod
              gpiod    = gpiodV1;  # for extra-libraries: gpiod
            };

            drv1 = pkgsU.haskell.lib.markUnbroken (pkgsU.haskell.lib.doJailbreak drv0);

            pcPath = pkgs.lib.makeSearchPath "lib/pkgconfig" [ gpiodV1 ];
          in
          drv1.overrideAttrs (old: {
            nativeBuildInputs = (old.nativeBuildInputs or []) ++ [ pkgs.pkg-config ];
            buildInputs       = (old.buildInputs       or []) ++ [ gpiodV1 ];

            # Force pkg-config to libgpiod v1 only (you already had this)
            PKG_CONFIG_PATH   = pcPath;
            PKG_CONFIG_LIBDIR = pcPath;

            strictDeps = true;

            # ⬇️ Skip the reproducibility double-build/compare
            passthru.forceRebuild = builtins.currentTime;
            doCheckReproducibility = false;
          });


        kvmNative  = mkKvmWith pkgs;
        kvmAarch64 = mkKvmWith pkgsAarch64; # Pi 64-bit OS
        kvmArmv7l  = mkKvmWith pkgsArmv7l;  # Pi 32-bit OS (common)

        gpiodV1 = pkgs.libgpiod1 or pkgs.libgpiod_1;
      in {
        packages.${packageName}           = kvmNative;
        packages.default                  = kvmNative;
        packages."${packageName}-aarch64" = kvmAarch64;
        packages."${packageName}-armv7l"  = kvmArmv7l;

        # nix run .#kvmSwitch
        apps =
          let app = flake-utils.lib.mkApp { drv = kvmNative; };
          in {
            default   = app;
            kvmSwitch = app;
          };

        devShells.default = pkgs.mkShell {
          inputsFrom = lib.optional (kvmNative ? env) kvmNative.env;
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server
            haskellPackages.hoogle
            cabal-install
            ghcid
            lsof
            xdotool
            pkg-config
            gpiodV1
            cachix
          ];
          shellHook = ''
            export LD_LIBRARY_PATH=${lib.makeLibraryPath [ gpiodV1 ]}:$LD_LIBRARY_PATH
          '';
        };

        devShell = self.devShells.${system}.default;
      });
}

