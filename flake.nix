{
  description = "kvmSwitch (libgpiod v1)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    edo.url = "github:cookEbox/edo";
  };

  outputs = { self, nixpkgs, flake-utils, edo }:
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

        withEdo = pkgsU:
          pkgsU.haskellPackages.override {
            overrides = self': super: {
              edo = self'.callCabal2nix "edo" edo { };
            };
          }; 

        mkKvmWith = pkgsU:
          let
            hp0  = withEdo pkgsU;
            gpiodV1 = (pkgsU.libgpiod1 or pkgsU.libgpiod_1);
            drv0 = hp0.callCabal2nix packageName self {
              libgpiod = gpiodV1;
              gpiod    = gpiodV1;
            };
            drv1 = pkgsU.haskell.lib.markUnbroken (pkgsU.haskell.lib.doJailbreak drv0);
            pcPath = pkgs.lib.makeSearchPath "lib/pkgconfig" [ gpiodV1 ];
          in
          drv1.overrideAttrs (old: {
            nativeBuildInputs = (old.nativeBuildInputs or []) ++ [ pkgs.pkg-config ];
            buildInputs       = (old.buildInputs       or []) ++ [ gpiodV1 ];
            PKG_CONFIG_PATH   = pcPath;
            PKG_CONFIG_LIBDIR = pcPath;
            strictDeps = true;
            passthru.forceRebuild = builtins.currentTime;
            doCheckReproducibility = false;
          });


        kvmNative  = mkKvmWith pkgs;
        kvmAarch64 = mkKvmWith pkgsAarch64;
        kvmArmv7l  = mkKvmWith pkgsArmv7l;

        gpiodV1 = pkgs.libgpiod1 or pkgs.libgpiod_1;

        copyToPi = import ./nix/copyToPi.nix {inherit pkgs; };

      in {
        packages.${packageName}           = kvmNative;
        packages.default                  = kvmNative;
        packages."${packageName}-aarch64" = kvmAarch64;
        packages."${packageName}-armv7l"  = kvmArmv7l;

        apps =
          let app = flake-utils.lib.mkApp { drv = kvmNative; };
          in {
            default   = app;
            kvmSwitch = app;
            copy-to-pi-aarch64 = {
              type = "app";
              program = "${copyToPi}/bin/copy-to-pi";
            };
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

