{
  description = "My haskell application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    # IMPORTANT: include aarch64-linux so the Pi sees its own outputs
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        lib  = pkgs.lib;

        # Cross targets (use pkgs.pkgsCross for the current host)
        pkgsAarch64 = pkgs.pkgsCross.aarch64-multiplatform;
        pkgsArmv7l  = pkgs.pkgsCross.armv7l-hf-multiplatform;

        packageName = "kvmSwitch";

        # Helper: jailbreak + unbreak
        jailbreakUnbreak = pkgsU: pkg:
          let hl = pkgsU.haskell.lib;
          in hl.markUnbroken (hl.doJailbreak pkg);

        # Optional: override specific Haskell deps here
        withOverrides = pkgsU:
          pkgsU.haskellPackages.override {
            overrides = self': super': {
              # example:
              # some-dep = jailbreakUnbreak pkgsU super'.some-dep;
            };
          };

        mkKvmWith = pkgsU:
          let
            hp0  = withOverrides pkgsU;  # or pkgsU.haskellPackages
            drv0 = hp0.callCabal2nix packageName self {
              # .cabal uses only: pkg-config-depends: libgpiod >= 1.6 && < 1.7
              libgpiod = (pkgsU.libgpiod1 or pkgsU.libgpiod_1);
            };

            drv1 = jailbreakUnbreak pkgsU drv0;

            # Restrict pkg-config to v1 only
            pcPath = pkgs.lib.makeSearchPath "lib/pkgconfig" [
              (pkgsU.libgpiod1 or pkgsU.libgpiod_1)
            ];
          in
          drv1.overrideAttrs (old: {
            nativeBuildInputs = (old.nativeBuildInputs or []) ++ [ pkgs.pkg-config ];
            buildInputs       = (old.buildInputs or [])       ++ [ (pkgsU.libgpiod1 or pkgsU.libgpiod_1) ];

            # ensure pkg-config resolves libgpiod v1 (not v2)
            PKG_CONFIG_PATH   = pcPath;
            PKG_CONFIG_LIBDIR = pcPath;
          });

        # Builds:
        # - kvmNative: host-native (handy for dev)
        # - kvmAarch64/Armv7l: native when host==target, otherwise cross via pkgsCross.*
        kvmNative  = mkKvmWith pkgs;

        kvmAarch64 =
          if pkgs.stdenv.hostPlatform.system == "aarch64-linux"
          then mkKvmWith pkgs
          else mkKvmWith pkgsAarch64;

        kvmArmv7l =
          if pkgs.stdenv.hostPlatform.system == "armv7l-linux"
          then mkKvmWith pkgs
          else mkKvmWith pkgsArmv7l;

        libgpiodV1 = pkgs.libgpiod1 or pkgs.libgpiod_1;
      in {
        packages.${packageName}            = kvmNative;
        packages.default                   = kvmNative;
        packages."${packageName}-aarch64"  = kvmAarch64;
        packages."${packageName}-armv7l"   = kvmArmv7l;

        devShells.default = pkgs.mkShell {
          inputsFrom = [ kvmNative.env ];
          buildInputs =
            [ pkgs.haskellPackages.haskell-language-server
              pkgs.haskellPackages.hoogle
              pkgs.cabal-install
              pkgs.ghcid
              pkgs.lsof
              pkgs.xdotool
              pkgs.pkg-config
              pkgs.cachix
            ]
            ++ lib.optionals pkgs.stdenv.isLinux [ libgpiodV1 ];
          shellHook = lib.optionalString pkgs.stdenv.isLinux ''
            export LD_LIBRARY_PATH=${lib.makeLibraryPath [ libgpiodV1 ]}:$LD_LIBRARY_PATH
          '';
        };

        devShell = self.devShells.${system}.default;
      });
}
