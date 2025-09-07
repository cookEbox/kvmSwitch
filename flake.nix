{
  description = "My haskell application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-darwin" "armv6l-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        lib  = pkgs.lib;

        # Cross targets
        pkgsAarch64 = import nixpkgs {
          localSystem = { inherit system; };
          crossSystem = nixpkgs.lib.systems.examples.aarch64-multiplatform;
        };
        pkgsArmv7l = import nixpkgs {
          localSystem = { inherit system; };
          crossSystem = nixpkgs.lib.systems.examples.armv7l-hf-multiplatform;
        };

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
            hp0  = withOverrides pkgsU;  # use overrides (or pkgsU.haskellPackages if you prefer)
            drv0 = hp0.callCabal2nix packageName self {
              # Your .cabal uses only pkg-config-depends: libgpiod
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

            PKG_CONFIG_PATH   = pcPath;
            PKG_CONFIG_LIBDIR = pcPath;
          });

        # Builds
        kvmNative  = mkKvmWith pkgs;
        kvmAarch64 = mkKvmWith pkgsAarch64;
        kvmArmv7l  = mkKvmWith pkgsArmv7l;

        libgpiodV1 = pkgs.libgpiod1 or pkgs.libgpiod_1;
      in {
        packages.${packageName} = kvmNative;
        packages.default = kvmNative;
        packages."${packageName}-aarch64" = kvmAarch64;
        packages."${packageName}-armv7l"  = kvmArmv7l;

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
