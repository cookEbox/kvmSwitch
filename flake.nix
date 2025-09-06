{
  description = "My haskell application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-darwin" "armv6l-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        lib  = pkgs.lib;

        haskellPackages = pkgs.haskellPackages;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        libgpiodV1 = pkgs.libgpiod1 or pkgs.libgpiod_1; 

        packageName = "kvmSwitch";
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self rec {
            gpiod = libgpiodV1;
            # Dependency overrides go here
          };

        packages.default = self.packages.${system}.${packageName};
        defaultPackage = self.packages.${system}.default;

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server # you must build it with your ghc to work
            haskellPackages.hoogle
            xdotool
            lsof
            ghcid
            cabal-install
            libgpiodV1
          ];
          shellHook = ''
          export LD_LIBRARY_PATH=${lib.makeLibraryPath [ libgpiodV1 ]}:$LD_LIBRARY_PATH
        '';
          inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
        };
        devShell = self.devShells.${system}.default;
      });
}
