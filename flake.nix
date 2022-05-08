{
  description = "Ema template app";
  inputs = {
    ema.url = "github:srid/ema/multisite";
    nixpkgs.follows = "ema/nixpkgs";
    tailwind-haskell.url = "github:srid/tailwind-haskell/master";
    flake-utils.follows = "ema/flake-utils";
    flake-compat.follows = "ema/flake-compat";

    unionmount.url = "github:srid/unionmount/master";
    unionmount.inputs.nixpkgs.follows = "ema/nixpkgs";

    heist = {
      url = "github:srid/heist/emanote-release--ghc9";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          name = "timedot-invoice";
          pkgs = nixpkgs.legacyPackages.${system};
          inherit (pkgs.lib.trivial) pipe flip;
          inherit (pkgs.lib.lists) optionals;
          hp = pkgs.haskellPackages;
          tailwind-haskell = inputs.tailwind-haskell.defaultPackage.${system};
          shellDeps = with hp; [
            cabal-fmt
            cabal-install
            ghcid
            haskell-language-server
            fourmolu
            pkgs.nixpkgs-fmt
            pkgs.treefmt
          ];
          project = returnShellEnv:
            hp.developPackage {
              inherit returnShellEnv name;
              root = ./.;
              withHoogle = false;
              overrides = self: super: with pkgs.haskell.lib; {
                ema = inputs.ema.defaultPackage.${system};
                tailwind = tailwind-haskell;
                unionmount = self.callCabal2nix "unionmount" inputs.unionmount { };
                heist-emanote = doJailbreak (dontCheck (self.callCabal2nix "heist-emanote" inputs.heist { }));
              };
              modifier = drv:
                let inherit (pkgs.haskell.lib) addBuildTools;
                in
                pipe drv
                  [
                    # Transform the Haskell derivation (`drv`) here.
                    (flip addBuildTools
                      (optionals returnShellEnv shellDeps))
                  ];
            };
        in
        {
          # Used by `nix build` & `nix run`
          defaultPackage = project false;

          # Used by `nix develop`
          devShell = project true;
        }) // {
      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
