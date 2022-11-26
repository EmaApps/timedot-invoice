{
  description = "timedot-invoice";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    # Haskell overrides
    ema.url = "github:EmaApps/ema";
    ema.flake = false;
    heist.url = "github:snapframework/heist"; # Waiting for 1.1.1.0 on nixpkgs cabal hashes
    heist.flake = false;
    heist-extra.url = "github:srid/heist-extra";
    heist-extra.flake = false;
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, haskell-flake, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        haskell-flake.flakeModule
      ];
      perSystem = { self', inputs', pkgs, ... }: {
        # "haskellProjects" comes from https://github.com/srid/haskell-flake
        haskellProjects.default = {
          packages.timedot-invoice.root = ./.;
          buildTools = hp: {
            inherit (pkgs)
              treefmt
              nixpkgs-fmt
              foreman;
            inherit (hp)
              cabal-fmt
              fourmolu;
          };
          source-overrides = {
            inherit (inputs) heist heist-extra;
            ema = inputs.ema + /ema;
            ema-generics = inputs.ema + /ema-generics;
            ema-extra = inputs.ema + /ema-extra;
          };
          overrides = self: super: with pkgs.haskell.lib; {
            ema-generics = dontCheck super.ema-generics;
            heist = dontCheck super.heist; # Tests are broken.
          };
        };
      };
    };
}
