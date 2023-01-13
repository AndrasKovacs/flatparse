{
  inputs = {
    #nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    #flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, ... }: {
        haskellProjects.default = {
          #haskellPackages = pkgs.haskell.packages.ghc944;
          haskellPackages = pkgs.haskell.packages.ghc925;
          packages = {
            flatparse.root = ./.;
          };
          # 2023-01-12 raehik: ghcid seems broken on nixpkgs GHC 9.4.{3,4}
          buildTools = hp: { ghcid = null; hls = null; };
          # overrides = self: super: { }
          # hlintCheck.enable = true;
          # hlsCheck.enable = true;
        };
      };
    };
}
