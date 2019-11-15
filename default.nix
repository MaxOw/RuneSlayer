let
  bootstrap = import <nixpkgs> {};
  nixpkgs_json = builtins.fromJSON (builtins.readFile ./nix/nixpkgs.json);
  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs_json) rev sha256;
  };
  pkgs = import src {};

  compiler = "ghc865";
  ghc = pkgs.haskell.packages.${compiler}.override {
      overrides = self: super: {
        Carnot     = loadLocal self ./Carnot;
        dhall      = super.dhall_1_24_0;
        dhall-json = pkgs.haskell.lib.dontCheck super.dhall-json_1_3_0;
      };
    };
  tools = with ghc; [ cabal-install ghcid pkgs.git ];

  loadLocal = self: name: self.callPackage (cabal2nixResultLocal (name)) {};
  overrideCabal = pkg: pkgs.haskell.lib.overrideCabal pkg
    ({buildDepends ? [], ...}: {
      buildDepends = buildDepends ++ tools;
    });
  cabal2nixResult = url: pkgs.runCommand "cabal2nixResult" {
    buildCommand = ''
      cabal2nix --no-check --jailbreak --no-haddock ${url} > $out
    '';
    buildInputs = [ pkgs.cabal2nix ];
  } "";
  cabal2nixResultLocal = path: cabal2nixResult "file://${path}";
  package = ghc.callPackage (cabal2nixResultLocal ./.) {};
  drv = overrideCabal package;

in if pkgs.lib.inNixShell then drv.env else drv
