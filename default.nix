{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc865" }:
let ghc = nixpkgs.haskell.packages.${compiler}.override {
      overrides = self: super: {
        # TODO: get packages from github
        # generic-lens-labels = loadLocal self "generic-lens-labels";
        # named               = loadLocal self "named";
        # concat-satisfy        = loadLocal self "concat/satisfy";
        # concat-known          = loadLocal self "concat/known";
        # concat-inline         = loadLocal self "concat/inline";
        # concat-classes        = loadLocal self "concat/classes";
        # concat-plugin         = loadLocal self "concat/plugin";
        # concat-graphics       = loadLocal self "concat/graphics";
        # concat-examples       = loadLocal self "concat/examples";

        reload-utils    = loadDirec self "${./../reload-utils}";
        Carnot          = loadDirec self "${./../Carnot}";
        # dhall           = loadUrl self dhall-url;
        dhall           = super.dhall_1_24_0;
        dhall-json      = dontCheck super.dhall-json_1_3_0;
      };
    };

    overrideDeriv = drv: f: drv.override (args: args // {
      mkDerivation = drv: args.mkDerivation (drv // f drv);
    });
    dontCheck = drv: overrideDeriv drv (drv: { doCheck = false; });

    loadUrl = self: name: self.callPackage (cabal2nixResult name) {};
    loadLocal = self: name:
      self.callPackage (cabal2nixResultLocal (./deps + "/${name}")) {};
    loadDirec = self: name:
      self.callPackage (cabal2nixResultLocal (name)) {};
    tools = with ghc; [ cabal-install ghcid ];
    overrideCabal = pkg: nixpkgs.haskell.lib.overrideCabal pkg
      ({buildDepends ? [], ...}: {
        buildDepends = buildDepends ++ tools;
      });
    cabal2nixResultLocal = src:          cabal2nixResult "file://${src}";
    cabal2nixResultCabal = name-version: cabal2nixResult "cabal://${name-version}";
    cabal2nixResult = url: nixpkgs.runCommand "cabal2nixResult" {
      buildCommand = ''
        cabal2nix --no-check --jailbreak --no-haddock ${url} > $out
      '';
      buildInputs = [ nixpkgs.cabal2nix ];
    } "";
    package = ghc.callPackage (cabal2nixResultLocal ./.) { };
    drv = overrideCabal package;
in if nixpkgs.lib.inNixShell then drv.env else drv
