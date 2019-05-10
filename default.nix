{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc863" }:
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
      };
    };

    overrideDeriv = drv: f: drv.override (args: args // {
      mkDerivation = drv: args.mkDerivation (drv // f drv);
    });
    dontCheck = drv: overrideDeriv drv (drv: { doCheck = false; });

    loadLocal = self: name:
      self.callPackage (cabal2nixResult (./deps + "/${name}")) {};
    loadDirec = self: name:
      self.callPackage (cabal2nixResult (name)) {};
    tools = with ghc; [ cabal-install ghcid ];
    overrideCabal = pkg: nixpkgs.haskell.lib.overrideCabal pkg
      ({buildDepends ? [], ...}: {
        buildDepends = buildDepends ++ tools;
      });
    cabal2nixResult = src: nixpkgs.runCommand "cabal2nixResult" {
      buildCommand = ''
        cabal2nix --no-check --jailbreak --no-haddock file://${src} > $out
      '';
      buildInputs = [ nixpkgs.cabal2nix ];
    } "";
    package = ghc.callPackage (cabal2nixResult ./.) { };
    drv = overrideCabal package;
in if nixpkgs.lib.inNixShell then drv.env else drv
