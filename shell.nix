# remember:
# $ nix-shell --argstr compiler ghc802

{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }: # ghc821

let
  inherit (nixpkgs) pkgs;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  ghc = haskellPackages.ghcWithPackages (ps: with ps; [
    cabal-install
    #ghc-mod
    hlint
    #------------------------
    HaTeX
    mtl
    parsec
    transformers
    #ipprint
    text
  ]);
in

pkgs.stdenv.mkDerivation {
  name = "my-haskell-env-gsynt";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
