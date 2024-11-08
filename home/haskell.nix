# Settings for haskell language

{ pkgs, ... }: {

  home.packages = with pkgs; [
    ghc                      # glasgow haskell compiler
    haskell-language-server
    haskellPackages.hoogle
  ];
}
