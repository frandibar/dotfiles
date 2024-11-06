# Settings for haskell language
#
{ pkgs, ... }: {

  environment.systemPackages = with pkgs; [
    ghc                      # glasgow haskell compiler
    haskell-language-server
    haskellPackages.hoogle
  ];
}
