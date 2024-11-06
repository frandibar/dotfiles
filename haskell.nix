# Settings for haskell language
#
{ pkgs, lib, ... }:

{
  environment.systemPackages = with pkgs; [
    ghc                      # glasgow haskell compiler
    haskell-language-server
    haskellPackages.hoogle
  ];
}
