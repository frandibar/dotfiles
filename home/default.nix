{ pkgs, ... }: {

  imports = [
    ./misc.nix
    ./browser.nix
    ./lisp.nix
    ./emacs.nix
    ./ledger.nix
    ./ledger-nano.nix
    ./syncthing.nix
  ];
}
