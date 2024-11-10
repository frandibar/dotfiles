{ pkgs, ... }: {

  imports = [
    ./browser.nix
    ./calc.nix
    ./dev.nix
    ./emacs.nix
    ./go.nix
    ./image.nix
    ./ledger-nano.nix
    ./ledger.nix
    ./lisp.nix
    ./misc.nix
    ./music.nix
    ./social.nix
    ./syncthing.nix
    ./video.nix
  ];
}
