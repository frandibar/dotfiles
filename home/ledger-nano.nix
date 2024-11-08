# Settings for hardware wallet Ledger Nano S

{ pkgs, ... }: {

  home.packages = with pkgs; [
    ledger-live-desktop
    ledger-udev-rules
  ];

}
