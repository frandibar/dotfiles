# Settings for hardware wallet Ledger Nano S

{ pkgs, ... }: {

  environment.systemPackages = with pkgs; [
    ledger-live-desktop
    ledger-udev-rules
  ];

  hardware.ledger.enable = true;
}
