# Settings for ledger accounting
#
{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    ledger
    hledger
    hledger-ui
    hledger-web

    # Required by icsv2ledger
    # https://github.com/quentinsf/icsv2ledger
    python311Packages.gnureadline
  ];

  hardware.ledger.enable = true;
}
