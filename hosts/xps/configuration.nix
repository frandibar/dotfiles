# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ../../configuration.nix
      ./hardware-configuration.nix
    ];

  # Setup keyfile
  boot.initrd.secrets = {
    "/crypto_keyfile.bin" = null;
  };

  # Enable swap on luks
  boot.initrd.luks.devices."luks-4b21db1e-2423-4ffc-8c42-39674e54f3db".device = "/dev/disk/by-uuid/4b21db1e-2423-4ffc-8c42-39674e54f3db";
  boot.initrd.luks.devices."luks-4b21db1e-2423-4ffc-8c42-39674e54f3db".keyFile = "/crypto_keyfile.bin";

  networking.hostName = "xps";

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;

  # HiDPI scale fonts
  # Extracted from https://nixos.wiki/wiki/Xorg
  services.xserver.dpi = 180;
  environment.variables = {
    GDK_SCALE = "2";
    GDK_DPI_SCALE = "0.5";
    _JAVA_OPTIONS = "-Dsun.java2d.uiScale=2";
  };

  home-manager.users.frandibar = {
    # Adjust pointer size on HiDPI
    home.pointerCursor = {
      x11.enable = true;
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ";
      size = 128;
    };
  };

}
