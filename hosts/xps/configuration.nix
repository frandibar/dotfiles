{ config, pkgs, ... }: {

  imports = [
      ../../configuration.nix
      ./hardware-configuration.nix
  ];

  networking.hostName = "xps";

  # Enable touchpad support
  services.xserver.libinput.enable = true;

  # HiDPI scale fonts
  # Extracted from https://nixos.wiki/wiki/Xorg
  services.xserver.dpi = 180;
  environment.variables = {
    GDK_SCALE = "2";
    GDK_DPI_SCALE = "0.5";
    _JAVA_OPTIONS = "-Dsun.java2d.uiScale=2";
  };
}
