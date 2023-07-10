{ config, pkgs, ... }: {

  imports = [
      ../../configuration.nix
      ./hardware-configuration.nix
  ];

  networking.hostName = "xps";

  # Enable touchpad support
  services.xserver.libinput.enable = true;

}
