{ config, pkgs, ... }: {

  imports = [
      ../../configuration.nix
      ./hardware-configuration.nix
  ];

  networking.hostName = "rog";

  # Enable touchpad support
  services.xserver.libinput.enable = true;

}
