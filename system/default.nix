{ config, pkgs, ... }: {

  imports = [
    ./apps.nix
    ./boot.nix
    ./desktop.nix
    ./fonts.nix
    ./hardware.nix
    ./localization.nix
    ./network.nix
    ./user.nix
  ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

  # Keep the system up to date (https://nixos.org/manual/nixos/stable/#sec-upgrading)
  # I'm keeping this commented out to prevent any breaking changes.
  # system.autoUpgrade.enable = true;
  # system.autoUpgrade.allowReboot = false;

  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
}
