# Settings for Syncthing
#
{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    syncthing
  ];

  services.syncthing = {
      enable = true;
      user = "frandibar";
      dataDir = "/home/frandibar/Sync";                 # Default folder for new synced folders
      configDir = "/home/frandibar/.config/syncthing";  # Folder for Syncthing's settings and keys
  };

}
