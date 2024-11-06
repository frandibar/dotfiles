# Settings for Syncthing

{ pkgs, ... }: {

  environment.systemPackages = with pkgs; [
    syncthing
  ];

  services.syncthing = {
      enable = true;
      user = "frandibar";
      # Default folder for new synced folders
      dataDir = "/home/frandibar/Sync";
      # Folder for Syncthing's settings and keys
      configDir = "/home/frandibar/.config/syncthing";
  };

}
