# Settings for keybase

{ pkgs, ... }: {

  environment.systemPackages = with pkgs; [
    keybase
    keybase-gui
  ];

  services.keybase.enable = true;
  services.kbfs.enable = true;
}
