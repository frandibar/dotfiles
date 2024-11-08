# Settings for Syncthing

{ pkgs, ... }: {

  home.packages = with pkgs; [
    syncthing
  ];

}
