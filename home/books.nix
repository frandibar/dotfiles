{ pkgs, ... }: {

  home.packages = with pkgs; [
    calibre           # ebook management
    zathura           # pdf
    python311Packages.oscrypto   # for Calibre DeACSM plugin
  ];
}
