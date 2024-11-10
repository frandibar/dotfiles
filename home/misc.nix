{ pkgs, ... }: {

  home.packages = with pkgs; [
    ntp            # ntpdate for setting time properly after hibernate lag `sudo ntpdate time.google.com`
    nix-index      # search in nix store
#   atool          # deal with zip files
#   pstree         # alternative to ps
#   feh            # change background image
#   flatpak
#   xorg.xev       # capture keycodes
    calibre           # ebook management
    graphviz
    gnome.file-roller # zip files
    zathura           # pdf
  ];
}
