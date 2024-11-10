{ pkgs, ... }: {

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    enableFishIntegration = true;
    nix-direnv.enable = true;
  };

  home.packages = with pkgs; [
    gnumake
    ntp            # ntpdate for setting time properly after hibernate lag `sudo ntpdate time.google.com`
    nix-index      # search in nix store
#   atool          # deal with zip files
#   pstree         # alternative to ps
#   feh            # change background image
#   flatpak
#   xorg.xev       # capture keycodes

    # Programming

    git
    difftastic
    black                   # python formatter
    python3Minimal

    jq                      # json query
    nodejs_20
    nixfmt-classic          # nix formatter
#   meld                    # diffs
    sqlite
    sqlitebrowser
    nodePackages.prettier

    # Music sheets
    musescore
    lilypond
    timidity                # midi

    # Calculator
    qalculate-gtk
    libqalculate      # qalc
    rofi-calc

    # Go
    gnugo             # play go
    cgoban            # kgs client, sgf editor
    sgfutils          # utils such as sgfcheck

    # Image
    gimp
    inkscape

    # Music
    spotify

    # Social
    telegram-desktop

    # Misc
    calibre           # ebook management
    graphviz
    gnome.file-roller # zip files
    gnumeric
    zoom-us
    zathura           # pdf
    #gnome.sushi      # file preview
    #kicad-small       # printed circuit board design
    #metabase          # data analytics
    # godot           # game engine
    gnuplot

    # Video
    mpv               # video player
    smplayer          # gui for mpv
  ];
}
