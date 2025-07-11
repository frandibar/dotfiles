# Settings for home manager

{ config, ... }: {

  imports = [ ./default.nix ];

  home.stateVersion = "23.05";

  home.file = {
    ".config/alacritty/alacritty.toml".source = ./sources/alacritty.toml;

    # I do lot's of tinkering on my emacs config, so better leave
    # emacs out of home-manager to prevent the burden of rebuilding
    # for every change, at least until it reaches a stable point.
    # For emacs to work correctly, create symlinks to lisp
    # directory, init.el and early-init.el.
    # ".config/emacs" = {
    #   source = ./sources/emacs;
    #   recursive = true;
    # };

    ".config/fish" = {
      source = ./sources/fish;
      recursive = true;
    };
    ".config/git/config".source = ./sources/git/config;
    # ".config/swayidle/config".source = ./sources/swayidle/config;
    # ".config/swaylock/config".source = ./sources/swaylock/config;
    # ".config/waybar" = {
    #   source = ./sources/waybar;
    #   recursive = true;
    # };
    # ".config/hypr" = {
    #   source = ./sources/hyprland;
    #   recursive = true;
    # };
    ".config/i3" = {
      source = ./sources/i3;
      recursive = true;
    };
    #      ".config/i3/config.host".source = ./sources/i3/config.${config.networking.hostName};
    ".config/ledger/ledgerrc".source = ./sources/ledgerrc;
    ".config/X11/Xresources".source = ./sources/X11/Xresources;
    ".vimrc".source = ./sources/vimrc;
    ".sbclrc".source = ./sources/sbclrc;

    # In order to be able to handle org-protocol scheme from within emacs.
    # i.e. capturing a link from the browser.
    # Further info: https://orgmode.org/worg/org-contrib/org-protocol.html
    ".local/share/applications/org-protocol.desktop".source = ./sources/emacs/org-protocol.desktop;
  };

  # Let home manager install and manage itself.
  programs.home-manager.enable = true;
}
