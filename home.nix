{ config, pkgs, ... }: {

  imports = [
    <home-manager/nixos>
  ];

  # Install packages to /etc/profiles instead of ~/.nix-profile
  home-manager.useUserPackages = true;

  # By default, Home Manager uses a private pkgs instance that is
  # configured via the home-manager.users.<name>.nixpkgs options.
  # To instead use the global pkgs that is configured via the system level nixpkgs options, set
  home-manager.useGlobalPkgs = true;

  home-manager.users.frandibar = {
    # Should be the same value as system.stateVersion defined in `configuration.nix`
    home.stateVersion = "23.05";

    home.file = {
      ".config/alacritty/alacritty.yml".source = ./sources/alacritty.yml;
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
      ".config/i3" = {
        source = ./sources/i3;
        recursive = true;
      };
      ".config/i3/config.host".source = ./sources/i3/config.${config.networking.hostName};
      ".config/ledger/ledgerrc".source = ./sources/ledgerrc;
      ".config/X11/Xresources".source = ./sources/X11/Xresources;
      ".vimrc".source = ./sources/vimrc;
      ".sbclrc".source = ./sources/sbclrc;
    };
  };

}
