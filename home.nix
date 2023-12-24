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
      ".config/doom" = {
        source = ./sources/doom;
        recursive = true;
      };
      # Commented out as it breaks rebuilds
      # Instead, do a manual git clone in ~/.config.emacs and use in the traditional non-nix way.
      # ".config/emacs" = {
      #   source = pkgs.fetchFromGitHub {
      #     owner = "doomemacs";
      #     repo = "doomemacs";
      #     rev = "master";
      #     sha256 = "sha256-qAI2FbELXIYDMsgMjn19MhYS9WaOxzpcWrATgQW+RP8=";
      #   };
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
      ".config/ledger/ledgerrc".source = ./sources/ledgerrc;
      ".config/X11/Xresources".source = ./sources/X11/Xresources;
      ".vimrc".source = ./sources/vimrc;
      ".sbclrc".source = ./sources/sbclrc;
    };
  };

}
