{ pkgs, ... }: {

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    enableFishIntegration = true;
    nix-direnv.enable = true;
  };

  home.packages = with pkgs; [
    gnumake
    git
    difftastic
    black                   # python formatter
    python3Minimal
    jq                      # json query
    nodejs_20
    nixfmt-classic          # nix formatter
    sqlite
    sqlitebrowser
    nodePackages.prettier
  ];
}
