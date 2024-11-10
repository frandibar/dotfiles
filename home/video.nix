{ pkgs, ... }: {

  home.packages = with pkgs; [
    mpv               # video player
    smplayer          # gui for mpv
  ];
}
