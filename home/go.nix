{ pkgs, ... }: {

  home.packages = with pkgs; [
    gnugo             # play go
    cgoban            # kgs client, sgf editor
    sgfutils          # utils such as sgfcheck
  ];
}
