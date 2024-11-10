{ pkgs, ... }: {

  home.packages = with pkgs; [
    spotify

    # Music sheets
    musescore
    lilypond
    timidity                # midi
  ];
}
