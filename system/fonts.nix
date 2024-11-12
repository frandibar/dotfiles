{ pkgs, ... }: {

  fonts.packages = with pkgs; [
      font-awesome
      dejavu_fonts
      fira-code
      fantasque-sans-mono
      etBook  # The one used in Edward Tufte's book
      material-design-icons
      # When using weather-icons the volume icon in status bar is
      # swapped with a rainy cloud in i3 :(
      weather-icons
  ];
}
