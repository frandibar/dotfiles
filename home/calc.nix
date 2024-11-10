{ pkgs, ... }: {

  home.packages = with pkgs; [
    gnumeric
    gnuplot
    qalculate-gtk
    libqalculate      # qalc
    rofi-calc
  ];
}
