{ pkgs, ... }: {

  imports = [
    ./hyprland.nix
  ];

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  services.libinput.touchpad.naturalScrolling = false;

  # Exclude gnome utilities
  # If we don't want any utility we can use this instead of naming each one.
  # services.gnome.core-utilities.enable = false;
  environment.gnome.excludePackages = with pkgs.gnome; [
    cheese      # photo booth
    epiphany    # web browser
    pkgs.gedit  # text editor
    simple-scan # document scanner
    totem       # video player
    yelp        # help viewer
    evince      # document viewer
    geary       # email client
    seahorse    # password manager
    gnome-calculator
    gnome-clocks
    gnome-contacts
    gnome-logs
    gnome-music
    gnome-weather
    pkgs.gnome-connections

    # We'll keep these...
    # eog                     # image viewer
    # baobab                  # disk usage analyzer
    # gnome-font-viewer
    # gnome-calendar
    # gnome-maps              # maps
    # gnome-characters
    # gnome-screenshot        # Not working in wayland
    # gnome-system-monitor
    # gnome-disk-utility
  ];
}
