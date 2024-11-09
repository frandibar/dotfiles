# Settings for hyprland window manager
#
{ pkgs, ... }: {

  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
  };

  hardware.opengl.enable = true;

  # Handle desktop program interactions, screen sharing, file and link
  # openning, etc.
  xdg.portal.enable = true;
  # Commented out as it raises an error with nixos-rebuild.
  # xdg.portal.extraPortals = [ pkgs.xdg-desktop-portal-gtk ];

  # Force apps to use wayland, as described here
  # https://wiki.hyprland.org/Getting-Started/Master-Tutorial/
  environment.variables.NIXOS_OZONE_WL = "1";

  # Nice login screen.
  services.xserver = {
    # Show alternative desktop options just in case...
    desktopManager.xterm.enable = false;
    desktopManager.gnome.enable = true;
    displayManager.gdm.enable = true;   # for screen lock from gnome
  };

  services.displayManager.defaultSession = "hyprland";

  environment.systemPackages = with pkgs; [
    libnotify     # send notifications
    swaynotificationcenter   # provides swaync
    networkmanagerapplet  # provides nm-applet for wifi in tray
    rofi-wayland  # launcher
    waybar        # status bar
    hyprpaper     # wallpaper utilities
    pavucontrol   # volume control
    wlogout       # logout menu for wayland
    swaylock      # lock screen for wayland
    swayidle      # lock when idle
    hyprshot      # screen capture
    wl-clipboard  # colorpicker to clipboard
    hyprpicker    # colorpicker
    wf-recorder   # screen recorder
    playerctl     # play/pause keys
  ];
}
