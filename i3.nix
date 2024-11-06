# Settings for i3 window manager
#
{ pkgs, lib, ... }:

{
  # i3 Setup
  #
  # Extracted from https://nixos.wiki/wiki/I3
  environment.pathsToLink = [ "/libexec" ];
  # services.displayManager.defaultSession = "none+i3";
  services.xserver = {
    desktopManager.xterm.enable = false;
    desktopManager.gnome.enable = true;

    displayManager.gdm.enable = true;          # for screen lock from gnome

    windowManager.i3.enable = true;
    windowManager.i3.extraPackages = with pkgs; [
        dmenu     # application launcher most people use
        i3status  # gives you the default i3 status bar
        i3lock    # default i3 screen locker
        i3blocks  # if you are planning on using i3blocks over i3status
    ];
  };

  environment.systemPackages = with pkgs; [
    arandr           # multiple monitor setup
    libnotify        # provides notify-send for toasts
    dunst            # notifications
    networkmanagerapplet  # provides nm-applet for wifi in tray
    yad              # i3blocks calendar
    xdotool          # i3blocks calendar
    acpi             # i3blocks battery
    iw               # i3blocks wlan
    xkblayout-state  # i3blocks keyboard layout
    # replaced with grim and slurp
    # flameshot        # screenshots
    lxappearance   # change theme icons and fonts
  ];
}
