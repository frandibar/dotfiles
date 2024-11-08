# Settings for Dell XPS laptop

{ config, pkgs, home-manager, ... }: {
  imports = [
      ../../system
      ./hardware-configuration.nix
    ];

  # Setup keyfile
  boot.initrd.secrets = {
    "/crypto_keyfile.bin" = null;
  };

  # Enable swap on luks
  boot.initrd.luks.devices."luks-4b21db1e-2423-4ffc-8c42-39674e54f3db".device = "/dev/disk/by-uuid/4b21db1e-2423-4ffc-8c42-39674e54f3db";
  boot.initrd.luks.devices."luks-4b21db1e-2423-4ffc-8c42-39674e54f3db".keyFile = "/crypto_keyfile.bin";

  networking.hostName = "xps";

  # Unfortunately this doesn't work, i3 fails to load
  # services.xserver.windowManager.i3.configFile = builtins.getEnv "HOME" + "/.config/i3/config.xps";

  # Enable touchpad support
  services.libinput.enable = true;

  # HiDPI scale fonts
  # Extracted from https://nixos.wiki/wiki/Xorg
  services.xserver.dpi = 180;
  environment.variables = {
    GDK_SCALE = "2";
    GDK_DPI_SCALE = "0.5";
    _JAVA_OPTIONS = "-Dsun.java2d.uiScale=3";
  };

  home-manager.users.frandibar = {
    # Adjust pointer size on HiDPI
    home.pointerCursor = {
      x11.enable = true;
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ";
      size = 128;
    };
  };

}
