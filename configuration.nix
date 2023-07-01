# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }: {
  #
  # NIX
  #
  imports = [
      ./home.nix
    ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  #
  # BOOT
  #
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  #
  # FILE SYSTEM
  #
  # boot.supportedFilesystems = [ "ntfs" "vfat" "exfat" ];

  # Keybase
  services.keybase.enable = true;
  services.kbfs.enable = true;

  services.syncthing = {
      enable = true;
      user = "frandibar";
      dataDir = "/home/frandibar/Sync";                 # Default folder for new synced folders
      configDir = "/home/frandibar/.config/syncthing";  # Folder for Syncthing's settings and keys
  };

  #
  # NETWORKING
  #
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking for easy wireless configuration.
  networking.networkmanager.enable = true;

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  #
  # LOCALIZATION
  #
  time.timeZone = "America/Argentina/Buenos_Aires";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "es_AR.UTF-8";
    LC_IDENTIFICATION = "es_AR.UTF-8";
    LC_MEASUREMENT = "es_AR.UTF-8";
    LC_MONETARY = "es_AR.UTF-8";
    LC_NAME = "es_AR.UTF-8";
    LC_NUMERIC = "es_AR.UTF-8";
    LC_PAPER = "es_AR.UTF-8";
    LC_TELEPHONE = "es_AR.UTF-8";
    LC_TIME = "es_AR.UTF-8";
  };

  #
  # DESKTOP
  #

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  services.xserver.libinput.touchpad.naturalScrolling = false;

  #
  # i3 Setup
  #
  # Extracted from https://nixos.wiki/wiki/I3
  environment.pathsToLink = [ "/libexec" ];
  services.xserver = {

    desktopManager.xterm.enable = false;
    desktopManager.gnome.enable = true;

    displayManager.defaultSession = "none+i3";
    displayManager.gdm.enable = true;          # for screen lock from gnome

    windowManager.i3.enable = true;
    windowManager.i3.extraPackages = with pkgs; [
        dmenu     # application launcher most people use
        i3status  # gives you the default i3 status bar
        i3lock    # default i3 screen locker
        i3blocks  # if you are planning on using i3blocks over i3status
    ];
  };

  # Exclude gnome utilities
  # If we don't want any utility we can use this instead of naming each one.
  # services.gnome.core-utilities.enable = false;
  environment.gnome.excludePackages = with pkgs.gnome; [
    cheese      # photo booth
    epiphany    # web browser
    gedit       # text editor
    simple-scan # document scanner
    totem       # video player
    yelp        # help viewer
    evince      # document viewer
    file-roller # archive manager
    geary       # email client
    seahorse    # password manager
    gnome-clocks
    gnome-contacts
    gnome-logs
    gnome-music
    gnome-weather
    pkgs.gnome-connections

    # We'll keep these...
    # eog         # image viewer
    # baobab      # disk usage analyzer
    # gnome-font-viewer
    # gnome-calculator
    # gnome-calendar
    # gnome-maps
    # gnome-characters
    # gnome-screenshot
    # gnome-system-monitor
    # gnome-disk-utility
  ];

  #
  # FONTS
  #
  fonts.fonts = with pkgs; [
      font-awesome
      dejavu_fonts
      fira-code

      # These fonts are used by Doom Emacs
      # Unfortunately `doom doctor` still reports them as missing...
      # Don't use weather-icons as the volume icon in status bar is swapped with a rainy cloud!
      material-design-icons
      # weather-icons
  ];

  #
  # HARDWARE
  #

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  #
  # USER ENVIRONMENT
  #
  environment.variables.EDITOR = "vim";

  # nixos-rebuild build-vm test user, see
  # https://nixos.wiki/wiki/NixOS:nixos-rebuild_build-vm
  users.users.nixostest = {
    isNormalUser = true;
    initialPassword = "password";
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.frandibar = {
    isNormalUser = true;
    description = "frandibar";
    extraGroups = [ "networkmanager" "wheel" ];
    shell = pkgs.fish;
    # Hashed using `mkpasswd --method=sha-512 --rounds=2000000`
    hashedPassword = "$6$rounds=2000000$GZ2APExIDrUDt0nC$79ehcC.gMttkHbnbNM.Xc.yoI.hborXNeKgm2I3jPD38EY4tkmieDgJzvgPuO2Rv00isxPEJX/TbREohEde3t1";
  };

  #
  # APPLICATIONS
  #

  # List packages installed in system profile. To search, run:
  # $ nix search nixpkgs vim
  environment.systemPackages = with pkgs; [

    # System
    vim
    fish
    curl
    wget
    alacritty      # terminal
    procs          # alternative to ps
    fd             # alternative to find
#   nushell
    rofi           # alternative to dmenu
    fuse
    ntfs3g
    bat            # alternative to cat
    pass
    killall
    gnupg
    unzip
    arandr         # multiple monitor setup
    file
    mc             # file browser
    tree
    tldr           # brief man
    xsel           # copy mouse selection
    openssh
    udiskie        # automount usb
    tmux           # terminal multiplexer
    tmuxp          # automate terminals
    rm-improved    # rip
    lxappearance   # change theme icons and fonts
    appimage-run
    direnv         # load env depending on current dir
    htop           # alternative to top
    brightnessctl  # adjust brightness
    libnotify      # provides notify-send for toasts
    gnumake
#   atool          # deal with zip files
#   pstree         # alternative to ps
#   feh            # change background image
#   flatpak
#   xorg.xev       # capture keycodes

    # Internet
    firefox
    nyxt
    chromium

    # Programming
    emacs
    emacsPackages.dired-fdclone
    # These fonts are used by Doom Emacs
    # Unfortunately `doom doctor` still reports them as missing
    emacsPackages.fontawesome
    emacsPackages.octicons
    emacsPackages.all-the-icons
    emacsPackages.nerd-icons

    git
    difftastic
    python3Minimal
    ghc                     # haskell
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-review
    elmPackages.elm-json
    elmPackages.elm-test
    jq                      # json query
    nodejs_20
    nixfmt                  # nix formatter
#   meld                    # diffs

    # Musicsheets
    musescore
    lilypond
    timidity          # midi

    # Misc
    spotify
    telegram-desktop
    zoom-us
    zathura           # pdf
    mpv               # video player
    flameshot         # screenshots
    #gnome.sushi      # file preview
    syncthing         # file sync
    keybase
    keybase-gui

    # Dependencies
    yad              # i3blocks calendar
    xdotool          # i3blocks calendar
    dunst            # notifications
    acpi             # i3blocks battery
    iw               # i3blocks wlan
    xkblayout-state  # i3blocks keyboard layout
    ripgrep          # emacs
  ];

  programs.fish.enable = true;

  programs.gnupg.agent = {
    enable = true;
    # enableSSHSupport = true;
  };
}
