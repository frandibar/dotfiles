{ pkgs, ... }: {

  imports = [
    ./nyxt.nix
    ./syncthing.nix
  ];

  # List packages installed in system profile. To search, run:
  # $ nix search nixpkgs <package>
  environment.systemPackages = with pkgs; [

    vim
    fish
    curl
    wget
#   alacritty      # terminal
    kitty          # terminal
    procs          # alternative to ps
    fd             # alternative to find
#   nushell
    rofi           # alternative to dmenu
    fuse
    ntfs3g
    bat            # alternative to cat
    pass           # passwords manager
    killall
    gnupg
    unzip
    file
    tree
    htop           # alternative to top
    brightnessctl  # adjust brightness
    rlwrap         # readline wrap for arrow keys to work in repl such as sbcl, python.
    mc             # file browser
    tldr           # brief man
    xsel           # copy mouse selection
    openssh
    udiskie        # automount usb
    tmux           # terminal multiplexer
    tmuxp          # automate terminals
    rm-improved    # rip
    appimage-run
  ];

  programs.fish.enable = true;

  programs.gnupg.agent = {
    enable = true;
    # enableSSHSupport = true;
  };

}
