# Settings for emacs

{ pkgs, ... }: {

  environment.systemPackages = with pkgs; [

    # emacs                 # font looks blurry in wayland
    emacs29-pgtk
    emacsPackages.dired-fdclone

    # Commented out as I'm no longer using Doom
    # html-tidy               # Doom doctor
    # nodePackages.stylelint  # Doom doctor
    # jsbeautifier            # Doom doctor
    # shellcheck              # Doom doctor

    # These fonts are used by doom-themes
    emacsPackages.all-the-icons
    emacsPackages.fontawesome
    emacsPackages.octicons
    emacsPackages.nerd-icons

    imagemagick      # casual suite

    ripgrep
    xclip            # emacs-everywhere
    xorg.xwininfo    # emacs-everywhere
  ];

  # To install and enable the systemd user service for Emacs daemon.
  # Right now I prefer to get the server lauched when running my first
  # emacs instance, so I'm commenting this line.
  # services.emacs.enable = true;

  # Set emacsclient as default editor.
  # FIXME: this is not being honored, $EDITOR = vim
  services.emacs.defaultEditor = true;
}
