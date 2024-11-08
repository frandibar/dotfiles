{ pkgs, ... }: {

  # These are not managed by home-manager
  home.packages = with pkgs; [
    sbcl       # steel bank common lisp
    cl-launch  # run lisp scripts
  ];

}
