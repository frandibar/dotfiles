# Settings for common lisp

{ pkgs, ... }: {

  environment.systemPackages = with pkgs; [
    sbcl       # steel bank common lisp
    cl-launch  # run lisp scripts
  ];
}
