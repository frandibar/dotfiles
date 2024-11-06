# Settings for common lisp
#
{ pkgs, lib, ... }:

{
  environment.systemPackages = with pkgs; [
    sbcl       # steel bank common lisp
    cl-launch  # run lisp scripts
  ];
}
