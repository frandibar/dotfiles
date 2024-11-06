# Settings for elm language
#
{ pkgs, lib, ... }:

{
  environment.systemPackages = with pkgs; [
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-review
    elmPackages.elm-json
    elmPackages.elm-test
  ];
}
