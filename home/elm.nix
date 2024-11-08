# Settings for elm language

{ pkgs, ... }: {

  home.packages = with pkgs; [
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-review
    elmPackages.elm-json
    elmPackages.elm-test
  ];
}
