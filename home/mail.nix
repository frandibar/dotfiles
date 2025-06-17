{ pkgs, ... }: {

  home.packages = with pkgs; [
    notmuch   # email search utility
    mb2md     # convert mbox file to maildir
  ];
}
