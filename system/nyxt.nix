# Settings for nyxt development

{ pkgs, ... }: {

  environment.systemPackages = with pkgs; [
    guix    # package manager
  ];

  # Ensure guix daemon can run.
  services.guix.enable = true;

}
