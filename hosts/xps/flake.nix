{
  description = "My NixOS flake configuration";

  inputs = {
    # NixOS official package source
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";

    # Match same version as in nixpkgs to avoid warning.
    home-manager.url = "github:nix-community/home-manager/release-24.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, home-manager, ... }@inputs: {

    nixosConfigurations.xps = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./configuration.nix
        home-manager.nixosModules.home-manager {
          # Install packages to /etc/profiles instead of ~/.nix-profile
          home-manager.useGlobalPkgs = true;
          # By default, Home Manager uses a private pkgs instance that is
          # configured via the home-manager.users.<name>.nixpkgs options.
          # To instead use the global pkgs that is configured via the
          # system level nixpkgs options, set
          home-manager.useUserPackages = true;
          home-manager.users.frandibar = import ../../home/home.nix;

          # Optionally, use home-manager.extraSpecialArgs to pass
          # arguments to home.nix

          # A backup is created when transitioning from a non-managed
          # to managed app.  i.e. firefox profile is backed up under
          # ~/.mozilla/firefox/profiles.ini.backup
          home-manager.backupFileExtension = "backup";
        }
      ];
    };
  };
}
