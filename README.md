# Dotfiles

My NixOS configuration files.

First time only:

Create a symlink to `configuration.nix` at `/etc/nixos` so that we don't need to specify the file to nix-build explicitly. If not we must run
`sudo nixos-rebuild switch -I nixos-config=configuration.nix`

Whenever a configuration file changes, run
`sudo nixos-rebuild switch`
