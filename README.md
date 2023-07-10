# Dotfiles

My NixOS configuration files.

## If installing NixOS from scratch

Generate a public key and add it to github:

Generate a key without a passphrase
```
ssh-keygen
```

Visit https://github.com/settings/keys and add the contents of `~/.ssh/id_rsa.pub`

Download the repository
```
nix-shell -p vim git
mkdir ~/github
cd ~/github
git clone git@github.com:frandibar/dotfiles.git
```

If the machine being installed was already configured, then the machine specific configuration files are already present in the repository.
Create a symlink to the proper configuration and rebuild

```
sudo rm /etc/nixos/configuration.nix
sudo ln -s /home/frandibar/github/dotfiles/hosts/<hostname>/configuration.nix /etc/nixos/configuration.nix
```

If the machine being installed was never configured, then we must create the configuration files specific to this machine.

```
mkdir /home/frandibar/github/dotfiles/hosts/<hostname>
cp /etc/nixos/hardware-configuration.nix /home/frandibar/github/dotfiles/hosts/<hostname>/
sudo rm /etc/nixos/configuration.nix
```

Create an initial configuration file under `~/github/dotfiles/hosts/<hostname>/configuration.nix` containing:

```
{ config, pkgs, ... }: {

  imports = [
    ../../configuration.nix
    ./hardware-configuration.nix
  ];
  
  networking.hostName = "<hostname>"
}
```

And make a symlink to this file
```
sudo ln -s /home/frandibar/github/dotfiles/hosts/<hostname>/configuration.nix /etc/nixos/configuration.nix
```

Now that the new configuration is in place, rebuild NixOS

```
sudo nixos-rebuild switch
```

Now everything should be configured properly, restart just in case to check everything is working as expected.

Any machine specific fine tuning should be added to the host config.


## First time only

Create a symlink to `configuration.nix` at `/etc/nixos` so that we don't need to specify the file to nix-build explicitly. If not we must run
```
sudo nixos-rebuild switch -I nixos-config=configuration.nix`
```

Add channel for home manager:
```
sudo nix-channel --add  https://github.com/nix-community/home-manager/archive/release-23.05.tar.gz home-manager
sudo nix-channel --update
```

Checkout Doom Emacs repository
```
cd ~
git clone git@github.com:doomemacs/doomemacs.git
ln -s /home/frandibar/doomemacs /home/frandibar/.config/emacs
cd doomemacs/bin
./doom install
./doom sync
```
Some cases may require creating a swap file if not partition was set. If so declare it in `hardware-configuration.nix` for the specific host.
Set the size as big as available RAM in order to be able to hibernate.
```
sudo dd if=/dev/zero of=/swap-file count=36000 bs=1M
```

## Build

Whenever a configuration file changes, run
```
sudo nixos-rebuild switch
```

