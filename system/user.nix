# Settings for the user environment.

{ pkgs, ... }: {

  environment.variables.EDITOR = "vim";

  # nixos-rebuild build-vm test user, see
  # https://nixos.wiki/wiki/NixOS:nixos-rebuild_build-vm
  users.users.nixostest = {
    isNormalUser = true;
    initialPassword = "password";
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.frandibar = {
    isNormalUser = true;
    description = "frandibar";
    extraGroups = [ "networkmanager" "wheel" ];
    shell = pkgs.fish;
    # Hashed using `mkpasswd --method=sha-512 --rounds=2000000`
    hashedPassword = "$6$rounds=2000000$GZ2APExIDrUDt0nC$79ehcC.gMttkHbnbNM.Xc.yoI.hborXNeKgm2I3jPD38EY4tkmieDgJzvgPuO2Rv00isxPEJX/TbREohEde3t1";
  };

}
