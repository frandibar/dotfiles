#!/usr/bin/env bash

# When trying out different configuration settings either for troubleshooting or
# trying out new things, it's tedious to require rebuilding with home-manager,
# so the purpose of this script is to "eject" from home-manager, and once we're
# satisfied with the results, we plug home-manager back in and commit our
# changes.

# WARNING:
#
# When toggling back on, nixos rebuild may not detect the changes so no symlinks
# will be created, leaving the system in a broken state. The workaround I found
# for the moment is modifying some file not involved in the ones we're messing
# up with in this script.

CONFIG=~/.config
SRC=~/github/dotfiles/sources

files=(
    "swayidle/config"
    "swaylock/config"
    "wlogout/layout"
    "waybar/config"
    "waybar/launch.sh"
    "hyprland/hyprland.conf"
    "doom/config.el"
    "doom/init.el"
    "doom/packages.el"
    "i3/config"
    "i3/i3blocks.conf"
    "i3/scripts/bluetooth.sh"
    "i3/scripts/battery2.py"
    "i3/scripts/calendar.sh"
    "i3/scripts/usb.py"
    "i3/scripts/volume.sh"
    "i3/scripts/wlan-dbm.sh"
)

unplug_file () {
    # If it's not a symbolic link then it's not a home-manager file
    if [ -L "$CONFIG/$1" ]; then
        echo "Renaming '$CONFIG/$1' into '$CONFIG/$1.bak'"
        mv "$CONFIG/$1" "$CONFIG/$1".bak
        echo "Copying '$SRC/$1' into '$CONFIG/$1'"
        cp "$SRC/$1" "$CONFIG/$1"
    else
        echo "File '$CONFIG/$1' is already unplugged"
    fi
}

plug_file () {
    # If it's a symbolic link then it's already plugged
    if [ -L "$CONFIG/$1" ]; then
        echo "File '$CONFIG/$1' is already plugged"
    else
        echo "Moving '$CONFIG/$1' into '$SRC/$1'"
        mv "$CONFIG/$1" "$SRC/$1"
        echo "Renaming '$CONFIG/$1.bak' into '$CONFIG/$1'"
        mv "$CONFIG/$1".bak "$CONFIG/$1"
    fi
}

map_files () {
    function=$1

    for file in "${files[@]}";
    do
        $function "$file"
    done
}

disable_hm () {
    # Replace CONFIG symlink files with SRC
    map_files unplug_file
    echo
    echo "You may now modify the config files under '$CONFIG' and see their immediate effect without having to rebuild NixOS."
    echo "Once you're happy with the results run '$0 enable' to prevent loosing the changes."
}

enable_hm () {
    # Move CONFIG files to SRC
    map_files plug_file
    echo
    echo "Restored files to previous state."
    echo "You may now rebuild NixOS to apply any changes."
}

status () {
    # if a file under $CONFIG is a symbolic link then it's enabled.
    if [ -L "$CONFIG/${files[0]}" ]; then
        echo "enabled"
    else
        echo "disabled"
    fi
}

main () {
    script_name=$0
    arg=$1

    stat=$(status)
    if [ "$arg" = "on" ]; then
        if [ "$stat" = "enabled" ]; then
            echo "Home Manager is already enabled. Aborting."
        else
            enable_hm
        fi
    elif [ "$arg" = "off" ]; then
        if [ "$stat" = "disabled" ]; then
            echo "Home Manager is already disabled. Aborting."
        else
            disable_hm
        fi
    elif [ "$arg" = "status" ]; then
        status
    else
        echo "Usage: $script_name on|off|status"
    fi
}

main "$1"
