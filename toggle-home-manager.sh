#!/usr/bin/env sh

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
SRC=~/dotfiles/sources

unplug_file () {
    # If it's not a symbolic link then it's not a home-manager file
    if [[ -L "$CONFIG/$1" ]]; then
        echo "Copying '$SRC/$1' into '$CONFIG/$1'"
        cp --force "$SRC/$1" "$CONFIG/$1"
    else
        echo "File '$CONFIG/$1' is already unplugged"
    fi
}

plug_file () {
    # If it's a symbolic link then it's already plugged
    if [[ -L $CONFIG/$1 ]]; then
        echo "File '$CONFIG/$1' is already plugged"
    else
        echo "Moving '$CONFIG/$1' into '$SRC/$1'"
        mv --force "$CONFIG/$1" "$SRC/$1"
    fi
}

files=(
    "doom/config.el"
    "doom/init.el"
    "doom/packages.el"
    # "i3/config"
    # "i3/i3blocks.conf"
    # "i3/scripts/bluetooth.sh"
    # "i3/scripts/calendar.sh"
    # "i3/scripts/usb.py"
    # "i3/scripts/volume.sh"
    # "i3/scripts/wlan-dbm.sh"
)

map_files () {
    for f in ${files[@]};
    do
        $1 $f
    done
}

disable () {
    # Replace CONFIG symlink files with SRC
    map_files unplug_file
}

enable () {
    # Move CONFIG files to SRC
    map_files plug_file
    echo "You may now run ./rebuild.sh"
}

status () {
    if [[ -L "$CONFIG/doom/config.el" ]]; then
        echo "enabled"
    else
        echo "disabled"
    fi
}

main () {
    if [ "$1" = "on" ]; then
        enable
    elif [ "$1" = "off" ]; then
        disable
    elif [ "$1" = "status" ]; then
        status
    else
        echo "Usage: toggle-home-manager.sh on|off|status"
    fi
}

main $1
