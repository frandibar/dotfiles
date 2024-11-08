#!/usr/bin/env sh

# Show a toast with the current volume level in percentage.
# An example output of `brightnessctl`:
#
#Device 'intel_backlight' of class 'backlight':
#	Current brightness: 937 (100%)
#	Max brightness: 937

level=`brightnessctl | awk -F"[()]" '/Current brightness:/ { print $2 }'`

# We pass the hint (brightness, but could be anything) so that the notification
# gets replaced on update.
# -h string:x-canonical-private-synchronous:brightness

# Show a progress bar
notify-send -t 1000 -h int:value:$level -h string:x-canonical-private-synchronous:brightness -a "" "Brightness: "$level
