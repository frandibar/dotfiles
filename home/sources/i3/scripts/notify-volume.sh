#!/usr/bin/env sh

# Show a toast with the current volume level in percentage and if muted.
# An example output of `amixer sget Master`:
#
#Simple mixer control 'Master',0
#  Capabilities: pvolume pswitch pswitch-joined
#  Playback channels: Front Left - Front Right
#  Limits: Playback 0 - 65536
#  Mono:
#  Front Left: Playback 52430 [80%] [on]
#  Front Right: Playback 52430 [80%] [on]

volumeData=(`amixer sget Master | awk -F"[][]" '/Left:/ { print $2,$4 }'`)
level="${volumeData[0]}"
onOff="${volumeData[1]}"

# We pass the hint (volume, but could be anything) so that the notification gets
# replaced on update.
# -h string:x-canonical-private-synchronous:volume

if [[ $onOff == "off" ]]; then
    notify-send -t 1000 -h string:x-canonical-private-synchronous:volume -a "" "Volume: Muted"
else
    # Show a progress bar
    notify-send -t 1000 -h int:value:$level -h string:x-canonical-private-synchronous:volume -a "" "Volume: "$level
fi
