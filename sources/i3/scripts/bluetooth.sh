#!/usr/bin/env sh

status=`bluetoothctl show | grep Powered: | awk '{print $2}'`
echo 
echo 
if [[ $status == "yes" ]]; then
    echo "#00FF00"
elif [[ $status == "no" ]]; then
    echo "#222222"
else
    echo "#FF0000"
fi
echo "#000000"
