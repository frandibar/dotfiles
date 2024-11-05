#!/bin/sh

# Get the active workspace
active_workspace=$(hyprctl activeworkspace | grep "ID" | awk '{print $3}')

# Get the name of the next monitor
monitors=($(hyprctl monitors | grep "Monitor" | awk '{print $2}'))
current_monitor=$(hyprctl monitors -j | jq -r '.[] | select(.focused) | .name')

# Find the index of the current monitor
current_index=0
for i in "${!monitors[@]}"; do
  if [ "${monitors[$i]}" = "$current_monitor" ]; then
    current_index=$i
    break
  fi
done

# Calculate the next monitor index
next_index=$(( (current_index + 1) % ${#monitors[@]} ))

# Move the active workspace to the next monitor
hyprctl dispatch moveworkspacetomonitor "$active_workspace" "${monitors[$next_index]}"
