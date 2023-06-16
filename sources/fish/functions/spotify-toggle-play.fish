function spotify-toggle-play
    dbus-send --session --dest=org.mpris.MediaPlayer2.spotify --type=method_call --print-reply /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause
end
