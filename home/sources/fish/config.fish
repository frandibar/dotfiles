# https://direnv.net/docs/hook.html
# For direnv to work properly it needs to be hooked into the shell.
direnv hook fish | source

# Abbreviations
abbr --add ec emacsclient --no-wait
abbr --add sbcl rlwrap sbcl
abbr --add python rlwrap python