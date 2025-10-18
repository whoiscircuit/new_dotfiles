#!/usr/bin/env sh
source "$HOME/.config/env"

#if [[ -n "$WAYLAND_DISPLAY" ]]; then
    systemctl --user enable --now serve-firefox-user-scripts.service
#fi
