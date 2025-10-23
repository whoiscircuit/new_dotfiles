#!/usr/bin/env sh
source "$HOME/.config/env"

! systemctl --use is-enabled serve-firefox-user-scripts.service && systemctl --user enable --now serve-firefox-user-scripts.service
! systemctl --use is-enabled hidrosis.service && systemctl --user enable --now hidrosis.service

