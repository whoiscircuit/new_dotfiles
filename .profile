#!/usr/bin/env sh
source "$HOME/.config/env"
if command -v dconf &>/dev/null; then
  cat "$HOME/.config/dconf/user.d"* | dconf load /
fi
