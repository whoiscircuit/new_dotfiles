#!/usr/bin/env zsh

# enable powerlevel10k instant prompt. should stay at the top of the rc file
# allow new shells to have zero delay displaying the prompt
[[ -r "${XDG_CACHE_HOME}/p10k-instant-prompt-${(%):-%n}.zsh" ]] && source "${XDG_CACHE_HOME}/p10k-instant-prompt-${(%):-%n}.zsh"

# bootstrap zert plugin manager
export ZERT_LOCKFILE="$ZDOTDIR/zert.lock"
[ ! -f "${ZERT_PLUGINS_DIR:-${ZERT_DIR:-${XDG_DATA_HOME:-$HOME/.local/share}/zert}/plugins}/zert/zert.plugin.zsh" ]  && {
   curl "https://raw.githubusercontent.com/whoiscircuit/zert/refs/heads/main/zert-bootstrap.zsh" | zsh
}

source "${ZERT_PLUGINS_DIR:-${ZERT_DIR:-${XDG_DATA_HOME:-$HOME/.local/share}/zert}/plugins}/zert/zert.plugin.zsh"

zert add zdharma-continuum/fast-syntax-highlighting

zert add romkatv/powerlevel10k
zert use ohmyzsh lib/clipboard
[ -f "$ZDOTDIR/p10k.zsh" ] && source "$ZDOTDIR/p10k.zsh"
[ -f "$ZDOTDIR/p10k_custom.zsh" ] && source "$ZDOTDIR/p10k_custom.zsh"
