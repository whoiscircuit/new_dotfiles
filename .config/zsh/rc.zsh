#!/usr/bin/env zsh

# these envrinment variables should be set here because when they are set in ~/.config/env (like other environment variables)
# they get overwritten by zsh at launch
export HISTFILE="${XDG_CACHE_HOME}/zsh_history"
export ZSH_COMPDUMP="${XDG_CACHE_HOME}/zcompdump"
export SAVEHIST=20000

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

zert add romkatv/powerlevel10k # the best zsh prompt
[ -f "$ZDOTDIR/p10k_custom.zsh" ] && source "$ZDOTDIR/p10k_custom.zsh"

# clipcopy - Copy data to clipboard
#   Usage:
#     <command> | clipcopy     - copies stdin to clipboard
#     clipcopy <file>          - copies a file's contents to clipboard
# clippaste - writes clipboard's contents to stdout
#   Usage:
#     clippaste | <command>   - paste contents and pipes it to another process
#     clippaste > <file>      - paste contents to a file
zert use ohmyzsh lib/clipboard

# automatically send system notifications for commands that take a long time
export bgnotify_threshold=60
zert use ohmyzsh plugins/bgnotify

# colorize man pages with less as pager
autoload -Uz colors && colors # required
zert use ohmyzsh plugins/colored-man-pages

# archive and compress files and directories with different formats
# usage: archive <format> [files]
# example: archive zip file1 file2 directory/ *.txt
zert use ohmyzsh plugins/universalarchive
alias archive="ua"

# extract compressed files with different formats
# usage: extract <file> OR unarchive <file>
zert use ohmyzsh plugins/extract
alias unarchive="extract"

#(( ${+commands[tldr]} )) && tldr --print-completion

fpath+=("$ZDOTDIR/completions")
autoload -Uz compinit && compinit -C
autoload -Uz bashcompinit && bashcompinit

[ -f "$ZDOTDIR/p10k.zsh" ] && source "$ZDOTDIR/p10k.zsh"
