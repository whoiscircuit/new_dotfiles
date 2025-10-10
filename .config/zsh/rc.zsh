#!usr/bin/env zsh

# these envrinment variables should be set here because when they are set in ~/.config/env (like other environment variables)
# they get overwritten by zsh at launch
export HISTFILE="${XDG_CACHE_HOME}/zsh_history"
export ZSH_COMPDUMP="${XDG_CACHE_HOME}/zcompdump"
export SAVEHIST=20000

# enable powerlevel10k instant prompt. should stay at the top of the rc file
# allow new shells to have zero delay displaying the prompt
[[ -r "${XDG_CACHE_HOME}/p10k-instant-prompt-${(%):-%n}.zsh" ]] && source "${XDG_CACHE_HOME}/p10k-instant-prompt-${(%):-%n}.zsh"


####################
# PLUGINS
####################
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

# direnv integration with a wrapper for _direnv_hook to make direnv work without nagging
# about .env files not being trusted. i do this because i have customized powerlevel10k
# to add information in prompt about the status of direnv so i don't need explicit error
# messages
if (( ${+commands[direnv]} )); then
    zert use ohmyzsh plugins/direnv
    function _direnv_hook(){
      trap -- '' SIGINT
      eval "$(direnv export zsh 2> >(grep -v 'is blocked' >&2) )"
      trap - SIGINT;
    }
fi

# show command autosuggestion (after the cursor in gray color) based on history, zsh completions, etc..
zert add zsh-users/zsh-autosuggestions
export ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=200
export ZSH_AUTOSUGGEST_COMPLETION_IGNORE="npm *"
export ZSH_AUTOSUGGEST_STRATEGY=(history completion)

zert add zsh-users/zsh-history-substring-search
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND=
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND=
HISTORY_SUBSTRING_SEARCH_PREFIXED=1

source "$ZDOTDIR/scripts/asciinema.zsh" # my custom functions and aliases for asciinema

# these plugins should be loaded before compinit
for completion_plugin in docker-compose gitfast git-extras flutter golang gh pylint; do
    zert use ohmyzsh plugins/$completion_plugin --no-alises
done

fpath+=("$ZDOTDIR/completions")
autoload -Uz compinit && compinit -C
autoload -Uz bashcompinit && bashcompinit

# these plugins should be loaded after compinit
for completion_plugin in dotnet lxd; do
    zert use ohmyzsh plugins/$completion_plugin --no-aliases
done
command -v fzf-share &> /dev/null && source "$(fzf-share)/completion.zsh"

####################
# OPTIONS
####################
# don't use any global rc files
setopt no_global_rcs

# enable bash-like extended globing
setopt ksh_glob

# ignore contigous duplicate commands in history
setopt hist_ignore_all_dups

# don't add commands that start with a space to history
setopt hist_ignore_space

# don't store the "history" command itself to history
setopt hist_no_store

# add entered commands to history immediatly not after the shell is closed
setopt inc_append_history

# run background jobs at a lower priority
setopt bg_nice

# ctrl-d will not exit the shell
setopt ignore_eof

# no beeping
setopt no_beep


####################
# KEYBINDINGS
####################
# use emacs keybindings
bindkey -e
WORDCHARS=
bindkey "^H" backward-kill-word # Ctrl+Backspace
bindkey "^Oc" forward-word # Ctrl+<Right>
bindkey "^[[1;5C" forward-word # Ctrl+<Right> for vterm
bindkey "^Od" backward-word # Ctrl+<Left>
bindkey "^[[1;5D" backward-word # Ctrl+<Left> for vterm


####################
# ALIASES
####################
source "$ZDOTDIR/alias.zsh"

[ -f "$ZDOTDIR/p10k.zsh" ] && source "$ZDOTDIR/p10k.zsh"
