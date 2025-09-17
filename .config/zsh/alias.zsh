#!/usr/bin/env zsh

# replace a command with another if possible
function replace(){
  local command=$1
  shift;
  command -v "$1" &> /dev/null && alias $command="$@"
}

function has(){
  command -v $1 &>/dev/null && return 0 || return 1
}


unalias run-help &>/dev/null # because run-help=man is a dumb default alias!

# aliases for ohmyzsh clipboard library
alias copy=clipcopy
alias paste=clippaste


# use lsd instead of ls if possible
replace ls lsd -v

# commonly used aliases for ls
alias la="ls -lAhv" # list everything except . & .. with human readable sizes
alias ll="ls -lhv" # list with human readable sizes
alias l="la"
alias l.="ls -ldv .*" # only list dotfiles
alias lt="ls -lth" # sort by time (most recent)
alias lS="ls -lSh" # sort by size (largest first)

# make grep use colors and ignore some special folders by default
GREP_OPTIONS="--color=auto --exclude-dir={.git,CVS,.hg,.bzr,.svn,.idea,.tox}"
alias grep="grep $GREP_OPTIONS"
alias egrep="egrep $GREP_OPTIONS"
alias fgrep="fgrep $GREP_OPTIONS"
unset GREP_OPTIONS

# don't add unnecessary commands to history. for this to work HIST_NO_SPACE
# must be set.
alias exit=" exit"

# human readable file sizes for du and df
alias df="df -h"
alias du="du -h"

# pretty print json from stdin
alias json="jq . --color-output"

# python aliases
has python3 && alias python="python3"
has pip3 && alias pip="pip3"

if has python3; then
  alias venv="python3 -m venv"
elif has python; then
  alias venv="python -m venv"
fi

# breaking the habbit of using clear instead of Ctrl-l
alias clear=" echo 'use Ctrl-l'"

# use bat instead of cat if possible
replace cat bat
# use bat to show pretified --help output for every command
has bat && alias -g -- --help='--help 2>&1 | bat --language=help --style=plain'

# base64
alias encode64="base64"
alias decode64="base64 -d"

# nixos aliases
alias hms="home-manager switch --flake ~/.nixconf#user"
alias nrs="sudo nixos-rebuild switch --flake ~/.nixconf#vm"
function whichnix(){ readlink -f "$(command which $1)"; }
alias which=whichnix

if has gtrash; then
  alias rm="gtrash put --rm-mode"
  alias restore="gtrash restore"
elif has rmtrash; then
  alias rm="rmtrash"
  alias rmdir="rmdirtrash"
fi

# qrcode generator
alias qrencode="qrencode -t ANSI256"
alias qr="qrencode"
alias qrcode="qrencode"

alias tolower="tr '[:upper:]' '[:lower:]'"
alias toupper="tr '[:lower:]' '[:upper:]'"
