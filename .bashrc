#!/usr/bin/env bash

if [[ $- != *i* ]] ; then
  printf "Shell is non-interactive so not sourcing $HOME/bashrc.d\n"
  return
fi

# On RedHat distros, if you don't source this file you don't
# set options like checkwinsize which causes bad terminal wrapping.
if [ -f /etc/bashrc ]; then
  . /etc/bashrc
fi

if [ -d $HOME/bashrc.d ]; then
  for dotfile in `find $HOME/bashrc.d/`; do
    [ -f $dotfile ] && source $dotfile
  done
fi

eval `keychain --quiet --eval github_rsa openwrt_git_rsa`

# Enable direnv.
eval "$(direnv hook bash)"

# tmux.
if command -v tmux &>/dev/null; then
    [[ -z "${TMUX}" ]] && (tmux attach || tmux new-session)
fi
