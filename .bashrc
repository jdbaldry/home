#!/usr/bin/env bash

if [[ $- != *i* ]]; then
  printf "Shell is non-interactive so not sourcing %s/bashrc.d\n" "${HOME}"
  return
fi

# On RedHat distros, if you don't source this file you don't
# set options like checkwinsize which causes bad terminal wrapping.
if [ -f /etc/bashrc ]; then
  . /etc/bashrc
fi

source "${HOME}"/bashrc.d/aliases
source "${HOME}"/bashrc.d/completions
source "${HOME}"/bashrc.d/functions
source "${HOME}"/bashrc.d/fzf
source "${HOME}"/bashrc.d/prompt
source "${HOME}"/bashrc.d/variables

eval "$(keychain --quiet --eval github_rsa openwrt_git_rsa)"

# Enable direnv.
eval "$(direnv hook bash)"
