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

# SSH agent configuration
if [ -f "${SSH_ENV}" ]; then
    . "${SSH_ENV}" > /dev/null
    ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
        ssh_agent;
    }
else
    ssh_agent;
fi

# Enable direnv.
eval "$(direnv hook bash)"

# tmux.
if command -v tmux &>/dev/null; then
    [[ -z "${TMUX}" ]] && (tmux attach || tmux new-session)
fi

ssh-add ~/.ssh/github_rsa
