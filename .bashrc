#!/bin/bash
# ~/.bashrc

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
  for dotfile in `find $HOME/bashrc.d/`
  do
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

# ctrl + w delete words instead of unix-word-rubout 
stty werase undef
bind '\C-w:backward-kill-word'

# bash-my-aws
export PATH="$PATH:$HOME/.bash-my-aws/bin"
source ~/.bash-my-aws/aliases

# For ZSH users, uncomment the following two lines:
# autoload -U +X compinit && compinit
# autoload -U +X bashcompinit && bashcompinit

source ~/.bash-my-aws/bash_completion.sh
