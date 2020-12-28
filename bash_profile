# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs

PATH=$PATH:$HOME/bin

# Dracula Theme Colors
export PS1="\[\033[36m\]\u\[\033[m\]@\[\033[32m\]\h:\[\033[33;1m\]\w\[\033[m\]\$ "
export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad
alias ls='ls -GFh'
source "$HOME/.cargo/env"
if [ -e /home/jg/.nix-profile/etc/profile.d/nix.sh ]; then . /home/jg/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
