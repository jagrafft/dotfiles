# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

## Dracula Theme Colors ##
export PS1="\[\033[36m\]\u\[\033[m\]@\[\033[32m\]\h:\[\033[33;1m\]\w\[\033[m\]\$ "
export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad

## Local adjustments ##
# PATH="$HOME/bin:$PATH"
# PATH="$HOME/.local/bin:$PATH"
export TERMINAL=/usr/local/bin/alacritty
export GRIM_OUTPUT_DIR=$HOME/documents/screenshots

alias ls='ls -GFh'
