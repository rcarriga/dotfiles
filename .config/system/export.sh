export PATH=$HOME/bin:/usr/local/bin:$PATH
export ZSH=~/".oh-my-zsh"
export TERM=xterm-kitty
export MANPATH="/usr/local/man:$MANPATH"
export EDITOR='nvim'
export PATH=$PATH:$M2_HOME/bin
export FZF_DEFAULT_COMMAND='ag --depth 10 --hidden --ignore .git -f -g ""'
export PATH=$HOME/.local/bin:$PATH
export SCRIPT_DIR=$HOME/.config/scripts
[[ -f ~/.config/system/local.export.sh ]] && source ~/.config/system/local.export.sh
