export PATH=$HOME/bin:/usr/local/bin:$PATH
export ZSH=~/".oh-my-zsh"
export TERM=xterm-256color
export MANPATH="/usr/local/man:$MANPATH"
export EDITOR='vim'
export PATH=$PATH:$M2_HOME/bin
export FZF_DEFAULT_COMMAND='ag --depth 10 --hidden --ignore .git -f -g ""'
export PATH=$HOME/.local/bin:$PATH
[[ -f ~/dotfiles/system/local.export.sh ]] && source ~/dotfiles/system/local.export.sh
