# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:/usr/local/bin:$PATH

export ZSH=~/".oh-my-zsh"

ZSH_THEME="refined"

HYPHEN_INSENSITIVE="true"

ENABLE_CORRECTION="true"

COMPLETION_WAITING_DOTS="true"

DISABLE_UNTRACKED_FILES_DIRTY="true"

HIST_STAMPS="dd/mm/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

plugins=(
  autojump
  git
  zsh-autosuggestions
  zsh-syntax-highlighting
)


source $ZSH/oh-my-zsh.sh
source ~/dotfiles/system/.alias
source ~/dotfiles/system/.export
source ~/dotfiles/system/.function

# User configuration

export MANPATH="/usr/local/man:$MANPATH"

export EDITOR='vim'
