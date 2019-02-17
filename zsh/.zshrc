source ~/dotfiles/system/.alias
source ~/dotfiles/system/.export
source ~/dotfiles/system/.function
source ~/dotfiles/zsh/antigen.zsh
antigen use oh-my-zsh

  antigen theme denysdovhan/spaceship-prompt

  antigen bundle zsh-users/zsh-autosuggestions
  antigen bundle zsh-users/zsh-completions
  antigen bundle zsh-users/zsh-syntax-highlighting
  antigen bundle wting/autojump 
  antigen bundle git
  antigen bundle supercrabtree/k

antigen apply

ZSH_THEME="spaceship"

HYPHEN_INSENSITIVE="true"

ENABLE_CORRECTION="true"

COMPLETION_WAITING_DOTS="true"

DISABLE_UNTRACKED_FILES_DIRTY="true"

HIST_STAMPS="dd/mm/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Source after all oh my zsh config
source $ZSH/oh-my-zsh.sh

# User configuration

export MANPATH="/usr/local/man:$MANPATH"

export EDITOR='vim'

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[[ -s /home/ronan/.autojump/etc/profile.d/autojump.sh ]] && source /home/ronan/.autojump/etc/profile.d/autojump.sh

	autoload -U compinit && compinit -u
