if [[ ! -d ~/.zplug ]]; then
  git clone https://github.com/zplug/zplug ~/.zplug
  source ~/.zplug/init.zsh && zplug update --self
fi
source ~/dotfiles/system/.alias
source ~/dotfiles/system/.export
source ~/dotfiles/system/.function
source ~/.zplug/init.zsh

#ZSH_THEME="spaceship"

#HYPHEN_INSENSITIVE="true"

#ENABLE_CORRECTION="true"

#COMPLETION_WAITING_DOTS="true"

#DISABLE_UNTRACKED_FILES_DIRTY="true"

#HIST_STAMPS="dd/mm/yyyy"

## Would you like to use another custom folder than $ZSH/custom?
## ZSH_CUSTOM=/path/to/new-custom-folder

## Source after all oh my zsh config
#source $ZSH/oh-my-zsh.sh
zplug "zsh-users/zsh-completions", depth:1 #more completions
zplug "zsh-users/zsh-autosuggestions", from:github #proposes transparent suggestions based on command history
zplug "zsh-users/zsh-syntax-highlighting", from:github, defer:1
zplug "plugins/autojump", from:oh-my-zsh
zplug "supercrabtree/k", from:github
zplug "plugins/git",   from:oh-my-zsh
zplug "denysdovhan/spaceship-prompt", use:spaceship.zsh, from:github, as:theme
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    else
        echo
    fi
fi
zplug load 
# User configuration

export MANPATH="/usr/local/man:$MANPATH"

export EDITOR='vim'

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[[ -s /home/ronan/.autojump/etc/profile.d/autojump.sh ]] && source /home/ronan/.autojump/etc/profile.d/autojump.sh

	autoload -U compinit && compinit -u
