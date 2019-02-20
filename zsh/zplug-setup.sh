#if [[ ! -d ~/.zplug ]]; then
#source ~/dotfiles/zsh/zplug-setup.sh
  #git clone https://github.com/zplug/zplug ~/.zplug
  #source ~/.zplug/init.zsh && zplug update --self
#fi

source ~/.zplug/init.zsh

zplug "zsh-users/zsh-completions", depth:1 #more completions
zplug "zsh-users/zsh-autosuggestions", from:github #proposes transparent suggestions based on command history
zplug "zsh-users/zsh-syntax-highlighting", from:github, defer:1
zplug "plugins/autojump", from:oh-my-zsh
zplug "supercrabtree/k", from:github
zplug "plugins/git",   from:oh-my-zsh
zplug "denysdovhan/spaceship-prompt", use:spaceship.zsh, from:github, as:theme
zplug "chrissicool/zsh-256color", from:github

if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    else
        echo
    fi
fi
zplug load 
