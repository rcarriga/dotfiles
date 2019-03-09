if [[ ! -d ~/.zplug ]]; then
  git clone https://github.com/zplug/zplug ~/.zplug
  source ~/.zplug/init.zsh && zplug update --self
fi

source ~/.zplug/init.zsh

zplug "zsh-users/zsh-completions", depth:1
zplug "zsh-users/zsh-autosuggestions", from:github
zplug "zsh-users/zsh-syntax-highlighting", from:github, defer:1
zplug "plugins/autojump", from:oh-my-zsh
zplug "supercrabtree/k", from:github
#zplug "plugins/git",   from:oh-my-zsh
zplug "sindresorhus/pure", use:pure.zsh, from:github, as:theme
#zplug "chrissicool/zsh-256color", from:github
zplug "mafredri/zsh-async", from:"github", use:"async.zsh"

zplug load 
