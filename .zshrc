# Add custom aliases here
source ~/.config/system/alias.sh

# Add environment var exports here
source ~/.config/system/export.sh

# Add custom functions here
source ~/.config/system/function.sh

if [[ ! -d ~/.zplug ]]; then
  git clone https://github.com/zplug/zplug ~/.zplug
  source ~/.zplug/init.zsh && zplug update 
fi

source ~/.zplug/init.zsh

zplug "zsh-users/zsh-completions", depth:1, as:plugin, lazy:true
zplug "zsh-users/zsh-autosuggestions", from:github, as:plugin
zplug "zsh-users/zsh-syntax-highlighting", from:github, defer:1, as:plugin
zplug "plugins/autojump", from:oh-my-zsh, as:command, lazy:true
zplug "supercrabtree/k", from:github, as:plugin
zplug "sindresorhus/pure", use:pure.zsh, from:github, as:theme
zplug "mafredri/zsh-async", from:"github", use:"async.zsh", as:plugin

zplug load 

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[[ -s /home/ronan/.autojump/etc/profile.d/autojump.sh ]] && source /home/ronan/.autojump/etc/profile.d/autojump.sh

###-tns-completion-start-###
if [ -f /Users/rcarriga/.tnsrc ]; then 
    source /Users/rcarriga/.tnsrc 
fi
###-tns-completion-end-###
