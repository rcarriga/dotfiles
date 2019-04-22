neofetch
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

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[[ -s /home/ronan/.autojump/etc/profile.d/autojump.sh ]] && source /home/ronan/.autojump/etc/profile.d/autojump.sh


#export NVM_DIR="$HOME/.nvm"
#[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
#[ -s "/usr/local/opt/nvm/etc/bash_completion" ] && . "/usr/local/opt/nvm/etc/bash_completion"  # This loads nvm bash_completion

#autoload -U add-zsh-hook
#load-nvmrc() {
  #local node_version="$(nvm version)"
  #local nvmrc_path="$(nvm_find_nvmrc)"

  #if [ -n "$nvmrc_path" ]; then
    #local nvmrc_node_version=$(nvm version "$(cat "${nvmrc_path}")")

    #if [ "$nvmrc_node_version" = "N/A" ]; then
      #nvm install
    #elif [ "$nvmrc_node_version" != "$node_version" ]; then
      #nvm use
    #fi
  #elif [ "$node_version" != "$(nvm version default)" ]; then
    #echo "Reverting to nvm default version"
    #nvm use default
  #fi
#}
#add-zsh-hook chpwd load-nvmrc
#load-nvmrc

###-tns-completion-start-###
if [ -f /Users/rcarriga/.tnsrc ]; then 
    source /Users/rcarriga/.tnsrc 
fi
###-tns-completion-end-###
