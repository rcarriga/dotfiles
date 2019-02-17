# Add custom aliases here
source ~/dotfiles/system/alias.sh

# Add environment var exports here
source ~/dotfiles/system/export.sh

# Add custom functions here
source ~/dotfiles/system/function.sh

# Oh-my-zsh settings go in this one
source ~/dotfiles/zsh/ohmyzsh-setup.sh

# ZPlug settings go in... you guessed it!
source ~/dotfiles/zsh/zplug-setup.sh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[[ -s /home/ronan/.autojump/etc/profile.d/autojump.sh ]] && source /home/ronan/.autojump/etc/profile.d/autojump.sh

autoload -U compinit && compinit -u
