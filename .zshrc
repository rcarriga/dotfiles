[[ -f ~/.config/system/alias.sh ]] && source ~/.config/system/alias.sh

[[ -f ~/.config/system/export.sh ]] && source ~/.config/system/export.sh

[[ -f ~/.config/system/function.sh ]] && source ~/.config/system/function.sh

[[ -d $ZGEN ]] || git clone https://github.com/tarjoilija/zgen.git $ZGEN

source "$ZGEN/zgen.zsh"

if ! zgen saved; then

    zgen load "zsh-users/zsh-autosuggestions"
    zgen load "zsh-users/zsh-syntax-highlighting"
    zgen load "zsh-users/zsh-completions" src
    zgen load "BrandonRoehl/zsh-clean"

    zgen save 
fi

source "$ZGEN/BrandonRoehl/zsh-clean-master/clean.plugin.zsh"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[[ -s ~/.autojump/etc/profile.d/autojump.sh ]] && source ~/.autojump/etc/profile.d/autojump.sh

[ -f ~/.tnsrc ] && source ~/.tnsrc 
