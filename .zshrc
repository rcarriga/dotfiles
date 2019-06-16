zmodload zsh/zprof

[[ -f "$HOME/.config/system/alias.sh" ]] && source "$HOME/.config/system/alias.sh"

[[ -f "$HOME/.config/system/export.sh" ]] && source "$HOME/.config/system/export.sh"

[[ -f "$HOME/.config/system/function.sh" ]] && source "$HOME/.config/system/function.sh"

[[ -f "$HOME/.local.zshrc" ]] && source "$HOME/.local.zshrc"

[[ -d "$HOME/.zgen" ]] || git clone https://github.com/tarjoilija/zgen.git $HOME/.zgen

source "$HOME/.zgen/zgen.zsh"

if ! zgen saved; then

    zgen load "zsh-users/zsh-autosuggestions"
    zgen load "zsh-users/zsh-syntax-highlighting"
    zgen load "zsh-users/zsh-completions" src
    zgen load "romkatv/powerlevel10k" powerlevel10k
    zgen save 

    source "$HOME/.zgen/BrandonRoehl/zsh-clean-master/clean.plugin.zsh"
fi

([[ -f "$HOME/.purepower" ]] || (cd && curl -fsSLO https://raw.githubusercontent.com/romkatv/dotfiles-public/master/.purepower)) && source "$HOME/.purepower"

[ -f "$HOME/.fzf.zsh" ] && source "$HOME/.fzf.zsh"

[[ -s "$HOME/.autojump/etc/profile.d/autojump.sh" ]] && source "$HOME/.autojump/etc/profile.d/autojump.sh"

[ -f "$HOME/.tnsrc" ] && source "$HOME/.tnsrc" 

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[ -f /etc/profile.d/nix.sh ] && source /etc/profile.d/nix.sh
