[[ -f "$HOME/.config/system/alias.sh" ]] && source "$HOME/.config/system/alias.sh"

[[ -f "$HOME/.config/system/export.sh" ]] && source "$HOME/.config/system/export.sh"

[[ -f "$HOME/.config/system/function.sh" ]] && source "$HOME/.config/system/function.sh"

[[ -f "$HOME/.local.zshrc" ]] && source "$HOME/.local.zshrc"

([[ -d "$HOME/powerlevel10k" ]] || git clone https://github.com/romkatv/powerlevel10k.git ~/powerlevel10k) && source ~/powerlevel10k/powerlevel10k.zsh-theme

[[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh

([[ -f "$HOME/.fzf.zsh" ]] || (git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && ~/.fzf/install)) && source "$HOME/.fzf.zsh"

([[ -f "$HOME/z.lua" ]] || (cd && curl -fsSLO https://raw.githubusercontent.com/skywind3000/z.lua/master/z.lua)) && eval "$(lua ~/z.lua --init zsh once enhanced)"

([[ -f "$HOME/.zgen/zgen.zsh" ]] || git clone https://github.com/tarjoilija/zgen.git "${HOME}/.zgen") && source "$HOME/.zgen/zgen.zsh" 

if ! zgen saved; then

    zgen load "zsh-users/zsh-autosuggestions"
    zgen load "zdharma/fast-syntax-highlighting"
    zgen load "zsh-users/zsh-completions" src
    zgen save 
fi
