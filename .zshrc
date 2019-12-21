[[ -f "$HOME/.config/system/alias.sh" ]] && source "$HOME/.config/system/alias.sh"

[[ -f "$HOME/.config/system/export.sh" ]] && source "$HOME/.config/system/export.sh"

[[ -f "$HOME/.config/system/function.sh" ]] && source "$HOME/.config/system/function.sh"

[[ -f "$HOME/.local.zshrc" ]] && source "$HOME/.local.zshrc"

[[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh

([[ -f "$HOME/.fzf.zsh" ]] || (git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && ~/.fzf/install)) && source "$HOME/.fzf.zsh"

([[ -f "$HOME/.cache/z.lua" ]] || (cd ~/.cache && curl -fsSLO https://raw.githubusercontent.com/skywind3000/z.lua/master/z.lua)) && eval "$(lua ~/.cache/z.lua --init zsh once enhanced)"

([[ -f "$HOME/.zgen/zgen.zsh" ]] || git clone https://github.com/tarjoilija/zgen.git "${HOME}/.zgen") && source "$HOME/.zgen/zgen.zsh" 


if ! zgen saved; then
    zgen load "zsh-users/zsh-autosuggestions"
    zgen load "zdharma/fast-syntax-highlighting"
    zgen load "zsh-users/zsh-completions" src
    zgen load "chisui/zsh-nix-shell"
    zgen load "spwhitt/nix-zsh-completions"
    zgen load "zsh-users/zaw"
    zgen load "romkatv/powerlevel10k" powerlevel10k
    zgen save 
fi

prompt_nix_shell_setup

kitty + complete setup zsh | source /dev/stdin

# added by travis gem
[ -f /home/ronan/.travis/travis.sh ] && source /home/ronan/.travis/travis.sh
