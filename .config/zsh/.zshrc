zmodload zsh/zpty

[[ -f "$HOME/.config/zsh/base.sh" ]] && source "$HOME/.config/zsh/base.sh"

[[ -d "$HOME/.local/bin" ]] || mkdir -p $HOME/.local/bin

[[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh

([[ -f "$HOME/.zgen/zgen.zsh" ]] || git clone https://github.com/tarjoilija/zgen.git "${HOME}/.zgen") && source "$HOME/.zgen/zgen.zsh" 


if ! zgen saved; then
    zgen load "zsh-users/zsh-autosuggestions"
    zgen load "zdharma/fast-syntax-highlighting"
    zgen load "zsh-users/zsh-completions" src
    zgen load "romkatv/powerlevel10k" powerlevel10k
    zgen save 
fi

([[ -f "$HOME/.fzf.zsh" ]] || (git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && ~/.fzf/install)) && source "$HOME/.fzf.zsh"

([[ -f "$HOME/.cache/z.lua" ]] || (cd ~/.cache && curl -fsSLO https://raw.githubusercontent.com/skywind3000/z.lua/master/z.lua)) && eval "$(lua ~/.cache/z.lua --init zsh once enhanced fzf)"


prompt_nix_shell_setup

kitty + complete setup zsh | source /dev/stdin

# added by travis gem
[ -f /home/ronan/.travis/travis.sh ] && source /home/ronan/.travis/travis.sh

setopt -o shareHistory
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
ZSH_AUTOSUGGEST_USE_ASYNC=1

# Autosuggestion Mappings
bindkey '^k' autosuggest-accept
bindkey '^h' autosuggest-fetch
bindkey '^j' autosuggest-execute
