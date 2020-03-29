zmodload zsh/zpty

[[ -f "$HOME/.config/zsh/base.sh" ]] && source "$HOME/.config/zsh/base.sh"

[[ -d "$HOME/.local/bin" ]] || mkdir -p $HOME/.local/bin

[[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh

([[ -f "$ZIM_HOME/init.zsh" ]] || (mkdir -p $ZIM_HOME && curl  -L https://github.com/zimfw/zimfw/releases/latest/download/zimfw.zsh > $ZIM_HOME/zimfw.zsh && source "$ZIM_HOME/zimfw.zsh" install))

if [[ $ZIM_HOME/init.zsh -ot $ZDOTDIR/.zimrc ]]; then
  # Update static initialization script if it's outdated, before sourcing it
  source $ZIM_HOME/zimfw.zsh init -q
fi
source $ZIM_HOME/init.zsh

([[ -f "$HOME/.fzf.zsh" ]] || (git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && ~/.fzf/install)) && source "$HOME/.fzf.zsh"

([[ -f "$HOME/.cache/z.lua" ]] || (cd ~/.cache && curl -fsSLO https://raw.githubusercontent.com/skywind3000/z.lua/master/z.lua)) && eval "$(lua ~/.cache/z.lua --init zsh once enhanced fzf)"

# added by travis gem
[ -f /home/ronan/.travis/travis.sh ] && source /home/ronan/.travis/travis.sh
