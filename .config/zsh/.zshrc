zmodload zsh/zpty

[[ -f "$ZDOTDIR/base.sh" ]] && source "$ZDOTDIR/base.sh"

[[ -f "$ZDOTDIR/.p10k.zsh" ]] && source "$ZDOTDIR/.p10k.zsh"

([[ -f "$ZIM_HOME/init.zsh" ]] || (mkdir -p $ZIM_HOME && curl  -L https://github.com/zimfw/zimfw/releases/latest/download/zimfw.zsh > $ZIM_HOME/zimfw.zsh && source "$ZIM_HOME/zimfw.zsh" install))

if [[ $ZIM_HOME/init.zsh -ot $ZDOTDIR/.zimrc ]]; then
  # Update static initialization script if it's outdated, before sourcing it
  source $ZIM_HOME/zimfw.zsh init -q
fi
source $ZIM_HOME/init.zsh

([[ -f "$XDG_CONFIG_HOME/fzf/fzf.zsh" ]] || (git clone --depth 1 https://github.com/junegunn/fzf.git "$XDG_CONFIG_HOME/fzf" && "$XDG_CONFIG_HOME/.fzf/install" --xdg)) && source "$XDG_CONFIG_HOME/fzf/fzf.zsh"

# added by travis gem
[ -f /home/ronan/.travis/travis.sh ] && source /home/ronan/.travis/travis.sh
