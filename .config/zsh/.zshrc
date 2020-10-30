zmodload zsh/zpty

[[ -f "$ZDOTDIR/base.sh" ]] && source "$ZDOTDIR/base.sh"

[[ -f "$ZDOTDIR/.p10k.zsh" ]] && source "$ZDOTDIR/.p10k.zsh"

([[ -f "$ZIM_HOME/init.zsh" ]] || (mkdir -p $ZIM_HOME && curl  -L https://github.com/zimfw/zimfw/releases/latest/download/zimfw.zsh > $ZIM_HOME/zimfw.zsh && source "$ZIM_HOME/zimfw.zsh" install))

([[ -f "$XDG_CONFIG_HOME/fzf/fzf.zsh" ]] || (git clone https://github.com/junegunn/fzf.git "$XDG_CONFIG_HOME/fzf" && "$XDG_CONFIG_HOME/fzf/install" --xdg)) && source "$XDG_CONFIG_HOME/fzf/fzf.zsh"

autoload -U compinit && compinit

if [[ $ZIM_HOME/init.zsh -ot $ZDOTDIR/.zimrc ]]; then
  # Update static initialization script if it's outdated, before sourcing it
  source $ZIM_HOME/zimfw.zsh init -q
fi
source $ZIM_HOME/init.zsh

source `which aws_zsh_completer.sh`

[[ -f "$ZDOTDIR/widgets.sh" ]] && source "$ZDOTDIR/widgets.sh"
