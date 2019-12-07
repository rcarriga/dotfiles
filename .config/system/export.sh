export TERM=xterm-kitty
export MANPATH="/usr/local/man:$MANPATH"
export EDITOR="nvim"
export FZF_DEFAULT_COMMAND="rg --files --max-depth=10 -g \"!.git\" --hidden --color never --follow"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_DEFAULT_OPTS="--height 96% --reverse --preview \"[[ -f \"{}\" ]] && head -\$LINES {}\""
export FZF_COMPLETION_TRIGGER="#"
export SCRIPT_DIR=$HOME/.config/scripts
export HISTFILE="$HOME/.zhistory"
export HISTSIZE="4000"
export MANPAGER="nvim -c \"set ft=man nomod nonumber\" -"
export PMY_FUZZY_FINDER_DEFAULT_CMD="fzf -0 -1 --height 20 --reverse --border --inline-info --ansi --color=dark"
export GOPATH="$HOME/.go"
export PATH="$HOME/.local/bin:$HOME/bin:/usr/local/bin:/home/ronan/.pyenv/bin:$GOPATH/bin:$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH:$M2_HOME/bin"
[[ -f ~/.config/system/local.export.sh ]] && source ~/.config/system/local.export.sh
