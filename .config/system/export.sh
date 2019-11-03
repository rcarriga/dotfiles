export PATH=$HOME/bin:/usr/local/bin:$PATH
export TERM=xterm-kitty
export MANPATH="/usr/local/man:$MANPATH"
export EDITOR="nvim"
export PATH=$PATH:$M2_HOME/bin
export FZF_DEFAULT_COMMAND="rg --max-depth 10 --hidden --ignore .git -L"
export PATH=$HOME/.local/bin:$PATH
export SCRIPT_DIR=$HOME/.config/scripts
export HISTFILE="$HOME/.zhistory"
export SAVEHIST=1000
export HISTSIZE="1000"
export MANPAGER="nvim -c \"set ft=man nomod nonumber\" -"
export PMY_FUZZY_FINDER_DEFAULT_CMD="fzf -0 -1 --height 20 --reverse --border --inline-info --ansi --color=dark"
export GOPATH="$HOME/.go"
export PATH="$GOPATH/bin:$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
[[ -f ~/.config/system/local.export.sh ]] && source ~/.config/system/local.export.sh
