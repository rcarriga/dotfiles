export QT_STYLE_OVERRIDE="gtk2"
export TERM=xterm-kitty
export MANPATH="/usr/local/man:$MANPATH"
export EDITOR="nvim"
export FZF_DEFAULT_COMMAND="rg --files --max-depth=10 -g \"!.git\" --hidden --color never --follow"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_DEFAULT_OPTS="--height 96% --reverse --preview \"source ~/.config/system/function.sh && fuzzy_preview {}\""
export FZF_COMPLETION_TRIGGER="#"
export SCRIPT_DIR=$HOME/.config/scripts
export MANPAGER="nvim -c \"set ft=man nomod nonumber\" -"
export PMY_FUZZY_FINDER_DEFAULT_CMD="fzf -0 -1 --height 20 --reverse --border --inline-info --ansi --color=dark"
export GOPATH="$HOME/.go"
export PATH="$HOME/.local/bin:$HOME/bin:/usr/local/bin:/home/ronan/.pyenv/bin:$GOPATH/bin:$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH:$M2_HOME/bin"
export ZIM_HOME=${ZDOTDIR:-${HOME}}/.zim

# Handling config and cache files

export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
export XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
export XDG_DATA_HOME="${XDG_CACHE_HOME:-$HOME/.local/share}"
if [ ! -w ${XDG_RUNTIME_DIR:="/run/user/$UID"} ]; then
    echo "\$XDG_RUNTIME_DIR ($XDG_RUNTIME_DIR) not writable. Setting to /tmp." >&2
    XDG_RUNTIME_DIR=/tmp
fi
export XDG_RUNTIME_DIR

export _ZL_DATA="$XDG_DATA_HOME/zlua"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/config"
export NPM_CONFIG_CACHE=$XDG_CACHE_HOME/npm
export NPM_CONFIG_TMP=$XDG_RUNTIME_DIR/npm
export PYLINTHOME="$XDG_CONFIG_HOME/pylint"
export XMONAD_CACHE_DIR="$XDG_CACHE_HOME/xmonad"
export XMONAD_CONFIG_DIR="$XDG_CONFIG_HOME/xmonad"
export XMONAD_DATA_DIR="$XDG_DATA_HOME/xmonad"
[[ -f ~/.config/system/local.export.sh ]] && source ~/.config/system/local.export.sh
