export QT_QPA_PLATFORMTHEME="qt5ct"
export KEYTIMEOUT=1
[[ $FPATH ]] && export FPATH="$ZDOTDIR/completions:$ZDOTDIR/functions:${FPATH}:$ZDOTDIR/functions.local" 
export TERM=xterm-kitty
export MANPATH="/usr/local/man:$MANPATH"
export EDITOR="nvim"
export SCRIPT_DIR=$HOME/.config/scripts
export MANPAGER='nvim +Man!'
export GOPATH="$HOME/.go"
export ZIM_HOME=${ZDOTDIR:-${HOME}}/.zim
export BAT_THEME="Monokai Extended Origin"
export ZVM_VI_SURROUND_BINDKEY="s-prefix"
export PIPENV_VENV_IN_PROJECT=1

# fzf
export FZF_DEFAULT_COMMAND="rg --files --max-depth=10 -g \"!.git\" -g \"!undodir\" --hidden --color never --follow"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window down:3:hidden:wrap --bind '?:toggle-preview'"
export FZF_CTRL_T_OPTS="--preview \". $ZDOTDIR/functions/fuzzy_preview {}\""
export FZF_DEFAULT_OPTS="--height 96% --reverse --bind=shift-right:preview-page-down,shift-left:preview-page-up"
export FZF_COMPLETION_TRIGGER="#"

# Handling config and cache files

export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
export XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
export XDG_DATA_HOME=${XDG_DATA_HOME:-$HOME/.local/share}
if [ ! -w ${XDG_RUNTIME_DIR:="/run/user/$UID"} ]; then
    echo "\$XDG_RUNTIME_DIR ($XDG_RUNTIME_DIR) not writable. Setting to /tmp." >&2
    XDG_RUNTIME_DIR=/tmp
fi
export XDG_RUNTIME_DIR


# Used in oh-my-zsh plugins...
export ZSH_CACHE_DIR="$XDG_CACHE_HOME"

export npm_config_prefix="$XDG_CACHE_HOME/node_modules"
export DOCKER_CONFIG="$XDG_CONFIG_HOME"/docker
export LESSHISTFILE="$XDG_CACHE_HOME/less.hst"
export _ZL_DATA="$XDG_DATA_HOME/zlua"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/config"
export NPM_CONFIG_CACHE=$XDG_CACHE_HOME/npm
export NPM_CONFIG_TMP=$XDG_RUNTIME_DIR/npm
export PYLINTHOME="$XDG_CONFIG_HOME/pylint"
export XMONAD_CACHE_DIR="$XDG_CACHE_HOME/xmonad"
export XMONAD_CONFIG_DIR="$XDG_CONFIG_HOME/xmonad"
export XMONAD_DATA_DIR="$XDG_DATA_HOME/xmonad"
export PATH="$SCRIPT_DIR:$HOME/.local/bin:$HOME/bin:/usr/local/bin:/home/ronan/.pyenv/bin:$GOPATH/bin:$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$M2_HOME/bin:$npm_config_prefix/bin:$HOME/.cache/gem/ruby/3.0.0/bin:$HOME/.cargo/bin/:${KREW_ROOT:-$HOME/.krew}/bin:$PATH"
export LD_LIBRARY_PATH="/usr/local/cuda-11.0/lib64:/usr/local/lib"
export HELM_EXPERIMENTAL_OCI=1
[[ -f ~/.config/system/local.export.sh ]] && source ~/.config/system/local.export.sh
