alias pip="pip3"
alias python="python3"
alias p="python3"
alias pe="pipenv"

alias tmux="TERM=screen-256color-bce tmux"
alias icat="kitty +kitten icat"
alias weather="curl http://wttr.in/"

alias ga="git add"
alias gb="git branch"
alias gc="git commit"
alias gco="git checkout"
alias gd="git diff"
alias gg="fuzzy_git_log"
alias gst="git stash"
alias gl="git pull"
alias gp="git push"
alias gr="git rebase"
alias gs="git status"
alias gf="fuzzy_git_status"
alias gt="cd \$(git rev-parse --show-toplevel)"
alias gi="fuzzy_git_ignore"

alias ya="yadm add"
alias yb="yadm branch"
alias yc="yadm commit"
alias yco="yadm checkout"
alias yd="yadm diff"
alias yg="GIT_EXEC=yadm fuzzy_git_log"
alias yh="yadm stash"
alias yl="yadm pull"
alias yp="yadm push"
alias yr="yadm rebase"
alias ys="yadm status"

alias za="zathura"
alias sv="sudo edit"
alias l="tree -DhvC -L 1"
alias t="tree -DhvC"
alias tl="tree -DhvC -L"
alias r="rg --max-depth=10 --hidden --pretty --follow"
alias x="z -I"
alias uuid="python -c \"import uuid; print(str(uuid.uuid4()));\""

alias kc="kubectl"
alias mk="minikube"
alias d="docker"
alias dc="docker compose"

alias sa="sudo aura"
alias sc="sudo systemctl"

alias e="set -a; source .env; set +a;"

if command -v floaterm &> /dev/null; then
  alias v=floaterm
else
  alias v=nvim
fi

alias tp="telepresence"

[[ -f "$HOME/.config/system/local.alias.sh" ]] && source "$HOME/.config/system/local.alias.sh"
