alias prettyjson="python -m json.tool"
alias ls='ls -GFh'
if [ -d "/usr/local/Cellar/vim/8.1.0800" ]; then
  alias vim=/usr/local/Cellar/vim/8.1.0800/bin/vim
fi
alias pip=pip3
alias python=python3.7
alias tmux="TERM=screen-256color-bce tmux"
alias icat="kitty +kitten icat"
alias h='k -h'
alias weather="curl http://wttr.in/"
alias gg="git log --graph --abbrev-commit --branches --decorate --format=format:'%C(bold blue)%h%C(reset) %x09%C(bold white)%<(70,trunc)%s%C(reset) %C(magenta)- %an %C(reset) %C(cyan)%ai%C(reset) %C(auto)%+d%C(reset)'"
alias yg="yadm log --graph --abbrev-commit --branches --decorate --format=format:'%C(bold blue)%h%C(reset) %x09%C(bold white)%<(70,trunc)%s%C(reset) %C(magenta)- %an %C(reset) %C(cyan)%ai%C(reset) %C(auto)%+d%C(reset)'"
alias gs="git status"
alias gp="git push"
alias ga="git add"
alias v=nvim
