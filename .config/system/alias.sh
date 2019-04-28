alias pip=pip3
alias python=python3.7
alias tmux="TERM=screen-256color-bce tmux"
alias icat="kitty +kitten icat"
alias weather="curl http://wttr.in/"
alias gg="git log --graph --abbrev-commit --branches --decorate --format=format:'%C(bold blue)%h%C(reset) %x09%C(bold white)%<(70,trunc)%s%C(reset) %C(magenta)- %an %C(reset) %C(cyan)%ai%C(reset) %C(auto)%+d%C(reset)'"
alias yg="yadm log --graph --abbrev-commit --branches --decorate --format=format:'%C(bold blue)%h%C(reset) %x09%C(bold white)%<(70,trunc)%s%C(reset) %C(magenta)- %an %C(reset) %C(cyan)%ai%C(reset) %C(auto)%+d%C(reset)'"
alias gs="git status"
alias gp="git push"
alias ga="git add"
alias gc="git commit"
alias gco="git checkout"
alias v=nvim
[[ -f "~/.config/system/local.alias.sh" ]] && source ~/.config/system/local.alias.sh
