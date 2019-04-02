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
alias v=nvim
alias gg="git log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)%aD%C(reset) %C(white)%s%C(reset) %C(dim white)- %an <%ae> %C(reset) %C(auto)%d%C(reset)'"
