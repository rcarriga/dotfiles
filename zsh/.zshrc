# Add custom aliases here
source ~/dotfiles/system/alias.sh

# Add environment var exports here
source ~/dotfiles/system/export.sh

# Add custom functions here
source ~/dotfiles/system/function.sh

# Oh-my-zsh settings go in this one
source ~/dotfiles/zsh/ohmyzsh-setup.sh

# ZPlug settings go in... you guessed it!
source ~/dotfiles/zsh/zplug-setup.sh
SPACESHIP_PROMPT_ORDER=(
  time          # Time stamps section
  user          # Username section
  dir           # Current directory section
  host          # Hostname section
  git           # Git section (git_branch + git_status)
  package       # Package version
  haskell       # Haskell Stack section
  docker        # Docker section
  aws           # Amazon Web Services section
  venv          # virtualenv section
  pyenv         # Pyenv section
  terraform     # Terraform workspace section
  exec_time     # Execution time
  line_sep      # Line break
  battery       # Battery level and status
  vi_mode       # Vi-mode indicator
  jobs          # Background jobs indicator
  exit_code     # Exit code section
  char          # Prompt character
)
SPACESHIP_CHAR_SYMBOL=❯

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[[ -s /home/ronan/.autojump/etc/profile.d/autojump.sh ]] && source /home/ronan/.autojump/etc/profile.d/autojump.sh

autoload -U compinit && compinit -u

###-tns-completion-start-###
if [ -f /Users/rcarriga/.tnsrc ]; then 
    source /Users/rcarriga/.tnsrc 
fi
###-tns-completion-end-###
