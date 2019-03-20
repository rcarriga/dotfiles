#!/bin/bash

YELLOW="\033[0;31m"
NC='\033[0m' # No Color
cd ~

echo -e $YELLOW"This process will delete .vimrc, .gitconfig .tmux.conf & .zshrc!$NC"
read -n 1 -p "Hit any key to continue (Or Ctrl-C to cancel)"

if command -v apt >/dev/null 2>&1; then
  sudo apt update
  sudo apt upgrade

  command -v curl >/dev/null 2>&1 || (echo -e $YELLOW"Installing curl$NC" && sudo apt install -y curl)
  
  command -v kitty >/dev/null 2>&1 || (echo -e $YELLOW"Installing kitty terminal" && sudo apt install -y kitty)
    
  command -v tmux >/dev/null 2>&1 || (echo -e $YELLOW"Installing Tmux$NC" && sudo apt install -y tmux)
    
  command -v vim >/dev/null 2>&1 || (echo -e $YELLOW"Installing Vim$NC" && sudo apt install -y vim)
    
  command -v zsh >/dev/null 2>&1 || (echo -e $YELLOW"Installing Zsh$NC" && sudo apt install -y zsh)

  command -v stack >/dev/null 2>&1 || (echo -e $YELLOW"Installing Stack$NC" && curl -sSL https://get.haskellstack.org/ | sh)
  
  command -v npm >/dev/null 2>&1 || (echo -e $YELLOW"Installing NodeJS & npm$NC" && curl -sL https://deb.nodesource.com/setup_11.x | sudo -E bash - && sudo apt-get install -y nodejs)

  command -v python3 >/dev/null 2>&1 || (echo -e $YELLOW"Installing python3$NC" && sudo apt install -y python3.7 && sudo apt install -y python3-pip && pip3 install neovim)

  command -v ag >/dev/null 2>&1 || echo -e $YELLOW"NEED TO INSTALL THE SILVER SEARCHER!!" &&
    
  command -v pyls >/dev/null 2>&1 || (echo -e $YELLOW"Installing Python Language Server$NC" && pip3 install "python-language-server[all]")

  command -v typescript-language-server >/dev/null 2>&1 || (echo -e $YELLOW"Installing TypeScript Language Server$NC" && sudo npm install -g typescript-language-server)


  if command -v hie >/dev/null 2>&1; then
    echo -e $YELLOW"Installing Haskell IDE Engine$NC" 
    git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules
    sudo apt-get install libtinfo-dev 2>&1 > /dev/null
    cd haskell-ide-engine
    stack upgrade 2>&1 >/dev/null
    stack ghc 2>&1 > /dev/null
    STACK_VERSION=$(stack ghc -- --version | awk "{print $NF}")
    read -p "Haskell IDE Engine can take a long time to setup. Do you want to do this now? Enter 'y'" -n 1 -r
    echo -e
    if [[ $REPLY =~ ^[Yy]$ ]]; then
      make hie-$STACK_VERSION
      make build-doc-$STACK_VERSION
    else 
      echo -e $YELLOW"Not setting up HIE. Run 'make hie-$STACK_VERSION' and 'make hie-$STACK_VERSION' in HIE directory to build$NC"
    fi

    cd # Return to home directory
  fi
fi

rm ~/.vimrc
rm ~/.zshrc
rm ~/.gitconfig
rm ~/.tmux.conf
ln -sv ~/dotfiles/git/.gitconfig ~
ln -sv ~/dotfiles/vim/.vimrc ~
ln -sv ~/dotfiles/zsh/.zshrc ~
ln -sv ~/dotfiles/tmux/.tmux.conf

echo -e $YELLOW"Setting up global .gitignore$NC"
git config --global core.excludesfile ~/dotfiles/git/.gitignore_global
  
chsh -s $(which zsh)
echo -e "Shell changed to zsh. Log out and in to start using!$NC"

