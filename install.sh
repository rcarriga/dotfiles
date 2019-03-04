#!/bin/bash

YELLOW="\033[0;31m"
NC='\033[0m' # No Color
cd ~

echo -e $YELLOW"This process will delete .vimrc, .gitconfig .tmux.conf & .zshrc!$NC"
read -n 1 -p "Hit any key to continue (Or Ctrl-C to cancel)"




if command -v apt >/dev/null 2>&1; then
  sudo apt update
    echo x
  sudo apt upgrade
    echo x

  command -v curl >/dev/null 2>&1 || 
    echo -e $YELLOW"Installing curl$NC" &&
    sudo apt install -y curl
    echo x

  command -v stack >/dev/null 2>&1 || 
    echo -e $YELLOW"Installing Stack$NC" && 
    curl -sSL https://get.haskellstack.org/ | sh
    echo x
  
  command -v npm >/dev/null 2>&1 || 
    echo -e $YELLOW"Installing NodeJS & npm$NC" && 
    curl -sL https://deb.nodesource.com/setup_11.x | sudo -E bash - && sudo apt-get install -y nodejs npm
    echo x

  command -v python3 >/dev/null 2>&1 || 
    echo -e $YELLOW"Installing python3$NC" &&
    sudo apt install -y python3.7
    echo x

  if command -v ag >/dev/null 2>&1 ; then
    echo -e $YELLOW"Installing the silver searcher$NC" 
    sudo add-apt-repository ppa:pgolm/the-silver-searcher
      echo x
    sudo apt-get update
      echo x
    sudo apt-get install the-silver-searcher
      echo x
  fi

  command -v pyls >/dev/null 2>&1 || 
    echo -e $YELLOW"Installing Python Language Server$NC" &&
    pip3 install "python-language-server[all]"
    echo x

  command -v typescript-language-server >/dev/null 2>&1 || 
    echo -e $YELLOW"Installing TypeScript Language Server$NC" &&
    npm install -g typescript-language-server
    echo x

  command -v tmux >/dev/null 2>&1 || 
    echo -e $YELLOW"Installing Tmux$NC" &&
    sudo apt install -y tmux
    echo x

  command -v vim >/dev/null 2>&1 || 
    echo -e $YELLOW"Installing Vim$NC" &&
    sudo apt install -y vim
    echo x

  command -v zsh >/dev/null 2>&1 || 
    echo -e $YELLOW"Installing Zsh$NC" &&
    sudo apt install -y zsh
    echo x

  if command -v hie >/dev/null 2>&1; then
    echo -e $YELLOW"Installing Haskell IDE Engine$NC" 
    git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules
      echo x
    cd haskell-ide-engine
    stack upgrade 2>&1 >/dev/null
    STACK_VERSION=$(stack -- --version | awk "{print $NF}")
    read -p "Haskell IDE Engine can take a long time to setup. Do you want to do this now? Enter 'y'" -n 1 -r
    echo -e    # (optional) move to a new line
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
  echo x
echo -e $YELLOW"Installing powerline fonts. (My favourite is cousine)$NC"
git clone https://github.com/powerline/fonts.git --depth=1
  echo x
cd fonts
./install.sh
  echo x
cd ..
rm -rf fonts

echo -e $YELLOW"Installing oh-my-zsh$NC"
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
  echo x
rm ~/.zshrc
mv ~/.zshrc.pre-oh-my-zsh ~/.zshrc

echo -e $YELLOW"Switching shell to zsh by default"
chsh -s $(which zsh)
echo -e "Shell changed to zsh. Log out and in to start using!$NC"
