#!/bin/bash

YELLOW='\033[0;31m'
cd ~

echo "\n$YELLOW The next step will remove .vimrc, .gitconfig .tmux.conf & .zshrc from home directory!\n"
read -n 1 -p "Hit any key to continue (Or Ctrl-C to cancel)" x
rm ~/.vimrc
rm ~/.zshrc
rm ~/.gitconfig
rm ~/.tmux.conf
ln -sv ~/dotfiles/git/.gitconfig ~
ln -sv ~/dotfiles/vim/.vimrc ~
ln -sv ~/dotfiles/zsh/.zshrc ~
ln -sv ~/dotfiles/tmux/.tmux.conf


echo "$YELLOW Setting up global .gitignore"
git config --global core.excludesfile ~/dotfiles/git/.gitignore_global


echo "$YELLOW Installing powerline fonts. (My favourite is cousine)"
git clone https://github.com/powerline/fonts.git --depth=1
cd fonts
./install.sh
cd ..
rm -rf fonts

if command -v apt >/dev/null 2>&1; then
  command -v stack >/dev/null 2>&1 || echo "$YELLOW Installing Stack" && curl -sSL https://get.haskellstack.org/ | sh
  command -v npm >/dev/null 2>&1 || echo "$YELLOW Installing NodeJS" && curl -sL https://deb.nodesource.com/setup_11.x | sudo -E bash - && sudo apt-get install -y nodejs
  command -v python3 >/dev/null 2>&1 || echo "$YELLOW Installing python3" &&  sudo apt install python3.7
  command -v ag >/dev/null 2>&1 || echo "$YELLOW Installing the silver searcher" && sudo apt install silversearcher-ag
  command -v pyls >/dev/null 2>&1 || echo "$YELLOW Installing Python Language Server" && pip install 'python-language-server[all]'
  command -v typescript-language-server >/dev/null 2>&1 || echo "$YELLOW Installing TypeScript Language Server" && npm install -g typescript-language-server
  command -v tmux >/dev/null 2>&1 || echo "$YELLOW Installing Tmux" && sudo apt install tmux 
  command -v vim >/dev/null 2>&1 || echo "$YELLOW Installing Vim" && sudo apt install vim
  command -v zsh >/dev/null 2>&1 || echo "$YELLOW Installing Zsh" && sudo apt install zsh
  if command -v hie >/dev/null 2>&1; then
    echo "$YELLOW Installing Haskell IDE Engine" 
    git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules
    cd haskell-ide-engine
    stack upgrade 2>&1 >/dev/null
    STACK_VERSION=$(stack -- --version | awk '{print $NF}')
    read -p "Haskell IDE Engine can take a long time to setup. Do you want to do this now? Enter 'y'" -n 1 -r
    echo    # (optional) move to a new line
    if [[ $REPLY =~ ^[Yy]$ ]]; then
      make hie-$STACK_VERSION
      make build-doc-$STACK_VERSION
    else 
      echo "$YELLOW Not setting up HIE. Run 'make hie-$STACK_VERSION' and 'make hie-$STACK_VERSION' in HIE directory to build"
    fi
    cd
  fi
fi

chsh -s $(which zsh)
echo "Shell changed to zsh. Log out and in to start using!\n"
