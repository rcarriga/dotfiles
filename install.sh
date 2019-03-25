#!/bin/bash

YELLOW="\033[0;31m"
NC='\033[0m' # No Color
cd ~

echo -e $YELLOW"This process will delete .vimrc, .gitconfig .tmux.conf & .zshrc!$NC"
read -n 1 -p "Hit any key to continue (Or Ctrl-C to cancel)"

if command -v apt-get >/dev/null 2>&1; then
  sudo apt-get update
  sudo apt-get upgrade

  command -v curl >/dev/null 2>&1 || (echo -e $YELLOW"Installing curl$NC" && sudo apt-get install -y curl)
  
  command -v kitty >/dev/null 2>&1 || (echo -e $YELLOW"Installing kitty terminal" && sudo apt-get install -y kitty)
    
  command -v tmux >/dev/null 2>&1 || (echo -e $YELLOW"Installing Tmux$NC" && sudo apt-get install -y tmux)
    
  command -v vim >/dev/null 2>&1 || (echo -e $YELLOW"Installing Vim$NC" && sudo apt-get install -y vim)
    
  command -v zsh >/dev/null 2>&1 || (echo -e $YELLOW"Installing Zsh$NC" && sudo apt-get install -y zsh)
  
  command -v i3 >/dev/null 2>&1 || (echo -e $YELLOW"Installing i3$NC" && sudo apt-get install -y i3)

  command -v i3blocks >/dev/null 2>&1 || (echo -e $YELLOW"Installing i3blocks$NC" && sudo apt-get install -y i3blocks)

  command -v blueman-applet >/dev/null 2>&1 || (echo -e $YELLOW"Installing blueman$NC" && sudo apt-get install -y blueman)

  command -v sensors >/dev/null 2>&1 || (echo -e $YELLOW"Installing lm-sensors$NC" && sudo apt-get install -y lm-sensors)

  command -v redshift >/dev/null 2>&1 || (echo -e $YELLOW"Installing redshift$NC" && sudo apt-get install -y redshift)

  command -v ranger >/dev/null 2>&1 || (echo -e $YELLOW"Installing ranger$NC" && sudo apt-get install -y ranger)

  command -v light >/dev/null 2>&1 || (echo -e $YELLOW"NEED TO MANUALLY INSTALL light https://github.com/haikarainen/light$NC" && sudo apt-get install -y light)

  command -v feh >/dev/null 2>&1 || (echo -e $YELLOW"Installing feh$NC" && sudo apt-get install -y feh)

  command -v lxappearance >/dev/null 2>&1 || (echo -e $YELLOW"Installing lxappearance$NC" && sudo apt-get install -y lxappearance)

  command -v rofi >/dev/null 2>&1 || (echo -e $YELLOW"Installing rofi$NC" && sudo apt-get install -y rofi)

  command -v stack >/dev/null 2>&1 || (echo -e $YELLOW"Installing Stack$NC" && curl -sSL https://get.haskellstack.org/ | sh)
  
  command -v npm >/dev/null 2>&1 || (echo -e $YELLOW"Installing NodeJS & npm$NC" && curl -sL https://deb.nodesource.com/setup_11.x | sudo -E bash - && sudo apt-get install -y nodejs)

  command -v python3 >/dev/null 2>&1 || (echo -e $YELLOW"Installing python3$NC" && sudo apt-get install -y python3.7)

  command -v pip3 >/dev/null 2>&1 || (echo -e $YELLOW"Installing pip3$NC" && sudo apt-get install -y python3-pip && pip3 install neovim)

  command -v ag >/dev/null 2>&1 || (echo -e $YELLOW"Installing the silver searcher$NC" && sudo apt-get install -y silversearcher-ag)
    
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

[[ ! -f ~/.vimrc ]] || test -h ~/.vimrc || ( rm ~/.vimrc && ln -sv ~/dotfiles/vim/.vimrc ~ )
[[ ! -f ~/.zshrc ]] || test -h ~/.zshrc || ( rm ~/.zshrc && ln -sv ~/dotfiles/zsh/.zshrc ~ )
[[ ! -f ~/.gitconfig ]] || test -h ~/.gitconfig || ( rm ~/.gitconfig && ln -sv ~/dotfiles/git/.gitconfig ~ )
[[ ! -f ~/.tmux.conf ]] || test -h ~/.tmux.conf || ( rm ~/.tmux.conf && ln -sv ~/dotfiles/tmux/.tmux.conf )


[[ -n $(fc-list | grep 'Fira Code') ]] || ( echo -e $YELLOW"Installing Fira Code for them beautiful ligatures" && sudo apt-get install fonts-firacode )
[[ -d /usr/share/icons/Numic-Circle ]] || ( echo -e $YELLOW"Installing Numix Icons$NC" && sudo apt-get install numix-icon-theme-circle )
[[ -d /usr/share/themes/Arc ]] || ( echo -e $YELLOW"Installing Arc theme$NC" && sudo apt-get install arc-theme )

echo -e $YELLOW"Setting up global .gitignore$NC"
git config --global core.excludesfile ~/dotfiles/git/.gitignore_global
  
sudo apt-get autoremove

[[ $(which zsh) == $SHELL ]] || chsh -s $(which zsh) && echo -e "Shell changed to zsh. Log out and in to start using!$NC" && echo -e "Run 'zplug install' once launched"
