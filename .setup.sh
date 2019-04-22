#!/bin/bash

cd

if command -v apt-get >/dev/null 2>&1; then
  sudo apt-get update
  sudo apt-get upgrade
  echo "Progress will be logged to setuplog.txt. If a step is taking a long time check the logs before quitting"

  echo "Beginning setup" >> setuplog.txt

  command -v curl >/dev/null 2>&1 || (echo -e "Installing curl" && sudo apt-get install -y curl >> setuplog.txt)
  
  command -v kitty >/dev/null 2>&1 || (echo -e "Installing kitty terminal" && sudo apt-get install -y kitty >> setuplog.txt)
    
  command -v tmux >/dev/null 2>&1 || (echo -e "Installing Tmux" && sudo apt-get install -y tmux >> setuplog.txt)
    
  command -v vim >/dev/null 2>&1 || (echo -e "Installing Vim" && sudo apt-get install -y vim >> setuplog.txt)
    
  command -v zsh >/dev/null 2>&1 || (echo -e "Installing Zsh" && sudo apt-get install -y zsh >> setuplog.txt)
  
  command -v i3 >/dev/null 2>&1 || (echo -e "Installing i3" && sudo apt-get install -y i3 >> setuplog.txt)

  command -v i3blocks >/dev/null 2>&1 || (echo -e "Installing i3blocks" && sudo apt-get install -y i3blocks >> setuplog.txt)

  command -v blueman-applet >/dev/null 2>&1 || (echo -e "Installing blueman" && sudo apt-get install -y blueman >> setuplog.txt)

  command -v sensors >/dev/null 2>&1 || (echo -e "Installing lm-sensors" && sudo apt-get install -y lm-sensors >> setuplog.txt)

  command -v redshift >/dev/null 2>&1 || (echo -e "Installing redshift" && sudo apt-get install -y redshift >> setuplog.txt)

  command -v ranger >/dev/null 2>&1 || (echo -e "Installing ranger" && sudo apt-get install -y ranger >> setuplog.txt)

  if ! command -v light >/dev/null 2>&1; then
    echo "Installing light"
    wget https://github.com/haikarainen/light/releases/download/v1.2/light-1.2.tar.gz  >> setuplog.txt
    tar xf light-1.2.tar.gz >> setuplog.txt
    cd light-1.2/
    ./configure && make >> setuplog.txt
    sudo make install >> setuplog.txt
    cd
    rm -rf light-1.2* >> setuplog.txt
  fi

  command -v feh >/dev/null 2>&1 || (echo -e "Installing feh" && sudo apt-get install -y feh >> setuplog.txt)

  command -v lxappearance >/dev/null 2>&1 || (echo -e "Installing lxappearance" && sudo apt-get install -y lxappearance >> setuplog.txt)

  command -v rofi >/dev/null 2>&1 || (echo -e "Installing rofi" && sudo apt-get install -y rofi >> setuplog.txt)

  command -v xautolock >/dev/null 2>&1 || (echo -e "Installing xautolock" && sudo apt-get install -y xautolock >> setuplog.txt)

  command -v i3lock-fancy >/dev/null 2>&1 || (echo -e "NEED TO MANUALL INSTALL i3lock-fancy https://github.com/meskarune/i3lock-fancy")
  
  command -v python3 >/dev/null 2>&1 || (echo -e "Installing python3" && sudo apt-get install -y python3.7 >> setuplog.txt)

  command -v pip3 >/dev/null 2>&1 || (echo -e "Installing pip3" && sudo apt-get install -y python3-pip && pip3 install neovim >> setuplog.txt)

  if ! command -v autojump >/dev/null 2>&1; then
    echo -e "Installing autojump" 
    git clone git://github.com/wting/autojump.git
    cd autojump
    python install.py
    cd
    rm -rf autojump
  fi

  command -v ag >/dev/null 2>&1 || (echo -e "Installing the silver searcher" && sudo apt-get install -y silversearcher-ag >> setuplog.txt)
    
  read -p "Do you want to install development tools? This can take upto 12GB if installing hie" -n 1 -r
  if [[ $REPLY =~ ^[Yy]$ ]]; then
    command -v stack >/dev/null 2>&1 || (echo -e "Installing Stack" && curl -sSL https://get.haskellstack.org/ | sh >> setuplog.txt)

    command -v npm >/dev/null 2>&1 || (echo -e "Installing NodeJS & npm" && curl -sL https://deb.nodesource.com/setup_11.x | sudo -E bash - >> setuplog.txt && sudo apt-get install -y nodejs >> setuplog.txt)

    command -v pyls >/dev/null 2>&1 || (echo -e "Installing Python Language Server" && pip3 install "python-language-server[all]" >> setuplog.txt)

    if ! command -v hie >/dev/null 2>&1; then
      echo -e "Installing Haskell IDE Engine. Takes a while so showing full output" 
      git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules
      sudo apt-get install libtinfo-dev 2>&1 > /dev/null
      cd haskell-ide-engine
      stack upgrade 2>&1 >/dev/null
      stack ghc -- --version 2>&1 > /dev/null
      STACK_VERSION=$(stack ghc -- --version | awk "{print $NF}")
      read -p "Haskell IDE Engine can take a long time to setup. Do you want to do this now? Enter 'y'" -n 1 -r
      if [[ $REPLY =~ ^[Yy]$ ]]; then
        stack ./install.hs hie-$STACK_VERSION
        stack ./install.hs build-doc-$STACK_VERSION
      else 
        echo -e "Not setting up HIE. Run 'make hie-$STACK_VERSION' and 'make hie-$STACK_VERSION' in HIE directory to build"
      fi

      cd # Return to home directory
    fi
  fi

  [[ -n $(fc-list | grep 'Fira Code') ]] || ( echo -e "Installing Fira Code for them beautiful ligatures" && sudo apt-get install fonts-firacode  >> setuplog.txt)
  [[ -n $(fc-list | grep 'FontAwesome') ]] || ( echo -e "Installing Font Awesome for system icons" && sudo apt-get install fonts-font-awesome  >> setuplog.txt)
  [[ -d /usr/share/icons/Numic-Circle ]] || ( echo -e "Installing Numix Icons" && sudo apt-get install numix-icon-theme-circle  >> setuplog.txt)
  [[ -d /usr/share/themes/Arc ]] || ( echo -e "Installing Arc theme" && sudo apt-get install arc-theme  >> setuplog.txt)

  sudo apt-get autoremove
else
  echo "This script is only meant for use with Ubuntu!"
fi

echo -e "Setting up global .gitignore"
git config --global core.excludesfile ~/dotfiles/git/.gitignore_global

[[ $(which zsh) == $SHELL ]] || (chsh -s $(which zsh) && echo -e "Shell changed to zsh. Log out and in to start using!" && echo -e "Run 'zplug install' once launched")
