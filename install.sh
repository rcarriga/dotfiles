#!/bin/bash

chsh -s $(which zsh)
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
echo "Shell changed to zsh. Log out and in to start using!\n"

echo "\nThe next step will remove .vimrc, .gitconfig & .zshrc from home directory!\n"
read -p "Hit any key to continue (Or Ctrl-C to cancel)" x
rm ~/.vimrc
rm ~/.zshrc
rm ~/.gitconfig
ln -sv ~/dotfiles/git/.gitconfig ~
ln -sv ~/dotfiles/vim/.vimrc ~
ln -sv ~/dotfiles/zsh/.zshrc ~

echo "Setting up global .gitignore"
git config --global core.excludesfile ~/dotfiles/git/.gitignore_global

echo "Installing zplug"
curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh

echo "Installing powerline fonts. (My favourite it cousine)"
git clone https://github.com/powerline/fonts.git --depth=1
cd fonts
./install.sh
cd ..
rm -rf fonts

echo "You will need to change executable paths for language servers in .vimrc!!"
