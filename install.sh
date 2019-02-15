# #!/bin/bash

chsh -s $(which zsh)
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
