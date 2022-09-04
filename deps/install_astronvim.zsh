#!/usr/bin/env zsh

source `dirname $0`/installer.zsh

if [[ "$+commands[git]" -eq "0" ]]
then
  echo "git is not installed. Installing git."
  install_package git
fi

if [[ "$+commands[nvim]" -eq "0" ]]
then
  echo "Neovim is not installed. Installing Neovim."
  install_package neovim
fi

if [[ "$+commands[rg]" -eq "0" ]]
then
  echo "Ripgrep is not installed. Installing Ripgrep."
  install_package ripgrep
fi

if [[ "$+commands[lazygit]" -eq "0" ]]
then
  echo "LazyGit is not installed. Installing LazyGit."
  install_package LazyGit
fi

if [[ -e "$HOME/.config/nvim/config.ld" ]]
then
  echo "Astro Nvim is already installed."
  return 0;
fi

git clone https://github.com/AstroNvim/AstroNvim $HOME/.config/nvim
nvim +PackerSync +qall
