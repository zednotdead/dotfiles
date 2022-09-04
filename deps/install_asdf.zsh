#!/usr/bin/env zsh

ASDF_URL="https://github.com/asdf-vm/asdf.git"
ASDF_VERSION="v0.10.2"
ASDF_PATH="$HOME/.asdf"

source `dirname $0`/installer.zsh

if [[ "$+commands[curl]" -eq "0" ]]
then
  echo "cURL is not installed. Installing cURL."
  install_package curl;
fi

if [[ "$+commands[git]" -eq "0" ]]
then
  echo "git is not installed. Installing git."
  install_package git;
fi

if [[ -d "$ASDF_PATH" ]]
then
  echo "asdf is already installed, good.";
else
  echo "Installing asdf..."
  git clone $ASDF_URL $ASDF_PATH --branch $ASDF_VERSION
fi

. $ASDF_PATH/asdf.sh

if [[ $+commands[asdf] -eq "0" ]]
then
  echo "asdf is not working for some reason."
fi

echo "Installing asdf plugins..."

function install_nodejs() {
  if [[ "$(asdf plugin list | grep nodejs)" -eq "nodejs" ]]
  then
    echo "NodeJS plugin is already installed."
  else
    install_package dirmngr
    install_package gpg
    install_package curl
    install_package gawk
    asdf plugin add nodejs https://github.com/asdf-vm/asdf-nodejs.git
  fi
}

install_nodejs

return 0;
