#!/usr/bin/env zsh

export PATH=$HOME/.local/bin:$PATH

SCRIPT_ROOT=$(readlink -f `dirname $0`)
source $SCRIPT_ROOT/installer.zsh
RUSTUP_URL=https://sh.rustup.rs
CARGO_PATH=$HOME/.cargo

if [[ -d "$CARGO_PATH" ]]
then
  echo "rustup is already installed."
  return 0
fi

if [[ "$+commands[curl]" -eq "0" ]]
then
  echo "cURL is not installed. Installing cURL."
  install_package curl;
fi

curl --proto '=https' --tlsv1.2 -sSf $RUSTUP_URL | sh -s -- --profile=default -y
