function install_package() {
  if [[ "$+commands[apt]" -eq "1" ]]
  then
    echo "APT NOT IMPLEMENTED YET.";
    return 1;
  elif [[ "$+commands[dnf]" -eq "1" ]]
  then
    sudo dnf install $1
  elif [[ "$+commands[pacman]" -eq "1" ]]
  then
    echo "PACMAN NOT IMPLEMENTED YET.";
    return 1;
  elif [[ "$+commands[zypper]" -eq "1" ]]
  then
    echo "ZYPPER NOT IMPLEMENTED YET.";
    return 1;
  else
    echo "You are using an unsupported OS. Sorry, but screw off.";
    return 1;
  fi
}

