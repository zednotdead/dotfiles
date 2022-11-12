#!/usr/bin/env bash

hostname=`cat /etc/hostname`

case $hostname in
  "home-main")
    bash $HOME/.screenlayout/desktop.sh
    ;;
  *)
    echo "Not running anything."
    ;;
esac
