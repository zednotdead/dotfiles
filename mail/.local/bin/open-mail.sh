#!/usr/bin/env bash

BROWSERS=$(cat $HOME/.config/mutt/browsers.json)
FZF_OPTIONS="--delimiter=: --height=8 --with-nth=2 --border=rounded --border-label=Browsers"
SELECTED_BROWSER=$(echo $BROWSERS | jq -r '.[] | "\(.command):\(.display)"' | fzf $FZF_OPTIONS | cut -d: -f1)

if [ -n "$SELECTED_BROWSER" ]; then
    bash -c "$SELECTED_BROWSER $1"
fi
