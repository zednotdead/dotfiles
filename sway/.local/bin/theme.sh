#!/usr/bin/env bash

CACHED_INFO_PATH="$HOME/.cache/sun_info.json"

. "$HOME/.local/bin/get_sun_info.sh"

THEME_DARK="Adwaita-dark"
THEME_LIGHT="Adwaita-dark"

sun_info=$(jq -r .results $CACHED_INFO_PATH)
sunrise=$(echo $sun_info | jq -r .sunrise)
sunset=$(echo $sun_info | jq -r .sunset)

sunrise_ts=$(date -d "$sunrise" +%s)
sunset_ts=$(date -d "$sunset" +%s)
current_ts=$(date +%s)

current_theme=$(gsettings get org.gnome.desktop.interface color-scheme | sed "s/'prefer-\(.*\)'/\\1/g")
new_theme="light"

if [ $current_ts -ge $sunrise_ts ] &&
	[ $current_ts -le $sunset_ts ]; then
	new_theme="light"
else
	new_theme="dark"
fi

if [ "${new_theme}" == "${current_theme}" ]; then
	notify-send "no changes"
	exit 0
fi

if [ $new_theme == "light" ]; then
	notify-send "Theme manager" "Changing theme to light..."
	gsettings set org.gnome.desktop.interface gtk-theme $THEME_LIGHT
	gsettings set org.gnome.desktop.interface color-scheme "'prefer-light'"
else
	notify-send "Theme manager" "Changing theme to dark..."
	gsettings set org.gnome.desktop.interface gtk-theme $THEME_DARK
	gsettings set org.gnome.desktop.interface color-scheme "'prefer-dark'"
fi
