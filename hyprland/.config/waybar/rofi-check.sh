#!/bin/sh
while true; do
	if pgrep -x "rofi" > /dev/null; then
		echo '{"class": "active", "text": "Start"}'
	else
		echo '{"class": "", "text": "Start"}'
	fi
	sleep 0.1
done
