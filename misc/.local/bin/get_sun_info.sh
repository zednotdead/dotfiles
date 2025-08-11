#!/usr/bin/env bash

DATE_FORMAT="+%D"

CACHED_INFO_PATH="$HOME/.cache/sun_info.json"
CACHED_INFO_DATE=$(date -d $(jq -r .results.date $CACHED_INFO_PATH) $DATE_FORMAT)

TODAY_DATE=$(date $DATE_FORMAT)

if [ $CACHED_INFO_DATE != $CACHED_INFO_DATE ]; then
	echo "The cached file out of date, getting a new file..."
	IP=$(curl -s https://ipinfo.io/ip)
	LOCATION=$(curl -s http://ip-api.com/json/$IP | jq -r '"lat=\(.lat)&lng=\(.lon)"')

	curl -s "https://api.sunrisesunset.io/json?$LOCATION" >$CACHED_INFO_PATH
	return 0
else
	echo "The cached file is up to date. Not fetching a new one."
	return 0
fi
