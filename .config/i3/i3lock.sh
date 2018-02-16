#!/bin/sh
scrot -q 100 /tmp/screen.png
convert /tmp/screen.png -scale 10% -scale 1000% /tmp/screen.png
[[ -f $1 ]] && convert /tmp/screen.png $1 -gravity center -composite -matte /tmp/screen.png
composite -gravity center ~/Pictures/Gentoo-logo-dark.png /tmp/screen.png /tmp/screen.png
i3lock -u -i /tmp/screen.png
rm /tmp/screen.png
