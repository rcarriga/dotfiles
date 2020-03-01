systemctl --user import-environment DISPLAY

eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

xinput set-prop "DLL07BE:01 06CB:7A13 Touchpad" "libinput Tapping Enabled" 1
xinput set-prop "DLL07BE:01 06CB:7A13 Touchpad" "libinput Natural Scrolling Enabled" 1
setxkbmap -layout gb
setxkbmap -option caps:escape
xset r rate 150 40
xsetroot -cursor_name left_ptr

exec xmonad
