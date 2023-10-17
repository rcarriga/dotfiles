#!/bin/sh

systemctl --user import-environment DISPLAY

export SSH_AUTH_SOCK

function base_settings() {
  setxkbmap -layout gb
  xset r rate 150 40
  xsetroot -cursor_name left_ptr
  xrdb -merge ~/.Xresources
}

case $(cat /etc/hostname) in
    cai)
        xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Tapping Enabled" 1
        xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Natural Scrolling Enabled" 1
        xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Accel Speed" 0.8
        export XMONAD_MODE="xmonadHIDPI"
        base_settings
        /usr/bin/prime-offload
        exec xmonad
        ;;
    XPS15)
        xinput set-prop "VEN_04F3:00 04F3:32AA Touchpad" "libinput Tapping Enabled" 1
        xinput set-prop "VEN_04F3:00 04F3:32AA Touchpad" "libinput Natural Scrolling Enabled" 1
        base_settings
        export XMONAD_MODE="xmonad"
        date >> ~/.cache/xmonad.log
        exec xmonad >> ~/.cache/xmonad.log
        ;;
    Desktop)
        base_settings
        export XMONAD_MODE="xmonad"
        date >> ~/.cache/xmonad.log
        exec xmonad >> ~/.cache/xmonad.log
        ;;
    Desktop)
        setxkbmap -layout gb
        xset r rate 150 40
        xsetroot -cursor_name left_ptr
        xrdb -merge ~/.Xresources
        export XMONAD_MODE="xmonad"
        date >> ~/.cache/xmonad.log
        exec xmonad >> ~/.cache/xmonad.log
        ;;
    *)
        ;;
esac

