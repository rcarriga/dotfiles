systemctl --user import-environment DISPLAY

export SSH_AUTH_SOCK


case $(hostname) in
    CalypsoAI)
        xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Tapping Enabled" 1
        xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Natural Scrolling Enabled" 1
        xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Accel Speed" 0.8
        setxkbmap -layout gb -option ctrl:nocaps
        xset r rate 150 40
        xsetroot -cursor_name left_ptr
        xrdb -merge ~/.Xresources
        export XMONAD_MODE=""
        /usr/bin/prime-offload
        exec xmonad
        ;;
    *)
        ;;
esac
