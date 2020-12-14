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
        export XMONAD_MODE="xmonadHIDPI"
        /usr/bin/prime-offload
        exec xmonad
        ;;
    XPS15)
        xinput set-prop "DLL07BE:01 06CB:7A13 Touchpad" "libinput Tapping Enabled" 1
        xinput set-prop "DLL07BE:01 06CB:7A13 Touchpad" "libinput Natural Scrolling Enabled" 1
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

