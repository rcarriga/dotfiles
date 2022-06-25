[[ -f "$HOME/.config/zsh/base.sh" ]] && source "$HOME/.config/zsh/base.sh"

if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]] && [[ $(fgconsole 2>/dev/null) == 1 ]]; then
   startx -- vt1 &> /dev/null
   if [[ $(/etc/hostname) == "CalypsoAI" ]]; then
       exec sudo /usr/bin/prime-switch
   fi
fi
