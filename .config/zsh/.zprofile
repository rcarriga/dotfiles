[[ -f "$HOME/.config/zsh/base.sh" ]] && source "$HOME/.config/zsh/base.sh"
if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
  [[ $(fgconsole 2>/dev/null) == 1 ]] && exec startx -- vt1 &> /dev/null
fi
