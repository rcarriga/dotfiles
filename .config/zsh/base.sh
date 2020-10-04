
[[ -f "$ZDOTDIR/alias.sh" ]] && source "$ZDOTDIR/alias.sh"

[[ -f "$ZDOTDIR/export.sh" ]] && source "$ZDOTDIR/export.sh"

[[ -f "$ZDOTDIR/options.sh" ]] && source "$ZDOTDIR/options.sh"

[[ -d "$ZDOTDIR/functions" ]] && autoload -Uz $(ls $ZDOTDIR/functions)

[[ -d "$ZDOTDIR/completions" ]] && autoload -Uz $(ls $ZDOTDIR/completions)

