# History {{{
    export HISTFILE="$ZDOTDIR/.zhistory"
    export HISTSIZE="4000"
    export SAVEHIST="5000"
    setopt HIST_IGNORE_ALL_DUPS
# }}}

# Remove path separator from WORDCHARS.
export WORDCHARS=${WORDCHARS//[\/]}

# Enable Vi mode
bindkey -v

# Completion {{{
    setopt menucomplete
# }}}

# Autosuggestions {{{Mappings
    setopt -o shareHistory
    ZSH_AUTOSUGGEST_STRATEGY=(history completion)
    ZSH_AUTOSUGGEST_USE_ASYNC=1

    bindkey '^k' autosuggest-accept
    bindkey '^h' autosuggest-fetch
    bindkey '^j' autosuggest-execute
# }}}

