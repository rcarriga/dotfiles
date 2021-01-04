
#### fzf-tab

zstyle ':completion:*' format '-- %d --'
# disable sort when completing options of any command
zstyle ':completion:complete:*:options' sort false

# use input as query string when completing zlua
zstyle ':fzf-tab:complete:_zlua:*' query-string input

# (experimental, may change in the future)
# some boilerplate code to define the variable `extract` which will be used later
# please remember to copy them
local extract="
# trim input(what you select)
local in=\${\${\"\$(<{f})\"%\$'\0'*}#*\$'\0'}
# get ctxt for current completion(some thing before or after the current word)
local -A ctxt=(\"\${(@ps:\2:)CTXT}\")
# real path
local realpath=\${ctxt[IPREFIX]}\${ctxt[hpre]}\$in
realpath=\${(Qe)~realpath}
"

# give a preview of commandline arguments when completing `kill`
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm,cmd -w -w"
zstyle ':fzf-tab:complete:kill:argument-rest' extra-opts --preview=$extract'ps --pid=$in[(w)1] -o cmd --no-headers -w -w' --preview-window=down:3:wrap

# give a preview of directory when completing cd
zstyle ':fzf-tab:complete:cd:*' fzf-preview ". $ZDOTDIR/functions/fuzzy_preview \$realpath"
zstyle ':fzf-tab:complete:nvim:*' fzf-preview ". $ZDOTDIR/functions/fuzzy_preview \$realpath"
zstyle ':fzf-tab:complete:rm:*' fzf-preview ". $ZDOTDIR/functions/fuzzy_preview \$realpath"

FZF_TAB_GROUP_COLORS=(
    $'\033[97m' $'\033[32m' $'\033[33m' $'\033[35m' $'\033[31m' $'\033[38;5;27m' $'\033[36m' \
    $'\033[38;5;100m' $'\033[38;5;98m' $'\033[91m' $'\033[38;5;80m' $'\033[92m' \
    $'\033[38;5;214m' $'\033[38;5;165m' $'\033[38;5;124m' $'\033[38;5;120m'
)

zstyle ':fzf-tab:*' group-colors $FZF_TAB_GROUP_COLORS

zstyle ':fzf-tab:*' single-group color
zstyle ':fzf-tab:*' show-group brief

