
fuzzy_preview () {
    if [[ -f "$1" ]]; then
        bat -r :"$LINES" "$1" --color always
        exit
    else 
        # Check if directories passed by z.lua
        PARSED=$(echo $1 | grep -o "/.*") || $1
        if [[ -d $PARSED ]]; then
            tree -DhvC -L 1 $PARSED
            exit
        fi
    fi
}
