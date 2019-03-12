
ref-aws() {
        eval $(sicom creds $1 -e)
}

# Automatically place virtualenvs in .virtualenvs so vim can use them
virenv() {
        virtualenv "$HOME/.virtualenvs/"$1
}

envactivate (){
        source "$HOME/.virtualenvs/$1/bin/activate"
}

ck() { 
        cd "$@" && k;
}
jk() { 
        j "$@" && k;
}
