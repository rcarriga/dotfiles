
ref-aws() {
        eval $(sicom creds $1 -e)
}

l (){
    eval "ls -Fhl $@ | awk '{print \"\033[0;33m\"\$1\"\033[0;37m\t\"\$2\"\033[0;34m\t\"\$3\"\033[1;32m\t\"\$5\"\033[0;37m\t\"\$6\" \"\$7\"\033[0;31m\t\"\$8\"\033[1;37m\t\"\$9}'"
}
