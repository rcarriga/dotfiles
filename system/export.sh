# If you come from bash you might have to change your $PATH.
export PATH=/Library/Java/JavaVirtualMachines/jdk1.8.0_201.jdk/bin/:$HOME/bin:/usr/local/bin:$PATH
export ZSH=~/".oh-my-zsh"
export TERM=xterm-256color
export MANPATH="/usr/local/man:$MANPATH"
export EDITOR='vim'
export JAVA_HOME=$(/usr/libexec/java_home -v 1.8)
export M2_HOME=/Applications/apache-maven-3.6.0
export PATH=$PATH:$M2_HOME/bin
export FZF_DEFAULT_COMMAND='ag --depth 10 --hidden --ignore .git -f -g ""'
