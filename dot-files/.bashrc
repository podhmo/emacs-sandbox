export PS1="\[\033[0;37m\][\w/ \t]\$ \[\033[0;00m\]"
export LESSCHARSET=utf-8

alias e="emacsclient -n"
alias githere="git branch | grep '^\* ' | cut -d ' ' -f 2"

function replace () { grep -lr $1 . --exclude=.git | xargs gsed -i "s@$1@$2@g"; }
function replace2 () { echo "grep -lr \"$1\" .  --exclude=.git | xargs gsed -i \"s@$1@$2@g\";"; }
