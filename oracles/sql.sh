curl $1 -d "$2=$3" 2>/dev/null | tee /dev/tty | grep -v "syntax error" >/dev/null

