sudo pkill use-cloud
stack build
if [[ $# -eq 2 ]] ; then
    for ((i = 1; i <= $2; i++)); do
        stack exec use-cloudhaskell-exe worker localhost 800$i &
    done
    read -p ""
    time stack exec use-cloudhaskell-exe manager localhost 8100 $1
fi
