__GLOBAL__print() {
    for var in "$@"
    do
        printf $(__VAL__ $var)
    done
    printf "\n"
}

__VAL__() {
    if [ "$1" = "s" ]; then
        printf ""
    else
        printf $(printf "$1" | tail -c $(expr ${#1} - 1))
    fi
}

__VAR__() {
    printf $1$2
}

# if 0, return 1. else return 0
__RETCODE__() {
    __TEMP__t=`printf $1 | head -c 1`
    __TEMP__v=$(__VAL__ $1)
    if [ "$__TEMP__t" = "s" ]; then
        [ "$__TEMP__v" != "" ]; return $?
    else
        [ "$__TEMP__v" -ne 0 ]; return $?
    fi
}
