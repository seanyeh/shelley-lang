__GLOBAL__print() {
    for var in "$@"
    do
        printf $(__VAL__ $var)
    done
    printf "\n"
}

__VAL__() {
    __TEMP__RAW=$(eval printf "$""$1")
    printf $(printf $__TEMP__RAW | tail -c $(expr ${#__TEMP__RAW} - 1))
}

__VAR__() {
    printf $1$2
}

# if 0, return 1. else return 0
__RETCODE__() {
    __TEMP__t=`printf $__RET__ | head -c 1`
    if [ __TEMP__t = "s" ]; then
        return 0
    else
        __TEMP__v=$(__VAL__ __RET__)
        [ "$__TEMP__v" -ne 0 ]
        return $?
    fi
}
