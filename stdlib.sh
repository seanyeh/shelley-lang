__COUNT__GLOBAL__=0

__GLOBAL__print() {
    for var in "$@"
    do
        printf "$(__VAL__ "$var")"
    done
    printf "\n"
}

__VAL__() {
    if [ "$1" = "s" ]; then
        printf ""
    else
        printf "$(printf "$1" | tail -c $(expr ${#1} - 1))"
    fi
}

__VAR__() {
    printf $1$2
}


__DEREF__() {
    __TEMP__var="$""$1"
    eval printf "\"$__TEMP__var\""
}

# __SET__ scope var val
__SET__() {
    __TEMP__count="__COUNT""$1"
    # [ -z "$(__DEREF__ $__TEMP__count)" ] && eval "$__TEMP__count=0"

    __TEMP__countval=$(eval printf "\$$__TEMP__count")
    __TEMP__varname="__COUNT$__TEMP__countval$2"
    eval "$__TEMP__varname=$3"
}

# __GET__ scope var
__GET__() {
    __TEMP__count="__COUNT""$1"
    if [ -z "$(__DEREF__ $__TEMP__count)" ]; then
        echo "Scope not found: $1"
        exit 1
    fi

    __TEMP__countval=$(eval printf "\$$__TEMP__count")
    __TEMP__varname="__COUNT$__TEMP__countval$2"
    eval printf "\"\$$__TEMP__varname\""
}

# __SETCOUNT__ scope
__SETCOUNT__() {
    __TEMP__varname="__COUNT""$1"
    __TEMP__countval="$(__DEREF__ $__TEMP__varname)"
    if [ -z "$__TEMP__countval" ]; then
        eval "$__TEMP__varname=1"
    else
        eval "$__TEMP__varname=`expr $__TEMP__countval + 1`"
    fi
}

__DECCOUNT__(){
    __TEMP__varname="__COUNT""$1"
    __TEMP__countval="$(__DEREF__ $__TEMP__varname)"
    eval "$__TEMP__varname=`expr $__TEMP__countval - 1`"
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
