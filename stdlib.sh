__COUNT__GLOBAL__=0

__PRINTF__() {
    printf -- "$1"
}

__STR__length() {
    __PRINTF__ `__PRINTF__ "$1" | wc -c`
}

__F__GLOBAL__print() {
    for var in "$@"
    do
        __PRINTF__ "`__VAL__ "$var"`"
    done
    __PRINTF__ "\n"
}


# should write stdlib in shelly I think
# __LOGICAL__ op e1 e2

# __F__COMPARE__() {
__F__GLOBAL__compare() {
    __TEMP__op="`__VAL__ "$1"`"
    __TEMP__e1="`__VAL__ "$2"`"
    __TEMP__e2="`__VAL__ "$3"`"
    if [ "$__TEMP__e1" "$__TEMP__op" "$__TEMP__e2" ]; then
        __RET__=i1
        # return 0
    else
        __RET__=i0
        # return 1
    fi

    __RETCODE__ $__RET__
    return $?
}


__VAL__() {
    if [ "$1" = "s" ]; then
        __PRINTF__ ""
    else
        __PRINTF__ "`__PRINTF__ "$1" | tail -c \`expr \\\`__STR__length "$1"\\\` - 1\``"
    fi
}

__VAR__() {
    __PRINTF__ $1$2
}


__DEREF__() {
    __TEMP__var="$""$1"
    eval __PRINTF__ "\"$__TEMP__var\""
}

# __SET__ scope var val
__SET__() {
    __TEMP__count="__COUNT""$1"
    # [ -z "$(__DEREF__ $__TEMP__count)" ] && eval "$__TEMP__count=0"

    __TEMP__countval=`eval __PRINTF__ "\\$$__TEMP__count"`
    __TEMP__varname="__COUNT$__TEMP__countval$2"
    eval "$__TEMP__varname=$3"
}

# __GET__ scope var
__GET__() {
    __TEMP__count="__COUNT""$1"
    if [ -z "`__DEREF__ $__TEMP__count`" ]; then
        echo "Scope not found: $1"
        exit 1
    fi

    __TEMP__countval=`eval __PRINTF__ "\\$$__TEMP__count"`
    __TEMP__varname="__COUNT$__TEMP__countval$2"
    eval __PRINTF__ "\"\$$__TEMP__varname\""
}

# __SETCOUNT__ scope
__SETCOUNT__() {
    __TEMP__varname="__COUNT""$1"
    __TEMP__countval="`__DEREF__ $__TEMP__varname`"
    if [ -z "$__TEMP__countval" ]; then
        eval "$__TEMP__varname=1"
    else
        eval "$__TEMP__varname=`expr $__TEMP__countval + 1`"
    fi
}

__DECCOUNT__(){
    __TEMP__varname="__COUNT""$1"
    __TEMP__countval="`__DEREF__ $__TEMP__varname`"
    eval "$__TEMP__varname=`expr $__TEMP__countval - 1`"
}

# if 0, return 1. else return 0
__RETCODE__() {
    __TEMP__t=`__PRINTF__ $1 | head -c 1`
    __TEMP__v=`__VAL__ "$1"`
    if [ "$__TEMP__t" = "s" ]; then
        [ "$__TEMP__v" != "" ]; return $?
    elif [ "$__TEMP__t" = "i" ]; then
        [ "$__TEMP__v" -ne 0 ]; return $?
    else
        return 0
    fi
}

__FUNCCALL__() {
    __TEMP__t=`__PRINTF__ $1 | head -c 1`
    __TEMP__v=`__VAL__ "$1"`

    if [ "$__TEMP__t" = "f" ]; then
        # If function does not exist
        type $__TEMP__v > /dev/null
        if [ $? -ne 0 ] ; then
            echo "Runtime Error: function $__TEMP__v not found"
            return 1
        fi

        shift
        $__TEMP__v "$@"

        __RETCODE__ $__RET__
        return $?
    else
        echo "\"$__TEMP__v\" is not a function"
    fi
}


# Global temp stuff
# __GLOBAL__print="f__F__GLOBAL__print"
__SET__ __GLOBAL__ __GLOBAL__print f__F__GLOBAL__print
__SET__ __GLOBAL__ __GLOBAL__compare f__F__GLOBAL__compare
# __SET__ __GLOBAL__ __COMPARE__ f__F__COMPARE__
