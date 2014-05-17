__GLOBAL__print() {
    for var in "$@"
    do
        printf "$var "
    done
    printf "\n"
}

