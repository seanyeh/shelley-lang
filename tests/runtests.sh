#!/bin/bash


declare -A TESTS=(
[sum]=9
[simple]=13
[simple2]=3
[indent]=101
[white]="     a"
[fac]=24
[logical]=1
[fib]=21
[funcs]=12
[exprfunc]=15
[hanoi]="12\n13\n23\n12\n31\n32\n12\n13\n23\n21\n31\n23\n12\n13\n23"
[compare]="1\n0\n0\n0\n1\n0\n1\n1"
)

run_tests(){
    shell="$1"
    echo "Testing shell: $shell"
    failed=0
    passed=0
    failed_tests=""
    for key in "${!TESTS[@]}"; do 
        testfile="tests/$key.shly"

        expected_output=$(printf "${TESTS[$key]}")
        output=$(cat $testfile | ./main.native | $shell)

        if [ $? -ne 0 ] || [ "$output" != "$expected_output" ]; then
            echo "Test $key:"
            echo "Output: $output, Expected: $expected_output"
            ((failed++))
            failed_tests="$key,$failed_tests"
        else
            ((passed++))
        fi
    done

    echo "Passed: $passed, Failed: $failed"
    [ $failed -ne 0 ] && echo "Failed tests: $failed_tests"

    echo ""
}


run_tests "dash"
run_tests "/usr/heirloom/bin/sh"
