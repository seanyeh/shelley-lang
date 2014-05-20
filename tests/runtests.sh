#!/bin/bash

declare -A TESTS=(
[sum]=9
[simple]=13
[simple2]=3
[indent]=1
[white]="     a"
[fac]="24"
)

failed=0
passed=0
failed_tests=""
for key in "${!TESTS[@]}"; do 
    testfile="tests/$key.shly"
    
    expected_output=${TESTS[$key]}
    output=$(cat $testfile | ./main.native | sh)

    if [ $? -ne 0 ] || [ "$output" != "$expected_output" ]; then
        ((failed++))
        failed_tests="$key,$failed_tests"
    else
        ((passed++))
    fi
done

echo "Passed: $passed, Failed: $failed"
[ $failed -ne 0 ] && echo "Failed tests: $failed_tests"

echo ""
