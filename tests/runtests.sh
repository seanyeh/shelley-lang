#!/bin/bash
for f in tests/*.test; do
    echo ""
    echo "File: $f";
    cat $f
    echo "--------"
    cat $f | ./main.native;
done

