#!/bin/bash
for f in tests/*.test; do echo "File: $f"; cat $f | ./main.native; done

