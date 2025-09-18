#!/bin/bash

INPUT_FILE="$1"
OUTPUT_FILE="$2"

awk -F',' 'BEGIN{
    OFS=","
}
NR == 1 {
    print; next
}
{
    for (i = 1; i <= NF; i++) {
        if ($i == "") continue
        if ($i ~ /^[+-]?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?$/) {
            $i = sprintf("%.3f", $i)
        }
    }
    print
}' "$INPUT_FILE" > "$OUTPUT_FILE"
