#!/bin/bash

mkdir -p tmp
for f in raw_lkp/lkp_child_bmi*.csv; do
    echo "Processing $f..."

    suffix=$(basename "$f" .csv | sed 's/lkp_child_bmi//')
    tmp_file="tmp/tmp_${suffix}.csv"
    output_file="dedupe_lkp/dedupe_${suffix}.csv"

    # Format the input CSV
    ./format.sh "$f" "$tmp_file"

    # Replace placeholders in the SQL template and run DuckDB
    # Note input and output might have slashes in them, so use
    # | as a delimiter in sed (avoids needing to escape things)
    sed -e "s|{{input}}|$tmp_file|g" \
        -e "s|{{output}}|$output_file|g" \
        dedupe.sql | duckdb

    echo "    Finished $f → $output_file"
done

# Combine into one file
head -n+1 dedupe_lkp/dedupe_02.csv > lkp_bmi_child.csv
for f in dedupe_lkp/*.csv; do
    tail -n+2 "$f" >> lkp_bmi_child.csv
done

rm -rf tmp/
