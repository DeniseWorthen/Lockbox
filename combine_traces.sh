#!/bin/bash
set -eux

src=test

# Parse ufs.configure and calculate total PEs for each component
declare -A pe_counts
while IFS= read -r line; do
    if [[ $line =~ ^([A-Z]+)_petlist_bounds:[[:space:]]+([0-9]+)[[:space:]]+([0-9]+) ]]; then
        component="${BASH_REMATCH[1]}"
        start="${BASH_REMATCH[2]}"
        end="${BASH_REMATCH[3]}"
        total=$((end - start + 1))
        pe_counts[$component]=$total
    fi
done < "${src}/ufs.configure"

# Build filename with PE counts
FILE="${src}"
for comp in ATM MED ICE OCN; do
    if [[ -n "${pe_counts[$comp]:-}" ]]; then
        FILE="${FILE}.$(echo $comp | tr '[:upper:]' '[:lower:]')${pe_counts[$comp]}"
    fi
done
FILE="${FILE}.trace"

if [ -f "$FILE" ]; then
    rm "$FILE"
fi

cat ${src}/ufs_trace_*.trace > all.traces
sed -i '$ s/.$//' all.traces
echo '[' > out.trace
cat all.traces >> out.trace
echo ']' >> out.trace
mv out.trace $FILE
