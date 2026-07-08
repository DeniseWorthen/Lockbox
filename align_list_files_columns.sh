#!/usr/bin/env bash
set -euo pipefail

usage() {
  echo "Usage: $0 [file_or_dir ...]"
  echo "If no arguments are provided, all files under tests/tests are processed."
  echo "Pre-step: removes leading standalone 'export' and left-aligns those lines."
  echo "Then aligns LIST_FILES continuation lines to the first text column after LIST_FILES=\"."
}

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [[ $# -eq 0 ]]; then
  set -- "$script_dir/tests"
fi

files=()
for input in "$@"; do
  if [[ -f "$input" ]]; then
    files+=("$input")
  elif [[ -d "$input" ]]; then
    while IFS= read -r -d '' f; do
      files+=("$f")
    done < <(find "$input" -type f -print0)
  else
    echo "Skipping missing path: $input" >&2
  fi
done

if [[ ${#files[@]} -eq 0 ]]; then
  echo "No files to process." >&2
  exit 1
fi

for file in "${files[@]}"; do

  tmp_file="${file}.tmp.$$"

  awk '
    BEGIN {
      in_list = 0
      align_prefix = ""
    }

    {
      line = $0

      # Pre-step: remove a leading standalone export token and bring
      # the remaining assignment/content to column 1.
      if (line ~ /^[[:space:]]*export[[:space:]]+/) {
        sub(/^[[:space:]]*export[[:space:]]+/, "", line)
        sub(/^[[:space:]]+/, "", line)
      }

      if (!in_list) {
        print line

        if (line ~ /LIST_FILES="/ && line ~ /\\[[:space:]]*$/) {
          q = index(line, "\"")
          if (q > 0) {
            rest = substr(line, q + 1)
            m = match(rest, /[^[:space:]]/)
            if (m > 0) {
              text_col = q + m
              align_prefix = sprintf("%*s", text_col - 1, "")
              in_list = 1
            }
          }
        }

        next
      }

      sub(/^[[:space:]]+/, "", line)
      print align_prefix line

      if (line ~ /"[[:space:]]*$/ && line !~ /\\[[:space:]]*$/) {
        in_list = 0
      }
    }
  ' "$file" > "$tmp_file"

  mv "$tmp_file" "$file"
  echo "Aligned LIST_FILES columns in: $file"
done
