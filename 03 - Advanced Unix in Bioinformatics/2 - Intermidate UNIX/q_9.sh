#!/bin/bash

cd data || exit 1

path="${1:-.}"
total=0

if [ -f "$path" ]; then
  total=$(wc -l < "$path")
elif [ -d "$path" ]; then
  for file in "$path"/*; do
    [ -f "$file" ] || continue
    lines=$(wc -l < "$file")
    total=$((total + lines))
  done
else
  echo "Error: '$path' is not a file or directory" >&2
  exit 1
fi

echo "Total number of lines: $total"
