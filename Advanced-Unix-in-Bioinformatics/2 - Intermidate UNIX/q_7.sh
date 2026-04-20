#!/bin/bash

# Move to data folder
cd data || exit 1

awk '{
    max = $3
    if ($4 > max) max = $4
    if ($5 > max) max = $5
    print $1 "\t" $2 "\t" max
}' ex1.tot > highest.txt

echo "Showing the head of highest.txt"
head highest.txt