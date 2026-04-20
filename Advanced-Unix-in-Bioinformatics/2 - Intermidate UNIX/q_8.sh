#!/bin/bash

# Move to data folder
cd data || exit 1


awk '{
    a=$3
    b=$4
    c=$5

    # find max
    max=a
    if (b>max) max=b
    if (c>max) max=c

    # find min
    min=a
    if (b<min) min=b
    if (c<min) min=c

    # middle = total - max - min
    middle = a + b + c - max - min

    print $1 "\t" $2 "\t" middle
}' ex1.tot > highest2.txt

echo "Showing the head of highest2.txt"
head highest2.txt