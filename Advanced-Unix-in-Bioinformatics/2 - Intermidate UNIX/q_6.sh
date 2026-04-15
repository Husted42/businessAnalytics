#!/bin/bash

# Move to data folder
cd data || exit 1

# Temporary files
pos_tmp="pos_tmp.$$"
neg_tmp="neg_tmp.$$"

# Extract positive and negative numbers (any column)
awk '{
    for(i=1;i<=NF;i++){
        if($i > 0) print $i >> "'$pos_tmp'"
        if($i < 0) print $i >> "'$neg_tmp'"
    }
}' ex1.dat

# Add date and description to final files
{
    echo "Date: $(date)"
    echo "Positive numbers from ex1.dat"
    cat "$pos_tmp"
} > ex1.pos2

{
    echo "Date: $(date)"
    echo "Negative numbers from ex1.dat"
    cat "$neg_tmp"
} > ex1.neg2

# Cleanup
rm -f "$pos_tmp" "$neg_tmp"


echo "Positive numbers:"
head ex1.pos2

echo
echo "Negative numbers"
head ex1.neg2