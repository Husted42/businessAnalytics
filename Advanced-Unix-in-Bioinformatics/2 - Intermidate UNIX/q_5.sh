#!/bin/bash

# Extract (cut) SwissProt ID and 3nd numerical data (column 1 and 5) from ex1.tot. Put results into a file ex1.res.

cd data || exit 1

# Find the 3 SwissProt ID's in ex1.res which have the largest number(s) in column 2, i.e. the top 3 entries. Display only the ID's.
echo "Cutting SwissProt ID's in ex1.res"
cut -f1,5 ex1.tot > ex1.res
echo "19 - Top 3 largest number SwissProt ID"
sort -k2,2nr ex1.res | head -n 3 | cut -f1
echo

# Find the lines (using grep) in orphans.sp which contain a GenBank accession number. There are 85, verify this. Note: An accession number is one or two capital letters and looks like this 'AB000114.CDS.1', i.e. Some letters followed by some numbers. The .CDS. part is kind of optional.

count=$(grep -E 'CDS' orphans.sp | wc -l)
echo "20 - Orphans with GenBank accession number: $count"
echo

# How many human genes with SwissProt IDs in orphans.sp exist ? How many of those are hypothetical ? (11)
cnt_id=$(grep -E '_HUMAN' orphans.sp | wc -l)
cnt_hypothetical=$(grep -E '_HUMAN' orphans.sp | grep -E 'HYPOTHETICAL' |  wc -l)
echo "21 - There are $cnt_id orphans with SwissProt ID and $cnt_hypothetical are hypothetical"
echo

# How many genes belong to the rat, and how many of those are precursors ? (9) Note: A Swissprot ID looks like 'PARG_HUMAN' or 'TF1A_MOUSE', with the gene being before the underscore and the organism after the underscore.
cnt_precursors=$(grep -E '_RAT' orphans.sp | grep -E 'PRECURSOR' |  wc -l)
echo "22 - No. of rat precursors : $cnt_precursors"
echo

# From the file ex1.res find the lines with positive numbers and put then into ex1.pos. The lines with negative number go into ex1.neg.
awk '$2 > 0 {print > "ex1.pos"} $2 < 0 {print > "ex1.neg"}' ex1.res
echo "23 - "
echo "Head of positive"
head ex1.pos
echo
echo "Head of negative"
head ex1.neg
