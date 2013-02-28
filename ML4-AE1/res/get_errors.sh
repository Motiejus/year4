#!/bin/bash -e

awk 'BEGIN {FS = "," } ; {print $1" "$2" "$3" "$4" "$5" "$6"-"$7 };' \
    courseworkdata.csv | sed '1d' > courseworkdata2.csv

for i in resultdata-*; do
    sed -e '1d; s/,/ /g' $i > resultdata2-${i#resultdata-}
done

awk '{print $6}' courseworkdata2.csv | sort | uniq > subjects.txt

for i in resultdata2-*; do
    awk '{print $2" "$3}' $i > short_$i;
done
rm resultdata2-*

awk '{print $6" "$2" "$3}' courseworkdata2.csv > short_courseworkdata.csv

for i in short_resultdata*; do
    paste short_courseworkdata.csv $i > shrinkdata-$i
    rm $i
done

for f in shrinkdata-*; do
    awk '{print $1" "sqrt(($1-$3)*($1-$3)+($2-$4)*($2-$4))}' $f \
        > error${f#shrinkdata-short_resultdata2}
done
rm shrinkdata-*

{
    for s in `cat subjects.txt`; do
        for i in error-*; do
            echo -n $s $i " "
            grep $s $i | awk '{a+=$2} END{print a/NR}'
        done
    done
} > means.txt
rm error-*

grep error-0.05-10 means.txt > means_a.txt
grep -- -0.1-0.9-0.001.csv means.txt > means_b.txt 
rm means.txt

while read line; do
    a=$(echo -n "$line" | cut -f5 -d"-")
    subj=$(echo -n "$line" | cut -f1 -d" ")
    val=$(echo $line | cut -f3 -d" ")
    echo $a $subj $val
done < means_a.txt > final_a.txt
rm means_a.txt

while read line; do
    a=$(echo -n "$line" | cut -f4 -d"-")
    subj=$(echo -n "$line" | cut -f1 -d" ")
    val=$(echo $line | cut -f3 -d" ")
    echo $a $subj $val
done < means_b.txt > final_b.txt
rm means_b.txt

for s in `cat subjects.txt`; do
    grep $s final_a.txt | sort -n > subj_a${s/Subject/}.txt;
    grep $s final_b.txt | sort -n > subj_b${s/Subject/}.txt;
done
