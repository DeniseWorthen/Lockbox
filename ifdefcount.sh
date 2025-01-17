#!/bin/bash

function wordfrequency() { awk 'BEGIN { FS="[^a-zA-Z]+" } { for (i=1; i<=NF; i++) { word = tolower($i); words[word]++ } } END { for (w in words) printf("%3d %s\n", words[w], w) } ' | sort -rn; }


#cat w3wavemd.F90 | wordfrequency | grep ifdef

#find . -name "*F90" | xargs cat | wordfrequency | grep ifdef

for file in src/*.F90
do
    #grep ifdef $file > count
    #printf $file
    #grep ifdef $file | wc -l>$file.count
    echo `grep ifdef $file | wc -l` $file
    #echo "$file",eval{cat $file | wordfrequency | grep ifdef}
    #cat $file | wordfrequency | grep ifdef
done
