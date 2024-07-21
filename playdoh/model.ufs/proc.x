#!/bin/bash

#cpp ${opts} $fname.F90 | awk "${awkstr}" | tr '[:upper:]' '[:lower:]'> $fname${tag}.cpp.F90

set -x

if [[ $1 == PDLIB ]]; then
    #opts="-DW3_PDLIB -DW3_NOGRB -DW3_DIST -DW3_MPI -DW3_OMPG -DW3_OMPH -DW3_PR3 -DW3_UQ -DW3_FLX0 -DW3_SEED -DW3_ST4 -DW3_STAB0 -DW3_NL1 -DW3_BT1 -DW3_DB1 -DW3_MLIM -DW3_FLD2 -DW3_TR0 -DW3_BS0 -DW3_RWND -DW3_WNX1 -DW3_WNT1 -DW3_CRX1 -DW3_CRT1 -DW3_O0 -DW3_O1 -DW3_O2 -DW3_O3 -DW3_O4 -DW3_O5 -DW3_O6 -DW3_O7 -DW3_O14 -DW3_O15 -DW3_IC0 -DW3_IS0 -DW3_REF0"
    opts="-DW3_PDLIB -DW3_MPI"
    tag=".noopts.pdlib"
else
    #opts="-DW3_NOGRB -DW3_DIST -DW3_MPI -DW3_OMPG -DW3_OMPH -DW3_PR3 -DW3_UQ -DW3_FLX0 -DW3_SEED -DW3_ST4 -DW3_STAB0 -DW3_NL1 -DW3_BT1 -DW3_DB1 -DW3_MLIM -DW3_FLD2 -DW3_TR0 -DW3_BS0 -DW3_RWND -DW3_WNX1 -DW3_WNT1 -DW3_CRX1 -DW3_CRT1 -DW3_O0 -DW3_O1 -DW3_O2 -DW3_O3 -DW3_O4 -DW3_O5 -DW3_O6 -DW3_O7 -DW3_O14 -DW3_O15 -DW3_IC0 -DW3_IS0 -DW3_REF0"
    opts="-DW3_MPI"
    tag=".noopts"
fi

awkstr='length($0) > 0 && $0 !~ /^[#] / { print $0 }'
# #trstr='[:upper:]' '[:lower:]'

targetpath=src
outputpath=ufs.cpp${tag}
#outputpath=tmp${tag}
mkdir -p $outputpath/$targetpath

files=($targetpath/w3*)
#printf 'File: %s\n' ${files[@]#$targetPath/}

for file in "${files[@]}";do                        # foreach file:
    fout=$outputpath/$file
    #echo Process "$file" "$fout"                            #   Process file
    cpp ${opts} $file | awk "${awkstr}" | tr '[:upper:]' '[:lower:]'> $fout
done

#declare -p files
#delcare -a files=([0]="./")

#files=(./w3*F90)
#[romtf 'File: %s\n' ${files[@]#$targetPath/}
