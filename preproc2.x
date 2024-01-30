
#!/bin/bash

set -x
fname=$2
fname=${fname%????}
#fname = ${{$2}::-4}

if [[ $1 == PDLIB ]]; then

  opts="-DW3_PDLIB -DW3_NOGRB -DW3_DIST -DW3_MPI -DW3_OMPG -DW3_OMPH -DW3_PR3 -DW3_UQ -DW3_FLX0 -DW3_SEED -DW3_ST4 -DW3_STAB0 -DW3_NL1 -DW3_BT1 -DW3_DB1 -DW3_MLIM -DW3_FLD2 -DW3_TR0 -DW3_BS0 -DW3_RWND -DW3_WNX1 -DW3_WNT1 -DW3_CRX1 -DW3_CRT1 -DW3_O0 -DW3_O1 -DW3_O2 -DW3_O3 -DW3_O4 -DW3_O5 -DW3_O6 -DW3_O7 -DW3_O14 -DW3_O15 -DW3_IC0 -DW3_IS0 -DW3_REF0"
  tag=""
else
  opts="-DW3_NOGRB -DW3_DIST -DW3_MPI -DW3_OMPG -DW3_OMPH -DW3_PR3 -DW3_UQ -DW3_FLX0 -DW3_SEED -DW3_ST4 -DW3_STAB0 -DW3_NL1 -DW3_BT1 -DW3_DB1 -DW3_MLIM -DW3_FLD2 -DW3_TR0 -DW3_BS0 -DW3_RWND -DW3_WNX1 -DW3_WNT1 -DW3_CRX1 -DW3_CRT1 -DW3_O0 -DW3_O1 -DW3_O2 -DW3_O3 -DW3_O4 -DW3_O5 -DW3_O6 -DW3_O7 -DW3_O14 -DW3_O15 -DW3_IC0 -DW3_IS0 -DW3_REF0"
  tag=".nodd"
fi

awkstr='length($0) > 0 && $0 !~ /^[#] / { print $0 } '
cpp ${opts} $fname.F90 | awk "${awkstr}" > $fname${tag}.cpp.F90

#emacs -l ~/.emacs2 -nw $fname${tag}.cpp.F90
