
#!/bin/bash

set -x

opts="-DW3_NOGRB -DW3_DIST -DW3_MPI -DW3_OMPG -DW3_OMPH -DW3_PR3 -DW3_UQ -DW3_FLX0 -DW3_SEED -DW3_ST4 -DW3_STAB0 -DW3_NL1 -DW3_BT1 -DW3_DB1 -DW3_MLIM -DW3_FLD2 -DW3_TR0 -DW3_BS0 -DW3_RWND -DW3_WNX1 -DW3_WNT1 -DW3_CRX1 -DW3_CRT1 -DW3_O0 -DW3_O1 -DW3_O2 -DW3_O3 -DW3_O4 -DW3_O5 -DW3_O6 -DW3_O7 -DW3_O14 -DW3_O15 -DW3_IC0 -DW3_IS0 -DW3_REF0"

#cpp ${opts} w3initmd.F90>w3initmd.nodd.cpp.F90
#cpp ${opts} w3wavemd.F90>w3wavemd.nodd.cpp.F90
#cpp ${opts} w3gridmd.F90>w3gridmd.nodd.cpp.F90
cpp ${opts} w3updtmd.F90>w3updtmd.nodd.cpp.F90

opts="-DW3_PDLIB -DW3_NOGRB -DW3_DIST -DW3_MPI -DW3_OMPG -DW3_OMPH -DW3_PR3 -DW3_UQ -DW3_FLX0 -DW3_SEED -DW3_ST4 -DW3_STAB0 -DW3_NL1 -DW3_BT1 -DW3_DB1 -DW3_MLIM -DW3_FLD2 -DW3_TR0 -DW3_BS0 -DW3_RWND -DW3_WNX1 -DW3_WNT1 -DW3_CRX1 -DW3_CRT1 -DW3_O0 -DW3_O1 -DW3_O2 -DW3_O3 -DW3_O4 -DW3_O5 -DW3_O6 -DW3_O7 -DW3_O14 -DW3_O15 -DW3_IC0 -DW3_IS0 -DW3_REF0"

#cpp ${opts} w3srcemd.F90>w3srcemd.cpp..F90
#cpp ${opts} w3src4md.F90>w3src4md.cpp.F90
#cpp ${opts} w3gridmd.F90>w3gridmd.cpp.F90
#cpp ${opts} w3gdatmd.F90>w3gdatmd.cpp.F90
#cpp ${opts} w3pro3md.F90>w3pro3md.cpp.F90

#cpp ${opts} w3iorsmd.F90>w3iorsmd.cpp.F90
#cpp ${opts} w3wavemd.F90>w3wavemd.cpp.F90
#cpp ${opts} w3initmd.F90>w3initmd.cpp.F90


#cpp ${opts} w3adatmd.F90>w3adatmd.cpp.F90
#cpp ${opts} pdlib_field_vec.F90>pdlib.cpp.F90
#cpp ${opts} w3profsmd_pdlib.F90>w3profsmd_pdlib.cpp.F90

cpp ${opts} w3updtmd.F90>w3updtmd.cpp.F90
#cpp ${opts} w3parall.F90>w3parall.cpp.F90
