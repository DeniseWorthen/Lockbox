#!/bin/sh
#This routine will make the mod_defs that are needed as input in
#ufs-apps using WW3
#   UFSMODELDIR is the location of your clone of ufs-weather-model
#   OUTDIR is the location where the mod_def.* outputs will end up from this script
#   SRCDIR is the location of the ww3_grid.inp files
#   WORKDIR is the working directory for log files, e

#export UFSMODELDIR=<pathto>/ufs-weather-model
#export OUTDIR=<pathtoinputdata>/WW3_input_data
#export SRCDIR=<pathtoinputdata>/WW3_input_data/createmoddefs
#export WORKDIR=<pathtoworkdir>/workdir
export MACHINE=hera

export WORKDIR=/scratch1/NCEPDEV/climate/Jessica.Meixner/PR_WW3/syncdev2ufs/work2
export SRCDIR=/scratch1/NCEPDEV/climate/Jessica.Meixner/PR_WW3/syncdev2ufs/WW3_input_02/createmoddefs
export OUTDIR=/scratch1/NCEPDEV/climate/Jessica.Meixner/PR_WW3/syncdev2ufs/WW3_input_02
export UFSMODELDIR=/scratch1/NCEPDEV/climate/Jessica.Meixner/PR_WW3/syncdev2ufs/ufs-01

###################################
#  Set up                         #
###################################
set +x

export target=${MACHINE}.intel

module use $UFSMODELDIR/modulefiles
module load ufs_$target
module list
set -x

########################################
#  Create Work and Build Dirs wmesmf   #
########################################

if [[ ! -d ${WORKDIR} ]] ; then
  echo "Make WORKDIR: $WORKDIR"
  mkdir -p $WORKDIR
fi

export WW3_DIR=$UFSMODELDIR/WW3


switchlist='multi_esmf meshcap_pdlib meshcap' # meshcap_pdlib_IC4'

for switch in ${switchlist}
do

  export SWITCHFILE="${WW3_DIR}/model/bin/switch_${switch}"

  path_build=$WORKDIR/build_${switch}
  mkdir -p $path_build
  cd $path_build

  ########################################
  #  Build ww3_grid exe for switch       #
  ########################################

  echo $(cat ${SWITCHFILE}) > ${path_build}/tempswitch

  sed -e "s/DIST/SHRD/g"\
      -e "s/OMPG / /g"\
      -e "s/OMPH / /g"\
      -e "s/MPIT / /g"\
      -e "s/MPI / /g"\
      -e "s/B4B / /g"\
      -e "s/PIO / /g"\
      -e "s/PDLIB / /g"\
      -e "s/SCOTCH / /g"\
      -e "s/METIS / /g"\
      -e "s/NOGRB/NCEP2/g"\
        ${path_build}/tempswitch > ${path_build}/switch
  rm ${path_build}/tempswitch

  echo "Switch file is $path_build/switch with switches:"
  cat $path_build/switch

  cmake $WW3_DIR -DSWITCH=$path_build/switch -DCMAKE_INSTALL_PREFIX=install
  rc=$?
  if [[ $rc -ne 0 ]] ; then
    echo "Fatal error in cmake."
    exit $rc
  fi
  make -j 8
  rc=$?
  if [[ $rc -ne 0 ]] ; then
    echo "Fatal error in make for switch=${switch}."
    exit $rc
  fi
  make install
  if [[ $rc -ne 0 ]] ; then
    echo "Fatal error in make install."
    exit $rc
  fi

  WW3_EXEDIR=$path_build/install/bin

  case "${switch}" in
    "multi_esmf")
        export grids='glo_1deg glo_15mxt gnh_10m gsh_15m aoc_9km pointsatmw'
        ;;
    "meshcap")
        export grids='mx025lite mx025 mx050 mx100 gwes_30m natl_6m points glo_900 glo_025'
        ;;
    "meshcap_pdlib")
        export grids='exp.global_270k uglo_900km'
        ;;
    "meshcap_pdlib_IC4")
        export grids='ic4.exp.global_270k'
        ;;
    *)
        echo "FATAL ERROR: Unrecognized CASE ${switch}, ABORT!"
        exit 1
        ;;
  esac

  ###################################
  #  Make mod_def files switch      #
  ###################################


   echo 'Removing ST4TABUHF2.bin mod_def.*'
   rm -rf ST4TABUHF2.bin mod_def.*


   if [[ ! -d ${OUTDIR} ]] ; then
     echo "Make OUTDIR: $OUTDIR"
     mkdir -p $OUTDIR
   fi

   for grid in ${grids}
   do
     WORKDIRg=${WORKDIR}/${switch}/${grid}
     if [[ ! -d ${WORKDIRg} ]] ; then
       echo "Make WORKDIR: $WORKDIRg"
       mkdir -p ${WORKDIRg}
     fi

     cd ${WORKDIRg}
     echo 'Clean up work directory'
     rm -rf ST4TABUHF2.bin mod_def.*
     if [ -f ${WORKDIRg}/ww3_${grid}.out ]; then
       rm ${WORKDIRg}/ww3_${grid}.out
     fi

     if [ -f ${SRCDIR}/ww3_grid.inp.${grid} ]
     then
       echo ' '
       echo " Copying WW3 grid input file ww3_grid.${grid} "
       echo ' '
       cp ${SRCDIR}/ww3_grid.inp.${grid} ww3_grid.inp
       if [ ${grid}=='exp.global_270k' ] || [  ${grid}=='ic4.exp.global_270k'] ; then
         cp ${SRCDIR}/global_270k_with_mom6mask.msh global_270k_with_mom6mask.msh
       fi
       if [ ${grid}=='uglo_900km' ] ; then
         cp ${SRCDIR}/uglo_900km.msh uglo_900km.msh
       fi

       echo "Executing ww3_grid, see grid output in ww3_${grid}.out"
       ${WW3_EXEDIR}/ww3_grid > ${WORKDIRg}/ww3_${grid}.out
       mv mod_def.ww3 ${OUTDIR}/mod_def.${grid}
     else
       echo ' '
       echo " WW3 grid input file ww3_grid.inp.${grid} not found! "
       echo ' ****************** ABORTING *********************'
       echo ' '
       exit
     fi
   done
done

echo "For the final directory, make sure you include all netcdf input files and wind.natl_6m"
