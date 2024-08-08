#!/bin/sh
#This routine will make the mod_defs that are needed as input in
#ufs-apps using WW3
#   UFSMODELDIR is the location of your clone of ufs-weather-model
#   OUTDIR is the location where the mod_def.* outputs will end up from this script
#   SRCDIR is the location of the ww3_grid.inp files
#   WORKDIR is the working directory for log files, e

#export UFSMODELDIR=<path-to-your>/ufs-weather-model
#export OUTDIR=<path-to-your>/outdir
#export SRCDIR=/scratch1/NCEPDEV/nems/emc.nemspara/RT/NEMSfv3gfs/input-data-20220414/WW3_input_data_20220418/createmoddefs
#export WORKDIR=<path-to-your>/workdir

export UFSMODELDIR=/glade/work/worthen/WaveIn_unstr5/ufs_wave
export OUTDIR=/glade/work/worthen/WaveIn_unstr5/moddefs
export SRCDIR=/glade/work/worthen/WaveIn_unstr5
export WORKDIR=/glade/work/worthen/WaveIn_unstr5/workdir

###################################
#  Set up                         #
###################################
set +x
#source ./machine-setup.sh > /dev/null 2>&1

#if [ $target = hera ]; then export target=hera.intel ; fi
#if [ $target = orion ]; then export target=orion.intel ; fi
#if [ $target = stampede ]; then export target=stampede.intel ; fi
#if [ $target = gaea ]; then export target=gaea.intel ; fi
#if [ $target = jet ]; then export target=jet.intel ; fi

export target=derecho.intel
module use $UFSMODELDIR/modulefiles
module load ufs_$target
module list
set -x
#######################################
#  Create Work and Build Dirs mesh    #
#######################################

if [[ ! -d ${WORKDIR} ]] ; then
  echo "Make WORKDIR: $WORKDIR"
  mkdir -p $WORKDIR
fi

export WW3_DIR=$UFSMODELDIR/WW3
export SWITCHFILE="${WW3_DIR}/model/bin/switch_meshcap"

path_build=$WORKDIR/build_mesh
mkdir -p $path_build
cd $path_build


########################################
#  Build ww3_grid exe meshcap switch   #
########################################

echo $(cat ${SWITCHFILE}) > ${path_build}/tempswitch

sed -e "s/DIST/SHRD/g"\
    -e "s/OMPG / /g"\
    -e "s/OMPH / /g"\
    -e "s/MPIT / /g"\
    -e "s/MPI / /g"\
    -e "s/B4B / /g"\
    -e "s/PDLIB / /g"\
    -e "s/NOGRB/NCEP2/g"\
       ${path_build}/tempswitch > ${path_build}/switch
rm ${path_build}/tempswitch

echo "Switch file is $path_build/switch with switches:"
cat $path_build/switch

#cmake $WW3_DIR -DSWITCH=$path_build/switch -DCMAKE_INSTALL_PREFIX=install -DCMAKE_BUILD_TYPE=Debug
cmake $WW3_DIR -DSWITCH=$path_build/switch -DCMAKE_INSTALL_PREFIX=install
rc=$?
if [[ $rc -ne 0 ]] ; then
  echo "Fatal error in cmake."
  exit $rc
fi
make -j 8
rc=$?
if [[ $rc -ne 0 ]] ; then
  echo "Fatal error in make."
  exit $rc
fi
make install
if [[ $rc -ne 0 ]] ; then
  echo "Fatal error in make install."
  exit $rc
fi

WW3_EXEDIR=$path_build/install/bin

###################################
#  Make mod_def files mesh        #
###################################

if [[ ! -d ${WORKDIR} ]] ; then
  echo "Make WORKDIR: $WORKDIR"
  mkdir -p $WORKDIR
fi

cd $WORKDIR

# mesh wave model grids
#grids='mx025lite mx025 mx050 mx100 gwes_30m natl_6m points'
#grids='with_land_CardDeck no_land_CardDeck no_land_Domain_Decomposition with_land_Domain_Decomposition'
#grids='trip1deg trip1deg_noland'
#grids='trip1deg_noland.mod'
#grids='trip1deg_noland_carddeck.mod'
#grids='trip1degmask_dd trip1degmask_cd'
#grids='trip10minmask_dd trip15minmask_dd'
#grids='global_270k global_350k global_500k'
#grids='global_270k'
#grids='exp.global_350k imp.global_350k'
#grids='exp.global_270k imp.global_270k'
#grids='exp.trip1degmask imp.trip1degmask'
#grids='cd.trip1degmask'
#grids='mx100 trip1degmask_dd trip1degmask_cd'
#grids='exp.trip1degmask'
#grids='exp.global_270k'
#grids='loice.exp.trip1degmask'
#grids='hiice.exp.trip1degmask'
#grids='exp.flckF.trip1degmask'
#grids='exp.flchF.trip1degmask exp.flF.trip1degmask'
#note these were created after updating WW3 from the ungridded stokes drift to the wise pr
#grids='imp.wise.trip1degmask exp.wise.trip1degmask'
#grids='exp.wise.global_500k'
# must change switch file for this, replace SEED with LN1
#grids='exp.noseed.trip1degmask'

#grids='exp.wise.tnx1v4.100k'
#grids='exp.wise.tnx1v4p.100k'
#grids='exp.wise.tnx1v4dry.100k'

#from baseline w/ mods to the number of freqs, timesteps, outs
#grids='mod.glo_1deg'

#grids='ic4.mx100'

#grids='exp.wise.trip1degmask'
grids='ic4.exp.wise.trip1degmask'

echo 'Removing ST4TABUHF2.bin mod_def.*'
rm -rf ST4TABUHF2.bin mod_def.*

echo "$OUTDIR"

if [[ ! -d ${OUTDIR} ]] ; then
  echo "Make OUTDIR: $OUTDIR"
  mkdir -p $OUTDIR
fi

for grid in ${grids}
do
  if [ -f ${SRCDIR}/ww3_grid.inp.${grid} ]
  then
    echo ' '
    echo " Copying WW3 grid input file ww3_grid.${grid} "
    echo ' '
    cp ${SRCDIR}/ww3_grid.inp.${grid} ww3_grid.inp

    if [ -f ${WORKDIR}/ww3_${grid}.out ]; then
      rm ${WORKDIR}/ww3_${grid}.out
    fi
    echo "Executing ww3_grid, see grid output in ww3_${grid}.out"
    ${WW3_EXEDIR}/ww3_grid > ${WORKDIR}/ww3_${grid}.out
    mv mod_def.ww3 ${OUTDIR}/mod_def.${grid}
    mv scrip.nc ${OUTDIR}/${grid}.SCRIP.nc
    rm -f ww3_grid.inp ST4TABUHF2.bin
  else
    echo ' '
    echo " WW3 grid input file ww3_grid.inp.${grid} not found! "
    echo ' ****************** ABORTING *********************'
    echo ' '
    exit
  fi
done
