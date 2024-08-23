!> @file
!> @brief contains module w3wavemd.
!>
!> @author h. l. tolman  @date 22-mar-2021
!>
!/ ------------------------------------------------------------------- /
!/    preprocessing macros
!/
!/                  +-----------------------------------+
!/                  | wavewatch iii           noaa/ncep |
!/                  |        t. j. campbell, nrl        |
!/                  |                               cpp |
!/                  | last update :         26-oct-2015 |
!/                  +-----------------------------------+
!/
!/    10-dec-2014 : origination.                     ( version 5.04 )
!/    26-oct-2015 : replace c style comments with fortran
!/                  style comments.                  ( version 5.09 )
!/
!/ 1. purpose :
!/
!/    define preprocessor macros for ww3 ftn source code.
!/
!/ 2. method :
!/
!/ 3. parameters :
!/
!/ 4. subroutines used :
!/
!/ 5. called by :
!/
!/ 6. error messages :
!/
!/ 7. remarks :
!/
!/    this file uses fortran style comments, and hence, can only be
!/    included in the fortran (ftn) source files.  the fortran style
!/    comments are used because not all fortran pre-processors recognize
!/    the c style comments.
!/
!/    the "src/w3macros.h" and 36 macros are defined by cpp.
!/
!/ 8. structure :
!/
!/    see source code.
!/
!/ 9. source code :
!/
!/ ------------------------------------------------------------------- /
!/
!/ macros to wrap checking allocate/deallocate status
!/
!/
!/ end of w3macros.h ------------------------------------------------- /
!/
!/ ------------------------------------------------------------------- /
!>
!> @brief contains wave model subroutine, w3wave.
!>
!> @author h. l. tolman  @date 22-mar-2021
!>
module w3wavemd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         13-sep-2022 |
  !/                  +-----------------------------------+
  !/
  !/    04-feb-2000 : origination.                        ( version 2.00 )
  !/                  for upgrades see subroutines.
  !/    14-feb-2000 : exact-nl added.                     ( version 2.01 )
  !/    05-jan-2001 : bug fix to allow model to run       ( version 2.05 )
  !/                  without output.
  !/    24-jan-2001 : flat grid version.                  ( version 2.06 )
  !/    09-feb-2001 : third propagation scheme added.     ( version 2.08 )
  !/    23-feb-2001 : check for barrier after source
  !/                  terms added ( w3nmin ).     ( delayed version 2.07 )
  !/    16-mar-2001 : fourth propagation scheme added.    ( version 2.09 )
  !/    30-mar-2001 : sub-grid obstacles added.           ( version 2.10 )
  !/    23-may-2001 : clean up and bug fixes.             ( version 2.11 )
  !/    10-dec-2001 : sub-grid obstacles for uq schemes.  ( version 2.14 )
  !/    11-jan-2002 : sub-grid ice.                       ( version 2.15 )
  !/    24-jan-2002 : zero time step dor data ass.        ( version 2.17 )
  !/    18-feb-2002 : point output diagnostics added.     ( version 2.18 )
  !/    30-apr-2002 : add field output types 17-18.       ( version 2.20 )
  !/    09-may-2002 : switch clean up.                    ( version 2.21 )
  !/    13-nov-2002 : add stress vector.                  ( version 3.00 )
  !/    26-dec-2002 : moving grid version.                ( version 3.02 )
  !/    01-aug-2003 : moving grid gse correction.         ( version 3.03 )
  !/    20-aug-2003 : output server options added.        ( version 3.04 )
  !/    07-oct-2003 : output options for nn training.     ( version 3.05 )
  !/    29-dec-2004 : multiple grid version.              ( version 3.06 )
  !/                  w3init, w3mpii-o and wwver moved to w3initmd.ftn
  !/    04-feb-2005 : add stamp to par list of w3wave.    ( version 3.07 )
  !/    04-may-2005 : change to mpi_comm_wave.            ( version 3.07 )
  !/    28-jun-2005 : adding map recalc for w3ulev call.  ( version 3.07 )
  !/    07-sep-2005 : updated boundary conditions.        ( version 3.08 )
  !/                  fix nrqsg1/2 = 0 array bound issue.
  !/    13-jun-2006 : split store in g/sstore             ( version 3.09 )
  !/    26-jun-2006 : add output type 6.                  ( version 3.09 )
  !/    04-jul-2006 : consolidate stress arrays.          ( version 3.09 )
  !/    18-oct-2006 : partitioned spectral data output.   ( version 3.10 )
  !/    02-feb-2007 : add flagst test.                    ( version 3.10 )
  !/    02-apr-2007 : add partitioned field data.         ( version 3.11 )
  !/    07-may-2007 : bug fix skip_o treatment.           ( version 3.11 )
  !/    17-may-2007 : adding ntproc/naproc separation.    ( version 3.11 )
  !/    08-oct-2007 : adding as cx-y to w3srce par. list. ( version 3.13 )
  !/    22-feb-2008 : initialize vgx-y properly.          ( version 3.13 )
  !/    10-apr-2008 : bug fix writing log file (mpi).     ( version 3.13 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/    30-oct-2009 : implement run-time grid selection.  ( version 3.14 )
  !/                  (w. e. rogers & t. j. campbell, nrl)
  !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
  !/                  (w. e. rogers & t. j. campbell, nrl)
  !/    29-mar-2010 : adding coupling, ice in w3srce.     ( version 3.14_shom )
  !/    16-may-2010 : adding transparencies in w3scre     ( version 3.14_shom )
  !/    23-jun-2011 : movable bed bottom friction bt4     ( version 4.04 )
  !/    03-nov-2011 : shoreline reflection on unst. grids ( version 4.04 )
  !/    02-jul-2011 : update for palm coupling            ( version 4.07 )
  !/    06-mar-2012 : initializing itest as needed.       ( version 4.07 )
  !/    02-jul-2012 : update for palm coupling            ( version 4.07 )
  !/    02-sep-2012 : clean up of open bc for ug grids    ( version 4.08 )
  !/    03-sep-2012 : fix format 902.                     ( version 4.10 )
  !/    07-dec-2012 : wrap w3srce with tmpn to limit warn ( version 4.of )
  !/    10-dec-2012 : modify field output mpi for new     ( version 4.of )
  !/                  structure and smaller memory footprint.
  !/    12-dec-2012 : adding smc grid.  jg_li             ( version 4.08 )
  !/    26-dec-2012 : move field init. to w3gath.         ( version 4.of )
  !/    16-sep-2013 : add arctic part for smc grid.       ( version 4.11 )
  !/    11-nov-2013 : smc and rotated grid incorporated in the main
  !/                  trunk                               ( version 4.13 )
  !/    14-nov-2013 : remove orphaned work arrays.        ( version 4.13 )
  !/    27-nov-2013 : fixes for openmp versions.          ( version 4.15 )
  !/    23-may-2014 : adding ice fluxes to w3srce         ( version 5.01 )
  !/    27-may-2014 : move to ompg/x switch.              ( version 5.02 )
  !/    24-apr-2015 : adding oasis coupling calls         ( version 5.07 )
  !/                  (m. accensi & f. ardhuin, ifremer)
  !/    27-aug-2015 : update for iceh, icef               ( version 5.08 )
  !/    14-sep-2018 : remove palm implementation          ( version 6.06 )
  !/    15-sep-2020 : bugfix field allocation. remove     ( version 7.11 )
  !/                  defunct ompx switches.
  !/    22-mar-2021 : update taua, rhoa                   ( version 7.13 )
  !/    06-may-2021 : use arctc and smctype options. jgli ( version 7.13 )
  !/    19-jul-2021 : momentum and air density support    ( version 7.14 )
  !/    11-nov-2021 : remove xyb since it is obsolete     ( version 7.xx )
  !/    13-sep-2022 : add omp for w3nmin loops. hide
  !/                  w3nmin in w3_debugrun for scaling.  ( version 7.xx )
  !/
  !/    copyright 2009-2014 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !  2. variables and types :
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3wave    subr. public   actual wave model.
  !      w3gath    subr. public   data transpose before propagation.
  !      w3scat    subr. public   data transpose after propagation.
  !      w3nmin    subr. public   calculate minimum number of sea
  !                               points per processor.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      w3setx    subr. w3xdatmd point to data structure.
  !
  !      w3ucur    subr. w3updtmd interpolate current fields in time.
  !      w3uwnd    subr. w3updtmd interpolate wind fields in time.
  !      w3uini    subr. w3updtmd update initial conditions if init.
  !                               with initial wind conditions.
  !      w3ubpt    subr. w3updtmd update boundary points.
  !      w3uice    subr. w3updtmd update ice coverage.
  !      w3ulev    subr. w3updtmd transform the wavenumber grid.
  !      w3ddxy    subr. w3updtmd calculate dirivatives of the depth.
  !      w3dcxy    subr. w3updtmd calculate dirivatives of the current.
  !
  !      w3mapn    subr. w3pronmd preparation for  ropagation schemes.
  !      w3xypn    subr. w3pronmd longitude-latitude ("xy") propagation.
  !      w3ktpn    subr. w3pronmd intra-spectral ("k-theta") propagation.
  !
  !      w3srce    subr. w3srcemd source term integration and calculation.
  !
  !      w3iogr    subr. w3iogrmd reading/writing model definition file.
  !      w3outg    subr. w3iogomd generate gridded output fields.
  !      w3iogo    subr. w3iogomd read/write gridded output.
  !      w3iope    subr. w3iopomd extract point output.
  !      w3iopo    subr. w3iopomd read/write point output.
  !      w3iotr    subr. w3iotrmd process spectral output along tracks.
  !      w3iors    subr. w3iorsmd read/write restart files.
  !      w3iobc    subr. w3iobcmd read/write boundary conditions.
  !      w3cprt    subr. w3iosfmd partition spectra.
  !      w3iosf    subr.   id.    write partitioned spectral data.
  !
  !      strace    subr. w3servmd subroutine tracing.
  !      wwtime    subr.   id.    system time in readable format.
  !      extcde    subr.   id.    program abort.
  !
  !      tick21    subr. w3timemd advance the clock.
  !      dsec21    func.   id.    difference between times.
  !      stme21    subr.   id.    time in readable format.
  !
  !      mpi_barrier, mpi_startall, mpi_waitall
  !                subr.          basic mpi routines.
  !     ----------------------------------------------------------------
  !
  !  5. remarks : call to w3nmin hidden behind w3_debugrun. this call
  !               currently only serves to warn when one or more procs
  !               have no active seapoints. it has been hid as this
  !               dramatically increases runtime performance.
  !
  !  6. switches :
  !
  !       !/shrd  switch for shared / distributed memory architecture.
  !       !/dist  id.
  !       !/mpi   id.
  !       !/ompg  id.
  !
  !       !/pr1   first order propagation schemes.
  !       !/pr2   ultimate quickest scheme.
  !       !/pr3   averaged ultimate quickest scheme.
  !       !/smc   uno2 scheme on smc grid.
  !
  !       !/s     enable subroutine tracing.
  !       !/t     test output.
  !       !/mpit  test output for mpi specific code.
  !
  !  7. source code :
  !
  !/ ------------------------------------------------------------------- /
  use w3servmd, only : print_memcheck
  use w3adatmd, only: mpibuf
  !module default
  implicit none
  !
  public
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief run wavewatch iii for a given time interval.
  !>
  !> @details currents are updated before winds as currents are used in wind
  !> and ustar processing.
  !>
  !> ice and water levels can be updated only once per call.
  !>
  !> if ice or water level time are undefined, the update
  !> takes place asap, otherwise around the "half-way point"
  !> between the old and new times.
  !>
  !> to increase accuracy, the calculation of the intra-spectral
  !> propagation is performed in two parts around the spatial propagation.
  !>
  !> @param[in] imod      model number.
  !> @param[in] tend      ending time of integration.
  !> @param[in] stamp     write time stamp (optional, defaults to t).
  !> @param[in] no_out    skip output (optional, defaults to f).
  !> @param[in] odat
  !> @param[in] id_lcomm  present only when using w3_oasis.
  !> @param[in] timen     present only when using w3_oasis.
  !>
  !> @author h. l. tolman  @date 22-mar-2021
  !>
  subroutine w3wave ( imod, odat, tend, stamp, no_out &
       )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    17-mar-1999 : distributed fortran 77 version.     ( version 1.18 )
    !/    04-feb-2000 : upgrade to fortran 90               ( version 2.00 )
    !/                  major changes to logistics.
    !/    05-jan-2001 : bug fix to allow model to run       ( version 2.05 )
    !/                  without output.
    !/    24-jan-2001 : flat grid version.                  ( version 2.06 )
    !/    09-feb-2001 : third propagation scheme added.     ( version 2.08 )
    !/    23-feb-2001 : check for barrier after source
    !/                  terms added ( w3nmin ).     ( delayed version 2.07 )
    !/    16-mar-2001 : fourth propagation scheme added.    ( version 2.09 )
    !/    30-mar-2001 : sub-grid obstacles added.           ( version 2.10 )
    !/    23-may-2001 : barrier added for dry run, changed  ( version 2.10 )
    !/                  declaration of fliwnd.
    !/    10-dec-2001 : sub-grid obstacles for uq schemes.  ( version 2.14 )
    !/    11-jan-2002 : sub-grid ice.                       ( version 2.15 )
    !/    24-jan-2002 : zero time step dor data ass.        ( version 2.17 )
    !/    09-may-2002 : switch clean up.                    ( version 2.21 )
    !/    13-nov-2002 : add stress vector.                  ( version 3.00 )
    !/    26-dec-2002 : moving grid version.                ( version 3.02 )
    !/    01-aug-2003 : moving grid gse correction.         ( version 3.03 )
    !/    07-oct-2003 : output options for nn training.     ( version 3.05 )
    !/    29-dec-2004 : multiple grid version.              ( version 3.06 )
    !/    04-feb-2005 : add stamp to par list.              ( version 3.07 )
    !/    04-may-2005 : change to mpi_comm_wave.            ( version 3.07 )
    !/    28-jun-2005 : adding map recalc for w3ulev call.  ( version 3.07 )
    !/    07-sep-2005 : updated boundary conditions.        ( version 3.08 )
    !/    26-jun-2006 : add output type 6.                  ( version 3.09 )
    !/    04-jul-2006 : consolidate stress arrays.          ( version 3.09 )
    !/    18-oct-2006 : partitioned spectral data output.   ( version 3.10 )
    !/    02-feb-2007 : add flagst test.                    ( version 3.10 )
    !/    02-apr-2007 : add partitioned field data.         ( version 3.11 )
    !/                  improve mpi_waitall call tests/allocations.
    !/    07-may-2007 : bug fix skip_o treatment.           ( version 3.11 )
    !/    17-may-2007 : adding ntproc/naproc separation.    ( version 3.11 )
    !/    08-oct-2007 : adding as cx-y to w3srce par. list. ( version 3.13 )
    !/    22-feb-2008 : initialize vgx-y properly.          ( version 3.13 )
    !/    10-apr-2008 : bug fix writing log file (mpi).     ( version 3.13 )
    !/    30-oct-2009 : implement run-time grid selection.  ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    31-mar-2010 : add reflections                     ( version 3.14.4 )
    !/    29-oct-2010 : implement unstructured grids        ( version 3.14.4 )
    !/                  (a. roland and f. ardhuin)
    !/    06-mar-2011 : output of max. cfl (f.ardhuin)      ( version 3.14.4 )
    !/    05-apr-2011 : implement iteration for dtmax <1s   ( version 3.14.4 )
    !/    02-jul-2012 : update for palm coupling            ( version 4.07 )
    !/    02-sep-2012 : clean up of open bc for ug grids    ( version 4.08 )
    !/    03-sep-2012 : fix format 902.                     ( version 4.10 )
    !/    10-dec-2012 : modify field output mpi for new     ( version 4.of )
    !/                  structure and smaller memory footprint.
    !/    16-nov-2013 : allows reflection on curvi. grids   ( version 4.13 )
    !/    27-nov-2013 : fixes for openmp versions.          ( version 4.15 )
    !/    23-may-2014 : adding ice fluxes to w3srce         ( version 5.01 )
    !/    27-may-2014 : move to ompg/x switch.              ( version 5.02 )
    !/    24-apr-2015 : adding oasis coupling calls         ( version 5.07 )
    !/                  (m. accensi & f. ardhuin, ifremer)
    !/    27-aug-2015 : update for iceh, icef               ( version 5.10 )
    !/    31-mar-2016 : current option for smc grid.        ( version 5.18 )
    !/    06-jun-2018 : add pdlib/memcheck/setup/netcdf_qad/timing
    !/                  oasis/debuginit/debugsrc/debugrun/debugcoh
    !/                  debugiobp/debugiobc                 ( version 6.04 )
    !/    14-sep-2018 : remove palm implementation          ( version 6.06 )
    !/    25-sep-2020 : oasis coupling at t+0               ( version 7.10 )
    !/    22-mar-2021 : update taua, rhoa                   ( version 7.13 )
    !/    06-may-2021 : use arctc and smctype options. jgli ( version 7.13 )
    !/
    !  1. purpose :
    !
    !     run wavewatch iii for a given time interval.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       imod    int.   i   model number.
    !       tend    i.a.   i   ending time of integration.
    !       stamp   log.   i   write(*,*)time stamp (optional, defaults to t).
    !       no_out  log.   i   skip output (optional, defaults to f).
    !                          skip at ending time only!
    !     ----------------------------------------------------------------
    !
    !     local parameters : flags
    !     ----------------------------------------------------------------
    !       floutg  log.  flag for running w3outg.
    !       flpart  log.  flag for running w3cprt.
    !       flzero  log.  flag for zero time interval.
    !       flag0   log.  flag for processors without tasks.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     see module documentation.
    !
    !  5. called by :
    !
    !     any program shell or integrated model which uses wavewatch iii.
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !     - currents are updated before winds as currents are used in wind
    !       and ustar processing.
    !     - ice and water levels can be updated only once per call.
    !     - if ice or water level time are undefined, the update
    !       takes place asap, otherwise around the "half-way point"
    !       betweem the old and new times.
    !     - to increase accuracy, the calculation of the intra-spectral
    !       propagation is performed in two parts around the spatial propagation.
    !
    !  8. structure :
    !
    !     -----------------------------------------------------------
    !       0.  initializations
    !         a point to data structures
    !         b subroutine tracing
    !         c local parameter initialization
    !         d test output
    !       1.  check the consistency of the input.
    !         a ending time versus initial time.
    !         b water level time.
    !         c current time interval.
    !         d wind time interval.
    !         e ice time.
    !       2.  determine next time from ending and output
    !           time and get corresponding time step.
    !       3.  loop over time steps (see below).
    !       4.  perform output to file if requested.
    !         a check if time is output time.
    !         b processing and mpp preparations.  ( w3cprt, w3outg )
    !         c reset next output time.
    !        -------------- loop over output types ------------------
    !         d perform output.                           ( w3ioxx )
    !         e update next output time.
    !        -------------------- end loop --------------------------
    !       5.  update log file.
    !       6.  if time is not ending time, branch back to 2.
    !     -----------------------------------------------------------
    !
    !      section 3.
    !     ----------------------------------------------------------
    !       3.1  interpolate winds and currents. ( w3ucur, w3dcxy )
    !                                                    ( w3uwnd )
    !                                                    ( w3uini )
    !       3.2  update boundary conditions.     ( w3iobc, w3ubpt )
    !       3.3  update ice coverage (if new ice map).   ( w3uice )
    !       3.4  transform grid (if new water level).    ( w3ulev )
    !       3.5  update maps and dirivatives.    ( w3mapn, w3ddxy )
    !                                            ( w3nmin, w3utrn )
    !            update grid advection vector.
    !       3.6  perform propagation
    !          a preparations.
    !          b intra spectral part 1.                  ( w3ktpn )
    !          c longitude-latitude       ( w3gath, w3xypn w3scat )
    !          b intra spectral part 2.                  ( w3ktpn )
    !       3.7  calculate and integrate source terms.   ( w3srce )
    !       3.8  update global time step.
    !     ----------------------------------------------------------
    !
    !  9. switches :
    !
    !     see module documentation.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    !/
    use w3gdatmd
    use w3wdatmd
    use w3adatmd
    use w3idatmd
    use w3odatmd
    !/
    use w3updtmd
    use w3srcemd
    !
    !/
    use w3triamd
    use w3iogrmd
    use w3iogomd
    use w3iopomd
    use w3iotrmd
    use w3iorsmd
    use w3iobcmd
    use w3iosfmd
    !/
    use w3servmd
    use w3timemd
    use w3parall, only : init_get_isea
    !
    include "mpif.h"
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)           :: imod, tend(2),odat(35)
    logical, intent(in), optional :: stamp, no_out
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters :
    !/
    integer                 :: ip
    integer                 :: tcalc(2), it, it0, nt, itest,        &
         itloc, itloch, ntloc, isea, jsea,    &
         ix, iy, ispec, j, tout(2), tlst(2),  &
         refled(6), ik, ith, is, nkcfl
    integer                 :: isp, ip_glob
    integer                 :: ttest(2),dttest
    real                    :: icedave
    !
    logical                 :: sbsed
    integer                 :: ierr_mpi, nrqmax
    integer, allocatable    :: statco(:,:), statio(:,:)
    integer                 :: ixrel
    real                    :: dttst, dttst1, dttst2, dttst3,       &
         dtl0, dti0, dtr0, dti10, dti50,      &
         dtga, dtg, dtgpre, dtres,            &
         fac, vgx, vgy, fack, facth,          &
         facx, xxx, reflec(4),                &
         delx, dely, dela, depth, d50, psic
    real                     :: vsiodummy(nspec), vdiodummy(nspec), vaolddummy(nspec)
    logical                  :: shavetotiodummy
    !
    real, allocatable       :: field(:)
    real                    :: tmp1(4), tmp2(3), tmp3(2), tmp4(2)
    !
    ! orphaned arrays from old data structure
    !
    real, allocatable       :: tauwx(:), tauwy(:)
    !
    logical                 :: flact, flzero, flfrst, flmap, tstamp,&
         skip_o, flag_o, flddir, readbc,      &
         flag0 = .false., floutg, flpfld,     &
         flpart, local, floutg2
    !
    logical                 :: flgmpi(0:8)
    logical                 :: ugdtupdate    ! true if time step should be updated for ug schemes
    character(len=8)        :: sttime
    character(len=21)       :: idact
    character(len=16)       :: outid
    character(len=23)       :: idtime
    integer eiobp
    integer ith_f
    !
    !
    !/
    integer :: memunit
    !/ ------------------------------------------------------------------- /
    ! 0.  initializations
    !
    xxx = undef
    memunit = 40000+iaproc
    ! 0.a set pointers to data structure
    !
    !
    if ( ioutp  .ne. imod ) call w3seto ( imod, ndse, ndst )
    if ( igrid  .ne. imod ) call w3setg ( imod, ndse, ndst )
    if ( iwdata .ne. imod ) call w3setw ( imod, ndse, ndst )
    if ( iadata .ne. imod ) call w3seta ( imod, ndse, ndst )
    if ( iidata .ne. imod ) call w3seti ( imod, ndse, ndst )
    !
    allocate(tauwx(nseal), tauwy(nseal))
    !
    if ( present(stamp) ) then
      tstamp = stamp
    else
      tstamp = .true.
    end if
    !
    if ( present(no_out) ) then
      skip_o = no_out
    else
      skip_o = .false.
    end if
    !
    ! 0.b subroutine tracing
    !
    !
    !
    ! 0.c local parameter initialization
    !
    ipass  = ipass + 1
    idact  = '                 '
    outid  = '           '
    flact  = itime .eq. 0
    flmap  = itime .eq. 0
    flddir = itime .eq. 0 .and. ( flcth .or. fsrefraction .or. flck .or. fsfreqshift )
    !
    flpfld = .false.
    do j=1,noge(4)
      flpfld = flpfld .or. flogrd(4,j) .or. flogr2(4,j)
    end do
    !
    if ( iaproc .eq. naplog ) backspace ( ndso )
    !
    if ( flcold ) then
      dtdyn = 0.
      fcut  = sig(nk) * tpiinv
    end if
    !
    if( gtype .eq. smctype ) then
      j = 1
    else
      allocate ( field(1-ny:ny*(nx+2)) )
    endif
    !
    local   = iaproc .le. naproc
    ugdtupdate = .false.
    if (flagll) then
      facx   =  1./(dera * radius)
    else
      facx   =  1.
    end if
    !
    sbsed = .false.
    !
    tauwx  = 0.
    tauwy  = 0.
    !
    ! 0.d test output
    !
    !
    ! 1.  check the consistency of the input ----------------------------- /
    ! 1.a ending time versus initial time
    !
    dttst  = dsec21 ( time , tend )
    flzero = dttst .eq. 0.
    if ( dttst .lt. 0. ) then
      if ( iaproc .eq. naperr ) write (ndse,1000)
      call extcde ( 1 )
    end if
    !
    ! 1.b water level time
    !
    if ( fllev ) then
      if ( tlev(1) .ge. 0. ) then
        dtl0   = dsec21 ( tlev , tln )
      else
        dtl0   = 1.
      end if
      if ( dtl0 .lt. 0. ) then
        if ( iaproc .eq. naperr ) write (ndse,1001)
        call extcde ( 2 )
      end if
    else
      dtl0   = 0.
    end if
    !
    ! 1.c current interval
    !
    if ( flcur ) then
      dttst1 = dsec21 ( tc0 , tcn )
      dttst2 = dsec21 ( tc0 , time )
      dttst3 = dsec21 ( tend , tcn )
      if ( dttst1.lt.0. .or. dttst2.lt.0. .or. dttst3.lt.0. ) then
        if ( iaproc .eq. naperr ) write (ndse,1002)
        call extcde ( 3 )
      end if
      if ( dttst2.eq.0..and. itime.eq.0 ) then
        idact(7:7) = 'f'
        tofrst = time
      end if
    end if
    !
    ! 1.d wind interval
    !
    if ( flwind ) then
      dttst1 = dsec21 ( tw0 , twn )
      dttst2 = dsec21 ( tw0 , time )
      dttst3 = dsec21 ( tend , twn )
      if ( dttst1.lt.0. .or. dttst2.lt.0. .or. dttst3.lt.0. ) then
        if ( iaproc .eq. naperr ) write (ndse,1003)
        call extcde ( 4 )
      end if
      if ( dttst2.eq.0..and. itime.eq.0 ) then
        idact(3:3) = 'f'
        tofrst = time
      end if
    end if
    !
    ! 1.e ice concentration interval
    !
    if ( flice ) then
      if ( tice(1) .ge. 0 ) then
        dti0   = dsec21 ( tice , tin )
      else
        dti0   = 1.
      end if
      if ( dti0 .lt. 0. ) then
        if ( iaproc .eq. naperr ) write (ndse,1004)
        call extcde ( 5 )
      end if
    else
      dti0   = 0.
    end if
    !
    ! 1.f momentum interval
    !
    if ( fltaua ) then
      dttst1 = dsec21 ( tu0 , tun )
      dttst2 = dsec21 ( tu0 , time )
      dttst3 = dsec21 ( tend , tun )
      if ( dttst1.lt.0. .or. dttst2.lt.0. .or. dttst3.lt.0. ) then
        if ( iaproc .eq. naperr ) write (ndse,1007)
        call extcde ( 3 )
      end if
      if ( dttst2.eq.0..and. itime.eq.0 ) then
        idact(9:9) = 'f'
        tofrst = time
      end if
    end if
    !
    ! 1.g air density time
    !
    if ( flrhoa ) then
      dttst1 = dsec21 ( tr0 , trn )
      dttst2 = dsec21 ( tr0 , time )
      dttst3 = dsec21 ( tend , trn )
      if ( dttst1.lt.0. .or. dttst2.lt.0. .or. dttst3.lt.0. ) then
        if ( iaproc .eq. naperr ) write (ndse,1008)
        call extcde ( 2 )
      end if
      if ( dttst2.eq.0..and. itime.eq.0 ) then
        idact(11:11) = 'f'
        tofrst = time
      end if
    end if
    !
    ! 1.e ice thickness interval
    !
    if ( flic1 ) then
      if ( tic1(1) .ge. 0 ) then
        dti10   = dsec21 ( tic1 , ti1 )
      else
        dti10   = 1.
      end if
      if ( dti10 .lt. 0. ) then
        if ( iaproc .eq. naperr ) write (ndse,1005)
        call extcde ( 5 )
      end if
    else
      dti10   = 0.
    end if
    !
    ! 1.e ice floe interval
    !
    !
    ! 2.  determine next time from ending and output --------------------- /
    !     time and get corresponding time step.
    !
    flfrst = .true.
    do
      !
      !
      ! 2.a pre-calculate table for ic3 ------------------------------------ /
      ! 2.b update group velocity and wavenumber from ice parameters ------- /
      !     from w3sic3md module. ------------------------------------------ /
      !     note: "if flfrst" can be added for efficiency, but testing req'd
      jsea=1 ! no switch (intentional)
          ! 2.b.1 using cheng method: requires stationary/uniform rheology.
          !       however, ice thickness may be input by either method
            ! 2.b.2 if not using cheng method: require flic1 to flic4 (not strictly
            !       necesssary, but makes code simpler)
      !
      if ( tofrst(1) .gt. 0 ) then
        dttst  = dsec21 ( tend , tofrst )
      else
        dttst  = 0.
      endif
      !
      if ( dttst.ge.0. ) then
        tcalc = tend
      else
        tcalc = tofrst
      end if
      !
      dttst  = dsec21 ( time , tcalc )
      nt     = 1 + int ( dttst / dtmax - 0.001 )
      dtga   = dttst / real(nt)
      if ( dttst .eq. 0. ) then
        it0    = 0
        if ( .not.flzero ) itime  = itime - 1
        nt     = 0
      else
        it0    = 1
      end if
      call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave')
      !
      !
      ! ==================================================================== /
      !
      ! 3.  loop over time steps
      !
      dtres  = 0.
      !
      do it = it0, nt
        ! copy old values
        !
        !
        call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave time loop 0')
        !
        itime  = itime + 1
        !
        dtg    = real(nint(dtga+dtres+0.0001))
        dtres  = dtres + dtga - dtg
        if ( abs(dtres) .lt. 0.001 ) dtres  = 0.
        call tick21 ( time , dtg )
        !
        call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave time loop 1')
        if ( tstamp .and. screen.ne.ndso .and. iaproc.eq.napout ) then
          call wwtime ( sttime )
          call stme21 ( time , idtime )
          write (screen,950) idtime, sttime
        end if
        !
        call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave time loop 2')
        vgx = 0.
        vgy = 0.
        if(inflags1(10)) then
          dttst1 = dsec21 ( time, tgn )
          dttst2 = dsec21 ( tg0, tgn )
          fac    = dttst1 / max ( 1. , dttst2 )
          vgx    = (fac*ga0+(1.-fac)*gan) * cos(fac*gd0+(1.-fac)*gdn)
          vgy    = (fac*ga0+(1.-fac)*gan) * sin(fac*gd0+(1.-fac)*gdn)
        end if
        !
        !
        ! 3.1 interpolate winds, currents, and momentum.
        !     (initialize wave fields with winds)
        !
        call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave time loop 3a')
        if ( flcur  ) then
          call w3ucur ( flfrst )
          call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave time loop 3b')
          if (gtype .eq. smctype) then
            ix = 1
          else if (gtype .eq. ungtype) then
            call ug_gradients(cx, dcxdx, dcxdy)
            call ug_gradients(cy, dcydx, dcydy)
            ugdtupdate=.true.
            cflxymax = 0.
          else
            call w3dzxy(cx(1:ubound(cx,1)),'m/s',dcxdx, dcxdy) !cx gradient
            call w3dzxy(cy(1:ubound(cy,1)),'m/s',dcydx, dcydy) !cy gradient
          endif  !! end gtype
          !
          call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave time loop 4')
          !
        else if ( flfrst ) then
          ugdtupdate=.true.
          cflxymax = 0.
          cx = 0.
          cy = 0.
        end if ! flcur
        !
        call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave time loop 5')
        if ( flwind ) then
          if ( flfrst ) asf = 1.
          call w3uwnd ( flfrst, vgx, vgy )
        else if ( flfrst ) then
          u10    = 0.01
          u10d   = 0.
          ust    = 0.05
          ustdir = 0.05
        end if
        call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave time loop 6')
        !
        if ( fliwnd .and. local ) call w3uini ( va )
        !
        if ( fltaua ) then
          call w3utau ( flfrst )
        else if ( flfrst ) then
          taua    = 0.01
          tauadir = 0.
        end if
        !
        if ( flrhoa ) then
          call w3urho ( flfrst )
        else if ( flfrst ) then
          rhoair = dair
        end if
        !
        ! 3.2 update boundary conditions if boundary flag is true (flbpi)
        !
        call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave time loop 7')
        if ( flbpi .and. local ) then
          !
          do
            if ( tbpin(1) .eq. -1 ) then
              readbc = .true.
              idact(1:1) = 'f'
            else
              readbc = dsec21(time,tbpin).lt.0.
              if (readbc.and.idact(1:1).eq.' ') idact(1:1) = 'x'
            end if
            flact  = readbc .or. flact
            if ( readbc ) then
              call w3iobc ( 'read', nds(9), tbpi0, tbpin, itest, imod )
              if ( itest .ne. 1 ) call w3ubpt
            else
              itest  = 0
            end if
            if ( itest .lt. 0 ) idact(1:1) = 'l'
            if ( itest .gt. 0 ) idact(1:1) = ' '
            if ( .not. (readbc.and.flbpi) ) exit
          end do
        end if
        call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave time loop 7')
        call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave time loop 8')
        !
        ! 3.3.1 update ice coverage (if new ice map).
        !     need to be run on output nodes too, to update mapstx
        !
        if ( flice .and. dti0.ne.0. ) then
          !
          if ( tice(1).ge.0 ) then
            if ( dti0 .lt. 0. ) then
              idact(13:13) = 'b'
            else
              dttst  = dsec21 ( time, tin )
              if ( dttst .le. 0.5*dti0 ) idact(13:13) = 'u'
            end if
          else
            idact(13:13) = 'i'
          end if
          !
          if ( idact(13:13).ne.' ' ) then
            call w3uice ( va )
            dti0   = 0.
            flact  = .true.
            flmap  = .true.
          end if
        end if
        call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave time loop 9')
        !
        ! 3.3.2 update ice thickness
        !
        if ( flic1 .and. dti10.ne.0. ) then
          !
          if ( tic1(1).ge.0 ) then
            if ( dti10 .lt. 0. ) then
              idact(15:15) = 'b'
            else
              dttst  = dsec21 ( time, ti1 )
              if ( dttst .le. 0.5*dti10 ) idact(15:15) = 'u'
            end if
          else
            idact(15:15) = 'i'
          end if
          !
          if ( idact(15:15).ne.' ' ) then
            call w3uic1 ( flfrst )
            dti10   = 0.
            flact  = .true.
            flmap  = .true.
          end if
          !
        end if
        call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave time loop 10')
        !
        ! 3.3.3 update ice floe diameter
        !
        call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave time loop 11a')
        !
        ! 3.4 transform grid (if new water level).
        !
        if ( fllev .and. dtl0 .ne.0. ) then
          !
          if ( tlev(1) .ge. 0 ) then
            if ( dtl0 .lt. 0. ) then
              idact(5:5) = 'b'
            else
              dttst  = dsec21 ( time, tln )
              if ( dttst .le. 0.5*dtl0 ) idact(5:5) = 'u'
            end if
          else
            idact(5:5) = 'i'
          end if
          !
          if ( idact(5:5).ne.' ' ) then
            call w3ulev ( va, va )
            ugdtupdate=.true.
            cflxymax = 0.
            dtl0   = 0.
            flact  = .true.
            flmap  = .true.
            flddir = flddir .or. flcth .or. fsrefraction .or. flck .or. fsfreqshift
          end if
        end if
        call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave time loop 11b')
        !
        ! 3.5 update maps and derivatives.
        !
        if ( flmap ) then
          if ( gtype .ne. smctype ) then
            call w3utrn ( trnx, trny )
          end if  !! gtype
          !! hides call to w3nmin, which currently only serves to warn when
          !! one or more procs have zero active seapoints.
          flmap  = .false.
        end if
        !
        !
        if ( flddir ) then
          if (gtype .eq. smctype) then
            ix = 1
          else if (gtype .eq. ungtype) then
            call ug_gradients(dw, dddx, dddy)
          else
            call w3dzxy(dw(1:ubound(dw,1)),'m',dddx,dddy)
          end if
          flddir = .false.
        end if
        call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave time loop 12')
        !
        ! calculate phase speed gradient.
        dcdx = 0.
        dcdy = 0.
        !
        !
        fliwnd = .false.
        flfrst = .false.
        !
        call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave time loop 13')
        !
        call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave time loop 14')
        if ( flzero ) then
          goto 400
        end if
        if ( it.eq.0 ) then
          dtg = 1.
          !            dtg = 60.
          goto 370
        end if
        if ( fldry .or. iaproc.gt.naproc ) then
          goto 380
        end if
        !
        ! estimation of the local maximum cfl for xy propagation
        !
        if ( flogrd(9,3).and. ugdtupdate ) then
          if (fstotalimp .eqv. .false.) then
            nkcfl=nk
            !
            !
            do jsea=1, nseal
              call init_get_isea(isea, jsea)
            end do
            !
            !
          end if
        end if
        call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave time loop 15')
        !
        !
        !
        ! 3.6 perform propagation = = = = = = = = = = = = = = = = = = = = = = =
        ! 3.6.1 preparations
        !
          ntloc  = 1 + int( dtg/dtcfli - 0.001 )
          !
          facth  = dtg / (dth*real(ntloc))
          fack   = dtg / real(ntloc)
          ttest(1) = time(1)
          ttest(2) = 0
          dttest = dsec21(ttest,time)
          itloch = ( ntloc + 1 - mod(nint(dttest/dtg),2) ) / 2
          !
          ! 3.6.2 intra-spectral part 1
          !
          if ( flcth .or. flck ) then
            do itloc=1, itloch
              !
              !
              do jsea=1, nseal
                call init_get_isea(isea, jsea)
                ix     = mapsf(isea,1)
                iy     = mapsf(isea,2)
                if ( gtype .eq. ungtype ) then
                  if (lpdlib) then
                  else
                    if (iobp(isea) .ne. 1) cycle
                  endif
                endif
                if ( mapsta(iy,ix) .eq. 1 ) then
                  depth  = max ( dmin , dw(isea) )
                  if (lpdlib) then
                    ixrel = jsea
                  else
                    ixrel = ix
                  end if
                  !
                  if( gtype .eq. smctype ) then
                    j = 1
                    !
                  else
                    j = 1
                    !
                    !
                  end if  !!  gtype
                  !
                end if
              end do
              !
              !
            end do
          end if
          call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave time loop 16')
          !
          ! 3.6.3 longitude-latitude
          !       (time step correction in routine)
          !
          if (gtype .eq. ungtype) then
            if (flagll) then
              facx   =  1./(dera * radius)
            else
              facx   =  1.
            end if
          end if
          if (lpdlib) then
            !
            !
          else
            if (flcx .or. flcy) then
              !
              if ( nrqsg1 .gt. 0 ) then
                call mpi_startall (nrqsg1, irqsg1(1,1), ierr_mpi)
                call mpi_startall (nrqsg1, irqsg1(1,2), ierr_mpi)
              end if
              !
              !
              ! initialize field variable
              field = 0.
              !
              do ispec=1, nspec
                if ( iappro(ispec) .eq. iaproc ) then
                  !
                  if( gtype .eq. smctype ) then
                    ix = 1
                  else if (.not.lpdlib ) then
                    call w3gath ( ispec, field )
                  end if   !! gtype
                  !
                  if (gtype .eq. smctype) then
                    ix = 1
                    !
                  else if (gtype .eq. ungtype) then
                    ix = 1
                    if (.not. lpdlib) then
                    end if
                    !
                  else
                    ix = 1
                    !
                  end if   !! gtype
                  !
                  if( gtype .eq. smctype ) then
                    ix = 1
                  else if (.not.lpdlib ) then
                    call w3scat ( ispec, mapsta, field )
                  end if   !! gtype
                end if
              end do
              !
              if ( nrqsg1 .gt. 0 ) then
                allocate ( statco(mpi_status_size,nrqsg1) )
                call mpi_waitall (nrqsg1, irqsg1(1,1), statco, ierr_mpi)
                call mpi_waitall (nrqsg1, irqsg1(1,2), statco, ierr_mpi)
                deallocate ( statco )
              end if
              call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave time loop 17')
              !
              !li   initialise ik ix iy in case arc option is not used to avoid warnings.
              ik=1
              ix=1
              iy=1
                  ! w3_smc ...
                  !
                  !
                  !
                !
                  !
              !
              ! end of test flcx.or.flcy
            end if
            !
          end if
          !
          ! 3.6.4 intra-spectral part 2
          !
          if ( flcth .or. flck ) then
            do itloc=itloch+1, ntloc
              !
              !
              do jsea = 1, nseal
                call init_get_isea(isea, jsea)
                ix     = mapsf(isea,1)
                iy     = mapsf(isea,2)
                depth  = max ( dmin , dw(isea) )
                if ( gtype .eq. ungtype ) then
                  if (lpdlib) then
                  else
                    if (iobp(isea) .ne. 1) cycle
                  endif
                endif
                if ( mapsta(iy,ix) .eq. 1 ) then
                  if (lpdlib) then
                    ixrel = jsea
                  else
                    ixrel = ix
                  end if
                  !
                  if( gtype .eq. smctype ) then
                    j = 1
                    !
                  else
                    j = 1
                    !
                  end if  !! gtype
                  !
                end if
              end do
              !
              !
            end do
          end if
          !
          ugdtupdate = .false.
          !
          ! 3.6 end propapgation  = = = = = = = = = = = = = = = = = = = = = = = =
          ! 3.7 calculate and integrate source terms.
          !
370       continue
          if ( flsou ) then
            !
            d50=0.0002
            reflec(:)=0.
            refled(:)=0
            psic=0.
            !
            !
            do jsea=1, nseal
              call init_get_isea(isea, jsea)
              ix     = mapsf(isea,1)
              iy     = mapsf(isea,2)
              dela=1.
              delx=1.
              dely=1.
              !
              if ( mapsta(iy,ix) .eq. 1 .and. flagst(isea)) then
                tmp1   = whitecap(jsea,1:4)
                tmp2   = bedforms(jsea,1:3)
                tmp3   = taubbl(jsea,1:2)
                tmp4   = tauice(jsea,1:2)
                  call w3srce(srce_direct, it, isea, jsea, ix, iy, imod, &
                       vaolddummy, va(:,jsea),                           &
                       vsiodummy, vdiodummy, shavetotiodummy,            &
                       alpha(1:nk,jsea), wn(1:nk,isea),                  &
                       cg(1:nk,isea), clats(isea), dw(isea), u10(isea),  &
                       u10d(isea),                                       &
                       as(isea), ust(isea),                              &
                       ustdir(isea), cx(isea), cy(isea),                 &
                       ice(isea), iceh(isea), icef(isea),                &
                       icedmax(isea),                                    &
                       reflec, refled, delx, dely, dela,                 &
                       trnx(iy,ix), trny(iy,ix), berg(isea),             &
                       fpis(isea), dtdyn(jsea),                          &
                       fcut(jsea), dtg, tauwx(jsea), tauwy(jsea),        &
                       tauox(jsea), tauoy(jsea), tauwix(jsea),           &
                       tauwiy(jsea), tauwnx(jsea),                       &
                       tauwny(jsea),  phiaw(jsea), charn(jsea),          &
                       tws(jsea), phioc(jsea), tmp1, d50, psic,tmp2,     &
                       phibbl(jsea), tmp3, tmp4 , phice(jsea),           &
                       tauocx(jsea), tauocy(jsea), wnmean(jsea),         &
                       rhoair(isea), asf(isea))
                whitecap(jsea,1:4) = tmp1
                bedforms(jsea,1:3) = tmp2
                taubbl(jsea,1:2) = tmp3
                tauice(jsea,1:2) = tmp4
              else
                ust   (isea) = undef
                ustdir(isea) = undef
                dtdyn (jsea) = undef
                fcut  (jsea) = undef
                !                    va(:,jsea)  = 0.
              end if
            end do
            !
            !
          end if
          !
          ! end of interations for dtmax < 1s
          !
        !
        !
        ! 3.8 update global time step.
        !     (branch point fldry, it=0)
        !
380     continue
        !
        if (it.ne.nt) then
          dttst  = dsec21 ( time , tcalc )
          dtg    = dttst / real(nt-it)
        end if
        !
        if ( flact .and. it.ne.nt .and. iaproc.eq.naplog ) then
          call stme21 ( time , idtime )
          if ( idlast .ne. time(1) ) then
            write (ndso,900) itime, ipass, idtime(01:19), idact, outid
            idlast = time(1)
          else
            write (ndso,901) itime, ipass, idtime(12:19), idact, outid
          end if
          flact  = .false.
          idact  = '         '
        end if
        !
        !
        !
      end do
      !
      call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave end time loop')
      !
      !     end of loop over time steps
      ! ==================================================================== /
      !
400   continue
      !
      ! 4.  perform output to file if requested ---------------------------- /
      ! 4.a check if time is output time
      !     delay if data assimilation time.
      !
      !
      if ( tofrst(1)  .eq. -1 ) then
        dttst  = 1.
      else
        dttst   = dsec21 ( time, tofrst )
      end if
      !
      if ( tdn(1)  .eq. -1 ) then
        dttst1 = 1.
      else
        dttst1  = dsec21 ( time, tdn )
      end if
      !
      dttst2 = dsec21 ( time, tend )
      flag_o = .not.skip_o .or. ( skip_o .and. dttst2.ne.0. )
      !
      !
      if ( dttst.le.0. .and. dttst1.ne.0. .and. flag_o ) then
        !
        !
        ! 4.b processing and mpp preparations
         !
         if (history_nc) then
            floutg = .false.
            floutg2 = .false.
            if (histwr) then
               if (flout(6))call w3cprt ( imod )
               call w3outg ( va, flpfld, .true., .false. )
               call w3iogoncmd(tend)
            end if
         else 
            if ( flout(1) ) then
               floutg = dsec21(time,tonext(:,1)).eq.0.
            else
               floutg = .false.
            end if
            !
            if ( flout(7) ) then
               floutg2 = dsec21(time,tonext(:,7)).eq.0.
            else
               floutg2 = .false.
            end if
            !
            flpart = .false.
            if ( flout(1) .and. flpfld ) flpart = flpart .or. dsec21(time,tonext(:,1)).eq.0.
            if ( flout(6) ) flpart = flpart .or. dsec21(time,tonext(:,6)).eq.0.
            !
            !
            if ( local .and. flpart ) call w3cprt ( imod )
            if ( local .and. (floutg .or. floutg2) ) then
               call w3outg ( va, flpfld, floutg, floutg2 )
            end if
         end if
         if (restart_nc) then
            if (rstwr) then
               call write_restart(....)
            end if
         end if
        !
        flgmpi = .false.
        nrqmax = 0
        !
        if ( (floutg) .or. (floutg2 .and. sbsed) ) then
          if (.not. lpdlib) then
            if (nrqgo.ne.0 ) then
              call mpi_startall ( nrqgo, irqgo , ierr_mpi )
              flgmpi(0) = .true.
              nrqmax    = max ( nrqmax , nrqgo )
            end if
            !
            if (nrqgo2.ne.0 ) then
              call mpi_startall ( nrqgo2, irqgo2, ierr_mpi )
              flgmpi(1) = .true.
              nrqmax    = max ( nrqmax , nrqgo2 )
            end if
          else
          end if ! if (.not. lpdlib) then
        end if
        call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave after time loop 1')
        !
        if ( flout(2) .and. nrqpo.ne.0 ) then
          if ( dsec21(time,tonext(:,2)).eq.0. ) then
            call mpi_startall ( nrqpo, irqpo1, ierr_mpi )
            flgmpi(2) = .true.
            nrqmax    = max ( nrqmax , nrqpo )
          end if
        end if
        !
        if (.not. restartnc) then
           if ( flout(4) .and. nrqrs.ne.0 ) then
              if ( dsec21(time,tonext(:,4)).eq.0. ) then
                 call mpi_startall ( nrqrs, irqrs , ierr_mpi )
                 flgmpi(4) = .true.
                 nrqmax    = max ( nrqmax , nrqrs )
              end if
           end if
           !
           if ( flout(8) .and. nrqrs.ne.0 ) then
              if ( dsec21(time,tonext(:,8)).eq.0. ) then
                 call mpi_startall ( nrqrs, irqrs , ierr_mpi )
                 flgmpi(8) = .true.
                 nrqmax    = max ( nrqmax , nrqrs )
              end if
           end if
        end if ! restartnc
           !
        if ( flout(5) .and. nrqbp.ne.0 ) then
          if ( dsec21(time,tonext(:,5)).eq.0. ) then
            call mpi_startall ( nrqbp , irqbp1, ierr_mpi )
            flgmpi(5) = .true.
            nrqmax    = max ( nrqmax , nrqbp )
          end if
        end if
        !
        if ( flout(5) .and. nrqbp2.ne.0 .and. iaproc.eq.napbpt) then
          if ( dsec21(time,tonext(:,5)).eq.0. ) then
            call mpi_startall (nrqbp2,irqbp2,ierr_mpi)
            nrqmax    = max ( nrqmax , nrqbp2 )
          end if
        end if
        !
        if ( nrqmax .ne. 0 ) allocate ( statio(mpi_status_size,nrqmax) )
        call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave after time loop 2')
        !
        ! 4.c reset next output time
        !
        tofrst(1) = -1
        tofrst(2) =  0
        !
        do j=1, notype
          if ( flout(j) .and. .not. history_nc) then
            !
            ! 4.d perform output
            !
            tout(:) = tonext(:,j)
            dttst   = dsec21 ( time, tout )
            !
            if ( dttst .eq. 0. ) then
              if ( ( j .eq. 1 )              &
                   ) then
                if ( iaproc .eq. napfld ) then
                  if ( flgmpi(1) ) call mpi_waitall ( nrqgo2, irqgo2, statio, ierr_mpi )
                  flgmpi(1) = .false.
                  !
                    call w3iogo( 'write', nds(7), itest, imod &
                            )
                  !
                  !
                end if
                !
              else if ( j .eq. 2 ) then
                !
                !   point output
                !
                if ( iaproc .eq. nappnt ) then
                  !
                  !   gets the necessary spectral data
                  !
                  call w3iope ( va )
                  call w3iopo ( 'write', nds(8), itest, imod &                          
                          )
                  end if
                !
              else if ( j .eq. 3 ) then
                !
                ! track output
                !
                call w3iotr ( nds(11), nds(12), va, imod )
              else if ( j .eq. 4 .and. .not. restart_nc) then
                call w3iors ('hot', nds(6), xxx, imod, flout(8) )
                itest = rstype
              else if ( j .eq. 5 ) then
                if ( iaproc .eq. napbpt ) then
                  if (nrqbp2.ne.0) call mpi_waitall ( nrqbp2, irqbp2,statio, ierr_mpi )
                  call w3iobc ( 'write', nds(10),         &
                       time, time, itest, imod )
                end if
              else if ( j .eq. 6 ) then
                call w3iosf ( nds(13), imod )
              end if
              !
              call tick21 ( tout, dtout(j) )
              tonext(:,j) = tout
              tlst        = tolast(:,j)
              dttst       = dsec21 ( tout , tlst )
              flout(j)    = dttst.ge.0.
              if ( flout(j) ) then
                outid(2*j-1:2*j-1) = 'x'
              else
                outid(2*j-1:2*j-1) = 'l'
              end if
            end if
            !
            ! 4.e update next output time
            !
            if ( flout(j) ) then
              if ( tofrst(1).eq.-1 ) then
                tofrst = tout
              else
                dttst  = dsec21 ( tout , tofrst )
                if ( dttst.gt.0.) then
                  tofrst = tout
                end if
              end if
            end if
            !
          end if
          !
        end do
        !
        call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave after time loop 3')
        ! if there is a second stream of restart files then j=8 and flout(8)=.true.
        j=8
        if ( flout(j) .and. .not. restart_nc) then
          !
          ! 4.d perform output
          !
          tout(:) = tonext(:,j)
          dttst   = dsec21 ( time, tout )
          if ( dttst .eq. 0. ) then
            call w3iors ('hot', nds(6), xxx, imod, flout(8) )
            itest = rstype
            call tick21 ( tout, dtout(j) )
            tonext(:,j) = tout
            tlst        = tolast(:,j)
            dttst       = dsec21 ( tout , tlst )
            flout(j)    = dttst.ge.0.
            if ( flout(j) ) then
              outid(2*j-1:2*j-1) = 'x'
            else
              outid(2*j-1:2*j-1) = 'l'
            end if
          end if
          !
          ! 4.e update next output time
          !
          if ( flout(j) ) then
            if ( tofrst(1).eq.-1 ) then
              tofrst = tout
            else
              dttst  = dsec21 ( tout , tofrst )
              if ( dttst.gt.0.) then
                tofrst = tout
              end if
            end if
          end if
        end if
        !        end of checkpoint
        !
        call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave after time loop 3')
        !
        if ( flgmpi(0) ) call mpi_waitall ( nrqgo, irqgo , statio, ierr_mpi )
        if ( flgmpi(2) ) call mpi_waitall ( nrqpo, irqpo1, statio, ierr_mpi )
        if ( flgmpi(4) ) call mpi_waitall ( nrqrs, irqrs , statio, ierr_mpi )
        if ( flgmpi(8) ) call mpi_waitall ( nrqrs, irqrs , statio, ierr_mpi )
        if ( flgmpi(5) ) call mpi_waitall ( nrqbp, irqbp1, statio, ierr_mpi )
        if ( nrqmax .ne. 0 ) deallocate ( statio )
        !
      end if
      call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave after time loop 4')
      !
      ! 5.  update log file ------------------------------------------------ /
      !
      if ( iaproc.eq.naplog ) then
        !
        call stme21 ( time , idtime )
        if ( flcur ) then
          dttst  = dsec21 ( time , tcn )
          if ( dttst .eq. 0. ) idact(7:7) = 'x'
        end if
        if ( flwind ) then
          dttst  = dsec21 ( time , twn )
          if ( dttst .eq. 0. ) idact(3:3) = 'x'
        end if
        if ( fltaua ) then
          dttst  = dsec21 ( time , tun )
          if ( dttst .eq. 0. ) idact(9:9) = 'x'
        end if
        if ( flrhoa ) then
          dttst  = dsec21 ( time , trn )
          if ( dttst .eq. 0. ) idact(11:11) = 'x'
        end if
        if ( tdn(1) .gt. 0  ) then
          dttst  = dsec21 ( time , tdn )
          if ( dttst .eq. 0. ) idact(21:21) = 'x'
        end if
        !
        if ( idlast.ne.time(1) ) then
          write (ndso,900) itime, ipass, idtime(1:19), idact, outid
          idlast = time(1)
        else
          write (ndso,901) itime, ipass, idtime(12:19), idact, outid
        end if
        !
      end if
      !
      idact  = '         '
      outid  = '           '
      flact  = .false.
      !
      ! 6.  if time is not ending time, branch back to 2 ------------------- /
      !
      dttst  = dsec21 ( time, tend )
      if ( dttst .eq. 0. ) exit
    end do
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave after time loop 5')
    !
    if ( tstamp .and. screen.ne.ndso .and. iaproc.eq.napout ) then
      call wwtime ( sttime )
      write (screen,951) sttime
    end if
    if ( iaproc .eq. naplog ) write (ndso,902)
    !
    deallocate(field)
    deallocate(tauwx, tauwy)
    !
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_wave end w3wave')
    !
    return
    !
    ! formats
    !
900 format (4x,i6,'|',i6,'| ', a19  ,' | ',a,' | ',a,' |')
901 format (4x,i6,'|',i6,'| ',11x,a8,' | ',a,' | ',a,' |')
902 format (2x,'--------+------+---------------------+'                  &
         ,'-----------------------+------------------+')
    !
950 format ('  wavewatch iii calculating for ',a,' at ',a)
951 format ('  wavewatch iii reached the end of a computation',          &
         ' loop at ',a)
1000 format (/' *** wavewatch iii error in w3wave :'/                    &
         '     ending time before starting time '/)
1001 format (/' *** wavewatch iii error in w3wave :'/                    &
         '     new water level before old water level '/)
1002 format (/' *** wavewatch iii error in w3wave :'/                    &
         '     illegal current interval '/)
1003 format (/' *** wavewatch iii error in w3wave :'/                    &
         '     illegal wind interval '/)
1004 format (/' *** wavewatch iii error in w3wave :'/                    &
         '     new ice field before old ice field '/)
1005 format (/' *** wavewatch iii error in w3wave :'/                    &
         '     new ic1 field before old ic1 field '/)
1007 format (/' *** wavewatch iii error in w3wave :'/                    &
         '     new atm momentum before old atm momentum '/)
1008 format (/' *** wavewatch iii error in w3wave :'/                    &
         '     new air density before old air density '/)
1030 format (/' *** wavewatch iii waring in w3wave :'/                   &
         '     at least one processor has 0 active points',              &
         ' in grid',i3)
    !
    !/
    !/ end of w3wave ----------------------------------------------------- /
    !/
  end subroutine w3wave
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief gather spectral bin information into a propagation field array.
  !>
  !> @details direct copy or communication calls (mpp version).
  !>  the field is extracted but not converted.
  !>
  !>  mpi version requires posing of send and receive calls in
  !>  w3wave to match local calls.
  !>
  !>  mpi version does not require an mpi_testall call for the
  !>  posted gather operation as mpi_waitall is mandatory to
  !>  reset persistent communication for next time step.
  !>
  !>  mpi version allows only two new pre-fetch postings per
  !>  call to minimize chances to be slowed down by gathers that
  !>  are not yet needed, while maximizing the pre-loading
  !>  during the early (low-frequency) calls to the routine
  !>  where the amount of calculation needed for proagation is
  !>  the largest.
  !>
  !> @param[in]  ispec spectral bin considered.
  !> @param[out] field full field to be propagated.
  !>
  !> @author h. l. tolman  @date 26-dec-2012
  !>
  subroutine w3gath ( ispec, field )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         26-dec-2012 |
    !/                  +-----------------------------------+
    !/
    !/    04-jan-1999 : distributed fortran 77 version.     ( version 1.18 )
    !/    13-jan-2000 : upgrade to fortran 90               ( version 2.00 )
    !/                  major changes to logistics.
    !/    29-dec-2004 : multiple grid version.              ( version 3.06 )
    !/    13-jun-2006 : split store in g/sstore             ( version 3.09 )
    !/    26-dec-2012 : move field init. to w3gath.         ( version 4.of )
    !/
    !  1. purpose :
    !
    !     gather spectral bin information into a propagation field array.
    !
    !  2. method :
    !
    !     direct copy or communication calls (mpp version).
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       ispec   int.   i   spectral bin considered.
    !       field   r.a.   o   full field to be propagated.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !
    !      mpi_startall, mpi_waitall
    !                subr. mpif.h   mpi persistent comm. routines (!/mpi).
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3wave    subr. w3wavemd actual wave model routine.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !       none.
    !
    !  7. remarks :
    !
    !     - the field is extracted but not converted.
    !     - mpi version requires posing of send and receive calls in
    !       w3wave to match local calls.
    !     - mpi version does not require an mpi_testall call for the
    !       posted gather operation as mpi_waitall is mandatory to
    !       reset persistent communication for next time step.
    !     - mpi version allows only two new pre-fetch postings per
    !       call to minimize chances to be slowed down by gathers that
    !       are not yet needed, while maximizing the pre-loading
    !       during the early (low-frequency) calls to the routine
    !       where the amount of calculation needed for proagation is
    !       the largest.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/shrd  switch for message passing method.
    !     !/mpi   id.
    !
    !     !/s     enable subroutine tracing.
    !     !/mpit  mpi test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use w3gdatmd, only: nspec, nx, ny, nsea, nseal, mapsf, dmin
    use w3parall, only: init_get_isea
    use w3wdatmd, only: a => va
    use w3adatmd, only: mpibuf, bstat, ibfloc, isploc, bispl, &
         nsploc, nrqsg2, irqsg2, gstore
    use w3odatmd, only: ndst, iaproc, naproc, notype
    !/
    !
    include "mpif.h"
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: ispec
    real, intent(out)       :: field(1-ny:ny*(nx+2))
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: status(mpi_status_size,nspec),  &
         ioff, ierr_mpi, jsea, isea,     &
         ixy, is0, ib0, npst, j
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    field  = 0.
    !
    ! 1.  shared memory version ------------------------------------------ /
    !
    !
    !
    ! 2.  distributed memory version ( mpi ) ----------------------------- /
    ! 2.a update counters
    !
    isploc = isploc + 1
    ibfloc = ibfloc + 1
    if ( ibfloc .gt. mpibuf ) ibfloc = 1
    !
    !
    ! 2.b check status of present buffer
    ! 2.b.1 scatter (send) still in progress, wait to end
    !
    if ( bstat(ibfloc) .eq. 2 ) then
      ioff =  1 + (bispl(ibfloc)-1)*nrqsg2
      if ( nrqsg2 .gt. 0 ) call mpi_waitall ( nrqsg2, irqsg2(ioff,2), status, ierr_mpi )
      bstat(ibfloc) = 0
    end if
    !
    ! 2.b.2 gather (recv) not yet posted, post now
    !
    if ( bstat(ibfloc) .eq. 0 ) then
      bstat(ibfloc) = 1
      bispl(ibfloc) = isploc
      ioff =  1 + (isploc-1)*nrqsg2
      if ( nrqsg2 .gt. 0 ) call mpi_startall ( nrqsg2, irqsg2(ioff,1), ierr_mpi )
    end if
    !
    ! 2.c put local spectral densities in store
    !
    do jsea=1, nseal
      call init_get_isea(isea, jsea)
      gstore(isea,ibfloc) = a(ispec,jsea)
    end do
    !
    ! 2.d wait for remote spectral densities
    !
    ioff =  1 + (bispl(ibfloc)-1)*nrqsg2
    if ( nrqsg2 .gt. 0 ) call mpi_waitall ( nrqsg2, irqsg2(ioff,1), status, ierr_mpi )
    !
    !
    ! 2.e convert storage array to field.
    !
    do isea=1, nsea
      ixy        = mapsf(isea,3)
      field(ixy) = gstore(isea,ibfloc)
    end do
    !
    ! 2.f pre-fetch data in available buffers
    !
    is0    = isploc
    ib0    = ibfloc
    npst   = 0
    !
    do j=1, mpibuf-1
      is0    = is0 + 1
      if ( is0 .gt. nsploc ) exit
      ib0    = 1 + mod(ib0,mpibuf)
      if ( bstat(ib0) .eq. 0 ) then
        bstat(ib0) = 1
        bispl(ib0) = is0
        ioff       = 1 + (is0-1)*nrqsg2
        if ( nrqsg2 .gt. 0 ) call mpi_startall ( nrqsg2, irqsg2(ioff,1), ierr_mpi )
        npst       = npst + 1
      end if
      if ( npst .ge. 2 ) exit
    end do
    !
    ! 2.g test output
    !
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3gath ----------------------------------------------------- /
    !/
  end subroutine w3gath
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief scatter data back to spectral storage after propagation.
  !>
  !> @details direct copy or communication calls (mpp version). see also w3gath.
  !>  the field is put back but not converted!
  !>  mpi persistent communication calls initialize in w3mpii.
  !>  see w3gath and w3mpii for additional comments on data buffering.
  !>
  !> @param[inout] ispec  spectral bin considered.
  !> @param[inout] mapsta status map for spatial grid.
  !> @param[inout] field  full field to be propagated.
  !>
  !> @author h. l. tolman  @date 13-jun-2006
  !>
  subroutine w3scat ( ispec, mapsta, field )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         13-jun-2006 |
    !/                  +-----------------------------------+
    !/
    !/    04-jan-1999 : distributed fortran 77 version.     ( version 1.18 )
    !/    13-jan-2000 : upgrade to fortran 90               ( version 2.00 )
    !/                  major changes to logistics.
    !/    28-dec-2004 : multiple grid version.              ( version 3.06 )
    !/    07-sep-2005 : updated boundary conditions.        ( version 3.08 )
    !/    13-jun-2006 : split store in g/sstore             ( version 3.09 )
    !/
    !  1. purpose :
    !
    !     'scatter' data back to spectral storage after propagation.
    !
    !  2. method :
    !
    !     direct copy or communication calls (mpp version).
    !     see also w3gath.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       ispec   int.   i   spectral bin considered.
    !       mapsta  i.a.   i   status map for spatial grid.
    !       field   r.a.   i   full field to be propagated.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !
    !      mpi_startall, mpi_waitall, mpi_testall
    !                subr. mpif.h   mpi persistent comm. routines (!/mpi).
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3wave    subr. w3wavemd actual wave model routine.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks :
    !
    !     - the field is put back but not converted !
    !     - mpi persistent communication calls initialize in w3mpii.
    !     - see w3gath and w3mpii for additional comments on data
    !       buffering.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/shrd  switch for message passing method.
    !     !/mpi   id.
    !
    !     !/s     enable subroutine tracing.
    !     !/mpit  mpi test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: nsea, nseal, mapsf, nspec, nx, ny
    !/
    use w3wdatmd, only: a => va
    use w3adatmd, only: mpibuf, bstat, ibfloc, isploc, bispl, &
         nsploc, nrqsg2, irqsg2, sstore
    use w3odatmd, only: ndst
    use w3odatmd, only: iaproc, naproc
    use constants, only : lpdlib
    use w3parall, only: init_get_isea
    !/
    !
    include "mpif.h"
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: ispec, mapsta(ny*nx)
    real, intent(in)        :: field(1-ny:ny*(nx+2))
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: isea, ixy, ioff, ierr_mpi, j,   &
         status(mpi_status_size,nspec),  &
         jsea, ib0
    logical                 :: done
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  shared memory version ------------------------------------------ *
    !
    !
    !
    ! 2.  distributed memory version ( mpi ) ----------------------------- *
    ! 2.a initializations
    !
    !
    !
    ! 2.b convert full grid to sea grid, active points only
    !
    do isea=1, nsea
      ixy    = mapsf(isea,3)
      if ( mapsta(ixy) .ne. 0 ) sstore(isea,ibfloc) = field(ixy)
    end do
    !
    ! 2.c send spectral densities to appropriate remote
    !
    ioff   = 1 + (isploc-1)*nrqsg2
    if ( nrqsg2 .gt. 0 ) call mpi_startall ( nrqsg2, irqsg2(ioff,2), ierr_mpi )
    bstat(ibfloc) = 2
    !
    ! 2.d save locally stored results
    !
    do jsea=1, nseal
      call init_get_isea(isea, jsea)
      ixy    = mapsf(isea,3)
      if (mapsta(ixy) .ne. 0) a(ispec,jsea) = sstore(isea,ibfloc)
    end do
    !
    ! 2.e check if any sends have finished
    !
    ib0    = ibfloc
    !
    do j=1, mpibuf
      ib0    = 1 + mod(ib0,mpibuf)
      if ( bstat(ib0) .eq. 2 ) then
        ioff   = 1 + (bispl(ib0)-1)*nrqsg2
        if ( nrqsg2 .gt. 0 ) then
          call mpi_testall ( nrqsg2, irqsg2(ioff,2), done, status, ierr_mpi )
        else
          done   = .true.
        end if
        if ( done .and. nrqsg2.gt.0 ) then
          call mpi_waitall ( nrqsg2, irqsg2(ioff,2), status, ierr_mpi )
        end if
        if ( done ) then
          bstat(ib0) = 0
        end if
      end if
    end do
    !
    ! 2.f last component, finish message passing, reset buffer control
    !
    if ( isploc .eq. nsploc ) then
      !
      do ib0=1, mpibuf
        if ( bstat(ib0) .eq. 2 ) then
          ioff   = 1 + (bispl(ib0)-1)*nrqsg2
          if ( nrqsg2 .gt. 0 ) call mpi_waitall ( nrqsg2, irqsg2(ioff,2), status, ierr_mpi )
          bstat(ib0) = 0
        end if
      end do
      !
      isploc = 0
      ibfloc = 0
      !
    end if
    !
    ! 2.g test output
    !
    !
    !
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3scat ----------------------------------------------------- /
    !/
  end subroutine w3scat
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief check minimum number of active sea points at given processor to
  !>  evaluate the need for a mpi_barrier call.
  !>
  !> @param[in]  mapsta status map for spatial grid.
  !> @param[out] flag0  flag to identify 0 as minimum.
  !>
  !> @author h. l. tolman  @date 28-dec-2004
  !>
  subroutine w3nmin ( mapsta, flag0 )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         28-dec-2004 |
    !/                  +-----------------------------------+
    !/
    !/    23-feb-2001 : origination.                        ( version 2.07 )
    !/    28-dec-2004 : multiple grid version.              ( version 3.06 )
    !/
    !  1. purpose :
    !
    !     check minimum number of active sea points at given processor to
    !     evaluate the need for a mpi_barrier call.
    !
    !  2. method :
    !
    !     evaluate mapsta.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       mapsta  i.a.   i   status map for spatial grid.
    !       flag0   log.   o   flag to identify 0 as minimum.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3wave    subr. w3wavemd actual wave model routine.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s     enable subroutine tracing.
    !     !/t     test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use w3gdatmd, only: nsea, mapsf, nx, ny
    use w3odatmd, only: ndst, naproc
    use w3parall, only: init_get_jsea_isproc
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: mapsta(ny*nx)
    logical, intent(out)    :: flag0
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: nmin, iproc, nloc, isea, ixy
    integer                 :: jsea, isproc
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    nmin   = nsea
    !
    do iproc=1, naproc
      nloc   = 0
      do isea=1, nsea
        call init_get_jsea_isproc(isea, jsea, isproc)
        if (isproc .eq. iproc) then
          ixy    = mapsf(isea,3)
          if ( mapsta(ixy) .eq. 1 ) nloc = nloc + 1
        end if
      end do
      !
      nmin   = min ( nmin , nloc )
    end do
    !
    flag0  = nmin .eq. 0
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3nmin ----------------------------------------------------- /
    !/
  end subroutine w3nmin
  !/
  !/ end of module w3wavemd -------------------------------------------- /
  !/
end module w3wavemd
