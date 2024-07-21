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
module w3gridmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |           j. h. alves             |
  !/                  |            f. ardhuin             |
  !/                  |                        fortran 90 |
  !/                  | last update :         27-may-2021 |
  !/                  +-----------------------------------+
  !/
  !/    14-jan-1999 : final fortran 77                    ( version 1.18 )
  !/    27-jan-2000 : upgrade to fortran 90               ( version 2.00 )
  !/                  add unformatted bath file option.
  !/                  read options with namelists.
  !/    14-feb-2000 : adding exact snl                    ( version 2.01 )
  !/    04-may-2000 : non central source term int.        ( version 2.03 )
  !/    24-jan-2001 : flat grid option.                   ( version 2.06 )
  !/    02-feb-2001 : xnl version 3.0                     ( version 2.07 )
  !/    09-feb-2001 : third propagation scheme added.     ( version 2.08 )
  !/    27-feb-2001 : o0 output switch added.             ( version 2.08 )
  !/    16-mar-2001 : fourth propagation scheme added.    ( version 2.09 )
  !/    29-mar-2001 : sub-grid island treatment.          ( version 2.10 )
  !/    20-jul-2001 : clean up.                           ( version 2.11 )
  !/    12-sep-2001 : clean up.                           ( version 2.13 )
  !/    09-nov-2001 : clean up.                           ( version 2.14 )
  !/    11-jan-2002 : sub-grid ice treatment.             ( version 2.15 )
  !/    17-jan-2002 : dsii bug fix.                       ( version 2.16 )
  !/    09-may-2002 : switch clean up.                    ( version 2.21 )
  !/    26-nov-2002 : adding first version of nl-3/4.     ( version 3.01 )
  !/                  removed before distribution in 3.12.
  !/    26-dec-2002 : relaxing cfl time step.             ( version 3.02 )
  !/    01-aug-2003 : modify gse correction for moving gr.( version 3.03 )
  !/                  add offset option for first direction.
  !/    24-dec-2004 : multiple grid version.              ( version 3.06 )
  !/    04-may-2005 : allow active points at edge.        ( version 3.07 )
  !/    07-jul-2005 : add mapst2 and map processing.      ( version 3.07 )
  !/    09-nov-2005 : remove soft boundary options.       ( version 3.08 )
  !/    23-jun-2006 : adding alternative source terms.    ( version 3.09 )
  !/                  module w3sln1md, dummy for others.
  !/    28-jun-2006 : adding file name preamble.          ( version 3.09 )
  !/    28-oct-2006 : spectral partitioning.              ( version 3.09 )
  !/    09-jan-2007 : correct edges of read mask.         ( version 3.10 )
  !/    26-mar-2007 : add to spectral partitioning.       ( version 3.11 )
  !/    14-apr-2007 : add miche style limiter.            ( version 3.11 )
  !/                  ( j. h. alves )
  !/    25-apr-2007 : battjes-janssen sdb added.          ( version 3.11 )
  !/                  ( j. h. alves )
  !/    18-sep-2007 : adding wam4 physics option.         ( version 3.13 )
  !/                  ( f. ardhuin )
  !/    09-oct-2007 : adding bottom scattering sbs1.      ( version 3.13 )
  !/                  ( f. ardhuin )
  !/    22-feb-2008 : initialize trnx-y properly.         ( version 3.13 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/    23-jul-2009 : modification of st3 namelist  .     ( version 3.14-shom )
  !/    31-mar-2010 : addition of shoreline reflection    ( version 3.14-ifremer )
  !/    29-jun-2010 : adding stokes drift profile output  ( version 3.14-ifremer )
  !/    30-aug-2010 : adding st4 option                   ( version 3.14-ifremer )
  !/    30-oct-2009 : implement run-time grid selection.  ( version 3.14 )
  !/                  (w. e. rogers & t. j. campbell, nrl)
  !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
  !/                  (w. e. rogers & t. j. campbell, nrl)
  !/    29-oct-2010 : clean up of unstructured grids      ( version 3.14.4 )
  !/                  (a. roland and f. ardhuin)
  !/    06-dec-2010 : change from global (logical) to iclose (integer) to
  !/                  specify index closure for a grid. change global
  !/                  input in ww3_grid.inp to cstrg.     ( version 3.14 )
  !/                  (t. j. campbell, nrl)
  !/    25-jun-2011 : adding movable bed friction         ( version 4.01 )
  !/    16-sep-2011 : clean up.                           ( version 4.05 )
  !/    01-dec-2011 : new namelist for reflection         ( version 4.05 )
  !/    01-mar-2012 : bug correction for nlprop in st2    ( version 4.05 )
  !/    12-jun-2012 : add /rtd rotated grid option. jgli  ( version 4.06 )
  !/    13-jul-2012 : move data structures gmd (snl3) and nonlinear
  !/                  filter (snls) from 3.15 (hlt).      ( version 4.07 )
  !/    02-sep-2012 : clean up of reflection and ug grids ( version 4.08 )
  !/    12-dec-2012 : adding smc grid.  jg_li             ( version 4.08 )
  !/    19-dec-2012 : add noswll as namelist variable.    ( version 4.of )
  !/    05-mar-2013 : adjusted default roughness for rocks( version 4.09 )
  !/    01-jun-2013 : adding namelist for spectral output ( version 4.10 )
  !/    12-sep-2013 : adding arctic part for smc grid.    ( version 4.11 )
  !/    01-nov-2013 : changed ug list name to unst        ( version 4.12 )
  !/    11-nov-2013 : make smc and rtd option compatible. ( version 4.13 )
  !/    13-nov-2013 : moved out reflection to w3updtmd    ( version 4.12 )
  !/    27-jul-2013 : adding free infragravity waves      ( version 4.15 )
  !/    02-dec-2013 : update of st4                       ( version 4.16 )
  !/    16-feb-2014 : adds wind bias correction: wcor     ( version 5.00 )
  !/    10-mar-2014 : adding namelist for ic2             ( version 5.01 )
  !/    29-may-2014 : adding namelist for ic3             ( version 5.01 )
  !/    15 oct-2015 : change smc grid input files. jgli   ( version 5.09 )
  !/    10-jan-2017 : changes for us3d and ussp           ( version 6.01 )
  !/    20-jan-2017 : bug fix for mask input from file.   ( version 6.02 )
  !/    01-mar-2018 : rtd poles info read from namelist   ( version 6.02 )
  !/    14-mar-2018 : option to read unst boundary file   ( version 6.02 )
  !/    26-mar-2018 : sea-point only wnd/cur input. jgli  ( version 6.02 )
  !/    15-may-2018 : dry sea points over zlim            ( version 6.04 )
  !/    06-jun-2018 : add implicit grid parameters for unstructured grids
  !/                  add debuggrid/debugstp              ( version 6.04 )
  !/    18-aug-2018 : s_{ice} ic5 (q. liu)                ( version 6.06 )
  !/    20-jun-2018 : update of st6  (q. liu)             ( version 6.06 )
  !/    26-aug-2018 : uost (mentaschi et al. 2015, 2018)  ( version 6.06 )
  !/    27-aug-2018 : add wbt parameter                   ( version 6.06 )
  !/    22-jan-2020 : update default values for is2       ( version 7.05 )
  !/    20-feb-2020 : include romero's dissipation in st4 ( version 7.06 )
  !/    15-apr-2020 : adds optional opt-out for cfl on bc ( version 7.08 )
  !/    18-jun-2020 : adds 360-day calendar option        ( version 7.08 )
  !/    24-jun-2020 : rtd output b. c. to rotated grid.   ( version 7.11 )
  !/    05-jan-2021 : update smc grid for multi-grid. jgli( version 7.13 )
  !/    27-may-2021 : updates for ic5 (q. liu)            ( version 7.12 )
  !/    27-may-2021 : moved to a subroutine               ( version 7.13 )
  !/    07-jun-2021 : s_{nl} gke nl5 (q. liu)             ( version 7.13 )
  !/    19-jul-2021 : momentum and air density support    ( version 7.14 )
  !/    28-feb-2023 : gqm as an alternative for nl1       ( version 7.15 )
  !/    11-jan-2024 : new namelist parameters for ic4     ( version 7.15 )
  !/    03-may-2024 : new capchnk parameters for sin4     ( version 7.15 )
  !/
  !/    copyright 2009-2013 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     "grid" preprocessing subroutine, which writes a model definition
  !     file containing the model parameter settigs and grid data.
  !
  !  2. method :
  !
  !     information is read from the file ww3_grid.inp (ndsi), or
  !     preset in this subroutine. a model definition file mod_def.ww3 is
  !     then produced by w3iogr. note that the name of the model
  !     definition file is set in w3iogr.
  !
  !  3. parameters :
  !
  !     local parameters.
  !     ----------------------------------------------------------------
  !       ndsi    int.  input unit number ("ww3_grid.inp").
  !       ndss    int.  scratch file.
  !       ndsg    int.  grid unit ( may be ndsi )
  !       ndstr   int.  sub-grid unit ( may be ndsi or ndsg )
  !       vsc     real  scale factor.
  !       vof     real  add offset.
  !       zlim    real  limiting bottom depth, used to define land.
  !       idla    int.  layout indicator used by ina2r.
  !       idfm    int.  id. format indicator.
  !       rform   c*16  id. format.
  !       fname   c*60  file name with bottom level data.
  !       from    c*4   test string for open, 'unit' or 'file'
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      w3nmod    subr. w3gdatmd set number of model.
  !      w3setg    subr.   id.    point to selected model.
  !      w3dims    subr.   id.    set array dims for a spectral grid.
  !      w3dimx    subr.   id.    set array dims for a spatial grid.
  !      w3grmp    subr. w3gsrumd compute bilinear interpolation for point
  !      w3nout    subr. w3odatmd set number of model for output.
  !      w3seto    subr.   id.    point to selected model for output.
  !      w3dmo5    subr.   id.    set array dims for output type 5.
  !      itrace    subr. w3servmd subroutine tracing initialization.
  !      strace    subr.   id.    subroutine tracing.
  !      nextln    subr.   id.    get next line from input file
  !      extcde    subr.   id.    abort program as graceful as possible.
  !      distab    subr. w3dispmd make tables for solution of the
  !                               dispersion relation.
  !      readnl    subr. internal read namelist.
  !      inar2r    subr. w3arrymd read in an real array.
  !      prtblk    subr.   id.    print plot of array.
  !      w3iogr    subr. w3iogrmd reading/writing model definition file.
  !     ----------------------------------------------------------------
  !
  !  5. called by :
  !
  !     ww3_grid program
  !
  !  6. error messages :
  !
  !  7. remarks :
  !
  !      physical grid :
  !     -----------------
  !
  !     the physical grid is defined by a grid counter ix defining the
  !     discrete longitude and iy defining the discrete latitude as shown
  !     below. for mathemathical convenience, these grid axes will
  !     generally be denoted as the x and y axes. two-dimensional arrays
  !     describing parameters on this grid are given as a(iy,ix).
  !
  !           iy=ny
  !             ^  |      |      |      |      |      |            ^ n
  !             |  |------|------|------|------|------|----        |
  !             |  |  ::  |  25  |  26  |  27  |  28  |          --|--
  !                |------|------|------|------|------|----        |
  !           iy=3 |  ::  |  ::  |  9   |  10  |  11  |            |
  !                |------|------|------|------|------|----
  !           iy=2 |  ::  |   1  |   2  |  ::  |   3  |
  !                |------|------|------|------|------|----
  !           iy=1 |  ::  |  ::  |  ::  |  ::  |  ::  |
  !                +------+------+------+------+------+----
  !                  ix=1   ix=2   ix=3   ix=4   ix=5   ---> ix=nx
  !
  !                                        :: is a land point.
  !
  !     to reduce memory usage of the model, spectra are stored for sea
  !     points only, in a one-dimensional grid with the length nsea. this
  !     grid is called the storage grid. the definition of the counter
  !     in the storage grid is graphically depicted above. to transfer
  !     data between the two grids, the maps mapfs and mapsf are
  !     determined. mapfs gives the counter of the storage grid isea
  !     for every physical grid point (iy,ix), such that
  !
  !             mapfs(iy,ix) = isea
  !
  !     isea = 0 corresponds to land points. the map mapsf gives the grid
  !     counters (iy,ix) for a given storage point isea.
  !
  !             mapsf(isea,1) = ix
  !             mapsf(isea,2) = iy
  !             mapsf(isea,3) = iy+(ix-1)*ny  ( filled during reading )
  !
  !     finally, a status maps mapsta and mapst2 are determined, where
  !     the status indicator istat = mapsta(iy,ix) determines the type
  !     of the grid point.
  !
  !         istat  means
  !       ---------------------------------------------------
  !           0    point excluded from grid.
  !        (-)1    sea point
  !        (-)2    "active" boundary point (data prescribed)
  !
  !     for istat=0, the secondary status counter ista2 is defined as
  !
  !         ista2  means
  !       ---------------------------------------------------
  !           0    land point.
  !           1    point excluded from grid.
  !
  !     negative values of istat identify points that are temporarily
  !     taken out of the computation. for these points ista2 are
  !     defined per bit
  !
  !         bit    means
  !       ---------------------------------------------------
  !          1     ice flag (1 = ice coverage)
  !          2     dry flag (1 = dry point with depth 0)
  !          3     inferred land in multi-grid model.
  !          4     masking in multi-grid model.
  !          5     land point flag for relocatable grid.
  !
  !      thus ista2=0 for istat<0 is in error, ista2=1 means ice cover,
  !      ista2=3 means ice on dry point, etc.
  !
  !      spectral grid :
  !     -----------------
  !
  !     in the spectral grid (and in physical space in general),
  !     the cartesian convention for directions is used, i.e., the
  !     direction 0 corresponds to waves propagating in the positive
  !     x-direction and 90 degr. corresponds to waves propagating in
  !     the positive y-direction. similar definitions are used for the
  !     internal description of winds and currents. output can obviously
  !     be transformed according to any preferred convention.
  !
  !          ith=nth
  !             ^  |      |      |      |      |
  !             |  |------|------|------|------|----
  !             |  |      |      |      |      |      th(3) = dth*2.
  !                |------|------|------|------|----
  !          ith=2 |      |      |      |      |      th(2) = dth
  !                |------|------|------|------|----
  !          ith=1 |      |      |      |      |      th(1) = 0.
  !                +------+------+------+------+----
  !                  ik=1   ik=2   ik=3   ik=4   ---> ik=nk
  !
  !     the spectral grid consists of nk wavenumbers. the first
  !     wavenumber ik=1 corresponds to the longest wave. the wavenumber
  !     grid varies in space, as given by an invariant relative freq.
  !     grid and the local depth. the spectral grid furthermore contains
  !     nth directions, equally spaced over a full circle. the first
  !     direction corresponds to the direction 0, etc.
  !
  ! (begin smc description)
  !
  !      spherical multiple-cell (smc) grid
  !     -----------------------------------
  !
  !     smc grid is a multi-resolution grid using cells of multiple times
  !     of each other.  it is similar to the lat-lon grid using rectangular
  !     cells but only cells at sea points are retained.  all land points
  !     have been removed from the model.  at high latitudes, cells are
  !     merged longitudinally to relax the cfl resctiction on time steps.
  !     near coastlines, cells are divided into quarters in a few steps so
  !     that high resolution is achieved to refine coastlines and resolve
  !     small islands.  at present, three tiers of quarter cells are used.
  !     for locating purpose, a usual x-y counter is setup by the smallest
  !     cell size and starting from the south-west corner of the usual
  !     rectuangular domain.  each sea cell is then given a pair of x-y
  !     index, plus a pair of increments.  these four index are stored in
  !     the cell array ijkcel(4, ncel), each row holds i, j, di, dj, and
  !     ijkdep holds ndps, where ndps is an integer depth in metre.  if
  !     precision higher than a metre is required, it may use other unit
  !     (cm for instance) with a conversion factor.
  !
  !     for transport calculation, two face arrays, ijkufc(7, nufc) and
  !     ijkvfc(7, nvfc), are also created to store the neighbouring cell
  !     sequential numbers and the face location and size.  the 3 arrays
  !     are calculated outside the wave model and input from text files.
  !
  !     boundary condition is added for smc grid so that it can be used for
  !     regional model as well.  most of the original boundary settings
  !     are reclaimed as long as the boundary condition file is provided
  !     by a lat-lon grid ww3 model, which will set the interpolation
  !     parameters in the boundary condition file.  the nbi number is
  !     reset with an input value because the nx-y double loop overcount
  !     the boundary cells for merged cells in the smc grid.  isbpi
  !     boundary cell mapping array is fine as mapfs uses duplicated cell
  !     number in any merged cell.  from there, all original nbi loops are
  !     reusable.
  !
  !     the whole arctic can be included in the smc grid if arctc variable
  !     is set to be .true. within the smc option.  the arctc option appends
  !     the polar arctic part above 86n to the existing smc grid and uses
  !     a map-east reference direction for this extra polar region.
  !     because the map-east direction changes with latitude and longitude
  !     the wave spectra defined to the map-east direction could not be
  !     mixed up with the conventional spectra defined to the local east
  !     direction.  a rotation sub is provided for convertion from one to
  !     another.  propagation part will be calculated together, including
  !     the boundary cells.  the boundary cells are then updated by
  !     assigning the corresponding inner cells to them after conversion.
  !     boundary cells are duplicated northmost 4 rows of the global part
  !     and they can be excluded for source term and output if required.
  !     for convenience, arctic cellls are all base level cells and are
  !     appended to the end of the global cells.  if refined cells were
  !     used in the arctic part, it would not be kept all together, making
  !     the sub-loops much more complicated. if refined resolution cells
  !     are required for a arctic regional model, users may consider use
  !     the rotated smc grid options (rtd and smc).
  !
  !     for more information about the smc grid, please refer to
  !     li, j.g. (2012) propagation of ocean surface waves on a spherical
  !     multiple-cell grid.  j. comput. phys., 231, 8262-8277.  online at
  !     http://dx.doi.org/10.1016/j.jcp.2012.08.007
  !
  ! (end smc description)
  !
  !     icewind is the scale factor for reduction of wind input by ice
  !     concentration. value specified corresponds to the fractional
  !     input for 100% ice concentration. default is 1.0, meaning that
  !     100% ice concentration result in zero wind input.
  !     sin_in_ice=sin_in_open_water * (1-ice*icewind)
  !     -----------------------------------------------------------------*
  !  8. structure :
  !
  !     ----------------------------------------------------------------
  !        1.   set up grid storage structure.
  !                               ( w3nmod , w3nout , w3setg , w3seto )
  !        2.a  i-o setup.
  !          b  print heading(s).
  !        3.   prepare int. table for dispersion relation   ( distab )
  !        4.   read and process input file up to spectrum.
  !          a  get comment character
  !          b  name of grid
  !          c  define spectrum                              ( w3dims )
  !        5.   set-up discrete spectrum.
  !          a  directions.
  !          b  frequency for spectrum.
  !        6.   read and process input file up to numerical parameters
  !          a  set model flags and time steps
  !          b  set / select source term package
  !          c  pre-process namelists.
  !          d  wind input source term.
  !          e  nonlinear interactions.
  !          f  whitecapping term.
  !          g  bottom friction source term.
  !          h  depth indiced breaking source term.
  !          i  triad interaction source term.
  !          j  bottom scattering source term.
  !          k  undefined source term.
  !          l  set / select propagaton scheme
  !          m  parameters for propagation scheme.
  !          n  set misc. parameters (ice, seeding, ...)
  !          o  end of namelist processing
  !          p  set various other variables
  !        7.   read and prepare grid.
  !          a  layout of grid
  !          b  storage of grid of grid
  !          c  read bottom depths
  !          d  set up temp map
  !          e  subgrid information
  !            1 info from input file
  !            2 open file and check if necessary
  !            3 read the data
  !            4 limit
  !        8    finalize status maps
  !          a  determine where to get the data
  !             get data in parts from input file
  !             ----------------------------------------------------
  !          b  read and update tmpsta with bound. and excl. points.
  !          c  finalize excluded points
  !             ----------------------------------------------------
  !             read data from file
  !             ----------------------------------------------------
  !          d  read data from file
  !             ----------------------------------------------------
  !          e  get nsea and other counters
  !          f  set up all maps                              ( w3dimx )
  !        9.   prepare output boundary points.
  !          a  read
  !          b  update
  !       10.   write model definition file.                 ( w3iogr )
  !     ----------------------------------------------------------------
  !
  !  9. switches :
  !
  !     !/flx1  stresses according to wu (1980).
  !     !/flx2  stresses according to t&c (1996).
  !     !/flx3  stresses according to t&c (1996) with cap on cd.
  !     !/flx4  stresses according to hwang (2011).
  !     !/flx5  direct use of stress from atmospheric model/input file.
  !
  !     !/ln0   no linear input source term.
  !     !/seed  'seeding' of lowest frequency for sufficiently strong
  !             winds. proxi for linear input.
  !     !/ln1   cavaleri and melanotte-rizzoli with tolman filter.
  !
  !     !/st0   no source terms included (input/dissipation)
  !     !/st1   wam-3 physics package.
  !     !/st2   tolman and chalikov (1996) physics package.
  !     !/st3   wam 4+ source terms from p.a.e.m. janssen and j-r. bidlot
  !     !/st4   input and dissipation using saturation following ardhuin et al. (2009,2010)
  !             filipot & ardhuin (2010) or romero (2019)
  !     !/st6   bydrz source term package featuring donelan et al.
  !             (2006) input and babanin et al. (2001,2010) dissipation.
  !
  !     !/nl0   no nonlinear interactions.
  !     !/nl1   discrete interaction approximation (dia or gqm).
  !     !/nl2   exact interactions (wrt).
  !     !/nl3   generalized multiple dia (gmd).
  !     !/nl4   two scale approximation
  !     !/nl5   generalized kinetic equation (gke)
  !     !/nls   snl based hf filter.
  !
  !     !/bt0   no bottom friction included.
  !     !/bt1   jonswap bottom friction package.
  !     !/bt4   showex bottom friction using movable bed roughness
  !                  (tolman 1994, ardhuin & al. 2003)
  !
  !     !/ic1   sink term for interaction with ice (uniform k_i)
  !     !/ic2   sink term for under-ice boundary layer friction
  !                  (liu et al.    1991: jgr 96 (c3), 4605-4621)
  !                  (liu and mollo 1988: jpo 18       1720-1712)
  !     !/ic3   sink term for interaction with ice (wang and shen method)
  !                  (wang and shen jgr 2010)
  !     !/ic4   sink term for empirical, frequency-dependent attenuation
  !                   in ice (wadhams et al. 1988: jgr 93 (c6) 6799-6818)
  !     !/ic5   sink term for interaction with ice (effective medium mod.)
  !                  (mosig et al. 2015, meylan et al. 2018, liu et al.
  !                   2020)
  !
  !     !/uost  unresolved obstacles source term (uost), mentaschi et al. 2015
  !
  !     !/db0   no depth-induced breaking included.
  !     !/db1   battjes-janssen depth-limited breaking.
  !     !/mlim  mich-style limiter.
  !
  !     !/tr0   no triad interactions included.
  !
  !     !/bs0   no bottom scattering included.
  !     !/bs1   routines from f. ardhuin.
  !
  !     !/pr1   first order propagation scheme.
  !     !/pr2   quickest scheme with ultimate limite and diffusion
  !             correction for swell dispersion.
  !     !/pr3   averaging ultimate quickest scheme.
  !
  !     !/rtd   rotated regular lat-lon grid. special case is standard polat=90.
  !     !/smc   spherical multiple-cell grid, may includes the whole arctic.
  !
  !     !/mgg   gse correction for moving grid.
  !
  !     !/s     enable subroutine tracing.
  !     !/t     enable test output.
  !     !/t0    enable test output tables for boundary output.
  !
  !     !/o0    print equivalent namelist setting to std out.
  !     !/o1    print tables with boundary points as part of output.
  !     !/o2    print mapsta as part of output.
  !     !/o2a   print land-sea mask in mask.ww3.
  !     !/o2b   print obstruction data.
  !     !/o2c   print extended status map.
  !
  ! 10. source code :
  !
  !/ ------------------------------------------------------------------- /
  use constants
  !/
  use w3triamd
  use w3gsrumd, only: w3grmp
  use w3odatmd, only: w3nout, w3seto, w3dmo5
  use w3iogrmd, only: w3iogr
  use w3servmd, only: itrace, nextln, extcde
  use w3arrymd, only: ina2r, ina2i
  use w3dispmd, only: distab
  !/
  use w3gdatmd
  use w3odatmd, only: ndse, ndst, ndso
  use w3odatmd, only: nbi, nbi2, nfbpo, nbo, nbo2, flbpi, flbpo,  &
       ipbpo, isbpo, xbpo, ybpo, rdbpo, fnmpre,    &
       ihmax, hspmin, wsmult, wscut, flcomb,       &
       noswll, ptmeth, ptfcut
  use w3timemd, only: caltype
  use w3nmlgridmd
  !
  !
  implicit none
  !/
  !/ ------------------------------------------------------------------- /
  !/ local parameters
  !/
  type(nml_spectrum_t)     :: nml_spectrum
  type(nml_run_t)          :: nml_run
  type(nml_timesteps_t)    :: nml_timesteps
  type(nml_grid_t)         :: nml_grid
  type(nml_rect_t)         :: nml_rect
  type(nml_curv_t)         :: nml_curv
  type(nml_unst_t)         :: nml_unst
  type(nml_smc_t)          :: nml_smc
  type(nml_depth_t)        :: nml_depth
  type(nml_mask_t)         :: nml_mask
  type(nml_obst_t)         :: nml_obst
  type(nml_slope_t)        :: nml_slope
  type(nml_sed_t)          :: nml_sed
  type(nml_inbnd_count_t)  :: nml_inbnd_count
  type(nml_inbnd_point_t), allocatable  :: nml_inbnd_point(:)
  type(nml_excl_count_t)   :: nml_excl_count
  type(nml_excl_point_t), allocatable   :: nml_excl_point(:)
  type(nml_excl_body_t), allocatable    :: nml_excl_body(:)
  type(nml_outbnd_count_t) :: nml_outbnd_count
  type(nml_outbnd_line_t), allocatable  :: nml_outbnd_line(:)
  !
  integer, parameter      :: nfl = 6
  integer                 :: ndsi, ndsi2, ndss, ndsm, ndsg, ndstr,&
       ierr, ndstrc, ntrace, ith, ik, ith0, &
       isp, iyn(nfl), nrlin, nrsrce, nrnl,  &
       nrbt, nrdb, nrtr, nrbs, nrprop,      &
       idla, idfm, ix0, ixn, ix, iy, isea,  &
       idx, ixo, idy, iyo, iba, nba, iloop, &
       ifl, nbotot, npo, ip, ix1, ix2, iy1, &
       iy2, j, jj, ixr(4), iyr(4), iseai(4),&
       ist, nki, nthi, nric, nris, i, idft, &
       nstat, nbt, nland, nosw, nmapb, imapb
  integer                 :: ncol =  78
  !
  !
  integer, allocatable    :: tmpsta(:,:), tmpmap(:,:), readmp(:,:)
  !
  real                    :: rxfr, rfr1, sigma, sxfr, fachf,      &
       vsc, vsc0, vof,                      &
       zlim, x, y, xp,  xo0, yo0, dxo, dyo, &
       xo, yo, rd(4), rdtot,                &
       factor, rth0, fmiche, rwndc,         &
       wcor1, wcor2
  !
  character(len=4)        :: gstrg, cstrg
  !
  ! variables used to allow spectral output on full grid
  !
  integer                 :: p2sf,i1p2sf,i2p2sf
  integer                 :: e3d,i1e3d,i2e3d
  integer                 :: us3d,i1us3d,i2us3d,                  &
       ussp, iussp,                         &
       th1mf, i1th1m, i2th1m,               &
       sth1mf, i1sth1m, i2sth1m,            &
       th2mf, i1th2m, i2th2m,               &
       sth2mf, i1sth2m, i2sth2m
  ! stk_wn are the decays for stokes drift partitions
  real                    :: stk_wn(25)
  !
  !
  !
  real, allocatable       :: xgrdin(:,:), ygrdin(:,:)
  real, allocatable       :: zbin(:,:), obsx(:,:), obsy(:,:)
  real, allocatable       :: refd(:,:), refd2(:,:), refs(:,:)
  !
  logical                 :: fllin, flinds, flnl, flbt, fldb,     &
       fltr, flbs, flprop, flref,     &
       first, connct, flnew, ingrid,flic,   &
       flis, flgnml
  logical                 :: fltc96 = .false.
  logical                 :: flnmlo = .false.
  logical                 :: flstb2 = .false.
  logical                 :: flst4  = .false.
  logical                 :: flst6  = .false.
  real                    :: facberg, refslope
  !
  !
  !
  character               :: comstr*1, pname*30, rform*16,        &
       from*4, fname*60, tname*60, line*80, &
       status*20,fname2*60, pname2*40
  character(len=6)        :: yesxno(2)
  !/ ------------------------------------------------------------------- /
  !/ namelists
  !/
  integer                 :: flagtr, ihm
  real                    :: cfltm, cice0, cicen, pmove, xfilt,    &
       lice, xseed, xr, hspm, wsm, wsc, stdx,&
       stdy, stdt, icehmin, icehfac, icehinit, &
       icesln, icewind, icesnl, icesds,        &
       icehdisp, icefdisp, iceddisp, btbet
  !
  real(8)                 :: gshift ! see notes in wmghgh
  logical                 :: flc, icedisp, trckcmpr
  integer                 :: ptm   ! partitioning method
  real                    :: ptfc  ! part. cut off freq (for method 5)
  real                    :: aircmin, airgb
  character               :: pmname*45, pmnam2*45  ! part. method desc.
  !
  !
  !
  !
  !
  logical :: jgs_terminate_maxiter
  logical :: jgs_terminate_difference
  logical :: jgs_terminate_norm
  logical :: jgs_limiter
  integer :: jgs_limiter_func
  logical :: jgs_block_gauss_seidel
  logical :: jgs_use_jacobi
  logical :: jgs_source_nonlinear
  logical :: ugobcauto
  logical :: ugbccfl
  logical :: expfsn
  logical :: expfspsi
  logical :: expfsfct
  logical :: impfsn
  logical :: exptotal
  logical :: imptotal
  logical :: imprefraction
  logical :: impfreqshift
  logical :: impsource
  logical :: setup_apply_wlv
  integer :: jgs_maxiter
  integer :: nbsel
  integer :: unstschemes(6)
  integer :: unstscheme
  integer :: jgs_nlevel
  real*8  :: jgs_pmin
  real*8  :: jgs_diff_thr
  real*8  :: jgs_norm_thr
  real*8  :: solverthr_setup
  real*8  :: crit_dep_setup
  !
  character               :: ugobcfile*60
  real                    :: ugobcdepth
  logical                 :: ugobcok
  !
  !
  !
  namelist /unst/ ugobcauto, ugobcdepth, ugobcfile,          &
       ugbccfl, expfsn, expfspsi, expfsfct,       &
       impfsn, imptotal, exptotal,                &
       imprefraction, impfreqshift,               &
       impsource,                                 &
       jgs_terminate_maxiter,                     &
       jgs_terminate_difference,                  &
       jgs_terminate_norm,                        &
       jgs_limiter,                               &
       jgs_limiter_func,                          &
       jgs_use_jacobi,                            &
       jgs_block_gauss_seidel,                    &
       jgs_maxiter,                               &
       jgs_pmin,                                  &
       jgs_diff_thr,                              &
       jgs_norm_thr,                              &
       jgs_nlevel,                                &
       jgs_source_nonlinear,                      &
       setup_apply_wlv, solverthr_setup,          &
       crit_dep_setup
  namelist /misc/ cice0, cicen, lice, xseed, flagtr, xp, xr, &
       xfilt, pmove, ihm, hspm, wsm, wsc, flc, fmiche, &
       rwndc, facberg, nosw, gshift, wcor1, wcor2,     &
       stdx, stdy, stdt, icehmin, icehinit, icedisp,   &
       icesln, icewind, icesnl, icesds, icehfac,       &
       icehdisp, iceddisp, icefdisp, caltype,          &
       trckcmpr, ptm, ptfc, btbet
  namelist /outs/ p2sf, i1p2sf, i2p2sf,                      &
       us3d, i1us3d, i2us3d,                    &
       ussp, iussp, stk_wn,                     &
       e3d, i1e3d, i2e3d,                       &
       th1mf, i1th1m, i2th1m,                   &
       sth1mf, i1sth1m, i2sth1m,                &
       th2mf, i1th2m, i2th2m,                   &
       sth2mf, i1sth2m, i2sth2m
  !/
  !/
  !/ ------------------------------------------------------------------- /
  !/
  data yesxno / 'yes/--' , '---/no' /
contains
  subroutine w3grid()
    !
    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! 1.  set up grid storage structure
    !
    call w3nmod ( 1, 6, 6 )
    call w3setg ( 1, 6, 6 )
    call w3nout (    6, 6 )
    call w3seto ( 1, 6, 6 )
    !
    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! 2.  io set-up.
    !
    ndsi   = 10
    ndss   = 99
    ndsm   = 20
    !
    inquire(file=trim(fnmpre)//"ww3_grid.nml", exist=flgnml)
    if (flgnml) then
      ! read namelist
      call w3nmlgrid (ndsi, trim(fnmpre)//'ww3_grid.nml', nml_spectrum, nml_run,  &
           nml_timesteps, nml_grid, nml_rect, nml_curv,   &
           nml_unst, nml_smc, nml_depth, nml_mask,        &
           nml_obst, nml_slope, nml_sed, nml_inbnd_count, &
           nml_inbnd_point, nml_excl_count,               &
           nml_excl_point, nml_excl_body,                 &
           nml_outbnd_count, nml_outbnd_line, ierr)
    else
      open (ndsi,file=trim(fnmpre)//'ww3_grid.inp',status='old',        &
           err=2000,iostat=ierr)
    end if
    !
    ndstrc =  6
    ntrace =  10
    call itrace ( ndstrc, ntrace )
    !
    write (ndso,900)
    !
    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! 3.a interpolation table for dispersion relation.
    !
    call distab
    !
    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! 3.b table for friction factors
    !
    call tabu_fw
    !
    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! 4   read and process input file up to spectrum
    !
    if (flgnml) then
      ! grid name
      gname=trim(nml_grid%name)
      write (ndso,902) gname
      ! spectrum parameters
      rxfr=nml_spectrum%xfr
      rfr1=nml_spectrum%freq1
      nki=nml_spectrum%nk
      nthi=nml_spectrum%nth
      rth0=nml_spectrum%thoff
    else
      read (ndsi,'(a)',end=2001,err=2002,iostat=ierr) comstr
      if (comstr.eq.' ') comstr = '$'
      write (ndso,901) comstr
      call nextln ( comstr , ndsi , ndse )
      !
      call nextln ( comstr , ndsi , ndse )
      read (ndsi,*,end=2001,err=2002) gname
      write (ndso,902) gname
      !
      call nextln ( comstr , ndsi , ndse )
      read (ndsi,*,end=2001,err=2002) rxfr, rfr1, nki, nthi, rth0
    end if
    nk     = nki
    nk2    = nki + 2
    nth    = nthi
    nspec  = nk * nth
    xfr    = max ( rxfr , 1.00001 )
    fr1    = max ( rfr1 , 1.e-6 )
    dth    = tpi / real(nth)
    rth0   = max ( -0.5 , min ( 0.5 , rth0 ) )
    write (ndso,903) nth, dth*rade
    write (ndso,904) 360./real(nth)*rth0
    write (ndso,905) nk, fr1, fr1*xfr**(nk-1), xfr
    !
    call w3dims ( 1, nk, nth, ndse, ndst )
    !
    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! 5.  initialize spectral parameters.
    ! 5.a directions :
    !
    do ith=1, nth
      th  (ith) = dth * ( rth0 + real(ith-1) )
      esin(ith) = sin ( th(ith) )
      ecos(ith) = cos ( th(ith) )
      if ( abs(esin(ith)) .lt. 1.e-5 ) then
        esin(ith) = 0.
        if ( ecos(ith) .gt. 0.5 ) then
          ecos(ith) =  1.
        else
          ecos(ith) = -1.
        end if
      end if
      if ( abs(ecos(ith)) .lt. 1.e-5 ) then
        ecos(ith) = 0.
        if ( esin(ith) .gt. 0.5 ) then
          esin(ith) =  1.
        else
          esin(ith) = -1.
        end if
      end if
      es2 (ith) = esin(ith)**2
      ec2 (ith) = ecos(ith)**2
      esc (ith) = esin(ith)*ecos(ith)
    end do
    !
    do ik=2, nk+1
      ith0   = (ik-1)*nth
      do ith=1, nth
        esin(ith0+ith) = esin(ith)
        ecos(ith0+ith) = ecos(ith)
        es2 (ith0+ith) = es2 (ith)
        ec2 (ith0+ith) = ec2 (ith)
        esc (ith0+ith) = esc (ith)
      end do
    end do
    !
    !   b frequencies :
    !
    sigma   = fr1 * tpi / xfr**2
    sxfr    = 0.5 * (xfr-1./xfr)
    !
    do ik=0, nk+1
      sigma    = sigma * xfr
      sig (ik) = sigma
      dsip(ik) = sigma * sxfr
    end do
    !
    dsii( 1) = 0.5 * sig( 1) * (xfr-1.)
    do ik=2, nk-1
      dsii(ik) = dsip(ik)
    end do
    dsii(nk) = 0.5 * sig(nk) * (xfr-1.) / xfr
    !
    do ik=1, nk
      dden(ik) = dth * dsii(ik) * sig(ik)
    end do
    !
    do isp=1, nspec
      ik         = 1 + (isp-1)/nth
      sig2 (isp) = sig (ik)
      dden2(isp) = dden(ik)
    end do
    !
    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! 6   read and process input file up to numerical parameters
    ! 6.a set model flags and time steps
    !
    write (ndso,910)
    if (flgnml) then
      fldry=nml_run%fldry
      flcx=nml_run%flcx
      flcy=nml_run%flcy
      flcth=nml_run%flcth
      flck=nml_run%flck
      flsou=nml_run%flsou
    else
      call nextln ( comstr , ndsi , ndse )
      read (ndsi,*,end=2001,err=2002)                                 &
           fldry, flcx, flcy, flcth, flck, flsou
    end if
    !
    iyn = 2
    if ( fldry ) iyn(1) = 1
    if ( flcx  ) iyn(2) = 1
    if ( flcy  ) iyn(3) = 1
    if ( flcth ) iyn(4) = 1
    if ( flck  ) iyn(5) = 1
    if ( flsou ) iyn(6) = 1
    !
    write (ndso,911) (yesxno(iyn(ifl)),ifl=1,nfl)
    !
    if ( .not. (fldry.or.flcx.or.flcy.or.flck.or.flcth.or.flsou) ) then
      write (ndse,1010)
      call extcde ( 2 )
    end if
    !
    if (flgnml) then
      dtmax=nml_timesteps%dtmax
      dtcfl=nml_timesteps%dtxy
      dtcfli=nml_timesteps%dtkth
      dtmin=nml_timesteps%dtmin
    else
      call nextln ( comstr , ndsi , ndse )
      read (ndsi,*,end=2001,err=2002) dtmax, dtcfl, dtcfli, dtmin
    end if
    dtmax  = max ( 1. , dtmax )
    !
    ! commented to allow very high resolution zooms
    !
    !      dtcfl  = max ( 1. , dtcfl  )
    !      dtcfli = min ( dtmax , max ( 1. , dtcfli ) )
    dtmin  = min ( dtmax , max ( 0. , dtmin  ) )
    write (ndso,912) dtmax, dtcfl, dtcfli, dtmin
    !
    ! 6.b set / select source term package
    !
    nrlin  = 0
    nrsrce = 0
    nrnl   = 0
    nrbt   = 0
    nric   = 0
    nris   = 0
    nrdb   = 0
    nrtr   = 0
    nrbs   = 0
    !
    fllin  = .true.
    flinds = .true.
    flnl   = .true.
    flbt   = .true.
    flic   = .false.
    flis   = .false.
    fldb   = .true.
    fltr   = .true.
    flbs   = .true.
    flref  = .false.
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    if ( .not.fllin .and.  .not.flinds .and.  .not.flnl .and.        &
         .not.flbt  .and.  .not.flic   .and.  .not.flis .and.        &
         .not.fldb  .and.  .not.fltr   .and.  .not.flbs .and.        &
         .not.flref .and.  flsou ) then
      write (ndse,1020)
      call extcde ( 10 )
    end if
    !
    if ( ( fllin .or. flinds .or. flnl .or. flbt .or. fldb .or.     &
         fltr .or. flbs .or. flref .or. flic )          &
         .and. .not.flsou ) then
      write (ndse,1021)
    end if
    !
    if ( nrlin .ne. 1 ) then
      write (ndse,1022) nrlin
      call extcde ( 11 )
    end if
    !
    if ( nrsrce .ne. 1 ) then
      write (ndse,1023) nrsrce
      call extcde ( 12 )
    end if
    !
    if ( nrnl .ne. 1 ) then
      write (ndse,1024) nrnl
      call extcde ( 13 )
    end if
    !
    if ( nrbt .ne. 1 ) then
      write (ndse,1025) nrbt
      call extcde ( 14 )
    end if
    !
    if ( nrdb .ne. 1 ) then
      write (ndse,1026) nrdb
      call extcde ( 15 )
    end if
    !
    if ( nrtr .ne. 1 ) then
      write (ndse,1027) nrtr
      call extcde ( 16 )
    end if
    !
    if ( nrbs .ne. 1 ) then
      write (ndse,1028) nrbs
      call extcde ( 17 )
    end if
    !
    if ( nric .gt. 1 ) then
      write (ndse,1034) nric
      call extcde ( 19 )
    end if
    !
    if ( nris .gt. 1 ) then
      write (ndse,1036) nris
      call extcde ( 26 )
    end if
    !
    ! 6.c read namelist file or pre-process namelists into scratch file
    !
    write (ndso,915)
    if (flgnml) then
      open (ndss,file=trim(fnmpre)//trim(nml_grid%nml),status='old',form='formatted')
    else
      open (ndss,file=trim(fnmpre)//'ww3_grid.scratch',form='formatted')
      do
        call nextln ( comstr , ndsi , ndse )
        read (ndsi,'(a)',end=2001,err=2002) line
        if ( line(1:16) .eq. 'end of namelists' ) then
          exit
        else
          write (ndss,'(a)') line
        endif
      end do
    end if
    write (ndso,916)
    !
    ! 6.d define sin.
    ! 6.d.1 stresses
    !
    !
    !
    ! 6.d.2 linear input
    !
    !
    !
    ! 6.d.3 exponential input
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    ! 6.e define snl.
    !
    !
    !
    !
    !
    !
    !
    !
    !
    ! 6.f define sds.
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    ! 6.g define sbt.
    !
    !
    !
    !
    !
    ! 6.h define sdb.
    !
    !
    !
    !
    !
    ! 6.i define str.
    !
    !
    ! 6.j define sbs.
    !
    !
    ! 6.k define sxx and sic.
    !
    !
    !
    !
    !
    !
    ! 6.l read unstructured data
    ! initialisation of logical related to unstructured grid
    ugobcauto = .true.
    ugbccfl = .true.
    ugobcdepth= -10.
    ugobcok = .false.
    ugobcfile = 'unset'
    expfsn    = .true.
    expfspsi  = .false.
    expfsfct  = .false.
    impfsn    = .false.
    imptotal  = .false.
    exptotal  = .false.
    imprefraction = .false.
    impfreqshift = .false.
    impsource = .false.
    setup_apply_wlv = .true.
    solverthr_setup=1e-6
    crit_dep_setup=0.1
    jgs_terminate_maxiter = .true.
    jgs_terminate_difference = .true.
    jgs_terminate_norm = .false.
    jgs_limiter = .false.
    jgs_limiter_func = 1
    jgs_block_gauss_seidel = .true.
    jgs_use_jacobi = .true.
    jgs_maxiter=100
    jgs_pmin = 1
    jgs_diff_thr = 1.e-10
    jgs_norm_thr = 1.e-20
    jgs_nlevel = 0
    jgs_source_nonlinear = .false.
    ! read data from the unstructured devoted namelist
    call readnl ( ndss, 'unst', status )
    b_jgs_use_jacobi = jgs_use_jacobi
    b_jgs_terminate_maxiter = jgs_terminate_maxiter
    b_jgs_terminate_difference = jgs_terminate_difference
    b_jgs_terminate_norm = jgs_terminate_norm
    b_jgs_limiter = jgs_limiter
    b_jgs_limiter_func = jgs_limiter_func
    b_jgs_block_gauss_seidel = jgs_block_gauss_seidel
    b_jgs_maxiter = jgs_maxiter
    b_jgs_pmin = jgs_pmin
    b_jgs_diff_thr = jgs_diff_thr
    b_jgs_norm_thr = jgs_norm_thr
    b_jgs_nlevel = jgs_nlevel
    b_jgs_source_nonlinear = jgs_source_nonlinear
    nbsel=0
    if (expfsn)   nbsel = nbsel+1
    if (expfspsi) nbsel = nbsel+1
    if (expfsfct) nbsel = nbsel+1
    if (impfsn)   nbsel = nbsel+1
    if (imptotal) nbsel = nbsel+1
    if (exptotal) nbsel = nbsel+1
    if (gtype .eq. ungtype) then
      if (nbsel .ne. 1) then
        if (nbsel .gt. 1) then
          write (ndse,*) 'more than one unstructured scheme selected'
          call extcde ( 19 )
        else if (nbsel .eq. 0) then
          write (ndse,*) 'nothing selected from the unstructured part'
          call extcde ( 19 )
        end if
      end if
    end if
    !
    ! 6.m select propagation scheme
    !
    write (ndso,950)
    !
    nrprop = 0
    flprop = .true.
    pname  = '                              '
    j = len_trim(pname)
    !
    !
    if ( (flcx.or.flcy.or.flcth.or.flck) .and. .not. flprop ) then
      write (ndse,1030)
      call extcde ( 20 )
    end if
    !
    if ( .not.(flcx.or.flcy.or.flcth.or.flck) .and. flprop ) then
      write (ndse,1031)
    end if
    !
    if ( nrprop.eq.0 ) then
      write (ndse,1032)
      call extcde ( 21 )
    end if
    !
    if ( nrprop .gt. 1 ) then
      write (ndse,1033) nrprop
      call extcde ( 22 )
    end if
    !
    ! 6.m parameters for propagation scheme
    !
    write (ndso,951) pname
    !
    cfltm  =  0.7
    !
    !
    !
    !
    !
    !
    !
    if (gtype.ne.ungtype) then
    endif
    !
    ctmax  = cfltm
    !
    !
    ! 6.n set miscellaneous parameters (ice, seeding, numerics ... )
    !
    cice0  = 0.5
    cicen  = 0.5
    lice   = 0.
    icehfac= 1.0
    icehmin= 0.2  ! the 0.2 value is arbitrary and needs to be tuned.
    icehinit= 0.5
    icesln = 1.0
    icewind= 1.0
    icesnl = 1.0
    icesds = 1.0
    icehdisp= 0.6 ! prevent from convergence crash in w3dispmd in the presence of ice, should be tuned
    iceddisp= 80
    icefdisp= 2
    gshift = 0.0d0
    pmove  = 0.5
    xseed  = 1.
    flagtr = 0
    xp     = 0.15
    xr     = 0.10
    xfilt  = 0.05
    ihm    = 100
    hspm   = 0.05
    wsm    = 1.7
    wsc    = 0.333
    flc    = .true.
    trckcmpr = .true.
    nosw   = 5
    !
    ! gas fluxes
    !
    aircmin   = 2.0  ! cmin for whitecap coverage and entrained air
    airgb     = 0.2  ! volume of entrained air constant (deike et al. 2017)
    !
    ptm    = 1    ! default to standard ww3 partitioning. c. bunney
    ptfc   = 0.1  ! part. method 5 cutoff freq default. c. bunney
    fmiche = 1.6
    rwndc  = 1.
    wcor1  = 99.
    wcor2  = 0.
    btbet  = 1.2 ! β for c / [u cos(θ - φ)] < β
    ! variables for space-time extremes
    !  default negative values make w3iogomd switch off space-time extremes
    !  forces user to provide namelist if wanting to compute ste parameters
    stdx = -1.
    stdy = -1.
    stdt = -1.
    icedisp = .false.
    caltype = 'standard'
    ! variables for 3d array output
    e3d=0
    i1e3d=1
    i2e3d=nk
    p2sf   = 0
    i1p2sf = 1
    i2p2sf = 15
    us3d   = 0
    i1us3d = 1
    i2us3d = nk
    ussp=0
    iussp=1
    stk_wn(:)=0.0
    stk_wn(1)=tpi/100. !set default decay of 100 m for stokes drift
    th1mf=0
    i1th1m=1
    i2th1m=nk
    sth1mf=0
    i1sth1m=1
    i2sth1m=nk
    th2mf=0
    i1th2m=1
    i2th2m=nk
    sth2mf=0
    i1sth2m=1
    i2sth2m=nk
    !
    facberg=1.
    !
    !
    !
    !
    !fixme: if usecgice = .true., don't allow use of ic3maxthk<100.0
    !
    !
    !
    !
    !
    !
    call readnl ( ndss, 'outs', status )
    write (ndso,4970) status
    !
    !
    ! output of frequency spectra, th1m ...
    !
    e3df(1,1) = e3d
    e3df(2,1) = min(max(1,i1e3d),nk)
    e3df(3,1) = min(max(1,i2e3d),nk)
    e3df(1,2) = th1mf
    e3df(2,2) = min(max(1,i1th1m),nk)
    e3df(3,2) = min(max(1,i2th1m),nk)
    e3df(1,3) = sth1mf
    e3df(2,3) = min(max(1,i1sth1m),nk)
    e3df(3,3) = min(max(1,i2sth1m),nk)
    e3df(1,4) = th2mf
    e3df(2,4) = min(max(1,i1th2m),nk)
    e3df(3,4) = min(max(1,i2th2m),nk)
    e3df(1,5) = sth2mf
    e3df(2,5) = min(max(1,i1sth2m),nk)
    e3df(3,5) = min(max(1,i2sth2m),nk)
    !
    ! output of microseismic source spectra
    !
    p2msf(1) = p2sf
    p2msf(2) = min(max(1,i1p2sf),nk)
    p2msf(3) = min(max(1,i2p2sf),nk)
    !
    ! output of stokes drift profile
    !
    us3df(1) = us3d
    us3df(2) = max( 1 , min( nk, i1us3d) )
    us3df(3) = max( 1 , min( nk, i2us3d) )
    !
    ! output of stokes drift partitions
    !
    usspf(1) = ussp
    usspf(2) = max( 1 , min(25, iussp ) )
    if (iussp.gt.25) then
      write(ndse,*) ' *** wavewatch iii error in ww3_grid:'
      write(ndse,*) "  stokes drift partition outputs not    "
      write(ndse,*) "   intended for use with more than 25   "
      write(ndse,*) "   partitions.  please reduce iussp     "
      write(ndse,*) "   specified in ww3_grid.inp to proceed "
      call extcde( 31)
    endif
    ussp_wn = 0.0 ! initialize to 0s
    do j=1,usspf(2)
      ussp_wn(j) = stk_wn(j)
    enddo
    !
    write (ndso,4971) p2msf(1:3)
    write (ndso,4972) us3df(1:3)
    write (ndso,4973) e3df(1:3,1)
    write (ndso,4974) usspf(1:2)
    do j=1,usspf(2)
      write(ndso,4975) j,ussp_wn(j)
    enddo
    !
    call readnl ( ndss, 'misc', status )
    write (ndso,960) status
    !
    if ( flagtr.lt.0 .or. flagtr.gt.6 ) flagtr = 0
    cicen  = min ( 1. , max ( 0. , cicen ) )
    icesln  = min ( 1. , max ( 0. , icesln ) )
    icewind = min ( 1. , max ( 0. , icewind ) )
    icesds  = min ( 1. , max ( 0. , icesds ) )
    icesnl  = min ( 1. , max ( 0. , icesnl ) )
    ficen  = cicen
    gridshift=gshift
    icescales(1)=icesln
    icescales(2)=icewind
    icescales(3)=icesnl
    icescales(4)=icesds
    cmprtrck=trckcmpr
    cice0  = min ( cicen , max ( 0. , cice0 ) )
    ficel  = lice
    iicehmin  = icehmin
    iicehfac  = icehfac
    iicehinit  = icehinit
    iicedisp= icedisp
    iicehdisp  = icehdisp
    iiceddisp  = iceddisp
    iicefdisp  = icefdisp
    pmove  = max ( 0. , pmove )
    pfmove = pmove
    !
    btbeta = min(max (1., btbet), 2.)
    aaircmin = alog(grav/aircmin/sig(1))/alog(xfr)+1 ! goes from phase speed c=g/sig to index
    aairgb = airgb
    !
    ! notes: presently, if we select cice0.ne.cicen requires an obstruction
    !     grid, that is initialized with zeros as default.
    if ( flagtr .lt. 3 ) then
      if (cice0.ne.cicen) then
        cice0 = cicen
        if (status=='(user def. values) :')  write (ndso,2961)
      end if
    end if
    write (ndso,961) cice0, cicen
    write (ndso,8972) icewind
    fice0  = cice0
    ! variables for space-time extremes
    stexu = stdx
    if ( stdy .le. 0. ) then
      stdy = stdx
    end if
    steyu = stdy
    stedu = stdt
    if ( stdx .gt. 0 ) then
      write (ndso,1040) stdx
      write (ndso,1041) stdy
    else
      write (ndso,1042)
    end if
    if ( stdt .gt. 0 ) then
      write (ndso,1043) stdt
    else
      write (ndso,1044)
    end if
    !
    write (ndso,1972) trckcmpr
    facsd  = xseed
    !
    xp     = max ( 1.e-6 , xp )
    xr     = max ( 1.e-6 , xr )
    xrel   = xr
    xfilt  = max ( 0. , xfilt )
    xflt   = xfilt
    write (ndso,965) xp, xr, xfilt
    facp   = xp / pi * 0.62e-3 * tpi**4 / grav**2
    !
    ihmax  = max ( 50, ihm )
    hspmin = max ( 0.0001 , hspm )
    wsmult = max ( 1. , wsm )
    wscut  = min ( 1.0001 , max ( 0. , wsc ) )
    flcomb = flc
    noswll = max ( 1 , nosw )
    ptmeth = ptm  ! partitioning method. chris bunney (jan 2016)
    ptfcut = ptfc ! freq cutoff for partitiong method 5
    pmnam2 = ""
    if( ptmeth .eq. 1 ) then
      pmname = "ww3 default"
    else if( ptmeth .eq. 2 ) then
      pmname = "watershedding plus wind cut-off"
    else if( ptmeth .eq. 3 ) then
      pmname = "watershedding only"
      wscut = 0.0 ! we don't want to classify by ws frac
      pmnam2 = "wsc set to 0.0"
    else if( ptmeth .eq. 4 ) then
      pmname = "wind speed cut-off only"
      pmnam2 = "wsc set to 0.0, nosw set to 1"
      wscut = 0.0 ! we don't want to classify by ws frac
      noswll = 1  ! only ever one swell
    else if( ptmeth .eq. 5 ) then
      write(pmname, '("2-band hi/low cutoff at ", f4.2,"hz")') ptfcut
      pmnam2 = "wsc set to 0.0, nosw set to 1"
      wscut = 0.0 ! we don't want to classify by ws frac
      noswll = 1  ! only ever one swell
    else
      write( ndse, * )                                                &
           "*** error - unknown partitioing method (ptm)! ***"
      call exit(1)
    endif
    if ( flcomb ) then
      j      = 1
    else
      j      = 2
    end if
    write (ndso,966) ihmax, hspmin, wsmult, wscut, yesxno(j), noswll
    write (ndso,5971) pmname
    if( pmnam2 .ne. "" ) write (ndso,5972) pmnam2
    !!    write (ndso,966) ihmax, hspmin, wsmult, wscut, yesxno(j)
    !
    fhmax  = max ( 0.01 , fmiche )
    j      = 2
    write (ndso,967) fhmax, fhmax/sqrt(2.), yesxno(j)
    if ( fhmax.lt.0.50 .and. j.eq.1 ) write (ndst,968)
    !
    if (trim(caltype) .ne. 'standard' .and.                           &
         trim(caltype) .ne. '360_day'  .and.                           &
         trim(caltype) .ne. '365_day' ) goto 2003
    write (ndst,1973) caltype
    write (ndso,*)
    !
    ! 6.x read values for fld stress calculation
    !
    !
    !
    ! 6.o end of namelist processing
    !
    if (flgnml) then
      close (ndss)
    else
      close (ndss,status='delete')
    end if
    !
    if ( flnmlo ) then
      write (ndso,917)
      if ( .not. flstb2 ) then
      else
      end if
      !
      !
      write (ndso,2956) ugbccfl, ugobcauto, ugobcdepth,trim(ugobcfile), &
           expfsn, expfspsi, expfsfct, impfsn, exptotal,&
           imptotal, imprefraction, impfreqshift,      &
           impsource, setup_apply_wlv,                 &
           jgs_terminate_maxiter,                      &
           jgs_terminate_difference,                   &
           jgs_terminate_norm,                         &
           jgs_limiter,                                &
           jgs_limiter_func,                           &
           jgs_use_jacobi,                             &
           jgs_block_gauss_seidel,                     &
           jgs_maxiter,                                &
           jgs_pmin,                                   &
           jgs_diff_thr,                               &
           jgs_norm_thr,                               &
           jgs_nlevel,                                 &
           jgs_source_nonlinear
      !
      write (ndso,2976)    p2sf, i1p2sf, i2p2sf,                    &
           us3d, i1us3d, i2us3d,                    &
           ussp, iussp,                             &
           e3d, i1e3d, i2e3d,                       &
           th1mf, i1th1m, i2th1m,                   &
           sth1mf, i1sth1m, i2sth1m,                &
           th2mf, i1th2m, i2th2m,                   &
           sth2mf, i1sth2m, i2sth2m
      !
      !
      !
      !
      !
      !
      !
      !
      !
      !
      if ( flcomb ) then
        write (ndso,2966) cice0, cicen, lice, pmove, xseed, flagtr, &
             xp, xr, xfilt, ihmax, hspmin, wsmult, &
             wscut, '.true.', noswll, fhmax,       &
             rwndc, wcor1, wcor2, facberg, gshift, &
             stdx, stdy, stdt, icehmin, icehfac,   &
             icehinit, icedisp, icehdisp,          &
             icesln, icewind, icesnl, icesds,      &
             iceddisp,icefdisp, caltype, trckcmpr, &
             btbeta
      else
        write (ndso,2966) cice0, cicen, lice, pmove, xseed, flagtr, &
             xp, xr, xfilt, ihmax, hspmin, wsmult, &
             wscut, '.false.', noswll, fhmax,      &
             rwndc, wcor1, wcor2, facberg, gshift, &
             stdx, stdy, stdt,  icehmin, icehfac,  &
             icehinit, icedisp, icehdisp,          &
             icesln, icewind, icesnl, icesds,      &
             iceddisp, icefdisp, caltype, trckcmpr,&
             btbeta
      end if
      !
      !
      write (ndso,918)
    end if
    !
    ! 6.p set various other values ...
    ! ... tail in integration       --> scale factor for a to e conv
    !
    fte    = 0.25 * sig(nk)      * dth * sig(nk)
    ftf    = 0.20                * dth * sig(nk)
    ftwn   = 0.20 * sqrt(grav)   * dth * sig(nk)
    fttr   = ftf
    ftwl   = grav / 6. / sig(nk) * dth * sig(nk)
    !
    !
    ! ... high frequency cut-off
    !
    fxfm   = 2.5
    fxpm   = 4.0
    fxpm   = fxpm * grav / 28.
    fxfm   = fxfm * tpi
    xfc    = 3.0
    !
    facti1 = 1. / log(xfr)
    facti2 = 1. - log(tpi*fr1) * facti1
    !
    ! setting of fachf moved to before !/nl2 set-up for consistency
    !
    fachfa = xfr**(-fachf-2)
    fachfe = xfr**(-fachf)
    !
    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! 7.  read and prepare the grid.
    ! 7.a type of grid
    !
    if (flgnml) then
      gstrg=trim(nml_grid%type)
      if (trim(nml_grid%coord).eq.'sphe') flagll=.true.
      if (trim(nml_grid%coord).eq.'cart') flagll=.false.
      cstrg=trim(nml_grid%clos)
    else
      call nextln ( comstr , ndsi , ndse )
      read (ndsi,*,end=2001,err=2002) gstrg, flagll, cstrg
      call nextln ( comstr , ndsi , ndse )
    end if
    select case (trim(gstrg))
    case ('rect')
      gtype = rlgtype
      write (ndso,3000) 'rectilinear'
    case ('curv')
      gtype = clgtype
      write (ndso,3000) 'curvilinear'
    case ('unst')
      gtype = ungtype
      write (ndso,3000) 'unstructured'
      !!li  add smc grid type option.  jgli12oct2020
    case ('smcg')
      gtype = smctype
      write (ndso,3000) 'smc grid'
    case default
      write (ndse,1007) trim(gstrg)
      call extcde ( 25 )
    end select
    !
    if ( flagll ) then
      factor = 1.
      write (ndso,3001) 'spherical'
    else
      factor = 1.e-3
      write (ndso,3001) 'cartesian'
    end if
    !
    !     only process grid closure string for logically rectangular grids.
    !     closure setting for unstructured grids is none.
    iclose = iclose_none
    if ( gtype.ne.ungtype ) then
      select case (trim(cstrg))
      case ('none')
        iclose = iclose_none
        write (ndso,3002) 'none'
      case ('smpl')
        iclose = iclose_smpl
        write (ndso,3002) 'simple'
      case ('trpl')
        write (ndse,'(/2a)') ' *** warning ww3_grid: tripole ',  &
             'grid closure implementation is incomplete ***'
        iclose = iclose_trpl
        write (ndso,3002) 'tripole'
        if ( gtype.eq.rlgtype ) then
          write (ndse,1009)
          call extcde ( 25 )
        end if
      case default
        ! check for old style global input
        select case (trim(cstrg))
        case ('t','t','.tru','.tru')
          iclose = iclose_smpl
          write (ndso,3002) 'simple'
          write (ndse,1013)
        case ('f','f','.fal','.fal')
          iclose = iclose_none
          write (ndso,3002) 'none'
          write (ndse,1013)
        case default
          write (ndse,1012) trim(cstrg)
          call extcde ( 25 )
        end select
      end select
      if ( iclose.ne.iclose_none .and. .not.flagll ) then
        write (ndse,1008)
        call extcde ( 25 )
      end if
    end if !gtype.ne.ungtype
    !
    ! 7.b size of grid
    !
    if (flgnml) then
      select case ( gtype )
        !!li  smctype shares domain info with rlgtype.  jgli12oct2020
      case ( rlgtype, smctype )
        nx = nml_rect%nx
        ny = nml_rect%ny
        nx = max ( 3 , nx )
        ny = max ( 3 , ny )
        write (ndso,3003) nx, ny
      case ( clgtype )
        nx = nml_curv%nx
        ny = nml_curv%ny
        nx = max ( 3 , nx )
        ny = max ( 3 , ny )
        write (ndso,3003) nx, ny
      case ( ungtype )
        ny=1
      end select
    else
      if ( gtype.ne.ungtype) then
        call nextln ( comstr , ndsi , ndse )
        read (ndsi,*,end=2001,err=2002) nx, ny
        nx     = max ( 3 , nx )
        ny     = max ( 3 , ny )
        write (ndso,3003) nx, ny
      else
        ny =1
      end if
    end if
    !
    ! propagation specific to unstructured grids
    !
    do_change_wlv=.false.
    if ( gtype.eq.ungtype) then
      unstschemes = 0
      if (expfsn)   unstschemes(1) = 1
      if (expfspsi) unstschemes(2) = 1
      if (expfsfct) unstschemes(3) = 1
      if (impfsn)   unstschemes(4) = 1
      if (imptotal) unstschemes(5) = 1
      if (exptotal) unstschemes(6) = 1
      if (sum(unstschemes) .eq. 0) then
        write(ndse,*) 'no unst scheme selected'
        call extcde ( 19 )
      else if (sum(unstschemes) .gt. 1) then
        write(ndse,*) 'more than one unst scheme selected'
        call extcde ( 19 )
      endif
      unstscheme=-1
      do ix=1,6
        if (unstschemes(ix).eq.1) then
          unstscheme=ix
          exit
        end if
      end do
      fsbccfl = ugbccfl
      select case (unstscheme)
      case (1)
        fsn = expfsn
        pname2 = 'n explicit (fluctuation splitting) '
      case (2)
        fspsi = expfspsi
        pname2 = 'psi explicit (fluctuation splitting)  '
      case (3)
        fsfct = expfsfct
        pname2 = ' flux corrected transport explicit'
      case (4)
        fsnimp = impfsn
        pname2 = 'n implicit (fluctuation splitting) '
      case (5)
        fstotalimp = imptotal
        pname2 = 'n implicit (fluctuation splitting) for total implicit'
      case (6)
        fstotalexp = exptotal
        pname2 = 'n explicit (fluctuation splitting) for one exchange explicit dc hpcf '
      end select
      if (fstotalimp .or. fstotalexp) then
        lpdlib = .true.
      endif
      !
      if (sum(unstschemes).gt.1) write(ndso,1035)
      write (ndso,2951) pname2
      if (imprefraction .and. imptotal .and. flcth) then
        fsrefraction = .true.
        pname2 = 'refraction done implicitly'
        write (ndso,2951) pname2
      else
        fsrefraction = .false.
      end if
      if (impfreqshift .and. imptotal .and. flck) then
        fsfreqshift = .true.
        pname2 = 'frequency shifting done implicitly'
        write (ndso,2951) pname2
      else
        fsfreqshift = .false.
      end if
      if (impsource .and. imptotal .and. flsou) then
        fssource = .true.
        pname2 = 'source terms integrated implicitly'
        write (ndso,2951) pname2
      else
        fssource = .false.
      end if
      if (setup_apply_wlv) then
        do_change_wlv = setup_apply_wlv
        pname2 = 'wave setup is added to the wlv'
        write (ndso,2952) pname2
      end if
      solverthr_stp = solverthr_setup
      crit_dep_stp  = crit_dep_setup
    end if
    !
    ! 7.c grid coordinates (branch here based on grid type)
    !
    if ( gtype.ne.ungtype) allocate ( xgrdin(nx,ny), ygrdin(nx,ny) )
    select case ( gtype )
      !
      ! 7.c.1 rectilinear grid
      !
      !!li  smc grid shares domain info with rlgtype.   jgli12oct2020
    case ( rlgtype, smctype )
      !
      if (flgnml) then
        sx = nml_rect%sx
        sy = nml_rect%sy
        vsc = nml_rect%sf
        x0 = nml_rect%x0
        y0 = nml_rect%y0
        vsc0 = nml_rect%sf0
      else
        call nextln ( comstr , ndsi , ndse )
        read (ndsi,*,end=2001,err=2002) sx, sy, vsc
        call nextln ( comstr , ndsi , ndse )
        read (ndsi,*,end=2001,err=2002) x0, y0, vsc0
      end if
      !
      vsc    = max ( 1.e-7 , vsc )
      sx     = sx / vsc
      sy     = sy / vsc
      sx     = max ( 1.e-7 , sx )
      sy     = max ( 1.e-7 , sy )
      if ( iclose.eq.iclose_smpl ) sx = 360. / real(nx)
      !
      vsc0    = max ( 1.e-7 , vsc0 )
      x0     = x0 / vsc0
      y0     = y0 / vsc0
      !
      if ( flagll ) then
        write (ndso,3004) factor*sx, factor*sy,         &
             factor*x0, factor*(x0+real(nx-1)*sx),    &
             factor*y0, factor*(y0+real(ny-1)*sy)
      else
        write (ndso,3005) factor*sx, factor*sy,         &
             factor*x0, factor*(x0+real(nx-1)*sx),    &
             factor*y0, factor*(y0+real(ny-1)*sy)
      end if
      !
      do iy=1, ny
        do ix=1, nx
          xgrdin(ix,iy) = x0 + real(ix-1)*sx
          ygrdin(ix,iy) = y0 + real(iy-1)*sy
        end do
      end do
      !
      ! 7.c.2 curvilinear grid
      !
    case ( clgtype )
      !
      ! 7.c.2.a process x-coordinates
      !
      if (flgnml) then
        ndsg = nml_curv%xcoord%idf
        vsc = nml_curv%xcoord%sf
        vof = nml_curv%xcoord%off
        idla = nml_curv%xcoord%idla
        idfm = nml_curv%xcoord%idfm
        rform = trim(nml_curv%xcoord%format)
        from = trim(nml_curv%xcoord%from)
        fname = trim(nml_curv%xcoord%filename)
      else
        call nextln ( comstr , ndsi , ndse )
        read (ndsi,*,end=2001,err=2002) ndsg, vsc, vof, &
             idla, idfm, rform, from, fname
      end if
      !
      if (idla.lt.1 .or. idla.gt.4) idla   = 1
      if (idfm.lt.1 .or. idfm.gt.3) idfm   = 1
      !
      write (ndso,3006) ndsg, vsc, vof, idla, idfm
      if (idfm.eq.2) write (ndso,3008) trim(rform)
      if (from.eq.'name' .and. ndsg.ne.ndsi) &
           write (ndso,3009) trim(fname)
      !
      if ( ndsg .eq. ndsi ) then
        if ( idfm .eq. 3 ) then
          write (ndse,1004) ndsg
          call extcde (23)
        else
          if (.not.flgnml) then
            call nextln ( comstr , ndsi , ndse )
          end if
        end if
      else
        if ( idfm .eq. 3 ) then
          if (from.eq.'name') then
            open (ndsg,file=trim(fnmpre)//trim(fname),&
                 form='unformatted', convert=file_endian,                 &
                 status='old',err=2000,iostat=ierr)
          else
            open (ndsg,                               &
                 form='unformatted', convert=file_endian,                 &
                 status='old',err=2000,iostat=ierr)
          end if
        else
          if (from.eq.'name') then
            open (ndsg,file=trim(fnmpre)//trim(fname),&
                 status='old',err=2000,iostat=ierr)
          else
            open (ndsg,                               &
                 status='old',err=2000,iostat=ierr)
          end if
        end if !idfm
      end if !ndsg
      !
      call ina2r ( xgrdin, nx, ny, 1, nx, 1, ny, ndsg, ndst, ndse, &
           idfm, rform, idla, vsc, vof)
      !
      ! 7.c.2.b process y-coordinates
      !
      if (flgnml) then
        ndsg = nml_curv%ycoord%idf
        vsc = nml_curv%ycoord%sf
        vof = nml_curv%ycoord%off
        idla = nml_curv%ycoord%idla
        idfm = nml_curv%ycoord%idfm
        rform = trim(nml_curv%ycoord%format)
        from = trim(nml_curv%ycoord%from)
        fname = trim(nml_curv%ycoord%filename)
      else
        call nextln ( comstr , ndsi , ndse )
        read (ndsi,*,end=2001,err=2002) ndsg, vsc, vof, &
             idla, idfm, rform, from, fname
      end if
      !
      if (idla.lt.1 .or. idla.gt.4) idla   = 1
      if (idfm.lt.1 .or. idfm.gt.3) idfm   = 1
      !
      write (ndso,3007) ndsg, vsc, vof, idla, idfm
      if (idfm.eq.2) write (ndso,3008) trim(rform)
      if (from.eq.'name' .and. ndsg.ne.ndsi) &
           write (ndso,3009) trim(fname)
      !
      if ( ndsg .eq. ndsi ) then
        if ( idfm .eq. 3 ) then
          write (ndse,1004) ndsg
          call extcde (23)
        else
          if (.not.flgnml) then
            call nextln ( comstr , ndsi , ndse )
          end if
        end if
      else
        if ( idfm .eq. 3 ) then
          if (from.eq.'name') then
            open (ndsg,file=trim(fnmpre)//trim(fname),&
                 form='unformatted', convert=file_endian,                 &
                 status='old',err=2000,iostat=ierr)
          else
            open (ndsg,                               &
                 form='unformatted', convert=file_endian,                 &
                 status='old',err=2000,iostat=ierr)
          end if
        else
          if (from.eq.'name') then
            open (ndsg,file=trim(fnmpre)//trim(fname),&
                 status='old',err=2000,iostat=ierr)
          else
            open (ndsg,                               &
                 status='old',err=2000,iostat=ierr)
          end if
        end if !idfm
      end if !ndsg
      !
      call ina2r ( ygrdin, nx, ny, 1, nx, 1, ny, ndsg, ndst, ndse, &
           idfm, rform, idla, vsc, vof)
      !
      ! 7.c.2.c check for obvious errors in grid definition or input
      !
      ! ....... check for inverted grid (can result from wrong idla)
      if ( (xgrdin(2,1)-xgrdin(1,1))*(ygrdin(1,2)-ygrdin(1,1)) .lt. &
           (ygrdin(2,1)-ygrdin(1,1))*(xgrdin(1,2)-xgrdin(1,1)) ) then
        write (ndse,1011) idla
        !.........notes: here, we are checking to make sure that the j axis is ~90 degrees
        !................counter-clockwise from the i axis (the standard cartesian setup).
        !................so, it is a check on the handedness of the grid.
        !................we have confirmed for one case that a left-handed grid produces
        !................errors in scrip. we have not confirmed that left-handed grids necessarily
        !................produce errors in single-grid simulations, or that they necessarily
        !................produce errors in all multi-grid simulations.
        !................note that transposing or flipping a grid will generally change the handedness.
        call extcde (25)
      end if
      !
      ! 7.c.3 unstructured grid
      !
    case ( ungtype )
      !
      maxx = 0.
      maxy = 0.
      dxymax = 0.
      write (ndso,1150)
      if (flgnml) then
        zlim = nml_grid%zlim
        dmin = nml_grid%dmin
        ndsg = nml_unst%idf
        vsc = nml_unst%sf
        idla = nml_unst%idla
        idfm = nml_unst%idfm
        rform = trim(nml_unst%format)
        from = 'name'
        fname = trim(nml_unst%filename)
        ugobcfile = trim(nml_unst%ugobcfile)
      end if
    end select !gtype
    !
    ! 7.d depth information for grid
    !
    if (flgnml) then
      if (gtype.ne.ungtype) then
        zlim = nml_grid%zlim
        dmin = nml_grid%dmin
        ndsg = nml_depth%idf
        vsc = nml_depth%sf
        idla = nml_depth%idla
        idfm = nml_depth%idfm
        rform = trim(nml_depth%format)
        from = trim(nml_depth%from)
        fname = trim(nml_depth%filename)
      end if
    else
      call nextln ( comstr , ndsi , ndse )
      read (ndsi,*,end=2001,err=2002) zlim, dmin, ndsg, vsc, idla,    &
           idfm, rform, from, fname
    end if
    !
    dmin    = max ( 1.e-3 , dmin )
    if (   abs(vsc) .lt. 1.e-7  ) vsc    = 1.
    if (idla.lt.1 .or. idla.gt.4) idla   = 1
    if (idfm.lt.1 .or. idfm.gt.3) idfm   = 1
    !
    write (ndso,972) ndsg, zlim, dmin, vsc, idla, idfm
    if (idfm.eq.2) write (ndso,973) trim(rform)
    if (from.eq.'name' .and. ndsg.ne.ndsi) &
         write (ndso,974) trim(fname)
    !
    ! 7.e read bottom depths
    !
    if ( gtype.ne.ungtype ) then
      !
      ! reading depths on structured grid
      !
      allocate ( zbin(nx,ny), obsx(nx,ny), obsy(nx,ny) )
      !
      !       initialize subgrid obstructions with zeros.
      zbin(:,:)=0.
      obsx(:,:)=0.
      obsy(:,:)=0.
      !li suspended for smc grid, which uses depth stored in its cell array.
      !li               jgli15oct2014
      if( gtype .ne. smctype ) then
        !
        if ( ndsg .eq. ndsi ) then
          if ( idfm .eq. 3 ) then
            write (ndse,1004) ndsg
            call extcde (23)
          else
            call nextln ( comstr , ndsi , ndse )
          end if
        else  ! ndsg.ne.ndsi
          if ( idfm .eq. 3 ) then
            if (from.eq.'name') then
              open (ndsg,file=trim(fnmpre)//trim(fname), &
                   form='unformatted', convert=file_endian,&
                   status='old',err=2000,iostat=ierr)
            else
              open (ndsg, form='unformatted', convert=file_endian,                &
                   status='old',err=2000,iostat=ierr)
            end if
          else
            if (from.eq.'name') then
              open (ndsg,file=trim(fnmpre)//trim(fname),  &
                   status='old',err=2000,iostat=ierr)
            else
              open (ndsg,                                     &
                   status='old',err=2000,iostat=ierr)
            end if
          end if
        end if  !( ndsg .eq. ndsi )
        !
        call ina2r ( zbin, nx, ny, 1, nx, 1, ny, ndsg, ndst, ndse,      &
             idfm, rform, idla, vsc, 0.0)
        !
        !li     end of if( gtype .ne. smctype ) block
      endif
      !
    else
      !
      ! reading depths on unstructured grid (this also sets number of mesh points, nx)
      !
      call readmsh(ndsg,fname)
      allocate(zbin(nx, ny),obsx(nx,ny),obsy(nx,ny))
      zbin(:,1) = vsc * zb(:)
      !
      ! subgrid obstructions are not yet handled in unstructured grids
      !
      obsx(:,:)=0.
      obsy(:,:)=0.
    end if
    !
    ! 7.f set up temporary map
    !
    allocate ( tmpsta(ny,nx), tmpmap(ny,nx) )
    tmpsta = 0
    !
    if (gtype .eq. ungtype) then
      tmpsta = 1
    else
      do iy=1, ny
        do ix=1, nx
          if ( zbin(ix,iy) .le. zlim ) tmpsta(iy,ix) = 1
        end do
      end do
    endif
    !
    !li   suspended for smc grid.  jgli15oct2014
    if( gtype .ne. smctype ) then
      !
      ! 7.g subgrid information
      !
      trflag = flagtr
      if ( trflag.gt.6 .or. trflag.lt.0 ) trflag = 0
      !
      if ( trflag .eq. 0 ) then
        write (ndso,976) 'not available.'
      else if ( trflag.eq.1 .or. trflag.eq.3 .or. trflag.eq.5 ) then
        write (ndso,976) 'in between grid points.'
      else
        write (ndso,976) 'at grid points.'
      end if
      !
      if ( trflag .ne. 0 ) then
        !
        ! 7.g.1 info from input file
        !
        if (flgnml) then
          ndstr = nml_obst%idf
          vsc = nml_obst%sf
          idla = nml_obst%idla
          idft = nml_obst%idfm
          rform = trim(nml_obst%format)
          from = trim(nml_obst%from)
          tname = trim(nml_obst%filename)
        else
          call nextln ( comstr , ndsi , ndse )
          read (ndsi,*,end=2001,err=2002) ndstr, vsc, idla, idft, rform, &
               from, tname
        end if
        !
        if (   abs(vsc) .lt. 1.e-7  ) vsc    = 1.
        if (idla.lt.1 .or. idla.gt.4) idla   = 1
        if (idft.lt.1 .or. idft.gt.3) idft   = 1
        !
        write (ndso,977) ndstr, vsc, idla, idft
        if (idft.eq.2) write (ndso,973) rform
        if (from.eq.'name' .and. ndsg.ne.ndstr) write (ndso,974) tname
        !
        ! 7.g.2 open file and check if necessary
        !
        if ( ndstr .eq. ndsi ) then
          if ( idft .eq. 3 ) then
            write (ndse,1004) ndstr
            call extcde (23)
          else
            call nextln ( comstr , ndsi , ndse )
          end if
        else if ( ndstr .eq. ndsg ) then
          if ( ( idfm.eq.3 .and. idft.ne.3 ) .or.                 &
               ( idfm.ne.3 .and. idft.eq.3 ) ) then
            write (ndse,1005) idfm, idft
            call extcde (24)
          end if
        else
          if ( idft .eq. 3 ) then
            if (from.eq.'name') then
              open (ndstr,file=trim(fnmpre)//tname,             &
                   form='unformatted', convert=file_endian,status='old',err=2000, &
                   iostat=ierr)
            else
              open (ndstr,           form='unformatted', convert=file_endian,      &
                   status='old',err=2000,iostat=ierr)
            end if
          else
            if (from.eq.'name') then
              open (ndstr,file=trim(fnmpre)//tname,             &
                   status='old',err=2000,iostat=ierr)
            else
              open (ndstr,                                    &
                   status='old',err=2000,iostat=ierr)
            end if
          end if
        end if
        !
        ! 7.g.3 read the data
        !
        call ina2r ( obsx, nx, ny, 1, nx, 1, ny, ndstr, ndst, ndse, &
             idft, rform, idla, vsc, 0.0)
        !
        if ( ndstr .eq. ndsi ) call nextln ( comstr , ndsi , ndse )
        !
        call ina2r ( obsy, nx, ny, 1, nx, 1, ny, ndstr, ndst, ndse, &
             idft, rform, idla, vsc, 0.0)
        !
        ! 7.g.4 limit
        !
        do ix=1, nx
          do iy=1, ny
            obsx(ix,iy) = max( 0. , min(1.,obsx(ix,iy)) )
            obsy(ix,iy) = max( 0. , min(1.,obsy(ix,iy)) )
          end do
        end do
        !
        write (ndso,*)
        !
      end if ! trflag
      !
      !li     end of if( gtype .ne. smctype ) block
    end if
    !
    !
    !
    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! 8.  finalize status maps
    ! 8.a defines open boundary conditions for unst grids
    !
    j = len_trim(ugobcfile)
    if (gtype.eq.ungtype.and.ugobcfile(:j).ne.'unset')  &
         call readmshobc(ndsg,ugobcfile,tmpsta,ugobcok)
    if ((gtype.eq.ungtype).and.ugobcauto.and.(.not.ugobcok))  &
         call ug_getopenboundary(tmpsta,zbin,ugobcdepth)
    !
    ! 8.b determine where to get the data
    !
    if (flgnml) then
      ndstr = nml_mask%idf
      idla = nml_mask%idla
      idft = nml_mask%idfm
      rform = trim(nml_mask%format)
      from = trim(nml_mask%from)
      tname = trim(nml_mask%filename)
      if (tname.eq.'unset' .or. tname.eq.'unset') from='part'
    else
      call nextln ( comstr , ndsi , ndse )
      read (ndsi,*,end=2001,err=2002) ndstr, idla, idft, rform,     &
           from, tname
    end if
    !
    ! ... data to be read in parts
    !
    if ( from .eq. 'part' ) then
      !
      ! 8.b update tmpsta with input boundary data (iloop=1)
      !                        and excluded points (iloop=2)
      !
      if ( iclose .eq. iclose_trpl ) then
        write(ndse,*)'program w3grid status map calculation is '//   &
             'not tested for tripole grids for case where user opts '//   &
             'to read data in parts. stopping now (107).'
        call extcde ( 107 )
      end if
      do iloop=1, 2
        !
        i = 1
        if ( iloop .eq. 1 ) then
          write (ndso,979) 'boundary points'
          nstat  = 2
        else
          write (ndso,979) 'excluded points'
          nstat  = -1
        end if
        first  = .true.
        !
        do
          if (flgnml) then
            ! inbound points
            if (iloop.eq.1) then
              if (nml_inbnd_count%n_point.gt.0 .and. i.le.nml_inbnd_count%n_point) then
                ix = nml_inbnd_point(i)%x_index
                iy = nml_inbnd_point(i)%y_index
                connct = nml_inbnd_point(i)%connect
                i=i+1
              else
                exit
              end if
              ! excluded points
            else if (iloop.eq.2) then
              if (nml_excl_count%n_point.gt.0 .and. i.le.nml_excl_count%n_point) then
                ix = nml_excl_point(i)%x_index
                iy = nml_excl_point(i)%y_index
                connct = nml_excl_point(i)%connect
                i=i+1
              else
                exit
              end if
            end if
          else
            call nextln ( comstr , ndsi , ndse )
            read (ndsi,*,end=2001,err=2002) ix, iy, connct
          end if
          !
          ! ... check if last point reached.
          !
          if (ix.eq.0 .and. iy.eq.0) exit
          !
          ! ... check if point in grid.
          !
          if (gtype.eq.ungtype.and.(ugobcauto.or.ugobcok)) cycle
          if (ix.lt.1 .or. ix.gt.nx .or.  iy.lt.1 .or. iy.gt.ny) then
            write (ndso,981)
            write (ndso,*) '       ', ix, iy
            cycle
          end if
          !
          ! ... check if intermediate points are to be added.
          !
          if ( connct .and. .not.first ) then
            idx    = ix - ixo
            idy    = iy - iyo
            if ( idx.eq.0 .or. idy.eq.0 .or.                      &
                 abs(idx).eq.abs(idy) ) then
              nba    = max ( max(abs(idx),abs(idy))-1 , 0 )
              if (idx.ne.0) idx = sign(1,idx)
              if (idy.ne.0) idy = sign(1,idy)
              ix     = ixo
              iy     = iyo
              do iba=1, nba
                ix     = ix + idx
                iy     = iy + idy
                if ( tmpsta(iy,ix).eq.1 .or. j.eq.2 ) then
                  tmpsta(iy,ix) = nstat
                else
                  write(ndso,*) 'warning: point (',ix,',',iy,  &
                       ') cannot be given the status ',nstat
                end if
              end do
              ix     = ix + idx
              iy     = iy + idy
            else
              write (ndso,982)
              write (ndso,*) '       ', ix , iy
              write (ndso,*) '       ', ixo, iyo
            end if
          end if
          !
          ! ... check if point itself is to be added
          !
          if ( tmpsta(iy,ix).eq.1 .or. j.eq.2 ) then
            tmpsta(iy,ix) = nstat
          end if
          !
          ! ... save data of previous point
          !
          ixo    = ix
          iyo    = iy
          first  = .false.
          !
          ! ... branch back to read.
          !
        end do
        !
        ! 8.c final processing excluded points
        !
        if ( iloop .eq. 2 ) then
          !
          i = 1
          do
            if (flgnml) then
              ! excluded bodies
              if (nml_excl_count%n_body.gt.0 .and. i.le.nml_excl_count%n_body) then
                ix = nml_excl_body(i)%x_index
                iy = nml_excl_body(i)%y_index
                i=i+1
              else
                exit
              end if
            else
              call nextln ( comstr , ndsi , ndse )
              read (ndsi,*,end=2001,err=2002) ix, iy
            end if
            !
            ! ... check if last point reached.
            !
            if (ix.eq.0 .and. iy.eq.0) exit
            !
            ! ... check if point in grid.
            !
            if (ix.lt.1 .or. ix.gt.nx .or. iy.lt.1 .or. iy.gt.ny) then
              write (ndso,981)
              write (ndso,*) '       ', ix, iy
              cycle
            end if
            !
            ! ... check if point already excluded
            !
            if ( tmpsta(iy,ix) .eq. nstat ) then
              write (ndso,1981)
              write (ndso,*) '       ', ix, iy
              cycle
            end if
            !
            ! ... search for points to exclude
            !
            tmpmap = tmpsta
            j      = 1
            ix1    = ix
            iy1    = iy
            !
            jj     = tmpsta(iy,ix)
            tmpsta(iy,ix) = nstat
            do
              nbt    = 0
              do ix=max(1,ix1-j), min(ix1+j,nx)
                do iy=max(1,iy1-j), min(iy1+j,ny)
                  if ( tmpsta(iy,ix) .eq. jj ) then
                    if (ix.gt.1) then
                      if (tmpsta(iy  ,ix-1).eq.nstat           &
                           .and. tmpmap(iy  ,ix-1).eq.jj ) then
                        tmpsta(iy,ix) = nstat
                      end if
                    end if
                    if (ix.lt.nx) then
                      if (tmpsta(iy  ,ix+1).eq.nstat           &
                           .and. tmpmap(iy  ,ix+1).eq.jj ) then
                        tmpsta(iy,ix) = nstat
                      end if
                    end if
                    if (iy.lt.ny) then
                      if (tmpsta(iy+1,ix  ).eq.nstat           &
                           .and. tmpmap(iy+1,ix  ).eq.jj ) then
                        tmpsta(iy,ix) = nstat
                      end if
                    end if
                    if (iy.gt.1) then
                      if (tmpsta(iy-1,ix  ).eq.nstat           &
                           .and. tmpmap(iy-1,ix  ).eq.jj ) then
                        tmpsta(iy,ix) = nstat
                      end if
                    end if
                    if (tmpsta(iy,ix).eq.nstat) nbt = nbt + 1
                  end if
                end do
              end do
              !
              if ( nbt .ne. 0 ) then
                j = j + 1
              else
                exit
              end if
            end do
          end do
          !
          ! ... outer boundary excluded points
          !
          if ( gtype.ne.ungtype ) then
            do ix=1, nx
              if ( tmpsta( 1,ix) .eq. 1 ) tmpsta( 1,ix) = nstat
              if ( tmpsta(ny,ix) .eq. 1 ) tmpsta(ny,ix) = nstat
            end do
            !
            if ( iclose.eq.iclose_none ) then
              do iy=2, ny-1
                if ( tmpsta(iy, 1) .eq. 1 ) tmpsta(iy, 1) = nstat
                if ( tmpsta(iy,nx) .eq. 1 ) tmpsta(iy,nx) = nstat
              end do
            end if
          end if ! gtype
          !
        end if ! iloop .eq. 2
        !
        ! ... branch back input / excluded points ( iloop in 8.b )
        !
      end do
      !
    else ! from .eq. part
      !
      ! 8.d read the map from file instead
      !
      nstat  = -1
      if (idla.lt.1 .or. idla.gt.4) idla   = 1
      if (idft.lt.1 .or. idft.gt.3) idft   = 1
      !!li  suspended for smc grid though the file input line in  ww3_grid.inp
      !!li  is kept to divert the program into this block.  jgli15oct2014
      !!li
      if( gtype .ne. smctype ) then
        !!li
        !
        write (ndso,978) ndstr, idla, idft
        if (idft.eq.2) write (ndso,973) rform
        if (from.eq.'name') write (ndso,974) tname
        !
        if ( ndstr .eq. ndsi ) then
          if ( idft .eq. 3 ) then
            write (ndse,1004) ndstr
            call extcde (23)
          else
            call nextln ( comstr , ndsi , ndse )
          end if
        else
          if ( idft .eq. 3 ) then
            if (from.eq.'name') then
              open (ndstr,file=trim(fnmpre)//tname,             &
                   form='unformatted', convert=file_endian,status='old',err=2000, &
                   iostat=ierr)
            else
              open (ndstr,           form='unformatted', convert=file_endian,      &
                   status='old',err=2000,iostat=ierr)
            end if
          else
            if (from.eq.'name') then
              open (ndstr,file=trim(fnmpre)//tname,             &
                   status='old',err=2000,iostat=ierr)
            else
              open (ndstr,                                    &
                   status='old',err=2000,iostat=ierr)
            end if
          end if
        end if
        !
        allocate ( readmp(nx,ny) )
        call ina2i ( readmp, nx, ny, 1, nx, 1, ny, ndstr, ndst,    &
             ndse, idft, rform, idla, 1, 0 )
        !
        if ( iclose.eq.iclose_none ) then
          do iy=2, ny-1
            if ( readmp( 1,iy) .eq. 1 ) readmp( 1,iy) = 3
            if ( readmp(nx,iy) .eq. 1 ) readmp(nx,iy) = 3
          end do
        end if
        !
        do ix=1, nx
          if ( readmp(ix, 1) .eq. 1 ) readmp(ix, 1) = 3
          if ( readmp(ix,ny) .eq. 1 .and. iclose .ne. iclose_trpl)   &
               readmp(ix,ny) = 3
        end do
        !
        do iy=1, ny
          do ix=1, nx
            if ( readmp(ix,iy) .eq. 3 ) then
              tmpsta(iy,ix) = nstat
            else
              tmpsta(iy,ix) = readmp(ix,iy)
              ! force to dry the sea points over zlim
              if ( zbin(ix,iy) .gt. zlim ) tmpsta(iy,ix) = 0
            end if
          end do
        end do
        deallocate ( readmp )
        !!li
      endif   !! gtype .ne. smctype
      !
    end if !from .ne. 'part'
    !
    ! 8.e get nsea and other counters
    !
    nsea   = 0
    nland  = 0
    nbi    = 0
    nbt    = 0
    !
    do ix=1, nx
      do iy=1, ny
        if ( tmpsta(iy,ix) .gt. 0 ) nsea   = nsea + 1
        if ( tmpsta(iy,ix) .eq. 0 ) nland  = nland + 1
        if ( tmpsta(iy,ix) .lt. 0 ) nbt    = nbt + 1
        if ( tmpsta(iy,ix) .eq. 2 ) nbi    = nbi + 1
      end do
    end do
    !
    !
    write (ndso,980)
    flbpi  = nbi .gt. 0
    if ( .not. flbpi ) then
      write (ndso,985)
    else
      write (ndso,986) nbi
    end if
    !
    write (ndso,1980)
    if ( nbt .eq. 0 ) then
      write (ndso,1985)
    else
      write (ndso,1986) nbt
    end if
    !
    ! 8.f set up all maps
    !
    call w3dimx ( 1, nx, ny, nsea, ndse, ndst  &
         )
    !
    ! 8.g activation of reflections and scattering
    ffacberg=facberg
    if (gtype.ne.ungtype) then
      do iy=1, ny
        do ix=1, nx
          xgrd(iy,ix) = xgrdin(ix,iy)
          ygrd(iy,ix) = ygrdin(ix,iy)
        end do
      end do
      deallocate ( xgrdin, ygrdin )
      call w3gntx ( 1, 6, 6 )
    else
    end if   ! gtype
    !
      !
      mapsta = tmpsta
      mapfs  = 0
      !
      !
      trnx   = 0.
      trny   = 0.
      !
      isea   = 0
      do iy=1, ny
        do ix=1, nx
          if ( tmpsta(iy,ix) .eq. nstat ) then
            mapsta(iy,ix) = 0
            mapst2(iy,ix) = 1
            tmpsta(iy,ix) = 3
          else
            mapsta(iy,ix) = tmpsta(iy,ix)
            mapst2(iy,ix) = 0
          end if
          if ( mapsta(iy,ix) .ne. 0 ) then
            isea           = isea + 1
            mapfs (iy,ix)  = isea
            zb(isea)       = zbin(ix,iy)
            mapsf(isea,1)  = ix
            mapsf(isea,2)  = iy
            if ( flagll ) then
              y              = ygrd(iy,ix)
              clats(isea)    = cos(y*dera)
              clatis(isea)   = 1. / clats(isea)
              cthg0s(isea)   = - tan(dera*y) / radius
            else
              clats(isea)    = 1.
              clatis(isea)   = 1.
              cthg0s(isea)   = 0.
            end if
          end if
          !/ ------------------------------------------------------------------- /
          ! notes: oct 22 2012: i moved the following "if-then" statement from
          ! inside the  "if ( mapsta(iy,ix) .ne. 0 )" statement to outside that
          ! statement. this is needed since later on, atrnx is computed from
          ! trnx(ix-1) , trnx(ix) etc. which causes boundary effects if the
          ! mapsta=0 values are set to trnx=0
          if ( trflag .ne. 0 ) then
            trnx(iy,ix) = 1. - obsx(ix,iy)
            trny(iy,ix) = 1. - obsy(ix,iy)
          end if
        end do
      end do
      !
    !
    !
    !
    do isp=1, nspec+nth
      mapwn(isp) = 1 + (isp-1)/nth
      mapth(isp) = 1 + mod(isp-1,nth)
    end do
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    ! 9.d estimates shoreline direction for reflection
    !     and shoreline treatment in general for unst grids.
    ! nb: this is updated with moving water levels in w3ulev
    ! ar: this is not anymore needed and will be deleted ...
    !
    if (gtype.eq.ungtype) then
      call set_ug_iobp
    end if
      !
    !
    deallocate ( zbin, tmpsta, tmpmap )
    !
    ! 9.e reads bottom information from file
    !
    !
    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! 10.  prepare output boundary points.
    !     iloop = 1 to count nfbpo and nbo
    !     iloop = 2 to fill data arrays
    !
    write (ndso,990)
    if ( .not. flgnml ) &
         open (ndss,file=trim(fnmpre)//'ww3_grid.scratch',form='formatted')
    !
    do iloop = 1, 2
      !
      if ( iloop.eq.2 ) call w3dmo5 ( 1, ndst, ndse, 2 )
      !
      i = 1
      nbotot = 0
      nfbpo  = 0
      nbo(0) = 0
      nbo2(0)= 0
      first  = .true.
      if ( .not. flgnml ) then
        rewind (ndss)
        if ( iloop .eq. 1 ) then
          ndsi2 = ndsi
        else
          ndsi2 = ndss
        end if
      end if
      !
      do
        if (flgnml) then
          ! outbound lines
          if (nml_outbnd_count%n_line.gt.0 .and. i.le.nml_outbnd_count%n_line) then
            xo0 = nml_outbnd_line(i)%x0
            yo0 = nml_outbnd_line(i)%y0
            dxo = nml_outbnd_line(i)%dx
            dyo = nml_outbnd_line(i)%dy
            npo = nml_outbnd_line(i)%np
            i=i+1
          else
            npo=0
          end if
        else
          call nextln ( comstr , ndsi2 , ndse )
          read (ndsi2,*,end=2001,err=2002) xo0, yo0, dxo, dyo, npo
        end if
        !
        if ( .not. flgnml .and. iloop .eq. 1 ) then
          backspace (ndsi)
          read (ndsi,'(a)') line
          write (ndss,'(a)') line
        end if
        !
        ! ... check if new file to be used
        !
        first  = first .or. npo.le.0
        npo    = abs(npo)
        !
        ! ... preparations for new output file including end check
        !     and output for last output file
        !
        if ( first ) then
          !
          first  = .false.
          !
          if ( nfbpo.ge.1 .and. iloop.eq.2 ) then
            write (ndso,991)  nfbpo, nbo(nfbpo) - nbo(nfbpo-1), &
                 nbo2(nfbpo) - nbo2(nfbpo-1)
          end if
          !
          if ( npo .eq. 0 ) exit
          !
          nfbpo  = nfbpo + 1
          if ( nfbpo .gt. 9 ) then
            write (ndse,1006)
            call extcde ( 50 )
          end if
          nbo2(nfbpo) = nbo2(nfbpo-1)
          nbo(nfbpo) = nbotot
          !
        end if
        !
        ! ... loop over line segment - - - - - - - - - - - - - - - - - - - - -
        !
        !
        do ip=1, npo
          !
          xo     = xo0 + real(ip-1)*dxo
          yo     = yo0 + real(ip-1)*dyo
          !
          ! ... compute bilinear remapping weights
          !
          ingrid = w3grmp( gsu, xo, yo, ixr, iyr, rd )
          !
          !           change cell-corners from counter-clockwise to column-major order
          ix     = ixr(3);  iy     = iyr(3);  x     = rd(3);
          ixr(3) = ixr(4);  iyr(3) = iyr(4);  rd(3) = rd(4);
          ixr(4) = ix    ;  iyr(4) = iy    ;  rd(4) = x    ;
          !
          !
          ! ... check if point in grid
          !
          if ( ingrid ) then
            !
            ! ... check if point not on land
            !
            if ( ( mapsta(iyr(1),ixr(1)).gt.0 .and.                 &
                 rd(1).gt.0.05 ) .or.          &
                 ( mapsta(iyr(2),ixr(2)).gt.0 .and.                 &
                 rd(2).gt.0.05 ) .or.          &
                 ( mapsta(iyr(3),ixr(3)).gt.0 .and.                 &
                 rd(3).gt.0.05 ) .or.          &
                 ( mapsta(iyr(4),ixr(4)).gt.0 .and.                 &
                 rd(4).gt.0.05 ) ) then
              !
              ! ... check storage and store coordinates
              !
              nbotot = nbotot + 1
              if ( iloop .eq. 1 ) cycle
              !
              xbpo(nbotot) = xo
              ybpo(nbotot) = yo
              !
              ! ... interpolation factors
              !
              rdtot = 0.
              do j=1, 4
                if ( mapsta(iyr(j),ixr(j)).gt.0 .and.               &
                     rd(j).gt.0.05 ) then
                  rdbpo(nbotot,j) = rd(j)
                else
                  rdbpo(nbotot,j) = 0.
                end if
                rdtot = rdtot + rdbpo(nbotot,j)
              end do
              !
              do j=1, 4
                rdbpo(nbotot,j) = rdbpo(nbotot,j) / rdtot
              end do
              !
              !
              ! ... determine sea and interpolation point counters
              !
              do j=1, 4
                iseai(j) = mapfs(iyr(j),ixr(j))
              end do
              !
              do j=1, 4
                if ( iseai(j).eq.0 .or. rdbpo(nbotot,j).eq. 0. ) then
                  ipbpo(nbotot,j) = 0
                else
                  flnew   = .true.
                  do ist=nbo2(nfbpo-1)+1, nbo2(nfbpo)
                    if ( iseai(j) .eq. isbpo(ist) ) then
                      flnew  = .false.
                      ipbpo(nbotot,j) = ist - nbo2(nfbpo-1)
                    end if
                  end do
                  if ( flnew ) then
                    nbo2(nfbpo)        = nbo2(nfbpo) + 1
                    ipbpo(nbotot,j)    = nbo2(nfbpo) - nbo2(nfbpo-1)
                    isbpo(nbo2(nfbpo)) = iseai(j)
                  end if
                end if
              end do
              !
              !
              ! ... error output
              !
            else
              if ( flagll ) then
                write (ndse,2995) factor*xo, factor*yo
              else
                write (ndse,995) factor*xo, factor*yo
              end if
            end if
          else
            if ( flagll ) then
              write (ndse,2994) factor*xo, factor*yo
            else
              write (ndse,994) factor*xo, factor*yo
            end if
          end if
          !
        end do
        !
        nbo(nfbpo) = nbotot
        !
        ! ... branch back to read.
        !
      end do
      !
      ! ... end of iloop loop
      !
    end do
    !
    if ( .not. flgnml ) close ( ndss, status='delete' )
    !
    flbpo  = nbotot .gt. 0
    if ( .not. flbpo ) then
      write (ndso,996)
    else
      write (ndso,997) nbotot, nbo2(nfbpo)
    end if
    !
    !
    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !10.  write model definition file.
    !
    write (ndso,999)
    call w3iogr ( 'write', ndsm &
            )
    !
    close (ndsm)
    !
    goto 2222
    !
    ! escape locations read errors :
    !
2000 continue
    write (ndse,1000) ierr
    call extcde ( 60 )
    !
2001 continue
    write (ndse,1001)
    call extcde ( 61 )
    !
2002 continue
    write (ndse,1002) ierr
    call extcde ( 62 )
    !
2003 continue
    write (ndse,1003)
    call extcde ( 64 )
    !
2222 continue
    if ( gtype .ne. ungtype) then
      if ( nx*ny .ne. nsea ) then
        write (ndso,9997) nx, ny, nx*ny, nsea,                       &
             100.*real(nsea)/real(nx*ny), nbi, nland, nbt
      else
        write (ndso,9998) nx, ny, nx*ny, nsea, nbi, nland, nbt
      end if
    else if ( gtype .eq. ungtype ) then
      if ( nx*ny .ne. nsea ) then
        write (ndso,9997)  0,  0, nx*ny, nsea,                       &
             100.*real(nsea)/real(nx*ny), nbi, nland, nbt
      else
        write (ndso,9998)  0,  0, nx*ny, nsea, nbi, nland, nbt
      end if
    endif ! gtype .eq. ungtype
    write (ndso,9999)
    !
    ! formats
    !
900 format (/15x,'    *** wavewatch iii grid preprocessor ***    '/ &
         15x,'==============================================='/)
901 format ( '  comment character is ''',a,''''/)
902 format ( '  grid name : ',a/)
903 format (/'  spectral discretization : '/                        &
         ' --------------------------------------------------'/ &
         '       number of directions        :',i4/             &
         '       directional increment (deg.):',f6.1)
904 format ( '       first direction       (deg.):',f6.1)
905 format ( '       number of frequencies       :',i4/             &
         '       frequency range        (hz) :',f9.4,'-',f6.4/  &
         '       increment factor            :',f8.3/)
    !
910 format (/'  model definition :'/                                &
         ' --------------------------------------------------')
911 format ( '       dry run (no calculations)   :  ',a/            &
         '       propagation in x-direction  :  ',a/            &
         '       propagation in y-direction  :  ',a/            &
         '       refraction                  :  ',a/            &
         '       current-induced k-shift     :  ',a/            &
         '       source term calc. and int.  :  ',a/)
912 format (/'  time steps : '/                                     &
         ' --------------------------------------------------'/ &
         '       maximum global time step      (s) :',f8.2/     &
         '       maximum cfl time step x-y     (s) :',f8.2/     &
         '       maximum cfl time step k-theta (s) :',f8.2/     &
         '       minimum source term time step (s) :',f8.2/)
913 format (/ '  warning, time step less than 1 s, niter:',i8 /)
915 format ( '  preprocessing namelists ...')
916 format ( '  preprocessing namelists finished.'/)
917 format (/'  equivalent namelists ...'/)
918 format (/'  equivalent namelists finished.'/)
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
950 format (/'  propagation scheme : '/                             &
         ' --------------------------------------------------')
951 format ( '       type of scheme (structured) :',1x,a)
2951 format ( '       type of scheme(unstructured):',1x,a)
2952 format ( '             wave setup computation:',1x,a)
952 format ( '                                    ',1x,a)
    !
    !
    !
    !
2956 format ( '  &unst ugbccfl =',l3,', ugobcauto =',l3,             &
         ', ugobcdepth =', f8.3,', ugobcfile=',a,','/           &
         ',  expfsn =',l3,',expfspsi =',l3,                     &
         ',  expfsfct =', l3,',impfsn =',l3,',exptotal=',l3,    &
         ',  imptotal=',l3,',imprefraction=', l3,               &
         ',  impfreqshift=', l3,', impsource=', l3,             &
         ',  setup_apply_wlv=', l3,                             &
         ',  jgs_terminate_maxiter=', l3,                       &
         ',  jgs_terminate_difference=', l3,                    &
         ',  jgs_terminate_norm=', l3,                          &
         ',  jgs_limiter=', l3,                                 &
         ',  jgs_limiter_func=', i3,                            &
         ',  jgs_use_jacobi=', l3,                              &
         ',  jgs_block_gauss_seidel=', l3,                      &
         ',  jgs_maxiter=', i5,                                 &
         ',  jgs_pmin=', f8.3,                                  &
         ',  jgs_diff_thr=', f8.3,                              &
         ',  jgs_norm_thr=', f8.3,                              &
         ',  jgs_nlevel=', i3,                                  &
         ',  jgs_source_nonlinear=', l3 / )
    !
960 format (/'  miscellaneous ',a/                                   &
         ' --------------------------------------------------')
2961 format ( ' *** wavewatch-iii warning in w3grid :'/               &
         '     cice0.ne.cicen requires flagtr>2'/                &
         '     parameters corrected: cice0 = cicen'/)
2962 format (/' *** wavewatch-iii warning in w3grid : user requests', &
         'cice0=cicen corresponding to discontinuous treatment of ',   &
         'ice, so we will change flagtr')
2963 format (/' *** wavewatch-iii warning in w3grid :'/               &
         '     ice physics used, so we will change flagtr.')
961 format ( '       ice concentration cut-offs  :',f8.2,f6.2)
1972 format ( '       compression of track output  : ',l3)
965 format (/'    dynamic source term integration scheme :'/        &
         '       xp                      (-) :',f9.3/           &
         '       xr                      (-) :',f9.3/           &
         '       xfilt                   (-) :',f9.3)
966 format (/'    wave field partitioning :'/                       &
         '       levels                  (-) :',i5/             &
         '       minimum wave height     (m) :',f9.3/           &
         '       wind area multiplier    (-) :',f9.3/           &
         '       cut-off wind sea fract. (-) :',f9.3/           &
         '       combine wind seas           :  ',a/            &
         '       number of swells in fld out :',i5)
967 format (/'    miche-style limiting wave height :'/              &
         '       hs,max/d factor         (-) :',f9.3/           &
         '       hrms,max/d factor       (-) :',f9.3/           &
         '       limiter activated           :  ',a)
968 format ( '          *** factor dangerously low ***')
1973 format (/'    calendar type                  :  ',a)
    !
    !
    !
5971 format ('       partitioning method         :  ',a)
5972 format ('       namelist options overridden :  ',a)
    !
    !
8972 format ( '       wind input reduction factor in presence of ', &
         /'         ice :',f6.2, &
         /'         (0.0==> no reduction and 1.0==> no wind', &
         /'         input with 100% ice cover)')
    !
    !
4970 format (/'  spectral output on full grid ',a/                   &
         ' --------------------------------------------------')
4971 format ( '       second order pressure at k=0:',3i4)
4972 format ( '       spectrum of uss             :',3i4)
4973 format ( '       frequency spectrum          :',3i4)
4974 format ( '       partions of uss             :',2i4)
4975 format ( '       partition wavenumber #',i2,'   : ',1f6.3)
    !
4980 format (/'  coastal / iceberg reflection  ',a/                   &
         ' --------------------------------------------------')
4981 format ( '       coefficient for shorelines  :',f6.4)
4989 format ( '          *** curvlinear grid: reflection not implemented yet ***')
2977 format ( '  &sig1  igmethod =',i2,', igaddoutp =',i2,', igsource =',i2, &
         ', igsterms = ',i2,', igbcoverwrite =', l3,','/        &
         '        igswellmax =', l3,', igmaxfreq =',f6.4,       &
         ', igsourceatbp = ',i2,', igkdmin = ',f6.4,','/        &
         '        igfixeddepth = ',f6.2,', igempirical = ',f8.6,' /')
    !
2978 format ( '  &sic2  ic2disper =',l3,', ic2turb =',f6.2,          &
         ', ic2rough  =',f10.6,','/                             &
         '        ic2reynolds = ',f10.1,', ic2smooth = ',f10.1, &
         ', ic2visc =',f6.3,','/                                &
         ',       ic2turbs =',f8.2,', ic2dmax =',f5.3,' /')
    !
2979 format ( '  &sic3 ic3maxthk =',f6.2, ', ic3maxcnc =',f6.2,','/  &
         '        ic2turb =',f8.2,                              &
         ', ic2rough  =',f7.3,','/                              &
         '        ic2reynolds = ',f10.1,', ic2smooth = ',f10.1, &
         ', ic2visc =',f10.3,','/                               &
         '        ic2turbs =',f8.2,', ic3cheng =',l3,           &
         ', usecgice =',l3,', ic3hilim = ',f6.2,','/            &
         '        ic3kilim = ',e9.2,', ic3hice = ',e9.2,        &
         ', ic3visc = ',e9.2,','/                               &
         '        ic3dens = ',e9.2,', ic3elas = ',e9.2,' /')
    !
2981 format ( '  &sic5 ic5minig = ', e9.2, ', ic5minwt = ', f5.2,    &
         ', ic5maxkratio = ', e9.2, ','/                        &
         '        ic5maxki = ', e9.2, ', ic5minhw = ', f4.0,    &
         ', ic5maxiter = ', f4.0, ','/                          &
         '        ic5rkick = ', f2.0, ', ic5kfilter = ', f7.4,  &
         ', ic5vemod   = ', f4.0, ' /')
    !
2966 format ( '  &misc cice0 =',f6.3,', cicen =',f6.3,               &
         ', lice = ',f8.1,', pmove =',f6.3,','/           &
         '        xseed =',f6.3,', flagtr = ', i1,              &
         ', xp =',f6.3,', xr =',f6.3,', xfilt =', f6.3 /  &
         '        ihm =',i5,', hspm =',f6.3,', wsm =',f6.3,     &
         ', wsc =',f6.3,', flc = ',a/                     &
         '        nosw =',i3,', fmiche =',f6.3,', rwndc =' ,    &
         f6.3,', wcor1 =',f6.2,', wcor2 =',f6.2,','/   &
         '        facberg =',f4.1,', gshift = ',e11.3,          &
         ', stdx = ' ,f7.2,', stdy =',f7.2,','/           &
         '        stdt =', f8.2,                                &
         ', icehmin =',f5.2,', icehfac =',f5.2,','/       &
         '        icehinit =',f5.2,', icedisp =',l3,            &
         ', icehdisp =',f5.2,','/                         &
         '        icesln = ',f6.2,', icewind = ',f6.2,          &
         ', icesnl = ',f6.2,', icesds = ',f5.2,','/       &
         '        iceddisp = ',f5.2,', icefdisp = ',f5.2,       &
         ', caltype = ',a8,' , trckcmpr = ', l3,','/      &
         '        btbet  = ', f6.2, ' /')
    !
2976 format ( '  &outs p2sf  =',i2,', i1p2sf =',i2,', i2p2sf =',i3,','/&
         '        us3d  =',i2,', i1us3d =',i3,', i2us3d =',i3,','/&
         '        ussp  =',i2,', iussp  =',i3,','/&
         '        e3d   =',i2,', i1e3d  =',i3,', i2e3d  =',i3,','/&
         '        th1mf =',i2,', i1th1m =',i3,', i2th1m =',i3,','/&
         '        sth1mf=',i2,', i1sth1m=',i3,', i2sth1m=',i3,','/&
         '        th2mf =',i2,', i1th2m =',i3,', i2th2m =',i3,','/&
         '        sth2mf=',i2,', i1sth2m=',i3,', i2sth2m=',i3,' /')
    !
2986 format ( '  &ref1 refcoast =',f5.2,', reffreq =',f5.2,', refslope =',f5.3, &
         ', refmap =',f4.1, ', refmapd =',f4.1, ', refsubgrid =',f5.2,','/ &
         '        refrmax=',f5.2,', reffreqpow =',f5.2,                    &
         ', reficeberg =',f5.2,', refcosp_straight =',f4.1,' /')
    !
2987 format ( '  &fld tail_id =',i1,' tail_lev =',f5.4,' tailt1 =',f5.3,&
         ' tailt2 =',f5.3,' /')
3000 format (/'  the spatial grid: '/                                &
         ' --------------------------------------------------'/ &
         /'       grid type                   : ',a)
3001 format ( '       coordinate system           : ',a)
3002 format ( '       index closure type          : ',a)
3003 format ( '       dimensions                  : ',i6,i8)
3004 format (/'       increments           (deg.) :',2f10.4/         &
         '       longitude range      (deg.) :',2f10.4/         &
         '       latitude range       (deg.) :',2f10.4)
3005 format ( '       increments             (km) :',2f8.2/          &
         '       x range                (km) :',2f8.2/          &
         '       y range                (km) :',2f8.2)
3006 format (/'       x-coordinate unit           :',i6/             &
         '       scale factor                :',f10.4/           &
         '       add offset                  :',e12.4/          &
         '       layout indicator            :',i6/             &
         '       format indicator            :',i6)
3007 format (/'       y-coordinate unit           :',i6/             &
         '       scale factor                :',f10.4/           &
         '       add offset                  :',e12.4/          &
         '       layout indicator            :',i6/             &
         '       format indicator            :',i6)
3008 format ( '       format                      : ',a)
3009 format ( '       file name                   : ',a)
972 format (/'       bottom level unit           :',i6/             &
         '       limiting depth          (m) :',f8.2/           &
         '       minimum depth           (m) :',f8.2/           &
         '       scale factor                :',f8.2/           &
         '       layout indicator            :',i6/             &
         '       format indicator            :',i6)
973 format ( '       format                      : ',a)
974 format ( '       file name                   : ',a)
976 format (/'       sub-grid information        : ',a)
977 format ( '       obstructions unit           :',i6/             &
         '       scale factor                :',f10.4/          &
         '       layout indicator            :',i6/             &
         '       format indicator            :',i6)
978 format (/'       mask information            : from file.'/     &
         '       mask unit                   :',i6/             &
         '       layout indicator            :',i6/             &
         '       format indicator            :',i6)
1977 format ( '       shoreline slope             :',i6/             &
         '       scale factor                :',f10.4/          &
         '       layout indicator            :',i6/             &
         '       format indicator            :',i6)
1978 format ( '       grain sizes                 :',i6/             &
         '       scale factor                :',f10.4/          &
         '       layout indicator            :',i6/             &
         '       format indicator            :',i6)
    !
979 format ( '  processing ',a)
980 format (/'  input boundary points : '/                          &
         ' --------------------------------------------------')
1980 format (/'  excluded points : '/                                &
         ' --------------------------------------------------')
981 format ( '   *** point outside grid (skipped), ix, iy =')
1981 format ( '   *** point already excluded (skipped), ix, iy =')
982 format ( '   *** cannot connect points, ix, iy =')
985 format ( '       no boundary points.'/)
986 format ( '       number of boundary points   :',i6/)
1985 format ( '       no excluded points.'/)
1986 format ( '       number of excluded points   :',i6/)
987 format ( '         nr.|   ix  |   iy  |  long.  |   lat.  '/        &
         '       -----|-------|-------|---------|---------')
1987 format ( '         nr.|   ix  |   iy  |     x     |     y     '/    &
         '       -----|-------|-------|-----------|-----------')
988 format ( '       ',i4,2(' |',i6),2(' |',f8.2))
1988 format ( '       ',i4,2(' |',i6),2(' |',f8.1,'e3'))
989 format ( ' ')
    !
990 format (/'  output boundary points : '/                         &
         ' --------------------------------------------------')
991 format ( '       file nest',i1,'.ww3  number of points  :',i6/  &
         '                       number of spectra :',i6)
1991 format ( '                       dest. grid polat:',f6.2,', polon:',f8.2)
992 format (/'         nr.|  long.  |   lat.  '/               &
         '       -----|---------|---------')
1992 format (/'         nr.|  long.  |   lat.  ',               &
         '         nr.|  long.  |   lat.  '/               &
         '       -----|---------|---------',               &
         '       -----|---------|---------')
993 format ( '       ',i4,2(' |',f8.2))
1993 format ( '       ',i4,2(' |',f8.2),                        &
         '        ',i4,2(' |',f8.2))
994 format ( '   *** point outside grid (skipped) : x,y =',2f10.5)
995 format ( '   *** point on land      (skipped) : x,y =',2f10.5)
2992 format (/'         nr.|     x     |     y     '/           &
         '       -----|-----------|-----------')
3992 format (/'         nr.|     x     |     y     ',           &
         '       nr.|     x     |     y     '/           &
         '       -----|-----------|-----------',           &
         '     -----|-----------|-----------')
2993 format ( '       ',i4,2(' |',f8.1,'e3'))
3993 format ( '       ',i4,2(' |',f8.1,'e3'),                   &
         '      ',i4,2(' |',f8.1,'e3'))
2994 format ( '   *** point outside grid (skipped) : x,y =',2(f8.1,'e3'))
2995 format ( '   *** point on land      (skipped) : x,y =',2(f8.1,'e3'))
996 format ( '       no boundary points.'/)
997 format ( '       number of boundary points   :',i6/             &
         '       number of spectra           :',i6/)
    !
    !
999 format (/'  writing model definition file ...'/)
    !
1000 format (/' *** wavewatch iii error in w3grid : '/               &
         '     error in opening input file'/                    &
         '     iostat =',i5/)
    !
1001 format (/' *** wavewatch iii error in w3grid : '/               &
         '     premature end of input file'/)
    !
1002 format (/' *** wavewatch iii error in w3grid : '/               &
         '     error in reading from input file'/               &
         '     iostat =',i5/)
    !
1003 format (/' *** wavewatch iii error in w3grid : '/               &
         '     invalid calendar type: select one of:',          &
         '     standard, 360_day, or 365_day '/)
    !
1004 format (/' *** wavewatch iii error in w3grid : '/               &
         '     cannot read unformatted (idfm = 3) from unit',   &
         i4,' (ww3_grid.inp)'/)
    !
1005 format (/' *** wavewatch iii error in w3grid : '/               &
         '     bottom and obstruction data from same file '/    &
         '     but with incompatible formats (',i1,',',i1,')'/)
    !
1006 format (/' *** wavewatch iii error in w3grid :'/                &
         '     too many nesting output files '/)
    !
1007 format (/' *** wavewatch-iii error in w3grid :'/                &
         '     illegal grid type:',a4)
    !
1008 format (/' *** wavewatch-iii error in w3grid :'/                &
         '     a cartesian with closure is not allowed')
    !
1009 format (/' *** wavewatch-iii error in w3grid :'/                &
         '     a rectilinear tripole grid is not allowed')
    !
1010 format (/' *** wavewatch-iii error in w3grid :'//               &
         '     no propagation + no source terms = no wave model'// &
         '     ( use dry run flag to temporarily switch off ',  &
         'calculations )'/)
    !
1011 format (/' *** wavewatch-iii warning in w3grid :'/              &
         '     left-handed grid -- possible cause is wrong '/   &
         '     idla:',i4,' . this may produce errors '/         &
         '     (comment this extcde at your own risk).')
    !
1012 format (/' *** wavewatch-iii error in w3grid :'/                &
         '     illegal grid closure type:',a4)
    !
1013 format (/' *** wavewatch-iii warning in w3grid :'/              &
         '     the global (logical) input flag is deprecated'/  &
         '     and replaced with a string indicating the type'/ &
         '     of grid index closure (none, smpl or trpl).'/    &
         ' *** please update your grid input file accordingly ***'/)
    !
    !
1020 format (/' *** wavewatch-iii error in w3grid :'/                &
         '     source terms requested but not selected'/)
1021 format (/' *** wavewatch iii warning in w3grid :'/              &
         '     source terms selected but not requested'/)
1022 format (/' *** wavewatch iii error in w3grid :'/                &
         '     illegal number of !/lnn or seed switches :',i3)
1023 format (/' *** wavewatch iii error in w3grid :'/                &
         '     illegal number of !/stn switches :',i3)
1024 format (/' *** wavewatch iii error in w3grid :'/                &
         '     illegal number of !/nln switches :',i3)
1025 format (/' *** wavewatch iii error in w3grid :'/                &
         '     illegal number of !/btn switches :',i3)
1026 format (/' *** wavewatch iii error in w3grid :'/                &
         '     illegal number of !/dbn switches :',i3)
1027 format (/' *** wavewatch iii error in w3grid :'/                &
         '     illegal number of !/trn switches :',i3)
1028 format (/' *** wavewatch iii error in w3grid :'/                &
         '     illegal number of !/bsn switches :',i3)
    !
1030 format (/' *** wavewatch iii error in w3grid :'/                &
         '     propagation requested but no scheme selected '/)
1031 format (/' *** wavewatch iii warning in w3grid :'/              &
         '     no propagation requested but scheme selected '/)
1032 format (/' *** wavewatch iii error in w3grid :'/                &
         '     no propagation scheme selected ( use !/pr0 ) '/)
1033 format (/' *** wavewatch iii error in w3grid :'/                &
         '     multiple propagation schemes selected :',i3/     &
         '     check !/prn switches'/)
1034 format (/' *** wavewatch iii error in w3grid :'/                &
         '     illegal number of !/icn switches :',i3)
1035 format (/' *** wavewatch iii warning in w3grid :'/              &
         '     only first propagation scheme will be used: ')
1036 format (/' *** wavewatch iii error in w3grid :'/                &
         '     illegal number of !/isn switches :',i3)
    !
1040 format ( '       space-time extremes dx      :',f10.2)
1041 format ( '       space-time extremes dx      :',f10.2)
1042 format ( '       space-time extremes dx-y set to default 1000 m')
1043 format ( '       space-time extremes dt      :',f8.2)
1044 format ( '       space-time extremes dt set to default 1200 s')
    !
1100 format (/'  status map, printed in',i6,' part(s) '/             &
         ' -----------------------------------'/)
1101 format (2x,180i2)
1102 format ( '  legend : '/                                         &
         ' -----------------------------'/                      &
         '    0 : land point            '/                      &
         '    1 : sea point             '/                      &
         '    2 : active boundary point '/                      &
         '    3 : excluded point        '/)
1103 format (/'  obstruction map ',a1,', printed in',i6,' part(s) '/ &
         ' ---------------------------------------------'/)
1104 format ( '  legend : '/                                         &
         ' --------------------------------'/                   &
         '    fraction of obstruction * 10 '/)
1105 format (/'  shoreline slope, printed in',i6,' part(s) '/ &
         ' ---------------------------------------------'/)
1106 format ( '  legend : '/                                         &
         ' --------------------------------'/                   &
         '   slope * 100'/)
1150 format (/'  reading unstructured grid definition files ...'/)
    !
9997 format (/'  summary grid statistics : '/                        &
         ' --------------------------------------------------'/ &
         '       number of longitudes      :',i10/              &
         '       number of latitudes       :',i10/              &
         '       number of grid points     :',i10/              &
         '       number of sea points      :',i10,' (',f4.1,'%)'/&
         '       number of input b. points :',i10/              &
         '       number of land points     :',i10/              &
         '       number of excluded points :',i10/)
9998 format (/'  summary grid statistics : '/                        &
         ' --------------------------------------------------'/ &
         '       number of longitudes      :',i10/              &
         '       number of latitudes       :',i10/              &
         '       number of grid points     :',i10/              &
         '       number of sea points      :',i10,' (100%)'/    &
         '       number of input b. points :',i10/              &
         '       number of land points     :',i10/              &
         '       number of excluded points :',i10/)
9999 format (/'  end of program '/                                   &
         ' ========================================'/           &
         '         wavewatch iii grid preprocessor '/)
    !
    !
  end subroutine w3grid
  !/
  !/ internal function readnl ------------------------------------------ /
  !/
  !/ ------------------------------------------------------------------- /
  subroutine readnl ( nds, name, status )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         01-jun-2013 |
    !/                  +-----------------------------------+
    !/
    !  1. purpose :
    !
    !     read namelist info from file if namelist is found in file.
    !
    !  2. method :
    !
    !     look for namelist with name name in unit nds and read if found.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       nds     int.   i   data set number used for search.
    !       name    c*4    i   name of namelist.
    !       status  c*20   o   status at end of routine,
    !                            '(default values)  ' if no namelist found.
    !                            '(user def. values)' if namelist read.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      extcde    subr. w3servmd abort program as graceful as possible.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !     program in which it is contained.
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !  9. switches :
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: nds
    character, intent(in)   :: name*4
    character, intent(out)  :: status*20
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ierr, i, j
    character               :: line*80
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    rewind (nds)
    status  = '(default values) :  '
    !
    do
      read (nds,'(a)',end=800,err=800,iostat=ierr) line
      do i=1, 70
        if ( line(i:i) .ne. ' ' ) then
          if ( line(i:i) .eq. '&' ) then
            if ( line(i+1:i+4) .eq. name ) then
              backspace (nds)
              select case(name)
              case('unst')
                read (nds,nml=unst,end=801,err=802,iostat=j)
              case('outs')
                read (nds,nml=outs,end=801,err=802,iostat=j)
              case('misc')
                read (nds,nml=misc,end=801,err=802,iostat=j)
              case default
                goto 803
              end select
              status  = '(user def. values) :'
              return
            end if
          else
            exit
          end if
        endif
      end do
    end do
    !
800 continue
    return
    !
801 continue
    write (ndse,1001) name
    call extcde(1)
    return
    !
802 continue
    write (ndse,1002) name, j
    call extcde(2)
    return
    !
803 continue
    write (ndse,1003) name
    call extcde(3)
    return
    !
    !
    !
    ! formats
    !
1001 format (/' *** wavewatch iii error in readnl : '/          &
         '     premature end of file in reading ',a/)
1002 format (/' *** wavewatch iii error in readnl : '/          &
         '     error in reading ',a,'  iostat =',i8/)
1003 format (/' *** wavewatch iii error in readnl : '/          &
         '     namelist name ',a,' not recognized'/)
    !/
    !/ end of readnl ----------------------------------------------------- /
    !/
  end subroutine readnl
  !/
  !/ end of w3grid ----------------------------------------------------- /
  !/
end module w3gridmd
