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
module w3odatmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         22-mar-2021 |
  !/                  +-----------------------------------+
  !/
  !/    13-dec-2004 : origination.                        ( version 3.06 )
  !/    20-jul-2005 : adding output fields.               ( version 3.07 )
  !/    29-sep-2005 : second storage for input bound. sp. ( version 3.08 )
  !/                  add filed for the dump of data.
  !/    26-jun-2006 : add output type 6, wave field sep.  ( version 3.09 )
  !/                  wiring of code only.
  !/    27-jun-2006 : adding file name preamble.          ( version 3.09 )
  !/    24-jul-2006 : adding unified point output storage.( version 3.10 )
  !/    25-jul-2006 : originating grid id for points.     ( version 3.10 )
  !/    04-oct-2006 : add filter to array pointers.       ( version 3.10 )
  !/    30-oct-2006 : add pars for partitioning.          ( version 3.10 )
  !/    26-mar-2007 : add pars for partitioning.          ( version 3.11 )
  !/    17-may-2007 : adding ntproc/naproc separation.    ( version 3.11 )
  !/    21-jun-2007 : dedicated output processes.         ( version 3.11 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
  !/                  (w. e. rogers & t. j. campbell, nrl)
  !/    14-jul-2010 : fix vaaux declaration bug.        ( version 3.14.2 )
  !/    27-jul-2010 : add nki, nthi, xfri, fr1i, th1i.  ( version 3.14.3 )
  !/    08-nov-2010 : implementing unstructured grids     ( version 3.14.4 )
  !/                  (a. roland and f. ardhuin)
  !/    18-dec-2012 : new 2d field output structure,      ( version 4.11 )
  !/                  reducing memory footprint for fields.
  !/    19-dec-2012 : move noswll to data structure.      ( version 4.11 )
  !/    10-dec-2014 : add checks for allocate status      ( version 5.04 )
  !/    27-aug-2015 : adding interpolated icef (mean ice  ( version 5.10 )
  !/                  floe diameter), iceh (ice thickness)
  !/                  and ice (ice concentration).
  !/    01-mar-2018 : include undef from constants.ftn to ( version 6.02 )
  !/                  avoid circular referencing in w3servmd
  !/    05-jun-2018 : add setup                           ( version 6.04 )
  !/    27-jul-2018 : added ptmeth and ptfcut variables   ( version 6.05 )
  !/                  for alternative partition methods.
  !/                  (c. bunney, ukmo)
  !/    25-sep-2020 : flags for coupling restart          ( version 7.10 )
  !/    15-jan-2020 : added tp based on existing fp       ( version 7.12 )
  !/                  internal fields. (c. bunney, ukmo)
  !/    22-mar-2021 : add extra coupling variables        ( version 7.13 )
  !/    07-jun-2021 : s_{nl} gke nl5 (q. liu)             ( version 7.13 )
  !/    19-jul-2021 : momentum and air density support    ( version 7.14 )
  !/
  !/    copyright 2009-2012 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     define data structures to set up wave model grids and aliases
  !     to use individual grids transparently. also includes subroutines
  !     to manage data structure and pointing to individual models.
  !     this module considers the parameters required for model output.
  !
  !  2. variables and types :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      noutp     int.  public   number of models in array dim.
  !      ioutp     int.  public   selected model for output, init. at -1.
  !      iostyp    int.  public   output data server type.
  !      nogrp     i.p.  public   number of output field groups
  !      ngrpp     i.p.  public   max numb of parameters per output group
  !      noge      i.p.  public   number of output group elements
  !      notype    i.p.  public   number of output types
  !      noextr    i.p.  public   number of extra (user available)
  !                               output fields.
  !      dimp      i.p.  public   number of parameters in partition
  !                               output group
  !      idout     c.a.  public   id strings for output fields.
  !      fnmpre    char  public   file name preamble.
  !      undef     real  public   value for undefined parameters in
  !                               gridded output fields.
  !      unipts    log.  public   flag for unified point output (output
  !                               to single file).
  !      upproc    log.  public   flag for dedicated point output proc.
  !      output    type  public   data structure defining output.
  !      outpts    grid  public   array of output for models.
  !     ----------------------------------------------------------------
  !
  !     elements of output are aliased to pointers with the same
  !     name. these pointers are defined as :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      ndso      int.  public   general output unit number ("log
  !                               file").
  !      ndse      int.  public   error output unit number.
  !      ndst      int.  public   test output unit number.
  !      screen    int.  public   unit for 'direct' output.
  !      ntproc    int.  public   number of processors.
  !      naproc    int.  public   number of processors for computation.
  !      iaproc    int.  public   actual processor number (base 1),
  !      naplog    int.  public   proc. dealing with log output.
  !      napout    int.  public   proc. dealing with standard output.
  !      naperr    int.  public   proc. dealing with error output.
  !      napfld    int.  public   proc. dealing with raw field output.
  !      nappnt    int.  public   proc. dealing with raw point output.
  !      naptrk    int.  public   proc. dealing with track output.
  !      naprst    int.  public   proc. dealing with restart output.
  !      napbpt    int.  public   proc. dealing with boundary output.
  !      napprt    int.  public   proc. dealing with partition output.
  !      noswll    i.p.  public   number of swell fields from part.
  !                               to be used in field output.
  !      tosnl5    i.a.  public   times for point ouput (!/nl5)
  !      tofrst    i.a.  public   times for first output.
  !      tonext    i.a.  public   times for next output.
  !      tolast    i.a.  public   times for last output.
  !      tbpi0     i.a   public   time of first set of input boundary
  !                               spectra.
  !      tbpin     i.a   public   id. second set.
  !      nds       i.a.  public   data set numbers (see w3init).
  !      dtout     r.a.  public   output intervals.
  !      flout     l.a.  public   output flags.
  !      out1      type  public   data structure of type otype1 with
  !                               suppl. data for output type 1.
  !      out2      type  public   data structure of type otype2 with
  !                               suppl. data for output type 2.
  !      out3      type  public   data structure of type otype3 with
  !                               suppl. data for output type 3.
  !      out4      type  public   data structure of type otype4 with
  !                               suppl. data for output type 4.
  !      out5      type  public   data structure of type otype5 with
  !                               suppl. data for output type 5.
  !      out6      type  public   data structure of type otype6 with
  !                               suppl. data for output type 6.
  !      ofiles   i.a.  public   output in one or several files.
  !     ----------------------------------------------------------------
  !
  !     elements of out1 are aliased to pointers with the same
  !     name. these pointers are defined as :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      ipass1    int.  public   pass counter for file management,
  !                               renamed to ipass in routine.
  !      write1    int.  public   write flag for file management,
  !                               renamed to write in routine.
  !      nrqgo(2)  int.  public   number of mpi handles w3iogo.
  !      irqgo     i.a.  public   array with mpi handles w3iogo.
  !      flogrd    l.a.  public   flags for output fields.
  !      flogr2    l.a.  public   flags for coupling fields.
  !      flogrr    l.a.  public   flags for optional coupling restart (2d).
  !      flogd     l.a.  public   flags for output groups
  !      flog2     l.a.  public   flags for coupling groups
  !      flogr     l.a.  public   flags for optional coupling restart (1d).
  !     ----------------------------------------------------------------
  !
  !     elements of out2 are aliased to pointers with the same
  !     name. these pointers are defined as :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      ipass2    int.  public   pass counter for file management,
  !                               renamed to ipass in routine.
  !      nopts     int.  public   number of output points.
  !      nrqpo(2)  int.  public   number of mpi handles irqpon. (!/mpi)
  !      iptint    i.a.  public   (i,j)-indices of enclosing cell corner points
  !      il        i.a.  public   number of land points in interpola-
  !                               tion box for output point.
  !      iw        i.a.  public   id. water.
  !      ii        i.a.  public   id. ice.
  !      irqpo1/2  i.a.  public   array with mpi handles.       (!/mpi)
  !      ptloc     r.a.  public   output locations.
  !      ptifac    r.a.  public   interpolation weights.
  !      dpo       r.a.  public   interpolated depths.
  !      wao       r.a.  public   interpolated wind speeds.
  !      wdo       r.a.  public   interpolated wind directions.
  !      aso       r.a.  public   interpolated air-sea temp. diff.
  !      tauao     r.a.  public   interpolated atm. stresses.
  !      taudo     r.a.  public   interpolated atm. stres directions.
  !      dairo     r.a.  public   interpolated rho atmosphere.
  !      cao       r.a.  public   interpolated current speeds.
  !      cdo       r.a.  public   interpolated current directions.
  !      spco      r.a.  public   output spectra.
  !      iceo      r.a.  public   interpolated ice concentration.
  !      iceho     r.a.  public   interpolated ice thickness.
  !      icefo     r.a.  public   interpolated ice floe.
  !      ptnme     c.a.  public   name of output locations.
  !      grdid     c.a.  public   originating grid id.
  !      o2init    log.  public   flag for array initialization.
  !      o2irqi    log.  public   flag for array initialization.
  !     ----------------------------------------------------------------
  !
  !     elements of out3 are aliased to pointers with the same
  !     name. these pointers are defined as :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      ipass3    int.  public   pass counter for file management,
  !                               renamed to ipass in routine.
  !      it0pnt    int.  public   base tag number of mpi communication.
  !      it0trk    int.  public   base tag number of mpi communication.
  !      it0prt    int.  public   base tag number of mpi communication.
  !      nrqtr     int.  public   number of handles in irqtr.
  !      irqtr     i.a.  public   array with mpi handles.
  !      o3init    log.  public   flag for array initialization.
  !      stop      log.  public   flag for end of output.
  !      maskn     l.a.  public   mask arrays for internal use.
  !     ----------------------------------------------------------------
  !
  !     elements of out4 are aliased to pointers with the same
  !     name. these pointers are defined as :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      ifile4    int.  public   file number for output files.
  !      nblkrs    int.  public   number of blocks in communication of
  !                               spectra.
  !      rsblks    int.  public   corresponding block size.
  !      nrqsr     int.  public   number of mpi handles.
  !      irqrs     i.a.  public   array with mpi handles.
  !      irqrss    i.a.  public   array with mpi handles.
  !      vaaux     r.a.  public   aux. spectra storage.
  !     ----------------------------------------------------------------
  !
  !     elements of out5 are aliased to pointers with the same
  !     name. these pointers are defined as :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      nbi(2)    int.  public   number of input bound. points.
  !      nfbpo     int.  public   number of files for output bound. data.
  !      nrqbp(2)  int.  public   number of mpi handles.
  !      nki,nthi  int.  public   size of input spectra
  !      nbo(2)    i.a.  public   number of output bound. pts. per file.
  !      ndsl      i.a.  public   array with unit numbers.
  !      ipbpi     i.a.  public   interpolation data input b.p.
  !      isbpi     i.a.  public   sea point counters for input b.p.
  !      irqbp1/2  i.a.  public   array with mpi handles.
  !      xfri, fr1i, th1i
  !                real  public   definition of input spectra.
  !      x/ybpi    r.a.  public   location of input boundary points.
  !      rdbpi     r.a.  public   interpolation factors input b.p.
  !      abpi0/n   r.a.  public   storage of spectra from which to
  !                               interpolate b.d.
  !      bbpi0/n   r.a.  public   idem, secondary storage.
  !      abpos     r.a.  public   temporarily storage for output b.d.
  !      ipbpo, isbpo, x/ybpo, rdbpo
  !                misc. public   id. for output b.p.
  !      flbpi     log.  public   flag for input of boundary data.
  !      flbpo     log.  public   flag for output of boundary data.
  !      filer/w/d log.  public   read/write flags for file management.
  !      spconv    log.  public   flag for change of spectral res.
  !      o5inin    log.  public   flag for array initializations.
  !     ----------------------------------------------------------------
  !
  !     elements of out6 are aliased to pointers with the same
  !     name. these pointers are defined as :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      ipass6    int.  public   pass counter for file management,
  !                               renamed to ipass in routine.
  !      ihmax     int.  public   number of discrete spectral levels.
  !      ix0/n/s   int.  public   first-last-step ix counters.
  !      iy0/n/s   int.  public   idem iy counters.
  !      hspmin    real  public   minimum significant height per part.
  !      wsmult    real  public   multiplier for wind sea boundary.
  !      wscut     real  public   cut-off wind factor for wind seas.
  !      icprt     i.a.  public   counters for partitions.
  !      dtprt     r.a.  public   data from partitions.
  !      flcomb    log.  public   flag for combining wind seas.
  !      flform    log.  public   flag for (un)formatted output
  !      o6init    log.  public   flag for array initializations.
  !     ----------------------------------------------------------------
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3nout    subr. public   set number of grids.
  !      w3dmo2    subr. public   allocate arrays output type 2.
  !      w3dmo3    subr. public   allocate arrays output type 3.
  !      w3dmo5    subr. public   allocate arrays output type 5.
  !      w3seto    subr. public   point to selected grid / model.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      w3setg    subr. w3gdatmd point to proper model grid.
  !      strace    subr. w3servmd subroutine tracing.
  !      extcde    subr. w3servmd abort program with exit code.
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !
  !     - the number of grids is taken from w3gdatmd, and needs to be
  !       set first with w3dimg.
  !
  !  6. switches :
  !
  !     !/mpi    mpi specific calls.
  !     !/s      enable subroutine tracing.
  !     !/t      enable test output
  !
  !  7. source code :
  !
  !/ ------------------------------------------------------------------- /
  use constants, only : undef
  ! module default
  implicit none
  public
  !/
  !/ module private variable for checking error returns
  !/
  integer, private        :: istat
  !/
  !/ conventional declarations
  !/
  integer                 :: noutp = -1, ioutp = -1, iostyp = 1
  !
  integer, parameter      :: nogrp = 10
  integer, parameter      :: ngrpp = 20
  integer, parameter      :: dimp = 15
  integer                 :: noge(nogrp)
  integer                 :: notype
  integer, parameter      :: noextr=  2
  character(len=20)       :: idout(nogrp,ngrpp)
  character(len=80)       :: fnmpre = './'
  !moved undef to constants and included above
  !real                    :: undef = -999.9
  logical                 :: unipts = .false., upproc = .false.
  !/
  !/ set noge and idout identifiers in w3nout
  !/
  !/ data structures
  !/
  type otype1
    integer               :: ipass1
    integer               :: nrqgo, nrqgo2
    integer, pointer      :: irqgo(:), irqgo2(:)
    logical               :: flogrd(nogrp,ngrpp), flogd(nogrp),   &
         flogr2(nogrp,ngrpp), flog2(nogrp),   &
         flogrr(nogrp,ngrpp), flogr(nogrp),   &
         write1
  end type otype1
  !/
  type otype2
    integer               :: ipass2, nopts
    integer               :: nrqpo, nrqpo2
    integer, pointer      :: iptint(:,:,:), il(:), iw(:), ii(:)
    integer, pointer      :: irqpo1(:), irqpo2(:)
    real, pointer         :: ptloc(:,:), ptifac(:,:),             &
         dpo(:), wao(:), wdo(:), aso(:),      &
         cao(:), cdo(:), iceo(:), iceho(:),   &
         icefo(:), spco(:,:)
    real, pointer         :: zet_seto(:)  ! for the wave setup.
    character(len=40), pointer :: ptnme(:)
    character(len=13), pointer :: grdid(:)
    logical               :: o2init
    logical               :: o2irqi
  end type otype2
  !/
  type otype3
    integer               :: ipass3
    integer               :: it0pnt, it0trk, it0prt, nrqtr
    integer, pointer      :: irqtr(:)
    logical               :: o3init, stop
    logical, pointer      :: mask1(:,:), mask2(:,:)
    character(len=32), pointer  :: trckid(:,:)
  end type otype3
  !/
  type otype4
    integer               :: ifile4
    integer               :: nrqrs, nblkrs, rsblks
    integer, pointer      :: irqrs(:), irqrss(:)
    real, pointer         :: vaaux(:,:,:)
  end type otype4
  !/
  type otype5
    integer               :: nbi, nbi2, nfbpo, nbo(0:9),          &
         nbo2(0:9), ndsl(9), nki, nthi
    integer               :: nrqbp = 0, nrqbp2 = 0
    integer, pointer      :: ipbpi(:,:), isbpi(:),                &
         ipbpo(:,:), isbpo(:)
    integer, pointer      :: irqbp1(:), irqbp2(:)
    real                  :: xfri, fr1i, th1i
    real, pointer         :: xbpi(:), ybpi(:), rdbpi(:,:),        &
         xbpo(:), ybpo(:), rdbpo(:,:),        &
         abpi0(:,:), abpin(:,:), abpos(:,:),  &
         bbpi0(:,:), bbpin(:,:)
    logical               :: o5ini1, o5ini2, o5ini3, o5ini4
    logical               :: flbpi, flbpo, filer, filew, filed,   &
         spconv
  end type otype5
  !/
  type otype6
    integer               :: ipass6, ihmax, ix0, ixn, ixs,        &
         iy0, iyn, iys
    integer, pointer      :: icprt(:,:)
    real                  :: hspmin, wsmult, wscut
    real, pointer         :: dtprt(:,:)
    logical               :: flform, flcomb, o6init
    integer               :: ptmeth   ! c. bunney; partitioning method
    real                  :: ptfcut   ! c. bunney; part. 5 freq cut
  end type otype6
  !/
  type output
    integer               :: ndso, ndse, ndst, screen
    integer               :: ntproc, naproc, iaproc, naplog,      &
         napout, naperr, napfld, nappnt,      &
         naptrk, naprst, napbpt, napprt
    integer               :: noswll
    integer               :: tofrst(2), tonext(2,8), tolast(2,8), &
         tbpi0(2), tbpin(2), nds(13), ofiles(7)
    real                  :: dtout(8)
    logical               :: flout(8)
    type(otype1)          :: out1
    type(otype2)          :: out2
    type(otype3)          :: out3
    type(otype4)          :: out4
    type(otype5)          :: out5
    type(otype6)          :: out6
  end type output
  !/
  !/ data storage
  !/
  type(output), target, allocatable :: outpts(:)
  !/
  !/ data aliasses for structure output
  !/
  integer, pointer        :: ndso, ndse, ndst, screen
  integer, pointer        :: ntproc, naproc, iaproc, naplog,      &
       napout, naperr, napfld, nappnt,      &
       naptrk, naprst, napbpt, napprt
  integer, pointer        :: noswll
  integer, pointer        :: tofrst(:), tonext(:,:), tolast(:,:), &
       tbpi0(:), tbpin(:), nds(:)
  integer, pointer        :: ofiles(:)
  real, pointer           :: dtout(:)
  logical, pointer        :: flout(:)
  !/
  !/ data aliasses for substructures for output types
  !/ type 1 ...
  !/
  integer, pointer        :: ipass1
  integer, pointer        :: nrqgo, nrqgo2
  integer, pointer        :: irqgo(:), irqgo2(:)
  logical, pointer        :: flogrd(:,:), flogr2(:,:),            &
       flogrr(:,:),flogd(:), flog2(:),      &
       flogr(:), write1
  !/
  !/ type 2 ...
  !/
  integer, pointer        :: ipass2, nopts
  integer, pointer        :: nrqpo, nrqpo2
  integer, pointer        :: iptint(:,:,:), il(:), iw(:), ii(:)
  integer, pointer        :: irqpo1(:), irqpo2(:)
  real, pointer           :: ptloc(:,:), ptifac(:,:),             &
       dpo(:), wao(:), wdo(:), aso(:),      &
       cao(:), cdo(:), iceo(:), iceho(:),   &
       icefo(:), spco(:,:)
  real, pointer           :: zet_seto(:)
  !
  character(len=40), pointer :: ptnme(:)
  character(len=13), pointer :: grdid(:)
  logical, pointer        :: o2init
  logical, pointer      :: o2irqi
  !/
  !/ type 3 ...
  !/
  integer, pointer        :: ipass3
  integer, pointer        :: it0pnt, it0trk, it0prt, nrqtr
  integer, pointer        :: irqtr(:)
  logical, pointer        :: o3init, stop
  logical, pointer        :: mask1(:,:), mask2(:,:)
  character(len=32), pointer   :: trckid(:,:)
  !/
  !/ type 4 ...
  !/
  integer, pointer        :: ifile4
  integer, pointer        :: nrqrs, nblkrs, rsblks
  integer, pointer        :: irqrs(:), irqrss(:)
  real, pointer           :: vaaux(:,:,:)
  !/
  !/ type 5 ...
  !/
  integer, pointer        :: nbi, nbi2, nfbpo, nki, nthi
  integer, pointer        :: nbo(:), nbo2(:), ndsl(:)
  integer, pointer        :: nrqbp, nrqbp2
  integer, pointer        :: ipbpi(:,:), isbpi(:),                &
       ipbpo(:,:), isbpo(:)
  integer, pointer        :: irqbp1(:), irqbp2(:)
  real, pointer           :: xfri, fr1i, th1i
  real, pointer           :: xbpi(:), ybpi(:), rdbpi(:,:),        &
       xbpo(:), ybpo(:), rdbpo(:,:),        &
       abpi0(:,:), abpin(:,:), abpos(:,:),  &
       bbpi0(:,:), bbpin(:,:)
  logical, pointer        :: o5ini1, o5ini2, o5ini3, o5ini4
  logical, pointer        :: flbpi, flbpo, filer, filew, filed,   &
       spconv
  !/
  !/ type 6 ...
  !/
  integer, pointer        :: ipass6, ihmax, ix0, ixn, ixs,        &
       iy0, iyn, iys, icprt(:,:)
  real, pointer           :: hspmin, wsmult, wscut, dtprt(:,:)
  logical, pointer        :: flform, flcomb, o6init
  integer, pointer        :: ptmeth   ! c. bunney; partitioning method
  real, pointer           :: ptfcut   ! c. bunney; part. 5 freq cut
  character(len=8)   :: runtype = ''                 !< @public the run type (startup,branch,continue)
  character(len=256) :: initfile = ''                !< @public name of wave initial condition file
                                                     !! if runtype is startup or branch run, then initfile is used
  logical            :: use_user_histname = .false.  !<@public logical flag for user set history filenames
  logical            :: use_user_restname = .false.  !<@public logical flag for user set restart filenames
  character(len=512) :: user_histfname = ''          !<@public user history filename prefix, timestring
                                                     !! yyyy-mm-dd-sssss will be appended
  character(len=512) :: user_restfname = ''          !<@public user restart filename prefix, timestring
                                                     !! yyyy-mm-dd-sssss will be appended
  logical            :: histwr = .false.             !<@public logical to trigger history write
                                                     !! if true => write history file (snapshot)
  logical            :: rstwr = .false.              !<@public logical to trigger restart write
                                                     !! if true => write restart
  logical            :: user_netcdf_grdout = .false. !<@public logical flag to use netcdf for gridded
                                                     !! field output
  character(len= 36) :: time_origin = ''             !< @public the time_origin used for netcdf output
  character(len= 36) :: calendar_name = ''           !< @public the calendar used for netcdf output
  integer(kind=8)    :: elapsed_secs = 0             !< @public the time in seconds from the time_origin
  !/
contains
  !/ ------------------------------------------------------------------- /
  subroutine w3nout ( ndserr, ndstst )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    13-dec-2004 : origination.                        ( version 3.06 )
    !/    27-jun-2006 : adding file name preamble           ( version 3.09 )
    !/    24-jul-2006 : adding unified point output storage.( version 3.10 )
    !/    04-oct-2006 : add filter to array pointers.       ( version 3.10 )
    !/    30-oct-2006 : add pars for partitioning.          ( version 3.10 )
    !/    26-mar-2007 : add pars for partitioning.          ( version 3.11 )
    !/    17-may-2007 : adding ntproc/naproc separation.    ( version 3.11 )
    !/    18-dec-2012 : moving idout initialization here.   ( version 4.11 )
    !/    19-dec-2012 : move noswll to data structure.      ( version 4.11 )
    !/    10-dec-2014 : add checks for allocate status      ( version 5.04 )
    !/    22-mar-2021 : add extra coupling variables        ( version 7.13 )
    !/
    !  1. purpose :
    !
    !     set up the number of grids to be used.
    !
    !  2. method :
    !
    !     use data stored in ngrids in w3gdatmd.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       ndserr  int.   i   error output unit number.
    !       ndstst  int.   i   test output unit number.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     see module documentation below.
    !
    !  5. called by :
    !
    !     any main program that uses this grid structure.
    !
    !  6. error messages :
    !
    !     - error checks on previous setting of variable ngrids.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !  9. switches :
    !
    !     !/s    enable subroutine tracing.
    !     !/t    enable test output
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: ngrids, nauxgr
    use w3servmd, only: extcde
    !
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: ndserr, ndstst
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: i, nlow, j
    character(len=20)       :: string
    !/
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input and module status
    !
    if ( ngrids .eq. -1 ) then
      write (ndserr,1001) ngrids
      call extcde (1)
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  set variable and allocate arrays
    !
    nlow   = min ( 0 , -nauxgr )
    allocate ( outpts(nlow:ngrids), stat=istat )
    check_alloc_status ( istat )
    noutp  = ngrids
    !
    ! -------------------------------------------------------------------- /
    ! 3.  initialize parameters
    !
    do i=nlow, ngrids
      !
      outpts(i)%ndso   = 6
      outpts(i)%ndse   = 6
      outpts(i)%ndst   = 6
      outpts(i)%screen = 6
      !
      outpts(i)%ntproc = 1
      outpts(i)%naproc = 1
      outpts(i)%iaproc = 1
      outpts(i)%naplog = 1
      outpts(i)%napout = 1
      outpts(i)%naperr = 1
      outpts(i)%napfld = 1
      outpts(i)%nappnt = 1
      outpts(i)%naptrk = 1
      outpts(i)%naprst = 1
      outpts(i)%napbpt = 1
      outpts(i)%napprt = 1
      !
      outpts(i)%noswll = -1
      !
      outpts(i)%tbpi0 = (-1,0)
      outpts(i)%tbpin = (-1,0)
      !
      outpts(i)%out1%ipass1 = 0
      outpts(i)%out1%nrqgo  = 0
      outpts(i)%out1%nrqgo2 = 0
      !
      outpts(i)%out2%ipass2 = 0
      outpts(i)%out2%nopts  = 0
      outpts(i)%out2%o2init = .false.
      outpts(i)%out2%o2irqi = .false.
      !
      outpts(i)%out3%ipass3 = 0
      outpts(i)%out3%o3init = .false.
      outpts(i)%out3%stop   = .false.
      outpts(i)%out3%nrqtr  = 0
      !
      outpts(i)%out4%ifile4 = 0
      outpts(i)%out4%nrqrs  = 0
      !
      outpts(i)%out5%o5ini1 = .false.
      outpts(i)%out5%o5ini2 = .false.
      outpts(i)%out5%o5ini3 = .false.
      outpts(i)%out5%o5ini4 = .false.
      outpts(i)%out5%filer  = .true.
      outpts(i)%out5%filew  = .true.
      outpts(i)%out5%filed  = .true.
      !
      outpts(i)%out6%ipass6 = 0
      outpts(i)%out6%o6init = .false.
      !
    end do
    !
    !     set idout
    !       commented outlines represent reserved slots.
    !
    do i=1, nogrp
      do j=1, ngrpp
        idout(i,j) = 'undefined / not used'
      end do
    end do
    !
    ! 1) forcing fields
    !
    noge(1) = 9
    !
    idout( 1, 1)  = 'water depth         '
    idout( 1, 2)  = 'current vel.        '
    idout( 1, 3)  = 'wind speed          '
    idout( 1, 4)  = 'air-sea temp. dif.  '
    idout( 1, 5)  = 'water level         '
    idout( 1, 6)  = 'ice concentration   '
    idout( 1, 7)  = 'iceberg damp coeffic'
    idout( 1, 8)  = 'atmospheric momentum'
    idout( 1, 9)  = 'air density         '
    !
    ! 2) standard mean wave parameters
    !
    noge(2) = 19
    !
    idout( 2, 1)  = 'wave height         '
    idout( 2, 2)  = 'mean wave length    '
    idout( 2, 3)  = 'mean wave period(+2)'
    idout( 2, 4)  = 'mean wave period(-1)'
    idout( 2, 5)  = 'mean wave period(+1)'
    idout( 2, 6)  = 'peak frequency      '
    idout( 2, 7)  = 'mean wave dir. a1b1 '
    idout( 2, 8)  = 'mean dir. spr. a1b1 '
    idout( 2, 9)  = 'peak direction      '
    idout( 2, 10)  = 'infragravity height'
    idout( 2, 11)  = 'space-time max e   '
    idout( 2, 12)  = 'space-time max std '
    idout( 2, 13)  = 'space-time hmax    '
    idout( 2, 14)  = 'spc-time hmax^crest'
    idout( 2, 15)  = 'std space-time hmax'
    idout( 2, 16)  = 'std st hmax^crest  '
    idout( 2, 17)  = 'dominant wave bt   '
    idout( 2, 18)  = 'peak prd. (from fp)'
    idout( 2, 19)  = 'mean wave number   '
    !      idout( 2,10)  = 'mean wave dir. a2b2'
    !      idout( 2,11)  = 'mean dir. spr. a2b2'
    !      idout( 2,12)  = 'windsea height(sin)'
    !      idout( 2,13)  = 'windsea peak f(sin)'
    !      idout( 2,14)  = 'subrange waveheight'
    !
    ! 3) frequency-dependent standard parameters
    !
    noge(3) = 6
    !
    idout( 3, 1)  = '1d freq. spectrum   '
    idout( 3, 2)  = 'mean wave dir. a1b1 '
    idout( 3, 3)  = 'mean dir. spr. a1b1 '
    idout( 3, 4)  = 'mean wave dir. a2b2 '
    idout( 3, 5)  = 'mean dir. spr. a2b2 '
    idout( 3, 6)  = 'wavenumber array    '
    !
    ! 4) spectral partitions parameters
    !
    noge(4) = 17
    !
    idout( 4, 1)  = 'part. wave height   '
    idout( 4, 2)  = 'part. peak period   '
    idout( 4, 3)  = 'part. peak wave len.'
    idout( 4, 4)  = 'part. mean direction'
    idout( 4, 5)  = 'part. dir. spread   '
    idout( 4, 6)  = 'part. wind sea frac.'
    idout( 4, 7)  = 'part. peak direction'
    idout( 4, 8)  = 'part. peakedness    '
    idout( 4, 9)  = 'part. peak enh. fac.'
    idout( 4,10)  = 'part. gaussian width'
    idout( 4,11)  = 'part. spectral width'
    idout( 4,12)  = 'part. mean per. (-1)'
    idout( 4,13)  = 'part. mean per. (+1)'
    idout( 4,14)  = 'part. mean per. (+2)'
    idout( 4,15)  = 'part. peak density  '
    idout( 4,16)  = 'total wind sea frac.'
    idout( 4,17)  = 'number of partitions'
    !
    ! 5) atmosphere-waves layer
    !
    noge(5) = 11
    !
    idout( 5, 1)  = 'friction velocity   '
    idout( 5, 2)  = 'charnock parameter  '
    idout( 5, 3)  = 'energy flux         '
    idout( 5, 4)  = 'wind-wave enrgy flux'
    idout( 5, 5)  = 'wind-wave net mom. f'
    idout( 5, 6)  = 'wind-wave neg.mom.f.'
    idout( 5, 7)  = 'whitecap coverage   '
    idout( 5, 8)  = 'whitecap mean thick.'
    idout( 5, 9)  = 'mean breaking height'
    idout( 5,10)  = 'dominant break prob '
    idout( 5,11)  = 'wind sea period' ! c.bunney - reinstated this as is used in ww3_ounf
    ! is it suposed to be defunct? it is not in ww3_outf...
    !
    ! 6) wave-ocean layer
    !
    noge(6) = 14
    !
    idout( 6, 1)  = 'radiation stresses  '
    idout( 6, 2)  = 'wave-ocean mom. flux'
    idout( 6, 3)  = 'wave ind p bern head'
    idout( 6, 4)  = 'wave-ocean tke  flux'
    idout( 6, 5)  = 'stokes transport    '
    idout( 6, 6)  = 'stokes drift at z=0 '
    idout( 6, 7)  = '2nd order pressure  '
    idout( 6, 8)  = 'stokes drft spectrum'
    idout( 6, 9)  = '2nd ord press spectr'
    idout( 6,10)  = 'wave-ice mom. flux  '
    idout( 6,11)  = 'wave-ice energy flux'
    idout( 6,12)  = 'split surface stokes'
    idout( 6,13)  = 'tot wav-ocn mom flux'
    idout( 6,14)  = 'stokes drift sfc ave'
    !
    ! 7) wave-bottom layer
    !
    noge(7) = 5
    !
    idout( 7, 1)  = 'bottom rms ampl.    '
    idout( 7, 2)  = 'bottom rms velocity '
    idout( 7, 3)  = 'bedform parameters  '
    idout( 7, 4)  = 'energy diss. in wbbl'
    idout( 7, 5)  = 'moment. loss in wbbl'
    !      idout( 7, 6)  = 'bottom mean period  '
    !      idout( 7, 7)  = 'bottom mean direct  '
    !      idout( 7, 8)  = 'bottom direct spread'
    !      idout( 7, 9)  = 'calc grain rough k_n'
    !
    ! 8) spectrum parameters
    !
    noge(8) = 5
    !
    idout( 8, 1)  = 'mean square slopes  '
    idout( 8, 2)  = 'phillips tail const'
    idout( 8, 3)  = 'slope direction     '
    idout( 8, 4)  = 'tail slope direction'
    idout( 8, 5)  = 'goda peakedness parm'
    !      idout( 8, 3)  = 'lx-ly mean wvlength'
    !      idout( 8, 4)  = 'surf grad correl xt'
    !      idout( 8, 5)  = 'surf grad correl yt'
    !      idout( 8, 6)  = 'surf grad correl xy'
    !      idout( 8, 7)  = 'surface crest param'
    !      idout( 8, 3)  = '3rd spectral moment '
    !      idout( 8, 4)  = '4th spectral moment '
    !      idout( 8, 6)  = 'kurtosis            '
    !      idout( 8, 7)  = 'skewness            '
    !
    ! 9) numerical diagnostics
    !
    noge(9) = 5
    !
    idout( 9, 1)  = 'avg. time step.     '
    idout( 9, 2)  = 'cut-off freq.       '
    idout( 9, 3)  = 'maximum spatial cfl '
    idout( 9, 4)  = 'maximum angular cfl '
    idout( 9, 5)  = 'maximum k advect cfl'
    !      idout( 9, 6)  = 'avg intrsp proptstep'
    !
    ! 10) user defined
    !
    noge(10) = noextr
    !
    do i=1, min ( 20 , noextr )
      write (string,'(a14,i2.2,a4)') 'user defined #', i, '    '
      idout(10, i) = string
    end do
    !
    !
    return
    !
    ! formats
    !
1001 format (/' *** error w3nout : ngrids not yet set *** '/         &
         '                    ngrids = ',i10/                   &
         '                    run w3nmod first'/)
    !
    !/
    !/ end of w3nout ----------------------------------------------------- /
    !/
  end subroutine w3nout
  !/ ------------------------------------------------------------------- /
  subroutine w3dmo2 ( imod, ndse, ndst, npt )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         10-dec-2014 |
    !/                  +-----------------------------------+
    !/
    !/    10-nov-2004 : origination.                        ( version 3.06 )
    !/    24-jul-2006 : adding unified point output storage.( version 3.10 )
    !/    25-jul-2006 : originating grid id for points.     ( version 3.10 )
    !/    04-oct-2006 : add filter to array pointers.       ( version 3.10 )
    !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    10-dec-2014 : add checks for allocate status      ( version 5.04 )
    !/
    !  1. purpose :
    !
    !     initialize an individual data storage for point output.
    !
    !  2. method :
    !
    !     allocate directly into the structure array. note that
    !     this cannot be done through the pointer alias!
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       imod    int.   i   model number to point to.
    !       ndse    int.   i   error output unit number.
    !       ndst    int.   i   test output unit number.
    !       npt     int.   i   array size.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     see module documentation below.
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3iopo    subr. w3iopomd point output module.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     - check on input parameters.
    !     - check on previous allocation.
    !
    !  7. remarks :
    !
    !     - w3seto needs to be called after allocation to point to
    !       proper allocated arrays.
    !     - note that nopts is overwritten in w3iopp.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s      enable subroutine tracing.
    !     !/t    enable test output
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: w3setg, ngrids, nauxgr, igrid, nspec
    use w3servmd, only: extcde
    !
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)           :: imod, ndse, ndst, npt
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: jgrid, nlow
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input and module status
    !
    if ( ngrids .eq. -1 ) then
      write (ndse,1001)
      call extcde (1)
    end if
    !
    nlow   = min ( 0 , -nauxgr )
    if ( imod.lt.nlow .or. imod.gt.noutp ) then
      write (ndse,1002) imod, nlow, noutp
      call extcde (2)
    end if
    !
    if ( outpts(imod)%out2%o2init ) then
      write (ndse,1003)
      call extcde (3)
    end if
    !
    !
    jgrid  = igrid
    if ( jgrid .ne. imod ) call w3setg ( imod, ndse, ndst )
    !
    ! -------------------------------------------------------------------- /
    ! 2.  allocate arrays
    !
    allocate ( outpts(imod)%out2%iptint(2,4,npt) ,            &
         outpts(imod)%out2%il(npt)         ,                  &
         outpts(imod)%out2%iw(npt)         ,                  &
         outpts(imod)%out2%ii(npt)         ,                  &
         outpts(imod)%out2%ptifac(4,npt)   ,                  &
         outpts(imod)%out2%ptnme(npt)      ,                  &
         outpts(imod)%out2%grdid(npt)      ,                  &
         outpts(imod)%out2%dpo(npt)        ,                  &
         outpts(imod)%out2%wao(npt)        ,                  &
         outpts(imod)%out2%zet_seto(npt)   ,                  &
         outpts(imod)%out2%wdo(npt)        ,                  &
         outpts(imod)%out2%aso(npt)        ,                  &
         outpts(imod)%out2%cao(npt)        ,                  &
         outpts(imod)%out2%cdo(npt)        ,                  &
         outpts(imod)%out2%iceo(npt)       ,                  &
         outpts(imod)%out2%iceho(npt)      ,                  &
         outpts(imod)%out2%icefo(npt)      ,                  &
         outpts(imod)%out2%spco(nspec,npt) ,                  &
         outpts(imod)%out2%ptloc(2,npt)    , stat=istat       )
    check_alloc_status ( istat )
    !
    outpts(imod)%out2%o2init = .true.
    !
    !
    ! -------------------------------------------------------------------- /
    ! 3.  point to allocated arrays
    !
    call w3seto ( imod, ndse, ndst )
    !
    !
    ! -------------------------------------------------------------------- /
    ! 4.  update counters in grid
    !
    nopts  = npt
    !
    !
    ! -------------------------------------------------------------------- /
    ! 5.  restore previous grid setting if necessary
    !
    if ( jgrid .ne. imod ) call w3setg ( jgrid, ndse, ndst )
    !
    return
    !
    ! formats
    !
1001 format (/' *** error w3dmo2 : grids not initialized *** '/ &
         '                    run w3nmod first '/)
1002 format (/' *** error w3dmo2 : illegal model number *** '/  &
         '                    imod   = ',i10/                   &
         '                    nlow   = ',i10/                   &
         '                    noutp  = ',i10/)
1003 format (/' *** error w3dmo2 : array(s) already allocated *** ')
    !
    !/
    !/ end of w3dmo2 ----------------------------------------------------- /
    !/
  end subroutine w3dmo2
  !/ ------------------------------------------------------------------- /
  subroutine w3dmo3 ( imod, ndse, ndst )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         10-dec-2014 !
    !/                  +-----------------------------------+
    !/
    !/    24-nov-2004 : origination.                        ( version 3.06 )
    !/    10-dec-2014 : add checks for allocate status      ( version 5.04 )
    !/
    !  1. purpose :
    !
    !     initialize an individual data storage for track output.
    !
    !  2. method :
    !
    !     allocate directly into the structure array. note that
    !     this cannot be done through the pointer alias!
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       imod    int.   i   model number to point to.
    !       ndse    int.   i   error output unit number.
    !       ndst    int.   i   test output unit number.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     see module documentation below.
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3iotr    subr. w3iotrmd track output module.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     - check on input parameters.
    !     - check on previous allocation.
    !
    !  7. remarks :
    !
    !     - w3seto needs to be called after allocation to point to
    !       proper allocated arrays.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/shrd, !/dist, !/mpi
    !              shared / distributed memory model
    !
    !     !/s      enable subroutine tracing.
    !     !/t    enable test output
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: w3setg, ngrids, igrid, nx, ny
    use w3servmd, only: extcde
    !
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)           :: imod, ndse, ndst
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: jgrid
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input and module status
    !
    if ( ngrids .eq. -1 ) then
      write (ndse,1001)
      call extcde (1)
    end if
    !
    if ( imod.lt.1 .or. imod.gt.noutp ) then
      write (ndse,1002) imod, noutp
      call extcde (2)
    end if
    !
    if ( outpts(imod)%out3%o3init ) then
      write (ndse,1003)
      call extcde (3)
    end if
    !
    !
    jgrid  = igrid
    if ( jgrid .ne. imod ) call w3setg ( imod, ndse, ndst )
    !
    ! -------------------------------------------------------------------- /
    ! 2.  allocate arrays
    !
    allocate ( outpts(imod)%out3%mask1(ny,nx) ,               &
         outpts(imod)%out3%mask2(ny,nx) ,                     &
         outpts(imod)%out3%trckid(ny,nx), stat=istat          )
    check_alloc_status ( istat )
    !
    outpts(imod)%out3%o3init = .true.
    !
    !
    ! -------------------------------------------------------------------- /
    ! 3.  point to allocated arrays
    !
    call w3seto ( imod, ndse, ndst )
    !
    !
    ! -------------------------------------------------------------------- /
    ! 4.  update counters in grid
    !
    !
    ! -------------------------------------------------------------------- /
    ! 5.  restore previous grid setting if necessary
    !
    if ( jgrid .ne. imod ) call w3setg ( jgrid, ndse, ndst )
    !
    return
    !
    ! formats
    !
1001 format (/' *** error w3dmo3 : grids not initialized *** '/      &
         '                    run w3nmod first '/)
1002 format (/' *** error w3dmo3 : illegal model number *** '/       &
         '                    imod   = ',i10/                        &
         '                    noutp  = ',i10/)
1003 format (/' *** error w3dmo3 : array(s) already allocated *** ')
    !
    !/
    !/ end of w3dmo3 ----------------------------------------------------- /
    !/
  end subroutine w3dmo3
  !/ ------------------------------------------------------------------- /
  subroutine w3dmo5 ( imod, ndse, ndst, iblock )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         10-dec-2014 !
    !/                  +-----------------------------------+
    !/
    !/    13-dec-2004 : origination.                        ( version 3.06 )
    !/    06-sep-2005 : second storage for input bound. sp. ( version 3.08 )
    !/    10-dec-2014 : add checks for allocate status      ( version 5.04 )
    !/
    !  1. purpose :
    !
    !     initialize an individual data storage for boundary data.
    !
    !  2. method :
    !
    !     allocate directly into the structure array. note that
    !     this cannot be done through the pointer alias!
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       imod    int.   i   model number to point to.
    !       ndse    int.   i   error output unit number.
    !       ndst    int.   i   test output unit number.
    !       iblock  int.   i   select block to allocate.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     see module documentation below.
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3iobc    subr. w3iobcmd boundary data output module.
    !      w3iogr    subr. w3iogrmd grid data output module.
    !      w3wave    subr. w3wavemd actual wave model routine.
    !      ww3_grid  prog.   n/a    grid preprocessing program.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     - check on input parameters.
    !     - check on previous allocation.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s      enable subroutine tracing.
    !     !/t    enable test output
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: w3setg, ngrids, igrid, nx, ny, nspec
    use w3servmd, only: extcde
    !
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)           :: imod, ndse, ndst, iblock
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: jgrid
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input and module status
    !
    if ( ngrids .eq. -1 ) then
      write (ndse,1001)
      call extcde (1)
    end if
    !
    if ( imod.lt.1 .or. imod.gt.noutp ) then
      write (ndse,1002) imod, noutp
      call extcde (2)
    end if
    !
    !
    ! -------------------------------------------------------------------- /
    ! 2.  allocate arrays and reset pointers
    !
    select case (iblock)
      !
    case (1)
      !
      allocate ( outpts(imod)%out5%ipbpi(nbi,4),              &
           outpts(imod)%out5%isbpi(nbi)  ,                    &
           outpts(imod)%out5%xbpi(nbi)   ,                    &
           outpts(imod)%out5%ybpi(nbi)   ,                    &
           outpts(imod)%out5%rdbpi(nbi,4), stat=istat         )
      check_alloc_status ( istat )
      !
      ipbpi  => outpts(imod)%out5%ipbpi
      isbpi  => outpts(imod)%out5%isbpi
      xbpi   => outpts(imod)%out5%xbpi
      ybpi   => outpts(imod)%out5%ybpi
      rdbpi  => outpts(imod)%out5%rdbpi
      !
      outpts(imod)%out5%o5ini1 = .true.
      !
    case (2)
      !
      allocate ( outpts(imod)%out5%ipbpo(nbo(nfbpo),4),       &
           outpts(imod)%out5%isbpo(4*nbo(nfbpo)),             &
           outpts(imod)%out5%xbpo(nbo(nfbpo))   ,             &
           outpts(imod)%out5%ybpo(nbo(nfbpo))   ,             &
           outpts(imod)%out5%rdbpo(nbo(nfbpo),4), stat=istat  )
      check_alloc_status ( istat )
      !
      ipbpo  => outpts(imod)%out5%ipbpo
      isbpo  => outpts(imod)%out5%isbpo
      xbpo   => outpts(imod)%out5%xbpo
      ybpo   => outpts(imod)%out5%ybpo
      rdbpo  => outpts(imod)%out5%rdbpo
      !
      outpts(imod)%out5%o5ini2 = .true.
      outpts(imod)%out5%isbpo = 0
      !
    case (3)
      !
      allocate ( outpts(imod)%out5%abpi0(nspec,0:nbi2),       &
           outpts(imod)%out5%abpin(nspec,0:nbi2),             &
           outpts(imod)%out5%bbpi0(nspec,0:nbi),              &
           outpts(imod)%out5%bbpin(nspec,0:nbi), stat=istat   )
      check_alloc_status ( istat )
      !
      abpi0  => outpts(imod)%out5%abpi0
      abpin  => outpts(imod)%out5%abpin
      bbpi0  => outpts(imod)%out5%bbpi0
      bbpin  => outpts(imod)%out5%bbpin
      bbpi0 = -1.
      !
      outpts(imod)%out5%o5ini3 = .true.
      !
    case (4)
      !
      allocate ( outpts(imod)%out5%abpos(nspec,0:nbo2(nfbpo)), stat=istat )
      check_alloc_status ( istat )
      !
      abpos  => outpts(imod)%out5%abpos
      !
      outpts(imod)%out5%o5ini4 = .true.
      !
    case default
      write (ndse,1010)
      call extcde (10)
      !
    end select
    !
    !
    ! -------------------------------------------------------------------- /
    !
    return
    !
    ! formats
    !
1001 format (/' *** error w3dmo5 : grids not initialized *** '/      &
         '                    run w3nmod first '/)
1002 format (/' *** error w3dmo5 : illegal model number *** '/       &
         '                    imod   = ',i10/                        &
         '                    noutp  = ',i10/)
1010 format (/' *** error w3dmo5 : illegal block number  *** '/      &
         '                    iblock = ',i10/)
    !
    !/
    !/ end of w3dmo5 ----------------------------------------------------- /
    !/
  end subroutine w3dmo5
  !/ ------------------------------------------------------------------- /
  subroutine w3seto ( imod, ndserr, ndstst )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         25-sep-2020 |
    !/                  +-----------------------------------+
    !/
    !/    13-dec-2004 : origination.                        ( version 3.06 )
    !/    06-sep-2005 : second storage for input bound. sp. ( version 3.08 )
    !/    24-jul-2006 : adding unified point output storage.( version 3.10 )
    !/    25-jul-2006 : originating grid id for points.     ( version 3.10 )
    !/    04-oct-2006 : add filter to array pointers.       ( version 3.10 )
    !/    30-oct-2006 : add pars for partitioning.          ( version 3.10 )
    !/    26-mar-2007 : add pars for partitioning.          ( version 3.11 )
    !/    17-may-2007 : adding ntproc/naproc separation.    ( version 3.11 )
    !/    27-jul-2010 : add nki, nthi, xfri, fr1i, th1i.    ( version 3.14.3 )
    !/    19-dec-2012 : move noswll to data structure.      ( version 4.11 )
    !/    12-dec-2014 : modify instanciation of nrqtr       ( version 5.04 )
    !/    25-sep-2020 : flags for coupling restart          ( version 7.10 )
    !/
    !  1. purpose :
    !
    !     select one of the wavewatch iii grids / models.
    !
    !  2. method :
    !
    !     point pointers to the proper variables in the proper element of
    !     the grids array.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       imod    int.   i   model number to point to.
    !       ndserr  int.   i   error output unit number.
    !       ndstst  int.   i   test output unit number.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     see module documentation below.
    !
    !  5. called by :
    !
    !     many subroutines in the wavewatch system.
    !
    !  6. error messages :
    !
    !     checks on parameter list imod.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !  9. switches :
    !
    !     !/mpi  mpi specific calls.
    !     !/s    enable subroutine tracing.
    !     !/t    enable test output
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: nauxgr
    use w3servmd, only: extcde
    !
    !
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: imod, ndserr, ndstst
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: nlow
    integer                 :: j
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input and module status
    !
    if ( noutp .eq. -1 ) then
      write (ndserr,1001)
      call extcde (1)
    end if
    !
    nlow   = min ( 0 , -nauxgr )
    if ( imod.lt.nlow .or. imod.gt.noutp ) then
      write (ndserr,1002) imod, nlow, noutp
      call extcde (2)
    end if
    !
    !
    ! -------------------------------------------------------------------- /
    ! 2.  set model number
    !
    ioutp  = imod
    !
    ! -------------------------------------------------------------------- /
    ! 3.  set pointers in structure output
    !
    ndso   => outpts(imod)%ndso
    ndse   => outpts(imod)%ndse
    ndst   => outpts(imod)%ndst
    screen => outpts(imod)%screen
    !
    ntproc => outpts(imod)%ntproc
    naproc => outpts(imod)%naproc
    iaproc => outpts(imod)%iaproc
    naplog => outpts(imod)%naplog
    napout => outpts(imod)%napout
    naperr => outpts(imod)%naperr
    napfld => outpts(imod)%napfld
    nappnt => outpts(imod)%nappnt
    naptrk => outpts(imod)%naptrk
    naprst => outpts(imod)%naprst
    napbpt => outpts(imod)%napbpt
    napprt => outpts(imod)%napprt
    !
    noswll => outpts(imod)%noswll
    !
    tofrst => outpts(imod)%tofrst
    tonext => outpts(imod)%tonext
    tolast => outpts(imod)%tolast
    tbpi0  => outpts(imod)%tbpi0
    tbpin  => outpts(imod)%tbpin
    nds    => outpts(imod)%nds
    ofiles => outpts(imod)%ofiles
    !
    dtout  => outpts(imod)%dtout
    flout  => outpts(imod)%flout
    !
    ipass1 => outpts(imod)%out1%ipass1
    write1 => outpts(imod)%out1%write1
    nrqgo  => outpts(imod)%out1%nrqgo
    nrqgo2 => outpts(imod)%out1%nrqgo2
    if ( nrqgo  .ne. 0 ) irqgo  => outpts(imod)%out1%irqgo
    if ( nrqgo2 .ne. 0 ) irqgo2 => outpts(imod)%out1%irqgo2
    flogrd => outpts(imod)%out1%flogrd
    flogr2 => outpts(imod)%out1%flogr2
    flogrr => outpts(imod)%out1%flogrr
    flogd   => outpts(imod)%out1%flogd
    flog2   => outpts(imod)%out1%flog2
    flogr   => outpts(imod)%out1%flogr
    !
    ipass2 => outpts(imod)%out2%ipass2
    nopts  => outpts(imod)%out2%nopts
    nrqpo  => outpts(imod)%out2%nrqpo
    nrqpo2 => outpts(imod)%out2%nrqpo2
    o2init => outpts(imod)%out2%o2init
    o2irqi => outpts(imod)%out2%o2irqi
    !
    if ( o2init ) then
      iptint => outpts(imod)%out2%iptint
      il     => outpts(imod)%out2%il
      iw     => outpts(imod)%out2%iw
      ii     => outpts(imod)%out2%ii
      ptloc  => outpts(imod)%out2%ptloc
      ptifac => outpts(imod)%out2%ptifac
      dpo    => outpts(imod)%out2%dpo
      wao    => outpts(imod)%out2%wao
      zet_seto => outpts(imod)%out2%zet_seto
      wdo    => outpts(imod)%out2%wdo
      aso    => outpts(imod)%out2%aso
      cao    => outpts(imod)%out2%cao
      cdo    => outpts(imod)%out2%cdo
      iceo   => outpts(imod)%out2%iceo
      iceho  => outpts(imod)%out2%iceho
      icefo  => outpts(imod)%out2%icefo
      spco   => outpts(imod)%out2%spco
      ptnme  => outpts(imod)%out2%ptnme
      grdid  => outpts(imod)%out2%grdid
    end if
    !
    if ( o2irqi ) then
      irqpo1 => outpts(imod)%out2%irqpo1
      irqpo2 => outpts(imod)%out2%irqpo2
    end if
    !
    ipass3 => outpts(imod)%out3%ipass3
    it0pnt => outpts(imod)%out3%it0pnt
    it0trk => outpts(imod)%out3%it0trk
    it0prt => outpts(imod)%out3%it0prt
    nrqtr  => outpts(imod)%out3%nrqtr
    if ( nrqtr .ne. 0 ) irqtr  => outpts(imod)%out3%irqtr
    o3init => outpts(imod)%out3%o3init
    stop   => outpts(imod)%out3%stop
    !
    if ( o3init ) then
      mask1  => outpts(imod)%out3%mask1
      mask2  => outpts(imod)%out3%mask2
      trckid => outpts(imod)%out3%trckid
    end if
    !
    ifile4 => outpts(imod)%out4%ifile4
    nrqrs  => outpts(imod)%out4%nrqrs
    nblkrs => outpts(imod)%out4%nblkrs
    rsblks => outpts(imod)%out4%rsblks
    if ( nrqrs .ne. 0 ) then
      irqrs  => outpts(imod)%out4%irqrs
    end if
    irqrss => outpts(imod)%out4%irqrss
    vaaux  => outpts(imod)%out4%vaaux
    !
    nbi    => outpts(imod)%out5%nbi
    nbi2   => outpts(imod)%out5%nbi2
    nfbpo  => outpts(imod)%out5%nfbpo
    nrqbp  => outpts(imod)%out5%nrqbp
    nrqbp2 => outpts(imod)%out5%nrqbp2
    nbo    => outpts(imod)%out5%nbo
    nbo2   => outpts(imod)%out5%nbo2
    ndsl   => outpts(imod)%out5%ndsl
    nki    => outpts(imod)%out5%nki
    nthi   => outpts(imod)%out5%nthi
    xfri   => outpts(imod)%out5%xfri
    fr1i   => outpts(imod)%out5%fr1i
    th1i   => outpts(imod)%out5%th1i
    flbpi  => outpts(imod)%out5%flbpi
    flbpo  => outpts(imod)%out5%flbpo
    filer  => outpts(imod)%out5%filer
    filew  => outpts(imod)%out5%filew
    filed  => outpts(imod)%out5%filed
    spconv => outpts(imod)%out5%spconv
    o5ini1 => outpts(imod)%out5%o5ini1
    o5ini2 => outpts(imod)%out5%o5ini2
    o5ini3 => outpts(imod)%out5%o5ini3
    o5ini4 => outpts(imod)%out5%o5ini4
    !
    if ( o5ini1 ) then
      ipbpi  => outpts(imod)%out5%ipbpi
      isbpi  => outpts(imod)%out5%isbpi
      xbpi   => outpts(imod)%out5%xbpi
      ybpi   => outpts(imod)%out5%ybpi
      rdbpi  => outpts(imod)%out5%rdbpi
    end if
    !
    if ( o5ini2 ) then
      ipbpo  => outpts(imod)%out5%ipbpo
      isbpo  => outpts(imod)%out5%isbpo
      xbpo   => outpts(imod)%out5%xbpo
      ybpo   => outpts(imod)%out5%ybpo
      rdbpo  => outpts(imod)%out5%rdbpo
    end if
    !
    if ( o5ini3 ) then
      abpi0  => outpts(imod)%out5%abpi0
      abpin  => outpts(imod)%out5%abpin
      bbpi0  => outpts(imod)%out5%bbpi0
      bbpin  => outpts(imod)%out5%bbpin
    end if
    !
    if ( o5ini4 ) then
      abpos  => outpts(imod)%out5%abpos
    end if
    !
    if ( nrqbp  .ne. 0 ) irqbp1 => outpts(imod)%out5%irqbp1
    if ( nrqbp2 .ne. 0 ) irqbp2 => outpts(imod)%out5%irqbp2
    !
    ipass6 => outpts(imod)%out6%ipass6
    ihmax  => outpts(imod)%out6%ihmax
    hspmin => outpts(imod)%out6%hspmin
    wsmult => outpts(imod)%out6%wsmult
    wscut  => outpts(imod)%out6%wscut
    ix0    => outpts(imod)%out6%ix0
    ixn    => outpts(imod)%out6%ixn
    ixs    => outpts(imod)%out6%ixs
    iy0    => outpts(imod)%out6%iy0
    iyn    => outpts(imod)%out6%iyn
    iys    => outpts(imod)%out6%iys
    icprt  => outpts(imod)%out6%icprt
    dtprt  => outpts(imod)%out6%dtprt
    flcomb => outpts(imod)%out6%flcomb
    ptmeth => outpts(imod)%out6%ptmeth
    ptfcut => outpts(imod)%out6%ptfcut
    flform => outpts(imod)%out6%flform
    o6init => outpts(imod)%out6%o6init
    !
    return
    !
    ! formats
    !
1001 format (/' *** error w3seto : grids not initialized *** '/      &
         '                    run w3nmod first '/)
1002 format (/' *** error w3seto : illegal model number *** '/       &
         '                    imod   = ',i10/                        &
         '                    nlow   = ',i10/                        &
         '                    noutp  = ',i10/)
    !
    !/
    !/ end of w3seto ----------------------------------------------------- /
    !/
  end subroutine w3seto
  !/
  !/ end of module w3odatmd -------------------------------------------- /
  !/
end module w3odatmd
