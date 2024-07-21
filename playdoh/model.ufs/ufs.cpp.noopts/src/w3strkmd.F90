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
module w3strkmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |     a. j. van der westhuysen      |
  !/                  |            jeff hanson            |
  !/                  |        eve-marie devaliere        |
  !/                  |                        fortran 95 |
  !/                  | last update :         03-mar-2016 |
  !/                  +-----------------------------------+
  !/
  !/    03-feb-2012 : origination, based on matlab code   ( version 4.05 )
  !/                  by jeff hanson & eve-marie devaliere
  !/    04-jan-2013 : inclusion in trunk                  ( version 4.08 )
  !/    29-nov-2013 : remove doc control characters,
  !/                  update mpi! to mpi/! (h. l. tolman). ( version 4.15 )
  !/    26-sep-2016 : optimization updates (a. van der westhuysen)
  !/                                                      ( version 5.15 )
  !/    03-mar-2016 : optimization updates for intersect,
  !/                  union, unique, sort, setdiff, findij
  !/                  (s. zieger, bom australia)          ( version 5.16 )
  !/
  !/    copyright 2009-2013 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  use constants, only: file_endian
  implicit none
  !
  !  1. purpose :
  !
  !     module containing data structures and subroutines for spatial and
  !     temporal tracking (part of wave partitioning).
  !
  !  2. method :
  !
  !     read raw partitioning data.
  !     perform tracking in space.
  !     perform tracking in time.
  !
  !  3. variables and types :
  !
  !     note: in fortran 90/95 derived types cannot contain allocatable arrays.
  !           the same functionality is achieved here using pointers (pointing
  !           to unnamed allocatable arrays). can be replaced by allocatable arrays
  !           when transitioning to the fortran 2003 standard.
  !
  !     name    type       description
  !     ----------------------------------------------------------------
  !     param   der. type  structure of basic spectrally partitioned results at a geo point
  !     hs      real arr   array of sign. wave height partitions
  !     tp      real arr   array of peak period partitions
  !     dir     real arr   array of mean direction partitions
  !     dspr    real arr   array of mean directional spread (one-sided) of partitions
  !     wf      real arr   array of wind fraction
  !     ipart   int arr    array of partition indices
  !     sys     int arr    array of system indices to which a given partition has been assigned
  !     ngbrsys int arr    array of system indices of neighboring grid points
  !     checked    int      0 = geo point not checked yet (in subroutine findsys)
  !                         1 = geo point has been checked
  !                        -1 = geo land point (i.e. no partitioning data found for this point)
  !                        -2 = geo land point, second passing.
  !
  type param
    real    :: hs(10)
    real    :: tp(10)
    real    :: dir(10)
    real    :: dspr(10)
    !         real    :: wf(10)
    integer :: ipart(10)
    integer :: sys(10)
    integer :: ngbrsys(50)
    integer :: checked
  end type param
  !
  !     wind    der. type     structure containing wind-related parameters
  !     wdir       real       wind direction at grid point (nautical or cartesian, invariant)
  !     wspd       real       wind speed at grid point
  !
  type wind
    real    :: wdir
    real    :: wspd
  end type wind
  !
  !     dat2d   der. type     2d data structure for storing raw partitioned data
  !     lat     real arr      2d array of latitudes of input partitioned data
  !     lon     real arr      2d array of longitudes of input partitioned data
  !     par  type(param) arr  2d array of partitioned parameter structures
  !     wnd  type(wind) arr   2d array of wind parameter structures
  !     maxi    int           size of 2d array of raw partitioned data in i dimension
  !     maxj    int           size of 2d array of raw partitioned data in j dimension
  !
  type dat2d
    real*8               :: date
    real, pointer        :: lat(:,:)
    real, pointer        :: lon(:,:)
    type(param), pointer :: par(:,:)
    type(wind), pointer  :: wnd(:,:)
    integer              :: maxi
    integer              :: maxj
  end type dat2d
  !
  !     neighbr  der. type    structure for storing data of neighboring grid point
  !     par     type(param)   partitioned parameter structure at neighboring grid point
  !     i          int        i index of neighboring grid point
  !     j          int        j index of neighboring grid point
  !
  type neighbr
    type(param) :: par
    integer     :: i
    integer     :: j
  end type neighbr
  !
  !     mtchsys  der. type    structure for storing data of matched systems
  !     sysval   int arr      array of indices of matched systems
  !     tpval    real arr     array of peak period values of matched systems
  !     wfval    real arr     array of wind fraction values of matched systems
  !
  type mtchsys
    integer     :: sysval(50)
    real        :: tpval(50)
    real        :: dirval(50)
    real        :: hsval(50)
    !         real        :: wfval(50)
  end type mtchsys
  !
  !     system   der. type    structure for storing spatially tracked systems (one time level)
  !     hs       real arr     sign wave height field assoc with wave system (in 1d array)
  !     tp       real arr     peak period field assoc with wave system (in 1d array)
  !     dir      real arr     mean direction field assoc with wave system (in 1d array)
  !     dspr     real arr     mean directional spread field assoc with wave system (in 1d array)
  !     wf       real arr     wind fraction assoc with wave system (in 1d array)
  !     i        int arr      i index of geo grid point in wave system (in 1d array)
  !     j        int arr      j index of geo grid point in wave system (in 1d array)
  !     lat      real arr     latitudes of grid point in wave system (in 1d array)
  !     lon      real arr     longitudes of grid point in wave system (in 1d array)
  !     sysind   int          index of current wave system
  !     hsmean   real         spatial mean sign wave height of current wave system
  !     tpmean   real         spatial mean peak period of current wave system
  !     dirmean  real         spatial mean wave direction of current wave system
  !     wfmean   real         spatial mean wind fraction of current wave system
  !     npoints  int          total number of grid points in current wave system
  !     ngbr     int arr      indices of neighboring wave systems
  !     grp      int          time-tracked group that system is assigned to
  !
  type system
    real, pointer    :: hs(:)
    real, pointer    :: tp(:)
    real, pointer    :: dir(:)
    real, pointer    :: dspr(:)
    !         real, pointer    :: wf(:)
    integer, pointer :: i(:)
    integer, pointer :: j(:)
    integer, pointer :: indx(:)
    real, pointer    :: lat(:)
    real, pointer    :: lon(:)
    integer          :: sysind
    real             :: hsmean
    real             :: tpmean
    real             :: dirmean
    !         real             :: wfmean
    integer          :: npoints
    integer          :: ngbr(1000)
    integer          :: grp
  end type system
  !
  !     timsys   der. type     structure for storing time-tracked systems (all time levels)
  !     sys  type(system) arr  array of all spatially+temporally tracked systems at given
  !                            time level
  !
  type timsys
    type(system), pointer :: sys(:)
  end type timsys
  !
  !     sysmemory  der. type   structure to store key characteristics of systems over multiple
  !                            time levels. used during the time tracking routine.
  !
  type sysmemory
    integer :: grp
    integer :: npoints
    integer, pointer :: indx(:)
    integer :: updated
    integer :: length
    real    :: lonmean
    real    :: latmean
    real    :: tpmean
    real    :: dirmean
  end type sysmemory
  !
  !  4. subroutines and functions used :
  !
  !     a. main subroutines for spatial/temporal tracking:
  !
  !     wavetracking_nws_v2  main subroutine of spatial and temporal tracking algorithm
  !     spiraltrackv3        performs the spatial spiral tracking for a given time step
  !     timetrackingv2       performs the time tracking of all wave systems
  !
  !     b. auxiliary subroutines and functions for tracking:
  !
  !     findway              find direction and no. steps in spatial search spiral
  !     findnext             find next point on spatial search spiral
  !     findsys              find all neighboring wave systems for given grid point
  !     combinewavesystems   combine wave systems, then remove small and low-energy systems
  !     printfinalsys        output the final output systems for this time step
  !     combinesys           combine wave systems
  !     combinepartitionsv2  combine two partitions that have been assigned to the same system
  !     func. mean_anglev2   compute the mean direction from array of directions
  !     findijv4             find indices of system "a" that lie over or next to system "b"
  !
  !     c. simple data manipulation (based on matlab intrinsic functions):
  !
  !     unique               removes duplicate reals from an vector
  !     sort                 sorts the vector in ascending or descending order
  !     setdiff              returns elements in vector1 that are not in vector2
  !     intersect            returns elements that are mutual in vector1 and vector2
  !     union                returns the union of vector1 and vector2
  !     func. length         finds no. of indices in vector not filled with blank entries.
  !     func. findfirst      returns index of first instance of a search value in vector
  !     func. std            computes standard deviation
  !
  !  5. called by :
  !
  !     ww3_systrk (main program)
  !
  !  6. error messages :
  !
  !  7. remarks :
  !
  !  8. structure :
  !
  !     the structure of the tracking algorithm is the following
  !     (parentheses indicate minor subroutines and functions):
  !
  !     +---- subroutine wavetracking_nws_v2       main subroutine of spatial and temporal tracking algorithm
  !     |        (call unique)                     removes duplicate reals from an vector
  !     |        call spiraltrackv3                see below
  !     |        call timetrackingv2               see below
  !     |
  !     +---+--- subroutine spiraltrackv3          performs the spatial spiral tracking for a given time step
  !     |   |       call findway                   find direction and no. steps in spatial search spiral
  !     |   |       call findnext                  find next point on spatial search spiral
  !     |   |       call findsys                   see below
  !     |   |       call combinewavesystems        see below
  !     |   |
  !     |   +------ subroutine findsys             find all neighboring wave systems for given grid point
  !     |   |          (call unique)
  !     |   |          call combinepartitionsv2    combine two partitions that have been assigned to the same system
  !     |   |
  !     |   +---+-- subroutine combinewavesystems  combine wave systems, then remove small and low-energy systems
  !     |       |      call printfinalsys          see below
  !     |       |      call combinesys             see below
  !     |       |
  !     |       +----- subroutine printfinalsys    output the final output systems for this time step
  !     |       |         (call unique)
  !     |       |         (call setdiff)           returns elements in vector1 that are not in vector2
  !     |       |         (call sort)              sorts the vector in ascending or descending order
  !     |       |
  !     |       +----- subroutine combinesys       combine wave systems
  !     |                 (call sort)
  !     |                 (call unique)
  !     |                 (call union)             returns the union of vector1 and vector2
  !     |                 (call setdiff)
  !     |                 call findijv4            find indices of system "a" that lie over or next to system "b"
  !     |                 call combinepartitionsv2
  !     |
  !     +------- subroutine timetrackingv2         performs the time tracking of all wave systems
  !                 (call sort)
  !                 (call setdiff)
  !
  !  9. switches :
  !
  !       !/shrd  switch for shared / distributed memory architecture.
  !       !/mpi   id.
  !
  ! 10. source code :
  !
  !/ ------------------------------------------------------------------- /
  !/
contains
  !/ ------------------------------------------------------------------- /
  subroutine wavetracking_nws_v2 (intype     ,tmax       , &
       tcur       ,filename   , &
       tstart     ,tend       , &
       dt         ,ntint      , &
       minlon     ,maxlon     , &
       minlat     ,maxlat     , &
       mxcwt      ,mycwt      , &
       dirknob    ,             &
       perknob    ,hsknob     , &
       wetpts     ,seedlat    , &
       seedlon    ,dirtimeknob, &
       tptimeknob ,paramfile  , &
       sysa       ,wsdat      , &
       maxsys     ,maxgroup   )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |     a. j. van der westhuysen      |
    !/                  |            jeff hanson            |
    !/                  |        eve-marie devaliere        |
    !/                  |                        fortran 95 |
    !/                  | last update :          4-jan-2013 |
    !/                  +-----------------------------------+
    !/
    !/    03-feb-2012 : origination, based on matlab code   ( version 4.05 )
    !/                  by jeff hanson & eve-marie devaliere
    !/    04-jan-2013 : inclusion in trunk                  ( version 4.08 )
    !/
    !/    copyright 2009-2013 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    implicit none
    include "mpif.h"
    !
    !  1. purpose :
    !
    !     main subroutine of spatial and temporal tracking algorithm
    !
    !  2. method
    !
    !     (1) read the raw partitioning output from one of two file formats:
    !         (a) "partres" format of ifp-swan (intype=1), or
    !         (b) ww3 spectral bulletin format (intype=2).
    !         if intype=0, the partition data is read from memory (not activated yet).
    !
    !     (2) perform tracking in space by calling subroutine spiraltrackv3
    !     (3) perform tracking in time by calling subroutine timetrackingv2
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     intype         int       input  for coupling: type of input (0 = memory; 1 = partres file; 2 = ww3 part file)
    !     tmax           int       input  for coupling: value of maxts to apply (1 or 2)
    !     tcur           int       input  for coupling: index of current time step (1 or 2)
    !     filename       char      input  file name of locally partitioned data output
    !     tstart         char      input  start time in raw partition file (if used)
    !     tend           char      input  end time in raw partition file (if used)
    !     minlon         real      input  lower lon boundary of domain to be processed
    !     maxlon         real      input  upper lon boundary of domain to be processed
    !     minlat         real      input  lower lat boundary of domain to be processed
    !     maxlat         real      input  upper lat boundary of domain to be processed
    !     dirknob        real      input  parameter in direction for combining fields in space
    !     perknob        real      input  parameter in period for combining fields in space
    !     hsknob         real      input  parameter in wave height for purging fields
    !     wetpts         real      input  percentage of wet points for purging fields (fraction)
    !     seedlat        real      input  start lat for tracking spiral (if =0 centre of field is used)
    !     seedlon        real      input  start lon for tracking spiral (if =0 centre of field is used)
    !     dirtimeknob    real      input  parameter in direction for combining fields in time
    !     tptimeknob     real      input  parameter in period for combining fields in time
    !     paramfile      char      input  file name of partitioning parameters                           is this used???
    !     sys         type(timsys) output final set of spatially and temporally tracked systems
    !     wsdat       type(dat2d)  output final version of 2d (gridded) partition data
    !     maxgroup       int       output maximum number of wave systems ("groups") tracked in time
    !
    character    :: filename*50, paramfile*32
    real         :: dirknob, perknob, hsknob, wetpts, seedlat, &
         seedlon, dirtimeknob, tptimeknob
    real*8       :: tstart, tend
    integer      :: maxgroup, intype, tmax, tcur, ntint
    integer, pointer :: maxsys(:)
    type(dat2d), pointer :: wsdat(:)
    type(timsys), pointer :: sysa(:), sysaa(:)
    integer      :: numconssys, iconssys
    real         :: dt
    real         :: minlon, maxlon, minlat, maxlat
    integer      :: mxcwt, mycwt
    !     note: variables wsdat, sysa and maxsys have in/out intent so that they
    !     can be manipulated outside of this subroutine, e.g. re-indexing of
    !     systems and groups during the simulation.
    intent (in)  intype, tmax, tcur, filename, paramfile, &
         minlon, maxlon, minlat, maxlat, &
         hsknob, wetpts, seedlat, seedlon, &
         dirknob, perknob, dirtimeknob, tptimeknob
    intent (out) maxgroup
    !      intent (in out) wsdat, sysa, maxsys
    !
    !     local variables
    !     ----------------------------------------------------------------
    !     llat     real    latitude of partition point, from input file
    !     llon     real    longitude of partition point, from input file
    !     ts       real    time step of partition, from input file
    !     hs0      real    wave height of partition, from input file
    !     tp0      real    peak period of partition, from input file
    !     dir0     real    mean direction of partition, from input file
    !     dspr0    real    mean directional spread of partition, from input file
    !     wf0      real    wind fraction of partition, from input file (removed)
    !     wndspd0  real    wind speed of partition, from input file
    !     wnddir0  real    wind direction of partition, from input file
    !     wndfce0  real    wind force of partition, from input file (not, used; removed)
    !     tss      int.    time step counter
    !     t0       int     index of first time step to compute
    !
    logical                :: file_exists, flform, loop
    logical                :: testout
    parameter (testout = .false.)
    character              :: dummy*10, dummyc*12
    character(len=10)      :: verprt
    character(len=35)      :: idstr
    character(len=78)      :: headln1
    character(len=51)      :: headln2
    integer                :: line
    integer, allocatable   :: ts(:), tmp_i4(:)
    real, allocatable      :: llat(:),llon(:),hs0(:), &
         tp0(:),dir0(:),dspr0(:),&
         wndspd0(:),wnddir0(:)
    real*8, allocatable :: date0(:),tmp_r8(:)
    integer    :: maxts, t0, nout1, nout2, maxi, maxj
    real, allocatable :: mlon(:,:), mlat(:,:), tmp_r4(:)
    real, pointer :: uniquetim(:),uniquelatraw(:),uniquelonraw(:), &
         uniquelat(:),uniquelon(:)
    integer    :: ioerr,ierr, i, j, k, l, alreadyin, ok, tss, tsa
    integer    :: maxpart, datetime(2)
    integer    :: tstep, iline, numpart, skipln, readln, filesize
    real       :: x,y,wnd,wnddir
    real       :: invar1, invar2, invar3, invar4
    real       :: invar5, invar6, invar7
    real, allocatable :: phs(:),ptp(:),pdir(:),pspr(:),pwf(:) ! current partition values
    real*8     :: date1, date2, ttest, ttemp
    integer    :: ic, leng, maxpartout                                  ! remove?
    real       :: dx
    integer    :: latind1, latind2, lonind1, lonind2
    real       :: lonext, latext
    integer    :: rank, irank, nproc, extent, domsize, tag1, tag2
    !      integer    :: mpi_int_domarr, mpi_real_domarr
    integer    :: mpi_status(mpi_status_size)
    integer    :: req(16)
    !    integer    :: istat(mpi_status_size,16)
    real       :: commarr1(44)
    integer    :: commarr2(11)
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      unique
    !      spiraltrackv3
    !      timetrackingv2
    !
    !  5. subroutines calling
    !
    !      ww3_systrk (main program)
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see above
    !
    !  9. switches :
    !
    !     none defined yet.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    call mpi_comm_rank(mpi_comm_world, rank, ierr)
    call mpi_comm_size(mpi_comm_world, nproc, ierr)
    nullify( sysa )
    nullify( maxsys )
    !     select input type for raw partitioning data
    if ((intype.eq.1).or.(intype.eq.2)) then
      !        raw partitioning data is coming from an input file.
      !        read file here, and set up 2d array wsdat with the data.
      t0 = 1
      if (intype.eq.1) then
        if (rank.eq.0) then
          !           read partres format file
          write(20,*) 'reading partres partitioning file...'
          filesize = 7500000
          allocate(ts(filesize))
          allocate(llat(filesize))
          allocate(llon(filesize))
          allocate(hs0(filesize))
          allocate(tp0(filesize))
          allocate(dir0(filesize))
          allocate(dspr0(filesize))
          !            allocate(wf0(filesize))
          allocate(wndspd0(filesize))
          allocate(wnddir0(filesize))
          allocate(date0(filesize))
          write(20,*) '*** max number of lines read from "partres" ', &
               'input file is = ',filesize,'!'
          write(6,*) 'reading partres file...'
          inquire(file=filename, exist=file_exists)
          if (.not.file_exists) then
            write(20,2001)
            write(6,2001)
            stop 1
          end if
          open(unit=11,file=filename,status='old')
          line = 1
          do while (.true.)
            read (11, *, end=113) dummyc,llat(line),llon(line),   &
                 ts(line),hs0(line),tp0(line),dir0(line), &
                 wndspd0(line),wnddir0(line),invar7
            !partres file does not contain the dspr variable
            dspr0(line) = 9999.
            !               wf0(line) = 9999.
            line = line+1
          enddo
113       ierr = -1
          close(11)
          line = line-1
          write(6,*) '... finished'
          !            deallocate(date0)
        end if
      else if (intype.eq.2) then
        if (rank.eq.0) then
          !           read ww3 spectral partition format file
          !           query input file to determine required array sizes
          inquire(file=filename, exist=file_exists)
          if (.not.file_exists) then
            write(20,2001)
            write(6,2001)
            stop 1
          end if
          !/       -------------------------------------------------
          !/          test unformatted read
          !/       -------------------------------------------------
          open(unit=11,file=filename,form='unformatted', convert=file_endian,status='old',access='stream')
          read(11,err=802,iostat=ioerr) i
          close(11)
          !/          --- first four-byte integer could possibly be byte-swapped,
          !               if ww3_shel was compiled on a different architecture. ---
          k = swapi4(i)
          flform = .not.(i.eq.(len(idstr)+len(verprt)).or.&
               k.eq.(len(idstr)+len(verprt))    )
          !        ======== count loop ===========
          if (flform) then
            !              input file in formatted ascii
            write(6,*) 'reading formatted ascii file...'
            open(unit=11,file=filename,status='old')
            read(11,'(78a)') headln1
            idstr = headln1(1:len(idstr))
            read(11,'(78a)') headln1
            read(11,'(51a)') headln2
          else
            if (k.eq.(len(idstr)+len(verprt))) then
              !/           --- stop here. the file appears to be endian encoded
              !                different from the native machine format. and, the
              !                compiler option will override support for fortran
              !                convert statements convert=file_endian or
              !                convert='little_endian'. ---
              write(20,1200)
              write(6,1200)
              stop 1
            else
              !                input file in unformatted binary
              write(6,*) 'reading binary formatted file...'
              open(unit=11,file=filename,form='unformatted', convert=file_endian, &
                   status='old')
            endif
            rewind(11)
            read(11,err=802,iostat=ioerr) idstr,verprt
            read(11,err=802,iostat=ioerr) headln1
            read(11,err=802,iostat=ioerr) headln2
          end if
          !/
          if (idstr(1:9).ne.'wavewatch') then
            close(11)
            write(20,1300)
            write(6,1300)
            stop 1
          endif
          !/       -------------------------------------------------
          !/          skip to start time
          !/       -------------------------------------------------
          skipln = 3
          ttest = 0
          do while (ttest.lt.tstart)
            if (flform) then
              read (11,1000,err=802,end=112) date1,date2,x,y, &
                   numpart,wnd,wnddir,invar6,invar7
              skipln = skipln+1
            else
              read (11,err=802,iostat=ioerr) datetime,x,y, &
                   dummy,numpart,invar1,wnd,wnddir, &
                   invar5,invar6
              ! write(*,*) '0:',datetime,numpart
              date1=dble(datetime(1))
              date2=dble(datetime(2))
            end if
            ttest = date1 + date2*1.0e-6
            if (flform) then
              do line = 1,numpart+1
                read(11,1010,end=111,err=802,iostat=ioerr) &
                     invar1,invar2,invar3,invar4
                ! write(*,*) '0+:',line,numpart+1,invar1,invar2,invar3,invar4
                skipln = skipln+1
              end do
            else
              do line = 1,numpart+1
                read (11,err=802,iostat=ioerr) iline,invar1, &
                     invar2,invar3,invar4,invar5,invar6
                ! write(*,*) '0+:',line,iline,invar1,invar2,invar3,invar4,invar5,invar6
              end do
            end if
          end do
          skipln = skipln-numpart-1-1
          !/       -------------------------------------------------
          !           read file for ntint time levels
          !/       -------------------------------------------------
          readln = numpart
          tstep = 1
          ttemp = tstart
          maxpart = numpart
          do while (tstep.le.ntint)
            if (readln.gt.0) then
              if (flform) then
                read (11,1000,err=802,end=111) date1,date2,x,y, &
                     numpart,wnd,wnddir,invar6,invar7
              else
                read (11,end=111,err=802,iostat=ioerr) datetime,  &
                     x,y,dummy,numpart,wnd,wnddir,invar5,invar6,invar7
                ! write(*,*) '1:',numpart,x,y
                date1=dble(datetime(1))
                date2=dble(datetime(2))
              end if
              maxpart = max(maxpart,numpart)
            end if
            ttest = date1 + date2*1.e-6
            if (ttest.gt.ttemp) then
              tstep = tstep+1
              ttemp = ttest
              if (tstep.gt.ntint) exit
            end if
            if (flform) then
              do line = 1,numpart+1
                read (11,1010,end=111,err=802,iostat=ioerr)      &
                     invar1,invar2,invar3,invar4
                readln = readln+1
              end do
            else
              do line = 1,numpart+1
                read (11,end=111,err=802,iostat=ioerr) iline,invar1,&
                     invar2,invar3,invar4,invar5,invar6
                readln = readln+1
              end do
            end if
          enddo
111       continue
          close(11)
          !        ===== end count loop =====
          !        ===== start read loop =====
          allocate(ts(readln))
          allocate(llat(readln))
          allocate(llon(readln))
          allocate(hs0(readln))
          allocate(tp0(readln))
          allocate(dir0(readln))
          allocate(dspr0(readln))
          !            allocate(wf0(readln))
          allocate(wndspd0(readln))
          allocate(wnddir0(readln))
          allocate(date0(readln))
          ts(1:readln)   = -1
          llat(1:readln) = 9999.
          llon(1:readln) = 9999.
          hs0(1:readln)  = 9999.
          tp0(1:readln)  = 9999.
          dir0(1:readln)  = 9999.
          dspr0(1:readln)  = 9999.
          if (flform) then
            open(unit=11,file=filename,status='old')
          else
            open(unit=11,file=filename,status='old', &
                 form='unformatted', convert=file_endian)
          end if
          line = 1
          tstep = 1
          !/       -------------------------------------------------
          !/          skip to start time
          !/       -------------------------------------------------
          if (flform) then
            do i = 1,skipln
              read (11, *)
            end do
          else
            ! --- repeat from above since access='direct'
            !     does not support fseek and ftell. ---
            read(11,end=112,err=802,iostat=ioerr) idstr,verprt
            read(11,end=112,err=802,iostat=ioerr) headln1
            read(11,end=112,err=802,iostat=ioerr) headln2
            !/ --- allocate buffer for all partition parameters
            !/     for a single grid point  ---
            if (.not.allocated(phs)) allocate(phs(maxpart))
            if (.not.allocated(ptp)) allocate(ptp(maxpart))
            if (.not.allocated(pdir)) allocate(pdir(maxpart))
            if (.not.allocated(pspr)) allocate(pspr(maxpart))
            if (.not.allocated(pwf)) allocate(pwf(maxpart))
            ttest = 0
            do while (ttest.lt.tstart)
              read (11,end=112,err=802,iostat=ioerr) datetime, &
                   invar1,invar2,dummy,numpart,invar3,   &
                   invar4,invar5,invar6,invar7
              date1=dble(datetime(1))
              date2=dble(datetime(2))
              ttest = date1 + date2*1.0e-6
              !/ --- reset buffer ---
              phs(:) = 0.
              ptp(:) = 0.
              pdir(:) = 0.
              pspr(:) = 0.
              pwf(:) = 0.
              !/ --- fill buffer with partition data ---
              read (11,end=112,err=802,iostat=ioerr) iline,invar1, &
                   invar2,invar3,invar4,invar5,invar6
              do i = 1,numpart
                read (11,end=112,err=802,iostat=ioerr) iline,      &
                     phs(i),ptp(i),invar3,pdir(i),pspr(i),pwf(i)
              end do
            end do
            !/ --- move buffer content to data array ---
            do i=1,numpart
              hs0(line)   = phs(i)
              tp0(line)   = ptp(i)
              dir0(line)  = pdir(i)
              dspr0(line) = pspr(i)
              date0(line) = date1 + date2*1.0e-6
              ts(line) = tstep
              llat(line) = x
              llon(line) = y
              wndspd0(line) = wnd
              wnddir0(line) = wnddir
              line = line + 1
            end do
          end if
          !/       -------------------------------------------------
          !           read file for ntint time levels
          !/       -------------------------------------------------
          ttemp = tstart
          do while (line.le.readln)
            if (flform) then
              read (11,1000,end=112) date1,date2,x,y,numpart, &
                   wnd,wnddir,invar6,invar7
            else
              read (11,err=802,iostat=ioerr) datetime,x,y,    &
                   dummy,numpart,wnd,wnddir,invar5,invar6,invar7
              date1=dble(datetime(1))
              date2=dble(datetime(2))
            end if
            ttest = date1 + date2*1.0e-6
            if (ttest.gt.ttemp) then
              tstep = tstep+1
              ttemp = ttest
              if (tstep.gt.ntint) exit
            end if
            if (flform) then
              read (11,1010,end=112) invar1,invar2,invar3,invar4 ! skip total integral parameters
              do i = 1,numpart
                if (line.le.readln) then
                  read (11,1010,end=112) hs0(line),tp0(line), &
                       dir0(line),dspr0(line)
                  date0(line) = ttest
                  ts(line) = tstep
                  llat(line) = x
                  llon(line) = y
                  wndspd0(line) = wnd
                  wnddir0(line) = wnddir
                  line = line+1
                end if
              end do
            else
              read (11,err=802,iostat=ioerr) k,invar1,invar2,  &
                   invar3,invar4,invar5
              do i = 1,numpart
                if (line.le.readln) then
                  read (11,end=112,err=802,iostat=ioerr) k,         &
                       hs0(line),tp0(line),invar3,dir0(line),       &
                       dspr0(line)
                  date0(line) = ttest
                  ts(line) = tstep
                  llat(line) = x
                  llon(line) = y
                  wndspd0(line) = wnd
                  wnddir0(line) = wnddir
                  line = line+1
                end if
              end do
            end if
          end do
110       ierr = -1
          close(11)
112       continue
          if (line.eq.1) then
            write(20,2002)
            write(6,2002)
            stop 1
          end if
          close(11)
          ! ===== read loop finished =====
          line=line-1
          write(6,*) '... finished'
          if (ttest.lt.tstart) then
            write(20,2003) tstart
            write(6,2003) tstart
            stop 1
          end if
          if (allocated(phs)) deallocate(phs)
          if (allocated(ptp)) deallocate(ptp)
          if (allocated(pdir)) deallocate(pdir)
          if (allocated(pspr)) deallocate(pspr)
          if (allocated(pwf)) deallocate(pwf)
        end if
      end if
      if (rank.eq.0) then
        !        find unique time steps (and sort in ascending order)
        call unique(real(ts(1:line)),line,uniquetim,maxts)
        !        find unique lat and lon values (and sort in ascending order)
        call unique(llat(1:line),size(llat(1:line)),uniquelatraw,nout1)
        call unique(llon(1:line),size(llon(1:line)),uniquelonraw,nout2)
        !--042916-----------------------
        !
        !        redefine uniquelatraw and uniquelonrawto based on domain extent
        write(20,*) 'uniquelatraw(:) =', uniquelatraw(:)
        write(20,*) 'uniquelonraw(:) =', uniquelonraw(:)
        write(20,*) 'no. increments: longitude, latitue =', mxcwt, mycwt
        deallocate(uniquelatraw)
        deallocate(uniquelonraw)
        allocate(uniquelatraw(mycwt+1))
        allocate(uniquelonraw(mxcwt+1))
        do i = 1,(mycwt+1)
          uniquelatraw(i) =  minlat + &
               (real(i)-1)/real(mycwt)*(maxlat-minlat)
        end do
        do i = 1,(mxcwt+1)
          uniquelonraw(i) =  minlon + &
               (real(i)-1)/real(mxcwt)*(maxlon-minlon)
        end do
        write(20,*) 'uniquelatraw(:) =', uniquelatraw(:)
        write(20,*) 'uniquelonraw(:) =', uniquelonraw(:)
        !
        !--042916-----------------------
        !        filter out lats and lons outside of domain of interest
        do latind1 = 1,size(uniquelatraw)
          if (uniquelatraw(latind1).ge.minlat) exit
        end do
        do latind2 = size(uniquelatraw),1,-1
          if (uniquelatraw(latind2).le.maxlat) exit
        end do
        do lonind1 = 1,size(uniquelonraw)
          if (uniquelonraw(lonind1).ge.minlon) exit
        end do
        do lonind2 = size(uniquelonraw),1,-1
          if (uniquelonraw(lonind2).le.maxlon) exit
        end do
        write(20,*) 'latind1, latind2, lonind1, lonind2 =', &
             latind1, latind2, lonind1, lonind2
        if ((latind1.ge.latind2).or.(lonind1.ge.lonind2)) then
          write(20,1400)
          write(6,1400)
          stop 1
        end if
        nullify(uniquelat)
        nullify(uniquelon)
        allocate(uniquelat(latind2-latind1+1))
        allocate(uniquelon(lonind2-lonind1+1))
        uniquelat = uniquelatraw(latind1:latind2)
        uniquelon = uniquelonraw(lonind1:lonind2)
        write(20,*) 'in wavetracking_nws_v2: longitude range =', &
             uniquelon(1), uniquelon(size(uniquelon))
        write(20,*) '                        latitude range  =', &
             uniquelat(1), uniquelat(size(uniquelat))
        !        map is transposed (rotated by 90 deg), so that:
        !          i (matrix row) represents longitute
        !          j (matrix column) represents latitude
        !          i.e. from this point onwards the indices (i,j) represents cart. coordinates
        allocate( mlon(size(uniquelon),size(uniquelat)) )
        allocate( mlat(size(uniquelon),size(uniquelat)) )
        !
        maxi = size(uniquelon)
        maxj = size(uniquelat)
        do i = 1,maxi
          do j = 1,maxj
            mlon(i,j) = uniquelon(i)
            mlat(i,j) = uniquelat(j)
          end do
        end do
      end if
      call mpi_bcast(maxi,1,mpi_integer,0,mpi_comm_world,ierr)
      call mpi_bcast(maxj,1,mpi_integer,0,mpi_comm_world,ierr)
      call mpi_bcast(maxts,1,mpi_integer,0,mpi_comm_world,ierr)
      !        allocate the wsdat structure
      if (rank.eq.0) then
        write(20,*) 'allocating wsdat...'
      end if
      nullify(wsdat)
      allocate(wsdat(maxts))
      if (rank.eq.0) then
        write(20,*) 'size(wsdat) = ',size(wsdat)
      end if
      !        allocate and initialize the wsdat array
      if (rank.eq.0) then
        do tsa = 1,maxts
          allocate(wsdat(tsa)%lat(maxi,maxj))
          allocate(wsdat(tsa)%lon(maxi,maxj))
          allocate(wsdat(tsa)%par(maxi,maxj))
          allocate(wsdat(tsa)%wnd(maxi,maxj))
          do j = 1,maxj
            do i = 1,maxi
              wsdat(tsa)%lat(i,j)=mlat(i,j)
              wsdat(tsa)%lon(i,j)=mlon(i,j)
              wsdat(tsa)%maxi=maxi
              wsdat(tsa)%maxj=maxj
              wsdat(tsa)%par(i,j)%hs(1:10)=9999.
              wsdat(tsa)%par(i,j)%tp(1:10)=9999.
              wsdat(tsa)%par(i,j)%dir(1:10)=9999.
              wsdat(tsa)%par(i,j)%dspr(1:10)=9999.
              !                  wsdat(tsa)%par(i,j)%wf(1:10)=9999.
              wsdat(tsa)%par(i,j)%ipart(1:10)=0
              wsdat(tsa)%par(i,j)%sys(1:10)=9999                      ! 40.par increase this array, or make allocatable
              wsdat(tsa)%par(i,j)%ngbrsys(1:50)=9999
              wsdat(tsa)%wnd(i,j)%wdir=9999.
              wsdat(tsa)%wnd(i,j)%wspd=9999.
              wsdat(tsa)%par(i,j)%checked=-1
            end do
          end do
        end do
        !        assign to each line in partition file an entry in wsdat
        !        at each time step each point contains all numpart partitions.
        !        only store the first 10 partitions.
        l = 1
        do while (l.le.line)
          do j = 1,maxj
            do i = 1,maxi
              !>042916                  if ( (llat(l).eq.mlat(i,j)).and. &
              !>042916                       (llon(l).eq.mlon(i,j)) ) then
              if ( (abs(llat(l)-mlat(i,j)).lt.1.e-2).and. &
                   (abs(llon(l)-mlon(i,j)).lt.1.e-2) ) then
                !                    write(20,*) 'matched! ',l,&
                !                       llat(l),mlat(i,j),abs(llat(l)-mlat(i,j)),&
                !                       llon(l),mlon(i,j),abs(llon(l)-mlon(i,j))
                wsdat(ts(l))%lat(i,j) = llat(l)
                wsdat(ts(l))%lon(i,j) = llon(l)
                !                   --- find all partition values associated with
                !                       lat(i,j) and lon(i,j). keep list index l
                !                       fixed and recycle iline as variable index. ---
                iline = l
                k = 1
                do while (                                         &
                     abs(wsdat(ts(l))%lat(i,j)-llat(iline)).lt.1.e-3 &
                     .and.abs(wsdat(ts(l))%lon(i,j)-llon(iline)).lt.1.e-3 )
                  if (k.le.10) then
                    wsdat(ts(iline))%par(i,j)%ipart(k) = k
                    wsdat(ts(iline))%par(i,j)%hs(k)    = hs0(iline)
                    wsdat(ts(iline))%par(i,j)%tp(k)    = tp0(iline)
                    wsdat(ts(iline))%par(i,j)%dir(k)   = dir0(iline)
                    wsdat(ts(iline))%par(i,j)%dspr(k)  = dspr0(iline)
                    !                       wsdat(ts(k))%par(i,j)%wf(k) = wf0(l)
                    if (k.eq.1) then
                      wsdat(ts(iline))%date = date0(iline)
                      wsdat(ts(iline))%wnd(i,j)%wdir = wnddir0(iline)
                      wsdat(ts(iline))%wnd(i,j)%wspd = wndspd0(iline)
                      wsdat(ts(iline))%par(i,j)%checked = 0
                    end if
                  end if
                  k = k + 1
                  iline = iline + 1
                  if (iline.gt.line) exit
                end do
                !                   --- account for increment at the end of loop (400 continue)
                !                       and go one element back in list because of increment. ---
                l = iline-1
                goto 400
              end if
            end do
          end do
400       continue
          if (l+1.le.line) then
            if (ts(l).lt.ts(l+1)) then
              k = line-l
              !             --- with each time step completed, deallocate processed 1:l
              !                 elements from 1d array. create a temporary array size of
              !                 (l+1:line) with k elements and reallocate original array. ---
              if (allocated(tmp_i4)) deallocate(tmp_i4)
              !             --- reallocate(integer arrays) ---
              allocate(tmp_i4(k))
              tmp_i4(1:k) = ts((l+1):line)
              deallocate(ts)
              allocate(ts(k))
              ts(1:k) = tmp_i4(1:k)
              deallocate(tmp_i4)
              !             --- reallocate(double precision arrays) ---
              if (allocated(tmp_r8)) deallocate(tmp_r8)
              allocate(tmp_r8(k))
              tmp_r8(1:k) = date0((l+1):line)
              deallocate(date0)
              allocate(date0(k))
              date0(1:k) = tmp_r8(1:k)
              deallocate(tmp_r8)
              !             --- reallocate(single precision arrays) ---
              if (allocated(tmp_r4)) deallocate(tmp_r4)
              allocate(tmp_r4(k))
              tmp_r4(1:k) = llat((l+1):line)
              deallocate(llat)
              allocate(llat(k))
              llat(1:k) = tmp_r4(1:k)
              tmp_r4(1:k) = llon((l+1):line)
              deallocate(llon)
              allocate(llon(k))
              llon(1:k) = tmp_r4(1:k)
              tmp_r4(1:k) = hs0((l+1):line)
              deallocate(hs0)
              allocate(hs0(k))
              hs0(1:k) = tmp_r4(1:k)
              tmp_r4(1:k) = tp0((l+1):line)
              deallocate(tp0)
              allocate(tp0(k))
              tp0(1:k) = tmp_r4(1:k)
              tmp_r4(1:k) = dir0((l+1):line)
              deallocate(dir0)
              allocate(dir0(k))
              dir0(1:k) = tmp_r4(1:k)
              tmp_r4(1:k) = dspr0((l+1):line)
              deallocate(dspr0)
              allocate(dspr0(k))
              dspr0(1:k) = tmp_r4(1:k)
              tmp_r4(1:k) = wndspd0((l+1):line)
              deallocate(wndspd0)
              allocate(wndspd0(k))
              wndspd0(1:k) = tmp_r4(1:k)
              tmp_r4(1:k) = wnddir0((l+1):line)
              deallocate(wnddir0)
              allocate(wnddir0(k))
              wnddir0(1:k) = tmp_r4(1:k)
              deallocate(tmp_r4)
              line = k
              l = 0
            end if
          end if
          l = l + 1
        end do
        !
        if (allocated(ts)) deallocate(ts)
        if (allocated(llat)) deallocate(llat)
        if (allocated(llon)) deallocate(llon)
        if (allocated(mlat)) deallocate(mlat)
        if (allocated(mlon)) deallocate(mlon)
        if (allocated(date0)) deallocate(date0)
        if (allocated(hs0)) deallocate(hs0)
        if (allocated(tp0)) deallocate(tp0)
        if (allocated(dir0)) deallocate(dir0)
        if (allocated(dspr0)) deallocate(dspr0)
        !         if (allocated(wf0)) deallocate(wf0)
        if (allocated(wndspd0)) deallocate(wndspd0)
        if (allocated(wnddir0)) deallocate(wnddir0)
      end if
      !         communicate the wsdat entries from rank=0 to other ranks
      do tsa = t0,maxts
        irank = mod((tsa-t0),min(nproc,maxts))
        !             write(20,*) 'rank,irank=',rank,irank
        if (irank.ne.0) then
          !               write(20,*) 'communicating for rank,irank=',rank,irank
          if (rank.eq.irank) then
            allocate(wsdat(tsa)%lat(maxi,maxj))
            allocate(wsdat(tsa)%lon(maxi,maxj))
            allocate(wsdat(tsa)%par(maxi,maxj))
            allocate(wsdat(tsa)%wnd(maxi,maxj))
            do j = 1,maxj
              do i = 1,maxi
                wsdat(tsa)%maxi=maxi
                wsdat(tsa)%maxj=maxj
                wsdat(tsa)%par(i,j)%hs(1:10)=9999.
                wsdat(tsa)%par(i,j)%tp(1:10)=9999.
                wsdat(tsa)%par(i,j)%dir(1:10)=9999.
                wsdat(tsa)%par(i,j)%dspr(1:10)=9999.
                wsdat(tsa)%par(i,j)%ipart(1:10)=0
                wsdat(tsa)%par(i,j)%sys(1:10)=9999                      ! 40.par increase this array, or make allocatable
                wsdat(tsa)%par(i,j)%ngbrsys(1:50)=9999
                wsdat(tsa)%wnd(i,j)%wdir=9999.
                wsdat(tsa)%wnd(i,j)%wspd=9999.
                wsdat(tsa)%par(i,j)%checked=-1
              end do
            end do
          end if
          do j = 1,maxj
            do i = 1,maxi
              tag1 = ((j-1)*maxi+i)*10
              if (rank.eq.0) then
                !                        write(6,*) '>> sending: rank,irank,tag1=', &
                !                                rank,irank,(tag1+1)
                commarr1 = (/wsdat(tsa)%par(i,j)%hs(:), &
                     wsdat(tsa)%par(i,j)%tp(:), &
                     wsdat(tsa)%par(i,j)%dir(:), &
                     wsdat(tsa)%par(i,j)%dspr(:), &
                     wsdat(tsa)%wnd(i,j)%wdir, &
                     wsdat(tsa)%wnd(i,j)%wspd, &
                     wsdat(tsa)%lat(i,j), &
                     wsdat(tsa)%lon(i,j)/)
                call mpi_send(commarr1,44,mpi_real,irank, &
                     (tag1+1),mpi_comm_world,ierr)
              end if
              if (rank.eq.irank) then
                !                         write(6,*) '<< receiving: rank,irank,tag1=', &
                !                                rank,irank,(tag1+1)
                call mpi_recv(commarr1,44,mpi_real,0,(tag1+1), &
                     mpi_comm_world,mpi_status,ierr)
                wsdat(tsa)%par(i,j)%hs = commarr1(1:10)
                wsdat(tsa)%par(i,j)%tp = commarr1(11:20)
                wsdat(tsa)%par(i,j)%dir = commarr1(21:30)
                wsdat(tsa)%par(i,j)%dspr = commarr1(31:40)
                wsdat(tsa)%wnd(i,j)%wdir = commarr1(41)
                wsdat(tsa)%wnd(i,j)%wspd = commarr1(42)
                wsdat(tsa)%lat(i,j) = commarr1(43)
                wsdat(tsa)%lon(i,j) = commarr1(44)
              end if
              if (rank.eq.0) then
                call mpi_send(wsdat(tsa)%date,1, &
                     mpi_double_precision,irank, &
                     (tag1+2),mpi_comm_world,ierr)
              end if
              if (rank.eq.irank) then
                call mpi_recv(wsdat(tsa)%date,1, &
                     mpi_double_precision,0,(tag1+2), &
                     mpi_comm_world,mpi_status,ierr)
              end if
              if (rank.eq.0) then
                !                        write(6,*) '>> sending: rank,irank,tag1=', &
                !                                rank,irank,(tag1+3)
                commarr2 = (/wsdat(tsa)%par(i,j)%ipart(:), &
                     wsdat(tsa)%par(i,j)%checked/)
                call mpi_send(commarr2,11, &
                     mpi_integer,irank,(tag1+3),mpi_comm_world,ierr)
              end if
              if (rank.eq.irank) then
                !                         write(6,*) '<< receiving: rank,irank,tag1=', &
                !                                rank,irank,(tag1+3)
                call mpi_recv(commarr2,11, &
                     mpi_integer,0,(tag1+3), &
                     mpi_comm_world,mpi_status,ierr)
                wsdat(tsa)%par(i,j)%ipart(:) = commarr2(1:10)
                wsdat(tsa)%par(i,j)%checked = commarr2(11)
              end if
            end do
          end do
        end if
      end do
      call mpi_barrier(mpi_comm_world,ierr)
      if (rank.eq.0) then
        ! ----*** test output *** --------------------------------------------------
        if (testout) then
          !-----raw partition output: coordinates
          open(unit=31,file='part_coord.out',status='unknown')
          write(31,*) 'longitude ='
          do j = maxj,1,-1
            do i = 1,maxi
              write(31,'(f7.2)',advance='no') wsdat(1)%lon(i,j)
            end do
            write(31,'(a)',advance='yes') ''
          end do
          write(31,*) 'latitude = '
          do j = maxj,1,-1
            do i = 1,maxi
              write(31,'(f7.2)',advance='no') wsdat(1)%lat(i,j)
            end do
            write(31,'(a)',advance='yes') ''
          end do
          close(31)
          !-----raw partition output: hs
          open(unit=32, file='part_hsign.out', &
               status='unknown')
          maxpartout = 5
          do tsa = 1,size(wsdat)
            write(32,'(i4,71x,a)') tsa,'time step'
            write(32,'(i4,71x,a)') maxpartout,'tot number of raw partitions'
            do k = 1,maxpartout
              write(32,'(i4,71x,a)') k,'system number'
              write(32,'(i4,71x,a)') 9999,'number of points in system'
              do j = maxj,1,-1
                do i = 1,maxi
                  write(32,'(f8.2)',advance='no') wsdat(tsa)%par(i,j)%hs(k)
                end do
                write(32,'(a)',advance='yes') ''
              end do
            end do
          end do
          close(32)
          !-----raw partition output: tp
          !      open(unit=33,recl=2147483646, file='part_tp.out', &
          !           status='unknown')
          open(unit=33, file='part_tp.out', &
               status='unknown')
          do tsa = 1,size(wsdat)
            write(33,'(i4,71x,a)') tsa,'time step'
            write(33,'(i4,71x,a)') maxpartout,'tot number of raw partitions'
            do k = 1,maxpartout
              write(33,'(i4,71x,a)') k,'system number'
              write(33,'(i4,71x,a)') 9999,'number of points in system'
              do j = maxj,1,-1
                do i = 1,maxi
                  write(33,'(f8.2)',advance='no') wsdat(tsa)%par(i,j)%tp(k)
                end do
                write(33,'(a)',advance='yes') ''
              end do
            end do
          end do
          close(33)
          !-----raw partition output: dir
          open(unit=34, file='part_dir.out', &
               status='unknown')
          do tsa = 1,size(wsdat)
            write(34,'(i4,71x,a)') tsa,'time step'
            write(34,'(i4,71x,a)') maxpartout,'tot number of raw partitions'
            do k = 1,maxpartout
              write(34,'(i4,71x,a)') k,'system number'
              write(34,'(i4,71x,a)') 9999,'number of points in system'
              do j = maxj,1,-1
                do i = 1,maxi
                  write(34,'(f8.2)',advance='no') wsdat(tsa)%par(i,j)%dir(k)
                end do
                write(34,'(a)',advance='yes') ''
              end do
            end do
          end do
          close(34)
          !-----raw partition output: dspr
          open(unit=35, file='part_dspr.out', &
               status='unknown')
          do tsa = 1,size(wsdat)
            write(35,'(i4,71x,a)') tsa,'time step'
            write(35,'(i4,71x,a)') maxpartout,'tot number of raw partitions'
            do k = 1,maxpartout
              write(35,'(i4,71x,a)') k,'system number'
              write(35,'(i4,71x,a)') 9999,'number of points in system'
              do j = maxj,1,-1
                do i = 1,maxi
                  write(35,'(f8.2)',advance='no') &
                       wsdat(tsa)%par(i,j)%dspr(k)
                end do
                write(35,'(a)',advance='yes') ''
              end do
            end do
          end do
          close(35)
        end if
      end if
      ! ------------------------------------------------------------------------
      !        allocate the sysa structure
      if (rank.eq.0) then
        write(20,*) 'allocating sysa...'
      end if
      allocate( sysa(maxts) )
      if (rank.eq.0) then
        write(20,*) 'size(sysa) = ',size(sysa)
        write(6,1020) ' number of time levels being processed:',size(sysa)
1020    format(a,i4)
      end if
      !        allocate maxsys
      allocate( maxsys(maxts) )
    else
      !        raw partitioning data from wave model memory, via the array wsdat.
      !        set maxts to the time step to compute: 1=first time step, 2=otherwise
      maxts = tmax
      t0 = tcur
      !        allocate the sysa structure
      allocate( sysa(1) )                                              !change to sysa(2)?
      !        allocate maxsys
      allocate( maxsys(1) )                                            !change to maxsys(2)?
    end if
    !     big loop over all time levels
    if (rank.eq.0) then
      write(6,*) 'performing spatial tracking...'
    end if
    !      write(20,*) 'rank,t0,maxts,nproc =',rank,t0,maxts,nproc
    do tsa = (t0+rank),maxts,min(nproc,maxts)
      !         write(20,*) 'computing: rank, tsa =',rank,tsa
        write(20,*) 'call spiraltrackv3, tsa=',tsa,'...'
        call spiraltrackv3 ( wsdat(tsa), dirknob, perknob, wetpts, &
             hsknob, seedlat, seedlon, &
             maxsys(tsa), sysa(tsa)%sys )
        write(20,*) '*** size(sysa(1:tsa)%sys) at end of time step', &
             tsa,':'
        write(20,*) size(sysa(tsa)%sys)
    end do
    call mpi_barrier(mpi_comm_world,ierr)
    !!     define communicator for array of integers in structure "system"
    !      domsize = maxi*maxj
    !      write(20,*) 'rank',rank,'domsize =',domsize
    !      call mpi_type_contiguous(domsize,mpi_integer,mpi_int_domarr,ierr)
    !      call mpi_type_commit(mpi_int_domarr,ierr)
    !      call mpi_type_extent(mpi_int_domarr,extent,ierr)
    !      write(20,*) 'rank',rank,'has set up communicator mpi_int_domarr, &
    !                  size =',extent
    !!     define communicator for array of reals in structure "system"
    !      call mpi_type_contiguous(domsize,mpi_real,mpi_real_domarr,ierr)
    !      call mpi_type_commit(mpi_real_domarr,ierr)
    !      call mpi_type_extent(mpi_real_domarr,extent,ierr)
    !      write(20,*) 'rank',rank,'has set up communicator mpi_real_domarr, &
    !                  size =',extent
    !     communicate results back to rank 0
    do tsa = t0,maxts
      irank = mod((tsa-t0),min(nproc,maxts))
      !         write(20,*) 'rank,irank=',rank,irank
      if (irank.ne.0) then
        !            write(20,*) 'communicating for rank,irank=',rank,irank
        !           send maxsys(tsa) at each time level to rank 0
        tag1 = tsa
        if (rank.eq.irank) then
          !              send results from current rank to rank 0 (blocking)
          !               write(20,*) '>> sending: rank,tsa,tag1=',rank,tsa,tag1
          call mpi_send(maxsys(tsa),1,mpi_integer,0,tag1, &
               mpi_comm_world,ierr)
          !               write(20,*) 'rank, ierr=',rank,ierr
        end if
        if (rank.eq.0) then
          !               write(20,*) '<< receiving: rank,tsa,tag1=',rank,tsa,tag1
          call mpi_recv(maxsys(tsa),1,mpi_integer, &
               irank,tag1,mpi_comm_world,mpi_status,ierr)
          !              allocate structure at this time level
          allocate( sysa(tsa)%sys(maxsys(tsa)) )
          do ic = 1,maxsys(tsa)
            nullify( sysa(tsa)%sys(ic)%i )
            nullify( sysa(tsa)%sys(ic)%j )
            nullify( sysa(tsa)%sys(ic)%lon )
            nullify( sysa(tsa)%sys(ic)%lat )
            nullify( sysa(tsa)%sys(ic)%hs )
            nullify( sysa(tsa)%sys(ic)%tp )
            nullify( sysa(tsa)%sys(ic)%dir)
            nullify( sysa(tsa)%sys(ic)%dspr)
            allocate( sysa(tsa)%sys(ic)%i(maxi*maxj) )
            allocate( sysa(tsa)%sys(ic)%j(maxi*maxj) )
            allocate( sysa(tsa)%sys(ic)%lon(maxi*maxj) )
            allocate( sysa(tsa)%sys(ic)%lat(maxi*maxj) )
            allocate( sysa(tsa)%sys(ic)%hs(maxi*maxj) )
            allocate( sysa(tsa)%sys(ic)%tp(maxi*maxj) )
            allocate( sysa(tsa)%sys(ic)%dir(maxi*maxj) )
            allocate( sysa(tsa)%sys(ic)%dspr(maxi*maxj) )
            sysa(tsa)%sys(ic)%i(:) = 9999
            sysa(tsa)%sys(ic)%j(:) = 9999
            sysa(tsa)%sys(ic)%lon(:) = 9999.
            sysa(tsa)%sys(ic)%lat(:) = 9999.
            sysa(tsa)%sys(ic)%hs(:) = 9999.
            sysa(tsa)%sys(ic)%tp(:) = 9999.
            sysa(tsa)%sys(ic)%dir(:) = 9999.
            sysa(tsa)%sys(ic)%dspr(:) = 9999.
            sysa(tsa)%sys(ic)%hsmean = 9999.
            sysa(tsa)%sys(ic)%tpmean = 9999.
            sysa(tsa)%sys(ic)%dirmean = 9999.
            sysa(tsa)%sys(ic)%sysind = 9999
            sysa(tsa)%sys(ic)%npoints = 9999
            sysa(tsa)%sys(ic)%grp = 9999
          end do
        end if
        !           send data fields at each (tsa,ic) combination
        if ((rank.eq.0).or.(rank.eq.irank)) then
          do ic = 1, maxsys(tsa)
            !              construct a unique tag for each message
            tag2 = tsa*10000 + ic*100
            domsize = maxi*maxj
            if (rank.eq.irank) then
              !                 write(20,*) '>> sending: rank,irank,tag2=', &
              !                             rank,irank,(tag2+1)
              call mpi_send(sysa(tsa)%sys(ic)%i(:),domsize, &
                   mpi_integer,0,(tag2+1),mpi_comm_world,req(1),ierr)
            end if
            if (rank.eq.0) then
              !                 write(20,*) '<< receiving: rank,irank,tag2=', &
              !                             rank,irank,(tag2+1)
              call mpi_recv(sysa(tsa)%sys(ic)%i(:),domsize, &
                   mpi_integer,irank,(tag2+1), &
                   mpi_comm_world,mpi_status,req(2),ierr)
            end if
            !               call mpi_waitall(2,req,istat,ierr)
            if (rank.eq.irank) then
              !                 write(20,*) '>> sending: rank,irank,tag2=', &
              !                             rank,irank,(tag2+2)
              call mpi_send(sysa(tsa)%sys(ic)%j(:),domsize, &
                   mpi_integer,0,(tag2+2),mpi_comm_world,req(1),ierr)
            end if
            if (rank.eq.0) then
              !                 write(20,*) '<< receiving: rank,irank,tag2=', &
              !                             rank,irank,(tag2+2)
              call mpi_recv(sysa(tsa)%sys(ic)%j(:),domsize, &
                   mpi_integer,irank,(tag2+2), &
                   mpi_comm_world,mpi_status,req(2),ierr)
            end if
            !               call mpi_waitall(2,req,istat,ierr)
            if (rank.eq.irank) then
              !                 write(20,*) '>> sending: rank,tag2=',rank,(tag2+3)
              call mpi_send(sysa(tsa)%sys(ic)%lon(:),domsize, &
                   mpi_real,0,(tag2+3),mpi_comm_world,req(1),ierr)
            end if
            if (rank.eq.0) then
              !                 write(20,*) '<< receiving: rank,tag2=',rank,(tag2+3)
              call mpi_recv(sysa(tsa)%sys(ic)%lon(:),domsize, &
                   mpi_real,irank,(tag2+3), &
                   mpi_comm_world,mpi_status,req(2),ierr)
            end if
            !               call mpi_waitall(2,req,istat,ierr)
            if (rank.eq.irank) then
              !                 write(20,*) '>> sending: rank,tag2=',rank,(tag2+4)
              call mpi_send(sysa(tsa)%sys(ic)%lat(:),domsize, &
                   mpi_real,0,(tag2+4),mpi_comm_world,req(1),ierr)
            end if
            if (rank.eq.0) then
              !                 write(20,*) '<< receiving: rank,tag2=',rank,(tag2+4)
              call mpi_recv(sysa(tsa)%sys(ic)%lat(:),domsize, &
                   mpi_real,irank,(tag2+4), &
                   mpi_comm_world,mpi_status,req(2),ierr)
            end if
            !               call mpi_waitall(2,req,istat,ierr)
            if (rank.eq.irank) then
              !                 write(20,*) '>> sending: rank,tag2=',rank,(tag2+5)
              call mpi_send(sysa(tsa)%sys(ic)%hs(:),domsize, &
                   mpi_real,0,(tag2+5),mpi_comm_world,req(1),ierr)
            end if
            if (rank.eq.0) then
              !                 write(20,*) '<< receiving: rank,tag2=',rank,(tag2+5)
              call mpi_recv(sysa(tsa)%sys(ic)%hs(:),domsize, &
                   mpi_real,irank,(tag2+5), &
                   mpi_comm_world,mpi_status,req(2),ierr)
            end if
            !               call mpi_waitall(2,req,istat,ierr)
            if (rank.eq.irank) then
              !                 write(20,*) '>> sending: rank,tag2=',rank,(tag2+6)
              call mpi_send(sysa(tsa)%sys(ic)%tp(:),domsize, &
                   mpi_real,0,(tag2+6),mpi_comm_world,req(1),ierr)
            end if
            if (rank.eq.0) then
              !                 write(20,*) '<< receiving: rank,tag2=',rank,(tag2+6)
              call mpi_recv(sysa(tsa)%sys(ic)%tp(:),domsize, &
                   mpi_real,irank,(tag2+6), &
                   mpi_comm_world,mpi_status,req(2),ierr)
            end if
            !               call mpi_waitall(2,req,istat,ierr)
            if (rank.eq.irank) then
              !                 write(20,*) '>> sending: rank,tag2=',rank,(tag2+7)
              call mpi_send(sysa(tsa)%sys(ic)%dir(:),domsize, &
                   mpi_real,0,(tag2+7),mpi_comm_world,req(1),ierr)
            end if
            if (rank.eq.0) then
              !                 write(20,*) '<< receiving: rank,tag2=',rank,(tag2+7)
              call mpi_recv(sysa(tsa)%sys(ic)%dir(:),domsize, &
                   mpi_real,irank,(tag2+7), &
                   mpi_comm_world,mpi_status,req(2),ierr)
            end if
            !               call mpi_waitall(2,req,istat,ierr)
            if (rank.eq.irank) then
              !                 write(20,*) '>> sending: rank,tag2=',rank,(tag2+8)
              call mpi_send(sysa(tsa)%sys(ic)%dspr(:),domsize, &
                   mpi_real,0,(tag2+8),mpi_comm_world,req(1),ierr)
            end if
            if (rank.eq.0) then
              !                 write(20,*) '<< receiving: rank,tag2=',rank,(tag2+8)
              call mpi_recv(sysa(tsa)%sys(ic)%dspr(:),domsize, &
                   mpi_real,irank,(tag2+8), &
                   mpi_comm_world,mpi_status,req(2),ierr)
            end if
            !               call mpi_waitall(2,req,istat,ierr)
            if (rank.eq.irank) then
              !                 write(20,*) '>> sending: rank,irank,tag2=', &
              !                             rank,irank,(tag2+9)
              call mpi_send(sysa(tsa)%sys(ic)%hsmean,1,mpi_real, &
                   0,(tag2+9),mpi_comm_world,ierr)
            end if
            if (rank.eq.0) then
              !                 write(20,*) '<< receiving: rank,irank,tag2=', &
              !                             rank,irank,(tag2+9)
              call mpi_recv(sysa(tsa)%sys(ic)%hsmean,1,mpi_real, &
                   irank,(tag2+9),mpi_comm_world,mpi_status,ierr)
            end if
            if (rank.eq.irank) then
              !                 write(20,*) '>> sending: rank,irank,tag2=', &
              !                             rank,irank,(tag2+10)
              call mpi_send(sysa(tsa)%sys(ic)%tpmean,1,mpi_real, &
                   0,(tag2+10),mpi_comm_world,ierr)
            end if
            if (rank.eq.0) then
              !                 write(20,*) '<< receiving: rank,irank,tag2=', &
              !                             rank,irank,(tag2+10)
              call mpi_recv(sysa(tsa)%sys(ic)%tpmean,1,mpi_real, &
                   irank,(tag2+10),mpi_comm_world,mpi_status,ierr)
            end if
            if (rank.eq.irank) then
              !                 write(20,*) '>> sending: rank,irank,tag2=', &
              !                             rank,irank,(tag2+11)
              call mpi_send(sysa(tsa)%sys(ic)%dirmean,1,mpi_real, &
                   0,(tag2+11),mpi_comm_world,ierr)
            end if
            if (rank.eq.0) then
              !                 write(20,*) '<< receiving: rank,irank,tag2=', &
              !                             rank,irank,(tag2+11)
              call mpi_recv(sysa(tsa)%sys(ic)%dirmean,1,mpi_real, &
                   irank,(tag2+11),mpi_comm_world,mpi_status,ierr)
            end if
            if (rank.eq.irank) then
              !                 write(20,*) '>> sending: rank,irank,tag2=', &
              !                             rank,irank,(tag2+12)
              call mpi_send(sysa(tsa)%sys(ic)%sysind,1,mpi_integer,&
                   0,(tag2+12),mpi_comm_world,ierr)
            end if
            if (rank.eq.0) then
              !                 write(20,*) '<< receiving: rank,irank,tag2=', &
              !                             rank,irank,(tag2+12)
              call mpi_recv(sysa(tsa)%sys(ic)%sysind,1,mpi_integer,&
                   irank,(tag2+12),mpi_comm_world,mpi_status,ierr)
            end if
            if (rank.eq.irank) then
              !                 write(20,*) '>> sending: rank,irank,tag2=', &
              !                             rank,irank,(tag2+13)
              call mpi_send(sysa(tsa)%sys(ic)%npoints,1,mpi_integer,&
                   0,(tag2+13),mpi_comm_world,ierr)
            end if
            if (rank.eq.0) then
              !                 write(20,*) '<< receiving: rank,irank,tag2=', &
              !                             rank,irank,(tag2+13)
              call mpi_recv(sysa(tsa)%sys(ic)%npoints,1,mpi_integer,&
                   irank,(tag2+13),mpi_comm_world,mpi_status,ierr)
            end if
            if (rank.eq.irank) then
              !                 write(20,*) '>> sending: rank,irank,tag2=', &
              !                             rank,irank,(tag2+14)
              call mpi_send(sysa(tsa)%sys(ic)%grp,1,mpi_integer,&
                   0,(tag2+14),mpi_comm_world,ierr)
            end if
            if (rank.eq.0) then
              !                 write(20,*) '<< receiving: rank,irank,tag2=', &
              !                             rank,irank,(tag2+14)
              call mpi_recv(sysa(tsa)%sys(ic)%grp,1,mpi_integer,&
                   irank,(tag2+14),mpi_comm_world,mpi_status,ierr)
            end if
          end do
        end if
      end if
    end do
    call mpi_barrier(mpi_comm_world,ierr)
    !      call mpi_type_free(mpi_int_domarr,ierr)
    !      call mpi_type_free(mpi_real_domarr,ierr)
    if (rank.eq.0) then
      write(6,*) 'performing temporal tracking...'
      write(20,*) 'calling timetrackingv2...'
      lonext = wsdat(1)%lon(maxi,1)-wsdat(1)%lon(1,1)
      latext = wsdat(1)%lat(1,maxj)-wsdat(1)%lat(1,1)
      call timetrackingv2 (sysa, maxsys, tptimeknob, dirtimeknob, 1, &
           maxgroup, dt, lonext, latext, maxi, maxj)
      !
    end if
    !
    return
    !
802 continue
    write (6,990) ioerr
    stop 1
990 format (/' *** wavewatch iii error in w3strkmd : '/            &
         '     error in reading from partition file'/          &
         '     iostat =',i5/)
1000 format (f9.0,f7.0,f8.3,f8.3,14x,i3,7x,f5.1,f6.1,f5.1,f6.1)
1010 format (3x,f8.2,f8.2,8x,f9.2,f9.2)
1200 format (/' *** wavewatch iii error in w3strkmd : '/            &
         '     error in reading partition file   '/            &
         '     incompatible endianess'/                        )
1300 format (/' *** wavewatch iii error in w3strkmd : '/            &
         '     error in reading partition file   '/                   &
         '     expected idstr "wavewatch iii partitioned data file"'/ )
1400 format (/' *** wavewatch iii error in w3strkmd : '/            &
         '     error in finding domain to process - '/         &
         '     specified lat/lon limits within domain '/       &
         '     of raw partition file?'/                        )
2001 format (/' *** wavewatch iii error in w3systrk : '/            &
         '     error in opening input file'/                   )
2002 format (/' *** wavewatch iii error in w3systrk : '/            &
         '     premature end of input file'/                   )
2003 format (/' *** wavewatch iii error in w3systrk : '/            &
         '     premature end of partition file - '/            &
         '     tstart=',f13.4/                                 )
    !
    !
  end subroutine wavetracking_nws_v2
  !/ end of wavetracking_nws_v2 ---------------------------------------- /
  !/
  !/ ------------------------------------------------------------------- /
  subroutine spiraltrackv3 (wsdat   ,dirknob ,perknob ,wetpts  , &
       hsknob  ,seedlat ,seedlon  , &
       maxsys  ,sys   )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |     a. j. van der westhuysen      |
    !/                  |            jeff hanson            |
    !/                  |        eve-marie devaliere        |
    !/                  |                        fortran 95 |
    !/                  | last update :          4-jan-2013 |
    !/                  +-----------------------------------+
    !/
    !/    03-feb-2012 : origination, based on matlab code   ( version 4.05 )
    !/                  by jeff hanson & eve-marie devaliere
    !/    04-jan-2013 : inclusion in trunk                  ( version 4.08 )
    !/
    !/    copyright 2009-2013 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    implicit none
    !
    !  1. purpose :
    !
    !     performs the spatial spiral tracking for a given time step
    !
    !  2. method
    !
    !     index convention on grid:
    !
    !      j
    !      ^
    !      |+(1,maxj)          +(maxi,maxj)
    !      |
    !      |
    !      |
    !      |
    !      |
    !      |
    !      |(1,1)              +(maxi,1)
    !      +----------------------> i
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     dirknob     real     input  parameter in direction for combining fields in space
    !     perknob     real     input  parameter in period for combining fields in space
    !     wetpts      real     input  percentage of wet points for purging fields (fraction)
    !     hsknob      real     input  parameter in wave height for purging fields
    !     seedlat     real     input  start lat for tracking spiral (if =0 centre of field is used)
    !     seedlon     real     input  start lon for tracking spiral (if =0 centre of field is used)
    !     wsdat       real arr output input 2d (gridded) data structure to be spiral tracked
    !     maxsys      int      output maximum number of partition systems
    !     sys     type(system) output final set of tracked systems, for one time level
    !
    type(dat2d)  :: wsdat
    real         :: dirknob,perknob,wetpts,hsknob,seedlat,seedlon
    integer      :: maxsys
    type(system), pointer :: sys(:)
    intent (in) wetpts,dirknob,perknob,hsknob,seedlat,seedlon
    intent (in out) wsdat
    !      intent (out) maxsys,sys
    !
    !     local variables
    !     ----------------------------------------------------------------
    !     ngbrext    int    how far do we want the neighbour to be considered
    !     combine    int    toggle (1=combine wave systems; 0=do not combine)
    !     maxi,maxj  int    dimensions of the 2d (gridded) data wsdat
    !     deltalat   real   delta in kilometers between 2 pts (in latitude)
    !
    logical   :: first
    character :: way *1
    integer   :: ngbrext, combine, maxi, maxj, i, j, oldj
    integer   :: horizstepcount, vertstepcount, checkcount, sc, &
         maxpts, landpts, horizborder, vertborder, m, k, &
         stepcount
    real      :: deltalat, minlat, maxlat, minlon, maxlon
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      findway
    !      findnext
    !      findsys
    !      combinewavesystems
    !
    !  5. subroutines calling
    !
    !     wavetracking_nws_v2
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     -
    !
    !  9. switches :
    !
    !     none defined yet.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    ! routine starts by identifying the starting point. choose the 'center' of the domain
    !     set the search distance for neighbors:
    !     1: 1 row and column out, i.e. the 8 neighbors around the current point
    !     2: 2 rows and columns out... etc.
    ngbrext=1
    combine=1
    write(20,*) 'in spiraltrackv3: combine = ',combine
    maxi = wsdat%maxi
    maxj = wsdat%maxj
    if ( (seedlat.eq.0).or.(seedlon.eq.0) ) then
      i=nint(real(maxi)/2.)
      j=nint(real(maxj)/2.)
      write(20,*) 'in spiraltrackv3, i=nint(maxi/2.) =',i
      write(20,*) 'in spiraltrackv3, j=nint(maxj/2.) =',j
    else
      i=1
      j=1
      do while ( (wsdat%lat(1,j).lt.seedlat).and.(j.lt.wsdat%maxj) )     !40.par !improve with swan's indice identification
        j=j+1
      end do
      do while ( (wsdat%lon(i,1).lt.seedlon).and.(i.lt.wsdat%maxi) )
        i=i+1
      end do
    end if
    !     in case center point is land point...
    if (wsdat%par(i,j)%checked.eq.-1) then
      oldj=j
      do while (wsdat%par(i,j)%checked.eq.-1)
        j=j+1
        if (j.eq.maxj) then
          j=oldj
          i=i+1
          oldj=oldj+1
        end if
      end do
    end if
    !     compute distance in km between 2 grid points (at equator)
    deltalat=(wsdat%lat(i,j)-wsdat%lat(i,j-1))*111.18
    !     starts the spiral
    !     intitiate variables
    horizstepcount=0
    vertstepcount=0
    way='r'
    first=.true.
    checkcount=1
    maxsys=0
    landpts=0
    minlat=minval(wsdat%lat)
    maxlat=maxval(wsdat%lat)
    minlon=minval(wsdat%lon)
    maxlon=maxval(wsdat%lon)
    horizborder=0
    vertborder=0
    do while (checkcount.le.(maxi*maxj-3) )
      !        from the direction (way) we were going before, find which direction we
      !        are going now and how many 'step' we need to take
      call findway(way, horizstepcount, vertstepcount, &
           vertborder, horizborder, stepcount)
      if (first) then
        m=0
        do k=1,length(wsdat%par(i,j)%hs, &
             size(wsdat%par(i,j)%hs),9999.)
          if ( (wsdat%par(i,j)%hs(k).eq.0.).and. &
               (wsdat%par(i,j)%tp(k).eq.0.) ) then
            wsdat%par(i,j)%sys(k)=-1
          else
            m=m+1
            wsdat%par(i,j)%sys(k)=m
          end if
        end do
        wsdat%par(i,j)%checked=1
        checkcount=checkcount+1
        first=.false.
      end if
      do sc = 1, stepcount
        call findnext (i,j,maxi,maxj,way,vertborder,horizborder)
        if ( wsdat%par(i,j)%checked.eq.-1 ) then
          !            land point is one of our grid points, so we need to update counter
          checkcount=checkcount+1
          landpts=landpts+1
          !                so that we don't count the land points twice....
          wsdat%par(i,j)%checked=-2
        else if ( wsdat%par(i,j)%checked.eq.0 ) then
          !            hasn't been checked yet and is not land point
          checkcount=checkcount+1
          call findsys(i, j, wsdat, maxsys, ngbrext, maxi, maxj, &
               perknob, dirknob, hsknob)
        end if
      end do
    end do
    !     wetpts% of wet points
    maxpts=nint(wetpts*(maxi*maxj-1))
    !
    write(20,*) 'call combinewavesystems...'
    call combinewavesystems(wsdat,maxsys,maxpts,maxi,maxj, &
         perknob,dirknob,hsknob,combine,sys)
    return
  end subroutine spiraltrackv3
  !/ end of spiraltrackv3 ---------------------------------------------- /
  !/
  !/ ------------------------------------------------------------------- /
  subroutine timetrackingv2 (sysa       ,maxsys     ,tptimeknob  , &
       dirtimeknob,ts0        ,maxgroup    , &
       dt         ,lonext     ,latext      , &
       maxi       ,maxj                    )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |     a. j. van der westhuysen      |
    !/                  |            jeff hanson            |
    !/                  |        eve-marie devaliere        |
    !/                  |                        fortran 95 |
    !/                  | last update :          4-jan-2013 |
    !/                  +-----------------------------------+
    !/
    !/    03-feb-2012 : origination, based on matlab code   ( version 4.05 )
    !/                  by jeff hanson & eve-marie devaliere
    !/    04-jan-2013 : inclusion in trunk                  ( version 4.08 )
    !/
    !/    copyright 2009-2013 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    implicit none
    !
    !  1. purpose :
    !
    !     performs the time tracking of the systems identified within
    !     the subroutine spiraltrackv3.
    !
    !  2. method
    !
    !     -
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     note: perknob, dirknob in matlab version replaced by tptimeknob, dirtimeknob!
    !
    !     sysa        type(timsys) in/out final set of spatially and temporally tracked systems
    !     dirtimeknob    real      input  parameter in direction for combining fields in time
    !     tptimeknob     real      input  parameter in period for combining fields in time
    !     ts0            int       input  time step to which default grp values are associated
    !     maxsys         int arr   input  total number of systems per time level
    !     maxgroup       int       output maximum number of wave systems ("groups") tracked in time
    !     lonext         real      input  longitudinal extent of domain
    !     latext         real      input  latitudinal extent of domain
    !     maxi, maxj     int       input  maximum indices of wave field
    !
    type(timsys), pointer :: sysa(:)
    integer, pointer :: maxsys(:)
    real         :: dirtimeknob, tptimeknob
    integer      :: ts0, maxgroup
    real         :: dt
    real         :: lonext, latext
    integer      :: maxi, maxj
    intent (in) tptimeknob, dirtimeknob, ts0, maxi, maxj
    !      intent (in out) sysa
    intent (out) maxgroup
    !
    !     local variables
    !     ----------------------------------------------------------------
    !     ic        int   counter for wave systems
    !     ts1       int   adjusted initial time step in case ts0 has only empty systems
    !
    logical :: file_exists
    character :: dummy*23
    type(sysmemory) :: sysmem(50)                                             !!! 50 memory spaces should be enough check!!!
    integer :: leng, l, i, ii, j, k, kk, idir, numsys, &
         counter, new, difsize, tpminind, dirminind, used, ok
    real    :: tb,  deltaper, deltadir, tpminval, dirminval, &
         dirfortpmin, tpfordirmin
    real, allocatable :: sysordered(:), temp(:), dirs(:)
    real, pointer :: difarr(:)
    integer, allocatable :: indsorted(:), alreadyused(:), allind(:)
    integer, allocatable :: ind(:), ind2(:)
    integer :: ts1
    real, allocatable :: gof(:,:), gofminval(:), gofminind(:), &
         tbsysmem(:), deltadirsysmem(:), &
         deltapersysmem(:),m1sysmem(:),m2sysmem(:)
    real    :: m1, m2
    real    :: lonmean, latmean, dmndiag
    integer :: npnts, npnts2
    real, allocatable :: mnlonlist(:), mnlatlist(:), mndist(:)
    real, pointer     :: dummy1(:),dummy2(:),dummy3(:)
    integer, allocatable :: olsize(:)
    real    :: temp1, temp2
    integer :: iii, jj, ll, idup
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      sort
    !      setdiff
    !
    !  5. subroutines calling
    !
    !     wavetracking_nws_v2
    !
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     -
    !
    !  9. switches :
    !
    !     none defined yet.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !     associate default grp value to time step 1
    write(20,*) 'time tracking'
    write(20,*) 'inside timetrackingv2: size(sysa(1)%sys) =', &
         size(sysa(1)%sys)
    write(20,*) 'inside timetrackingv2: maxsys(1) =',maxsys(1)
    write(20,*) 'ts0 = ',ts0
    ts1 = ts0
    !     skip initial time steps with empty systems (e.g. when starting from rest)
    do i = ts1, size(sysa)
      if (size(sysa(ts1)%sys).eq.0) ts1 = ts1+1
      !        no non-empty systems found
      if (ts1.gt.size(sysa)) then
        maxgroup = 0
        goto 2000
      end if
    end do
    write(20,*) 'ts = ',ts1
    if (size(sysa(ts1)%sys).gt.0) then
      !        initialize system memory groups
      sysa(ts1)%sys(:)%grp = 9999
      sysmem(:)%grp = 9999
      sysmem(:)%npoints = 0
      sysmem(:)%lonmean = 9999.
      sysmem(:)%latmean = 9999.
      sysmem(:)%tpmean = 9999.
      sysmem(:)%dirmean = 9999.
      sysmem(:)%updated = -9999
      sysmem(:)%length = 0
      do iii = 1,50
        allocate(sysmem(iii)%indx(maxi*maxj))
        sysmem(iii)%indx = 9999
      end do
      inquire(file="sys_restart.ww3", exist=file_exists)
      if (file_exists) then
        !           use groups from wave tracking hotfile
        write(20,*) '*** using group memory hotfile'
        open(unit=12,file='sys_restart.ww3',status='old')
        read(12,'(a23,i10)') dummy,maxgroup
        write(20,*) 'reading ',maxgroup,' systems'
        do k = 1,maxgroup
          read(12,'(a23,i10)') dummy,sysmem(k)%grp
          read(12,'(a23,i10)') dummy,sysmem(k)%npoints
          read(12,'(a23,f10.4)') dummy,sysmem(k)%lonmean
          read(12,'(a23,f10.4)') dummy,sysmem(k)%latmean
          read(12,'(a23,f10.3)') dummy,sysmem(k)%tpmean
          read(12,'(a23,f10.3)') dummy,sysmem(k)%dirmean
          read(12,'(a23,i10)') dummy,sysmem(k)%updated
          read(12,'(a23,i10)') dummy,sysmem(k)%length
          do j = maxj,1,-1
            read(12,*) (sysmem(k)%indx((j-1)*maxi+i), i = 1,maxi)
          end do
          !reset update counter
          sysmem(k)%updated = 0
        end do
        close(12)
        ts1 = ts1-1
      else
        !        set up the group number array for the first time level to be tracked
        allocate( sysordered(maxsys(ts1)) )
        allocate( indsorted(maxsys(ts1)) )
        call sort (real(sysa(ts1)%sys(1:maxsys(ts1))%npoints), &
             maxsys(ts1),sysordered,indsorted,'d')
        sysa(ts1)%sys(1:maxsys(ts1)) = sysa(ts1)%sys(indsorted)
        if (allocated(sysordered)) deallocate(sysordered)
        if (allocated(indsorted)) deallocate(indsorted)
        !        set the initial long-term system memory
        do i = 1, maxsys(ts1)
          sysa(ts1)%sys(i)%grp = i
          !           set initial values of long-term system memory
          sysmem(i)%grp = i
          sysmem(i)%npoints = sysa(ts1)%sys(i)%npoints
          sysmem(i)%lonmean = &
               sum(sysa(ts1)%sys(i)%lon(1:sysmem(i)%npoints))/&
               sysmem(i)%npoints
          sysmem(i)%latmean = &
               sum(sysa(ts1)%sys(i)%lat(1:sysmem(i)%npoints))/&
               sysmem(i)%npoints
          !070512----------- weight averages with hm0 ---------------------
          temp1 = 0.
          temp2 = 0.
          do iii = 1,sysmem(i)%npoints
            temp1 = temp1 + &
                 (sysa(ts1)%sys(i)%hs(iii)**2)*sysa(ts1)%sys(i)%lon(iii)
            temp2 = temp2 + &
                 (sysa(ts1)%sys(i)%hs(iii)**2)*sysa(ts1)%sys(i)%lat(iii)
          end do
          sysmem(i)%lonmean = temp1/&
               max(sum(sysa(ts1)%sys(i)%hs(1:sysmem(i)%npoints)**2),&
               0.001)
          sysmem(i)%latmean = temp2/&
               max(sum(sysa(ts1)%sys(i)%hs(1:sysmem(i)%npoints)**2),&
               0.001)
          !070512----------- weight averages with hm0 ---------------------
          sysmem(i)%tpmean = sysa(ts1)%sys(i)%tpmean
          sysmem(i)%dirmean = sysa(ts1)%sys(i)%dirmean
          sysmem(i)%updated = ts1
          sysmem(i)%length = 1
          !071012----------- grid point indexing --------------------------
          do iii = 1,sysmem(i)%npoints
            sysmem(i)%indx(iii) = (sysa(ts1)%sys(i)%j(iii)-1)*maxi +&
                 sysa(ts1)%sys(i)%i(iii)
          end do
          !071012----------- grid point indexing --------------------------
        end do
        maxgroup = maxsys(ts1)
        !         i = ts1
      end if
      !******** test output ***********************
      do i = 1, maxgroup
        write(20,*) 'sysmem(',i,')%grp =',sysmem(i)%grp
        write(20,*) 'sysmem(',i,')%npoints =',sysmem(i)%npoints
        write(20,*) 'sysmem(',i,')%lonmean =',sysmem(i)%lonmean
        write(20,*) 'sysmem(',i,')%latmean =',sysmem(i)%latmean
        write(20,*) 'sysmem(',i,')%tpmean =',sysmem(i)%tpmean
        write(20,*) 'sysmem(',i,')%dirmean =',sysmem(i)%dirmean
        write(20,*) 'sysmem(',i,')%updated =',sysmem(i)%updated
        write(20,*) 'sysmem(',i,')%length =',sysmem(i)%length
      end do
      !********************************************
    end if
    !     loop over all time levels to track systems in time
    write(20,*) 'number of time levels = ',size(sysa)
    do i = (ts1+1), size(sysa)
      write(20,*) 'ts = ',i
      if (size(sysa(i)%sys).gt.0) then
        !           *** added: 02/29/12 *************************************
        !           sort groups, so that larger systems get associated first
        allocate( sysordered(maxsys(i)) )
        allocate( indsorted(maxsys(i)) )
        call sort (real(sysa(i)%sys(1:maxsys(i))%npoints), &
             maxsys(i),sysordered,indsorted,'d')
        sysa(i)%sys(1:maxsys(i)) = sysa(i)%sys(indsorted)
        if (allocated(sysordered)) deallocate(sysordered)
        if (allocated(indsorted)) deallocate(indsorted)
        !           *** added: 02/29/12 *************************************
        !           initialize groups                                             ! optimize?
        sysa(i)%sys(:)%grp = 9999                                     ! optimize?
        counter = 0
        leng = length(real(sysmem(:)%grp), &
             size(sysmem(:)%grp),real(9999))
        allocate( alreadyused(leng+10) )                              !make space for 10 new potential entries. improve!!!
        write(20,*) 'sysmem(1:leng)%grp =', &
             sysmem(1:leng)%grp
        allocate( allind(leng) )
        alreadyused(:) = 0
        allind(:) = sysmem(1:leng)%grp
        !071212-----gof 2d-------------------------------
        allocate( ind(size(allind)) )
        ind(:) = allind
        allocate( ind2(size(ind)) )
        do ii = 1, size(ind)
          ind2(ii) = findfirst(real(allind),size(allind), &
               real(ind(ii)))
        end do
        !        define 2d array for evaluating best fit for systems
        allocate( gof(maxsys(i),maxgroup) )
        allocate( gofminval(maxgroup) )
        allocate( gofminind(maxgroup) )
        allocate( tbsysmem(maxgroup) )
        allocate( deltadirsysmem(maxgroup) )
        allocate( deltapersysmem(maxgroup) )
        allocate( m1sysmem(maxgroup) )
        allocate( m2sysmem(maxgroup) )
        !071212-----gof 2d-------------------------------
        do j = 1, maxsys(i)
          npnts = sysa(i)%sys(j)%npoints
          lonmean = sum(sysa(i)%sys(j)%lon(1:npnts))/npnts
          latmean = sum(sysa(i)%sys(j)%lat(1:npnts))/npnts
          !070512----------- weight averages with hm0 ---------------------
          temp1 = 0.
          temp2 = 0.
          do iii = 1,npnts
            temp1 = temp1 + &
                 (sysa(i)%sys(j)%hs(iii)**2)*sysa(i)%sys(j)%lon(iii)
            temp2 = temp2 + &
                 (sysa(i)%sys(j)%hs(iii)**2)*sysa(i)%sys(j)%lat(iii)
          end do
          lonmean=temp1/max(sum(sysa(i)%sys(j)%hs(1:npnts)**2),0.001)
          latmean=temp2/max(sum(sysa(i)%sys(j)%hs(1:npnts)**2),0.001)
          !070512----------- weight averages with hm0 ---------------------
          !071012----------- grid point indexing --------------------------
          allocate(sysa(i)%sys(j)%indx(maxi*maxj))
          sysa(i)%sys(j)%indx = 9999
          do iii = 1,sysa(i)%sys(j)%npoints
            sysa(i)%sys(j)%indx(iii) = &
                 (sysa(i)%sys(j)%j(iii)-1)*maxi + &
                 sysa(i)%sys(j)%i(iii)
          end do
          !071012----------- grid point indexing --------------------------
          write(20,*) 'system no. ',j,' of ',maxsys(i)
          write(20,*) 'size =', npnts
          write(20,*) 'lonmean =', lonmean
          write(20,*) 'latmean =', latmean
          write(20,*) 'tpmean =', sysa(i)%sys(j)%tpmean
          write(20,*) 'dirmean =', sysa(i)%sys(j)%dirmean
          sysa(i)%sys(j)%grp = 9999                                     !now redundant?
          !           compute deltas
          tbsysmem = sysmem(1:maxgroup)%tpmean
          write(20,*) 'tbsysmem(:) =       ', tbsysmem(:)
          !           compute deltas the same way as for field combining - they should
          !           be of the same degree of strictness as the latter, otherwise
          !           the time combining will lose track!
          !3stddev            m1 = -3.645*tb + 63.211
          !3stddev            m1 = max(m1,10.)
          !3stddev            m2 = -0.346*tb + 3.686
          !3stddev            m2 = max(m2,0.6)
          !1stddev            m1 = -2.219*tb + 35.734
          !1stddev            m1 = max(m1,5.)
          !1stddev            m2 = -0.226*tb + 2.213
          !1stddev            m2 = max(m2,0.35)
          !071412            m1 = -5.071*tb + 90.688
          !071412            m1 = max(m1,16.)
          !071412            m2 = -0.467*tb + 5.161
          !071412            m2 = max(m2,1.0)
          !071412            deltadir = (m1*1. + dirtimeknob)*1.
          !071412            deltaper = (m2*1. + tptimeknob)*1.
          do ii = 1,size(ind2)
            m1sysmem(ii) = max((-3.645*tbsysmem(ii)+63.211),10.)
            m2sysmem(ii) = max((-0.346*tbsysmem(ii)+3.686),0.6)
          end do
          deltadirsysmem = m1sysmem(:)*1. + dirtimeknob
          deltapersysmem = m2sysmem(:)*1. + tptimeknob
          write(20,*) 'deltadirsysmem(:) = ',deltadirsysmem
          write(20,*) 'deltapersysmem(:) = ',deltapersysmem
          !                 criterion 1: mean period
          allocate( temp(size(ind2)) )
          temp = abs( sysa(i)%sys(j)%tpmean - &
               sysmem(ind2(:))%tpmean )
          write(20,*) 'tpmean list =', &
               sysmem(ind2(:))%tpmean
          write(20,*) 'tpminval list =', temp
          tpminval = minval(temp)
          tpminind = findfirst(temp,size(temp),tpminval)
          !                 criterion 2: mean direction
          allocate( dirs(size(ind2)) )
          dirs(:)=abs( sysa(i)%sys(j)%dirmean - &
               sysmem(ind2(:))%dirmean )
          !                 deal with wrap around
          do idir = 1, size(dirs)
            if (dirs(idir).ge.180.) dirs(idir)=360-dirs(idir)
          end do
          write(20,*) 'dirmean list =', &
               sysmem(ind2(:))%dirmean
          write(20,*) 'dirminval list =', dirs
          !                 criterion 3: size
          write(20,*) 'size list =', &
               sysmem(ind2(:))%npoints
          !                 criterion 4: distance between systems
          allocate (mnlonlist(size(ind2)))
          allocate (mnlatlist(size(ind2)))
          allocate (mndist(size(ind2)))
          do ii = 1,size(ind2)
            mnlonlist(ii) = sysmem(ind2(ii))%lonmean
            mnlatlist(ii) = sysmem(ind2(ii))%latmean
            mndist(ii) = sqrt((lonmean-mnlonlist(ii))**2 + &
                 (latmean-mnlatlist(ii))**2)
          end do
          dmndiag = sqrt(lonext**2+latext**2)
          write(20,*) 'distance list =',mndist(:)
          write(20,*) 'domain diagonal =',dmndiag
          !                 criterion 5: overlap of systems
          allocate (olsize(size(ind2)))
          do ii = 1,size(ind2)
            if (sysmem(ind2(ii))%npoints.gt.0) then
              call intersect(real(sysa(i)%sys(j)%indx(1:npnts)),npnts, &
                   real(sysmem(ind2(ii))%indx(1:sysmem(ind2(ii))%npoints)),&
                   sysmem(ind2(ii))%npoints,dummy1,olsize(ii),dummy2,dummy3)
            else
              olsize(ii) = 0
            end if
          end do
          gof(j,1:size(ind2)) = (temp/deltapersysmem(:))**2 + &
               (dirs/deltadirsysmem(:))**2 + &
               !                    (4*mndist(:)/dmndiag)**2
               ( (real(olsize(:)) - &
               real(sysmem(ind2(:))%npoints) )/&
               (0.50*max(real(sysmem(ind2(:))%npoints),0.001)) )**2
          !                 remove gof entries which exceed predifined tolerances
          do ii = 1,size(ind2)
            write(20,*) 'testing: ii,olsize(ii),size,frac =',&
                 ii,olsize(ii),sysmem(ind2(ii))%npoints,&
                 real(olsize(ii))/&
                 max(real(sysmem(ind2(ii))%npoints),0.001)
            if ( real(olsize(ii)).lt.&
                 0.50*real(sysmem(ind2(ii))%npoints) ) then
              gof(j,ii) = 9999.
            end if
            if ( (temp(ii).gt.deltapersysmem(ii)).or.&
                 (dirs(ii).gt.deltadirsysmem(ii)) ) then
              gof(j,ii) = 9999.
            end if
          end do
          write(20,*) 'gof(j,:) =',gof(j,:)
          if (allocated(temp)) deallocate(temp)
          if (allocated(dirs)) deallocate(dirs)
          if (allocated(mnlonlist)) deallocate(mnlonlist)
          if (allocated(mnlatlist)) deallocate(mnlatlist)
          if (allocated(mndist)) deallocate(mndist)
          if (allocated(olsize)) deallocate(olsize)
          !071212-----------gof 2d-------------
        end do
        if (allocated(tbsysmem)) deallocate(tbsysmem)
        if (allocated(deltadirsysmem)) deallocate(deltadirsysmem)
        if (allocated(deltapersysmem)) deallocate(deltapersysmem)
        if (allocated(m1sysmem)) deallocate(m1sysmem)
        if (allocated(m2sysmem)) deallocate(m2sysmem)
        write(20,*) 'gof3:'
        do jj = 1,maxsys(i)
          write(20,*) gof(jj,:)
        end do
        !        find minima in gof
        do k = 1,maxgroup
          gofminval(k) = minval(gof(:,k))
          gofminind(k) = findfirst(gof(:,k),size(gof,1),gofminval(k))
          if (gofminval(k).eq.9999) then
            gofminind(k) = 0
          end if
        end do
        if (allocated(gof)) deallocate(gof)
        do j = 1, maxsys(i)
          new = 0
          !                 look up sysmem match for this current system. if no match
          !                 is found, the index value 0 is returned.
          tpminind = 0
          temp1 = 9999.
          do jj = 1, size(gofminind)
            if (gofminind(jj).eq.j) then
              if (gofminval(jj).lt.temp1) then
                tpminind = jj
                temp1 = gofminval(jj)
              end if
            end if
          end do
          dirminind = tpminind
          write(20,*) 'system, gofminind: ',j,tpminind
          if (tpminind.ne.0) then
            !                    success
            !071212-----------gof 2d-------------
            counter = counter+1
            sysa(i)%sys(j)%grp = &
                 sysmem(ind2(dirminind))%grp
            alreadyused(counter) = sysa(i)%sys(j)%grp
            write(20,*) 'case 1: matched this ts (',i, &
                 ') sys ',sysa(i)%sys(j)%sysind,' (tp=', &
                 sysa(i)%sys(j)%tpmean,' dir=', &
                 sysa(i)%sys(j)%dirmean,') with grp ', &
                 sysmem(ind2(dirminind))%grp
            write(20,*) 'added ',alreadyused(counter), &
                 ' in array *alreadyused*'
          else
            new = 1
          end if
          if (new.eq.1) then
            used = 0
            do k = 1, maxgroup
              ok = 1
              write(20,*) 'maxgroup,k,ok,used =', &
                   maxgroup,k,ok,used
              !                       make sure it hasn't been used yet (at current time level)
              if ((i.gt.2).and. &
                   (.not.any(alreadyused(:).eq.k))) then
                !                           make sure it hasn't been used yet (at previous time level)
                do l = 1, maxgroup
                  !                              if last update of system was more that *6* time steps
                  !                              ago, system can be released (to calibrate)
                  if ( (sysmem(l)%grp.eq.k).and. &
                       ((i-sysmem(l)%updated).lt.6) ) ok = 0
                  write(20,*) 'l, ok = ',l,ok
                end do
                if (ok.eq.1) then
                  sysa(i)%sys(j)%grp = k
                  counter = counter+1;
                  alreadyused(counter) = k
                  used = 1
                  write(20,*) 'k,used,counter =', &
                       k,used,counter
                  exit
                end if
              end if
            end do
            if (used.eq.0) then
              maxgroup = maxgroup+1
              sysa(i)%sys(j)%grp = maxgroup
              !                       increase sysmem by one slot
              sysmem(maxgroup)%grp = maxgroup
              counter = counter+1
              alreadyused(counter) = maxgroup
            end if
            write(20,*) 'counter,maxgroup,sysa(i)%sys(j)%grp =',&
                 counter,maxgroup,sysa(i)%sys(j)%grp
            write(20,*) 'no grp match case 2'
          end if
        end do
        if (allocated(ind)) deallocate(ind)                  !071212 shifted
        if (allocated(ind2)) deallocate(ind2)                !071212 shifted
        if (allocated(gofminval)) deallocate(gofminval)
        if (allocated(gofminind)) deallocate(gofminind)
        if (allocated(alreadyused)) deallocate(alreadyused)
        if (allocated(allind)) deallocate(allind)
        !        update sysmem
        do k = 1, maxgroup
          do kk = 1, maxsys(i)
            if (sysa(i)%sys(kk)%grp.eq.sysmem(k)%grp) then
              sysmem(k)%npoints = sysa(i)%sys(kk)%npoints
              sysmem(k)%lonmean = &
                   sum(sysa(i)%sys(kk)%lon(1:sysmem(k)%npoints))/&
                   sysmem(k)%npoints
              sysmem(k)%latmean = &
                   sum(sysa(i)%sys(kk)%lat(1:sysmem(k)%npoints))/&
                   sysmem(k)%npoints
              !070512----------- weight averages with hm0 ---------------------
              temp1 = 0.
              temp2 = 0.
              do iii = 1,sysmem(k)%npoints
                temp1 = temp1 + &
                     (sysa(i)%sys(kk)%hs(iii)**2)*sysa(i)%sys(kk)%lon(iii)
                temp2 = temp2 + &
                     (sysa(i)%sys(kk)%hs(iii)**2)*sysa(i)%sys(kk)%lat(iii)
              end do
              sysmem(k)%lonmean = temp1/&
                   max(sum(sysa(i)%sys(kk)%hs(1:sysmem(k)%npoints)**2),&
                   0.001)
              sysmem(k)%latmean = temp2/&
                   max(sum(sysa(i)%sys(kk)%hs(1:sysmem(k)%npoints)**2),&
                   0.001)
              !070512----------- weight averages with hm0 ---------------------
              sysmem(k)%tpmean = sysa(i)%sys(kk)%tpmean
              sysmem(k)%dirmean = sysa(i)%sys(kk)%dirmean
              !071012----------- grid point indexing --------------------------
              sysmem(k)%indx(:) = 9999
              do iii = 1,sysmem(k)%npoints
                sysmem(k)%indx(iii) = &
                     (sysa(i)%sys(kk)%j(iii)-1)*maxi + &
                     sysa(i)%sys(kk)%i(iii)
              end do
              !071012----------- grid point indexing --------------------------
              sysmem(k)%updated = i
              sysmem(k)%length = sysmem(k)%length + 1
            end if
          end do
          !test for expired groups
          if ((i-sysmem(k)%updated).ge.6) then
            sysmem(k)%npoints = 0
            sysmem(k)%lonmean = 9999.
            sysmem(k)%latmean = 9999.
            sysmem(k)%tpmean = 9999.
            sysmem(k)%dirmean = 9999.
            sysmem(k)%indx(:) = 9999
            sysmem(k)%updated = -9999
            sysmem(k)%length = 0
          end if
        end do
        !083012  !filter out duplicates groups that can develop
        do l = 1, maxgroup
          do ll = (l+1), maxgroup
            deltadir = max((-3.645*sysmem(l)%tpmean+63.211),10.)*1.
            deltaper = max((-0.346*sysmem(l)%tpmean+3.686),0.6)*1.
            if ( (abs(sysmem(l)%tpmean-sysmem(ll)%tpmean).lt.&
                 deltaper).and. &
                 (abs(sysmem(l)%dirmean-sysmem(ll)%dirmean).lt.&
                 deltadir).and. &
                 (sysmem(l)%updated.ne.sysmem(ll)%updated).and. &
                 (sysmem(ll)%npoints.ne.0) ) then
              !find the more recent entry, and delete from group
              if (sysmem(ll)%length.lt.sysmem(l)%length) then
                idup = ll
                write(20,*) 'deleting memgroup ',ll, &
                     '(updated',sysmem(ll)%updated,', length', &
                     sysmem(ll)%length,'), duplicate of memgroup', &
                     l,'(updated',sysmem(l)%updated,', length', &
                     sysmem(l)%length,'):'
              else
                idup = l
                write(20,*) 'deleting memgroup ',l, &
                     '(updated',sysmem(l)%updated,', length', &
                     sysmem(l)%length,'), duplicate of memgroup', &
                     ll,'(updated',sysmem(ll)%updated,', length', &
                     sysmem(ll)%length,'):'
              end if
              write(20,*) 'deltaper, diff per:',deltaper,&
                   abs(sysmem(l)%tpmean-sysmem(ll)%tpmean)
              write(20,*) 'deltadir, diff dir:',deltadir,&
                   abs(sysmem(l)%dirmean-sysmem(ll)%dirmean)
              sysmem(idup)%npoints = 0
              sysmem(idup)%lonmean = 9999.
              sysmem(idup)%latmean = 9999.
              sysmem(idup)%tpmean = 9999.
              sysmem(idup)%dirmean = 9999.
              sysmem(idup)%indx(:) = 9999
              sysmem(idup)%updated = -9999
              sysmem(idup)%length = 0
            end if
          end do
        end do
      else
        write(20,*) '*** no systems at this time level. ', &
             'no. systems =',size(sysa(i)%sys)
        !test for expired groups
        do k = 1, maxgroup
          if ((i-sysmem(k)%updated).ge.6) then
            sysmem(k)%npoints = 0
            sysmem(k)%lonmean = 9999.
            sysmem(k)%latmean = 9999.
            sysmem(k)%tpmean = 9999.
            sysmem(k)%dirmean = 9999.
            sysmem(k)%indx(:) = 9999
            sysmem(k)%updated = -9999
            sysmem(k)%length = 0
          end if
        end do
      end if
      !        ******** test output ***********************
      do k = 1, maxgroup
        write(20,*) 'sysmem(',k,')%grp =',sysmem(k)%grp
        write(20,*) 'sysmem(',k,')%npoints =',sysmem(k)%npoints
        write(20,*) 'sysmem(',k,')%lonmean =',sysmem(k)%lonmean
        write(20,*) 'sysmem(',k,')%latmean =',sysmem(k)%latmean
        write(20,*) 'sysmem(',k,')%tpmean =',sysmem(k)%tpmean
        write(20,*) 'sysmem(',k,')%dirmean =',sysmem(k)%dirmean
        write(20,*) 'sysmem(',k,')%updated =',sysmem(k)%updated
        write(20,*) 'sysmem(',k,')%length =',sysmem(k)%length
      end do
      !        ********************************************
    end do
    !     write hotfile of wave groups
    open(unit=27,file='sys_restart1.ww3',status='unknown')
    write(27,'(a23,i10)') 'maxgroup             =',maxgroup
    do k = 1, maxgroup
      write(27,'(a8,i3,a12,i10)') 'sysmem( ',k, &
           ' )%grp     =',sysmem(k)%grp
      write(27,'(a8,i3,a12,i10)') 'sysmem( ',k, &
           ' )%npoints =',sysmem(k)%npoints
      write(27,'(a8,i3,a12,f10.4)') 'sysmem( ',k, &
           ' )%lonmean =',sysmem(k)%lonmean
      write(27,'(a8,i3,a12,f10.4)') 'sysmem( ',k, &
           ' )%latmean =',sysmem(k)%latmean
      write(27,'(a8,i3,a12,f10.3)') 'sysmem( ',k, &
           ' )%tpmean  =',sysmem(k)%tpmean
      write(27,'(a8,i3,a12,f10.3)') 'sysmem( ',k, &
           ' )%dirmean =',sysmem(k)%dirmean
      write(27,'(a8,i3,a12,i10)') 'sysmem( ',k, &
           ' )%updated =',sysmem(k)%updated
      write(27,'(a8,i3,a12,i10)') 'sysmem( ',k, &
           ' )%length  =',sysmem(k)%length
      do j = maxj,1,-1
        do i = 1,maxi
          write(27,'(i8)',advance='no') sysmem(k)%indx((j-1)*maxi+i)
        end do
        write(27,'(a)',advance='yes') ''
      end do
    end do
    close(27)
2000 continue
    return
  end subroutine timetrackingv2
  !/ end of timetrackingv2 --------------------------------------------- /
  !/
  !/ ------------------------------------------------------------------- /
  subroutine findway (way         ,horizstepcount,vertstepcount , &
       vertborder  ,horizborder   ,stepcount     )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |     a. j. van der westhuysen      |
    !/                  |            jeff hanson            |
    !/                  |        eve-marie devaliere        |
    !/                  |                        fortran 95 |
    !/                  | last update :          4-jan-2013 |
    !/                  +-----------------------------------+
    !/
    !/    03-feb-2012 : origination, based on matlab code   ( version 4.05 )
    !/                  by jeff hanson & eve-marie devaliere
    !/    04-jan-2013 : inclusion in trunk                  ( version 4.08 )
    !/
    !/    copyright 2009-2013 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    implicit none
    !
    !  1. purpose :
    !
    !     from the direction (way) we were going before, find which direction we
    !     are going now and how many 'steps' we need to take
    !
    !  2. method
    !
    !     -
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     way         char     in/out direction of spiral search
    !     vertborder  int      input
    !     horizborder int      input
    !     stepcount   int      output number of steps to go in the selected direction (way)
    !
    character   :: way *1
    integer     :: horizstepcount, vertstepcount, &
         vertborder, horizborder, stepcount
    intent (in) vertborder, horizborder
    intent (out) stepcount
    intent (in out) way
    !
    !     local variables
    !     ----------------------------------------------------------------
    !     -
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !     -
    !
    !  5. subroutines calling
    !
    !     spiraltrackv3
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see above
    !
    !  9. switches :
    !
    !     none defined yet.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    select case (way)
    case ('r')
      way='d'
      vertstepcount=vertstepcount+1
      if (horizborder.eq.1) then
        horizstepcount=horizstepcount-1
      end if
      stepcount=vertstepcount
    case ('d')
      way='l'
      horizstepcount=horizstepcount+1
      if (vertborder.eq.1) then
        vertstepcount=vertstepcount-1
      end if
      stepcount=horizstepcount
    case ('l')
      way='u'
      vertstepcount=vertstepcount+1
      if (horizborder.eq.1) then
        horizstepcount=horizstepcount-1
      end if
      stepcount=vertstepcount
    case ('u')
      way='r'
      horizstepcount=horizstepcount+1
      if (vertborder.eq.1) then
        vertstepcount=vertstepcount-1
      end if
      stepcount=horizstepcount
    case default
      write(20,*) 'in spatack:findway should not go here!'
    end select
    return
  end subroutine findway
  !/ end of findway ---------------------------------------------------- /
  !/
  !/ ------------------------------------------------------------------- /
  subroutine findnext (i      ,j          ,maxi       ,maxj     , &
       way    ,vertborder ,horizborder          )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |            jeff hanson            |
    !/                  |        eve-marie devaliere        |
    !/                  |                        fortran 95 |
    !/                  | last update :          4-jan-2013 |
    !/                  +-----------------------------------+
    !/
    !/    03-feb-2012 : origination, based on matlab code   ( version 4.05 )
    !/                  by jeff hanson & eve-marie devaliere
    !/    04-jan-2013 : inclusion in trunk                  ( version 4.08 )
    !/
    !/    copyright 2009-2013 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    implicit none
    !
    !  1. purpose :
    !
    !     find next point on spatial search spiral
    !
    !  2. method
    !
    !     -
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     i,j         int      in/out  current grid indices
    !     maxi, maxj  int      input   maximum indices of wave field
    !     way         char     input   direction of spiral search
    !     vertborder  int      output  flag indicating that vert domain edge has been hit
    !     horizborder int      output  flag indicating that hor domain edge has been hit
    !
    character   :: way
    integer     :: i, j, maxi, maxj, vertborder, horizborder
    intent (in) maxi, maxj, way
    intent (in out) i, j
    intent (out) vertborder, horizborder
    !
    !     local variables
    !     ----------------------------------------------------------------
    !     -
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !     -
    !
    !  5. subroutines calling
    !
    !     spiraltrackv3
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     -
    !
    !  9. switches :
    !
    !     none defined yet.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    vertborder=0
    horizborder=0
    select case (way)
    case ('r')
      if (i.lt.maxi) then
        i=i+1
      else
        !                 need to tell findway that if we hit the border we don't
        !                 increment stepcount...
        horizborder=1
      end if
    case ('d')
      if (j.gt.1) then
        j=j-1
      else
        vertborder=1
      end if
    case ('l')
      if (i.gt.1) then
        i=i-1
      else
        horizborder=1
      end if
    case ('u')
      if (j.lt.maxj) then
        j=j+1
      else
        vertborder=1
      end if
    end select
    return
  end subroutine findnext
  !/ end of findnext --------------------------------------------------- /
  !/
  !/ ------------------------------------------------------------------- /
  subroutine findsys (i        ,j        ,wsdat    ,maxsys    , &
       ngbrext  ,maxi     ,maxj     ,perknob   , &
       dirknob  ,hsknob                           )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |     a. j. van der westhuysen      |
    !/                  |            jeff hanson            |
    !/                  |        eve-marie devaliere        |
    !/                  |                        fortran 95 |
    !/                  | last update :          4-jan-2013 |
    !/                  +-----------------------------------+
    !/
    !/    03-feb-2012 : origination, based on matlab code   ( version 4.05 )
    !/                  by jeff hanson & eve-marie devaliere
    !/    04-jan-2013 : inclusion in trunk                  ( version 4.08 )
    !/
    !/    copyright 2009-2013 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    implicit none
    !
    !  1. purpose :
    !
    !     find all wave systems that neighbour the grid point (i,j), and
    !     match these with the systems at (i,j).
    !
    !  2. method
    !
    !     for the given point (i,j), find all wave systems at neighbouring grid
    !     points within the reach specified by ngbrext.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     i,j         int      input   current grid indices
    !     maxi, maxj  int      input   maximum indices of wave field
    !     wsdat   type(dat2d)  in/out  input data structure to be spiral tracked
    !     maxsys      int      in/out  maximum number of systems identified
    !
    type(dat2d) :: wsdat
    integer     :: i, j, maxi, maxj, ngbrext, maxsys
    real        :: perknob ,dirknob, hsknob
    intent (in) i, j, maxi, maxj, ngbrext, perknob ,dirknob
    intent (in out) wsdat, maxsys
    !
    !     local variables
    !     ----------------------------------------------------------------
    !     tmpsys  type(system)   temporary instance of the wave system variable
    !     nngbr       int        number of neighbours found
    !
    type(system), allocatable :: tmpsys(:)
    type(neighbr) :: ngbr(50)
    type(mtchsys) :: match
    logical       :: found
    integer       :: counter, ii, jj, nngbr, startcount, endcount, l,&
         nout, maxs, s, p, n, countall, ind, minind, &
         npart, pp, leng
    integer       :: allfullsys(50)
    real, pointer :: realarr(:)
    integer, allocatable :: allsys(:)
    real         :: hsall(50),tpall(50),dirall(50),gof(50)
    real         :: absdir,absper,abshs,t,&
         deltaper,deltadir,deltahs,temp
    real         :: dx, m1, m2
    real         :: gofminval
    integer      :: gofminind
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      unique
    !      combinepartitionsv2
    !
    !  5. subroutines calling
    !
    !     spiraltrackv3
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     -
    !
    !  9. switches :
    !
    !     none defined yet.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    nullify(realarr)
    !      write(20,*) 'findsys: i,j,maxsys =',i,j,maxsys
    !     first find the checked neighbor
    counter=1
    do ii=(i-ngbrext), (i+ngbrext)
      do jj=(j-ngbrext), (j+ngbrext)
        if ( (ii.gt.0).and.(jj.gt.0).and. &
             (jj.le.maxj).and.(ii.le.maxi) ) then
          if ( wsdat%par(ii,jj)%checked.eq.1 ) then
            ngbr(counter)%par = wsdat%par(ii,jj)                !added the par field to maintain the data structure
            ngbr(counter)%i = ii
            ngbr(counter)%j = jj
            counter=counter+1
          end if
        end if
      end do
    end do
    !     new variable nngbr
    nngbr=counter-1
    if (nngbr.gt.0) then
      allfullsys(:) = 0
      startcount=1
      l=1
      do while (l.le.nngbr)
        leng = length(real(ngbr(l)%par%sys), &
             size(ngbr(l)%par%sys),real(9999))
        endcount = startcount+leng-1
        allfullsys(startcount:endcount) = ngbr(l)%par%sys(1:leng)
        startcount=endcount+1
        l=l+1
      end do
      if (endcount.eq.0) write(20,*) '***1.calling unique w. len=0!'
      call unique (real(allfullsys),endcount,realarr,nout)            !can one do this?
      allocate(allsys(nout))
      allsys = int(realarr)                                           !can one do this?
      if (associated(realarr)) deallocate(realarr)
      maxs = maxval(allsys)
      if (maxsys.lt.maxs) then
        maxsys=maxs
      end if
      !         initiate sys num
      allocate( tmpsys(size(allsys)) )
      !         clear the wsdat%par(i,j)%sys field, new values assigned below.
      !         system info temporarily stored in allsys
      wsdat%par(i,j)%sys(1:10) = 9999
      do s=1, size(allsys)
        hsall(:) = 0.
        tpall(:) = 0.
        dirall(:) = 0.
        !              wfall(:) = 0.
        n=1
        countall=0
        do while (n.le.nngbr)
          !                calculate mean of common neighbor wave system
          !                for every neigbor wave system
          found = .false.
          do ind = 1, size(ngbr(n)%par%sys)                       !optimize this?
            if ( ngbr(n)%par%sys(ind).eq.allsys(s) ) then        !put sys under par to maintain structure
              found = .true.
              exit
            end if
          end do
          if (found) then
            countall=countall+1
            hsall(countall)=ngbr(n)%par%hs(ind)
            tpall(countall)=ngbr(n)%par%tp(ind)
            dirall(countall)=ngbr(n)%par%dir(ind)
            !                     wfall(countall)=ngbr(n)%par%wf(ind)
          else
            n=n+1
            cycle
          end if
          n=n+1
        end do
        tmpsys(s)%hsmean = sum(hsall(1:countall))/countall
        tmpsys(s)%tpmean = sum(tpall(1:countall))/countall
        tmpsys(s)%dirmean = &
             mean_anglev2(dirall(1:countall),countall)
        !              tmpsys(s)%wfmean = sum(wfall(1:countall))/countall
      end do
      !         find the partition at current (i,j) point that matches previously
      !         identified wave systems if any...
      wsdat%par(i,j)%ngbrsys(1:size(allsys)) = allsys
      npart = length(real(wsdat%par(i,j)%ipart), &
           size(wsdat%par(i,j)%ipart),real(0))
      do p = 1, npart
        if ( (wsdat%par(i,j)%hs(p).lt.hsknob).or. &
             (wsdat%par(i,j)%tp(p).eq.0.) ) then
          wsdat%par(i,j)%sys(p)=-1
          cycle
        end if
        ind=0                                                       !replaced 'index' by 'ind'
        match%sysval(:) = 9999
        match%tpval(:) = 9999.
        match%dirval(:) = 9999.
        !              match%wfval(:) = 9999.
        !             cycle through the neighbouring systems identified above
        do s=1,size(allsys)
          abshs = abs(wsdat%par(i,j)%hs(p)-tmpsys(s)%hsmean)
          absper = abs(wsdat%par(i,j)%tp(p)-tmpsys(s)%tpmean)
          absdir = abs(wsdat%par(i,j)%dir(p)-tmpsys(s)%dirmean)
          !                  abswf = abs(wsdat%par(i,j)%wf(p)-tmpsys(s)%wfmean)
          if (absdir.gt.180) then
            absdir = 360 - absdir
            if (absdir.lt.0) then
              write(20,*) '*** warning: absdir negative!'
              write(20,*) 'wsdat%par(i,j)%dir(p) =', &
                   wsdat%par(i,j)%dir(p)
              write(20,*) 'tmpsys(s)%dirmean) =', &
                   tmpsys(s)%dirmean
            end if
          end if
          !                 calculate delta dir and freq as a function of the partition
          !                 dir and freq
          t = tmpsys(s)%tpmean
          dx = 0.5*( (wsdat%lon(2,1)-wsdat%lon(1,1)) + &
               (wsdat%lat(1,2)-wsdat%lat(1,1)) )
          m1 = -3.645*t + 63.211
          m1 = max(m1,10.)
          m2 = -0.346*t + 3.686
          m2 = max(m2,0.6)
          !1stddev                  m1 = -2.219*t + 35.734
          !1stddev                  m1 = max(m1,5.)
          !1stddev                  m2 = -0.226*t + 2.213
          !1stddev                  m2 = max(m2,0.35)
          !5stddev                  m1 = -5.071*t + 90.688
          !5stddev                  m1 = max(m1,16.)
          !5stddev                  m2 = -0.467*t + 5.161
          !5stddev                  m2 = max(m2,1.0)
          deltadir = m1*dx + dirknob
          deltaper = m2*dx + perknob
          deltahs = 0.25*tmpsys(s)%hsmean
          if ((absper.lt.deltaper).and.(absdir.lt.deltadir)) then
            ind=ind+1
            match%sysval(ind) = allsys(s)
            match%tpval(ind) = absper
            match%dirval(ind) = absdir
            match%hsval(ind) = abshs
            !                      match%wfval(ind) = abswf
          end if
        end do
        if (ind.gt.0) then
          if (ind.eq.1) then
            wsdat%par(i,j)%sys(p) = match%sysval(1)
          else
            !                    take the closest match, using gof function
            gof(:) = 9999.
            gof(1:ind) = (match%tpval(1:ind)/deltaper)**2 + &
                 (match%dirval(1:ind)/deltadir)**2 + &
                 (match%hsval(1:ind)/deltahs)**2
            gofminval = minval(gof(1:ind))
            gofminind = findfirst(gof(1:ind),ind,gofminval)
            wsdat%par(i,j)%sys(p) = match%sysval(gofminind)         !the index of the system is swapped - the remaining info stays the same!
          end if
        end if
      end do
    end if
    !     now check if 2 partitions have been associated to the same wave system, if
    !     so combine them
    npart = length(real(wsdat%par(i,j)%ipart), &
         size(wsdat%par(i,j)%ipart),real(0))
    do p = 1, (npart-1)                                                 !could probably be optimized!
      do pp = (p+1), npart
        if (wsdat%par(i,j)%sys(p).eq.wsdat%par(i,j)%sys(pp)) then
          !              there is at least one duplicate, so combine systems
          call combinepartitionsv2(wsdat%par(i,j))
        end if
      end do
    end do
    !     now that we have associated any possible partition to an existing
    !     wave system, we check if any wave system is free. if so give it a
    !     new wave system number
    npart = length(real(wsdat%par(i,j)%ipart), &
         size(wsdat%par(i,j)%ipart),real(0))
    do p = 1, npart
      if (wsdat%par(i,j)%sys(p).eq.9999) then
        maxsys = maxsys + 1
        wsdat%par(i,j)%sys(p) = maxsys
      end if
    end do
    wsdat%par(i,j)%checked=1
    if (allocated(allsys)) deallocate(allsys)
    if (allocated(tmpsys)) deallocate(tmpsys)
    return
  end subroutine findsys
  !/ end of findsys ---------------------------------------------------- /
  !/
  !/ ------------------------------------------------------------------- /
  subroutine combinewavesystems (wsdat   ,maxsys  ,maxpts   , &
       maxi    ,maxj    ,perknob  , &
       dirknob ,hsknob  ,combine  , &
       sys   )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |     a. j. van der westhuysen      |
    !/                  |            jeff hanson            |
    !/                  |        eve-marie devaliere        |
    !/                  |                        fortran 95 |
    !/                  | last update :          4-jan-2013 |
    !/                  +-----------------------------------+
    !/
    !/    03-feb-2012 : origination, based on matlab code   ( version 4.05 )
    !/                  by jeff hanson & eve-marie devaliere
    !/    04-jan-2013 : inclusion in trunk                  ( version 4.08 )
    !/
    !/    copyright 2009-2013 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    implicit none
    !
    !  1. purpose :
    !
    !     combine wave systems. then remove small and low-energy systems from set,
    !     based on the parameters maxpts and maxhgt.
    !
    !  2. method
    !
    !     -
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     wsdat   type(dat2d)  output  combined wave system data structure
    !     sys     type(system) output  final set of tracked systems, for one time level
    !     maxi, maxj  int      input   maximum indices of wave field
    !     maxsys      int      input   maximum number of systems identified
    !     maxpts      int      input   number of points req for valid system
    !     hsknob      real     input   parameter for identifying valid system
    !     combine     int      input   toggle: 1=combine systems; 0=do not combine
    type(dat2d) :: wsdat
    type(system), pointer :: sys(:), systemp(:)
    integer      :: maxsys, maxpts, maxi, maxj, combine
    real         :: perknob ,dirknob, hsknob
    intent (in) maxpts, maxi, maxj, hsknob, combine
    intent (in out) wsdat, maxsys                                       !in the matlab code maxsys is only input ???
    !      intent (out) sys
    !
    !     local variables
    !     ----------------------------------------------------------------
    !     nsys       int   number of wave systems (for checking iterative combining loop)
    !
    logical   :: found
    integer, allocatable :: sysout(:)
    integer, allocatable :: actsysind(:)
    integer   :: iter, ok, nsys, ms, s, so, ss, ind, leng, &
         iw, jw, iloop
    integer   :: actsys
    real      :: dev, hscmp, maxhgt, temp(5)
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      printfinalsys
    !      combinesys
    !
    !  5. subroutines calling
    !
    !     spiraltrackv3
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     -
    !
    !  9. switches :
    !
    !     none defined yet.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !012912      write(20,*) 'maxsys,maxpts,maxi,maxj,hsknob,combine =', &
    !012912                  maxsys,maxpts,maxi,maxj,hsknob,combine
    !     set up initial index array of active systems
    if (.not.allocated(actsysind)) allocate( actsysind(maxsys) )
    actsysind(1:maxsys) = (/ (ind, ind = 1, maxsys) /)
    !opt      write(20,*) 'actsysind =',actsysind
    if (combine.eq.1) then
      !         combine wave systems
      write(20,*) 'calling printfinalsys...'
      call printfinalsys (wsdat,maxsys,actsysind,maxi,maxj,1,sys)
      iter=0
      ok=0
      !         keep on combining wave systems until all possible combining
      !         has been carried out (based on the combining criteria)
      do while (ok.eq.0)
        iter = iter+1
        !             no of systems before combining
        if (allocated(actsysind)) then
          nsys = size(actsysind)
        else
          nsys = maxsys
        end if
        write(20,'(a,a,i3,a,i5,a)') 'calling combinesys for ', &
             'iteration',iter,' (maxsys =',nsys,').'
        !opt              write(20,*) 'size(sys)=',size(sys)
        call combinesys (wsdat,sys,maxsys,maxi,maxj, &
             actsysind,perknob,dirknob)
        !             no of systems after combining
        !opt              write(20,*) 'maxsys,nsys,size(actsysind) =', &
        !opt                           maxsys,nsys,size(actsysind)
        !              if (maxsys.eq.nsys) ok = 1
        if (size(actsysind).eq.nsys) ok = 1
      end do
    else
      !         do not combine wave systems
      call printfinalsys (wsdat,maxsys,actsysind,maxi,maxj,3,sys)
    end if
    !     remove small and low-energy systems from set, based on
    !     the parameters maxpts and maxhgt.
    !      allocate( sysout(maxsys) )
    !      sysout = sys(1:maxsys)%sysind
    !      ms = maxsys
    ms = size(actsysind)
    ss = 1
    write(20,*) 'filtering the set of',ms,'systems on size and mag.'
    do so = 1, ms
      s = actsysind(so)
      !opt      note: if we deallocate the individual records without
      !opt      compressing sys, then s and sysind will remain the same
      ss = s
      leng = length(sys(ss)%hs,size(sys(ss)%hs),9999.)
      dev  = std(sys(ss)%hs(1:leng),leng)
      hscmp = sys(ss)%hsmean + 2.*dev
      maxhgt = hsknob
      if ( (hscmp.lt.maxhgt).or.(sys(ss)%npoints.lt.maxpts) ) then
        !             remove system, and shift up indices to fill the gap
        do ind = 1, maxsys
          !                find index to remove
          if (ind.eq.ss) then
            !                   shift up entries, deleting the duplicate partition
            !                   replace with cshift(array, shift, dim)?
            !                    if (ind.lt.maxsys) &
            !                       sys( ind:(maxsys-1) ) = sys( (ind+1):maxsys )
            if (ind.le.maxsys) then
              !                      since we use pointers, we have to copy each index and
              !                      field individually. otherwise memory corruption occurs.
              do iloop = ind,ind
                sys(iloop)%sysind = 9999
                sys(iloop)%npoints = 0
                sys(iloop)%grp = 9999
                deallocate( sys(iloop)%hs )
                deallocate( sys(iloop)%tp )
                deallocate( sys(iloop)%dir )
                deallocate( sys(iloop)%dspr )
                !                          deallocate( sys(iloop)%wf )
                deallocate( sys(iloop)%i )
                deallocate( sys(iloop)%j )
                deallocate( sys(iloop)%lat )
                deallocate( sys(iloop)%lon )
                !                          deallocate( sys(iloop)%hsmean )
                !                          deallocate( sys(iloop)%tpmean )
                !                          deallocate( sys(iloop)%dirmean )
                !                          deallocate( sys(iloop)%ngbr )
              end do
            end if
          end if
        end do
        !             update wsdat as well
        do iw = 1, maxi
          do jw = 1, maxj
            leng = length(real(wsdat%par(iw,jw)%sys), &
                 size(wsdat%par(iw,jw)%sys),real(9999))
            ind = 1
            found = .false.
            !                    identify system index (there are no duplicate
            !                    systems at this point.
            do while (ind.le.leng)
              if ( wsdat%par(iw,jw)%sys(ind).eq.s ) then
                found = .true.
                exit
              end if
              ind = ind + 1
            end do
            if (found) then
              !                       blank out used record
              wsdat%par(iw,jw)%sys(ind) = 9999
              wsdat%par(iw,jw)%ipart(ind) = 9999
            end if
          end do
        end do
      end if
    end do
    !     compile array index of active systems in sys
    actsys = 0
    do so = 1,maxsys
      if (sys(so)%npoints>0) actsys = actsys + 1
    end do
    if (allocated(actsysind)) deallocate(actsysind)
    allocate( actsysind(actsys) )
    actsys = 0
    do so = 1,maxsys
      if (sys(so)%npoints>0) then
        actsys = actsys + 1
        actsysind(actsys) = sys(so)%sysind
      end if
    end do
    !opt      write(20,*) 'actsysind =',actsysind
    do so = 1,size(actsysind)
      s = actsysind(so)
      !opt         write(20,*) 'sys(',s,')%sysind =',sys(s)%sysind
    end do
    call printfinalsys (wsdat,maxsys,actsysind,maxi,maxj,1,sys)
    !opt      write(20,*) 'actsysind =',actsysind
    !opt      do so = 1,maxsys
    !opt         write(20,*) 'sys(',so,')%sysind =',sys(so)%sysind, &
    !opt                     ',  sys(',so,')%npoints =',sys(so)%npoints
    !opt      end do
    return
  end subroutine combinewavesystems
  !/ end of combinewavesystems ----------------------------------------- /
  !/
  !/ ------------------------------------------------------------------- /
  subroutine printfinalsys (wsdat    ,maxsys    ,actsysind , &
       maxi     ,maxj      ,flag      ,sys    )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |     a. j. van der westhuysen      |
    !/                  |            jeff hanson            |
    !/                  |        eve-marie devaliere        |
    !/                  |                        fortran 95 |
    !/                  | last update :          4-jan-2013 |
    !/                  +-----------------------------------+
    !/
    !/    03-feb-2012 : origination, based on matlab code   ( version 4.05 )
    !/                  by jeff hanson & eve-marie devaliere
    !/    04-jan-2013 : inclusion in trunk                  ( version 4.08 )
    !/
    !/    copyright 2009-2013 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    implicit none
    !
    !  1. purpose :
    !
    !     output (print) the final output systems for this time step.
    !
    !  2. method
    !
    !     -
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     wsdat   type(dat2d)  input   combined data structure
    !     maxi, maxj  int      input   maximum indices of wave field
    !     maxsys      int      input   maximum number of systems identified
    !     flag        int      input   flag for printing system
    !     sys     type(system) output  final set of tracked systems, for one time level
    !
    type(dat2d)  :: wsdat
    type(system), pointer :: sys(:)
    integer      :: maxsys, maxi, maxj, flag
    integer, allocatable :: actsysind(:)
    intent (in) wsdat, actsysind, maxi, maxj, flag
    intent (out) maxsys
    !      intent (in out) sys
    !
    !     local variables
    !     ----------------------------------------------------------------
    !     ic        int   counter for wave systems
    !
    integer   :: ic, nguys, startind, endind, i, j, ind, leng, leng2
    integer   :: unisize, difsize
    real, allocatable :: sysordered(:)
    real, pointer :: uniarr(:), difarr(:)
    integer, allocatable :: ngbrsysall(:), syssortedind(:)
    real      :: temp(2), temp1, temp2
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      unique
    !      setdiff
    !      sort
    !
    !  5. subroutines calling
    !
    !     combinewavesystems
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     -
    !
    !  9. switches :
    !
    !     none defined yet.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !     initialize sys structure
    if (flag.ne.2) then
      !         allocate data structure with the final wave systems
      write(20,*) 'in printfinalsys...'
      maxsys = size(actsysind)
      nullify(sys)
      allocate( sys(maxsys) )
      write(20,*) 'allocated sys okay, size(sys) =',size(sys)
      allocate( ngbrsysall(50*maxi*maxj) )                            !large enough?
      do ic = 1, maxsys
        nullify( sys(ic)%hs )
        nullify( sys(ic)%tp )
        nullify( sys(ic)%dir )
        nullify( sys(ic)%dspr )
        !              nullify( sys(ic)%wf )
        nullify( sys(ic)%i )
        nullify( sys(ic)%j )
        nullify( sys(ic)%lat )
        nullify( sys(ic)%lon )
        allocate( sys(ic)%hs(maxi*maxj) )
        allocate( sys(ic)%tp(maxi*maxj) )
        allocate( sys(ic)%dir(maxi*maxj) )
        allocate( sys(ic)%dspr(maxi*maxj) )
        !              allocate( sys(ic)%wf(maxi*maxj) )
        allocate( sys(ic)%i(maxi*maxj) )
        allocate( sys(ic)%j(maxi*maxj) )
        allocate( sys(ic)%lat(maxi*maxj) )
        allocate( sys(ic)%lon(maxi*maxj) )
        sys(ic)%hs(:) = 9999.                                       !optimize this further?
        sys(ic)%tp(:) = 9999.
        sys(ic)%dir(:) = 9999.
        sys(ic)%dspr(:) = 9999.
        !              sys(ic)%wf(:) = 9999.
        sys(ic)%i(:) = 9999
        sys(ic)%j(:) = 9999
        sys(ic)%lat(:) = 9999.
        sys(ic)%lon(:) = 9999.
        sys(ic)%sysind = 9999
        sys(ic)%hsmean = 9999.
        sys(ic)%tpmean = 9999.
        sys(ic)%dirmean = 9999.
        sys(ic)%npoints = 0
        sys(ic)%ngbr(:) = 9999
        sys(ic)%grp = 9999
        ngbrsysall(:) = 0
        startind=1
        nguys=0
        do i = 1, maxi
          do j = 1, maxj
            !                     ind=wsdat.par(i,j).sys==ic;
            do ind = 1, size(wsdat%par(i,j)%sys)                !40.81 !optimize this?
              if (wsdat%par(i,j)%sys(ind).eq.actsysind(ic)) &
                   then
                nguys=nguys+1
                sys(ic)%hs(nguys)=wsdat%par(i,j)%hs(ind)
                sys(ic)%tp(nguys)=wsdat%par(i,j)%tp(ind)
                sys(ic)%dir(nguys)=wsdat%par(i,j)%dir(ind)
                sys(ic)%dspr(nguys)=wsdat%par(i,j)%dspr(ind)
                !                           sys(ic)%wf(nguys)=wsdat%par(i,j)%wf(ind)
                sys(ic)%i(nguys)=i
                sys(ic)%j(nguys)=j
                sys(ic)%lat(nguys)=wsdat%lat(i,j)
                sys(ic)%lon(nguys)=wsdat%lon(i,j)
                leng = length(real(wsdat%par(i,j)%ngbrsys), &
                     size(wsdat%par(i,j)%ngbrsys),real(9999))
                endind = startind + leng-1
                ngbrsysall(startind:endind) = &
                     wsdat%par(i,j)%ngbrsys(1:leng)
                startind=endind+1
              end if
            end do
          end do
        end do
        !              if ~isempty(sys)
        if (nguys.gt.0) then
          sys(ic)%sysind=ic
          sys(ic)%hsmean = sum(sys(ic)%hs(1:nguys))/nguys
          sys(ic)%tpmean = sum(sys(ic)%tp(1:nguys))/nguys
          !                 sys(ic)%dirmean=mean_angle_single(sys(ic).dir)          40.81 replaced with two-argument mean_anglev2
          sys(ic)%dirmean = &
               mean_anglev2(sys(ic)%dir(1:nguys),nguys)
          !070512----------- weight averages with hm0 ---------------------
          temp1 = 0.
          temp2 = 0.
          do i = 1,nguys
            temp1 = temp1 + (sys(ic)%hs(i)**2)*sys(ic)%hs(i)
            temp2 = temp2 + (sys(ic)%hs(i)**2)*sys(ic)%tp(i)
          end do
          sys(ic)%hsmean = &
               temp1/max(sum(sys(ic)%hs(1:nguys)**2),0.001)
          sys(ic)%tpmean = &
               temp2/max(sum(sys(ic)%hs(1:nguys)**2),0.001)
          sys(ic)%dirmean = mean_anglev3(sys(ic)%dir(1:nguys), &
               sys(ic)%hs(1:nguys),nguys)
          !070512----------- weight averages with hm0 ---------------------
          sys(ic)%npoints = nguys
          if (endind.gt.0) then
            call unique(real(ngbrsysall(1:endind)),endind, &
                 uniarr,unisize)
            temp = (/real(sys(ic)%sysind),real(sys(ic)%sysind)/)
            call setdiff(real(uniarr),unisize, &
                 temp,2,difarr,difsize)
            difsize = min(difsize,size(sys(ic)%ngbr))
            sys(ic)%ngbr(1:difsize) = nint(difarr(1:difsize))
            if (associated(uniarr)) deallocate(uniarr)
            if (associated(difarr)) deallocate(difarr)
          end if
        else
          cycle
        end if
      end do
      if (allocated(ngbrsysall)) deallocate(ngbrsysall)
    end if
    !     print the sorted field to the screen
    leng = length(real(sys(:)%npoints), &
         size(sys(:)%npoints),real(9999))
    allocate( sysordered(leng) )
    allocate( syssortedind(leng) )
    call sort (real(sys(:)%npoints),leng, &
         sysordered,syssortedind,'d')
    leng = length(real(sysordered), &
         size(sysordered),real(0))
    do ic = 1, leng
      leng2 = length(real(sys(syssortedind(ic))%ngbr), &
           size(sys(syssortedind(ic))%ngbr),real(9999))
    end do
    if (allocated(sysordered)) deallocate(sysordered)
    if (allocated(syssortedind)) deallocate(syssortedind)
    return
  end subroutine printfinalsys
  !/ end of printfinalsys ---------------------------------------------- /
  !/
  !/ ------------------------------------------------------------------- /
  subroutine combinesys (wsdat    ,sys      ,maxsys   ,maxi     , &
       maxj     ,actsysind,perknob  ,dirknob  )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |     a. j. van der westhuysen      |
    !/                  |            jeff hanson            |
    !/                  |        eve-marie devaliere        |
    !/                  |                        fortran 95 |
    !/                  | last update :          4-jan-2013 |
    !/                  +-----------------------------------+
    !/
    !/    03-feb-2012 : origination, based on matlab code   ( version 4.05 )
    !/                  by jeff hanson & eve-marie devaliere
    !/    04-jan-2013 : inclusion in trunk                  ( version 4.08 )
    !/
    !/    copyright 2009-2013 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    implicit none
    !
    !  1. purpose :
    !
    !     combine wave systems
    !
    !  2. method
    !
    !     -
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     wsdat   type(dat2d)  input   combined data structure
    !     maxi, maxj  int      input   maximum indices of wave field
    !     sys     type(system) output  final set of tracked systems, for one time level
    !     maxsys      int      input  number of systems
    !     dirknob     real     input  parameter in direction for combining fields in space
    !     perknob     real     input  parameter in period for combining fields in space
    !
    type(dat2d)  :: wsdat                                               !40.par
    type(system), pointer :: sys(:)                                     !40.par
    integer      :: maxsys, maxi, maxj                                  !40.par
    integer, allocatable :: actsysind(:)
    real         :: perknob ,dirknob
    real         :: dx, m1, m2
    intent (in) maxi, maxj, perknob, dirknob                           !40.par
    !      intent (in out) wsdat, sys, maxsys                                  !40.par
    !
    !     local variables
    !     ----------------------------------------------------------------
    !     ngbindex    int   arr   array of neighbours
    !
    integer, allocatable :: syssortedind(:), sysout(:)
    integer, pointer :: indsys1(:), indsys2(:)
    real, allocatable    :: sysordered(:), rounded(:)
    real, pointer    :: uniarr(:), difarr(:), allngbr(:)
    integer   :: leng, leng2, s, ss, so, ngb, lsys, lsys2, hh, i, j, &
         ii, jj, ind, ind2, nn, nbr, icend,ic,iii,iloop
    integer   :: myngbr, indmatch, matchsys, keep, replacedind, &
         hhforindmatch, lmatch, tot, outsize
    integer   :: ngbindex(10000), keepind(maxi*maxj), oneless(1000)     !array large enough?
    !      real      :: tb,deltaperb,deltadirb,absdir,absper,abshs,abswf
    real      :: tb,deltaperb,deltadirb,deltahsb,absdir,absper,abshs
    logical   :: file_exists
    integer   :: mask(maxi,maxj)
    real      :: lonmean, latmean, dist
    !061512 -----------------------------------------------
    logical   :: zipmatch
    integer   :: counter, count2, izp, izp2, in, jn, icnt, ngbrext
    real      :: t, ngb_tp, ngb_dir
    real      :: ngbmatch(maxi*maxj)
    type(neighbr) :: ngbr(50)
    !061512 -----------------------------------------------
    real      :: temp1, temp2
    integer   :: actsys
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      sort
    !      findijv4
    !      unique
    !      combinepartitionsv2
    !      union
    !      setdiff
    !
    !  5. subroutines calling
    !
    !     combinewavesystems
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     -
    !
    !  9. switches :
    !
    !     none defined yet.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !     initialize pointer (first use)
    nullify(indsys1)
    nullify(indsys2)
    !     flag to combine systems on a point-by-point basis along boundary,
    !     instead of using mean values.
    zipmatch = .false.
    ngbrext = 1
    !     combine systems on the basis of tpmean
    allocate( sysordered(maxsys) )
    allocate( syssortedind(maxsys) )
    allocate( sysout(maxsys) )
    allocate( rounded(maxsys) )
    !     sort in descending tp: the following improves the iterative combining in
    !     the special case that the wave period is constant over the domain, but
    !     tpmean is not because of truncation errors at very high decimals.
    rounded = real(int(sys(1:maxsys)%tpmean*1.e4))*1.e-4
    call sort(rounded,maxsys,sysordered,syssortedind,'d')
    sysout=sys(syssortedind)%sysind
    if (allocated(rounded)) deallocate(rounded)
    !051612 --- land mask addition
    mask(:,:) = 0
    inquire(file="sys_mask.ww3", exist=file_exists)
    if (file_exists) then
      write(20,*) '*** using land mask'
      open(unit=13,file='sys_mask.ww3',status='old')
      do j = maxj,1,-1
        read(13,*) (mask(i,j), i=1,maxi)
      end do
      close(13)
    end if
    !051612 --- land mask addition
    !opt      write(20,*) 'size(sysout)=',size(sysout)
    do so = 1, size(sysout)
      !          write(20,*) 'so =',so
      s = sysout(so)
      ss = findfirst(real(sys(:)%sysind),size(sys(:)%sysind), &
           real(s))
      !opt          write(20,*) 's,ss=',s,ss
      ngbindex(:) = 0
      ii = 1
      leng = length(real(sys(ss)%ngbr),size(sys(ss)%ngbr), &
           real(9999))
      !         identify the indices of all the systems that neighbour the current system s,
      !         store in ngbindex(:)
      do ngb = 1, leng
        if ( sys(ss)%ngbr(ngb).ne.s ) then
          myngbr = 1
          do while (myngbr.le.size(sysout))
            if (sys(myngbr)%sysind.eq.sys(ss)%ngbr(ngb)) then
              ngbindex(ii) = myngbr
              ii = ii+1
              if (ii.gt.1000) &
                   write(20,*) '*** warning: ngbindex(:) exceeded!'
            end if
            myngbr = myngbr+1
          end do
        end if
      end do
      ii = ii-1
      !opt          write(20,*) so,'.   sys =',s,', tp =',sys(s)%tpmean, &
      !opt                      ', size=',sys(s)%npoints,', #neighbours=',ii
      if ( ii.gt.0 ) then
        do ngb = 1, ii
          !                 we first need to find the (i,j) points that are either common
          !                 to both these systems, or at the boundary of the two systems. here
          !                 sys 1 will carry the 'ss' index and sys 2 the ngbindex(ngb) index.
          call findijv4 (sys(ss),sys(ngbindex(ngb)), &
               maxi,maxj,indsys1,indsys2)
          if ((size(indsys1)>10).and.(size(indsys2)>10).and. &
               (sys(ss)%npoints.gt.sys(ngbindex(ngb))%npoints)) &
               then
            lsys = size(indsys1)
            lsys2 = size(indsys2)
            !061512---------------add zipper compare
            if (zipmatch) then
              !                       omit small systems to save time
              if ((sys(ss)%npoints.lt.5).or. &
                   (sys(ngbindex(ngb))%npoints.lt.5)) then
                cycle
              end if
              dx=0.5*((wsdat%lon(2,1)-wsdat%lon(1,1)) + &
                   (wsdat%lat(1,2)-wsdat%lat(1,1)))
              ngbmatch(:)=0.
              do izp = 1,lsys
                !                          find neighbors of this point
                counter=0
                do in=(sys(ss)%i(indsys1(izp))-ngbrext), &
                     (sys(ss)%i(indsys1(izp))+ngbrext)
                  do jn=(sys(ss)%j(indsys1(izp))-ngbrext), &
                       (sys(ss)%j(indsys1(izp))+ngbrext)
                    counter=counter+1
                    ngbr(counter)%i = in
                    ngbr(counter)%j = jn
                  end do
                end do
                !                          find these points in neighboring system
                ngb_tp = 0.
                ngb_dir = 0.
                count2 = 0
                do izp2 = 1,lsys2
                  do icnt = 1,counter
                    if ((sys(ngbindex(ngb))%i(indsys2(izp2)) &
                         .eq.ngbr(icnt)%i).and. &
                         (sys(ngbindex(ngb))%j(indsys2(izp2)) &
                         .eq.ngbr(icnt)%j)) then
                      count2 = count2+1
                      ngb_tp = ngb_tp + &
                           sys(ngbindex(ngb))%tp(indsys2(izp2))
                      ngb_dir = ngb_dir + &
                           sys(ngbindex(ngb))%dir(indsys2(izp2))
                    end if
                  end do
                end do
                if (count2.gt.0) then
                  absper = abs(sys(ss)%tp(indsys1(izp))-ngb_tp/count2)
                  absdir = abs(sys(ss)%dir(indsys1(izp))-ngb_dir/count2)
                  t = sys(ss)%tp(indsys1(izp))
                  m1 = -3.645*t + 63.211
                  m1 = max(m1,10.)
                  m2 = -0.346*t + 3.686
                  m2 = max(m2,0.6)
                  deltadirb = (m1*dx + dirknob)*1.
                  deltaperb = (m2*dx + perknob)*1.
                  if ( (absper.lt.deltaperb).and. &
                       (absdir.lt.deltadirb) ) then
                    ngbmatch(izp)=1.
                  end if
                end if
              end do
              !                       if >80% of neighbors fall within criteria, system is matched
              if ((sum(ngbmatch(1:lsys))/lsys).gt.0.50) then
                indmatch = ngbindex(ngb)
                matchsys = sys(indmatch)%sysind
              else
                cycle
              end if
            else
              !061512---------------------------------
              tb = max(sum(sys(ss)%tp(indsys1))/lsys, &
                   sum(sys(ngbindex(ngb))%tp(indsys2))/lsys2)
              !                      deltaperb = (-0.06*tb+2+perknob)*1.5
              !                      deltadirb = (-tb+(25+10*dirknob))*1.5
              !                      deltaperb = (-0.06*tb+2+2)*1.5
              !                      deltadirb = (-tb+(25+10*2))*1.5
              dx=0.5*((wsdat%lon(2,1)-wsdat%lon(1,1)) + &
                   (wsdat%lat(1,2)-wsdat%lat(1,1)))
              m1 = -3.523*tb + 64.081
              m1 = max(m1,10.)
              m2 = -0.337*tb + 3.732
              m2 = max(m2,0.6)
              !1stddev                      m1 = -2.219*tb + 35.734
              !1stddev                      m1 = max(m1,5.)
              !1stddev                      m2 = -0.226*tb + 2.213
              !1stddev                      m2 = max(m2,0.35)
              !5stddev                      m1 = -5.071*tb + 90.688
              !5stddev                      m1 = max(m1,16.)
              !5stddev                      m2 = -0.467*tb + 5.161
              !5stddev                      m2 = max(m2,1.0)
              deltadirb = (m1*1. + dirknob)*1.
              deltaperb = (m2*1. + perknob)*1.
              deltahsb = 0.50*sum(sys(ss)%hs(indsys1))/lsys
              !                      deltahsb = 0.25*sum(sys(ss)%hs(indsys1))/lsys
              !051612               --- land mask addition
              !                     option 1: if system centroid is near a land mask (e.g. 3 arc-deg),
              !                               increase the tolerances
              if (any(mask.eq.1)) then
                lonmean = sum(sys(ss)%lon(indsys1))/lsys
                latmean = sum(sys(ss)%lat(indsys1))/lsys
                do j = 1,maxj
                  do i = 1,maxi
                    if (mask(i,j).eq.1) then
                      !                             land point found. compute distance to system centroid
                      dist = sqrt((lonmean-wsdat%lon(i,j))**2 +&
                           (latmean-wsdat%lat(i,j))**2)
                      if (dist.lt.3.) then
                        !                               system assumed to be influenced by land,
                        !                               increase tolerances to deltadirb=30,deltaperb=3
                        !                                deltadirb = (m1*1. + 30)*1.
                        !                                deltaperb = (m2*1. + 3)*1.
                        deltadirb = (m1*1. + 30)*1.
                        deltaperb = (m2*1. + 3)*1.
                        !remove dhs limitation from criteria
                        deltahsb = 9999.
                        goto 500
                      end if
                    end if
                  end do
                end do
              end if
500           continue
              !051612               --- land mask addition
              abshs = abs( sum(sys(ss)%hs(indsys1))/lsys - &
                   sum(sys(ngbindex(ngb))%hs(indsys2))/lsys2 )
              absper = abs( sum(sys(ss)%tp(indsys1))/lsys - &
                   sum(sys(ngbindex(ngb))%tp(indsys2))/lsys2 )
              absdir = abs( &
                   mean_anglev2(sys(ss)%dir(indsys1),lsys) - &
                   mean_anglev2(sys(ngbindex(ngb))%dir(indsys2), &
                   lsys2) )
              if (absdir.gt.180) absdir = 360.-absdir
              !                      abswf = abs( sum(sys(ss)%wf(indsys1))/lsys - &
              !                        sum(sys(ngbindex(ngb))%wf(indsys2))/lsys2 )
              if ( (absper.lt.deltaperb).and. &
                   (absdir.lt.deltadirb).and. &
                   (abshs.lt.deltahsb) ) then
                indmatch = ngbindex(ngb)
                matchsys = sys(indmatch)%sysind
                !opt                         write(20,*) '-> matched sys',s, &
                !opt                                     'with neighbor sys',matchsys
              else
                cycle
              end if
              !061512---------------------------------
            end if
            !061512---------------------------------
            keep = 0
            keepind(:) = 0
            do hh = 1, sys(ss)%npoints
              ii = sys(ss)%i(hh)
              jj = sys(ss)%j(hh)
              ind = 0
              ind = findfirst(real(wsdat%par(ii,jj)%sys), &
                   size(wsdat%par(ii,jj)%sys),real(s))        !shouldn't real(s) be matchsys...
              if (ind.ne.0) then
                wsdat%par(ii,jj)%sys(ind)=matchsys            !...and matchsys be s, (i.e. add the matching neigbour to the base?)
              end if
              !                        remove the "-1" system from the set
              ind2 = 1
              oneless(:) = 9999                                !streamline this?
              leng = length(real(wsdat%par(ii,jj)%sys), &
                   size(wsdat%par(ii,jj)%sys),real(9999))
              do ind = 1, leng
                if ( wsdat%par(ii,jj)%sys(ind).ne.-1 ) then
                  oneless(ind2) = wsdat%par(ii,jj)%sys(ind)
                  ind2 = ind2+1
                end if
              end do
              ind2 = ind2-1
              !                        combine any partitions assigned to the same systems
              !                        check for duplicates
              if (ind2.eq.0) &
                   write(20,*) '***2.calling unique w. len=0!'
              call unique(real(oneless(1:ind2)),ind2, &
                   uniarr,outsize)
              if (associated(uniarr)) deallocate(uniarr)
              if (ind2.gt.outsize) then
                !                          there is at least one duplicate, so combine systems
                call combinepartitionsv2(wsdat%par(ii,jj))
                !                          update the combined partitions values into the system we are keeping.
                !                          since partitions have been combined we don't know if the index is the same
                replacedind = &
                     findfirst(real(wsdat%par(ii,jj)%sys(:)), &
                     size(wsdat%par(ii,jj)%sys(:)), &
                     real(matchsys))
                hhforindmatch = 1
                do while (hhforindmatch.le. &
                     sys(indmatch)%npoints)
                  if ( (sys(indmatch)%i(hhforindmatch) &
                       .eq.ii).and. &
                       (sys(indmatch)%j(hhforindmatch) &
                       .eq.jj) ) exit
                  hhforindmatch = hhforindmatch + 1
                end do
                sys(indmatch)%hs(hhforindmatch) = &
                     wsdat%par(ii,jj)%hs(replacedind)
                sys(indmatch)%tp(hhforindmatch) = &
                     wsdat%par(ii,jj)%tp(replacedind)
                sys(indmatch)%dir(hhforindmatch) = &
                     wsdat%par(ii,jj)%dir(replacedind)
                sys(indmatch)%dspr(hhforindmatch) = &
                     wsdat%par(ii,jj)%dspr(replacedind)
                !                           sys(indmatch)%wf(hhforindmatch) = &
                !                                  wsdat%par(ii,jj)%wf(replacedind)
              else
                keep = keep+1
                keepind(keep) = hh
              end if
            end do
            leng = length(real(sys(indmatch)%hs), &
                 size(sys(indmatch)%hs),real(9999.))
            !                     update system info
            !                     ------------------
            !                     first need to find which points were common to both systems =>
            !                     keepind since that means partitions have not been combined for those
            !                     points as a result of the combination of those 2 systems =>
            !                     distinct points
            !                      keepind = keepind(1:keep)
            lmatch = length(real(sys(indmatch)%hs), &
                 size(sys(indmatch)%hs),real(9999.))
            tot = lmatch + keep
            call union (real(sys(indmatch)%ngbr), &
                 size(sys(indmatch)%ngbr), &
                 real(sys(ss)%ngbr), &
                 size(sys(ss)%ngbr), &
                 allngbr,outsize)
            call setdiff(allngbr,size(allngbr), &
                 real((/sys(indmatch)%sysind, &
                 sys(ss)%sysind/)), &
                 size((/sys(indmatch)%sysind, &
                 sys(ss)%sysind/)),difarr,outsize)
            sys(indmatch)%ngbr(:) = 9999
            outsize = min(outsize,size(sys(indmatch)%ngbr))
            sys(indmatch)%ngbr(1:outsize) = nint(difarr(1:outsize))
            if (associated(allngbr)) deallocate(allngbr)
            if (associated(difarr)) deallocate(difarr)
            leng = length(real(sys(indmatch)%i), &
                 size(sys(indmatch)%i),real(9999))
            sys(indmatch)%hsmean = sum((/ &
                 sys(ss)%hs(keepind(1:keep)), &
                 sys(indmatch)%hs(1:leng) /))/tot
            sys(indmatch)%tpmean = sum((/ &
                 sys(ss)%tp(keepind(1:keep)), &
                 sys(indmatch)%tp(1:leng) /))/tot
            sys(indmatch)%dirmean = &
                 mean_anglev2((/ sys(ss)%dir(keepind(1:keep)), &
                 sys(indmatch)%dir(1:leng) /),tot)
            !070512----------- weight averages with hm0 ---------------------
            temp1 = 0.
            temp2 = 0.
            do iii = 1,keep
              temp1 = temp1 + (sys(ss)%hs(keepind(iii))**2)*&
                   sys(ss)%hs(keepind(iii))
              temp2 = temp2 + (sys(ss)%hs(keepind(iii))**2)*&
                   sys(ss)%tp(keepind(iii))
            end do
            do iii = 1,leng
              temp1 = temp1 + (sys(indmatch)%hs(iii)**2)*&
                   sys(indmatch)%hs(iii)
              temp2 = temp2 + (sys(indmatch)%hs(iii)**2)*&
                   sys(indmatch)%tp(iii)
            end do
            sys(indmatch)%hsmean = temp1/max(sum((/ &
                 sys(ss)%hs(keepind(1:keep))**2, &
                 sys(indmatch)%hs(1:leng)**2 /)),0.001)
            sys(indmatch)%tpmean = temp2/max(sum((/ &
                 sys(ss)%hs(keepind(1:keep))**2, &
                 sys(indmatch)%hs(1:leng)**2 /)),0.001)
            sys(indmatch)%dirmean = &
                 mean_anglev3((/ sys(ss)%dir(keepind(1:keep)), &
                 sys(indmatch)%dir(1:leng) /), &
                 (/ sys(ss)%hs(keepind(1:keep)), &
                 sys(indmatch)%hs(1:leng) /),tot)
            !070512----------- weight averages with hm0 ---------------------
            sys(indmatch)%i(1:(keep+leng))= &
                 (/sys(ss)%i(keepind(1:keep)), &
                 sys(indmatch)%i(1:leng)/)
            sys(indmatch)%j(1:(keep+leng))= &
                 (/sys(ss)%j(keepind(1:keep)), &
                 sys(indmatch)%j(1:leng)/)
            sys(indmatch)%lat(1:(keep+leng)) = &
                 (/sys(ss)%lat(keepind(1:keep)), &
                 sys(indmatch)%lat(1:leng)/)
            sys(indmatch)%lon(1:(keep+leng)) = &
                 (/sys(ss)%lon(keepind(1:keep)), &
                 sys(indmatch)%lon(1:leng)/)
            sys(indmatch)%dir(1:(keep+leng)) = &
                 (/sys(ss)%dir(keepind(1:keep)), &
                 sys(indmatch)%dir(1:leng)/)
            sys(indmatch)%dspr(1:(keep+leng)) = &
                 (/sys(ss)%dspr(keepind(1:keep)), &
                 sys(indmatch)%dspr(1:leng)/)
            !                      sys(indmatch)%wf(1:(keep+leng)) = &
            !                          (/sys(ss)%wf(keepind(1:keep)), &
            !                            sys(indmatch)%wf(1:leng)/)
            sys(indmatch)%hs(1:(keep+leng)) = &
                 (/sys(ss)%hs(keepind(1:keep)), &
                 sys(indmatch)%hs(1:leng)/)
            sys(indmatch)%tp(1:(keep+leng)) = &
                 (/sys(ss)%tp(keepind(1:keep)), &
                 sys(indmatch)%tp(1:leng)/)
            sys(indmatch)%npoints = &
                 length(real(sys(indmatch)%i), &
                 size(sys(indmatch)%i),real(9999))
            !                     clear array of system that has just been combined with another
            sys(ss)%npoints = 0
            sys(ss)%ngbr(:) = 9999
            write(20,*) 'deallocating sys',s
            deallocate( sys(ss)%hs )                            !opt
            deallocate( sys(ss)%tp )                            !opt
            deallocate( sys(ss)%dir )                           !opt
            deallocate( sys(ss)%dspr )                          !opt
            !                      deallocate( sys(ss)%wf )                           !opt
            deallocate( sys(ss)%i )                             !opt
            deallocate( sys(ss)%j )                             !opt
            deallocate( sys(ss)%lat )                           !opt
            deallocate( sys(ss)%lon )                           !opt
            !                      deallocate( sys(ss)%hsmean )                        !opt
            !                      deallocate( sys(ss)%tpmean )                        !opt
            !                      deallocate( sys(ss)%dirmean )                       !opt
            !                     loop through wsdat to update neighbouring system values
            do i = 1, maxi
              do j = 1, maxj
                ind = findfirst(real(wsdat%par(i,j)%ngbrsys), &
                     size(wsdat%par(i,j)%ngbrsys),real(s))
                if (ind.ne.0) then
                  wsdat%par(i,j)%ngbrsys(ind)=matchsys
                end if
                leng = length(real(wsdat%par(i,j)%ngbrsys), &
                     size(wsdat%par(i,j)%ngbrsys),real(9999))
                if (leng.gt.0) then
                  call unique( &
                       real(wsdat%par(i,j)%ngbrsys(1:leng)), &
                       leng,uniarr,outsize)
                  wsdat%par(i,j)%ngbrsys(:) = 9999
                  wsdat%par(i,j)%ngbrsys(1:outsize) = &
                       nint(uniarr)
                  if (associated(uniarr)) deallocate(uniarr)
                else
                  wsdat%par(i,j)%ngbrsys(:) = 9999
                end if
              end do
            end do
            !                     update neigbors in sys structure
            do nn = 1, maxsys
              nbr = findfirst(real(sys(nn)%ngbr), &
                   size(sys(nn)%ngbr),real(s))
              if (nbr.ne.0) then
                !                             write(20,*) 'update'
                sys(nn)%ngbr(nbr)=matchsys
              end if
              leng2 = length(real(sys(nn)%ngbr), &
                   size(sys(nn)%ngbr),real(9999))
              if (leng2.gt.0) then
                call unique(real(sys(nn)%ngbr(1:leng2)), &
                     leng2,uniarr,outsize)
                sys(nn)%ngbr(:) = 9999
                sys(nn)%ngbr(1:outsize) = nint(uniarr)
                if (associated(uniarr)) deallocate(uniarr)
                !                             write(20,*) 'has now ngbr: ', &
                !                                sys(nn)%ngbr(1:outsize)
              end if
            end do
            exit
          end if
          if (associated(indsys1)) deallocate(indsys1)
          if (associated(indsys2)) deallocate(indsys2)
        end do
      end if
    end do
    if (allocated(sysordered)) deallocate(sysordered)
    if (allocated(syssortedind)) deallocate(syssortedind)
    if (allocated(sysout)) deallocate(sysout)
    !     compile array index of active systems in sys
    actsys = 0
    do ic = 1,maxsys
      if (sys(ic)%npoints>0) actsys = actsys + 1
    end do
    if (allocated(actsysind)) deallocate(actsysind)
    allocate( actsysind(actsys) )
    actsys = 0
    do ic = 1,maxsys
      if (sys(ic)%npoints>0) then
        actsys = actsys + 1
        actsysind(actsys) = sys(ic)%sysind
      end if
    end do
    !opt      write(20,*) 'actsys =',actsys
    !opt      write(20,*) 'actsysind =',actsysind
    !opt      do ic = 1,size(actsysind)
    !opt         s = actsysind(ic)
    !opt         write(20,*) 'sys(',s,')%sysind =',sys(s)%sysind
    !opt      end do
    write(20,*) 'leaving combinesys...'
    return
  end subroutine combinesys
  !/ end of combinesys ------------------------------------------------- /
  !/
  !/ ------------------------------------------------------------------- /
  subroutine combinepartitionsv2 (dat)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |     a. j. van der westhuysen      |
    !/                  |            jeff hanson            |
    !/                  |        eve-marie devaliere        |
    !/                  |                        fortran 95 |
    !/                  | last update :          4-jan-2013 |
    !/                  +-----------------------------------+
    !/
    !/    03-feb-2012 : origination, based on matlab code   ( version 4.05 )
    !/                  by jeff hanson & eve-marie devaliere
    !/    04-jan-2013 : inclusion in trunk                  ( version 4.08 )
    !/
    !/    copyright 2009-2013 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    implicit none
    !
    !  1. purpose :
    !
    !     combine two partitions that have been assigned to the same system
    !
    !  2. method
    !
    !     of all the partitions associated with a certain common system,
    !     add all the hs values to the partition with the largest hs,
    !     and delete the rest. note that the tp and dir values of this
    !     maximum partition is not adjusted!
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     dat    type(param)   in/out  input data structure (partitions set)
    !                                  to combine
    !
    type(param) :: dat
    intent (in out) dat
    !
    !     local variables
    !     ----------------------------------------------------------------
    type duplicate
      integer :: val
      integer :: ndup
      integer :: ind(50)
    end type duplicate
    type(duplicate) :: dup(100)                                         !40.par
    logical :: found
    integer :: nsys, ndup, p, pp, maxind, npart, s, ss, ppp
    real :: temp
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      -
    !
    !  5. subroutines calling
    !
    !     findsys
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     -
    !
    !  9. switches :
    !
    !     none defined yet.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !     find indices in dat%sys(:) of all partition associated with
    !     the same wave system, and store them in the data structure
    !     dup(1:nsys). here nsys is the number of systems for which duplicates
    !     were found, and dup(s)%ndup the number of partitions assigned
    !     to the same system s.
    nsys = 0
    dup(:)%ndup = 0
    dup(:)%val = 9999
    do s = 1,100
      dup(s)%ind(:) = 0
    end do
    npart = length(real(dat%ipart),size(dat%ipart),real(0))
    do p = 1, npart-1
      found = .false.
      if (any(dat%sys(p).eq.dup(:)%val)) cycle                         !found = .true.
      do pp = (p+1), npart
        if (dat%sys(p).eq.dat%sys(pp)) then
          !              first value
          if (.not.found) then
            nsys=nsys+1
            dup(nsys)%val = dat%sys(p)
            dup(nsys)%ndup = 1
            dup(nsys)%ind(dup(nsys)%ndup) = p
            found = .true.
          end if
          !              subsequent duplicates
          if (.not.any(pp.eq.dup(nsys)%ind(:))) then
            dup(nsys)%ndup = dup(nsys)%ndup+1
            dup(nsys)%ind(dup(nsys)%ndup) = pp
          end if
        end if
      end do
    end do
    !     now go through array of duplicates for each of n systems
    !     to add all the wave energy to the most energetic of the
    !     duplicates, and then remove the rest.
    maxind = 0
    temp = -9999.
    do s = 1, nsys
      !        find duplicate partition with the largest hs (most energy)
      do p = 1, dup(s)%ndup
        if ( temp.lt.dat%hs(dup(s)%ind(p)) ) then
          temp = dat%hs(dup(s)%ind(p))
          maxind = p
        end if
      end do
      !        add all energy (hs) to this partition
      dat%hs(dup(s)%ind(maxind)) = &
           sqrt( sum(dat%hs(dup(s)%ind(1:dup(s)%ndup))**2) )
      !        remove duplicate partitions which did not have the maximum hs,
      !        and shift up indices to fill the gap
      do p = 1, dup(s)%ndup
        !           find index to remove
        if (p.ne.maxind) then
          !              shift up entries, deleting the duplicate partition
          !              replace with cshift(array, shift, dim) ?
          dat%hs( dup(s)%ind(p):(npart-1) ) = &
               dat%hs( (dup(s)%ind(p)+1):npart)
          dat%tp( dup(s)%ind(p):(npart-1) ) = &
               dat%tp( (dup(s)%ind(p)+1):npart)
          dat%dir( dup(s)%ind(p):(npart-1) ) = &
               dat%dir( (dup(s)%ind(p)+1):npart)
          dat%dspr( dup(s)%ind(p):(npart-1) ) = &
               dat%dspr( (dup(s)%ind(p)+1):npart)
          !               dat%wf( dup(s)%ind(p):(npart-1) ) = &
          !                       dat%wf( (dup(s)%ind(p)+1):npart)
          dat%sys( dup(s)%ind(p):(npart-1) ) = &
               dat%sys( (dup(s)%ind(p)+1):npart)
          dat%ipart( dup(s)%ind(p):(npart-1) ) = &
               dat%ipart( (dup(s)%ind(p)+1):npart)
          !              shift up indices
          do ss = 1, nsys
            do ppp = 1, dup(ss)%ndup
              if (dup(ss)%ind(ppp).gt.dup(s)%ind(p)) &
                   dup(ss)%ind(ppp) = dup(ss)%ind(ppp)-1
            end do
          end do
          !              add blank to end
          dat%hs(npart) = 9999.
          dat%tp(npart) = 9999.
          dat%dir(npart) = 9999.
          dat%dspr(npart) = 9999.
          !               dat%wf(npart) = 9999.
          dat%sys(npart) = 9999
          dat%ipart(npart) = 0
        end if
      end do
    end do
    return
  end subroutine combinepartitionsv2
  !/ end of combinepartitionsv2 ---------------------------------------- /
  !/
  !/ ------------------------------------------------------------------- /
  real function mean_anglev2(ang,ll)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |     a. j. van der westhuysen      |
    !/                  |            jeff hanson            |
    !/                  |        eve-marie devaliere        |
    !/                  |                        fortran 95 |
    !/                  | last update :          4-jan-2013 |
    !/                  +-----------------------------------+
    !/
    !/    03-feb-2012 : origination, based on matlab code   ( version 4.05 )
    !/                  by jeff hanson & eve-marie devaliere
    !/    04-jan-2013 : inclusion in trunk                  ( version 4.08 )
    !/
    !/    copyright 2009-2013 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    implicit none
    !
    !  1. purpose :
    !
    !     compute the mean direction from array of directions
    !
    !  2. method
    !
    !     ang is a column vector of angles
    !     m_ang is the mean from a unit-vector average of ang
    !     assumes clockwise rotation from north = 0.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     ang     real   input    array of angles to average
    !     ll      int    input    length of ang
    !
    real :: ang(ll)
    integer :: ll
    !
    !     local variables
    !     ----------------------------------------------------------------
    !     u,v       real   arrays of u,v dir components to average
    !     um,vm     real   mean u,v dir components
    !     theta     real   mean direction relative to north
    !
    real      :: pi
    parameter  (pi = 3.1416)
    real      :: u(ll), v(ll), vm, um, theta
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      -
    !
    !  5. subroutines calling
    !
    !     findsys
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     -
    !
    !  9. switches :
    !
    !     none defined yet.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !     north and east components
    v(:) = cos(ang(:)*(pi/180.))
    u(:) = sin(ang(:)*(pi/180.))
    vm = sum(v)/ll
    um = sum(u)/ll
    !     compute mean magnitude and direction relative to north (from upolar.m)
    theta = (atan2(um,vm))*(180/pi)
    !     convert inputs to radians, the to the -pi to pi range
    !     (incorporated from original function xunwrapv2.m)
    !     convert to radians
    theta = theta*(pi/180)
    theta = pi*((abs(theta)/pi) - &
         2*ceiling(((abs(theta)/pi)-1)/2))*sign(1.,theta)
    !     shift the points in the -pi to 0 range to the pi to 2pi range
    if (theta.lt.0.) theta = theta + 2*pi
    !     convert back to degrees and return value
    mean_anglev2 = theta*(180/pi)
    return
  end function mean_anglev2
  !/ end of mean_anglev2 ----------------------------------------------- /
  !/
  !/ ------------------------------------------------------------------- /
  real function mean_anglev3(ang,hsign,ll)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |     a. j. van der westhuysen      |
    !/                  |            jeff hanson            |
    !/                  |        eve-marie devaliere        |
    !/                  |                        fortran 95 |
    !/                  | last update :          4-jan-2013 |
    !/                  +-----------------------------------+
    !/
    !/    03-feb-2012 : origination, based on matlab code   ( version 4.05 )
    !/                  by jeff hanson & eve-marie devaliere
    !/    04-jan-2013 : inclusion in trunk                  ( version 4.08 )
    !/
    !/    copyright 2009-2013 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    implicit none
    !
    !  1. purpose :
    !
    !     compute the mean direction from array of directions,
    !     including weighting with hmo
    !
    !  2. method
    !
    !     ang is a column vector of angles
    !     m_ang is the mean from a unit-vector average of ang
    !     assumes clockwise rotation from north = 0.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     ang     real   input    array of angles to average
    !     ll      int    input    length of ang
    !
    real :: ang(ll), hsign(ll)
    real :: temp1, temp2
    integer :: ll
    !
    !     local variables
    !     ----------------------------------------------------------------
    !     u,v       real   arrays of u,v dir components to average
    !     um,vm     real   mean u,v dir components
    !     theta     real   mean direction relative to north
    !
    real      :: pi
    parameter  (pi = 3.1416)
    real      :: u(ll), v(ll), vm, um, theta
    integer   :: i
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      -
    !
    !  5. subroutines calling
    !
    !     findsys
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     -
    !
    !  9. switches :
    !
    !     none defined yet.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !     north and east components
    v(:) = cos(ang(:)*(pi/180.))
    u(:) = sin(ang(:)*(pi/180.))
    temp1 = 0.
    temp2 = 0.
    do i = 1,ll
      temp1 = temp1 + (hsign(i)**2)*v(i)
      temp2 = temp2 + (hsign(i)**2)*u(i)
    end do
    vm = temp1/max(sum(hsign**2),0.001)
    um = temp2/max(sum(hsign**2),0.001)
    !     compute mean magnitude and direction relative to north (from upolar.m)
    theta = (atan2(um,vm))*(180/pi)
    !     convert inputs to radians, the to the -pi to pi range
    !     (incorporated from original function xunwrapv2.m)
    !     convert to radians
    theta = theta*(pi/180)
    theta = pi*((abs(theta)/pi) - &
         2*ceiling(((abs(theta)/pi)-1)/2))*sign(1.,theta)
    !     shift the points in the -pi to 0 range to the pi to 2pi range
    if (theta.lt.0.) theta = theta + 2*pi
    !     convert back to degrees and return value
    mean_anglev3 = theta*(180/pi)
    return
  end function mean_anglev3
  !/ end of mean_anglev3 ----------------------------------------------- /
  !/
  !/ ------------------------------------------------------------------- /
  subroutine unique (inarray,insize,outarray,outsize)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |     a. j. van der westhuysen      |
    !/                  |            jeff hanson            |
    !/                  |        eve-marie devaliere        |
    !/                  |                        fortran 95 |
    !/                  | last update :         22-dec-2016 |
    !/                  +-----------------------------------+
    !/
    !/    03-feb-2012 : origination, based on matlab code   ( version 4.05 )
    !/                  by jeff hanson & eve-marie devaliere
    !/    04-jan-2013 : inclusion in trunk                  ( version 4.08 )
    !/    12-dec-2016 : change algorithm from n*n to n*log(n)
    !/                  (s. zieger bom australia)           ( version 5.16 )
    !/
    !/    copyright 2009-2013 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    implicit none
    !
    !  1. purpose :
    !
    !     returns the sorted elements that are unique in inarray.
    !
    !  2. method
    !
    !     1. sort input array with quicksort
    !     2. copy sequential-elements if the 'current' element
    !        is not equal to the 'previous' element in array.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     inarray   real    arr  input    input array
    !     insize    integer      input    size of input array
    !     outarray  real    arr  output   output array (sorted)
    !     outsize   integer      output   size of output array (number of unique elements)
    !
    integer, intent(in)  :: insize
    integer, intent(out) :: outsize
    real, intent(in)     :: inarray(insize)
    real, pointer        :: outarray(:)
    !
    !     local variables
    !     ----------------------------------------------------------------
    integer          :: i, k
    real             :: array(insize), temp(insize)
    !
    !  4. subroutines used :
    !
    !      name      type  scope    description
    !     ----------------------------------------------------------------
    !      qsort     subr. private  quicksort algorithm
    !
    !  5. subroutines calling
    !
    !     wavetracking_nws_v2
    !     findsys
    !     printfinalsys
    !     combinesys
    !     findijv4
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     -
    !
    !  9. switches :
    !
    !     none defined yet.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    k = 1
    if ( insize.eq.0 ) then
      write(20,*) '*** in subr. unique: input array has length=0!'
    else
      !/    --- setup input arrays and temporary arrays. ---
      do i=1,insize
        array(i) = inarray(i)
        temp(i)  = real(i)
      end do
      !/
      !/      --- sort input arrays (use temporary array to store indices). ---
      call qsort(array,temp,1,insize)
      !/
      !/      --- reset temporary array. ---
      temp(:) = 9999.
      !/
      !/      --- initialise first values and array index. ---
      k = 1
      temp(k) = array(k)
      k = k + 1
      !/      --- iterate over elements in array. ---
      do i=2,insize
        !/        --- compare sequential array values ('previous' less than 'next')
        !/            and test against the last list element check in. ---
        if ( array(i).gt.array(i-1) .and. &
             array(i).gt.temp(k-1)        ) then
          temp(k) = array(i)
          k = k + 1
        end if
      end do
      !/      --- allocate output array ---
      outsize = k - 1
      allocate(outarray(outsize))
      !/      --- transfer output from temporary array to output array. ---
      if ( outsize.ge.1 ) then
        do i=1,outsize
          outarray(i) = temp(i)
        end do
      end if
    end if
    !/
    return
    !/
  end subroutine unique
  !/ end of unique ----------------------------------------------------- /
  !/
  !/ ------------------------------------------------------------------- /
  subroutine sort (inarray,insize,outarray,iy,direction)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |     a. j. van der westhuysen      |
    !/                  |            jeff hanson            |
    !/                  |        eve-marie devaliere        |
    !/                  |                        fortran 95 |
    !/                  | last update :         20-dec-2016 |
    !/                  +-----------------------------------+
    !/
    !/    03-feb-2012 : origination, based on matlab code   ( version 4.05 )
    !/                  by jeff hanson & eve-marie devaliere
    !/    04-jan-2013 : inclusion in trunk                  ( version 4.08 )
    !/    20-dec-2016 : add quicksort algorithm (s. zieger) ( version 5.16 )
    !/
    !/    copyright 2009-2013 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/
    implicit none
    !
    !  1. purpose :
    !
    !     sorts the array inarray in ascending (direction = 'a') or
    !     descending (direciton = 'd') order. the sorted array is
    !     stored in outarray, and the sorted array of the original
    !     indices is stored in iy.
    !
    !  2. method
    !
    !     sort algorithm based on quicksort.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     inarray   real    arr  input    input array
    !     insize    integer      input    size of input array
    !     outarray  real    arr  output   sorted output array
    !     iy        integer arr  output   sorted array of the original indices
    character         :: direction *1
    integer           :: insize
    integer           :: iy(insize)
    real              :: inarray(insize), outarray(insize)
    intent (in)  inarray, insize, direction
    intent (out) outarray, iy
    !
    !     local variables
    !     ----------------------------------------------------------------
    !      inarray - array of values to be sorted
    !      iy -      array to be carried with x (all swaps of x elements are       ??? edit!
    !                matched in iy .  after the sort iy(j) contains the original
    !                postition of the value x(j) in the unsorted x array.
    !      n -       number of values in array x to be sorted
    integer           :: i
    real              :: ind(insize)
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      -
    !
    !  5. subroutines calling
    !
    !     printfinalsys
    !     combinesys
    !     timetrackingv2
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     -
    !
    !  9. switches :
    !
    !     none defined yet.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !     sort outarray in as/decending order
    if (insize.eq.0) then
      write(20,*) '*** in subr. sort: input array has length=0 !!!'
    else
      do i = 1, insize
        outarray(i) = inarray(i)
        ind(i) = real(i)
      end do
      if (direction .eq. 'a') then
        call qsort(outarray,ind,1,insize)
      else if (direction .eq. 'd') then
        call qsort_desc(outarray,ind,1,insize)
      end if
    end if
    !
    !/    --- cast index array to integer. ---
    do i = 1, insize
      iy(i) = int(ind(i))
    end do
    !
    return
    !
  end subroutine sort
  !/ end of sort ------------------------------------------------------- /
  !/
  !/ ------------------------------------------------------------------- /
  subroutine setdiff (inarray1,   insize1,  inarray2,   insize2, &
       outarray,   outsize)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |     a. j. van der westhuysen      |
    !/                  |            jeff hanson            |
    !/                  |        eve-marie devaliere        |
    !/                  |                        fortran 95 |
    !/                  | last update :         20-dec-2016 |
    !/                  +-----------------------------------+
    !/
    !/    03-feb-2012 : origination, based on matlab code   ( version 4.05 )
    !/                  by jeff hanson & eve-marie devaliere
    !/    04-jan-2013 : inclusion in trunk                  ( version 4.08 )
    !/    20-dec-2016 : add quicksort algorithm (s.zieger)  ( version 5.16 )
    !/
    !/    copyright 2009-2013 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    implicit none
    !
    !  1. purpose :
    !
    !     (i)  returns the elements in inarray1 that are not in inarray2.
    !     (ii) sort the resulting array in ascending order.
    !
    !  2. method
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     inarray1  real    arr  input    input array
    !     insize1   integer      input    size of input array
    !     inarray2  real    arr  input    input array
    !     insize2   integer      input    size of input array
    !     outarray  real    arr  output   output array
    !     outsize   integer      output   size of output array (number of unique elements)
    integer           :: insize1, insize2, outsize
    real              :: inarray1(insize1), inarray2(insize2)
    real, pointer     :: outarray(:)
    intent (in)  inarray1, insize1, inarray2, insize2
    intent (out) outsize
    !
    !     local variables
    !     ----------------------------------------------------------------
    integer          :: i,j,k
    real             :: temp(insize1)
    real             :: array1(insize1),array2(insize2)
    real             :: id1(insize1),id2(insize2)
    logical          :: loop
    !
    !  4. subroutines used :
    !
    !      name      type  scope    description
    !     ----------------------------------------------------------------
    !      qsort     subr. private  quicksort algorithm
    !
    !  5. subroutines calling
    !
    !     printfinalsys
    !     combinesys
    !     timetrackingv2
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     -
    !
    !  9. switches :
    !
    !     none defined yet.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    if ( (insize1).eq.0 ) then
      outsize = 0
      allocate(outarray(outsize))
    else if ( insize2.eq.0 ) then
      call unique(inarray1,insize1,outarray,outsize)
    else
      !/      --- setup input arrays. ---
      do i=1,insize1
        array1(i) = inarray1(i)
        id1(i)    = real(i)
      end do
      do i=1,insize2
        array2(i) = inarray2(i)
        id2(i)    = real(i)
      end do
      !/
      !/      --- sort input arrays. ---
      call qsort(array1,id1,1,insize1)
      call qsort(array2,id2,1,insize2)
      !/
      !/      --- initialise indices. ---
      i = 1
      j = 1
      k = 1
      !/
      !/      --- allocate and initialize temporary output ---
      temp(:) = 9999.
      !/
      !/      --- loop though both arrays by incrementing i,j. ---
      loop = .true.
      do while ( loop )
        !/
        if ( array1(i).lt.array2(j) .or.   &
             array1(i).gt.array2(insize2)  ) then
          !/            --- populate output array. check for dumplicates
          !/                in output array. ---
          if ( k.eq.1 ) then
            temp(k) = array1(i)
            k = k + 1
          else if ( temp(k-1).lt.array1(i) ) then
            temp(k) = array1(i)
            k = k + 1
          end if
          i = i + 1
        else if ( array2(j).lt.array1(i) ) then
          j = j + 1
        else
          i = i + 1
          j = j + 1
        end if
        !/          --- check for exit the loop. ---
        if ( i.gt.insize1 ) then
          loop = .false.
        end if
        !/          --- make sure array pointer i,j are within array bounds. ---
        i = min(i,insize1)
        j = min(j,insize2)
        !/
      end do
      !/
      !/      --- allocate output array ---
      outsize = k-1
      allocate(outarray(outsize))
      !/        --- transfer output from temporary array to output array. ---
      do i=1,outsize
        outarray(i) = temp(i)
      end do
    end if
    !/
    return
    !/
  end subroutine setdiff
  !/ end of setdiff ---------------------------------------------------- /
  !/
  !/ ------------------------------------------------------------------- /
  subroutine intersect (inarray1 ,insize1  ,inarray2 ,insize2  , &
       outarray ,outsize  ,ind1     ,ind2     )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |     a. j. van der westhuysen      |
    !/                  |            jeff hanson            |
    !/                  |        eve-marie devaliere        |
    !/                  |                        fortran 95 |
    !/                  | last update :         20-dec-2016 |
    !/                  +-----------------------------------+
    !/
    !/    03-feb-2012 : origination, based on matlab code   ( version 4.05 )
    !/                  by jeff hanson & eve-marie devaliere
    !/    04-jan-2013 : inclusion in trunk                  ( version 4.08 )
    !/    20-dec-2016 : add count-histogram method based on
    !/                  algorithm from mirko velic (bom)
    !/                  (s. zieger bom, australia)          ( version 5.16 )
    !/
    !/    copyright 2009-2013 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    implicit none
    !
    !  1. purpose :
    !
    !     (i)  returns the elements that are mutual in inarray1 and inarray2.
    !     (ii) sort the resulting array in ascending order.
    !
    !  2. method
    !
    !     sort with counting/histogram method with input array being
    !     cast as integer.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     inarray1  real    arr  input    input array
    !     insize1   integer      input    size of input array
    !     inarray2  real    arr  input    input array
    !     insize2   integer      input    size of input array
    !     outarray  real    arr  output   output array
    !     outsize   integer      output   size of output array (number of
    !     intersects)
    !
    integer           :: insize1, insize2, outsize
    real              :: inarray1(insize1), inarray2(insize2)
    real, pointer :: outarray(:)
    real, pointer :: ind1(:), ind2(:)
    !
    intent (in)  inarray1, insize1, inarray2, insize2
    intent (out) outsize
    !
    !     local variables
    !     ----------------------------------------------------------------
    !     vidx1, vidx2 - array(s) in which the value is represented by
    !                    its index (i.e. histogram with frequency 1)
    !     n            - data range and size of possible intersections.
    !
    logical,allocatable :: vidx1(:),vidx2(:)
    integer,allocatable :: ipos1(:),ipos2(:)
    !
    integer             :: i, j
    integer             :: n, imin, imax
    integer             :: minv1,maxv1, minv2, maxv2
    !
    !  4. subroutines used :
    !
    !      name      type  scope    description
    !     ----------------------------------------------------------------
    !
    !  5. subroutines calling
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     -
    !
    !  9. switches :
    !
    !     none defined yet.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    outsize = 0
    !/    --- calculate the range of the two sets. ---
    minv1  = int(minval(inarray1))
    maxv1  = int(maxval(inarray1))
    minv2  = int(minval(inarray2))
    maxv2  = int(maxval(inarray2))
    !/    --- check if ranges overlap. ---
    if ( maxv1.lt.minv2.or.insize1.eq.0.or.insize2.eq.0 ) then
      allocate(outarray(outsize))
      allocate(ind1(outsize))
      allocate(ind2(outsize))
    else
      !/      --- calculate size of temporary output arrays. allow
      !/          extra elements: zero, and make sure index is 1:n. ---
      imin = min(minv1,minv2)-1
      imax = max(maxv1,maxv2)+1
      n = imax-imin
      allocate(vidx1(n),vidx2(n))
      allocate(ipos1(n),ipos2(n))
      vidx1(1:n) = .false.
      vidx2(1:n) = .false.
      do i=1,insize1
        j = int(inarray1(i)-imin)
        vidx1(j) = .true.
        ipos1(j) = i
      end do
      do i=1,insize2
        j = int(inarray2(i)-imin)
        !/        --- intersect arrays and check for
        !/            duplicate elements in array2. ---
        if  ( vidx1(j).and..not.vidx2(j) ) then
          outsize = outsize + 1
          vidx2(j) = .true.
          ipos2(j) = i
        end if
      end do
      !/      --- allocate output arrays. ---
      allocate(outarray(outsize))
      allocate(ind1(outsize))
      allocate(ind2(outsize))
      !/      --- transfer contents. ---
      i = 1
      do j=1,n
        if ( vidx1(j).and.vidx2(j).and.i.le.outsize ) then
          outarray(i) = inarray1(ipos1(j))
          ind1(i)     = ipos1(j)
          ind2(i)     = ipos2(j)
          i = i + 1
        end if
      end do
      !/      --- free memory. ---
      deallocate(vidx1,vidx2)
      deallocate(ipos1,ipos2)
    end if
    !/
    return
    !/
  end subroutine intersect
  !/ end of intersect -------------------------------------------------- /
  !/
  !/ ------------------------------------------------------------------- /
  subroutine union (inarray1,   insize1,  inarray2,   insize2, &
       outarray,   outsize)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |     a. j. van der westhuysen      |
    !/                  |            jeff hanson            |
    !/                  |        eve-marie devaliere        |
    !/                  |                        fortran 95 |
    !/                  | last update :          4-jan-2013 |
    !/                  +-----------------------------------+
    !/
    !/    03-feb-2012 : origination, based on matlab code   ( version 4.05 )
    !/                  by jeff hanson & eve-marie devaliere
    !/    04-jan-2013 : inclusion in trunk                  ( version 4.08 )
    !/    20-dec-2016 : add count-histogram method similarly
    !/                  to intersect (s. zieger)            ( version 5.16 )
    !/
    !/    copyright 2009-2013 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    implicit none
    !
    !  1. purpose :
    !
    !     (i)  returns the union of inarray1 and inarray2.
    !     (ii) sort the resulting array in ascending order.
    !
    !  2. method
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     inarray   real    arr  input    input array
    !     insize    integer      input    size of input array
    !     outarray  real    arr  output   output array (sorted)
    !     outsize   integer      output   size of output array (number of
    !                                     unique elements)
    integer           :: insize1, insize2, outsize
    real              :: inarray1(insize1), inarray2(insize2)
    real, pointer     :: outarray(:)
    !
    intent (in)  inarray1, insize1, inarray2, insize2
    intent (out) outsize
    !
    !     local variables
    !     ----------------------------------------------------------------
    !     vidx1, vidx2 - array(s) in which the value is represented by
    !                    its index (i.e. histogram with frequency 1)
    !     n            - data range and size of possible intersections.
    !
    logical,allocatable :: vidx1(:),vidx2(:)
    integer,allocatable :: ipos1(:),ipos2(:)
    real,allocatable    :: temp(:)
    !
    integer             :: i, j
    integer             :: n, imin, imax
    integer             :: minv1,maxv1, minv2, maxv2
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      qsort     subr. private  quicksort algorithm
    !
    !  5. subroutines calling
    !
    !     combinesys
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     -
    !
    !  9. switches :
    !
    !     none defined yet.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/    --- setup input arrays. ---
    if ( (insize1+insize2).eq.0 ) then
      outsize = 0
      allocate(outarray(outsize))
    elseif ( insize1.eq.0 ) then
      outsize = insize2
      allocate(outarray(outsize))
      allocate(temp(outsize))
      do i=1,outsize
        outarray(i) = inarray2(i)
        temp(i)     = real(i)
      end do
      call qsort(outarray,temp,1,outsize)
    elseif ( insize2.eq.0 ) then
      outsize = insize1
      allocate(outarray(outsize),temp(outsize))
      do i=1,outsize
        outarray(i) = inarray1(i)
        temp(i)     = real(i)
      end do
      call qsort(outarray,temp,1,outsize)
    else
      outsize = 0
      !/      --- calculate the range of the two sets. ---
      minv1  = int(minval(inarray1))
      maxv1  = int(maxval(inarray1))
      minv2  = int(minval(inarray2))
      maxv2  = int(maxval(inarray2))
      !
      !/  --- allow extra elementes: zero, and make sure index is 1:n. ---
      imin = min(minv1,minv2)-1
      imax = max(maxv1,maxv2)+1
      n = imax-imin
      allocate(vidx1(n),vidx2(n))
      allocate(ipos1(n),ipos2(n))
      vidx1(1:n) = .false.
      vidx2(1:n) = .false.
      ipos1(1:n) = -9999
      ipos2(1:n) = -9999
      !/
      do i=1,insize1
        j = int(inarray1(i)-imin)
        if  ( .not.vidx1(j) ) then
          outsize = outsize + 1
          vidx1(j) = .true.
          ipos1(j) = i
        end if
      end do
      do i=1,insize2
        j = int(inarray2(i)-imin)
        if  ( .not.vidx1(j).and..not.vidx2(j) ) then
          outsize = outsize + 1
          vidx2(j) = .true.
          ipos2(j) = i
        end if
      end do
      allocate(outarray(outsize))
      i = 1
      do j=1,n
        if ( vidx1(j).and.i.le.outsize ) then
          outarray(i) = inarray1(ipos1(j))
          i = i + 1
        elseif ( vidx2(j).and.i.le.outsize ) then
          outarray(i) = inarray2(ipos2(j))
          i = i + 1
        end if
      end do
      deallocate(vidx1,vidx2)
      deallocate(ipos1,ipos2)
    end if
    !/
    return
    !/
  end subroutine union
  !/ end of union ------------------------------------------------------ /
  !/
  !/ ------------------------------------------------------------------- /
  integer function length(array,arrsize,val)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |     a. j. van der westhuysen      |
    !/                  |            jeff hanson            |
    !/                  |        eve-marie devaliere        |
    !/                  |                        fortran 95 |
    !/                  | last update :          4-jan-2013 |
    !/                  +-----------------------------------+
    !/
    !/    03-feb-2012 : origination, based on matlab code   ( version 4.05 )
    !/                  by jeff hanson & eve-marie devaliere
    !/    04-jan-2013 : inclusion in trunk                  ( version 4.08 )
    !/
    !/    copyright 2009-2013 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    implicit none
    !
    !  1. purpose :
    !
    !     find largest index in array with a value not equal to the
    !     filler value val.
    !     e.g. if val = 9999. and array = [x x x x 9999. 9999. 9999.],
    !     the function returns 4.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    integer :: arrsize
    real :: array(arrsize)
    real :: val
    !
    !     local variables
    !     ----------------------------------------------------------------
    real :: field
    integer :: i
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    if (arrsize.gt.0) then
      i = 1
      field = array(i)
      do while (field.ne.val)
        i = i+1
        if (i.gt.size(array)) exit
        field = array(i)
      end do
      length = i-1
    else
      length = 0
    end if
    return
  end function length
  !/ end of length ----------------------------------------------------- /
  !/
  !/ ------------------------------------------------------------------- /
  integer function findfirst(array,arrsize,val)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |     a. j. van der westhuysen      |
    !/                  |            jeff hanson            |
    !/                  |        eve-marie devaliere        |
    !/                  |                        fortran 95 |
    !/                  | last update :          4-jan-2013 |
    !/                  +-----------------------------------+
    !/
    !/    03-feb-2012 : origination, based on matlab code   ( version 4.05 )
    !/                  by jeff hanson & eve-marie devaliere
    !/    04-jan-2013 : inclusion in trunk                  ( version 4.08 )
    !/
    !/    copyright 2009-2013 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    implicit none
    !
    !  1. purpose :
    !
    !     fast algorithm to find the *first* index ind in array
    !     for which array(ind) = val. use only when there are
    !     no duplicates in array!
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    integer :: arrsize
    real :: array(arrsize)
    real :: val
    !
    !     local variables
    !     ----------------------------------------------------------------
    integer :: ind
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    ind = 1
    do while (ind.le.arrsize)
      if ( array(ind).eq.val ) exit
      ind = ind + 1
    end do
    if (ind.gt.arrsize) then
      findfirst = 0
    else
      findfirst = ind
    endif
    return
  end function findfirst
  !/ end of findfirst -------------------------------------------------- /
  !/
  !/ ------------------------------------------------------------------- /
  real function std(array,n)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |     a. j. van der westhuysen      |
    !/                  |            jeff hanson            |
    !/                  |        eve-marie devaliere        |
    !/                  |                        fortran 95 |
    !/                  | last update :          4-jan-2013 |
    !/                  +-----------------------------------+
    !/
    !/    03-feb-2012 : origination, based on matlab code   ( version 4.05 )
    !/                  by jeff hanson & eve-marie devaliere
    !/    04-jan-2013 : inclusion in trunk                  ( version 4.08 )
    !/
    !/    copyright 2009-2013 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    implicit none
    !
    !  1. purpose :
    !
    !     computes standard deviation.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     array    real    input array for which to compute the std dev.
    !     n        int     size of array
    !
    real    :: array(n)
    integer :: n
    !
    !     local variables
    !     ----------------------------------------------------------------
    real    :: mn
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    if (n.gt.1) then
      mn = sum(array)/n
      std = sqrt( 1/(real(n)-1)*sum( (array(:)-mn)**2 ) )
    else
      std = 0.
    end if
    return
  end function std
  !/ end of std -------------------------------------------------------- /
  !/
  recursive subroutine qsort(array,idx,lo,hi)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |            stefan zieger          |
    !/                  |                        fortran 95 |
    !/                  | last update :          6-sep-2016 |
    !/                  +-----------------------------------+
    !/
    !/    06-sep-2016 : origination, based on code by mirko ( version 5.16 )
    !/                  velic (bom, australia)
    !/
    !/    copyright 2009-2013 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    !
    !  1. purpose :
    !
    !     quicksort algorithm.
    !
    !  2. method
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     array   real    arr  in/out   input array
    !     idx     real    arr  in/out   original indices of input array
    !     lo      integer      input    first element
    !     hi      integer      input    last element
    !
    implicit none
    !/
    integer, intent(in) :: lo,hi
    real,intent(inout)  :: array(:),idx(:)
    !/
    !     local variables
    !     ----------------------------------------------------------------
    logical :: loop
    integer :: top, bot
    real    :: val, tmp
    !
    !  4. subroutines used :
    !
    !      name      type  scope    description
    !     ----------------------------------------------------------------
    !
    !  5. subroutines calling
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
    !
    !/    --- check array size and bounds. ---
    if ( size(array).eq. 0 ) then
      write(6,199)
      call abort
    else if ( size(array).ne.size(idx) ) then
      write(6,201)
      call abort
    else if ( lbound(array,1).gt.lo ) then
      write(6,203)
      call abort
    else if ( ubound(array,1).lt.hi ) then
      write(6,205)
      call abort
    end if
    !
    top = lo
    bot = hi
    val = array(int((lo+hi)/2))
    !
    loop = .true.
    do while ( loop )
      do while ( array(top).lt.val )
        top = top + 1
      end do
      do while ( val.lt.array(bot) )
        bot = bot - 1
      end do
      if ( top.lt.bot ) then
        !/        --- swap values at indices top and bot ---
        tmp = array(top)
        array(top) = array(bot)
        array(bot) = tmp
        !/        --- swap index values at indices top and bot ---
        tmp = idx(top)
        idx(top) = idx(bot)
        idx(bot) = tmp
        !
        top = top + 1
        bot = bot - 1
      else
        loop = .false.
      end if
    end do
    !/    --- recursive call quicksort ---
    if (lo.lt.top-1) call qsort(array,idx,lo,top-1)
    if (bot+1.lt.hi) call qsort(array,idx,bot+1,hi)
    !
    return
    !/
199 format (/' *** wavewatch iii error in w3systrk : '/            &
         '     qsort array is empty'                           )
201 format (/' *** wavewatch iii error in w3systrk : '/            &
         '     qsort array size and index array size mismatch' )
203 format (/' *** wavewatch iii error in w3systrk : '/            &
         '     qsort array index out of lower bound'           )
205 format (/' *** wavewatch iii error in w3systrk : '/            &
         '     qsort array index out of upper bound'           )
    !/
  end subroutine qsort
  !/ ------------------------------------------------------------------- /
  !/
  recursive subroutine qsort_desc(array,idx,lo,hi)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |            stefan zieger          |
    !/                  |                        fortran 95 |
    !/                  | last update :          6-sep-2016 |
    !/                  +-----------------------------------+
    !/
    !/    06-sep-2016 : origination, based on code by mirko ( version 5.16 )
    !/                  velic (bom, australia)
    !/
    !/    copyright 2009-2013 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    !
    !  1. purpose :
    !
    !     quicksort algorithm with descending sort order.
    !
    !  2. method
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     array   real    arr  in/out   input array
    !     lo      integer      input    first element
    !     hi      integer      input    last element
    !
    implicit none
    !/
    integer, intent(in) :: lo,hi
    real,intent(inout)  :: array(:),idx(:)
    !/
    !     local variables
    !     ----------------------------------------------------------------
    integer :: top, bot, i
    real    :: val, tmp
    logical :: loop
    !
    !  4. subroutines used :
    !
    !      name      type  scope    description
    !     ----------------------------------------------------------------
    !
    !  5. subroutines calling
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
    !
    !/    --- check array size and bounds. ---
    if ( size(array).eq. 0 ) then
      write(6,199)
      call abort
    else if ( size(array).ne.size(idx) ) then
      write(6,201)
      call abort
    else if ( lbound(array,1).gt.lo ) then
      write(6,203)
      call abort
    else if ( ubound(array,1).lt.hi ) then
      write(6,205)
      call abort
    end if
    !
    top = lo
    bot = hi
    val = array(int((lo+hi)/2))
    !
    loop = .true.
    do while ( loop )
      do while ( array(top).gt.val )
        top = top + 1
      end do
      do while ( val.gt.array(bot) )
        bot = bot - 1
      end do
      if ( top.lt.bot ) then
        !/        --- swap values at indices top and bot ---
        tmp = array(top)
        array(top) = array(bot)
        array(bot) = tmp
        !/        --- swap index values at indices top and bot ---
        tmp = idx(top)
        idx(top) = idx(bot)
        idx(bot) = tmp
        !
        top = top + 1
        bot = bot - 1
      else
        loop = .false.
      end if
    end do
    !/    --- recursive call quicksort ---
    if (lo.lt.top-1) call qsort_desc(array,idx,lo,top-1)
    if (bot+1.lt.hi) call qsort_desc(array,idx,bot+1,hi)
    !
    return
    !/
199 format (/' *** wavewatch iii error in w3systrk : '/            &
         '     qsort array is empty'                           )
201 format (/' *** wavewatch iii error in w3systrk : '/            &
         '     qsort array size and index array size mismatch' )
203 format (/' *** wavewatch iii error in w3systrk : '/            &
         '     qsort array index out of lower bound'           )
205 format (/' *** wavewatch iii error in w3systrk : '/            &
         '     qsort array index out of upper bound'           )
    !/
  end subroutine qsort_desc
  !/ ------------------------------------------------------------------- /
  !/
  function swapi4(int4) result(int4swp)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           s. zieger               |
    !/                  |                        fortran 90 |
    !/                  | last update :         03-jan-2017 |
    !/                  +-----------------------------------+
    !/
    !/    03-jan-2017 : origination                         ( version 5.16 )
    !/                                                        (s. zieger)
    !/
    !  1. purpose :
    !
    !     return a byte-swapped integer (size of 4 bytes)
    !
    !  2. source code :
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    integer(kind=4), intent(in)  :: int4
    integer(kind=4)              :: int4swp
    !/
    !     local variables
    !     ----------------------------------------------------------------
    integer(kind=1), dimension(4) :: bytein, byteout
    !/
    bytein  = transfer(int4, bytein)
    byteout = (/bytein(4),bytein(3),bytein(2),bytein(1)/)
    int4swp = transfer(byteout, int4swp)
    !/
    return
    !/
  end function swapi4
  !/ ------------------------------------------------------------------- /
  subroutine findijv4 (a   ,b   ,maxi,  maxj ,inda ,indb )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |     a. j. van der westhuysen      |
    !/                  |            jeff hanson            |
    !/                  |        eve-marie devaliere        |
    !/                  |                        fortran 95 |
    !/                  | last update :         03-mar-2017 |
    !/                  +-----------------------------------+
    !/
    !/    03-feb-2012 : origination, based on matlab code   ( version 4.05 )
    !/                  by jeff hanson & eve-marie devaliere
    !/    04-jan-2013 : inclusion in trunk                  ( version 4.08 )
    !/    03-mar-2017 : calls to intersect and union        ( version 5.16 )
    !/                  replaced  (s. zieger, bom, australia)
    !/
    !/    copyright 2009-2013 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    implicit none
    !
    !  1. purpose :
    !
    !     find a(i,j) indices of system "a" that lie over or along the
    !     fringes of system "b".
    !
    !  2. method
    !
    !     (i)  use an index matrix to map locations of wave systems in b
    !     (ii) avoid multiple use of intersect and union as in findijv3
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     a, b    type(system)  input   final set of tracked systems, for one time level
    !     maxi         int      input   number rows indices of wave field
    !     maxj         int      input   number column indices of wave field
    !     inda*, indb* int.a.   output  pointer array of indices for combining systems
    !
    type(system) :: a, b
    integer      :: maxi, maxj
    integer, pointer :: inda(:), indb(:)
    !
    intent (in) a, b, maxi,maxj
    !
    !     local variables
    !     ----------------------------------------------------------------
    !     posb         int              neighbour index
    !     posb_mm      int              neighbour index (-1,-1)
    !     posb_mp      int              neighbour index (-1,+1)
    !     posb_pm      int              neighbour index (+1,-1)
    !     posb_pp      int              neighbour index (+1,+1)
    !     tmpa*, tmpb* int.a.           array of indices for combining
    !     systems
    !
    integer      :: leng_ai,leng_bi
    integer      :: outa,outb,i,j,ind,outdumb
    integer      :: posb,posb_mm,posb_pm,posb_mp,posb_pp
    integer      :: ind_b2(maxi,maxj)
    real,allocatable :: tmpa(:),duma(:),tmpb(:)
    real,pointer     :: dumb(:)
    logical      :: found
    !
    !  4. subroutines used :
    !
    !      name      type  scope    description
    !     ----------------------------------------------------------------
    !      qsort     subr. private  quicksort algorithm
    !      unique    subr. private  return sorted unique numbers of an array
    !
    !  5. subroutines calling
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
    !
    nullify(dumb)
    !
    if (associated(inda)) deallocate(inda)
    if (associated(indb)) deallocate(indb)
    !
    leng_ai = length(real(a%i),size(a%i),real(9999))
    leng_bi = length(real(b%i),size(b%i),real(9999))
    !
    allocate(tmpa(leng_ai))
    allocate(tmpb(5*leng_ai))
    !
    tmpa(:) = 9999.
    tmpb(:) = 9999.
    !
    outa = 0
    outb = 0
    ind_b2(:,:) = 0
    !
    do ind=1,leng_bi
      i = b%i(ind)
      j = b%j(ind)
      if (ind_b2(i,j).eq.0) ind_b2(i,j) = ind
    end do
    !
    do ind=1,leng_ai
      i = a%i(ind)
      j = a%j(ind)
      posb = ind_b2(i,j)
      posb_mm = 0
      posb_pp = 0
      posb_mp = 0
      posb_pm = 0
      if (i.gt.1.and.j.gt.1)       posb_mm = ind_b2(i-1,j-1)
      if (i.gt.1.and.j.lt.maxj)    posb_mp = ind_b2(i-1,j+1)
      if (i.lt.maxi.and.j.lt.maxj) posb_pp = ind_b2(i+1,j+1)
      if (i.lt.maxi.and.j.gt.1)    posb_pm = ind_b2(i+1,j-1)
      found = .false.
      if (posb.ne.0) then
        outb = outb + 1
        tmpb(outb) = real(posb)
        if (.not.found) then
          outa = outa + 1
          tmpa(outa) = real(ind)
          found = .true.
        end if
      end if
      if (posb_mm.ne.0) then
        outb = outb + 1
        tmpb(outb) = real(posb_mm)
        if (.not.found) then
          outa = outa + 1
          tmpa(outa) = real(ind)
          found = .true.
        end if
      end if
      if (posb_mp.ne.0) then
        outb = outb + 1
        tmpb(outb) = real(posb_mp)
        if (.not.found) then
          outa = outa + 1
          tmpa(outa) = real(ind)
          found = .true.
        end if
      end if
      if (posb_pm.ne.0) then
        outb = outb + 1
        tmpb(outb) = real(posb_pm)
        if (.not.found) then
          outa = outa + 1
          tmpa(outa) = real(ind)
          found = .true.
        end if
      end if
      if (posb_pp.ne.0) then
        outb = outb + 1
        tmpb(outb) = real(posb_pp)
        if (.not.found) then
          outa = outa + 1
          tmpa(outa) = real(ind)
          found = .true.
        end if
      end if
    end do
    !
    !/    compact indices for wave systems in b.
    !/    check for empty arrays first.
    if (outb.gt.0) then
      call unique(tmpb,outb,dumb,outdumb)
      outb = outdumb
    end if
    allocate(indb(outb))
    if (outb.gt.0) indb(1:outb) = int(dumb(1:outb))
    if (associated(dumb)) deallocate(dumb)
    !
    !/    allocate output array and transfer content
    !/    for wave systems in a.
    allocate(inda(outa))
    if (outa.gt.0) then
      allocate(duma(outa))
      duma(:) = 0
      call qsort(tmpa(1:outa),duma(1:outa),1,outa)
      if (allocated(duma)) deallocate(duma)
      inda(1:outa) = int(tmpa(1:outa))
    end if
    !/
    if (allocated(tmpa)) deallocate(tmpa)
    if (allocated(tmpb)) deallocate(tmpb)
    !/
    return
    !/
  end subroutine findijv4
  !/ end of findijv4 --------------------------------------------------- /
  !/
  !/ ------------------------------------------------------------------- /
  !/
end module w3strkmd
!/
!/ end of module w3strkmd -------------------------------------------- /
!/
