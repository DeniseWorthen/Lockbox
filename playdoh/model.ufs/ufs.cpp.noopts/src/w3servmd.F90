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
module w3servmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         15-jan-2021 |
  !/                  +-----------------------------------+
  !/
  !/    for update log see individual subroutines.
  !/    12-jun-2012 : add /rtd option or rotated grid option.
  !/                  (jian-guo li)                       ( version 4.06 )
  !/    11-nov-2013 : smc and rotated grid incorporated in the main
  !/                  trunk                               ( version 4.13 )
  !/    18-aug-2016 : add dist_sphere: angular distance   ( version 5.11 )
  !/    01-mar-2016 : added w3thrtn and w3xyrtn for post  ( version 6.02 )
  !/                  processing rotated grid data
  !/    15-jan-2021 : added uv_to_mag_dir routine         ( version 7.12 )
  !/
  !/    copyright 2009-2012 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     in this module all wavewatch specific service routines have
  !     been gathered.
  !
  !  2. variables and types :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      ndstrc    int.  private  data set number for output of strace
  !                               (set in itrace).
  !      ntrace    int.  private  maximum number of trace prints in
  !                               strace (set in itrace).
  !     ----------------------------------------------------------------
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      itrace    subr. public   (re-) initialization for strace.
  !      strace    subr. public   enable subroutine tracing, usually
  !                               activated with the !/s switch.
  !      nextln    subr. public   get to next line in input command file.
  !      w3s2xy    subr. public   grid conversion routine.
  !      ej5p      r.f.  public   five parameter jonswap spectrum.
  !      wwdate    subr. public   get system date.
  !      wwtime    subr. public   get system time.
  !      extcde    subr. public   abort program with exit code.
  !     four subs for rotated grid are appended to this module.  as they
  !     are shared with smc grid, they are not quoted by option /rtd but
  !     are available for general use.     jgli12jun2012
  !     w3spectn       turns wave spectrum anti-clockwise by angld
  !     w3acturn       turns wave action(k,nth) anti-clockwise by angld.
  !     w3lltoeq       convert standard into rotated lat/lon, plus angld
  !     w3eqtoll       revers of the lltoeq, but angld unchanged.
  !     w3thtrn        turns direction value anti-clockwise by angld
  !     w3xytrn        turns 2d vectors anti-clockwise by angld
  !
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !     none.
  !
  !  5. remarks :
  !
  !  6. switches
  !
  !       !/s    enable subroutine tracing using strace in this module.
  !
  !  7. source code :
  !
  !/ ------------------------------------------------------------------- /
  ! module default
  implicit none
  public
  !
  integer, private        :: ndstrc = 6, ntrace = 0
  !
contains
  !/ ------------------------------------------------------------------- /
  subroutine itrace (nds, nmax)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         23-nov-1999 |
    !/                  +-----------------------------------+
    !/
    !/    23-nov-1999 : first version of routine.           ( version 2.00 )
    !/
    !  1. purpose :
    !
    !     (re-) initialization for module version of strace.
    !
    !  3. parameter list
    !     ----------------------------------------------------------------
    !       nds     int.   i   data set number ofr trace file.
    !       nmax    int.   i   maximum number of traces per routine.
    !     ----------------------------------------------------------------
    !
    !     private to module :
    !     ----------------------------------------------------------------
    !       ndstrc  int.  output unit number for trace.     ( from nds  )
    !       ntrace  int.  maximum number of trace prints.   ( from nmax )
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     none.
    !
    !  5. called by :
    !
    !     any program, multiple calls allowed.
    !
    !  9. switches :
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: nds, nmax
    !/
    !/ ------------------------------------------------------------------- /
    !/
    ntrace = max ( 0 , nmax )
    ndstrc = nds
    !
    return
    !/
    !/ end of itrace ----------------------------------------------------- /
    !/
  end subroutine itrace
  !/ ------------------------------------------------------------------- /
  subroutine strace (ient, sname)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         25-jan-2000 |
    !/                  +-----------------------------------+
    !/                                   original version by n. booij, dut
    !/
    !/    30-mar-1993 : final fortran 77                    ( version 1.18 )
    !/    23-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    25-jan-2000 : force flushing of uniit.            ( version 2.00 )
    !/                  this was taken out around version 3.01.
    !/
    !  1. purpose :
    !
    !     keep track of entered subroutines.
    !
    !  3. parameter list
    !     ----------------------------------------------------------------
    !       ient    int.  i/o  number of times that strace has been
    !                          called by the routine.
    !       sname   char.  i   name of the subroutine (max. 6 characters)
    !     ----------------------------------------------------------------
    !
    !     private to module :
    !     ----------------------------------------------------------------
    !       ndstrc  int.  output unit number for trace.
    !       ntrace  int.  maximum number of trace prints.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     none.
    !
    !  5. called by :
    !
    !     any program, after private variables have been set by ntrace.
    !
    !  9. switches :
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(inout)  :: ient
    character, intent(in)   :: sname*(*)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    if (ntrace.eq.0 .or. ient.ge.ntrace) return
    !
    ient = ient + 1
    if (ient.eq.1) then
      write (ndstrc,10) sname
    else
      write (ndstrc,11) sname, ient
    end if
    !
    return
    !
    ! formats
    !
10  format (' ---> trace subr : ',a6)
11  format (' ---> trace subr : ',a6,'  entry: ',i6)
    !/
    !/ end of strace ----------------------------------------------------- /
    !/
  end subroutine strace
  !/ ------------------------------------------------------------------- /
  subroutine nextln ( chckc , ndsi , ndse )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         10-dec-2014 |
    !/                  +-----------------------------------+
    !/
    !/    15-jan-1999 : final fortran 77                    ( version 1.18 )
    !/    18-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    10-dec-2014 : skip blank lines and leading blanks ( version 5.04 )
    !/
    !  1. purpose :
    !
    !     sets file pointer to next active line of input file, by skipping
    !     blank lines and lines starting with the character chckc. leading
    !     white space is allowed before the character chckc.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       chckc   c*1   i  check character for defining comment line.
    !       ndsi    int.  i  input dataset number.
    !       ndse    int.  i  error output dataset number.
    !                        (no output if ndse < 0).
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !       strace ( !/s switch )
    !
    !  5. called by :
    !
    !       any routine.
    !
    !  6. error messages :
    !
    !     - on eof or error in input file.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: ndsi, ndse
    character, intent(in)   :: chckc*1
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ierr
    character(128)          :: msg
    character(256)          :: line, test
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
100 continue
    ! read line
    read ( ndsi, 900, end=800, err=801, iostat=ierr, iomsg=msg ) line
    ! leading blanks removed and placed on the right
    test = adjustl ( line )
    if ( test(1:1).eq.chckc .or. len_trim(test).eq.0 ) then
      ! if comment or blank line, then skip
      goto 100
    else
      ! otherwise, backup to beginning of line
      backspace ( ndsi, err=802, iostat=ierr, iomsg=msg )
    endif
    return
    !
800 continue
    if ( ndse .ge. 0 ) write (ndse,910)
    call extcde ( 1 )
    !
801 continue
    if ( ndse .ge. 0 ) write (ndse,911) ierr, trim(msg)
    call extcde ( 2 )
    !
802 continue
    if ( ndse .ge. 0 ) write (ndse,912) ierr, trim(msg)
    call extcde ( 3 )
    !
    ! formats
    !
900 format (a)
910 format (/' *** wavewatch iii error in nextln : '/ &
         '     premature end of input file'/)
911 format (/' *** wavewatch iii error in nextln : '/ &
         '     error in reading from file'/           &
         '     iostat =',i5,/                         &
         '     iomsg = ',a/)
912 format (/' *** wavewatch iii error in nextln : '/ &
         '     error on backspace'/                   &
         '     iostat =',i5,/                         &
         '     iomsg = ',a/)
    !/
    !/ end of nextln ----------------------------------------------------- /
    !/
  end subroutine nextln
  !/ ------------------------------------------------------------------- /
  subroutine w3s2xy ( nsea, msea, mx, my, s, mapsf, xy )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii            noaa/nmc |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         23-nov-1999 |
    !/                  +-----------------------------------+
    !/
    !/    11-dec-1996 : final fortran 77                    ( version 1.18 )
    !/    23-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/
    !  1. purpose :
    !
    !     convert a data array on the storage grid to a data array on the
    !     full spatial grid. land and ice points in the full grid are
    !     not touched. output array of conventional type xy(ix,iy).
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       nsea    int.   i    number of sea points.
    !       msea, mx, my
    !               int.   i    array dimensions.
    !       s       r.a.   i    data on storage grid.
    !       mapsf   i.a.   i    storage map for ix and iy, resp.
    !       xy      r.a.   o    data on xy grid.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     none.
    !
    !  5. called by :
    !
    !     any wavewatch iii routine.
    !
    !  9. switches :
    !
    !     none.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: msea, nsea, mx, my, mapsf(msea,2)
    real, intent(in)        :: s(msea)
    real, intent(out)       :: xy(mx,my)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: isea, ix, iy
    !/
    !/ ------------------------------------------------------------------- /
    !/
    do isea=1, nsea
      ix     = mapsf(isea,1)
      iy     = mapsf(isea,2)
      xy(ix,iy) = s(isea)
    end do
    !/
    !/ end of w3s2xy ----------------------------------------------------- /
    !/
  end subroutine w3s2xy
  !/ ------------------------------------------------------------------- /
  real function ej5p ( f, alfa, fp, yln, siga, sigb )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         23-nov-1999 |
    !/                  +-----------------------------------+
    !/
    !/    23-amy-1985 : original by g. ph. van vledder.
    !/    23-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/
    !  1. purpose :
    !
    !     computation of spectral density using a 5-parameter
    !     jonswap-spectrum
    !
    !  2. method
    !
    !     ej5p(f) = a.exp(b + ln(y).exp(c))
    !
    !     where: a = alfa * 0.06175 * f**(-5)
    !            b = -1.25*(fp/f)**4
    !            c = -0.5 * ((f - fp)/(sig * fp))**2
    !     and
    !            grav**2/(2.pi)**4 = 0.06175
    !
    !  3. parameters :
    !
    !     parameter list
    !
    !     ----------------------------------------------------------------
    !       f       real   i    frequency in hz
    !       alfa    real   i    energy scaling factor
    !       fp      real   i    peak frequency in hz
    !       yln     real   i    peak overshoot factor, given by ln-value
    !       siga    real   i    spectral width, for f < fp
    !       sigb    real   i    spectral width, for f > fp
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     none.
    !
    !  5. called by :
    !
    !     any.
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !     expmin is a machine dependant constant such that
    !     exp(expmin) can be successfully evaluated without
    !     underflow by the compiler supllied exp routine.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     none.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: f, alfa, fp, yln, siga, sigb
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    real                    :: sig, a, b, c
    real, save              :: eps=1.e-4, expmin=-180.
    !/
    !/ ------------------------------------------------------------------- /
    !/
    if(f.lt.eps) then
      ej5p = 0.0
      return
    end if
    !
    a = alfa * 0.06175 / f**5
    b = -1.25 * (fp/f)**4
    b = max(b,expmin)
    !
    if (yln.lt.eps) then
      ej5p = a * exp(b)
    else
      if( f.le.fp) then
        sig = siga
      else
        sig = sigb
      end if
      c = -0.5 * ((f - fp)/(sig * fp))**2
      c = max(c,expmin)
      ej5p = a * exp(b + exp(c) * yln)
    end if
    !
    return
    !/
    !/ end of nextln ----------------------------------------------------- /
    !/
  end function ej5p
  !/ ------------------------------------------------------------------- /
  real function dist_sphere ( lo1,la1,lo2,la2 )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           f. ardhuin              |
    !/                  |                        fortran 90 |
    !/                  | last update :         18-aug-2016 |
    !/                  +-----------------------------------+
    !/
    !/    18-aug-2016 :  creation                           ( version 5.11 )
    !/
    !  1. purpose :
    !
    !     computes distance between two points on a sphere
    !
    !  2. method
    !
    !
    !  3. parameters :
    !
    !     parameter list
    !
    !     ----------------------------------------------------------------
    !       lo1     real   i    longitude of 1st point
    !       la1     real   i    latitude of 1st point
    !       lo2     real   i    longitude of 2nd point
    !       la2     real   i    latitude of 2nd point
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     none.
    !
    !  5. called by :
    !
    !     ww3_bounc
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !     none.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     none.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: lo1, la1, lo2, la2
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !  none
    !/
    !/ ------------------------------------------------------------------- /
    !/
    dist_sphere=acos(sin(la2*dera)*sin(la1*dera)+ &
         cos(la2*dera)*cos(la1*dera)*cos((lo2-lo1)*dera))*rade
    !
    return
    !/
    !/ end of nextln ----------------------------------------------------- /
    !/
  end function dist_sphere
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine wwdate (strng)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         26-dec-2012 |
    !/                  +-----------------------------------+
    !/
    !/    23-dec-1998 : final fortran 77                    ( version 1.18 )
    !/    23-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    18-sep-2000 : pgi switch added                    ( version 2.04 )
    !/    13-mar-2001 : lf95 switch added                   ( version 2.09 )
    !/    08-may-2002 : replace obsolete switches with f90  ( version 2.21 )
    !/    26-dec-2012 : modified obsolete declarations.     ( version 4.11 )
    !/
    !  1. purpose :
    !
    !     get date from machine dependent routine.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       strng   c*10   o   string with date in format yyyy/mm/dd
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     machine dependent.
    !
    !  5. called by :
    !
    !     any routine.
    !
    !  9. switches :
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    character, intent(out)  :: strng*10
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    character(len=8)        :: date
    character(len=10)       :: time
    character(len=5)        :: zone
    integer                 :: values(8)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    strng = '----/--/--'
    call date_and_time ( date, time, zone, values )
    strng(1:4) = date(1:4)
    strng(6:7) = date(5:6)
    strng(9:10) = date(7:8)
    !
    !
    return
    !/
    !/ end of wwdate ----------------------------------------------------- /
    !/
  end subroutine wwdate
  !/ ------------------------------------------------------------------- /
  subroutine wwtime (strng)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         26-dec-2012 |
    !/                  +-----------------------------------+
    !/
    !/    23-dec-1998 : final fortran 77                    ( version 1.18 )
    !/    23-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    18-sep-2000 : pgi switch added                    ( version 2.04 )
    !/    13-mar-2001 : lf95 switch added                   ( version 2.09 )
    !/    08-may-2002 : replace obsolete switches with f90  ( version 2.21 )
    !/    26-dec-2012 : modified obsolete declarations.     ( version 4.11 )
    !/
    !  1. purpose :
    !
    !     get time from machine dependent routine.
    !
    !  2. method :
    !
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       strng   c*8    o   string with time in format hh:mm:ss
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     machine dependent.
    !
    !  5. called by :
    !
    !     any routine.
    !
    !  9. switches :
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    character, intent(out)  :: strng*8
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    character(len=8)        :: date
    character(len=10)       :: time
    character(len=5)        :: zone
    integer                 :: values(8)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    strng = '--:--:--'
    call date_and_time ( date, time, zone, values )
    strng(1:2) = time(1:2)
    strng(4:5) = time(3:4)
    strng(7:8) = time(5:6)
    !
    return
    !/
    !/ end of wwtime ----------------------------------------------------- /
    !/
  end subroutine wwtime
  !/ ------------------------------------------------------------------- /
  subroutine extcde ( iexit, unit, msg, file, line, comm )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         06-jun-2018 |
    !/                  +-----------------------------------+
    !/
    !/    06-jan-1998 : final fortran 77                    ( version 1.18 )
    !/    23-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    10-dec-2014 : add checks for allocate status      ( version 5.04 )
    !/    11-mar-2015 : allow non-error exit (iexit=0)      ( version 5.04 )
    !/    20-jan-2017 : add optional mpi communicator arg   ( version 6.02 )
    !/    06-jun-2018 : add optional mpi                    ( version 6.04 )
    !/
    !  1. purpose :
    !
    !     perform a program stop with an exit code.
    !
    !     if exit code iexit=0, then it is not an error, but
    !     a stop has been requested by the calling routine:
    !     wait for other processes in communicator to catch up.
    !
    !     if exit code iexit.ne.0, then abort program w/out
    !     waiting for other processes to catch up (important for example
    !     when not all processes are used by ww3).
    !
    !  2. method :
    !
    !     machine dependent.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       iexit   int.   i   exit code to be used.
    !       unit    int.   i   (optional) file unit to write error message
    !       msg     str.   i   (optional) error message
    !       file    str.   i   (optional) name of source code file
    !       line    int.   i   (optional) line number in source code file
    !       comm    int.   i   (optional) mpi communicator
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !  5. called by :
    !
    !     any.
    !
    !  9. switches :
    !
    !     !/mpi  mpi finalize interface if active
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    include "mpif.h"
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in) :: iexit
    integer,      intent(in), optional :: unit
    character(*), intent(in), optional :: msg
    character(*), intent(in), optional :: file
    integer,      intent(in), optional :: line
    integer,      intent(in), optional :: comm
    !/
    !/ ------------------------------------------------------------------- /
    !/
    integer                 :: ierr_mpi
    logical                 :: run
    integer                 :: iun
    character(256)          :: lmsg = ""
    character(6)            :: lstr
    character(10)           :: prefix = "ww3 error:"
    !/
    !/ set file unit for error output
    !/
    iun = 0
    if (present(unit)) iun = unit
    !/
    !/ report error message
    !/
    if (present(msg)) then
      write (iun,"(a)") prefix//" "//trim(msg)
    end if
    !/
    !/ report context
    !/
    if ( present(file) ) then
      lmsg = trim(lmsg)//" file="//trim(file)
    end if
    if ( present(line) ) then
      write (lstr,'(i0)') line
      lmsg = trim(lmsg)//" line="//trim(lstr)
    end if
    if ( len_trim(lmsg).gt.0 ) then
      write (iun,"(a)") prefix//trim(lmsg)
    end if
    !/
    !/ handle mpi exit
    !/
    call mpi_initialized ( run, ierr_mpi )
    if ( run ) then
      if ( iexit.eq.0 ) then ! non-error state
        if ( present(comm) ) call mpi_barrier ( comm, ierr_mpi )
        call mpi_finalize (ierr_mpi )
      else ! error state
        write(*,'(/a,i6/)') 'extcde mpi_abort, iexit=', iexit
        if (present(unit)) then
          write(*,'(/a,i6/)') 'extcde unit=', unit
        end if
        if (present(msg)) then
          write(*,'(/2a/)') 'extcde msg=', msg
        end if
        if (present(file)) then
          write(*,'(/2a/)') 'extcde file=', file
        end if
        if (present(line)) then
          write(*,'(/a,i8/)') 'extcde line=', line
        end if
        if (present(comm)) then
          write(*,'(/a,i6/)') 'extcde comm=', comm
        end if
        call mpi_abort ( mpi_comm_world, iexit, ierr_mpi )
      end if
    end if
    !/
    !/ handle non-mpi exit
    !/
    call exit ( iexit )
    !/
    !/ end of extcde ----------------------------------------------------- /
    !/
  end subroutine extcde
  !/ ------------------------------------------------------------------- /
  !  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  this subroutine turn the wave spectrum by an fixed angle anti-clockwise
  !  so that it may be used in the rotated or stanadard system.
  !  first created:   26 aug 2005   jian-guo li
  !  last modified:   21 feb 2008   jian-guo li
  !
  ! subroutine interface:
  subroutine w3spectn( nfreq, ndirc, alpha, spectr )
    ! description:
    !   rotates wave spectrum anticlockwise by angle alpha in degree
    !   this routine is distinct from w3acturn since orders spectrum as freq, dirn
    !
    ! subroutine arguments
    integer, intent(in) :: nfreq, ndirc         ! no. freq and dirn bins
    real,    intent(in) :: alpha                ! turning angle (degrees)
    real, intent(inout) :: spectr(nfreq,ndirc)  ! wave spectrum in/out
    ! local variables
    integer :: ii, jj, kk, nsft
    real    :: ddirc, frac, cnst
    real, dimension(nfreq)      ::  wrkfrq, tmpfrq
    real, dimension(nfreq,ndirc)::  wrkspc
    ! check input bin numbers
    if( (nfreq .lt. 0) .or. (ndirc .lt. 0) )  then
      print*, " invalid bin number nf or nd", nfreq, ndirc
      return
    else
      ddirc=360.0/float(ndirc)
    endif
    ! work out shift bin number and fraction
    cnst=alpha/ddirc
    nsft=int( cnst )
    frac= cnst - float( nsft )
    !     print*, ' nsft and frac =', nsft, frac
    ! shift nsft bins if >=1
    if( abs(nsft) .ge. 1 )  then
      do ii=1, ndirc
        ! wave spectral direction bin number is assumed to increase anti-clockwise from east
        ! so shift nsft bins anticlockwise results in local bin number decreasing by nsft
        jj=ii - nsft
        ! as nsft may be either positive or negative depends on alpha, wrapping may
        ! happen in either ends of the bin number train
        if( jj > ndirc )  jj=jj - ndirc
        if( jj < 1     )  jj=jj + ndirc
        ! copy the selected bin to the loop bin number
        wrkspc(:,ii)=spectr(:,jj)
      enddo
      ! if nsft=0, no need to shift, simply copy
    else
      wrkspc = spectr
    endif
    ! pass fraction of wave energy in frac direction
    ! wave spectral direction bin number is assumed to increase anti-clockwise from east
    ! so positive frac or anticlock case, smaller bin upstream
    if( frac > 0.0 ) then
      tmpfrq=wrkspc(:,ndirc)*frac
      do kk=1, ndirc
        wrkfrq=wrkspc(:,kk)*frac
        spectr(:,kk)=wrkspc(:,kk) - wrkfrq + tmpfrq
        tmpfrq=wrkfrq
      enddo
    else
      ! negative or clockwise case, larger bin upstream
      tmpfrq=wrkspc(:,1)*frac
      do kk=ndirc, 1, -1
        wrkfrq=wrkspc(:,kk)*frac
        spectr(:,kk)=wrkspc(:,kk) + wrkfrq - tmpfrq
        tmpfrq=wrkfrq
      enddo
    endif
    ! spectral turning completed
    return
  end subroutine w3spectn
  !
  !  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  this subroutine turn the wave action by an angle (deg) anti-clockwise
  !  so that it may be used in the rotated or stanadard system.
  !  first created:   26 aug 2005   jian-guo li
  !  last modified:    9 oct 2008   jian-guo li
  !
  ! subroutine interface:
  subroutine w3acturn( ndirc, nfreq, alpha, spectr )
    ! description:
    !   rotates wave spectrum anticlockwise by angle alpha
    !   routine is distinct from w3spectn since orders spectrum as dirn, freq
    !
    ! subroutine arguments
    integer, intent(in) :: nfreq, ndirc          ! no. freq and dirn bins
    real,    intent(in) :: alpha                 ! turning angle (degrees)
    real, intent(inout) :: spectr(ndirc, nfreq)  ! wave action in/out
    ! local variables
    integer :: ii, jj, kk, nsft
    real    :: ddirc, frac, cnst
    real, dimension(nfreq)      ::  wrkfrq, tmpfrq
    real, dimension(ndirc,nfreq)::  wrkspc
    ! check input bin numbers
    if( (nfreq .lt. 0) .or. (ndirc .lt. 0) )  then
      print*, " invalid bin number nf or nd", nfreq, ndirc
      return
    else
      ddirc=360.0/float(ndirc)
    endif
    ! work out shift bin number and fraction
    cnst=alpha/ddirc
    nsft=int( cnst )
    frac= cnst - float( nsft )
    !     print*, ' nsft and frac =', nsft, frac
    ! shift nsft bins if >=1
    if( abs(nsft) .ge. 1 )  then
      do ii=1, ndirc
        ! wave spectral direction bin number is assumed to increase anti-clockwise from east
        ! so shift nsft bins anticlockwise results in local bin number decreasing by nsft
        jj=ii - nsft
        ! as nsft may be either positive or negative depends on alpha, wrapping may
        ! happen in either ends of the bin number train
        if( jj > ndirc )  jj=jj - ndirc
        if( jj < 1     )  jj=jj + ndirc
        ! copy the selected bin to the loop bin number
        wrkspc(ii,:)=spectr(jj,:)
      enddo
      ! if nsft=0, no need to shift, simply copy
    else
      wrkspc = spectr
    endif
    ! pass fraction of wave energy in frac direction
    ! wave spectral direction bin number is assumed to increase anti-clockwise from east
    ! so positive frac or anticlock case, smaller bin upstream
    if( frac > 0.0 ) then
      tmpfrq=wrkspc(ndirc,:)*frac
      do kk=1, ndirc
        wrkfrq=wrkspc(kk,:)*frac
        spectr(kk,:)=wrkspc(kk,:) - wrkfrq + tmpfrq
        tmpfrq=wrkfrq
      enddo
    else
      ! negative or clockwise case, larger bin upstream
      tmpfrq=wrkspc(1,:)*frac
      do kk=ndirc, 1, -1
        wrkfrq=wrkspc(kk,:)*frac
        spectr(kk,:)=wrkspc(kk,:) + wrkfrq - tmpfrq
        tmpfrq=wrkfrq
      enddo
    endif
    ! spectral turning completed
    return
  end subroutine w3acturn
  !
  !li  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !li
  !li  merged um source code for rotated grid, consisting the following
  !li  original subroutines in um 6.1
  !li    lltoeq1a  wcoeff1a  and  lbcrotwinds1
  !li  the last subroutine is modified to process only one level winds
  !li  cpp directives are removed and required header c_pi.h inserted.
  !li         jian-guo li     26 may 2005
  !li
  !li  the wcoeff1a subroutine is merged into lltoeq to reduce repetition
  !li  of the same calculations. subroutine interface changed to
  !li  lltoeqangle
  !li         jian-guo li     23 aug 2005
  !li
  !li  subroutine w3lltoeq   --------------------------------------------
  !li
  !li  purpose:  calculates latitude and longitude on equatorial
  !li            latitude-longitude (eq) grid used in regional
  !li            models from input arrays of latitude and
  !li            longitude on standard grid. both input and output
  !li            latitudes and longitudes are in degrees.
  !li            also calculate rotation angle in degree to tranform
  !li            standard wind velocity into equatorial wind.
  !li            valid for 0<phi_pole<90 or new pole in n. hemisphere.
  !li
  !* arguments:--------------------------------------------------------
  subroutine w3lltoeq ( phi, lambda, phi_eq, lambda_eq, angled, phi_pole, &
       lambda_pole, points )
    integer:: points    !in  number of points to be processed
    real :: phi_pole,  & !in  latitude of equatorial lat-lon pole
         &        lambda_pole  !inout  longitude of equatorial lat-lon pole
    real, dimension(points) ::         &
         &        phi,       & !in  latitude
         &        lambda,    & !in  longitude
         &        angled,    & !out turning angle in deg for standard wind
         &        lambda_eq, & !out longitude in equatorial lat-lon coords
         &        phi_eq       !out latitude in equatorial lat-lon coords
    ! define local varables:-----------------------------------------------
    real(kind=8) :: a_lambda, a_phi, e_lambda, e_phi, sin_phi_pole, cos_phi_pole, &
         term1, term2, arg, lambda_zero, lambda_pole_keep
    integer      :: i
    real(kind=8), parameter :: small=1.0e-6
    ! double precision versions of values in constants.ftn:
    real(kind=8), parameter         :: pi = 3.141592653589793
    real(kind=8), parameter         :: recip_pi_over_180 = 180. / pi
    real(kind=8), parameter         :: pi_over_180   = pi / 180.
    !*----------------------------------------------------------------------
    ! 1. initialise local constants
    ! scale lambda pole to range -180 to 180 degs
    lambda_pole_keep=lambda_pole
    if (lambda_pole.le.-180.0) lambda_pole=lambda_pole+360.d0
    if (lambda_pole.gt. 180.0) lambda_pole=lambda_pole-360.d0
    ! latitude of zeroth meridian
    lambda_zero=lambda_pole+180.d0
    ! sine and cosine of latitude of eq pole
    if (phi_pole >= 0.0) then
      sin_phi_pole =  sin(pi_over_180*phi_pole)
      cos_phi_pole =  cos(pi_over_180*phi_pole)
    else
      sin_phi_pole = -sin(pi_over_180*phi_pole)
      cos_phi_pole = -cos(pi_over_180*phi_pole)
    endif
    ! 2. transform from standard to equatorial latitude-longitude
    do i= 1, points
      ! scale longitude to range -180 to +180 degs
      a_lambda=lambda(i)-lambda_zero
      if(a_lambda.gt. 180.0) a_lambda=a_lambda-360.d0
      if(a_lambda.le.-180.0) a_lambda=a_lambda+360.d0
      ! convert latitude & longitude to radians
      a_lambda=pi_over_180*a_lambda
      a_phi=pi_over_180*phi(i)
      ! compute eq latitude using equation (4.4)
      arg=-cos_phi_pole*cos(a_phi)*cos(a_lambda) + sin_phi_pole*sin(a_phi)
      arg=min(arg, 1.d0)
      arg=max(arg,-1.d0)
      e_phi=asin(arg)
      phi_eq(i)=recip_pi_over_180*e_phi
      ! compute eq longitude using equation (4.6)
      term1 = sin_phi_pole*cos(a_phi)*cos(a_lambda) + cos_phi_pole*sin(a_phi)
      term2 = cos(e_phi)
      if(term2 .lt. small) then
        e_lambda=0.d0
      else
        arg=term1/term2
        arg=min(arg, 1.d0)
        arg=max(arg,-1.d0)
        e_lambda=recip_pi_over_180*acos(arg)
        e_lambda=sign(e_lambda,a_lambda)
      endif
      ! scale longitude to range 0 to 360 degs
      if(e_lambda.ge.360.0) e_lambda=e_lambda-360.d0
      if(e_lambda.lt.  0.0) e_lambda=e_lambda+360.d0
      lambda_eq(i)=e_lambda
      !li  calculate turning angle for standard wind velocity
      e_lambda=pi_over_180*lambda_eq(i)
      ! formulae used are from eqs (4.19) and (4.21)
      term2=sin(e_lambda)
      arg= sin(a_lambda)*term2*sin_phi_pole      &
           &    +cos(a_lambda)*cos(e_lambda)
      arg=min(arg, 1.d0)
      arg=max(arg,-1.d0)
      term1=recip_pi_over_180*acos(arg)
      angled(i)=sign(term1,term2)
      !li
    enddo
    ! reset lambda pole to the setting on entry to subroutine
    lambda_pole=lambda_pole_keep
    return
  end subroutine w3lltoeq
  !
  !li  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !li
  !li  merged um source code for rotated grid, consiting the following
  !li  original subroutines in um 6.1
  !li    eqtoll1a  wcoeff1a  and  lbcrotwinds1
  !li  the last subroutine is modified to process only one level winds
  !li  cpp directives are removed and required header c_pi.h inserted.
  !li         jian-guo li     26 may 2005
  !li
  !li  the wcoeff1a subroutine is merged into eqtoll to reduce repetition
  !li  of the same calculations. subroutine interface changed to
  !li  eqtollangle
  !li  first created:   jian-guo li     23 aug 2005
  !li  last modified:   jian-guo li     25 feb 2008
  !li
  !li  subroutine w3eqtoll  --------------------------------------------
  !li
  !li  purpose:  calculates latitude and longitude on standard grid
  !li            from input arrays of latitude and longitude on
  !li            equatorial latitude-longitude (eq) grid used
  !li            in regional models. both input and output latitudes
  !li            and longitudes are in degrees.
  !li            also calculate rotation angle in degree to tranform
  !li            standard wind velocity into equatorial wind.
  !li            valid for 0<phi_pole<90 or new pole in n. hemisphere.
  !li
  !li  arguments:--------------------------------------------------------
  subroutine w3eqtoll( phi_eq, lambda_eq, phi, lambda,   &
       &                 angled, phi_pole, lambda_pole, points )
    integer:: points      !in  number of points to be processed
    real :: phi_pole,   & !in  latitude of equatorial lat-lon pole
         &        lambda_pole   !in  longitude of equatorial lat-lon pole
    real, dimension(points) ::         &
         &        phi,       & !out latitude
         &        lambda,    & !out longitude (0 =< lon < 360)
         &        angled,    & !out turning angle in deg for standard wind
         &        lambda_eq, & !in  longitude in equatorial lat-lon coords
         &        phi_eq       !in  latitude in equatorial lat-lon coords
    ! local varables:------------------------------------------------------
    real(kind=8) :: e_lambda, e_phi, a_lambda, a_phi,                 &
         sin_phi_pole, cos_phi_pole,                       &
         term1, term2, arg, lambda_zero
    integer :: i
    real(kind=8), parameter :: small=1.0e-6
    ! double precision versions of values in constants.ftn:
    real(kind=8), parameter         :: pi = 3.141592653589793
    real(kind=8), parameter         :: recip_pi_over_180 = 180. / pi
    real(kind=8), parameter         :: pi_over_180   = pi / 180.
    ! ----------------------------------------------------------------------
    ! 1. initialise local constants
    ! latitude of zeroth meridian
    lambda_zero=lambda_pole+180.d0
    ! sine and cosine of latitude of eq pole
    if (phi_pole >= 0.0) then
      sin_phi_pole =  sin(pi_over_180*phi_pole)
      cos_phi_pole =  cos(pi_over_180*phi_pole)
    else
      sin_phi_pole = -sin(pi_over_180*phi_pole)
      cos_phi_pole = -cos(pi_over_180*phi_pole)
    endif
    ! 2. transform from equatorial to standard latitude-longitude
    do i= 1, points
      ! scale eq longitude to range -180 to +180 degs
      e_lambda=lambda_eq(i)
      if(e_lambda.gt. 180.0) e_lambda=e_lambda-360.d0
      if(e_lambda.lt.-180.0) e_lambda=e_lambda+360.d0
      ! convert eq latitude & longitude to radians
      e_lambda=pi_over_180*e_lambda
      e_phi=pi_over_180*phi_eq(i)
      ! compute latitude using equation (4.7)
      arg=cos_phi_pole*cos(e_phi)*cos(e_lambda) + sin_phi_pole*sin(e_phi)
      arg=min(arg, 1.d0)
      arg=max(arg,-1.d0)
      a_phi=asin(arg)
      phi(i)=recip_pi_over_180*a_phi
      ! compute longitude using equation (4.8)
      term1 = cos(e_phi)*sin_phi_pole*cos(e_lambda) - sin(e_phi)*cos_phi_pole
      term2 = cos(a_phi)
      if(term2.lt.small) then
        a_lambda=0.d0
      else
        arg=term1/term2
        arg=min(arg, 1.d0)
        arg=max(arg,-1.d0)
        a_lambda=recip_pi_over_180*acos(arg)
        a_lambda=sign(a_lambda,e_lambda)
        a_lambda=a_lambda+lambda_zero
      end if
      ! scale longitude to range 0 to 360 degs
      if(a_lambda.ge.360.0) a_lambda=a_lambda-360.d0
      if(a_lambda.lt.  0.0) a_lambda=a_lambda+360.d0
      lambda(i)=a_lambda
      !li  calculate turning angle for standard wind velocity
      a_lambda=pi_over_180*(lambda(i)-lambda_zero)
      ! formulae used are from eqs (4.19) and (4.21)
      term2=sin(e_lambda)
      arg=sin(a_lambda)*term2*sin_phi_pole     &
           &           +cos(a_lambda)*cos(e_lambda)
      arg=min(arg, 1.d0)
      arg=max(arg,-1.d0)
      term1=recip_pi_over_180*acos(arg)
      angled(i)=sign(term1,term2)
      !li
    enddo
    return
  end subroutine w3eqtoll
  !li
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine w3thrtn ( nsea, theta, angld, degrees )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii            noaa/nmc |
    !/                  |             a. saulter            |
    !/                  |                        fortran 90 |
    !/                  | last update :         01-mar-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-mar-2018 : added subroutine                   ( version 6.02 )
    !
    !  1. purpose :
    !     subroutine to de-rotate directions from rotated to standard pole
    !     reference system
    !
    !  2. method:
    !   rotates x,y vectors anticlockwise by angle alpha in radians
    !
    !/ ------------------------------------------------------------------- /
    use constants, only : dera, tpi, undef
    !
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in) :: nsea        ! number of sea points
    real,    intent(in) :: angld(nsea) ! turning angle (degrees)
    logical, intent(in) :: degrees     ! use degrees or radians
    real, intent(inout) :: theta(nsea) ! direction seapoint array
    !
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer :: isea
    !
    !/ ------------------------------------------------------------------- /
    ! apply the rotation
    !
    do isea=1, nsea
      if ( theta(isea) .ne. undef ) then
        if ( degrees ) then
          theta(isea) = theta(isea) - angld(isea)
          if ( theta(isea) .lt. 0 ) theta(isea) = theta(isea) + 360.0
        else
          theta(isea) = theta(isea) - angld(isea)*dera
          if ( theta(isea) .lt. 0 ) theta(isea) = theta(isea) + tpi
        end if
      endif
    end do
    return
  end subroutine w3thrtn
  !
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine w3xyrtn ( nsea, xvec, yvec, angld )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii            noaa/nmc |
    !/                  |             a. saulter            |
    !/                  |                        fortran 90 |
    !/                  | last update :         01-mar-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-mar-2018 : added subroutine                   ( version 6.02 )
    !
    !  1. purpose :
    !     subroutine to de-rotate x,y vectors from rotated to standard pole
    !     reference system
    !
    !  2. method:
    !   rotates x,y vectors anticlockwise by angle alpha in radians
    !
    !/ ------------------------------------------------------------------- /
    use constants, only : dera, tpi, undef
    !
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in) :: nsea        ! number of sea points
    real,    intent(in) :: angld(nsea) ! turning angle (degrees)
    real, intent(inout) :: xvec(nsea), yvec(nsea)
    !
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer :: isea
    real    :: xvtmp, yvtmp
    !
    !/ ------------------------------------------------------------------- /
    ! apply the rotation
    !
    do isea=1, nsea
      if (( xvec(isea) .ne. undef ) .and. &
           ( yvec(isea) .ne. undef )) then
        xvtmp = xvec(isea)*cos(angld(isea)*dera) + yvec(isea)*sin(angld(isea)*dera)
        yvtmp = yvec(isea)*cos(angld(isea)*dera) - xvec(isea)*sin(angld(isea)*dera)
        xvec(isea) = xvtmp
        yvec(isea) = yvtmp
      end if
    end do
    return
  end subroutine w3xyrtn
  !
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  !/
  subroutine strsplit(string,tab)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |          m. accensi               |
    !/                  |                        fortran 90 |
    !/                  | last update :         29-apr-2013 !
    !/                  +-----------------------------------+
    !/
    !/    29-mar-2013 : origination.                        ( version 4.10 )
    !/
    !  1. purpose :
    !
    !     splits string into words
    !
    !  2. method :
    !
    !     finds spaces and loops
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       string   str   o   string to be splitted
    !       tab      str   o   array of strings
    !     ----------------------------------------------------------------
    !
    character(len=*), intent(in)         :: string
    character(len=100), intent(inout)    :: tab(*)
    integer                              :: cnt, i
    character(len=1024)                  :: tmp_str, ori_str
    ! initializes arrays
    ori_str=adjustl(trim(string))
    tmp_str=ori_str
    cnt=0
    ! counts the number of substrings
    do while ((index(tmp_str,' ').ne.0) .and. (len_trim(tmp_str).ne.0))
      tmp_str=adjustl(tmp_str(index(tmp_str,' ')+1:))
      cnt=cnt+1
    enddo
    !
    ! reinitializes arrays
    !
    tmp_str=ori_str
    ! loops on each substring
    do i=1,cnt
      tab(i)=tmp_str(:index(tmp_str,' '))
      tmp_str=adjustl(tmp_str(index(tmp_str,' ')+1:))
    end do
    return
    !/
    !/ end of strsplit ----------------------------------------------------- /
    !/
  end subroutine strsplit
  !/
  !/ ------------------------------------------------------------------- /
  subroutine str_to_upper(str)
    character(*), intent(inout) :: str
    integer :: i
    do i = 1, len(str)
      select case(str(i:i))
      case("a":"z")
        str(i:i) = achar(iachar(str(i:i))-32)
      end select
    end do
    !/ end of str_to_upper
    !/ ------------------------------------------------------------------- /
  end subroutine str_to_upper
  !**********************************************************************
  !*                                                                    *
  !*********************************************************************
  subroutine diagonalize(a1,d,v,nrot)
    !*********************************************************************
    integer,                          intent(out)   :: nrot
    double precision, dimension(:)  , intent(out)   ::d
    double precision, dimension(:,:), intent(in)    ::a1  ! modified from inout to in by f.a. on 2018/01/21
    double precision, dimension(:,:), intent(out)   ::v
    integer    i,j,ip,iq,n
    double precision       c,g,h,s,sm,t,tau,theta,tresh
    double precision    , dimension(size(d)) ::b,z
    double precision, dimension(size(d),size(d)) :: a
    logical, dimension(size(d),size(d)) :: upper_triangle
    a=a1
    n=size(d)
    v(:,:)=0.
    upper_triangle(:,:)=.false.
    do i=1,n
      v(i,i)=1.
      b(i)=a(i,i)
      do j=i+1,n
        upper_triangle(i,j)=.true.
      enddo
    enddo
    d(:)=b(:)
    z(:)=0.0
    nrot=0
    do i=1,50
      sm=sum(abs(a),mask=upper_triangle)
      if (sm.eq.0.0) return
      tresh=merge(0.2*sm/n**2,0.0d0,i<4)
      do ip=1,n-1
        do iq=ip+1,n
          g=100.0*abs(a(ip,iq))
          if((i > 4).and.(abs(d(ip))+g.eq.abs(d(ip))) &
               .and.(abs(d(iq))+g.eq.abs(d(iq)))) then
            a(ip,iq)=0.0
          else if (abs(a(ip,iq)) > tresh) then
            h=d(iq)-d(ip)
            if (abs(h)+g == abs(h)) then
              t=a(ip,iq)/h
            else
              theta=0.5*h/a(ip,iq)
              t=1.0/(abs(theta)+sqrt(1.0+theta**2))
              if ( theta < 0.0) t=-t
            endif
            c=1.0/sqrt(1+t**2)
            s=t*c
            tau=s/(1.0+c)
            h=t*a(ip,iq)
            z(ip)=z(ip)-h
            z(iq)=z(iq)+h
            d(ip)=d(ip)-h
            d(iq)=d(iq)+h
            a(ip,iq)=0.0
            if (ip.ge.1) call rotate(a(1:ip-1,ip),a(1:ip-1,iq))
            !the if test was added by f.a. (2005/04/04) because of the following error:
            !subscript out of range. location: line 593 column 36 of 'cb_botsc.f90'
            !subscript number 1 has value 0 in array 'a'
            call rotate(a(ip,ip+1:iq-1),a(ip+1:iq-1,iq))
            call rotate(a(ip,iq+1:n),a(iq,iq+1:n))
            call rotate(v(:,ip),v(:,iq))
            nrot=nrot+1
          endif
        enddo
      enddo
      b(:)=b(:)+z(:)
      d(:)=b(:)
      z(:)=0.0
    enddo
    write(6,*) 'too many iterations in diagonalize'
  contains
    subroutine rotate(x1,x2)
      double precision, dimension(:), intent(inout) :: x1,x2
      double precision, dimension(size(x1)) :: mem
      mem(:)=x1(:)
      x1(:)=x1(:)-s*(x2(:)+x1(:)*tau)
      x2(:)=x2(:)+s*(mem(:)-x2(:)*tau)
    end subroutine rotate
  end subroutine diagonalize
  !/ ------------------------------------------------------------------- /
  subroutine uv_to_mag_dir(u, v, nsea, mag, dir, tolerance, conv)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           c. bunney               |
    !/                  |                        fortran 90 |
    !/                  | last update :         15-jan-2021 |
    !/                  +-----------------------------------+
    !/
    !/    15-jan-2021 : creation                            ( version 7.12 )
    !/
    !  1. purpose :
    !
    !     converts seapoint arrays formulated as u/v vectors into magnitude
    !     and direction arrays.
    !
    !     if mag and dir input parameters are not specificed then the
    !     conversion is performed in-place (u => mag, v => dir).
    !
    !  2. parameters
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       u/v       r.arr  i  array of u/v components
    !       nsea      int    i  number of sea points
    !       mag       r.arr  o  magnitude array            (optional)
    !       dir       r.arr  o  direction array (degrees)  (optional)
    !       tolerance real   i  minimum allowed magnitude  (optional)
    !       conv      char   i  ouput direciton convention (optional)
    !     ----------------------------------------------------------------
    !
    !  3. remarks
    !
    !     optional conv specifies direction convention. must be one of:
    !       'n'=nautical     : north=0, clockwise, direction-from (default)
    !       'o'=oceangraphic : north=0, clockwise, direction-to
    !       'c'=cartesian    : north=90, counter-clockwise, direction-to
    !
    !/ ------------------------------------------------------------------- /
    use constants, only: rade, undef
    real, intent(inout)             :: u(nsea), v(nsea)
    integer, intent(in)             :: nsea
    real, intent(out), optional     :: mag(nsea), dir(nsea)
    real, intent(in), optional      :: tolerance
    character, intent(in), optional :: conv
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !
    real :: tol, sgn, offset, tmp
    character :: dirconv
    integer :: isea
    logical :: inplace
    dirconv = 'n'
    tol = 1.0
    inplace = .true.
    if(present(tolerance)) tol = tolerance
    if(present(conv)) dirconv = conv
    if(present(mag) .and. present(dir)) inplace = .false.
    select case (conv)
    case('n')
      offset = 630.
      sgn = -1.
    case('o')
      offset = 450.
      sgn = -1.
    case('c')
      offset = 360.
      sgn = 1.
    case default
      write(*,*) "uv_to_mag_dir: unknown dir convention: ", dirconv
      call extcde(1)
    end select
    if(inplace) then
      do isea=1, nsea
        tmp = sqrt(u(isea)**2 + v(isea)**2)
        if(tmp .ge. tol) then
          v(isea) = mod(offset + (sgn * rade * atan2(v(isea), u(isea))), 360.)
          u(isea) = tmp
        else
          u(isea) = undef
          v(isea) = undef
        end if
      end do
    else
      do isea=1, nsea
        mag(isea) = sqrt(u(isea)**2 + v(isea)**2)
        if(mag(isea) .ge. tol) then
          dir(isea) = mod(offset + (sgn * rade * atan2(v(isea), u(isea))), 360.)
        else
          mag(isea) = undef
          dir(isea) = undef
        end if
      end do
    endif
  end subroutine uv_to_mag_dir
  !========================================================================
  !> write memory statistics if requested
  !!
  !> @details writes a single line of memory statistics
  !!
  !! @param[in]   iun               unit number
  !! @param[in]   msg               message
  !!
  !> @author mvertens@ucar.edu, denise.worthen@noaa.gov
  !> @date 06-01-2022
  subroutine print_memcheck(iun, msg)
    integer          , intent(in) :: iun
    character(len=*) , intent(in) :: msg
  end subroutine print_memcheck
  !/
  !/ end of module w3servmd -------------------------------------------- /
  !/
end module w3servmd
