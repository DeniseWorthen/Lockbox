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
module w3timemd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         23-feb-2024 |
  !/                  +-----------------------------------+
  !/
  !/    copyright 2009 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     routines for management of date and time.
  !
  !  2. variables and types :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      prftb     i.a.  private  base time for profiling.
  !      flprof    log.  private  flag for profiling initialization.
  !     ----------------------------------------------------------------
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      tick21    subr. public   increment a date and time array with
  !                               a given number of seconds.
  !      iymd21    i.f.  tick21   date increment function.
  !      dsec21    r.f.  public   calculate the difference in seconds
  !                               between two data/time arrays.
  !      tdiff     r.f.  public   calculate the difference in seconds
  !                               between two date/time arrays that
  !                               were generated from date_and_time
  !      mymd21    i.f.  dsec21   julian date function.
  !      stme21    subr. public   converts integer time to string.
  !      julday    i.f.  public   julian date function
  !      caldat    subr. public   transform julian day to date
  !      prinit    subr. public   initialize profiling.
  !      prtime    subr. public   get profiling time.
  !      d2j       subr. public   convert date array to julian date
  !      j2d       subr. public   convert julian date to date array
  !      t2d       subr. public   convert time array to date array
  !      tsub      i.d.  public   substract two time arrays in days
  !      tsubsec   i.d.  public   substract two time arrays in seconds
  !      u2d       subr. public   convert time units attribute to date array
  !      t2iso     subr. public   convert time array to iso time string
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      strace    subr. w3servmd subroutine tracing.
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !
  !  6. switches :
  !
  !  7. source code :
  !
  !/ ------------------------------------------------------------------- /
  !/
  !
  public
  !
  integer, private        :: prftb(8)
  logical, private        :: flprof = .false.
  character, public       :: caltype*8
  !
contains
  !/ ------------------------------------------------------------------- /
  subroutine tick21 ( time, dtime )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         29-nov-1999 |
    !/                  +-----------------------------------+
    !/                                based on tick of the gla gcm.
    !/
    !/    23-mar-1993 : final fortran 77                    ( version 1.18 )
    !/    29-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/
    !  1. purpose :
    !
    !     updates time information, dtime=0 converts to "legal" time.
    !     goes into the 21st century.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       time    i.a.  i/o  (1) current date in yyyymmdd format.
    !                          (2) current time in hhmmss format.
    !       dtime   real   i   time step in seconds.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !      iymd21    func. internal increment date in yyyymmdd format.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !     any other routine.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing using strace.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(inout)  :: time(2)
    real, intent(in)        :: dtime
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: nymd, nhms, nsec
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! zero increment: get "legal" date
    !
    nymd   = time(1)
    nhms   = time(2)
    if (dtime.eq.0.) then
      nymd = iymd21 (nymd,-1)
      nymd = iymd21 (nymd, 1)
    end if
    !
    ! convert and increment time :
    !
    nsec = nhms/10000*3600 + mod(nhms,10000)/100* 60 +        &
         mod(nhms,100) + nint(dtime)
    !
    ! check change of date :
    !
100 continue
    if (nsec.ge.86400)  then
      nsec = nsec - 86400
      nymd = iymd21 (nymd,1)
      goto 100
    end if
    !
200 continue
    if (nsec.lt.00000)  then
      nsec = 86400 + nsec
      nymd = iymd21 (nymd,-1)
      goto 200
    end if
    !
    nhms = nsec/3600*10000 + mod(nsec,3600)/60*100 + mod(nsec,60)
    !
    time(1) = nymd
    time(2) = nhms
    !
    return
    !/
    !/ internal function iymd21 ------------------------------------------ /
    !/
  contains
    !/ ------------------------------------------------------------------- /
    integer function iymd21 ( nymd ,m )
      !/
      !/                  +-----------------------------------+
      !/                  | wavewatch iii           noaa/ncep |
      !/                  |           h. l. tolman            |
      !/                  |                        fortran 90 |
      !/                  | last update :         18-jun-2020 |
      !/                  +-----------------------------------+
      !/                                based on incymd of the gla gcm.
      !/
      !/    18-oct-1998 : final fortran 77                    ( version 1.18 )
      !/    29-nov-1999 : upgrade to fortran 90               ( version 2.00 )
      !/    10-jan-2017 : add noleap option, 365 day calendar ( version 6.00 )
      !/    18-jun-2020 : add 360-day calendar option         ( version 7.08 )
      !/
      !  1. purpose :
      !
      !     increment date in yyyymmdd format by +/- 1 day.
      !
      !  3. parameters :
      !
      !     parameter list
      !     ----------------------------------------------------------------
      !       nymd    int.   i   old date in yymmdd format.
      !       m       int.   i   +/- 1 (day adjustment)
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
      !     any subroutine.
      !
      !  8. structure :
      !
      !     see source code.
      !
      !  9. switches :
      !
      !     !/s  enable subroutine tracing using strace.
      !
      ! 10. source code :
      !
      !/ ------------------------------------------------------------------- /
      !/
      implicit none
      !/
      !/ ------------------------------------------------------------------- /
      !/ parameter list
      !/
      integer, intent(in)     :: nymd, m
      !/
      !/ ------------------------------------------------------------------- /
      !/ local parameters
      !/
      integer                 :: ny, nm, nd
      integer, save           :: ndpm(12)
      logical                 :: leap
      !/
      !/ ------------------------------------------------------------------- /
      !/
      !
      ! declare the number of days in month depending on calendar
      !
      if (trim(caltype) .eq. '360_day' ) then
        ndpm=(/ 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30 /)
      else
        ndpm=(/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
      end if
      !
      ! "unpack" and increment date :
      !
      ny   = nymd / 10000
      nm   = mod(nymd,10000) / 100
      nm   = min ( 12 , max(1,nm) )
      nd   = mod(nymd,100) + m
      ! add override for simulations with no leap years
      if (trim(caltype) .eq. 'standard' ) then
        leap = mod(ny,400).eq.0 .or.                           &
             ( mod(ny,4).eq.0 .and. mod(ny,100).ne.0 )
      else
        leap = .false.
      end if
      !
      ! m = -1, change month if necessary :
      !
      if (nd.eq.0) then
        nm   = nm - 1
        if (nm.eq.0) then
          nm   = 12
          ny   = ny - 1
        endif
        nd   = ndpm(nm)
        if (nm.eq.2 .and. leap)  nd = 29
      end if
      !
      ! m = 1, leap year
      !
      if (nd.eq.29 .and. nm.eq.2 .and. leap)  go to 20
      !
      !        next month
      !
      if (nd.gt.ndpm(nm)) then
        nd = 1
        nm = nm + 1
        if (nm.gt.12) then
          nm = 1
          ny = ny + 1
        endif
      end if
      !
20    continue
      iymd21 = ny*10000 + nm*100 + nd
      !
      return
      !/
      !/ end of iymd21 ----------------------------------------------------- /
      !/
    end function iymd21
    !/
    !/ end of tick21 ----------------------------------------------------- /
    !/
  end subroutine tick21
  !/ ------------------------------------------------------------------- /
  real function dsec21 ( time1, time2 )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         18-jun-2020 |
    !/                  +-----------------------------------+
    !/
    !/    23-mar-1993 : final fortran 77                    ( version 1.18 )
    !/    29-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    05-jan-2001 : y2k leap year error correction.     ( version 2.05 )
    !/    18-jun-2020 : add 360-day calendar support        ( version 7.08 )
    !/
    !/
    !  1. purpose :
    !
    !     calculate the time difference in seconds between two times in
    !     yymmd hhmmmss formats.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       timen   i.a.   i   times, timen(1) is date in yyyymmdd format,
    !                          timen(2) is time in hhmmss format.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !      mymd21    func. internal calculate julian date.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !     any routine.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing using strace.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: time1(2), time2(2)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ny1, nd1, ny2, nd2, ns1, ns2, ns,   &
         nd, nst
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! convert dates and times :
    !
    ny1    = time1(1) / 10000
    nd1    = mymd21 ( time1(1) )
    ns1    = time1(2)/10000*3600 + mod(time1(2),10000)/100*60 + &
         mod(time1(2),100)
    !
    ny2    = time2(1) / 10000
    nd2    = mymd21 ( time2(1) )
    ns2    = time2(2)/10000*3600 + mod(time2(2),10000)/100*60 + &
         mod(time2(2),100)
    !
    ! number of days and seconds in difference :
    !
    nd     = nd2 - nd1
    !
    if ( ny1 .ne. ny2 ) then
      nst    = sign ( 1 , ny2-ny1 )
100   continue
      if (ny1.eq.ny2) goto 200
      if (nst.gt.0) then
        ny2    = ny2 - 1
        if (trim(caltype) .eq. '360_day' ) then
          nd     = nd  + mymd21 ( ny2*10000 + 1230 )
        else
          nd     = nd  + mymd21 ( ny2*10000 + 1231 )
        end if
      else
        if (trim(caltype) .eq. '360_day' ) then
          nd     = nd  - mymd21 ( ny2*10000 + 1230 )
        else
          nd     = nd  - mymd21 ( ny2*10000 + 1231 )
        end if
        ny2    = ny2 + 1
      endif
      goto 100
200   continue
    end if
    !
    ns     = ns2 - ns1
    !
    ! output of time difference :
    !
    dsec21 = real(ns) + 86400.*real(nd)
    !
    return
    !/
    !/ internal function mymd21 ------------------------------------------ /
    !/
  contains
    !/ ------------------------------------------------------------------- /
    integer function mymd21 ( nymd )
      !/
      !/                  +-----------------------------------+
      !/                  | wavewatch iii           noaa/ncep |
      !/                  |           h. l. tolman            |
      !/                  |                        fortran 90 |
      !/                  | last update :         18-jun-2020 |
      !/                  +-----------------------------------+
      !/                                based on modymd of the gla gcm.
      !/
      !/    19-oct-1998 : final fortran 77                    ( version 1.18 )
      !/    29-nov-1999 : upgrade to fortran 90               ( version 2.00 )
      !/    10-jan-2017 : add noleap option, 365 day calendar ( version 6.01 )
      !/    18-jun-2020 : add 360-day calendar support        ( version 7.08 )
      !/
      !  1. purpose :
      !
      !     convert date in yymmdd format to julian date.
      !
      !  3. parameters :
      !
      !     parameter list
      !     ----------------------------------------------------------------
      !       nymd    int.   i   date in yymmdd format.
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
      !     any subroutine.
      !
      !  8. structure :
      !
      !     see source code.
      !
      !  9. switches :
      !
      !     !/s  enable subroutine tracing using strace.
      !
      ! 10. source code :
      !
      !/ ------------------------------------------------------------------- /
      !/
      implicit none
      !/
      !/ ------------------------------------------------------------------- /
      !/ parameter list
      !/
      integer, intent(in)     :: nymd
      !/
      !/ ------------------------------------------------------------------- /
      !/ local parameters
      !/
      integer                 :: ny, nm, nd
      integer, save           :: ndpm(12)
      logical                 :: leap
      !/
      !/ ------------------------------------------------------------------- /
      !/
      !
      ! declare the number of days in month depending on calendar
      !
      if (trim(caltype) .eq. '360_day' ) then
        ndpm=(/ 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30 /)
      else
        ndpm=(/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
      end if
      !
      ! "unpack" and increment date :
      !
      ny   = nymd / 10000
      nm   = mod(nymd,10000) / 100
      nd   = mod(nymd,100)
      !allow override for noleap simulations
      if (trim(caltype) .eq. 'standard' ) then
        leap = mod(ny,400).eq.0 .or.                           &
             ( mod(ny,4).eq.0 .and. mod(ny,100).ne.0 )
      else
        leap=.false.
      endif
      !
      ! loop over months :
      !
      if (nm.gt.2 .and. leap)  nd = nd + 1
      !
40    continue
      if (nm.le.1)  go to 60
      nm = nm - 1
      nd = nd + ndpm(nm)
      go to 40
      !
60    continue
      mymd21 = nd
      !
      return
      !/
      !/ end of mymd21 ----------------------------------------------------- /
      !/
    end function mymd21
    !/
    !/ end of dsec21 ----------------------------------------------------- /
    !/
  end function dsec21
  !/ ------------------------------------------------------------------- /
  real function tdiff ( t1, t2 )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           arun chawla             |
    !/                  |           mark szyszka            |
    !/                  |                        fortran 90 |
    !/                  | last update :         02-feb-2014 |
    !/                  +-----------------------------------+
    !/
    !/    02-feb-2014 : original code         ( version 4.18 )
    !/
    !/
    !  1. purpose :
    !
    !     calculate the time difference in seconds between two time arrays
    !     that have been generated from the f90 internal function
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       tn      i.a.   i   this is an integer array returned from the
    !                          internal subroutine date_and_time. the type
    !                          is integer(8). individual values are
    !                          tn(1)    the year
    !                          tn(2)    the month
    !                          tn(3)    day of the month
    !                          tn(4)    time difference with utc in minutes
    !                          tn(5)    hour of the day
    !                          tn(6)    minutes of the hour
    !                          tn(7)    seconds of the minute
    !                          tn(8)    milli seconds of the second
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
    !     any routine.
    !
    !  7. remarks :
    !
    !     this code has been provided by mark szyszka of rpsgroup
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing using strace.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: t1(8), t2(8)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: a1, b1, c1, d1, a2, b2, c2, d2
    real                    :: e1, e2
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! convert dates and times :
    !
    a1 = (14-t1(2))/12
    b1 = t1(1) + 4800 - a1
    c1 = t1(2) + 12*a1 - 3
    d1 = t1(3) + (153*c1 + 2)/5 + 365*b1 + b1/4 -b1/100 + b1/400
    e1 = 3600.0*t1(5) + 60.0*(t1(6)-t1(4)) + t1(7) + t1(8)/1000.0
    !
    a2 = (14-t2(2))/12
    b2 = t2(1) + 4800 - a2
    c2 = t2(2) + 12*a2 - 3
    d2 = t2(3) + (153*c2 + 2)/5 + 365*b2 + b2/4 -b2/100 + b2/400
    e2 = 3600.0*t2(5) + 60.0*(t2(6)-t2(4)) + t2(7) + t2(8)/1000.0
    !
    tdiff = 86400.0*(d2-d1) + e2-e1
    !
    return
    !/
    !/ end of tdiff ------------------------------------------------------ /
    !/
  end function tdiff
  !/ ------------------------------------------------------------------- /
  subroutine stme21 ( time , dtme21 )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         23-nov-1999 |
    !/                  +-----------------------------------+
    !/
    !/    21-jun-1993 : final fortran 77                    ( version 1.18 )
    !/    23-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/
    !  1. purpose :
    !
    !     converts time to more readable string.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       time    i.a.  i   time in yyyymmdd hhmmss format.
    !                         time(1) < 0 indicates that time is not set.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !       none.
    !
    !  5. called by :
    !
    !       any subroutine/program.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: time(2)
    character, intent(out)  :: dtme21*23
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: iy, imo, id, ih, imi, is
    !/
    !/ ------------------------------------------------------------------- /
    !/
    if ( time(1) .lt. 0 ) then
      dtme21 = ' date and time not set.'
    else
      iy     = time(1) / 10000
      imo    = mod(time(1),10000) / 100
      id     = mod(time(1),100)
      ih     = time(2) / 10000
      imi    = mod(time(2),10000) / 100
      is     = mod(time(2),100)
      write (dtme21,900) iy, imo, id, ih, imi, is
    endif
    !
    return
    !
    ! formats
    !
900 format (i4.4,'/',i2.2,'/',i2.2,' ',i2.2,':',i2.2,':',i2.2,' utc')
    !/
    !/ end of stme21 ----------------------------------------------------- /
    !/
  end subroutine stme21
  !/ ------------------------------------------------------------------- /
  integer function julday(id,mm,iyyy)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           f. ardhuin              |
    !/                  |                        fortran 90 |
    !/                  | last update :         23-sep-2012 |
    !/                  +-----------------------------------+
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    integer(kind=4),    intent(in)  :: id,mm,iyyy
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer(kind=4), parameter :: igreg=15+31*(10+12*1582)
    integer(kind=4) ja,jm,jy
    jy=iyyy
    if (jy.eq.0) write(6,*) 'there is no zero year !!'
    if (jy.lt.0) jy=jy+1
    if (mm.gt.2) then
      jm=mm+1
    else
      jy=jy-1
      jm=mm+13
    endif
    julday=int(365.25*jy)+int(30.6001*jm)+id+1720995
    if (id+31*(mm+12*iyyy).ge.igreg) then
      ja=int(0.01*jy)
      julday=julday+2-ja+int(0.25*ja)
    end if
    return
    !/
    !/ end of julday ----------------------------------------------------- /
    !/
  end function julday
  !/ ------------------------------------------------------------------- /
  subroutine caldat(julian,id,mm,iyyy)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           f. ardhuin              |
    !/                  |                        fortran 90 |
    !/                  | last update :         23-sep-2012 |
    !/                  +-----------------------------------+
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    implicit none
    !/
    ! see numerical recipes 2nd ed. the order of month and day have been swapped!
    !
    !/
    integer(kind=4),    intent(in)  :: julian
    integer(kind=4),    intent(out) :: id,mm,iyyy
    integer(kind=4), parameter :: igreg=2299161
    integer(kind=4) ja,jalpha,jb,jc,jd,je
    if (julian.ge.igreg) then
      jalpha=int(((julian-1867216)-0.25)/36524.25)
      ja=julian+1+jalpha-int(0.25*jalpha)
    else
      ja=julian
    end if
    jb=ja+1524
    jc=int(6680.+((jb-2439870)-122.1)/365.25)
    jd=365*jc+int(0.25*jc)
    je=int((jb-jd)/30.6001)
    id=jb-jd-int(30.6001*je)
    mm=je-1
    if (mm.gt.12) mm=mm-12
    iyyy=jc-4715
    if (mm.gt.2) iyyy=iyyy-1
    if (iyyy.le.0) iyyy=iyyy-1
    return
    !/
    !/ end of caldat ----------------------------------------------------- /
    !/
  end subroutine caldat
  !/ ------------------------------------------------------------------- /
  real(kind=8) function  time2hours(time)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           f. ardhuin              |
    !/                  |                        fortran 90 |
    !/                  | last update :         26-sep-2012 |
    !/                  +-----------------------------------+
    !
    !  1. purpose :
    !
    !     gives date as real number
    !
    !  2. method :
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       time    i.a.  i/o  (1) current date in yyyymmdd format.
    !                          (2) current time in hhmmss format.
    !       dtime   real   i   time step in seconds.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !      iymd21    func. internal increment date in yyyymmdd format.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !     any other routine.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing using strace.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(inout)  :: time(2)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: iy,imo,id,ih,imi,is
    integer(kind=4)         :: jday
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! zero increment: get "legal" date
    !
    iy     = time(1) / 10000
    imo    = mod(time(1),10000) / 100
    id     = mod(time(1),100)
    ih     = time(2) / 10000
    imi    = mod(time(2),10000) / 100
    is     = mod(time(2),100)
    jday    = julday(id,imo,iy)
    time2hours = 24.d0*dfloat(jday)+dfloat(ih)+dfloat(is+imi*60)/3600.d0
    return
    !/
    !/ end of time2hours-------------------------------------------------- /
    !/
  end function time2hours
  !/ ------------------------------------------------------------------- /
  subroutine prinit
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         06-may-2005 !
    !/                  +-----------------------------------+
    !/
    !/    06-may-2005 : origination.                        ( version 3.07 )
    !/
    !  1. purpose :
    !
    !     initialize profiling routine prtime.
    !
    !  2. method :
    !
    !     fortran 90 system_clock intrinsic routine.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      system_clock
    !                sur.    n/a    get system time
    !     ----------------------------------------------------------------
    !
    !  5. called by :
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
    implicit none
    !/
    ! -------------------------------------------------------------------- /
    !
    call date_and_time ( values=prftb )
    !
    flprof = .true.
    !
    return
    !/
    !/ end of prinit ----------------------------------------------------- /
    !/
  end subroutine prinit
  !/ ------------------------------------------------------------------- /
  subroutine prtime ( ptime )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         06-may-2005 !
    !/                  +-----------------------------------+
    !/
    !/    06-may-2005 : origination.                        ( version 3.07 )
    !/
    !  1. purpose :
    !
    !     get wallclock time for profiling purposes.
    !
    !  2. method :
    !
    !     fortran 90 system_clock intrinsic routine.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       ptime   real   o   time retrieced from system.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      system_clock
    !                sur.    n/a    get system time
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !     any, after prinit has been called.
    !
    !  6. error messages :
    !
    !     - if no initialization, returned time equals -1.
    !     - if no system clock, returned time equals -1.
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
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(out)       :: ptime
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: prfta(8)
    !
    ! -------------------------------------------------------------------- /
    !
    ptime  = -1.
    !
    if ( .not. flprof ) return
    !
    call date_and_time ( values=prfta )
    ptime  = tdiff ( prftb,prfta )
    !
    return
    !/
    !/ end of prtime ----------------------------------------------------- /
    !/
  end subroutine prtime
  !/ ------------------------------------------------------------------- /
  subroutine t2d(time,dat,ierr)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                        fortran 90 |
    !/                  | last update :         04-jan-2018 |
    !/                  +-----------------------------------+
    !/
    !/    04-jan-2018 : origination                         ( version 6.04 )
    !/
    !  1. purpose :
    !
    !     converts time array from time(2) to dat(8)
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       time     i.a.     i   time array like 'yyyymmdd hhmmss'
    !       dat      i.a.     o   time array like returned by date_and_time(3f)
    !       ierr     integer  o   error code returned
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
    !       any subroutine/program.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer,intent(in)            :: time(2)  ! array like 'yyyymmdd hhmmss'
    integer,intent(out)           :: dat(8)   ! array like returned by date_and_time(3f)
    integer,intent(out)           :: ierr     ! error return, 0 for successful execution
    ! otherwise return 1
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    dat(1)=time(1)/10000
    dat(2)=(time(1)-dat(1)*10000)/100
    dat(3)=time(1)-dat(1)*10000-100*dat(2)
    dat(4)=0
    dat(5)=time(2)/10000
    dat(6)=(time(2)-dat(5)*10000)/100
    dat(7)=time(2)-dat(5)*10000-100*dat(6)
    dat(8)=0
    ierr=0
    !
    return
    !/
    !/ end of t2d ----------------------------------------------------- /
    !/
  end subroutine t2d
  !/ ------------------------------------------------------------------- /
  subroutine d2t(dat,time,ierr)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                        fortran 90 |
    !/                  | last update :         04-jan-2018 |
    !/                  +-----------------------------------+
    !/
    !/    04-jan-2018 : origination                         ( version 6.04 )
    !/
    !  1. purpose :
    !
    !     converts time array from dat(8) to time(2)
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       dat      i.a.     i   time array like returned by date_and_time(3f)
    !       time     i.a.     o   time array like 'yyyymmdd hhmmss'
    !       ierr     integer  o   error code returned
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
    !       any subroutine/program.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer,intent(in)            :: dat(8)   ! array like returned by date_and_time(3f)
    integer,intent(out)           :: time(2)  ! array like 'yyyymmdd hhmmss'
    integer,intent(out)           :: ierr     ! error return, 0 for successful execution
    ! otherwise return 1
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    time(1)=dat(1)*10000+dat(2)*100+dat(3)
    time(2)=dat(5)*10000+dat(6)*100+dat(7)
    ierr=0
    !
    return
    !/
    !/ end of d2t ----------------------------------------------------- /
    !/
  end subroutine d2t
  !/ ------------------------------------------------------------------- /
  subroutine d2j(dat,julian,ierr)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                        fortran 90 |
    !/                  | last update :         04-jan-2018 |
    !/                  +-----------------------------------+
    !/
    !/    04-jan-2018 : origination from m_time library     ( version 6.04 )
    !/    23-feb-2024 : updated to handle 360_day calendar  ( version 7.14 )
    !/
    !  1. purpose :
    !
    !     converts proleptic gregorian date array to julian day
    !
    !
    ! * udunits standard : mixed gregorian/julian  calendar  system.
    !                      dates  prior to 1582-10-15 are assumed to use
    !                      the julian calendar, which was introduced by julius caesar
    !                      in 46 bce and is based on a year that is exactly 365.25 days
    !                      long.  dates on and after 1582-10-15 are assumed to use  the
    !                      gregorian calendar, which was introduced on that date and is
    !                      based on a year that is exactly 365.2425 days long.  (a year
    !                      is  actually  approximately 365.242198781 days long.)
    !
    ! * there is no year zero
    ! * julian day must be non-negative
    ! * julian day starts at noon; while civil calendar date starts at midnight
    ! * if caltype is "360_day" a simpler calculation is used (30 days in every
    !   month) with a reference date of 1800-01-01.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       dat      i.a.     i   time array like returned by date_and_time(3f)
    !       julian   double   o   julian day
    !       ierr     integer  o   error code returned
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
    !       any subroutine/program.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer,intent(in)            :: dat(8)   ! array like returned by date_and_time(3f)
    double precision,intent(out)  :: julian   ! julian day (non-negative, but may be non-integer)
    integer,intent(out)           :: ierr     ! error return, 0 for successful execution
    ! -1=invalid year,-2=invalid month,-3=invalid day,
    ! -4=invalid date (29th feb, non leap-year)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: year, month, day, utc, hour, minute
    real                    :: second
    integer                 :: a, y, m, jdn
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    year   = dat(1)                        ! year
    month  = dat(2)                        ! month
    day    = dat(3)                        ! day
    utc    = dat(4)*60                     ! delta from utc, convert from minutes to seconds
    hour   = dat(5)                        ! hour
    minute = dat(6)                        ! minute
    second = dat(7)-utc+dat(8)/1000.d0     ! second   ! correction for time zone and milliseconds
    julian = -huge(99999)                  ! this is the date if an error occurs and ierr is < 0
    ! special case for 360 day climate calendar; return a pseudo-julian day
    ! assumes a reference date of 1800-01-01 00:00:00
    if( caltype .eq. "360_day" ) then
      julian = (year - 1800) * 360.0 +     &  ! years since 1800
            (month - 1) * 30.0  +          &  
            (day - 1) +                    & 
            hour / 24.0_8 +                &
            minute / 1440.0_8 +            &
            second / 86400.0_8
      ierr = 0
      return
    endif
    ! standard/gregorian calendar - return standard julian day calculation:
    if(year==0 .or. year .lt. -4713) then
      ierr=-1
      return
    end if
    !  you must compute first the number of years (y) and months (m) since march 1st -4800 (march 1, 4801 bc)
    a=(14-month)/12 ! a will be 1 for january or febuary, and 0 for other months, with integer truncation
    y=year+4800-a
    m=month+12*a-3  ! m will be 0 for march and 11 for febuary
    !  all years in the bc era must be converted to astronomical years, so that 1bc is year 0, 2 bc is year "-1", etc.
    !  convert to a negative number, then increment towards zero
    !  starting from a gregorian calendar date
    jdn=day + (153*m+2)/5 + 365*y + y/4 - y/100 + y/400 - 32045  !  with integer truncation
    !  finding the julian date given the jdn (julian day number) and time of day
    julian=dble(jdn) + dble(hour-12)/24.0d0 + dble(minute)/1440.0d0 + dble(second)/86400.0d0
    ! check if julian day is non-negative
    if(julian.lt.0.d0) then
      ierr=1
    else
      ierr=0
    end if
    !
    return
    !/
    !/ end of d2j ----------------------------------------------------- /
    !/
  end subroutine d2j
  !/ ------------------------------------------------------------------- /
  subroutine j2d(julian,dat,ierr)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                        fortran 90 |
    !/                  | last update :         04-jan-2018 |
    !/                  +-----------------------------------+
    !/
    !/    04-jan-2018 : origination from m_time library     ( version 6.04 )
    !/    23-feb-2024 : upated to handle 360_day calendar   ( version 7.14 )
    !/
    !  1. purpose :
    !
    !     converts julian day to date array
    !
    ! * there is no year zero
    ! * julian day must be non-negative
    ! * julian day starts at noon; while civil calendar date starts at midnight
    ! * if caltype is "360_day" a simpler calculation is used (30 days in every
    !   month) with a reference date of 1800-01-01.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       julian   double   i   julian day
    !       dat      i.a.     o   time array like returned by date_and_time(3f)
    !       ierr     integer  o   error code returned
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
    !       any subroutine/program.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    double precision,intent(in)   :: julian   ! julian day (non-negative, but may be non-integer)
    integer,intent(out)           :: dat(8)   ! array like returned by date_and_time(3f)
    integer,intent(out)           :: ierr     ! error return, 0 for successful execution
    !                                         ! otherwise return 1
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    real                      :: secday=86400.0d0
    integer                   :: timezone(8), tz
    real                      :: second
    integer                   :: year, month, day, hour, minute
    integer                   :: jalpha,ja,jb,jc,jd,je,ijul
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    if(caltype .eq. 'standard' .and. julian .lt. 0.d0) then 
      ! negative julian day not allowed
      ierr=1
      return
    end if
    !call date_and_time(values=timezone)         ! get the timezone
    !tz=timezone(4)
    tz=0                                         ! force to utc timezone
    ! calculation for time (hour,min,sec) same for julian
    ! and 360_day calendars:
    ijul=idint(julian)                           ! integral julian day
    second=sngl((julian-dble(ijul))*secday)      ! seconds from beginning of jul. day
    second=second+(tz*60)
    if(caltype .eq. "standard") then
      if(second.ge.(secday/2.0d0)) then          ! in next calendar day
        ijul=ijul+1
        second=second-(secday/2.0d0)             ! adjust from noon to midnight
      else                                       ! in same calendar day
        second=second+(secday/2.0d0)             ! adjust from noon to midnight
      end if
    end if
    if(second.ge.secday) then                    ! final check to prevent time 24:00:00
      ijul=ijul+1
      second=second-secday
    end if
    minute=int(second/60.0)                      ! integral minutes from beginning of day
    second=second-float(minute*60)               ! seconds from beginning of minute
    hour=minute/60                               ! integral hours from beginning of day
    minute=minute-hour*60                        ! integral minutes from beginning of hour
    if(caltype .eq. '360_day') then
      ! calculate date parts for 360 day climate calendar
      year = int(julian / 360) + 1800 ! (base year is 1800)
      month = mod(int(julian / 30), 12) + 1
      day = mod(int(julian), 30) + 1
    else ! stardard julian day calculation
      !---------------------------------------------
      jalpha=idint((dble(ijul-1867216)-0.25d0)/36524.25d0) ! correction for gregorian calendar
      ja=ijul+1+jalpha-idint(0.25d0*dble(jalpha))
      !---------------------------------------------
      jb=ja+1524
      jc=idint(6680.d0+(dble(jb-2439870)-122.1d0)/365.25d0)
      jd=365*jc+idint(0.25d0*dble(jc))
      je=idint(dble(jb-jd)/30.6001d0)
      day=jb-jd-idint(30.6001d0*dble(je))
      month=je-1
      if(month.gt.12) then
        month=month-12
      end if
      year=jc-4715
      if(month.gt.2) then
        year=year-1
      end if
  
      if(year.le.0) then
        year=year-1
      end if
    endif
  
    dat(1)=year
    dat(2)=month
    dat(3)=day
    dat(4)=tz
    dat(5)=hour
    dat(6)=minute
    dat(7)=int(second)
    dat(8)=int((second-int(second))*1000.0)
    ierr=0
    !
    return
    !/
    !/
  end subroutine j2d
  !/ ------------------------------------------------------------------- /
  double precision function tsub ( t1, t2 )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                        fortran 90 |
    !/                  | last update :         18-jun-2020 |
    !/                  +-----------------------------------+
    !/
    !/    15-may-2018 : origination                         ( version 6.05 )
    !/    18-jun-2020 : addition of 360-day calendar        ( version 7.08 )
    !/
    !  1. purpose :
    !
    !     substract two time arrays to get the time difference in days
    !     in a way to avoid decimal approximation error
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       t1       i.a.     i   time array
    !       t2       i.a.     i   time array
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
    !     any routine.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: t1(8), t2(8)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: a1, b1, c1, d1, a2, b2, c2, d2
    double precision        :: e1, e2
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! convert dates and times :
    !
    if (trim(caltype) .eq. '360_day' ) then
      a1 = (t2(1)-t1(1))*360 + (t2(2)-t1(2))*30 + (t2(3)-t1(3))
      e1 = 3600.0*t1(5) + 60.0*(t1(6)-t1(4)) + t1(7) + t1(8)/1000.0
      e2 = 3600.0*t2(5) + 60.0*(t2(6)-t2(4)) + t2(7) + t2(8)/1000.0
      !
      tsub = dble(a1) + (e2-e1)/86400.0d0
    else
      a1 = (14-t1(2))/12
      b1 = t1(1) + 4800 - a1
      c1 = t1(2) + 12*a1 - 3
      d1 = t1(3) + (153*c1 + 2)/5 + 365*b1
      if (trim(caltype) .eq. 'standard' ) then
        d1 = d1 + b1/4 -b1/100 + b1/400
      endif
      e1 = 3600.0*t1(5) + 60.0*(t1(6)-t1(4)) + t1(7) + t1(8)/1000.0
      !
      a2 = (14-t2(2))/12
      b2 = t2(1) + 4800 - a2
      c2 = t2(2) + 12*a2 - 3
      d2 = t2(3) + (153*c2 + 2)/5 + 365*b2
      if (trim(caltype) .eq. 'standard' ) then
        d2 = d2 + b2/4 -b2/100 + b2/400
      endif
      e2 = 3600.0*t2(5) + 60.0*(t2(6)-t2(4)) + t2(7) + t2(8)/1000.0
      !
      tsub = dble(d2-d1) + (e2-e1)/86400.0d0
    endif
    !
    return
    !/
    !/ end of tsub ------------------------------------------------------- /
    !/
  end function tsub
  !/ ------------------------------------------------------------------- /
  double precision function tsubsec ( t1, t2 )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |           c. bunney               |
    !/                  |                        fortran 90 |
    !/                  | last update :         18-jun-2020 |
    !/                  +-----------------------------------+
    !/
    !/    15-may-2018 : origination (adapted from tsub)     ( version 7.12 )
    !/
    !  1. purpose :
    !
    !     substract two time arrays to get the time difference in seconds.
    !     the milliseconds part of the array (index 8) is rounded to the
    !     nearest whole second.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       t1       i.a.     i   time array
    !       t2       i.a.     i   time array
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
    !     any routine.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: t1(8), t2(8)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer(kind=8)         :: a1, b1, c1, d1, a2, b2, c2, d2
    integer(kind=8)         :: e1, e2
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    if (trim(caltype) .eq. '360_day' ) then
      a1 = (t2(1)-t1(1))*360 + (t2(2)-t1(2))*30 + (t2(3)-t1(3))
      e1 = 3600.0*t1(5) + 60.0*(t1(6)-t1(4)) + t1(7) + nint(t1(8) / 1000.0)
      e2 = 3600.0*t2(5) + 60.0*(t2(6)-t2(4)) + t2(7) + nint(t2(8) / 1000.0)
      !
      tsubsec = a1 * 86400 + (e2-e1)
    else
      a1 = (14-t1(2))/12
      b1 = t1(1) + 4800 - a1
      c1 = t1(2) + 12*a1 - 3
      d1 = t1(3) + (153*c1 + 2)/5 + 365*b1
      if (trim(caltype) .eq. 'standard' ) then
        d1 = d1 + b1/4 -b1/100 + b1/400
      endif
      e1 = 3600.0*t1(5) + 60.0*(t1(6)-t1(4)) + t1(7) + nint(t1(8) / 1000.0)
      !
      a2 = (14-t2(2))/12
      b2 = t2(1) + 4800 - a2
      c2 = t2(2) + 12*a2 - 3
      d2 = t2(3) + (153*c2 + 2)/5 + 365*b2
      if (trim(caltype) .eq. 'standard' ) then
        d2 = d2 + b2/4 -b2/100 + b2/400
      endif
      e2 = 3600.0*t2(5) + 60.0*(t2(6)-t2(4)) + t2(7) + nint(t1(8) / 1000.0)
      !
      tsubsec = (d2-d1)*86400 + (e2-e1)
    endif
    !
    return
    !/
    !/ end of tsubsec ---------------------------------------------------- /
    !/
  end function tsubsec
  !/ ------------------------------------------------------------------- /
  subroutine u2d(units,dat,ierr)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                        fortran 90 |
    !/                  | last update :         15-may-2018 |
    !/                  +-----------------------------------+
    !/
    !/    15-may-2018 : origination                         ( version 6.05 )
    !/
    !  1. purpose :
    !
    !     convert time units attribute to date array
    !
    ! * units attribute must respect convention iso8601
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       units    char     i   units attribute
    !       dat      i.a.     o   time array like returned by date_and_time(3f)
    !       ierr     integer  o   error code returned
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
    !       any subroutine/program.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use w3servmd, only: extcde
    use w3odatmd, only: ndse
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    character(*),intent(in)       :: units    ! units attribute
    integer,intent(out)           :: dat(8)   ! array like returned by date_and_time(3f)
    integer,intent(out)           :: ierr     ! error return, 0 for successful execution
    ! otherwise returnb 1
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    dat(4) = 0  ! force to utc timezone
    dat(8) = 0  ! force milliseconds to 0
    ! seconds
    if (index(units, "seconds").ne.0) then
      ! seconds since yyyy-mm-dd hh:mm:ss
      if (index(units, "-", .true.).eq.22) then
        read(units(15:18),'(i4.4)',end=804,err=805,iostat=ierr) dat(1)
        read(units(20:21),'(i2.2)',end=804,err=805,iostat=ierr) dat(2)
        read(units(23:24),'(i2.2)',end=804,err=805,iostat=ierr) dat(3)
        read(units(26:27),'(i2.2)',end=804,err=805,iostat=ierr) dat(5)
        read(units(29:30),'(i2.2)',end=804,err=805,iostat=ierr) dat(6)
        read(units(32:33),'(i2.2)',end=804,err=805,iostat=ierr) dat(7)
        ! seconds since yyyy-m-d ...
      else if (index(units, "-", .true.).eq.21) then
        read(units(15:18),'(i4.4)',end=804,err=805,iostat=ierr) dat(1)
        read(units(20:20),'(i1.1)',end=804,err=805,iostat=ierr) dat(2)
        read(units(22:22),'(i1.1)',end=804,err=805,iostat=ierr) dat(3)
        ! seconds since yyyy-m-d h:m:s
        if (index(units, ":", .false.).eq.25) then
          read(units(24:24),'(i1.1)',end=804,err=805,iostat=ierr) dat(5)
          read(units(26:26),'(i1.1)',end=804,err=805,iostat=ierr) dat(6)
          read(units(28:28),'(i1.1)',end=804,err=805,iostat=ierr) dat(7)
          ! seconds since yyyy-m-d hh:mm:ss
        else if (index(units, ":", .false.).eq.26) then
          read(units(24:25),'(i2.2)',end=804,err=805,iostat=ierr) dat(5)
          read(units(27:28),'(i2.2)',end=804,err=805,iostat=ierr) dat(6)
          read(units(30:31),'(i2.2)',end=804,err=805,iostat=ierr) dat(7)
        else
          goto 804
        end if
      else
        goto 804
      end if
      ! days
    else if (index(units, "days").ne.0) then
      ! days since yyyy-mm-dd hh:mm:ss
      if (index(units, "-", .true.).eq.19) then
        read(units(12:15),'(i4.4)',end=804,err=805,iostat=ierr) dat(1)
        read(units(17:18),'(i2.2)',end=804,err=805,iostat=ierr) dat(2)
        read(units(20:21),'(i2.2)',end=804,err=805,iostat=ierr) dat(3)
        read(units(23:24),'(i2.2)',end=804,err=805,iostat=ierr) dat(5)
        read(units(26:27),'(i2.2)',end=804,err=805,iostat=ierr) dat(6)
        read(units(29:30),'(i2.2)',end=804,err=805,iostat=ierr) dat(7)
        ! days since yyyy-m-d ...
      else if (index(units, "-", .true.).eq.18) then
        read(units(12:15),'(i4.4)',end=804,err=805,iostat=ierr) dat(1)
        read(units(17:17),'(i1.1)',end=804,err=805,iostat=ierr) dat(2)
        read(units(19:19),'(i1.1)',end=804,err=805,iostat=ierr) dat(3)
        ! days since yyyy-m-d h:m:s
        if (index(units, ":", .false.).eq.22) then
          read(units(21:21),'(i1.1)',end=804,err=805,iostat=ierr) dat(5)
          read(units(23:23),'(i1.1)',end=804,err=805,iostat=ierr) dat(6)
          read(units(25:25),'(i1.1)',end=804,err=805,iostat=ierr) dat(7)
          ! days since yyyy-m-d hh:mm:ss
        else if (index(units, ":", .false.).eq.23) then
          read(units(21:22),'(i2.2)',end=804,err=805,iostat=ierr) dat(5)
          read(units(24:25),'(i2.2)',end=804,err=805,iostat=ierr) dat(6)
          read(units(27:28),'(i2.2)',end=804,err=805,iostat=ierr) dat(7)
        else
          goto 804
        end if
      else
        goto 804
      end if
      ! hours
    else if (index(units, "hours").ne.0) then
      ! hours since yyyy-mm-dd hh:mm:ss
      if (index(units, "-", .true.).eq.20) then
        read(units(13:16),'(i4.4)',end=804,err=805,iostat=ierr) dat(1)
        read(units(18:19),'(i2.2)',end=804,err=805,iostat=ierr) dat(2)
        read(units(21:22),'(i2.2)',end=804,err=805,iostat=ierr) dat(3)
        read(units(24:25),'(i2.2)',end=804,err=805,iostat=ierr) dat(5)
        read(units(27:28),'(i2.2)',end=804,err=805,iostat=ierr) dat(6)
        read(units(30:31),'(i2.2)',end=804,err=805,iostat=ierr) dat(7)
        ! hours since yyyy-m-d ...
      else if (index(units, "-", .true.).eq.19) then
        read(units(13:16),'(i4.4)',end=804,err=805,iostat=ierr) dat(1)
        read(units(18:18),'(i1.1)',end=804,err=805,iostat=ierr) dat(2)
        read(units(20:20),'(i1.1)',end=804,err=805,iostat=ierr) dat(3)
        ! hours since yyyy-m-d h:m:s
        if (index(units, ":", .false.).eq.23) then
          read(units(22:22),'(i1.1)',end=804,err=805,iostat=ierr) dat(5)
          read(units(24:24),'(i1.1)',end=804,err=805,iostat=ierr) dat(6)
          read(units(26:26),'(i1.1)',end=804,err=805,iostat=ierr) dat(7)
          ! hours since yyyy-m-d hh:mm:ss
        else if (index(units, ":", .false.).eq.24) then
          read(units(22:23),'(i2.2)',end=804,err=805,iostat=ierr) dat(5)
          read(units(25:26),'(i2.2)',end=804,err=805,iostat=ierr) dat(6)
          read(units(28:29),'(i2.2)',end=804,err=805,iostat=ierr) dat(7)
        else
          goto 804
        end if
      else
        goto 804
      end if
      ! minutes
    else if (index(units, "minutes").ne.0) then
      ! minutes since yyyy-mm-dd hh:mm:ss
      if (index(units, "-", .true.).eq.22) then
        read(units(15:18),'(i4.4)',end=804,err=805,iostat=ierr) dat(1)
        read(units(20:21),'(i2.2)',end=804,err=805,iostat=ierr) dat(2)
        read(units(23:24),'(i2.2)',end=804,err=805,iostat=ierr) dat(3)
        read(units(26:27),'(i2.2)',end=804,err=805,iostat=ierr) dat(5)
        read(units(29:30),'(i2.2)',end=804,err=805,iostat=ierr) dat(6)
        read(units(32:33),'(i2.2)',end=804,err=805,iostat=ierr) dat(7)
        ! minutes since yyyy-m-d ...
      else if (index(units, "-", .true.).eq.21) then
        read(units(15:18),'(i4.4)',end=804,err=805,iostat=ierr) dat(1)
        read(units(20:20),'(i1.1)',end=804,err=805,iostat=ierr) dat(2)
        read(units(22:22),'(i1.1)',end=804,err=805,iostat=ierr) dat(3)
        ! minutes since yyyy-m-d h:m:s
        if (index(units, ":", .false.).eq.25) then
          read(units(24:24),'(i1.1)',end=804,err=805,iostat=ierr) dat(5)
          read(units(26:26),'(i1.1)',end=804,err=805,iostat=ierr) dat(6)
          read(units(28:28),'(i1.1)',end=804,err=805,iostat=ierr) dat(7)
          ! minutes since yyyy-m-d hh:mm:ss
        else if (index(units, ":", .false.).eq.26) then
          read(units(24:25),'(i2.2)',end=804,err=805,iostat=ierr) dat(5)
          read(units(27:28),'(i2.2)',end=804,err=805,iostat=ierr) dat(6)
          read(units(30:31),'(i2.2)',end=804,err=805,iostat=ierr) dat(7)
        else
          goto 804
        end if
      else
        goto 804
      end if
      ! nothing
    else
      goto 804
    end if
    !
    goto 888
    !
    ! error escape locations
    !
804 continue
    write (ndse,1004) trim(units)
    call extcde ( 44 )
    !
805 continue
    write (ndse,1005) ierr
    call extcde ( 45 )
    !
888 continue
    !
    ! formats
    !
1004 format (/' *** wavewatch iii error in w3timemd : '/               &
         '     premature end of time attribute '/                 &
         '     ',a/                                               &
         '     differs from conventions iso8601 '/                &
         '     xxx since yyyy-mm-dd hh:mm:ss'/                    &
         '     xxx since yyyy-m-d h:m:s'/                         &
         '     xxx since yyyy-m-d hh:mm:ss'/)
    !
1005 format (/' *** wavewatch iii error in w3timemd : '/               &
         '     error in reading of time attribute '/              &
         '     ',a/                                               &
         '     differs from conventions iso8601 '/                &
         '     xxx since yyyy-mm-dd hh:mm:ss'/                    &
         '     xxx since yyyy-m-d h:m:s'/                         &
         '     xxx since yyyy-m-d hh:mm:ss'/                      &
         '     iostat =',i5/)
    !
    return
    !/
    !/ end of u2d ----------------------------------------------------- /
    !/
  end subroutine u2d
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine t2iso(time,isodt)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           c. bunney               |
    !/                  |                        fortran 90 |
    !/                  | last update :         19-jan-2020 |
    !/                  +-----------------------------------+
    !/
    !/    19-jan-2020 : origination                         ( version 7.12 )
    !/
    !  1. purpose :
    !
    !     convert time array to iso8601 format string
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       time     i.a.     i   time array like 'yyyymmdd hhmmss'
    !       isodt    char.    o   iso8601 datetime string
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
    !       any subroutine/program.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use w3servmd, only: extcde
    use w3odatmd, only: ndse
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer,intent(in)            :: time(2)  ! array like 'yyyymmdd hhmmss'
    character(len=32),intent(out) :: isodt    ! iso date time
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !/
    write(isodt,'(i4,"-",i2.2,"-",i2.2,"t",i2.2,":",i2.2,":",i2.2)') &
         time(1) / 10000,                                              &
         mod(time(1) / 100, 100),                                      &
         mod(time(1), 100),                                            &
         time(2) / 10000,                                              &
         mod(time(2) / 100, 100),                                      &
         mod(time(2), 100)
    !/
    !/ end of t2iso ------------------------------------------------------ /
    !/
  end subroutine t2iso
  !/ end of module w3timemd -------------------------------------------- /
  !/
end module w3timemd
