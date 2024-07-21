!> @file
!> @brief contains module w3bullmd.
!>
!> @author j. h. alves
!> @author h. l. tolman
!> @date   26-dec-2012
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
!> @brief module w3bullmd.
!>
!> @author j. h. alves
!> @author h. l. tolman
!> @date   26-dec-2012
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3bullmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch-iii           noaa/ncep |
  !/                  |           j. h. alves             |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         26-dec-2012 |
  !/                  +-----------------------------------+
  !/
  !/    01-apr-2010 : origination.                        ( version 3.14 )
  !/    25-jun-2011 : temporary change of hsmin           ( version 4.05 )
  !/    15-aug-2011 : changing hsmin to bhsmin bugfix     ( version 4.05 )
  !/    26-dec-2012 : modified obsolete declarations.     ( version 4.11 )
  !/
  !/ ------------------------------------------------------------------- /
  use w3gdatmd, only: gname, nk, nth, nspec, flagll
  use w3odatmd, only: nopts, ptloc, ptnme, dimp
  use constants, only: pi, tpi
  use w3wdatmd, only: time
  use w3timemd, only: dsec21
  public
  integer, parameter   :: nptab = 6, nfld = 50, npmax = 80
  !
  real, parameter      :: bhsmin = 0.15, bhsdrop = 0.05
  real                 :: hst(nptab,2), tpt(nptab,2),     &
       dmt(nptab,2)
  character(len=129)   :: ascbline
  character(len=664)   :: csvbline
  logical              :: iyy(npmax)
  !/
  !/ conventional declarations
  !/
  !/
  !/ private parameter statements (id strings)
  !/
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief read a wavewatch-iii version 1.17 point output data file and
  !>     produces a table of mean parameters for all individual wave
  !>     systems.
  !>
  !> @details partitioning is made using the built-in module w3partmd.
  !>     partitions are ranked and organized into coherent sequences that
  !>     are then written as tables to output files. input options for generating
  !>     tables are defined in ww3_outp.inp. this module sorts the table
  !>     data, output to file is controlled by ww3_outp.
  !>
  !> @param[in]    npart
  !> @param[in]    xpart
  !> @param[in]    dimxp
  !> @param[in]    uabs
  !> @param[in]    ud
  !> @param        ipnt
  !> @param[in]    iout
  !> @param[inout] timev
  !>
  !> @author j. h. alves
  !> @author h. l. tolman
  !> @date   11-mar-2013
  !>
  subroutine w3bull                                                &
       ( npart, xpart, dimxp, uabs, ud, ipnt, iout, timev )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch-iii           noaa/ncep |
    !/                  |           j. h. alves             |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         11-mar-2013 !
    !/                  +-----------------------------------+
    !/
    !/    01-apr-2010 : origination.                        ( version 3.14 )
    !/    26-dec-2012 : modified obsolete declarations.     ( version 4.11 )
    !/    15-aug-2011 : adjustments to version 4.05         ( version 4.05 )
    !/    11-mar-2013 : minor cleanup                       ( version 4.09 )
    !/
    !  1. purpose :
    !
    !     read a wavewatch-iii version 1.17 point output data file and
    !     produces a table of mean parameters for all individual wave
    !     systems.
    !
    !  2. method :
    !
    !     partitioning is made using the built-in module w3partmd. partitions
    !     are ranked and organized into coherent sequences that are then
    !     written as tables to output files. input options for generating
    !     tables are defined in ww3_outp.inp. this module sorts the table
    !     data, output to file is controlled by ww3_outp.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       dhsmax  real   max. change in hs for system to be considered
    !                      related to previous time.
    !       dtpmax  real   id. tp.
    !       ddmmax  real   id. dm.
    !       ddwmax  real   maximum differences in wind and wave direction
    !                      for marking of system as under the influence
    !                      of the local wind,
    !       agemin  real   id. wave age.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    sur.  w3servmd subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !     ww3_outp
    !
    !  6. error messages :
    !
    !     error control made in ww3_outp.
    !
    !  7. remarks :
    !
    !     current version does not allow generating tables for multiple
    !     points.
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
    !     use constants
    !
    implicit none
    !
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !/
    !
    ! -------------------------------------------------------------------- /
    ! 1.  initializations
    !
    real                    :: dhsmax, dtpmax,        &
         ddmmax, ddwmax, agemin
    parameter     ( dhsmax =   1.50 )
    parameter     ( dtpmax =   1.50 )
    parameter     ( ddmmax =  15.   )
    parameter     ( ddwmax =  30.   )
    parameter     ( agemin =   0.8  )
    integer, intent(in)     :: npart, dimxp, iout
    integer, intent(inout)  :: timev(2)
    real, intent(in)        :: uabs,    &
         ud, xpart(dimp,0:dimxp)
    integer                 :: ipg1,ipi(npmax), ilen(npmax), ip,     &
         ipnow, ifld, inotab, ipnt, itab,      &
         doutp, fcsti, nzero
    real                    :: afr, age, ddmmaxr, deldm, deldmr,     &
         deldw, delhs, deltp, dhsmaxr,  &
         dtpmaxr, hmax, hstot, tp, udir, fact
    real                    :: hsp(npmax), tpp(npmax), &
         dmp(npmax), wnp(npmax), hsd(npmax),   &
         tpd(npmax), wdd(npmax)
    logical                 :: flag(npmax)
    character(len=129)      :: blank, tail !, ascbline
    character(len=15)       :: part
    character(len=664)      :: blank2 !,csvbline
    character               :: stime*8,form*20,form1*2
    character(len=16)       :: part2
    !/
    !/ ------------------------------------------------------------------- /
    !
    !
    ! 1.a constants etc.
    !
    ! set fact to proper scaling according to spherical or cartesian
    if ( flagll ) then
      fact = 1.
    else
      fact = 1.e-3
    endif
    !
    ! convert wind direction to azimuthal reference
    udir   = mod( ud+180., 360. )
    !
    tail (  1: 40) = '+-------+-----------+-----------------+-'
    tail ( 41: 80) = '----------------+-----------------+-----'
    tail ( 81:120) = '------------+-----------------+---------'
    tail (120:129) = '---------+'
    blank(  1: 40) = '| nn nn |      nn   |                 | '
    blank( 41: 80) = '                |                 |     '
    blank( 81:120) = '            |                 |         '
    blank(120:129) = '         |'
    ascbline       = blank
    !
    blank2(  1: 40)='    ,    ,  ,  ,  ,     ,   ,     ,     '
    blank2( 41: 88)=',     ,     ,   ,     ,     ,   ,     ,     ,   '
    blank2( 89:136)=',     ,     ,   ,     ,     ,   ,     ,     ,   '
    blank2(137:184)=',     ,     ,   ,     ,     ,   ,     ,     ,   '
    blank2(185:232)=',     ,     ,   ,     ,     ,   ,     ,     ,   '
    blank2(233:280)=',     ,     ,   ,     ,     ,   ,     ,     ,   '
    blank2(281:328)=',     ,     ,   ,     ,     ,   ,     ,     ,   '
    blank2(329:376)=',     ,     ,   ,     ,     ,   ,     ,     ,   '
    blank2(377:424)=',     ,     ,   ,     ,     ,   ,     ,     ,   '
    blank2(425:472)=',     ,     ,   ,     ,     ,   ,     ,     ,   '
    blank2(473:520)=',     ,     ,   ,     ,     ,   ,     ,     ,   '
    blank2(521:568)=',     ,     ,   ,     ,     ,   ,     ,     ,   '
    blank2(569:616)=',     ,     ,   ,     ,     ,   ,     ,     ,   '
    blank2(617:664)=',     ,     ,   ,     ,     ,   ,     ,     ,   '
    !
    csvbline      = blank2
    !
    if (iout .eq. 1) then
      ipg1 = 0
      do ip=1, nptab
        hst(ip,1) = -99.9
        tpt(ip,1) = -99.9
        dmt(ip,1) = -99.9
      enddo
      do ip=1, npmax
        iyy(ip) = .false.
        ipi(ip)=1
        ilen(ip)=0
      enddo
    endif
    !
    ! 3.  get overall wave height ---------------------------------------- *
    !
    hstot  = xpart(1,0)
    tp     = xpart(2,0)
    hsp = xpart(1,1:npart)
    tpp = xpart(2,1:npart)
    wnp = tpi / xpart(3,1:npart)
    dmp = mod( xpart(4,1:npart) + 180., 360.)
    nzero = 0
    nzero = count( hsp <= bhsmin .and. hsp /= 0.  )
    !
    ! 4.  process all partial fields ------------------------------------- *
    !
    do ip=npart+1, npmax
      hsp(ip) =    0.00
      tpp(ip) = -999.99
      dmp(ip) = -999.99
    enddo
    do ip=1, nptab
      hst(ip,2) = hst(ip,1)
      tpt(ip,2) = tpt(ip,1)
      dmt(ip,2) = dmt(ip,1)
      hst(ip,1) = -1.
      tpt(ip,1) = -1.
      dmt(ip,1) = -1.
    enddo
    !
    ! 5.  generate output table ------------------------------------------ *
    ! 5.a time and overall wave height to string
    !
    ascbline = blank
    csvbline = blank2
    !
    ! fill the variable forecast time with hrs relative to reference time
    if ( timev(1) .le. 0 ) timev = time
    fcsti = dsec21 (timev, time) / 3600
    write(csvbline(1:4),'(i4)')fcsti
    !
    do ifld=1,nptab
      iyy(ifld)=.false.
    enddo
    !
    ! ... write the time labels for current table line
    write (csvbline(6:9),'(i4)') int(time(1)/10000)
    write (csvbline(11:12),'(i2)')                                  &
         int(time(1)/100)-100*int(time(1)/10000)
    write (csvbline(14:15),'(i2)') mod(time(1),100)
    write (csvbline(17:18),'(i2)') time(2)/10000
    write (csvbline(20:24),'(f5.2)') uabs
    write (csvbline(26:28),'(i3)') int(udir)
    if ( hstot .gt. 0. ) write (csvbline(30:34),'(f5.2)') hstot
    if ( hstot .gt. 0. ) write (csvbline(36:40),'(f5.2)') tp
    !
    write (ascbline(3:4),'(i2)') mod(time(1),100)
    write (ascbline(6:7),'(i2)') time(2)/10000
    !
    if ( hstot .gt. 0. ) write (ascbline(10:14),'(f5.2)') hstot
    write (ascbline(16:17),'(i2)') npart - nzero
    !
    !
    if ( npart.eq.0 .or. hstot.lt.0.1 ) goto 699
    !
    ! 5.b switch off peak with too low wave height
    !
    do ip=1, npart
      flag(ip) = hsp(ip) .gt. bhsmin
    enddo
    !
    ! 5.c find next highest wave height
    !
    inotab   = 0
    !
601 continue
    !
    hmax   = 0.
    ipnow  = 0
    do ip=1, npart
      if ( hsp(ip).gt.hmax .and. flag(ip) ) then
        ipnow  = ip
        hmax   = hsp(ip)
      endif
    enddo
    !
    ! 5.d no more peaks, skip to output
    !
    if ( ipnow .eq. 0 ) goto 699
    !
    ! 5.e find matching field
    !
    itab   = 0
    !
    do ip=1, nptab
      if ( tpt(ip,2) .gt. 0. ) then
        !
        delhs  = abs ( hst(ip,2) - hsp(ipnow) )
        deltp  = abs ( tpt(ip,2) - tpp(ipnow) )
        deldm  = abs ( dmt(ip,2) - dmp(ipnow) )
        if ( deldm .gt. 180. ) deldm = 360. - deldm
        if ( delhs.lt.dhsmax .and. &
             deltp.lt.dtpmax .and. &
             deldm.lt.ddmmax ) itab = ip
        !
      endif
    enddo
    !
    ! 5.f no matching field, find empty fields
    !
    if ( itab .eq. 0 ) then
      do ip=nptab, 1, -1
        if ( tpt(ip,1).lt.0. .and. tpt(ip,2).lt.0. )    &
             itab = ip
      enddo
    endif
    !
    ! 5.g slot in table found, write
    !
    ! remove clear windseas
    !
    if ( itab .ne. 0 ) then
      !
      write (part,'(1x,f5.2,f5.1,i4)')                             &
           hsp(ipnow), tpp(ipnow), nint(dmp(ipnow))
      deldw  = mod ( abs ( udir - dmp(ipnow) ) , 360. )
      if ( deldw .gt. 180. ) deldw = 360. - deldw
      afr    = 2.*pi/tpp(ipnow)
      age    = uabs * wnp(ipnow) / afr
      if ( deldw.lt.ddwmax .and. age.gt.agemin ) part(1:1) = '*'
      !
      ascbline(5+itab*18:19+itab*18) = part
      !
      do ifld=1,nptab
        if(itab.eq.ifld)then
          iyy(ifld)=.true.
          hsd(ifld)=hsp(ipnow)
          tpd(ifld)=tpp(ipnow)
          wdd(ifld)=nint(dmp(ipnow))
        endif
      enddo
      !
      hst(itab,1) = hsp(ipnow)
      tpt(itab,1) = tpp(ipnow)
      dmt(itab,1) = dmp(ipnow)
      !
      ! 5.h no slot in table found, write
      !
    else
      !
      inotab   = inotab + 1
      write (ascbline(19:19),'(i1)') inotab
      !
    endif
    !
    flag(ipnow) = .false.
    goto 601
    !
    ! 5.i end of processing, write line in table
    !
699 continue
    !
    do ifld=1,nptab
      if(iyy(ifld))then
        ilen(ifld)=ilen(ifld)+1
        if (ilen(ifld).eq.1)then
          ipi(ifld)=ipg1+1
          ipg1=ipg1+1
        endif
        write (part2,'(",",f5.2,",",f5.2,",",i3)')                   &
             hsd(ifld), tpd(ifld), nint(wdd(ifld))
        csvbline(25+ipi(ifld)*16:40+ipi(ifld)*16) = part2
      else
        ilen(ifld)=0
      endif
    enddo
    !
    return
    !/
    !/ end of w3bull ----------------------------------------------------- /
    !/
  end subroutine w3bull
  !/
  !/ end of module w3bullmd -------------------------------------------- /
  !/
end module w3bullmd
