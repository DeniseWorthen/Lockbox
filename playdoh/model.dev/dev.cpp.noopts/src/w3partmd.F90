!> @file w3partmd.f90
!> @brief spectral partitioning module
!>
!> @authors barbara tracy, h. l. tolman, m. szyszka, chris bunney
!> @date 23 jul 2018
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
!> @brief spectral partitioning according to the watershed method
!>
!> @details multiple partitioning methods are provided in this
!>  module that can be selected via the \c ptmeth namelist variable.
!>  please see the w3part() subroutine for details
!>
!> @author barbara tracey, h. l. tolman, m. szyszka, chris bunney
!> @date 23 jul 2018
!>
module w3partmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii          usace/noaa |
  !/                  |          barbara  tracy           |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         23-jul-2018 |
  !/                  +-----------------------------------+
  !/
  !/    01-nov-2006 : origination.                        ( version 3.10 )
  !/    02-nov-2006 : adding tail to integration.         ( version 3.10 )
  !/    24-mar-2007 : bug fix imi, adding overall field   ( version 3.11 )
  !/                  and sorting.
  !/    15-apr-2008 : clean up for distribution.          ( version 3.14 )
  !/    02-dec-2010 : adding a mapping pmap between       ( version 3.14 )
  !/                  original and combined partitions
  !/                  ( m. szyszka )
  !/    23-jul-2018 : added alternative partitioning      ( version 6.05 )
  !/                  methods (c. bunney, ukmo)
  !  1. purpose :
  !
  !     spectral partitioning according to the watershed method.
  !
  !  2. variables and types :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      mk, mth   int.  private  dimensions of stored neighour array.
  !      neigh     i.a.  private  nearest neighbor array.
  !     ----------------------------------------------------------------
  !      note: ihmax, hspmin, wsmult, wscut and flcomb used from w3odatmd.
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3part    subr. public   interface to watershed routines.
  !      ptsort    subr. public   sort discretized image.
  !      ptnghb    subr. public   defeine nearest neighbours.
  !      pt_fld    subr. public   incremental flooding algorithm.
  !      fifo_add, fifo_empty, fifo_first
  !                subr. pt_fld   queue management.
  !      ptmean    subr. public   compute mean parameters.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      strace    subr. w3servmd subroutine traceing.
  !      wavnu1    subr. w3dispmd wavenumber computation.
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !
  !  6. switches :
  !
  !     !/s    enable subroutine tracing.
  !     !/t    enable test output
  !
  !  7. source code :
  !
  !/ ------------------------------------------------------------------- /
  !
  use w3odatmd, only: ihmax, hspmin, wsmult, dimp, ptmeth, ptfcut
  !
  public
  !
  !> nearest neighbour array frequency dimension size
  integer, private              :: mk = -1
  !> nearest neighbour array direction dimension size
  integer, private              :: mth = -1
  !> nearest neighbour array
  integer, allocatable, private :: neigh(:,:)
  !/
contains
  !/ ------------------------------------------------------------------- /
  !> @brief interface to watershed partitioning routines.
  !>
  !> @details watershed algorithm of vincent and soille, 1991,
  !>  implemented by barbara tracy (usace/erdc) for noaa/ncep.
  !>
  !>  this version of w3part contains alternate met office partitioning
  !>  methods, selected at runtime using the \c ptmeth namlist variable:
  !>    -# standard ww3 partitioning, as per original method described
  !>       by barbary tracy.
  !>    -# met office extended partitioning using split-partitions
  !>       (removes the wind sea part of any swell partiton and combines
  !>       with total wind sea partition).
  !>    -# met office "wave systems" - no classification or combining of
  !>       wind sea partitions. all partitions output and ordered simply
  !>       by wave height.
  !>    -# classic, simple wave age based partitioning generating
  !>       a single wind sea and swell partition.
  !>    -# 2-band partitioning; produces hi and low freqency band partitions
  !>       using a user-defined cutoff frequency (\c ptfcut).
  !>
  !> @remarks
  !>    - \c dimxp will always be of size 2 when using \c ptmeth 4 or 5.
  !>
  !>    - to achieve minimum storage but guaranteed storage of all
  !>      partitions <tt>dimxp = ((nk+1)/2) * ((nth-1)/2)</tt>
  !>      unless specified otherwise below.
  !>
  !> @param[in]    spec    2-d spectrum e(f,theta)
  !> @param[in]    uabs    wind speed
  !> @param[in]    udir    wind direction
  !> @param[in]    depth   water depth
  !> @param[in]    wn      wavenumebers for each frequency
  !> @param[out]   np      number of partitions found
  !>                       (-1=spectrum without minumum energy;
  !>                       0=spectrum with minumum energy but no partitions)
  !> @param[out]   xp      parameters describing partitions.
  !>                       entry '0' contains entire spectrum
  !> @param[in]    dimxp   second dimension of xp
  !>
  !> @author barbara tracey, h. l. tolman, m. szyszka, chris bunney
  !> @date 23 jul 2018
  !>
  subroutine w3part ( spec, uabs, udir, depth, wn, np, xp, dimxp )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii          usace/noaa |
    !/                  |          barbara  tracy           |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         02-dec-2010 !
    !/                  +-----------------------------------+
    !/
    !/    28-oct-2006 : origination.                       ( version 3.10 )
    !/    02-dec-2010 : adding a mapping pmap between      ( version 3.14 )
    !/                  original and combined partitions
    !/                  ( m. szyszka )
    !/
    !  1. purpose :
    !
    !     interface to watershed partitioning routines.
    !
    !  2. method :
    !
    !     watershed algorithm of vincent and soille, 1991, implemented by
    !     barbara tracy (usace/erdc) for noaa/ncep.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       spec    r.a.   i   2-d spectrum e(f,theta).
    !       uabs    real   i   wind speed.
    !       udir    real   i   wind direction.
    !       depth   real   i   water depth.
    !       wn      r.a.   i   wavenumebers for each frequency.
    !       np      int.   o   number of partitions.
    !                           -1 : spectrum without minumum energy.
    !                            0 : spectrum with minumum energy.
    !                                but no partitions.
    !       xp      r.a.   o   parameters describing partitions.
    !                          entry '0' contains entire spectrum.
    !       dimxp   int.   i   second dimension of xp.
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
    !  6. error messages :
    !
    !  7. remarks :
    !
    !     - to achieve minimum storage but guaranteed storage of all
    !       partitions dimxp = ((nk+1)/2) * ((nth-1)/2) unless specified
    !       otherwise below.
    !
    !     this version of w3part contains alternate met office partitioning
    !     methods, selected at runtime using the ptmeth namlist variable:
    !         1) standard ww3 partitioning
    !         2) met office extended partitioning using split-partitions
    !            (removes the wind sea part of any swell partiton and combines
    !            with total wind sea partition).
    !         3) met office "wave systems" - no classification or combining of
    !            wind sea partitions. all partitions output and ordered simply
    !            by wave height.
    !         4) classic, simple wave age based partitioning generating
    !            a single wind sea and swell partition. [dimxp = 2]
    !         5) 2-band partitioning; produces hi and low freqency band partitions
    !            using a user-defined cutoff frequency (ptfcut). [dimxp = 2]
    !
    !     (chris bunney, uk met office, jul 2018)
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
    !/
    use constants
    !
    use w3gdatmd, only: nk, nth, nspec, sig, th
    use w3odatmd, only: wscut, flcomb
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(out)          :: np
    integer, intent(in)           :: dimxp
    real, intent(in)              :: spec(nk,nth), wn(nk), uabs,    &
         udir, depth
    real, intent(out)             :: xp(dimp,0:dimxp)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ith, imi(nspec), imd(nspec),         &
         imo(nspec), ind(nspec), np_max,      &
         ip, it(1), index(dimxp), nws,        &
         ipw, ipt, isp
    integer                 :: pmap(dimxp)
    real                    :: zp(nspec), zmin, zmax, z(nspec),     &
         fact, wsmax, hsmax
    real                    :: tp(dimp,dimxp)
    integer                 :: ik, wind_part    ! chrisb; added for new
    real                    :: c, upar, sigcut  ! ukmo partioning methods
    !/
    !/ ------------------------------------------------------------------- /
    ! 0.  initializations
    !
    !
    np     = 0
    xp     = 0.
    !
    ! -------------------------------------------------------------------- /
    ! 1.  process input spectrum
    ! 1.a 2-d to 1-d spectrum
    !
    do ith=1, nth
      zp(1+(ith-1)*nk:ith*nk) = spec(:,ith)
    end do
    !
    ! ptmeth == 4 : do simple partitioning based solely on the
    ! wave age criterion (produces one swell and one wind sea only):
    !
    if( ptmeth .eq. 4 ) then
      do ik=1, nk
        do ith=1, nth
          isp = ik + (ith-1) * nk ! index into partition array imo
          upar = wsmult * uabs * max(0.0, cos(th(ith)-dera*udir))
          c = sig(ik) / wn(ik)
          if( upar .le. c ) then
            ! is swell:
            imo(isp) = 2
          else
            ! is wind sea:
            imo(isp) = 1
          endif
        enddo
      enddo
      ! we have a max of up to two partitions:
      np_max=2
      ! calculate mean parameters:
      call ptmean ( np_max, imo, zp, depth, uabs, udir, wn,           &
           np, xp, dimxp, pmap )
      ! no more processing required, return:
      return
    endif ! ptmeth == 4
    !
    ! ptmeth == 5 : produce "high" and "low" band partitions
    ! using a frequency cutoff:
    !
    if( ptmeth .eq. 5 ) then
      sigcut = tpi * ptfcut
      do ik = 1, nk
        ! if bin center <= freq cutoff then mark as "low band".
        if(sig(ik) .le. sigcut) then
          ip = 2
        else
          ip = 1
        endif
        do ith=1, nth
          isp = ik + (ith-1) * nk ! index into partition array imo
          imo(isp) = ip
        enddo
      enddo
      ! we only ever have 2 partitions:
      np_max=2
      ! calculate mean parameters:
      call ptmean ( np_max, imo, zp, depth, uabs, udir, wn,           &
           np, xp, dimxp, pmap )
      ! no more processing required, return:
      return
    endif ! ptmeth == 5
    !
    ! 1.b invert spectrum and 'digitize'
    !
    zmin   = minval ( zp )
    zmax   = maxval ( zp )
    if ( zmax-zmin .lt. 1.e-9 ) return
    !
    z      = zmax - zp
    !
    fact   = real(ihmax-1) / ( zmax - zmin )
    imi    = max ( 1 , min ( ihmax , nint ( 1. + z*fact ) ) )
    !
    ! 1.c sort digitized image
    !
    call ptsort ( imi, ind, ihmax )
    !
    ! -------------------------------------------------------------------- /
    ! 2.  perform partitioning
    ! 2.a update nearest neighbor info as needed.
    !
    call ptnghb
    !
    ! 2.b incremental flooding
    !
    call pt_fld ( imi, ind, imo, zp, np_max )
    !
    ! 2.c compute parameters per partition
    !     np and nx initialized inside routine.
    !
    call ptmean ( np_max, imo, zp, depth, uabs, udir, wn,           &
         np, xp, dimxp, pmap )
    !
    ! 2.d ptmeth == 2: move the wind sea part of the partitions into a
    !     seperate partition and recalculate the mean parameters.
    !
    if ( np .gt. 0 .and. ptmeth .eq. 2 ) then
      wind_part = np_max
      do ik=1, nk
        do ith=1, nth
          isp = ik + (ith-1) * nk ! index into partition array imo
          upar = wsmult * uabs * max(0.0, cos(th(ith)-dera*udir))
          c = sig(ik) / wn(ik)
          if( c .lt. upar ) then
            ! bin is wind forced - mark as new wind partition:
            wind_part = np_max + 1
            ! update status map to show new wind partition
            imo(isp) = wind_part
          endif
        enddo
      enddo
      if( wind_part .ne. np_max ) then
        ! some bins were marked as wind sea - recalculate
        ! integrated parameters:
        np_max = wind_part
        call ptmean ( np_max, imo, zp, depth, uabs, udir, wn,     &
             np, xp, dimxp, pmap )
      endif
    endif
    !
    ! -------------------------------------------------------------------- /
    ! 3.  sort and recombine wind seas as needed
    ! 3.a sort by wind sea fraction
    !
    if ( np .le. 1 ) return
    ! -----------------------------------------------------------------
    ! ptmeth == 3: don't classify or combine any partitions as wind sea.
    ! simply sort by hs and return.
    ! -----------------------------------------------------------------
    if( ptmeth .eq. 3 ) then
      tp(:,1:np)  = xp(:,1:np)
      xp(:,1:np)  = 0.
      do ip=1, np
        it          = maxloc(tp(1,1:np))
        xp(:,ip)    = tp(:,it(1))
        tp(1,it(1)) = -1.
      end do
      return ! don't process any further
    endif ! ptmeth == 3
    ! -----------------------------------------------------------------
    ! ptmeth == 1: default ww3 partitioning.
    ! -----------------------------------------------------------------
    tp(:,1:np)  = xp(:,1:np)
    xp(:,1:np)  = 0.
    index(1:np) = 0
    nws         = 0
    !
    do ip=1, np
      it          = maxloc(tp(6,1:np))
      index(ip)   = it(1)
      xp(:,ip)    = tp(:,index(ip))
      if ( tp(6,it(1)) .ge. wscut ) nws = nws + 1
      tp(6,it(1)) = -1.
    end do
    !
    ! 3.b combine wind seas as needed and resort
    !
    if ( nws.gt.1 .and. flcomb ) then
      ipw    = pmap(index(1))
      do ip=2, nws
        ipt    = pmap(index(ip))
        do isp=1, nspec
          if ( imo(isp) .eq. ipt ) imo(isp) = ipw
        end do
      end do
      !
      call ptmean ( np_max, imo, zp, depth, uabs, udir, wn,       &
           np, xp, dimxp, pmap )
      if ( np .le. 1 ) return
      !
      tp(:,1:np)  = xp(:,1:np)
      xp(:,1:np)  = 0.
      index(1:np) = 0
      nws         = 0
      !
      do ip=1, np
        it          = maxloc(tp(6,1:np))
        index(ip)   = it(1)
        xp(:,ip)    = tp(:,index(ip))
        if ( tp(6,it(1)) .ge. wscut ) nws = nws + 1
        tp(6,it(1)) = -1.
      end do
      !
    end if
    !
    ! 3.c sort remaining fields by wave height
    !
    nws    = min ( 1 , nws )
    !
    tp(:,1:np)  = xp(:,1:np)
    xp(:,1:np)  = 0.
    !
    if ( nws .gt. 0 ) then
      xp(:,1) = tp(:,1)
      tp(1,1) = -1.
      nws     = 1
    end if
    !
    do ip=nws+1, np
      it          = maxloc(tp(1,1:np))
      xp(:,ip)    = tp(:,it(1))
      tp(1,it(1)) = -1.
    end do
    !
    ! -------------------------------------------------------------------- /
    ! 4.  end of routine
    !
    return
    !/
    !/ end of w3part ----------------------------------------------------- /
    !/
  end subroutine w3part
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief sorts the image data in ascending order.
  !>
  !> @details this sort original to f. t. tracy (2006)
  !>
  !> @param[in]   imi     input discretized spectrum
  !> @param[out]  ind     sorted data
  !> @param[in]   ihmax   number of integer levels
  !>
  !> @author barbara tracy
  !> @date 19 oct 2006
  !>
  subroutine ptsort ( imi, ind, ihmax )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii          usace/noaa |
    !/                  |          barbara  tracy           |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         19-oct-2006 !
    !/                  +-----------------------------------+
    !/
    !/    19-oct-2006 : origination.                       ( version 3.10 )
    !/
    !  1. purpose :
    !
    !     this subroutine sorts the image data in ascending order.
    !     this sort original to f.t.tracy (2006)
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       imi     i.a.   i   input discretized spectrum.
    !       ind     i.a.   o   sorted data.
    !       ihmax   int.   i   number of integer levels.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    sur.  w3servmd subroutine tracing.
    !     ----------------------------------------------------------------
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    !
    use w3gdatmd, only: nspec
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)      :: ihmax, imi(nspec)
    integer, intent(out)     :: ind(nspec)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: i, in, iv
    integer                 :: numv(ihmax), iaddr(ihmax),           &
         iorder(nspec)
    !/
    !
    ! -------------------------------------------------------------------- /
    ! 1.  occurences per height
    !
    numv   = 0
    do i=1, nspec
      numv(imi(i)) = numv(imi(i)) + 1
    end do
    !
    ! -------------------------------------------------------------------- /
    ! 2.  starting address per height
    !
    iaddr(1) = 1
    do i=1, ihmax-1
      iaddr(i+1) = iaddr(i) + numv(i)
    end do
    !
    ! -------------------------------------------------------------------- /
    ! 3.  order points
    !
    do i=1, nspec
      iv        = imi(i)
      in        = iaddr(iv)
      iorder(i) = in
      iaddr(iv) = in + 1
    end do
    !
    ! -------------------------------------------------------------------- /
    ! 4.  sort points
    !
    do i=1, nspec
      ind(iorder(i)) = i
    end do
    !
    return
    !/
    !/ end of ptsort ----------------------------------------------------- /
    !/
  end subroutine ptsort
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief nearest neighbour calculation
  !>
  !> @details
  !>  computes the nearest neighbors for each grid point. wrapping of
  !>  directional distribution (0 to 360) is taken care of using the
  !>  nearest neighbor system
  !>
  !> @param[in]   imi     input discretized spectrum
  !> @param[out]  imd     sorted data
  !> @param[in]   ihmax   number of integer levels
  !>
  !> @author barbara tracy
  !> @date 20 oct 2006
  !>
  subroutine ptnghb
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii          usace/noaa |
    !/                  |          barbara  tracy           |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         20-oct-2006 !
    !/                  +-----------------------------------+
    !/
    !/    20-oct-2006 : origination.                       ( version 3.10 )
    !/
    !  1. purpose :
    !
    !     this subroutine computes the nearest neighbors for each grid
    !     point. wrapping of directional distribution (0 to 360)is taken
    !     care of using the nearest neighbor system
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       imi     i.a.   i   input discretized spectrum.
    !       imd     i.a.   o   sorted data.
    !       ihmax   int.   i   number of integer levels.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    sur.  w3servmd subroutine tracing.
    !     ----------------------------------------------------------------
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    !
    use w3gdatmd, only: nk, nth, nspec
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !     integer, intent(in)      :: ihmax, imi(nspec)
    !     integer, intent(in)      :: imd(nspec)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: n, j, i, k
    !
    ! -------------------------------------------------------------------- /
    ! 1.  check on need of processing
    !
    if ( mk.eq.nk .and. mth.eq.nth ) return
    !
    if ( mk.gt.0 ) deallocate ( neigh )
    allocate ( neigh(9,nspec) )
    mk     = nk
    mth    = nth
    !
    ! -------------------------------------------------------------------- /
    ! 2.  build map
    !
    neigh  = 0
    !
    ! ... base loop
    !
    do n = 1, nspec
      !
      j      = (n-1) / nk + 1
      i      = n - (j-1) * nk
      k      = 0
      !
      ! ... point at the left(1)
      !
      if ( i .ne. 1 ) then
        k           = k + 1
        neigh(k, n) = n - 1
      end if
      !
      ! ... point at the right (2)
      !
      if ( i .ne. nk ) then
        k           = k + 1
        neigh(k, n) = n + 1
      end if
      !
      ! ... point at the bottom(3)
      !
      if ( j .ne. 1 ) then
        k           = k + 1
        neigh(k, n) = n - nk
      end if
      !
      ! ... add point at bottom_wrap to top
      !
      if ( j .eq. 1 ) then
        k          = k + 1
        neigh(k,n) = nspec - (nk-i)
      end if
      !
      ! ... point at the top(4)
      !
      if ( j .ne. nth ) then
        k           = k + 1
        neigh(k, n) = n + nk
      end if
      !
      ! ... add point to top_wrap to bottom
      !
      if ( j .eq. nth ) then
        k          = k + 1
        neigh(k,n) = n - (nth-1) * nk
      end if
      !
      ! ... point at the bottom, left(5)
      !
      if ( (i.ne.1) .and. (j.ne.1) ) then
        k           = k + 1
        neigh(k, n) = n - nk - 1
      end if
      !
      ! ... point at the bottom, left with wrap.
      !
      if ( (i.ne.1) .and. (j.eq.1) ) then
        k          = k + 1
        neigh(k,n) = n - 1 + nk * (nth-1)
      end if
      !
      ! ... point at the bottom, right(6)
      !
      if ( (i.ne.nk) .and. (j.ne.1) ) then
        k           = k + 1
        neigh(k, n) = n - nk + 1
      end if
      !
      ! ... point at the bottom, right with wrap
      !
      if ( (i.ne.nk) .and. (j.eq.1) ) then
        k           = k + 1
        neigh(k,n) = n + 1 + nk * (nth - 1)
      end  if
      !
      ! ... point at the top, left(7)
      !
      if ( (i.ne.1) .and. (j.ne.nth) ) then
        k           = k + 1
        neigh(k, n) = n + nk - 1
      end if
      !
      ! ... point at the top, left with wrap
      !
      if ( (i.ne.1) .and. (j.eq.nth) ) then
        k           = k + 1
        neigh(k,n) = n - 1 - (nk) * (nth-1)
      end if
      !
      ! ... point at the top, right(8)
      !
      if ( (i.ne.nk) .and. (j.ne.nth) ) then
        k           = k + 1
        neigh(k, n) = n + nk + 1
      end if
      !
      ! ... point at top, right with wrap
      !
      !
      if ( (i.ne.nk) .and. (j.eq.nth) ) then
        k           = k + 1
        neigh(k,n) = n + 1 - (nk) * (nth-1)
      end if
      !
      neigh(9,n) = k
      !
    end do
    !
    return
    !/
    !/ end of ptnghb ----------------------------------------------------- /
    !/
  end subroutine ptnghb
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief image watersheding
  !>
  !> @details
  !>  this subroutine does incremental flooding of the image to
  !>  determine the watershed image.
  !>
  !> @param[in]   imi     input discretized spectrum
  !> @param[in]   ind     sorted addresses
  !> @param[out]  imo     output partitioned spectrum
  !> @param[in]   zp      spectral array
  !> @param[out]  npart   number of partitions found
  !>
  !> @author h.l. tolman
  !> @date 01 nov 2006
  !>
  subroutine pt_fld ( imi, ind, imo, zp, npart )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         01-nov-2006 !
    !/                  +-----------------------------------+
    !/
    !/    01-nov-2006 : origination.                       ( version 3.10 )
    !/
    !  1. purpose :
    !
    !     this subroutine does incremental flooding of the image to
    !     determine the watershed image.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       imi     i.a.   i   input discretized spectrum.
    !       ind     i.a.   i   sorted addresses.
    !       imo     i.a.   o   output partitioned spectrum.
    !       zp      r.a.   i   spectral array.
    !       npart   int.   o   number of partitions found.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    sur.  w3servmd subroutine tracing.
    !     ----------------------------------------------------------------
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    !
    use w3gdatmd, only: nspec
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: imi(nspec), ind(nspec)
    integer, intent(out)    :: imo(nspec), npart
    real, intent(in)        :: zp(nspec)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: mask, init, iwshed, imd(nspec),      &
         ic_label, ifict_pixel, m, ih, msave, &
         ip, i, ipp, ic_dist, iempty, ippp,   &
         jl, jn, ipt, j
    integer                 :: iq(nspec), iq_start, iq_end
    real                    :: zpmax, ep1, diff
    !/
    !
    ! -------------------------------------------------------------------- /
    ! 0.  initializations
    !
    mask        = -2
    init        = -1
    iwshed      =  0
    imo         = init
    ic_label    =  0
    imd         =  0
    ifict_pixel = -100
    !
    iq_start    =  1
    iq_end      =  1
    !
    zpmax       = maxval ( zp )
    !
    ! -------------------------------------------------------------------- /
    ! 1.  loop over levels
    !
    m      =  1
    !
    do ih=1, ihmax
      msave  = m
      !
      ! 1.a pixels at level ih
      !
      do
        ip     = ind(m)
        if ( imi(ip) .ne. ih ) exit
        !
        !     flag the point, if it stays flagge, it is a separate minimum.
        !
        imo(ip) = mask
        !
        !     consider neighbors. if there is neighbor, set distance and add
        !     to queue.
        !
        do i=1, neigh(9,ip)
          ipp    = neigh(i,ip)
          if ( (imo(ipp).gt.0) .or. (imo(ipp).eq.iwshed) ) then
            imd(ip) = 1
            call fifo_add (ip)
            exit
          end if
        end do
        !
        if ( m+1 .gt. nspec ) then
          exit
        else
          m = m + 1
        end if
        !
      end do
      !
      ! 1.b process the queue
      !
      ic_dist = 1
      call fifo_add (ifict_pixel)
      !
      do
        call fifo_first (ip)
        !
        !     check for end of processing
        !
        if ( ip .eq. ifict_pixel ) then
          call fifo_empty (iempty)
          if ( iempty .eq. 1 ) then
            exit
          else
            call fifo_add (ifict_pixel)
            ic_dist = ic_dist + 1
            call fifo_first (ip)
          end if
        end if
        !
        !     process queue
        !
        do i=1, neigh(9,ip)
          ipp = neigh(i,ip)
          !
          !     check for labeled watersheds or basins
          !
          if ( (imd(ipp).lt.ic_dist) .and. ( (imo(ipp).gt.0) .or.  &
               (imo(ipp).eq.iwshed))) then
            !
            if ( imo(ipp) .gt. 0 ) then
              !
              if ((imo(ip) .eq. mask) .or. (imo(ip) .eq. &
                   iwshed)) then
                imo(ip) = imo(ipp)
              else if (imo(ip) .ne. imo(ipp)) then
                imo(ip) = iwshed
              end if
              !
            else if (imo(ip) .eq. mask) then
              !
              imo(ip) = iwshed
              !
            end if
            !
          else if ( (imo(ipp).eq.mask) .and. (imd(ipp).eq.0) ) then
            !
            imd(ipp) = ic_dist + 1
            call fifo_add (ipp)
            !
          end if
          !
        end do
        !
      end do
      !
      ! 1.c check for mask values in imo to identify new basins
      !
      m = msave
      !
      do
        ip     = ind(m)
        if ( imi(ip) .ne. ih ) exit
        imd(ip) = 0
        !
        if (imo(ip) .eq. mask) then
          !
          ! ... new label for pixel
          !
          ic_label = ic_label + 1
          call fifo_add (ip)
          imo(ip) = ic_label
          !
          ! ... and all connected to it ...
          !
          do
            call fifo_empty (iempty)
            if ( iempty .eq. 1 ) exit
            call fifo_first (ipp)
            !
            do i=1, neigh(9,ipp)
              ippp   = neigh(i,ipp)
              if ( imo(ippp) .eq. mask ) then
                call fifo_add (ippp)
                imo(ippp) = ic_label
              end if
            end do
            !
          end do
          !
        end if
        !
        if ( m + 1 .gt. nspec ) then
          exit
        else
          m = m + 1
        end if
        !
      end do
      !
    end do
    !
    ! -------------------------------------------------------------------- /
    ! 2.  find nearest neighbor of 0 watershed points and replace
    !     use original input to check which group to affiliate with 0
    !     soring changes first in imd to assure symetry in adjustment.
    !
    do j=1, 5
      imd    = imo
      do jl=1 , nspec
        ipt    = -1
        if ( imo(jl) .eq. 0 ) then
          ep1    = zpmax
          do jn=1, neigh (9,jl)
            diff   = abs ( zp(jl) - zp(neigh(jn,jl)))
            if ( (diff.le.ep1) .and. (imo(neigh(jn,jl)).ne.0) ) then
              ep1    = diff
              ipt    = jn
            end if
          end do
          if ( ipt .gt. 0 ) imd(jl) = imo(neigh(ipt,jl))
        end if
      end do
      imo    = imd
      if ( minval(imo) .gt. 0 ) exit
    end do
    !
    npart = ic_label
    !
    return
    !
  contains
    !/ ------------------------------------------------------------------- /
    !> @brief add point to fifo queue
    !>
    !> @param[in]  iv  point to add
    !>
    !> @author barbara tracy
    !> @date 01 nov 2006
    subroutine fifo_add ( iv )
      integer, intent(in)      :: iv
      !
      iq(iq_end) = iv
      !
      iq_end = iq_end + 1
      if ( iq_end .gt. nspec ) iq_end = 1
      !
      return
    end subroutine fifo_add
    !/ ------------------------------------------------------------------- /
    !> @brief check if queue is empty.
    !>
    !> @param[out]  iempty  set to 1 if queue is empty, else 0
    !>
    !> @author barbara tracy
    !> @date 01 nov 2006
    !>
    subroutine fifo_empty ( iempty )
      integer, intent(out)     :: iempty
      !
      if ( iq_start .ne. iq_end ) then
        iempty = 0
      else
        iempty = 1
      end if
      !
      return
    end subroutine fifo_empty
    !/ ------------------------------------------------------------------- /
    !> @brief get point out of queue.
    !>
    !> @param[out]  iv  returned point
    !>
    !> @author barbara tracy
    !> @date 01 nov 2006
    !>
    subroutine fifo_first ( iv )
      integer, intent(out)     :: iv
      !
      iv = iq(iq_start)
      !
      iq_start = iq_start + 1
      if ( iq_start .gt. nspec ) iq_start = 1
      !
      return
    end subroutine fifo_first
    !/
    !/ end of pt_fld ----------------------------------------------------- /
    !/
  end subroutine pt_fld
  !/ ------------------------------------------------------------------- /
  !> @brief compute mean parameters per partition
  !>
  !> @param[in]    npi     number of partitions found.
  !> @param[in]    imo     partition map.
  !> @param[in]    zp      input spectrum.
  !> @param[in]    depth   water depth.
  !> @param[in]    uabs    wind speed.
  !> @param[in]    udir    wind direction.
  !> @param[in]    wn      wavenumebers for each frequency.
  !> @param[out]   npo     number of partitions with mean parameters.
  !> @param[out]   xp      array with output parameters.
  !> @param[in]    dimxp   second dimension of xp.
  !> @param[out]   pmap    mapping between orig. and combined partitions
  !>
  !> @author barbara tracy, h. l. tolman, m. szyszka, c. bunney
  !> @date 02 dec 2010
  !>
  subroutine ptmean ( npi, imo, zp, depth, uabs, udir, wn,        &
       npo, xp, dimxp, pmap )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii          usace/noaa |
    !/                  |          barbara  tracy           |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         02-dec-2010 !
    !/                  +-----------------------------------+
    !/
    !/    28-oct-2006 : origination.                       ( version 3.10 )
    !/    02-nov-2006 : adding tail to integration.        ( version 3.10 )
    !/    24-mar-2007 : adding overall field.              ( version 3.11 )
    !/    02-dec-2010 : adding a mapping pmap between      ( version 3.14 )
    !/                  original and combined partitions
    !/                  ( m. szyszka )
    !/
    !  1. purpose :
    !
    !     compute mean parameters per partition.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       npi     int.   i   number of partitions found.
    !       imo     i.a.   i   partition map.
    !       zp      r.a.   i   input spectrum.
    !       depth   real   i   water depth.
    !       uabs    real   i   wind speed.
    !       udir    real   i   wind direction.
    !       wn      r.a.   i   wavenumebers for each frequency.
    !       npo     int.   o   number of partitions with mean parameters.
    !       xp      r.a.   o   array with output parameters.
    !       dimxp   int.   i   second dimension of xp.
    !       pmap    i.a.   o   mapping between orig. and combined partitions
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    sur.  w3servmd subroutine tracing.
    !      wavnu1    subr. w3dispmd wavenumber computation.
    !     ----------------------------------------------------------------
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    use constants
    use w3dispmd, only: wavnu1
    !
    use w3gdatmd, only: nk, nth, nspec, dth, sig, dsii, dsip,       &
         ecos, esin, xfr, fachfe, th, fte
    use w3odatmd, only: iaproc, naperr, ndse, ndst
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: npi, imo(nspec), dimxp
    integer, intent(out)    :: npo, pmap(dimxp)
    real, intent(in)        :: zp(nspec), depth, uabs, udir, wn(nk)
    real, intent(out)       :: xp(dimp,0:dimxp)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ik, ith, isp, ip, ifpmax(0:npi)
    real                    :: sumf(0:nk+1,0:npi), sumfw(nk,0:npi), &
         sumfx(nk,0:npi), sumfy(nk,0:npi),    &
         sume(0:npi), sumew(0:npi),           &
         sumex(0:npi), sumey(0:npi),          &
         efpmax(0:npi), fcdir(nth)
    real,dimension(0:npi)   :: sume1, sume2, sumem1, sumqp
    real                    :: hs, xl, xh, xl2, xh2, el, eh, denom, &
         sigp, wnp, cgp, upar, c(nk), rd, fact
    real                    :: qp, m0, m1, m2, mm1, fsprd, epm_fp, alp_pm
    real                    :: y, yhat, xhat, sumxy, sumylogy, numer,&
         sumy, sumxxy, sumxylogy, sumexp, sumeyp
    real                    :: fteii
    !/
    !
    ! -------------------------------------------------------------------- /
    ! 1.  check on need of processing
    !
    npo    = 0
    xp     = 0.
    !
    if ( npi .eq. 0 ) return
    !
    ! -------------------------------------------------------------------- /
    ! 2.  initialize arrays
    !
    sumf   = 0.
    sumfw  = 0.
    sumfx  = 0.
    sumfy  = 0.
    sume   = 0.
    !
    sume1  = 0.  !/ first spectral moment
    sume2  = 0.  !/ second spectral moment
    sumem1 = 0.  !/ inverse spectral moment
    sumqp  = 0.  !/ peakedness parameter
    !
    sumew  = 0.
    sumex  = 0.
    sumey  = 0.
    ifpmax = 0
    efpmax = 0.
    !
    do ik=1, nk
      c(ik)  = sig(ik) / wn(ik)
    end do
    !
    do ith=1, nth
      upar   = wsmult * uabs * max(0.,cos(th(ith)-dera*udir))
      if ( upar .lt. c(nk) ) then
        fcdir(ith) = sig(nk+1)
      else
        do ik=nk-1, 2, -1
          if ( upar .lt. c(ik) ) exit
        end do
        rd     = (c(ik)-upar) / (c(ik)-c(ik+1))
        if ( rd .lt. 0 ) then
          ik     = 0
          rd     = max ( 0., rd+1. )
        end if
        fcdir(ith) = rd*sig(ik+1) + (1.-rd)*sig(ik)
      end if
    end do
    !
    ! -------------------------------------------------------------------- /
    ! 3.  spectral integrals and preps
    ! 3.a integrals
    !     note: factor dth only used in hs computation.
    !
    do ik=1, nk
      do ith=1, nth
        isp    = ik + (ith-1)*nk
        ip     = imo(isp)
        fact   = max ( 0. , min ( 1. ,                              &
             1. - ( fcdir(ith) - 0.5*(sig(ik-1)+sig(ik)) ) / dsip(ik) ) )
        sumf (ik, 0) = sumf (ik, 0) + zp(isp)
        sumfw(ik, 0) = sumfw(ik, 0) + zp(isp) * fact
        sumfx(ik, 0) = sumfx(ik, 0) + zp(isp) * ecos(ith)
        sumfy(ik, 0) = sumfy(ik, 0) + zp(isp) * esin(ith)
        if ( ip .eq. 0 ) cycle
        sumf (ik,ip) = sumf (ik,ip) + zp(isp)
        sumfw(ik,ip) = sumfw(ik,ip) + zp(isp) * fact
        sumfx(ik,ip) = sumfx(ik,ip) + zp(isp) * ecos(ith)
        sumfy(ik,ip) = sumfy(ik,ip) + zp(isp) * esin(ith)
      end do
    end do
    sumf(nk+1,:) = sumf(nk,:) * fachfe
    !
    do ip=0, npi
      do ik=1, nk
        sume (ip) = sume (ip) + sumf (ik,ip) * dsii(ik)
        sumqp(ip) = sumqp(ip) + sumf (ik,ip)**2 * dsii(ik) * sig(ik)
        sume1(ip) = sume1(ip) + sumf (ik,ip) * dsii(ik) * sig(ik)
        sume2(ip) = sume2(ip) + sumf (ik,ip) * dsii(ik) * sig(ik)**2
        sumem1(ip) = sumem1(ip) + sumf (ik,ip) * dsii(ik) / sig(ik)
        sumew(ip) = sumew(ip) + sumfw(ik,ip) * dsii(ik)
        sumex(ip) = sumex(ip) + sumfx(ik,ip) * dsii(ik)
        sumey(ip) = sumey(ip) + sumfy(ik,ip) * dsii(ik)
        if ( sumf(ik,ip) .gt. efpmax(ip) ) then
          ifpmax(ip) = ik
          efpmax(ip) = sumf(ik,ip)
        end if
      end do
      !sume (ip) = sume (ip) + sumf (nk,ip) * fte
      !sume1(ip) = sume1(ip) + sumf (nk,ip) * fte
      !sume2(ip) = sume2(ip) + sumf (nk,ip) * fte
      !sumem1(ip) = sumem1(ip) + sumf (nk,ip) * fte
      !sumqp(ip) = sumqp(ip) + sumf (nk,ip) * fte
      !sumew(ip) = sumew(ip) + sumfw(nk,ip) * fte
      !sumex(ip) = sumex(ip) + sumfx(nk,ip) * fte
      !sumey(ip) = sumey(ip) + sumfy(nk,ip) * fte
      ! met office: proposed bugfix for tail calculations, previously
      !  pt1 and pt2 values were found to be too low when using the
      !  fte scaling factor for the tail. i think there are two issues:
      !  1. energy spectrum is scaled in radian frequency space above by dsii.
      !     this needs to be consistent and fte contains a dth*sig(nk)
      !     factor that is not used in the dsii scaled calcs above
      !  2. the tail fit calcs for period parameters needs to follow
      !     the form used in w3iogomd and scaling should be
      !     based on the relationship between fte and ft1, fttr etc.
      !     as per w3iogomd and ww3_grid
      fteii = fte / (dth * sig(nk))
      sume (ip) = sume (ip) + sumf (nk,ip) * fteii
      sume1(ip) = sume1(ip) + sumf (nk,ip) * sig(nk) * fteii * (0.3333 / 0.25)
      sume2(ip) = sume2(ip) + sumf (nk,ip) * sig(nk)**2 * fteii * (0.5 / 0.25)
      sumem1(ip) = sumem1(ip) + sumf (nk,ip) / sig(nk) * fteii * (0.2 / 0.25)
      sumqp(ip) = sumqp(ip) + sumf (nk,ip) * fteii
      sumew(ip) = sumew(ip) + sumfw(nk,ip) * fteii
      sumex(ip) = sumex(ip) + sumfx(nk,ip) * fteii
      sumey(ip) = sumey(ip) + sumfy(nk,ip) * fteii
    end do
    !
    ! -------------------------------------------------------------------- /
    ! 4.  compute pars
    !
    npo    = -1
    !
    do ip=0, npi
      !
      sumexp = 0.
      sumeyp = 0.
      !
      m0 = sume(ip)  * dth * tpiinv
      hs     = 4. * sqrt ( max( m0 , 0. ) )
      if ( hs .lt. hspmin ) then
        ! for wind cutoff and 2-band partitioning methods, keep the
        ! partition, but set the integrated parameters to undef
        ! for hs values less that hspmin:
        if( ptmeth .eq. 4 .or. ptmeth .eq. 5 ) then
          npo = npo + 1
          xp(:,npo) = undef
          xp(6,npo) = 0.0 ! set wind sea frac to zero
        endif
        cycle
      endif
      !
      if ( npo .ge. dimxp ) goto 2000
      npo = npo + 1
      if (ip.gt.0)then
        if(npo.lt.1)cycle
        pmap(npo) = ip
      endif
      !
      m1 = sume1(ip) * dth * tpiinv**2
      m2 = sume2(ip) * dth * tpiinv**3
      mm1 = sumem1(ip) * dth
      qp = sumqp(ip) *(dth * tpiinv)**2
      !       m1 = max( m1, 1.e-7 )
      !       m2 = max( m2, 1.e-7 )
      !
      xl     = 1. / xfr - 1.
      xh     = xfr - 1.
      xl2    = xl**2
      xh2    = xh**2
      el     = sumf(ifpmax(ip)-1,ip) - sumf(ifpmax(ip),ip)
      eh     = sumf(ifpmax(ip)+1,ip) - sumf(ifpmax(ip),ip)
      denom  = xl*eh - xh*el
      sigp   = sig(ifpmax(ip))
      if (denom.ne.0.) then
        sigp   = sigp *( 1. + 0.5 * ( xl2*eh - xh2*el ) &
             / sign ( abs(denom) , denom ) )
      end if
      call wavnu1 ( sigp, depth, wnp, cgp )
      !
      !/ --- parabolic fit around the spectral peak ---
      ik = ifpmax(ip)
      efpmax(ip) = sumf(ik,ip) * dth
      if (ik.gt.1 .and. ik.lt.nk) then
        el    = sumf(ik-1,ip) * dth
        eh    = sumf(ik+1,ip) * dth
        numer = 0.125 * ( el - eh )**2
        denom = el - 2. * efpmax(ip) + eh
        if (denom.ne.0.) efpmax(ip) = efpmax(ip)         &
             - numer / sign( abs(denom),denom )
      end if
      !
      !/ --- weighted least-squares regression to estimate frequency
      !/     spread (fsprd) to an exponential function:
      !/              e(f) = a * exp(-1/2*(f-fp)/b)**2             ,
      !/     where b is frequency spread and  e(f) is used for
      !/     weighting to avoid greater weights to smalll values
      !/     in ordinary least-square fit. ---
      fsprd     = undef
      sumy      = 0.
      sumxy     = 0.
      sumxxy    = 0.
      sumylogy  = 0.
      sumxylogy = 0.
      !
      do ik=1, nk
        y = sumf(ik,ip)*dth
        ! --- sums for weighted least-squares ---
        if (y.ge.1.e-15) then
          yhat = log(y)
          xhat = -0.5 * ( (sig(ik)-sigp)*tpiinv )**2
          sumy      = sumy + y
          sumxy     = sumxy + xhat * yhat
          sumxxy    = sumxxy + xhat * xhat * y
          sumylogy  = sumylogy + y * yhat
          sumxylogy = sumxylogy + sumxy * yhat
        end if
      end do
      !
      numer = sumy * sumxxy - sumxy**2
      denom = sumy * sumxylogy - sumxy * sumylogy
      if (denom.ne.0.)  fsprd = sqrt( numer / sign(abs(denom),numer) )
      !
      sumexp = sumfx(ifpmax(ip),ip) * dsii(ifpmax(ip))
      sumeyp = sumfy(ifpmax(ip),ip) * dsii(ifpmax(ip))
      !
      !/ --- significant wave height ---
      xp(1,npo) = hs
      !/ --- peak wave period ---
      xp(2,npo) = tpi / sigp
      !/ --- peak wave length ---
      xp(3,npo) = tpi / wnp
      !/ --- mean wave direction ---
      xp(4,npo) = mod( 630.-atan2(sumey(ip),sumex(ip))*rade , 360. )
      !/ --- mean directional spread ---
      xp(5,npo) = rade * sqrt ( max ( 0. , 2. * ( 1. - sqrt ( &
           max(0.,(sumex(ip)**2+sumey(ip)**2)/sume(ip)**2) ) ) ) )
      !/ --- wind sea fraction ---
      xp(6,npo) = sumew(ip) / sume(ip)
      !/ --- peak wave direction ---
      xp(7,npo) =  mod(630.-atan2(sumeyp,sumexp)*rade , 360.)
      !/ --- spectral width (longuet-higgins 1975) ---
      xp(8,npo) = sqrt( max( 1. , m2*m0 / m1**2 ) - 1. )
      !/ --- jonswap peak enhancement parameter (e(fp)/epm(fp))---
      !  epm_fmx = alpha_pm_fmx * grav**2 * tpi * sigp**-5 * exp(-5/4)
      alp_pm = 0.3125 * hs**2 * (sigp)**4
      epm_fp = alp_pm * tpi * (sigp**(-5)) * 2.865048e-1
      xp(9,npo) = max( efpmax(ip) / epm_fp , 1.0 )
      !/ --- peakedness parameter (goda 1970) ---
      xp(10,npo) = 2. * qp / m0**2
      !/ --- gaussian frequency width ---
      xp(11,npo) = fsprd
      !/ --- wave energy period (inverse moment) ---
      xp(12,npo) = mm1 / m0
      !/ --- mean wave period (first moment) ---
      xp(13,npo) = m0 / m1
      !/ --- zero-upcrossing period (second moment) ---
      xp(14,npo) = sqrt( m0 / m2 )
      !/ --- peak spectral density (one-dimensional) ---
      xp(15,npo) = efpmax(ip)
      !
    end do
    !
    return
    !
    ! escape locations read errors --------------------------------------- *
    !
2000 continue
    if ( iaproc .eq. naperr ) write (ndse,1000) npo+1
    return
    !
    ! formats
    !
1000 format (/' *** wavewatch iii error in ptmean :'/                &
         '     xp array too small at partition',i6/)
    !/
    !/ end of ptmean ----------------------------------------------------- /
    !/
  end subroutine ptmean
  !/
  !/ end of module w3partmd -------------------------------------------- /
  !/
end module w3partmd
