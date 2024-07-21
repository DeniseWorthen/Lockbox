!> @file
!> @brief convert spectra to new discrete spectral grid.
!>
!> @author h. l. tolman
!> @date   01-nov-2012
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
!> @brief convert spectra to new discrete spectral grid.
!>
!> @author h. l. tolman
!> @date   01-nov-2012
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3cspcmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         01-nov-2012 |
  !/                  +-----------------------------------+
  !/
  !/    19-sep-2005 : origination.                        ( version 3.08 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/    01-nov-2012 : minor code clean-up (tabs & coments)( version 4.08 )
  !/
  !/    copyright 2009 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     convert spectra to new discrete spectral grid.
  !
  !  2. variables and types :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      ncases    int.  private  number of cases for which interpol.
  !                               data is stored.
  !      idata     case  private  interpolation data.
  !     ----------------------------------------------------------------
  !
  !     elements of the data structure case are given below. the middle
  !     block pf parameters has pointer aliasses with the same name in
  !     the subroutine.
  !
  !      name      type  description
  !     ----------------------------------------------------------------
  !      icase     int.  number of case.
  !      nfr1, nth1, nfr2, nth2, xf1, fr1, th1, xf2, fr2, th2
  !                      same as in parameter list of routine.
  !
  !      dth1      real  directional increment.
  !      dth2      real  directional increment.
  !      idth      i.a.  index information for redistribution of
  !                      energy in direction space.
  !      rdth      r.a.  factors corresponding to idth.
  !      frq1      r.a.  frequencies.
  !      frq2      r.a.  frequencies.
  !      xdf1      real  factor for increments.
  !      xdf2      real  factor for increments.
  !      nfr2t     int.  frequency to start the tail.
  !      idfr      i.a.  idem for frequencies.
  !      rdfr      r.a.  factors corresponding to idfr.
  !
  !      next      case  pointer to next data set stored.
  !     ----------------------------------------------------------------
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3cspc    subr. public   perform conversion for vector of
  !                               spectra.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !     see subroutine w3cspc.
  !
  !  5. remarks :
  !
  !     - conversion data are sored in an endless linked chain, which
  !       is tested at the beginning of the routine.
  !
  !  6. switches :
  !
  !     see subroutine.
  !
  !  7. source code :
  !
  !/ ------------------------------------------------------------------- /
  public
  !/
  type case
    integer               :: icase, nfr1, nth1, nfr2, nth2, nfr2t
    real                  :: xf1, fr1, th1, xf2, fr2, th2,       &
         dth1, dth2, xdf1, xdf2
    integer, pointer      :: idth(:,:), idfr(:,:)
    real, pointer         :: rdth(:,:), frq1(:), frq2(:), rdfr(:,:)
    type(case), pointer   :: next
  end type case
  !/
  integer, private        :: ncases = 0
  type(case), private, pointer :: idata
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief convert a set of spectra to a new spectral grid.
  !>
  !> @details conservative distribution of input energies over new grid.
  !>
  !> @param[in]  sp1  input spectra.
  !> @param[in]  nfr1 input number of frequencies.
  !> @param[in]  nth1 input number of directions.
  !> @param[in]  xf1  input frequency increment factor.
  !> @param[in]  fr1  first input frequency.
  !> @param[in]  th1  first input direction.
  !> @param[out] sp2  output spectra.
  !> @param[in]  nfr2 output number of frequencies.
  !> @param[in]  nth2 output number of directions.
  !> @param[in]  xf2  output frequency increment factor.
  !> @param[in]  fr2  first output frequency.
  !> @param[in]  th2  first output direction.
  !> @param[in]  nsp  number of spectra.
  !> @param[in]  ndst unit number for test output.
  !> @param[in]  ndse unit number for error output.
  !> @param[in]  ftl  factor for tail description = xf2**n.
  !>
  !> @author h. l. tolman
  !> @date   01-nov-2012
  !>
  subroutine w3cspc ( sp1, nfr1, nth1, xf1, fr1, th1,             &
       sp2, nfr2, nth2, xf2, fr2, th2,             &
       nsp, ndst, ndse, ftl )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         01-nov-2012 !
    !/                  +-----------------------------------+
    !/
    !/    19-sep-2005 : origination.                        ( version 3.08 )
    !/    01-nov-2012 : code clean up (tab spaces, comments)( version 4.08 )
    !/
    !  1. purpose :
    !
    !     convert a set of spectra to a new spectral grid.
    !
    !  2. method :
    !
    !     conservative distribution of input energies over new grid.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       sp1     r.a.   i   input spectra.
    !       nfr1    int.   i   input number of frequencies.
    !       nth1    int.   i   input number of directions.
    !       xfr     real   i   input frequency increment factor.
    !       fr1     real   i   first input frequency.
    !       th1     real   i   first input direction.
    !       sp2     r.a.   o   output spectra.
    !       nfr2, nth2, xf2, fr2, th2
    !                      !   specral description for output spectra.
    !       nsp     int.   i   number of spectra.
    !       ndst    int.   i   unit number for test output.
    !       ndse    int.   i   unit number for error output.
    !       ftail   real   i   factor for tail description = xf2**n
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    sur.  w3servmd subroutine tracing.
    !      extcde    sur.    id     program abort.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3iobc    subr. w3iobcmd updating boundary conditions.
    !                subr           multi scale model bound. data input.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     - check on input parameters.
    !
    !  7. remarks :
    !
    !     - the inner loop of the actual redistribution is over the
    !       individual spectra, optimizing this routine for large numbers
    !       of conversions in a single call.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s    enable subroutine tracing.
    !
    !     !/t    enable test output.
    !     !/t1   test output for searching in stored data.
    !     !/t2   test output for redistribution data.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    !
    use w3servmd, only: extcde
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: nsp, nfr1, nth1, nfr2, nth2, ndst, ndse
    real, intent(in)        :: sp1(nth1,nfr1,nsp), xf1, fr1, th1,   &
         xf2, fr2, th2, ftl
    real, intent(out)       :: sp2(nth2,nfr2,nsp)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: i, nrmax, j, i1, l1, j1, i2, l2, j2, &
         isp
    real                    :: low, hgh, rlow, rhgh, blow, bhgh,    &
         frac, aux1, aux2, r1, r2, fact
    logical                 :: found
    type(case), pointer     :: current
    !/
    !/ ------------------------------------------------------------------- /
    !/ pointers for aliases
    !/
    integer, pointer        :: idth(:,:), idfr(:,:), nfr2t
    real, pointer           :: dth1, dth2, rdth(:,:), frq1(:),      &
         frq2(:), xdf1, xdf2, rdfr(:,:)
    !/
    !
    ! -------------------------------------------------------------------- /
    ! 0.  initializations
    ! 0.a check input
    !
    if ( nfr1.lt.3 .or. nth1.lt.4 .or. xf1.le.1. .or. fr1.le.0. .or.&
         nfr2.lt.3 .or. nth2.lt.4 .or. xf2.le.1. .or. fr2.le.0. ) then
      write (ndse,900) nfr1, nth1, xf1, fr1, nfr2, nth2, xf2, fr2
      call extcde ( 1 )
    end if
    !
    if ( nsp .lt. 0 ) then
      write (ndse,901)
      call extcde ( 2 )
    end if
    !
    if ( nsp .eq. 0 ) then
      write (ndse,902)
      return
    end if
    !
    ! 0.b test output
    !
    !
    ! -------------------------------------------------------------------- /
    ! 1.  search stored interpolation data for match
    !
    found  = .false.
    !
    do i=1, ncases
      !
      if ( i .eq. 1 ) then
        current => idata
      else
        current => current%next
      end if
      !
      !
      found = current%nfr1.eq.nfr1 .and. current%nfr2.eq.nfr2 .and. &
           current%nth1.eq.nth1 .and. current%nth2.eq.nth2 .and. &
           current%xf1 .eq.xf1  .and. current%xf2 .eq.xf2  .and. &
           current%fr1 .eq.fr1  .and. current%fr2 .eq.fr2  .and. &
           current%th1 .eq.th1  .and. current%th2 .eq.th2
      if ( found ) exit
      !
    end do
    !
    ! -------------------------------------------------------------------- /
    ! 2.  link or compute interpolation data
    ! 2.a link
    !
    if ( found ) then
      !
      !
      dth1   => current%dth1
      dth2   => current%dth2
      idth   => current%idth
      rdth   => current%rdth
      !
      frq1   => current%frq1
      frq2   => current%frq2
      xdf1   => current%xdf1
      xdf2   => current%xdf2
      nfr2t  => current%nfr2t
      idfr   => current%idfr
      rdfr   => current%rdfr
      !
      ! 2.b compute
      !
    else
      !
      ncases = ncases + 1
      !
      ! 2.b.1 point and allocate as necessary
      !
      if ( ncases .eq. 1 ) then
        allocate ( idata )
        current => idata
      else
        allocate ( current%next )
        current => current%next
      end if
      !
      ! 2.b.2 store test data
      !
      current%icase = ncases
      current%nfr1  = nfr1
      current%nth1  = nth1
      current%xf1   = xf1
      current%fr1   = fr1
      current%th1   = th1
      current%nfr2  = nfr2
      current%nth2  = nth2
      current%xf2   = xf2
      current%fr2   = fr2
      current%th2   = th2
      !
      ! 2.b.3 directional redistribution data
      !
      dth1   => current%dth1
      dth1   = tpi / real(nth1)
      dth2   => current%dth2
      dth2   = tpi / real(nth2)
      !
      if ( dth1 .le. dth2 ) then
        nrmax  = 2
      else
        nrmax  = 2 + int(dth1/dth2)
      end if
      !
      allocate (current%idth(0:nrmax,nth1),current%rdth(nrmax,nth1))
      idth   => current%idth
      rdth   => current%rdth
      idth   = 0
      rdth   = 0.
      !
      do i=1, nth1
        low    = th1 + real(i-1)*dth1 - 0.5*dth1
        hgh    = low + dth1
        rlow   = 1. + (low-th2)/dth2
        rhgh   = 1. + (hgh-th2)/dth2
        do j=nint(rlow), nint(rlow)+nrmax-1
          blow   = th2 + real(j-1)*dth2 - 0.5*dth2
          bhgh   = blow + dth2
          frac   = (min(bhgh,hgh)-max(blow,low)) / (hgh-low)
          if ( frac .gt. 1.e-5 ) then
            idth(0,i) = idth(0,i) + 1
            idth(idth(0,i),i) = 1 + mod(j-1+nth2,nth2)
            rdth(idth(0,i),i) = frac
          end if
        end do
      end do
      !
      ! 2.b.4 frequency redistribution data
      !
      allocate ( current%frq1(nfr1), current%frq2(nfr2) )
      frq1   => current%frq1
      frq2   => current%frq2
      !
      frq1(1) = fr1
      do i=2, nfr1
        frq1(i) = xf1 * frq1(i-1)
      end do
      !
      frq2(1) = fr2
      do i=2, nfr2
        frq2(i) = xf2 * frq2(i-1)
      end do
      !
      xdf1   => current%xdf1
      xdf1   = 0.5 * ( xf1 - 1./xf1 )
      xdf2   => current%xdf2
      xdf2   = 0.5 * ( xf2 - 1./xf2 )
      !
      if ( xdf1 .le. xdf2 ) then
        nrmax  = 2
      else
        nrmax  = 1
        aux1   = xdf1
        aux2   = xdf2
        do
          nrmax  = nrmax + 1
          aux1   = aux1 - aux2
          aux2   = aux2 / xf2
          if ( aux1 .lt. 0. ) exit
        end do
      end if
      !
      allocate (current%idfr(0:nrmax,nfr1),current%rdfr(nrmax,nfr1))
      idfr   => current%idfr
      rdfr   => current%rdfr
      idfr   = 0
      rdfr   = 0.
      !
      do i=1, nfr1
        if ( i .eq. 1 ) then
          hgh    = 0.5 * ( frq1(i) + frq1(i+1) )
          low    = hgh - xdf1*frq1(i)
        else
          low    = 0.5 * ( frq1(i) + frq1(i-1) )
          hgh    = low + xdf1*frq1(i)
        end if
        do j=1, nfr2
          if ( j .eq. 1 ) then
            bhgh   = 0.5 * ( frq2(j) + frq2(j+1) )
            blow   = bhgh - xdf2*frq2(j)
          else
            blow   = 0.5 * ( frq2(j) + frq2(j-1) )
            bhgh   = blow + xdf2*frq2(j)
          end if
          if ( bhgh .le. low ) cycle
          if ( blow .ge. hgh ) exit
          frac   = (min(bhgh,hgh)-max(blow,low)) / (hgh-low)
          if ( frac .lt. 1.e-5 ) cycle
          idfr(0,i) = idfr(0,i) + 1
          idfr(idfr(0,i),i) = j
          rdfr(idfr(0,i),i) = frac
        end do
      end do
      !
      nfr2t  => current%nfr2t
      nfr2t  = nfr2 + 1
      do j=nfr2, 1, -1
        if ( j .eq. 1 ) then
          bhgh   = 0.5 * ( frq2(j) + frq2(j+1) )
        else
          blow   = 0.5 * ( frq2(j) + frq2(j-1) )
          bhgh   = blow + xdf2*frq2(j)
        end if
        if ( bhgh .gt. hgh ) then
          nfr2t  = j
        else
          exit
        end if
      end do
      !
    end if
    !
    ! 2.c test output
    !
    !
    ! -------------------------------------------------------------------- /
    ! 3.  convert
    ! 3.a discrete energies
    !
    !
    sp2    = 0.
    !
    do i2=1, nfr1
      do l2=1, idfr(0,i2)
        j2   = idfr(l2,i2)
        r2   = rdfr(l2,i2)
        do i1=1,nth1
          do l1=1, idth( 0,i1)
            j1   = idth(l1,i1)
            r1   = rdth(l1,i1)
            frac   = r2 * frq1(i2) * xdf1 * r1 * dth1
            sp2(j1,j2,:) = sp2(j1,j2,:) + frac * sp1(i1,i2,:)
          end do
        end do
      end do
    end do
    !
    ! 3.b energy densities
    !
    !
    do j2=1, nfr2
      do j1=1, nth2
        fact   = 1. / ( frq2(j2) * xdf2 * dth2 )
        sp2(j1,j2,:) = fact * sp2(j1,j2,:)
      end do
    end do
    !
    ! 3.c add the tail
    !
    !
    do j2=nfr2t, nfr2
      sp2(:,j2,:) = ftl * sp2(:,j2-1,:)
    end do
    !
    return
    !
    ! formats
    !
900 format (/' *** error w3cspc: illegal input parameters ***'/     &
         '                   input  : ',2i8,2f10.4/             &
         '                   output : ',2i8,2f10.4)
901 format (/' *** error w3cspc: negative number of spectra ***'/)
902 format (/' *** warning w3cspc: no spectra ***'/)
    !
    !
    !
    !
    !/
    !/ end of w3cspc ----------------------------------------------------- /
    !/
  end subroutine w3cspc
  !/
  !/ end of module w3cspcmd -------------------------------------------- /
  !/
end module w3cspcmd
