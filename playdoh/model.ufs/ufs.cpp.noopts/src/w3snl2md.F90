!> @file
!> @brief interface module to exact nonlinear interactions.
!>
!> @author h. l. tolman
!> @author g. ph. van vledder
!> @date   29-may-2009
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
!> @brief interface module to exact nonlinear interactions.
!>
!> @author h. l. tolman
!> @author g. ph. van vledder
!> @date   29-may-2009
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3snl2md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |        g. ph. van vledder         |
  !/                  |                        fortran 90 |
  !/                  | last update :         29-may-2009 |
  !/                  +-----------------------------------+
  !/
  !/    14-feb-2000 : origination.                        ( version 2.01 )
  !/    02-feb-2001 : exact-nl version 3.0                ( version 2.07 )
  !/    26-aug-2002 : exact-nl version 4.0                ( version 2.22 )
  !/    11-nov-2002 : interface fix.                      ( version 3.00 )
  !/    25-sep-2003 : exact-nl version 5.0                ( version 3.05 )
  !/    24-dec-2004 : multiple model version.             ( version 3.06 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/
  !/    copyright 2009 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     interface module to exact nonlinear interactions.
  !
  !  2. variables and types :
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3snl2    subr. public   interface to xnl calculation routines.
  !      insnl2    subr. public   initialization of xnl routines.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !     see subroutine.
  !
  !  5. remarks :
  !
  !  6. switches :
  !
  !       !/s   enable subroutine tracing.
  !       !/t   enable general test output.
  !       !/t0  2-d print plot of source term.
  !       !/t1  print arrays.
  !
  !  7. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  public
  !/
contains
!/ ------------------------------------------------------------------- /
!>
!> @brief interface to exact interactions.
!>
!> @param[in] a      action spectrum a(ith,ik) as a function of
!>                   direction (rad)  and wavenumber.
!> @param[in] cg     group velocities (dimension nk).
!> @param[in] depth  water depth in meters.
!> @param[out] s     source term.
!> @param[out] d     diagonal term of derivative.
!>
!> @author h. l. tolman
!> @author g. ph. van vledder
!> @date   24-dec-2004
!>
  subroutine w3snl2 (  a, cg, depth, s, d )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |        g. ph. van vledder         |
    !/                  |                        fortran 90 |
    !/                  | last update :         24-dec-2004 |
    !/                  +-----------------------------------+
    !/
    !/    14-feb-2000 : origination                         ( version 2.01 )
    !/    02-feb-2001 : exact-nl version 3.0                ( version 2.07 )
    !/    26-aug-2002 : exact-nl version 4.0                ( version 2.22 )
    !/    11-nov-2002 : interface fix                       ( version 3.00 )
    !/    25-sep-2003 : exact-nl version 5.0                ( version 3.05 )
    !/    24-dec-2004 : multiple model version.             ( version 3.06 )
    !/
    !  1. purpose :
    !
    !     interface to exact interactions
    !
    !  2. method :
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a       r.a.  i   action spectrum a(ith,ik) as a function of
    !                         direction (rad)  and wavenumber.
    !       cg      r.a.  i   group velocities (dimension nk).
    !       depth   real  i   water depth in meters.
    !       s       r.a.  o   source term.
    !       d       r.a.  o   diagonal term of derivative.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module     description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd   subroutine tracing.
    !      xnl_main  subr. m_xnldata  main xnl routine.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3srce    subr. w3srcemd source term integration.
    !      w3expo    subr.   n/a    point output post-processor.
    !      gxexpo    subr.   n/a    grads point output post-processor.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !       none.
    !
    !  7. remarks :
    !
    !     - the following settings are hardwired into the xnl_init routine
    !       of gerbrant van vledder.
    !
    !         iufind = 0
    !         iq_prt = 0
    !         iq_test = 0
    !         iq_trace = 0
    !         iq_log = 0
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !       !/s   enable subroutine tracing.
    !       !/t   enable general test output.
    !       !/t0  2-d print plot of source term.
    !       !/t1  print arrays.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use constants
    use w3gdatmd, only: nk, nth, sig, th, iqtpe
    use w3odatmd, only: ndse, ndst, iaproc, naperr
    use w3servmd, only: extcde
    use m_xnldata, only: xnl_main
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: a(nth,nk), cg(nk), depth
    real, intent(out)       :: s(nth,nk), d(nth,nk)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ik, ith, ierr = 0
    real                    :: a2(nk,nth), s2(nk,nth), d2(nk,nth)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  convert input spectrum ----------------------------------------- *
    !     (action sigma spectrum, reversed indices)
    !
    do ik=1, nk
      do ith=1, nth
        a2(ik,ith) = a(ith,ik) / cg(ik)
      end do
    end do
    !
    ! 2.  call exact interaction routines -------------------------------- *
    !
    call xnl_main ( a2, sig(1:nk), th, nk, nth, depth, iqtpe,       &
         s2, d2, iaproc, ierr )
    !
    if ( ierr .ne. 0 ) goto 800
    !
    ! 3.  pack results in proper format ---------------------------------- *
    !
    do ik=1, nk
      do ith=1, nth
        s(ith,ik) = s2(ik,ith) * cg(ik)
        d(ith,ik) = d2(ik,ith)
      end do
    end do
    !
    ! ... test output :
    !
    !
    !
    return
    !
    !     error escape locations
    !
800 continue
    if ( iaproc .eq. naperr ) write (ndse,1000) ierr
    call extcde ( 1 )
    !
    ! format statements
    !
1000 format (/' *** wavewatch iii error in w3snl2 :'/                &
         '     xnl_main return code non zero : ',i4,' ***'/)
    !
    !/
    !/ end of w3snl2 ----------------------------------------------------- /
    !/
  end subroutine w3snl2
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief preprocessing for nonlinear interactions (xnl).
  !>
  !> @author h. l. tolman
  !> @author g. ph. van vledder
  !> @date   24-dec-2004
  !>
  subroutine insnl2
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |        g. ph. van vledder         |
    !/                  |                        fortran 90 |
    !/                  | last update :         24-dec-2004 |
    !/                  +-----------------------------------+
    !/
    !/    02-feb-2001 : origination.                        ( version 2.07 )
    !/    25-sep-2003 : exact-nl version 5.0                ( version 3.05 )
    !/    24-dec-2004 : multiple model version.             ( version 3.06 )
    !/
    !  1. purpose :
    !
    !     preprocessing for nonlinear interactions (xnl).
    !
    !  2. method :
    !
    !     see xnl documentation.
    !
    !  3. parameters :
    !
    !  4. subroutines used :
    !
    !      name      type  module      description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd    subroutine tracing.
    !      init_constants
    !                subr. m_xnldata   xnl initialization routine.
    !      xnl_init  subr. m_constants xnl initialization routine.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3iogr    subr. w3iogrmd model definition file management.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     - see source code.
    !
    !  9. switches :
    !
    !       !/s      enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    use w3gdatmd, only: nk, nth, sig, th,                           &
         nltail, dpthnl, ndpths, iqtpe
    use w3odatmd, only: ndse, ndst, iaproc, naperr
    use w3servmd, only: extcde
    use m_xnldata
    use m_constants, only: init_constants
    !/
    implicit none
    !/
    !/ local parameters
    !/
    integer                 :: igrd, ierr
    real                    :: xgrav
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  set necessary values : ----------------------------------------- *
    !
    xgrav  = grav
    igrd   = 3
    !
    !
    ! 2.  call initialization routines : --------------------------------- *
    !
    call init_constants
    !
    call xnl_init ( sig(1:nk), th, nk, nth, nltail, xgrav,          &
         dpthnl, ndpths, iqtpe, igrd, iaproc, ierr )
    !
    if ( ierr .ne. 0 ) goto 800
    !
    return
    !
    !     error escape locations
    !
800 continue
    if ( iaproc .eq. naperr ) write (ndse,1000) ierr
    call extcde ( 1 )
    !
    !     format statements
    !
1000 format (/' *** wavewatch iii error in insnl2 :'/                &
         '     xnl_init return code non zero : ',i8/)
    !
    !/
    !/ end of insnl2 ----------------------------------------------------- /
    !/
  end subroutine insnl2
  !/
  !/ end of module w3snl2md -------------------------------------------- /
  !/
end module w3snl2md
