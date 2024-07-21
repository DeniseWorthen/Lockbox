!> @file
!> @brief contains module w3str1md.
!>
!> @author a. j. van der westhuysen @date 13-jan-2013
!> @author a. roland @date 22-feb-2023
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
!> @brief module for inclusion of triad nonlinear interaction
!>  according to eldeberky's (1996) lumped triad interaction (lta)
!>  source term.
!>
!> @author a. j. van der westhuysen @date 13-jan-2013
!>
module w3str1md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |     a. j. van der westhuysen      |
  !/                  |                        fortran 90 |
  !/                  | last update :         13-jan-2013 |
  !/                  +-----------------------------------+
  !/
  !/    13 jan-2013 : origination, based on swan v40.91 code ( version 4.08 )
  !/
  !/    copyright 2009 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     module for inclusion of triad nonlinear interaction according to
  !     eldeberky's (1996) lumped triad interaction (lta) source term.
  !
  !  2. variables and types :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !     ----------------------------------------------------------------
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3str1    subr. public   user supplied triad interactions.
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
  !     wavewatch iii is designed as a highly plug-compatible code.
  !     source term modules can be included as self-contained modules,
  !     with limited changes needed to the interface of routine calls
  !     in w3srce, and in the point postprocessing programs only.
  !     codes submitted for inclusion in wavewatch iii should be
  !     self-contained in the way described below, and might be
  !     provided with distributions fully integrated in the data
  !     structure, or as an optional version of this module to be
  !     included by the user.
  !
  !     rules for preparing a module to be included in or distributed
  !     with wavewatch iii :
  !
  !      - fully document the code following the outline given in this
  !        file, and according to all other wavewatch iii routines.
  !      - provide a file with necessary modifications to w3srce and
  !        all other routines that require modification.
  !      - provide a test case with expected results.
  !      - it is strongly recommended that the programming style used
  !        in wavewatch iii is followed, in particular
  !          a) for readability, write as if in fixed fortran format
  !             regarding column use, even though all files are f90
  !             free format.
  !          b) i prefer upper case programming for permanent code,
  !             as i use lower case in debugging and temporary code.
  !
  !     this module needs to be self-contained in the following way.
  !
  !      a) all saved variables connected with this source term need
  !         to be declared in the module header. upon acceptance as
  !         permanent code, they will be converted to the wavewatch iii
  !         dynamic data structure.
  !      b) provide a separate computation and initialization routine.
  !         in the submission, the initialization should be called
  !         from the computation routine upon the first call to the
  !         routine. upon acceptance as permanent code, the
  !         initialization routine will be moved to a more appropriate
  !         location in the code (i.e., being absorbed in ww3_grid or
  !         being moved to w3iogr).
  !
  !     see notes in the file below where to add these elements.
  !
  !  6. switches :
  !
  !     !/s  enable subroutine tracing.
  !
  !  7. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  !     *****************************************
  !     ***    declare saved variables here   ***
  !     ***  public or private as appropriate ***
  !     *****************************************
  !
  public
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief triad interaction source term computed using the lumped
  !>  triad appproximation (lta) of eldeberky (1996).
  !>
  !> @verbatim
  !>     the parametrized biphase is given by:
  !>
  !>                                  0.2
  !>     beta = - pi/2 + pi/2 tanh ( ----- )
  !>                                   ur
  !>
  !>     where ur is the ursell number.
  !>
  !>     the source term as function of frequency p is:
  !>
  !>             +      -
  !>     s(p) = s(p) + s(p)
  !>
  !>     in which
  !>
  !>      +
  !>     s(p) = alpha cp cg,p (r(p/2,p/2))**2 sin (|beta|) ( e(p/2)**2 -2 e(p) e(p/2) )
  !>
  !>      -          +
  !>     s(p) = - 2 s(2p)
  !>
  !>     with alpha a tunable coefficient and r(p/2,p/2) is the interaction
  !>     coefficient of which the expression can be found in eldeberky (1996).
  !>
  !>     note that a slightly adapted formulation of the lta is used in
  !>     in the swan model:
  !>
  !>     - only positive contributions to higher harmonics are considered
  !>       here (no energy is transferred to lower harmonics).
  !>
  !>     - the mean frequency in the expression of the ursell number
  !>       is calculated according to the first order moment over the
  !>       zeroth order moment (personal communication, y.eldeberky, 1997).
  !>
  !>     - the interactions are calculated up to 2.5 times the mean
  !>       frequency only.
  !>
  !>     - since the spectral grid is logarithmically distributed in frequency
  !>       space, the interactions between central bin and interacting bin
  !>       are interpolated such that the distance between these bins is
  !>       factor 2 (nearly).
  !>
  !>     - the interactions are calculated in terms of energy density
  !>       instead of action density. so the action density spectrum
  !>       is firstly converted to the energy density grid, then the
  !>       interactions are calculated and then the spectrum is converted
  !>       to the action density spectrum back.
  !> @endverbatim
  !>
  !> @param[in] a action density spectrum (1-d)
  !> @param[in] cg group velocities.
  !> @param[in] wn wavenumbers.
  !> @param[in] depth mean water depth.
  !> @param[in] ix
  !> @param[out] s source term (1-d version).
  !> @param[out] d diagonal term of derivative (1-d version).
  !>
  !> @author a. j. van der westhuysen  @date 13-jan-2013
  !>
  subroutine w3str1 (a, aold, cg, wn, depth, ix, s, d)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |     a. j. van der westhuysen      |
    !/                  |     a. roland                     |
    !/                  |                        fortran 90 |
    !/                  | last update :         13-jan-2013 |
    !/                  +-----------------------------------+
    !/
    !/    13 jan-2013 : origination, based on swan v40.91 code ( version 4.08 )
    !/    05 oct-2016 : avoiding divide by zero for emean      ( version 5.15 )
    !/    28 feb-2023 : improvement of efficiency and stability ( version 7.xx)
    !/
    !  1. purpose :
    !
    !     triad interaction source term computed using the lumped triad
    !     appproximation (lta) of eldeberky (1996).
    !
    !  2. method :
    !
    !     (taken from swan v40.91, based on code by marcel zijlema, tu delft)
    !
    !     the parametrized biphase is given by:
    !
    !                                  0.2
    !     beta = - pi/2 + pi/2 tanh ( ----- )
    !                                   ur
    !
    !     where ur is the ursell number.
    !
    !     the source term as function of frequency p is:
    !
    !             +      -
    !     s(p) = s(p) + s(p)
    !
    !     in which
    !
    !      +
    !     s(p) = alpha cp cg,p (r(p/2,p/2))**2 sin (|beta|) ( e(p/2)**2 -2 e(p) e(p/2) )
    !
    !      -          +
    !     s(p) = - 2 s(2p)
    !
    !     with alpha a tunable coefficient and r(p/2,p/2) is the interaction
    !     coefficient of which the expression can be found in eldeberky (1996).
    !
    !     note that a slightly adapted formulation of the lta is used in
    !     in the swan model:
    !
    !     - only positive contributions to higher harmonics are considered
    !       here (no energy is transferred to lower harmonics).
    !
    !     - the mean frequency in the expression of the ursell number
    !       is calculated according to the first order moment over the
    !       zeroth order moment (personal communication, y.eldeberky, 1997).
    !
    !     - the interactions are calculated up to 2.5 times the mean
    !       frequency only.
    !
    !     - since the spectral grid is logarithmically distributed in frequency
    !       space, the interactions between central bin and interacting bin
    !       are interpolated such that the distance between these bins is
    !       factor 2 (nearly).
    !
    !     - the interactions are calculated in terms of energy density
    !       instead of action density. so the action density spectrum
    !       is firstly converted to the energy density grid, then the
    !       interactions are calculated and then the spectrum is converted
    !       to the action density spectrum back.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a       r.a.  i   action density spectrum (1-d)
    !       cg      r.a.  i   group velocities.
    !       wn      r.a.  i   wavenumbers.
    !       depth   real  i   mean water depth.
    !       emean   real  i   mean wave energy.
    !       fmean   real  i   mean wave frequency.
    !       s       r.a.  o   source term (1-d version).
    !       d       r.a.  o   diagonal term of derivative (1-d version).
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
    !  8. structure :
    !
    !     determine resonance condition and the maximum discrete freq.
    !     for which the interactions are calculated.
    !
    !     if ursell number larger than prescribed value compute interactions
    !        calculate biphase
    !        do for each direction
    !           convert action density to energy density
    !           do for all frequencies
    !             calculate interaction coefficient and interaction factor
    !             compute interactions and store results in matrix
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants, only: grav, pi, tpi
    use w3gdatmd, only: nk, nth, nspec, dth, sig, dden, fte, ftf
    use w3odatmd, only: ndse
    use w3servmd, only: extcde
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: cg(nk), wn(nk), depth, a(nspec), aold(nspec) 
    integer, intent(in)     :: ix
    real, intent(out)       :: s(nspec), d(nspec)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !     aux1  :     auxiliary real
    !     aux2  :     auxiliary real
    !     biph  :     parameterized biphase of the spectrum
    !     c0    :     phase velocity at central bin
    !     cm    :     phase velocity at interacting bin
    !     dep   :     water depth
    !     dep_2 :     water depth to power 2
    !     dep_3 :     water depth to power 3
    !     e     :     energy density as function of frequency
    !     e0    :     energy density at central bin
    !     em    :     energy density at interacting bin
    !     hs    :     significant wave height
    !     ft    :     auxiliary real indicating multiplication factor
    !                 for triad contribution
    !     i1    :     auxiliary integer
    !     i2    :     auxiliary integer
    !     id    :     counter
    !     iddum :     loop counter in direction space
    !     ient  :     number of entries
    !     ii    :     loop counter
    !     is    :     loop counter in frequency space
    !     ism   :     negative range for is
    !     ism1  :     negative range for is
    !     ismax :     maximum of the counter in frequency space for
    !                 which the triad interactions are calculated (cut-off)
    !     isp   :     positive range for is
    !     isp1  :     positive range for is
    !     rint  :     interaction coefficient
    !     sa    :     interaction contribution of triad
    !     sigpicg :   sigma times 2pi/cg (a jacobian for e(f) -> e(k))
    !     sinbph:     absolute sine of biphase
    !     stri  :     total triad contribution
    !     wism  :     interpolation weight factor corresponding to lower harmonic
    !     wism1 :     interpolation weight factor corresponding to lower harmonic
    !     wisp  :     interpolation weight factor corresponding to higher harmonic
    !     wisp1 :     interpolation weight factor corresponding to higher harmonic
    !     w0    :     radian frequency of central bin
    !     wm    :     radian frequency of interacting bin
    !     wn0   :     wave number at central bin
    !     wnm   :     wave number at interacting bin
    !     xis   :     rate between two succeeding frequency counters
    !     xisln :     log of xis
    !
    integer :: i1, i2, id, iddum, ii, is, ism, ism1, ismax
    integer :: isp, isp1, ith, ik
    real    :: aux1, aux2, biph, c0, cm, dep, dep_2, dep_3, e0, em, hs
    real    :: ft, rint, sigpicg, sinbph, stri, wism, wism1, wisp
    real    :: wisp1, w0, wm, wn0, wnm, xis, xisln, edm, ed0, g9dep, stri2
    real    :: e(nk), sa(nth,100), sa2(nth,100), a2(nspec), a3(nspec), hmax
    real    :: eb(nk), eband, emean, sigm01, ed(nk)
!----- temp (to be moved) -----
    real    :: ef(nk), jaceps, diffstr
    real    :: ptriad(5)
    real    :: ursell, alphar
!------------------------------
!/
!/ ------------------------------------------------------------------- /
!/
!ar: todo: check all prx routines for differences, check original thesis of elderberky. 
!
! 1.  integral over directions
!
    sigm01 = 0.
    emean  = 0.
    jaceps = 1e-12
    hmax   = depth * 0.73
    do ik=1, nk
      eb(ik) = 0.
      ed(ik) = 0.
      do ith=1, nth
        eb(ik) = eb(ik) + a(ith+(ik-1)*nth)
        ed(ik) = ed(ik) + a(ith+(ik-1)*nth) * dden(ik) / cg(ik)
      end do
    end do
!
! 2.  integrate over frequencies. 
!
    do ik=1, nk
      eb(ik) = eb(ik) * dden(ik) / cg(ik)
      emean  = emean  + eb(ik)
      sigm01  = sigm01  + eb(ik)*sig(ik)
    end do
!
! 3.  add tail beyond discrete spectrum
!     ( dth * sig(nk) absorbed in ftxx )
!
    eband  = eb(nk) / dden(nk)
    emean  = emean  + eband * fte
    sigm01 = sigm01 + eband * ftf
!
! 4.  final processing
!
    sigm01 = sigm01 / emean
!---- temporary parameters (to be replaced by namelists) -----
    ptriad(1) = 1. 
    ptriad(2) = 10.
    ptriad(3) = 10. ! not used 
    ptriad(4) = 0.2
    ptriad(5) = 0.01
    hs = 4.*sqrt( max(0.,emean) )
    ursell = (grav*hs)/(2.*sqrt(2.)*sigm01**2*depth**2)
!---------------------------------------------
    dep   = depth
    dep_2 = dep**2
    dep_3 = dep**3
    g9dep = grav * dep
!
!     --- compute some indices in sigma space
!
    i2     = int (float(nk) / 2.)
    i1     = i2 - 1
    xis    = sig(i2) / sig(i1)
    xisln  = log( xis )
    isp    = int( log(2.) / xisln )
    isp1   = isp + 1
    wisp   = (2. - xis**isp) / (xis**isp1 - xis**isp)
    wisp1  = 1. - wisp
    ism    = int( log(0.5) / xisln )
    ism1   = ism - 1
    wism   = (xis**ism -0.5) / (xis**ism - xis**ism1)
    wism1  = 1. - wism
    e  = 0.
    sa = 0.
!
!     --- compute maximum frequency for which interactions are calculated
!
    ismax = 1
    do ik = 1, nk
     if ( sig(ik) .lt. ( ptriad(2) * sigm01) ) then
        ismax = ik
     endif
    enddo
    ismax = max ( ismax , isp1 )
!
!     --- compute 3-wave interactions
!
      if (ursell.ge.ptriad(5) ) then ! ar: no need for switching it off from my point of view!
!
!       --- calculate biphase
!
        biph   = (0.5*pi)*(tanh(ptriad(4)/ursell)-1.)
        sinbph = abs(sin(biph) )
        ef     = 0.
        do ith = 1, nth
          do ik = 1, nk
            e(ik)  = a(ith+(ik-1)*nth) * tpi * sig(ik) / cg(ik)
            ef(ik) = ef(ik) + e(ik)        
          end do
          do ik = 1, ismax
            e0  = e(ik)
            ed0 = eb(ik) 
            w0  = sig(ik)
            wn0 = wn(ik)
            c0  = w0 / wn0
            if ( ik.gt.-ism1 ) then
               em  = wism * e(ik+ism1)   + wism1 * e(ik+ism)
               edm = wism * eb(ik+ism1)  + wism1 * eb(ik+ism)
               wm  = wism * sig(ik+ism1) + wism1 * sig(ik+ism)
               wnm = wism * wn(ik+ism1)  + wism1 * wn(ik+ism)
               cm  = wm / wnm
            else
               em  = 0.
               edm = 0.
               wm  = 0.
               wnm = 0.
               cm  = 0.
            end if
            aux1 = wnm**2 * ( g9dep + 2*cm**2 ) 
            aux2 = wn0*dep* (g9dep+(2./15.)*grav*dep_3*wn0**2-(2./5.)*w0**2*dep_2)
            rint = aux1 / aux2
            ft   = ptriad(1) * c0 * cg(ik) * rint**2 * sinbph 
            sa(ith,ik) = max(0.,ft * ( em * em - 2. * em * e0)) ! 1/(m²*s²) * m4 = m²/s² !!! [m²/s]
          end do
        end do
        do ik = 1, nk - 1 
          sigpicg = sig(ik)*tpi/cg(ik) ! 1/s * s/m = 1/m
          do ith = 1, nth
            stri = sa(ith,ik) - 2 * (wisp *  sa(ith,ik+isp1) + wisp1 *  sa(ith,ik+isp))
            if (a(ith+(ik-1)*nth) .gt. jaceps) then
              d(ith+(ik-1)*nth) = stri / ((a(ith+(ik-1)*nth)) * sigpicg) 
              s(ith+(ik-1)*nth) = stri / sigpicg 
            else
              d(ith+(ik-1)*nth) = 0.
              s(ith+(ik-1)*nth) = 0.
            endif
          end do
        end do
      else
        d = 0.
        s = 0.
      end if
    !/
    !/ end of w3str1 ----------------------------------------------------- /
    !/
  end subroutine w3str1
  !/ ------------------------------------------------------------------- /
end module w3str1md
