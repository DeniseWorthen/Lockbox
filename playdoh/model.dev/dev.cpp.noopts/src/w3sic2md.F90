!> @file
!> @brief calculate ice dissipation source term s_{ice}.
!>
!> @author e. rogers
!> @author s. zieger
!> @author f. ardhuin
!> @author g. boutin
!> @date   05-jan-2018
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
!> @brief calculate ice dissipation source term s_{ice}.
!>
!> @author e. rogers
!> @author s. zieger
!> @author f. ardhuin
!> @author g. boutin
!> @date   05-jan-2018
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3sic2md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           e. rogers               |
  !/                  |           s. zieger               |
  !/                  |     f. ardhuin & g. boutin        |
  !/                  |                        fortran 90 |
  !/                  | last update :         05-jan-2018 |
  !/                  +-----------------------------------+
  !/
  !/    10-mar-2014 : generalization with turbulent bl    ( version 5.01 )
  !/    05-jan-2018 : addition of floe size effect        ( version 6.04 )
  !/
  !/    for updates see w3sic1 documentation.
  !/
  !  1. purpose :
  !
  !     calculate ice dissipation source term s_{ice}.
  !          exponential decay rate according to liu et al., which
  !          uses as input: 1) ice thickness, and 2) an eddy
  !          viscosity parameter. this method is non-uniform in
  !          frequency. this is discussed further below, in
  !          subroutine "liu_reverse_dispersion".
  !
  !          includes generalization by f. ardhuin with viscous and tubulent
  !          boundary layers. that part is activating by setting namelist
  !          parameters that define the under-ice roughness and a friction
  !          coefficient. for example: &ic2 ic2turb = 1. , ic2rough =0.0001
  !
  !        references for subtype 2:
  !              liu et al.    1991: jgr 96 (c3), 4605-4621
  !              liu and mollo 1988: jpo 18       1720-1712
  !              stopa et al.  2016: the cryosphere
  !
  !  2. variables and types :
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3sic2                 subr. public   ice source term.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !     see subroutine documentation.
  !
  !  5. remarks :
  !
  !     reference:rogers, w.e. and m.d. orzech, 2013: implementation and
  !        testing of ice and mud source functions in wavewatch iii(r),
  !        nrl/mr/7320--13-9462, 31pp.
  !        available from http://www7320.nrlssc.navy.mil/pubs.php
  !        direct link:
  !        http://www7320.nrlssc.navy.mil/pubs/2013/rogers2-2013.pdf
  !
  !  6. switches :
  !
  !     see subroutine documentation.
  !
  !  7. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  public  :: w3sic2
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief s_{ice} source term using 5 parameters read from input files.
  !>
  !> @param[in]  a      action density spectrum (1-d).
  !> @param[in]  depth  local water depth.
  !> @param[in]  iceh   ice thickness.
  !> @param[in]  icef   ice floe diameter.
  !> @param[in]  cg     group velocities.
  !> @param[in]  wn     wavenumbers.
  !> @param[in]  ix     grid index.
  !> @param[in]  iy     grid index.
  !> @param[out] s      source term (1-d version).
  !> @param[out] d      diagonal term of derivative (1-d version).
  !> @param[in]  wn_r   wavenumbers in ice.
  !> @param[in]  cg_ice group velocities in ice.
  !> @param[in]  alpha  exponential decay rate of energy.
  !> @param[in]  r      ratio of energy to wave energy without ice.
  !>
  !> @author e. rogers
  !> @author s. zieger
  !> @author f. ardhuin
  !> @author g. boutin
  !> @date   04-jan-2018
  !>
  subroutine w3sic2 (a, depth, iceh, icef, cg, wn, ix, iy, s, d, wn_r, &
       cg_ice, alpha, r)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           e. rogers               |
    !/                  |           s. zieger               |
    !/                  |    f. ardhuin & g. boutin         |
    !/                  |                        fortran 90 |
    !/                  | last update :         04-jan-2018 |
    !/                  +-----------------------------------+
    !/
    !/    16-oct-2012 : origination.                        ( version 4.04 )
    !/                                                        (e. rogers)
    !/    09-oct-2013 : w3sic1 subtype=2 outsourced to w3sic2 (s. zieger)
    !/    10-mar-2014 : generalization with turbulent bl    ( version 5.01 )
    !/    16-feb-2016 : passes iceh as parameter            ( version 5.10 )
    !/    02-may-2016 : call to liu disp moved to w3srce    ( version 5.10 )
    !/    04-jan-2018 : includes floe size dependance       ( version 6.02 )
    !/        fixme   : move field input to w3srce and provide
    !/     (s.zieger)   input parameter to w3sic1 to make the subroutine
    !/                : versatile for point output processors ww3_outp
    !/                  and ww3_ounp.
    !/
    !/    copyright 2009 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    !  1. purpose :
    !
    !     s_{ice} source term using 5 parameters read from input files.
    !     these parameters are allowed to vary in space and time.
    !     the parameters control the exponential decay rate k_i
    !     since there are 5 parameters, this permits description of
    !     dependence of k_i on frequency or wavenumber.
    !
    !/ ------------------------------------------------------------------- /
    !
    !  2. method :
    !
    !     regarding i/o (general to all sice modules): s_{ice} source term
    !     is calculated using up to 5 parameters read from input files.
    !     these parameters are allowed to vary in space and time.
    !     the parameters control the exponential decay rate k_i
    !     since there are 5 parameters, this permits description of
    !     dependence of k_i on frequency or wavenumber.
    !
    !     sea ice affects the wavenumber k of wind-generated ocean waves.
    !     the ice-modified wavenumber can be expressed as a complex number
    !     k = k_r + i*k_i, with the real part k_r representing impact of
    !     the sea ice on the physical wavelength and propagation speeds,
    !     producing something analogous to shoaling and refraction by
    !     bathymetry, whereas the imaginary part of the complex
    !     wavenumber, k_i, is an exponential decay coefficient
    !     k_i(x,y,t,sigma) (depending on location, time and frequency,
    !     respectively), representing wave attenuation, and can be
    !     introduced in a wave model such as ww3 as s_ice/e=-2*cg*k_i,
    !     where s_ice is one of several dissipation mechanisms, along
    !     with whitecapping, for example, s_ds=s_wc+s_ice+⋯. the k_r -
    !     modified by ice would enter the model via the c calculations
    !     on the left-hand side of the governing equation.the fundamentals
    !     are straightforward, e.g. rogers and holland (2009 and
    !     subsequent unpublished work) modified a similar model, swan
    !     (booij et al. 1999) to include the effects of a viscous mud
    !     layer using the same approach (k = k_r + i*k_i) previously.
    !
    !     general approach is analogous to rogers and holland (2009)
    !         approach for mud.
    !     see text near their eq. 1 :
    !       k        = k_r  +  i * k_i
    !       eta(x,t) = real( a * exp( i * ( k * x - sigma * t ) ) )
    !       a        = a0 * exp( -k_i * x )
    !       s / e    = -2 * cg * k_i (see also komen et al. (1994, pg. 170)
    !
    !     please note that s is source term for action.
    !
    !     notes regarding numerics:
    !     (note by f. ardhuin: these may not apply in version 5 thanks to splitting
    !                          of ice source terms and implicit integration in w3srce)
    !     experiments with constant k_i values suggest that :
    !       for dx=20.0 km, k_i should not exceed 3.5e-6
    !      (assumes 2.7% hs error in my particular test case is intolerable)
    !       for dx=5.0 km,  k_i should not exceed 2.0e-5
    !       for dx=2.5 km,  k_i should not exceed 5.0e-5
    !       for dx=1.0 km,  k_i should not exceed 2.0e-4
    !       for dx=0.35 km, error is less than 2.1% for all k_i tested
    !       for dx=0.10 km, error is less than 1.3% for all k_i tested
    !     "ground truth" used for this is an exponential decay profile.
    !
    !      for reference, acnfs is 1/12th deg, so delta_latitude=9.25 km.
    !
    !     {put more equations here}
    !
    !     the laminar to turbulent transition is described in
    !         stopa et al. (the cryosphere, 2016).
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a       r.a.   i   action density spectrum (1-d)
    !       depth   real   i   local water depth
    !       iceh    real   i   ice thickness
    !       cg      r.a.   i   group velocities
    !       wn      r.a.   i   wavenumbers
    !       ix,iy   i.s.   i   grid indices
    !       s       r.a.   o   source term (1-d version)
    !       d       r.a.   o   diagonal term of derivative (1-d version)
    !       wn_r    r.a.   i   wavenumbers in ice
    !       cg_ice  r.a.   i   group velocities in ice
    !       alpha   r.a.   i   exponential decay rate of energy
    !       r       r.a.   i   ratio of energy to wave energy without ice
    !       icef    real   i   ice floe diameter
    !
    !       imported via module:
    !       icep2   r.a.   i   eddy viscosity
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing (!/s switch).
    !      prt2ds    subr. w3arrymd print plot output (!/t1 switch).
    !      outmat    subr. w3arrymd matrix output (!/t2 switch).
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3srce    subr. w3srcemd source term integration.
    !      w3expo    subr.   n/a    ascii point output post-processor.
    !      w3exnc    subr.   n/a    netcdf point output post-processor.
    !      gxexpo    subr.   n/a    grads point output post-processor.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks :
    !
    !     if ice parameter 1 is zero, no calculations are made.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !     !/t   enable general test output.
    !     !/t0  2-d print plot of source term.
    !     !/t1  print arrays.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    use w3odatmd, only: ndse
    use w3servmd, only: extcde
    use w3dispmd
    use w3gdatmd, only: nk, nth, nspec, sig, mapwn, ic2pars, dden,  &
         flagll, ygrd, gtype, rlgtype
    use w3idatmd, only: inflags2,icep1,icep2,icep3,icep4,icep5,icei
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    real, intent(in)       :: a(nspec), depth, iceh
    real, intent(in)       :: cg(nk),   wn(nk)
    real, intent(out)      :: s(nspec), d(nspec)
    real, intent(in)       :: alpha(nk) ! exponential (spatial) decay rate for energy (1/m)
    integer, intent(in)    :: ix, iy
    real, intent(in)       :: wn_r(nk), cg_ice(nk), r(nk)
    real, intent(in)       :: icef ! hypothesis: friction does not occur for broken ice
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ikth, ik
    real                    :: d1d(nk)        !in sbt1: d1d was named "cbeta"
    real                    :: icecoef1, icecoef2, iceconc
    real, allocatable       :: wn_i(:)  ! exponential (spatial) decay rate for amplitude (1/m)
    real                    :: viscm=1.83e-6 ! molecular viscosity of water at freezing
    real                    :: pturb, pvisc, dturb, dvisc,   &
         smooth, re, uorb, aorb, eb,   &
         deli1, deli2, fw, xi, fturb,  &
         cg_eff(nk), wlg_r(nk), smooth_dmax(nk)
    integer                 :: ind, ith, is
    logical                 :: noice=.false.
    ! warning, alpha = 2 * wn_i -> makes wn_i useless, doesnt it ?
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 0.  initializations ------------------------------------------------ *
    !
    d        = 0.0
    !
    allocate(wn_i(nk))
    wn_i     = 0.0
    icecoef1 = iceh
    icecoef2 = 0.0
    iceconc  = 0.0
    cg_eff = 0.
    smooth_dmax(:)=1.
    !
    if (inflags2(-7))icecoef1 = iceh
    if (inflags2(-6))icecoef2 = icep2(ix,iy)
    if (inflags2(4))  iceconc = icei(ix,iy)
    !
    !
    ! 1.  no ice --------------------------------------------------------- /
    !
    noice=.false.
    if (icecoef1==0.0) noice=.true.
    if (inflags2(4).and.(iceconc==0.0)) noice=.true.
    if ( noice ) then
      d = 0.0
      !
      ! 2.  ice ------------------------------------------------------------ /
    else
      !
      ! 2.a set constant(s) and write test output -------------------------- /
      !
      !         (none)
      !
      !
      ! 2.b make calculations ---------------------------------------------- /
      !  icecoef1 = h_ice
      !  icecoef2 = visc
      !
      ! branches out depending on choice of dispersion relation...
      ! by default ic2pars(1)=0, and attenuation computed as described in stopa et al. 2016
      !
      if (ic2pars(1).gt.0.5) then
        if (.not.inflags2(-7))then
          write (ndse,1001) 'ice parameter 1'
          call extcde(2)
        endif
        if (.not.inflags2(-6))then
          write (ndse,1001) 'ice parameter 2'
          call extcde(2)
        endif
        !
        wn_i(:) = 0.5 * alpha(:) !  alpha=2*wn_i
        do ik=1, nk
          !            recall that d=s/e=-2*cg*k_i
          !            note: we should not use cg_ice here unless cg_ice is also
          !            used for advection in w3wavemd.ftn (see lines for ic3
          !            there).
          d1d(ik)= -2.0 * cg(ik) * wn_i(ik)
        end do
        !
        ! alternative by f.a.: generalization to a turbulent boundary layer
        !                      uses the ice-free dispersion, to be updated later
        !
      else ! goes here if ic2pars(1).le.0.5 (this is the default behavior)
        if (ic2pars(2).gt.0.) then
          uorb=0.
          aorb=0.
          fturb = ic2pars(2)
          ! special treatment in the southern ocean ...
          if (ic2pars(7).gt.0) then
            if (ygrd(iy,ix).lt.0.and.gtype.eq.rlgtype.and.flagll) fturb = ic2pars(7)
          end if
          do ik=1, nk
            eb  = 0.
            do ith=1, nth
              is=ith+(ik-1)*nth
              eb  = eb  + a(is)
            end do
            !
            !  uorb and aorb are the variances of the orbital velocity and surface elevation
            ! of the water relative to the ice ... this is only correct if the ice layer
            ! does not move. this should is changed by taking into account dmax when ic2dmax > 0:
            !
            !
            if (r(ik).gt.1.) then
              uorb = uorb + eb * smooth_dmax(ik)* sig(ik)**2 * dden(ik) / cg(ik) &
                   / (r(ik)*cg_ice(ik)/cg(ik))
              aorb = aorb + eb * smooth_dmax(ik)             * dden(ik) / cg(ik) &
                   / (r(ik)*cg_ice(ik)/cg(ik)) !deep water only
            else
              uorb = uorb + eb * smooth_dmax(ik) *sig(ik)**2 * dden(ik) / cg(ik)
              aorb = aorb + eb * smooth_dmax(ik)             * dden(ik) / cg(ik) !deep water only
            end if
          end do
          !
          aorb = 2*sqrt(aorb)  ! significant amplitude
          uorb = 2*sqrt(uorb)  ! significant amplitude
          re = uorb*aorb / viscm
          smooth = 0.5*tanh((re-ic2pars(4))/ic2pars(5))
          pturb=(0.5+smooth)
          pvisc=(0.5-smooth)
          xi=(alog10(max(aorb/ic2pars(3),3.))-abmin)/delab
          ind  = min (sizefwtable-1, int(xi))
          deli1= min (1. ,xi-float(ind))
          deli2= 1. - deli1
          fw =fwtable(ind)*deli2+fwtable(ind+1)*deli1
          dturb= fturb*fw*uorb/grav
        else ! so case of ic2pars(2).le.0.
          dturb = 0.
          pturb = 0.
          pvisc = 1.
        end if ! if (ic2pars(2).gt.0.)
        !
        do ik=1, nk
          ! wn_r is used here but warning, this is only ok for unbroken ice
          dvisc = ic2pars(6) * wn_r(ik) * sqrt(viscm* sig(ik) / 2.)
          d1d(ik) = -1.*(pturb*max(dturb*sig(ik)**2,dvisc) + pvisc*dvisc) &
               *smooth_dmax(ik)
        end do
      end if !  if (ic2pars(1).gt.0.5)
      !
      ! 2.c fill diagional matrix
      !
      do ikth=1, nspec
        d(ikth) = d1d(mapwn(ikth))
      end do
      !
    end if !    if ( noice ) then
    !
    s = d * a
    !
    ! ... test output of arrays
    !
    !
    !
    ! formats
    !
1001 format (/' *** wavewatch iii error in w3sic2 : '/               &
         '     ',a,' required but not selected'/)
    !
    !/
    !/ end of w3sic2 ----------------------------------------------------- /
    !/
  end subroutine w3sic2
  !/
  !/ end of module w3sic2md -------------------------------------------- /
  !/
end module w3sic2md
