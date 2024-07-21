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
module w3sic3md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           e. rogers               |
  !/                  |           s. zieger               |
  !/                  |           x. zhao                 |
  !/                  |           s. cheng                |
  !/                  |                        fortran 90 |
  !/                  | last update :         04-jan-2016 |
  !/                  +-----------------------------------+
  !/
  !/    updates:
  !/    29-may-2014 : generalization with turbulent bl
  !/           (f.a. method imported from ic2 by e.r.)    ( version 5.01 )
  !/    04-jan-2016 : importing code provided by s. cheng
  !/           (improved solution methods for wang and shen model)
  !/
  !  1. purpose :
  !
  !     calculate ice source term s_{ice} according to a viscoelastic sea
  !     ice model (wang and shen 2010).
  !
  !     reference: wang, r., and h. h. shen (2010), gravity waves
  !     propagating into an ice‐covered ocean: a viscoelastic model, j.
  !     geophys. res., 115, c06024, doi:10.1029/2009jc005591 .
  !
  !  2. variables and types :
  !      name              type  scope    description
  !     ------------------------------------------------------------------
  !      ic3table_cheng    int.  public   table of wave number k_r,
  !                                       attenuation k_i and group
  !                                       velocity cg
  !      ic3_ditk          r.a.  private  ice thickness increment
  !      ic3_maxitk        r.a.  private  maximum ice thickness, the code
  !                                       may fail for situation with ice
  !                                       thickness larger than this value
  !
  !  3. subroutines and functions :
  !
  !      name              type  scope    description
  !     ------------------------------------------------------------------
  !      w3sic3            subr. public   ice source term.
  !      bsdet             func. private  calculate the determinant for
  !                                       the dispersion relation.
  !      wn_cmplx_v1       func. private  calculate complex wavenumber in
  !                                       ice
  !      wn_precalc_cheng  subr. private  calculate complex wavenumber in
  !                                       ice
  !      wn_cmplx_hf       func. private  like above, but for h-f waves
  !      cmplx_root_muller_cheng func. private  find root for complex
  !                                             numbers
  !      fun_zhao          func. private  wrapper function for func0/func1
  !      func0_zhao        func. private
  !      func1_zhao        func. private
  !      w3ic3wncg         subr. public   calculate kr,ki and cg for all
  !                                       frequency at each grid point
  !      ic3precalc_cheng  subr. private  calculate kr,ki and cg table for
  !                                       all frequencies and ice
  !                                       thickness from 0~ic3_maxitk
  !      cginic3_cheng           func. private  calculate group velocity
  !                                       related to ic3 model
  !      f_zhao            func. private  wrapper function for double/
  !                                       quadruple precision
  !     ------------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !     see subroutine documentation.
  !
  !  5. remarks :
  !
  !  6. switches :
  !
  !     see subroutine documentation.
  !
  !  7. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  public  ::  w3sic3, w3ic3wncg_v1, w3ic3wncg_cheng
  private ::  wn_cmplx_v1, wn_cmplx_hf
  private ::  cmplx_root_muller_v1, cmplx_root_muller_cheng
  private ::  f_zhao_v1, f_zhao_cheng
  private ::  func1_zhao, func0_zhao, bsdet
  integer,save ::  calledic3table = 0
  real,private,parameter    ::  ic3_ditk  = 0.01, ic3_maxitk = 3.
  public  ::  ic3table_cheng
  private ::  ic3precalc_cheng, cginic3_cheng
contains
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3sic3 (a, depth, cg, wn, ix, iy, s, d)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           e. rogers               |
    !/                  |           s. zieger               |
    !/                  |                        fortran 90 |
    !/                  | last update :         11-oct-2013 |
    !/                  +-----------------------------------+
    !/
    !/    06-may-2013 : origination (copied from sice1)     ( version 4.10 )
    !/                                                        (e. rogers)
    !/    09-oct-2013 : update to meet ww3 coding standard    (s. zieger)
    !/
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
    !     calculate ice source term s_{ice} according to a viscoelastic sea
    !     ice model (wang and shen 2010).
    !
    !     reference: wang, r., and h. h. shen (2010), gravity waves
    !     propagating into an ice‐covered ocean: a viscoelastic model, j.
    !     geophys. res., 115, c06024, doi:10.1029/2009jc005591 .
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
    !     following w3sbt1 as a guide, equation 1 of w3sbt1 says:
    !         s = d * e
    !     however, the code of w3sbt1 has
    !         s = d * a
    !     this leads me to believe that the calling routine is
    !         expecting "s/sigma" not "s"
    !     thus we will use d = s/e = -2 * cg * k_i
    !
    !     the calling routine is expecting "s/sigma" not "s"
    !        thus we will use d = s/e = -2 * cg * k_i
    !        (see also documentation of w3sic1)
    !
    !     notes regarding numerics:
    !
    !     experiments with constant k_i values suggest that results may be
    !        dependent on resolution if insufficient resolution is used.
    !        for detailed information, see documentation of w3sic1.
    !
    !     note regarding applicability/validity:
    !
    !     the wang and shen model is intended as a generalized model for
    !     various types of ice cover. it is a "continuum" model for
    !     which the same model is used from the ice edge to the ice
    !     interior. though the ice types are expected to be very different
    !     from the edge to the interior, this is accomodated by the relative
    !     importance of the "effective viscosity" and the "modulus of
    !     elasticity". at the ice edge, where one finds frazil ice, pancake
    !     ice, or ice floes much smaller than the wave length, the "viscous"
    !     component of the model is believed to be most appropriate. at the
    !     interior, where one finds a continuous ice sheet, the "elastic
    !     model" component of the generalized visco-elastic model is
    !     expected to be appropriate. in addition to the case of continuous
    !     ice, wang and shen argue that the elastic model is also applicable
    !     to ice floes when the floe sizes are large relative to the
    !     wavelength. so to summarize,
    !     * frazil ice, pancake ice, and floes smaller than wavelength :
    !          viscosity dominates
    !     * continuous ice, and floes larger than wavelength :
    !          elasticity dominates
    !     * intermediate conditions: neither dominates
    !     all this is accomodated in ww3 by using non-uniform specification
    !     of viscosity and elasticity.
    !
    !     in the case where a user wishes to utilize only the "viscous
    !     model" aspect of wang and shen, and use an alternative scheme for
    !     continous ice and large ice floes, we allow this through the use
    !     of a user-defined namelist parameter "ic3maxthk". floe size is
    !     not (at time of writing) an output available from ice model cice,
    !     so we use the ice thickness as a way to anticipate the floe size.
    !     when ice thickness exceeds ic3maxthk, ww3 will use another model
    !     in place of the wang and shen formulation :
    !
    !     s_ice by f.a., an estimation of dissipation by turbulence
    !     at the ice-water interface. it uses only namelists for input, and
    !     no space/time varying input (though of course ice concentration is
    !     space/time varying). unlike liu et al. (ic2), it does not use
    !     ice thickness and does not yield a new c|cg|k (i.e. it is non-
    !     dispersive), but it has the very nice feature of not requiring
    !     an eddy viscosity, which is a major drawback of the liu et al.
    !     model. that is why we use it here, vs. liu et al.
    !     (s_ice by liu et al. and s_ice by f.a. are the two options
    !     available in ic2, i.e. w3sic2md.ftn)
    !
    !     at time of writing (march 23 2015), there may be some problems
    !     with the root-selection of ic3. for example with these settings:
    !     hice=1   ; rho_ice=917.0 ; emod=4.9e+12 ; visc=5e+7 ;
    !     h_water=deep (without using the ic3maxthk feature) the solution
    !     is rather irregular:
    !     t=11.11    k_i = 215.0e-5
    !     t=10       k_i = 266.0e-5
    !     t=9        k_i =   1.4e-5
    !     t=8.1      k_i =   1.7e-5
    !     with hice=0.1, ki is monotonically increasing in that range:
    !     t=11.11    k_i = 0.97e-5
    !     t=10       k_i = 2.64e-5
    !     t=9        k_i = 3.90e-5
    !     t=8.1      k_i = 4.47e-5
    !     of course, when using ic3maxthk=0.1, the first example (hice=1)
    !     would switch to the "s_ice by f.a." model, and so this problem
    !     is circumvented.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a       r.a.  i   action density spectrum (1-d).
    !       depth   real  i   local water depth.
    !       cg      r.a.  i   group velocities.
    !       wn      r.a.  i   wavenumbers.
    !       ix,iy   i.s.  i   grid indices.
    !       s       r.a.  o   source term (1-d version).
    !       d       r.a.  o   diagonal term of derivative (1-d version).
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
    !     code by s. cheng sets noice=.true. if isnan(icecoef1).
    !     comments are "maps may not be compatible"
    !     this feature is not understood by me (er) and so omitted.
    !
    !/ ------------------------------------------------------------------- /
    !     on array size, s. cheng says:
    !     upon checking the origin of cg, i would say cg = cg_ic3, this is
    !     the topic ‘call w3ic3wncg twice’. recently i find they are not
    !     exactly the same due to calculation and smoothing of cg using
    !     several neighbor points. for different input array size, results
    !     are slightly different. in subr. w3wave, the size of input arrays
    !     is 0:nk+1, while in w3sic3, the size of all input arrays is 1:nk.
    !     this array size  difference is reflected in the resulting cg. the
    !     small difference in cg between calling ic3 twice or just calling
    !     once produces small difference in swh. to eliminate this small
    !     difference, i suggest to keep cg instead of cg_ic3, as well as wn,
    !     wn_i, because other source terms use cg. i confirmed this change
    !     would make results of ice the same whether calling twice or once
    !     by defining dimension wn_r, wn_i, cg_ic3 as 0:nk+1 instead of
    !     1:nk. then cg_ic3 = cg.
    !/ ------------------------------------------------------------------- /
    !     on optimization, s. cheng says:
    !     for wang and shen’s model, d does not change in the loop
    !     corresponding to nsteps in subr. w3srce. i find the most efficient
    !     and easy way to speed up is that add d and nsteps as inputs of
    !     w3sic3
    !     if nsteps==1
    !          current wang and shen’s model code above.
    !     else
    !          s = d*a
    !     endif
    !/ ------------------------------------------------------------------- /
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
    use constants, only: tpi, dwat, abmin, delab, sizefwtable,      &
         fwtable, grav
    use w3odatmd, only: ndse, iaproc, naproc, naperr
    !     use wmmdatmd, only: improc, nmperr ! wmmdatmd unavailable to outp
    use w3servmd, only: extcde
    use w3gdatmd, only: nk, nth, nspec, sig, mapwn, ic3pars, dden,  &
         flagll, ygrd, gtype, rlgtype
    use w3idatmd, only: icep1, icep2, icep3, icep4, icep5, icei,    &
         inflags2
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: cg(nk),  wn(nk), a(nspec), depth
    real, intent(out)       :: s(nspec), d(nspec)
    integer, intent(in)     :: ix, iy
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                    :: ith
    integer                    :: ikth, ik
    real                       :: icecoef1, icecoef2, icecoef3, &
         icecoef4, icecoef5, iceconc
    real, dimension(nk)        :: d1d, wn_i, wn_r, cg_ic3, cg_tmp
    logical                    :: noice
    real                       :: viscm=1.83e-6
    real                       :: freq
    !  ............viscm=1.83e-6 :  molecular viscosity of water at freezing
    real                    :: pturb, pvisc, dturb, dvisc,   &
         smooth, re, uorb, aorb, eb,   &
         deli1, deli2, fw, xi, fturb,  &
         maxthk, maxcnc, use_cheng,    &
         use_cgice, fixedhice,   &
         fixedvisc,fixeddens,fixedelas
    integer                 :: ind, is, numin
    !
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 0.  initializations ------------------------------------------------ /
    !
    !
    d        = 0.0
    d1d      = 0.0
    !
    wn_r     = wn
    wn_i     = 0.0
    cg_ic3   = 0.0
    cg_tmp   = 0.0
    !
    icecoef1 = 0.0
    icecoef2 = 0.0
    icecoef3 = 0.0
    icecoef4 = 0.0
    icecoef5 = 0.0
    iceconc  = 0.0
    !
    !     rename variables to make code easier to read.
    maxthk=ic3pars(1)
    maxcnc=ic3pars(8)
    use_cheng=ic3pars(9)
    use_cgice=ic3pars(12)
    fixedhice=ic3pars(13)
    fixedvisc=ic3pars(14)
    fixeddens=ic3pars(15)
    fixedelas=ic3pars(16)
    !     --- error checking for input ----------------------------------- /
    !     --- allow one and only one input option for each variable ------ /
    numin=0
    if (inflags2(-7)) numin=numin+1
    if (fixedhice.ge.0.0) numin=numin+1
    if (numin.ne.1) then
      if ( iaproc .eq. naperr )                       &
           write (ndse,1001) 'ice parameter 1 (hice)',numin
      call extcde(2)
    endif
    numin=0
    if (inflags2(-6)) numin=numin+1
    if (fixedvisc.ge.0.0) numin=numin+1
    if (numin.ne.1) then
      if ( iaproc .eq. naperr )                       &
           write (ndse,1001) 'ice parameter 2 (visc)',numin
      call extcde(2)
    endif
    numin=0
    if (inflags2(-5)) numin=numin+1
    if (fixeddens.ge.0.0) numin=numin+1
    if (numin.ne.1) then
      if ( iaproc .eq. naperr )                       &
           write (ndse,1001) 'ice parameter 3 (dens)',numin
      call extcde(2)
    endif
    numin=0
    if (inflags2(-4)) numin=numin+1
    if (fixedelas.ge.0.0) numin=numin+1
    if (numin.ne.1) then
      if ( iaproc .eq. naperr )                       &
           write (ndse,1001) 'ice parameter 4 (elas)',numin
      call extcde(2)
    endif
    !     --- set local value to be used subsequently (icepx variables
    !         are not used beyond this point). --------------------------- /
    if (inflags2(-7)) then
      icecoef1 = icep1(ix,iy) ! ice thickness
    else
      icecoef1 = fixedhice
    endif
    if (inflags2(-6)) then
      icecoef2 = icep2(ix,iy) ! effective viscosity of ice cover
    else
      icecoef2 = fixedvisc
    endif
    if (inflags2(-5)) then
      icecoef3 = icep3(ix,iy) ! density of ice
    else
      icecoef3 = fixeddens
    endif
    if (inflags2(-4)) then
      icecoef4 = icep4(ix,iy) ! effective shear modulus of ice
    else
      icecoef4 = fixedelas
    endif
    !     icecoef5 = icep5(ix,iy) ! icep5 is inactive in w3sic3
    if (inflags2(4))  iceconc = icei(ix,iy)
    !
    ! 1.  no ice --------------------------------------------------------- /
    !
    noice=.false.
    if (icecoef1==0.0) noice=.true.
    if (inflags2(4).and.(iceconc==0.0)) noice=.true.
    if ( noice ) then
      d1d=0.0
      !
      ! 2.  ice ------------------------------------------------------------ /
    elseif ( use_cheng==1.0 .and.  &
         ((icecoef1.le.maxthk).or.(iceconc.le.maxcnc)) ) then
      ! 2.a write test output ---------------------------------------------- /
      ! 2.b make calculations using cheng routines ------------------------- /
      !     --- input to routine (part 1): 6 ice parameters from single
      !         precision variables. ---------------------------------------
      call w3ic3wncg_cheng(wn_r, wn_i, cg_ic3, icecoef1, icecoef2, &
           icecoef3, icecoef4, depth)
      !
      !    --- calculate source term --------------------------------------- /
      !    --- see remarks section re: array size -------------------------- /
      if ( use_cgice==1.0 ) then
        cg_tmp=cg_ic3
      else
        cg_tmp=cg
      endif
      do ik=1, nk
        !            recall that d=s/e=-2*cg*k_i
        d1d(ik)= -2.0 * cg_tmp(ik) * wn_i(ik)
      end do
    elseif ( (icecoef1 .le. maxthk) .or. (iceconc .le. maxcnc) ) then
      !.......... e.g. if ice thickness is .le. 10 cm
      !...............or concentration is .le. 1.0
      !
      ! 2.a write test output ---------------------------------------------- /
      !
      ! 2.b make calculations using original routines ---------------------- /
      !     --- input to routine (part 1): 6 ice parameters from single
      !         precision variables. ---------------------------------------
      call w3ic3wncg_v1(wn_r, wn_i, cg_ic3, icecoef1, icecoef2, &
           icecoef3, icecoef4, depth     )
      !
      !    --- calculate source term --------------------------------------- /
      if ( use_cgice==1.0 ) then
        cg_tmp=cg_ic3
      else
        cg_tmp=cg
      endif
      do ik=1, nk
        !            recall that d=s/e=-2*cg*k_i
        d1d(ik)= -2.0 * cg_tmp(ik) * wn_i(ik)
      end do
      !
    else ! .. e.g. if ice thickness is .gt. 10 cm
      ! alternative by f.a., see remarks section.
      if (ic3pars(2).gt.0.) then
        uorb=0.
        aorb=0.
        fturb = ic3pars(2)
        if (ic3pars(7).gt.0) then
          if (ygrd(iy,ix).lt.0.and.gtype.eq.rlgtype.and.flagll) &
               fturb = ic3pars(7)
        end if
        do ik=1, nk
          eb  = 0.
          do ith=1, nth
            is=ith+(ik-1)*nth
            eb  = eb  + a(is)
          end do
          !
          !  uorb and aorb are the variances of the orbital
          !  velocity and surface elevation
          !
          uorb = uorb + eb *sig(ik)**2 * dden(ik) / cg(ik)
          aorb = aorb + eb             * dden(ik) / cg(ik)
          !deep water only
        end do
        !
        aorb = 2*sqrt(aorb)  ! significant amplitude
        uorb = 2*sqrt(uorb)  ! significant amplitude
        re = uorb*aorb / viscm
        smooth = 0.5*tanh((re-ic3pars(4))/ic3pars(5))
        pturb=(0.5+smooth)
        pvisc=(0.5-smooth)
        xi=(alog10(max(aorb/ic3pars(3),3.))-abmin)/delab
        ind  = min (sizefwtable-1, int(xi))
        deli1= min (1. ,xi-float(ind))
        deli2= 1. - deli1
        fw =fwtable(ind)*deli2+fwtable(ind+1)*deli1
        dturb=-1.* fturb*fw*uorb/grav
      else ! so case of ic3pars(2).le.0.
        dturb = 0.
      end if ! if (ic3pars(2).gt.0.)
      do ik=1, nk
        dvisc = -1. *ic3pars(6) * wn(ik) * sqrt(viscm* sig(ik) / 2.)
        d1d(ik) = pturb*dturb*sig(ik)**2 +  pvisc*dvisc
      end do
    end if !   if ( noice ) then
    ! 2.c fill diagional matrix ------------------------------------------ /
    !
    do ikth=1, nspec
      d(ikth) = d1d(mapwn(ikth))
    end do
    !
    ! sign convention (example):
    !        s is from -10e-3 to 0
    !        a is from 0 to 10
    ! see remarks section re: optimization
    s = d * a
    !
    ! ... test output of arrays
    !
    !
    !
    ! formats
    !
1001 format (/' *** wavewatch iii error in w3sic3 : '/               &
         '     ',a,' required once, but was provided by user '/ &
         '     ',i4,' times.'/)
    !
    !/
    !/ end of w3sic3 ----------------------------------------------------- /
    !/
  end subroutine w3sic3
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3ic3wncg_v1(wn_r,wn_i,cg,ice1,ice2,ice3,ice4,dpt)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           e. rogers               |
    !/                  |           s. zieger               |
    !/                  |                        fortran 90 |
    !/                  | last update :         25-oct-2013 |
    !/                  +-----------------------------------+
    !/
    !/    06-may-2013 : origination (port from clarkson.f90)( version 4.10 )
    !/                                                        (e. rogers)
    !/    09-oct-2013 : update to meet ww3 coding standard    (s. zieger)
    !/
    !  1. purpose :
    !
    !     calculation of complex wavenumber for waves in ice. outsourced
    !     from w3sic3 to allow update on wavenumbers and  group
    !     velocities at each time step an ice parameter is updated.
    !
    !  2. method :
    !
    !     <text here>
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       wn_r   r. a.  i/o  wave number (real part)
    !       wn_i   r. a.  i/o  wave number (imag. part=wave attenuation)
    !       cg     r. a.  i/o  group velocity
    !       ice1   real    i   thickness of ice                 [in m]
    !       ice2   real    i   effective viscosity of ice       [in m2/s]
    !       ice3   real    i   density of ice                  [in kg/m3]
    !       ice4   real    i   effective shear modulus of ice   [in pa]
    !       dpt    real    i   water depth                      [in m]
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name    type  module    description
    !     ----------------------------------------------------------------
    !     wavnu1   subr. w3dispmd   wavenumber for waves in open water.
    !     wn_cmplx func. w3sic3md   complex wavenumber for waves in ice.
    !     wn_cmplx_hf func. w3sic3md   like wn_cmplx, but for h-f
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name    type  module   description
    !     ----------------------------------------------------------------
    !      w3sic3  subr. w3sic3md ice source term.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     see format 900.
    !
    !  7. remarks :
    !
    !     optional: cap wn_i at 2.0e-4, since in simple tests with "normal
    !     resolution" (not finer than 1 km), ww3 has trouble resolving the
    !     dissipation if k_i>2e-4. also, very large values of dissipation
    !     (e.g. k_i=100e-4) with ic3 occurs more in the higher frequencies
    !     which makes ww3 slow down quite a bit. this is done via icekilim
    !     in the namelists.
    !
    !     this function does not get used in update by s. cheng.
    !     it should be removed if/when "v1" routines are removed.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. source code :
    !/
    !/ ------------------------------------------------------------------- /
    !/
    use w3gdatmd, only: nk, sig, ic3pars
    use w3dispmd, only: wavnu1
    use w3odatmd, only: ndse
    use w3servmd, only: extcde
    use constants, only: tpi
    !/
    implicit none
    !/
    real, intent(inout):: wn_r(:),wn_i(:),cg(:)
    real, intent(in)   :: ice1, ice2, ice3, ice4, dpt
    integer            :: ik, kl,ku
    real, allocatable  :: sigma(:),cg_ic3(:)
    real               :: k_ocean, cg_ocean
    double precision   :: kh, k_noice, hwat, hice, nu, dice, es_mod
    double precision,parameter :: khmax = 18.0d0 ! 18=ok, 19=fails
    double complex     :: wncomplex,wncomplex_old
    real               :: stensec
    real               :: ic3hilim,ic3kilim
    !
    allocate( cg_ic3( size(cg) ) )
    allocate(  sigma( size(cg) ) )
    cg_ic3  = 0.
    sigma   = 0.
    stensec=tpi/10.0 ! sigma for t=10 sec
    ic3hilim=ic3pars(10)
    ic3kilim=ic3pars(11)
    !
    !     --- input to routine (part 1): set 6 double precision variables
    !        using single precision variables. -------------------------- /
    hwat    = dble(dpt)     ! water depth
    hice    = dble(ice1)    ! ice thickness
    nu      = dble(ice2)    ! "effective viscosity" parameter
    dice    = dble(ice3)    ! density of ice
    es_mod  = dble(ice4)    ! effective shear modulus of ice
    ! optional: limit ice thickness
    hice=min(dble(ic3hilim),hice)
    if (size(wn_r,1).eq.nk) then
      kl    = 1
      ku    = nk
      sigma = sig(1:nk)
    else if (size(wn_r,1).eq.nk+2) then
      kl    = 1
      ku    = nk+2
      sigma = sig(0:nk+1)
    else
      write(ndse,900)
      call extcde(3)
    end if
    !
    wncomplex_old=cmplx(0.0d0,0.0d0)
    do ik = kl,ku
      !     --- input to routine (part 2): set 2 double precision variables
      !         using single precision variable. --------------------------- /
      call wavnu1(sigma(ik),dpt,k_ocean,cg_ocean)
      k_noice = dble(k_ocean)
      !
      !     --- muller method fails for deep water: workaround follows ----- /
      kh        = k_noice * hwat ! kh w/out ice
      if (kh.gt.khmax) then
        hwat   = khmax / k_noice
      endif
      !     --- calculate complex wavenumber ------------------------------- /
      if((ik.gt.kl).and.(sigma(ik).gt.stensec))then
        wncomplex = wn_cmplx_hf(dble(sigma(ik)),k_noice,es_mod,nu, &
             dice,hice,hwat,dble(sigma(ik-1)),wncomplex_old)
        wncomplex_old=wncomplex
      else
        wncomplex = wn_cmplx_v1(dble(sigma(ik)),k_noice,es_mod,nu, &
             dice,hice,hwat)
        wncomplex_old=wncomplex
      endif
      !     --- output from function is type of double complex. set
      !         precision of imaginary to single precision array element --- /
      wn_i(ik)  = real(aimag(wncomplex))
      ! optional : limit ki
      wn_i(ik)  = min(wn_i(ik),ic3kilim) ! see remarks above
      wn_r(ik)  = real(wncomplex)
    end do
    !     --- update group velocitiy ----
    cg_ic3 = delta(sigma) / delta(wn_r)
    cg     = cg_ic3
    deallocate(cg_ic3)
    !
900 format (/' *** wavewatch iii error in w3sic3_w3ic3wncg : '/&
         '     cannot determine bounds of wavenumber array.')
    !
    !/
  end subroutine w3ic3wncg_v1
  !/ ------------------------------------------------------------------- /
  !/
  function wn_cmplx_v1(sigma,wn_o,es,nu,dice,hice,depth)    result(wn)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           e. rogers               |
    !/                  |           s. zieger               |
    !/                  |                        fortran 90 |
    !/                  | last update :         30-oct-2013 |
    !/                  +-----------------------------------+
    !/
    !/    06-may-2013 : origination (port from clarkson.f90)( version 4.10 )
    !/                                                        (e. rogers)
    !/    09-oct-2013 : update to meet ww3 coding standard    (s. zieger)
    !/    30-oct-2013 : clarkson.f90 update added             (s. zieger)
    !/
    !  1. purpose :
    !
    !     calculate complex wavenumber for waves in ice.
    !
    !  2. method :
    !
    !     wang and shen (jgr 2010)
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       wn     cmplx dbl o  wave number (imag. part=wave attenuation)
    !       sigma  real dbl  i  wave angular frequency           [in rad]
    !       wn_o   real dbl  i  wave number (open water)
    !       es     real dbl  i  effective shear modulus of ice   [in pa]
    !       nu     real dbl  i  effective viscosity of ice       [in m2/s]
    !       dice   real dbl  i  density of ice                  [in kg/m3]
    !       hice   real dbl  i  thickness of ice                 [in m]
    !       depth  real dbl  i  water depth                      [in m]
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name               type  module   description
    !     ----------------------------------------------------------------
    !      cmplx_root_muller_cheng  func. w3sic3md find root for complex
    !                                         wavenumbers for waves in ice.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !     w3ic3wncg_v1    subr. w3sic3md ice source term.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !   original authors: zhao and shen.
    !   this code is based on fortran code provided by hayley shen (clarkson
    !     university) to erick rogers (nrl) on april 19 2013.
    !
    !   hayley shen says,
    !     we have determined that it may not be necessary to use curve
    !     fitting or lookup tables to get the group velocity and the
    !     attenuation coefficient. attached is a short report with some
    !     sample numerical solutions. to implement the viscoelastic model,
    !     there are 4 fortran programs. according to xin zhao, the graduate
    !     student, it is very fast to find roots. i suggest that perhaps you
    !     try the pure viscous case by setting g=0 to start with. nu can be
    !     set at 0.05*ice concentration (m^2/s) to begin with, because for
    !     grease ice newyear's data showed nu to be about 0.02-0.03 m^2/s.
    !     by setting g=0 in you get exactly the keller model for pure
    !     viscous layer.
    !
    !   this routine provides the initial guess according to the parameters
    !   of the present case. t>10s use open water, t<10s cases, calculate
    !   t=10s first using open water as the initial guess.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants, only: tpi
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    double precision, intent(in) :: sigma,wn_o,es,nu,dice,hice,depth
    double complex               :: wn                      ! result
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer            :: i, nsub
    double precision   :: tt, ts, t
    double complex     :: x0, x1, x2, wn0
    !/
    !/ ------------------------------------------------------------------- /
    t    = dble(tpi) / sigma
    ts   = 10.
    nsub = int((ts-t) * 10.)
    !/
    if (hice<0.001) then
      wn  = cmplx(wn_o,0.)
    else if (t.lt.ts) then
      x0  = 0.01
      x1  = 0.1
      x2  = 1.0
      wn0 = cmplx_root_muller_v1(x0,x1,x2,0,dble(tpi)/ts, &
           es,nu,dice,hice,depth    )
      x0  = 0.90 * wn0
      x1  = wn0
      x2  = 1.1*wn0
      wn  = cmplx_root_muller_v1(x0,x1,x2,1,dble(tpi)/ts, &
           es,nu,dice,hice,depth    )
      do i=1,nsub
        x0 = 0.90 * wn
        x1 = wn
        x2 = 1.1 * wn
        tt = ts - (ts-t) / real(nsub) * real(i)
        wn = cmplx_root_muller_v1(x0,x1,x2,1,dble(tpi)/tt, &
             es,nu,dice,hice,depth    )
      enddo
    else
      x0  = 0.01
      x1  = 0.1
      x2  = 1.0
      wn0 = cmplx_root_muller_v1(x0,x1,x2,0,sigma,        &
           es,nu,dice,hice,depth    )
      x0  = 0.8 * wn0
      x1  = wn0
      x2  = 1.2 * wn0
      wn  = cmplx_root_muller_v1(x0,x1,x2,1,sigma,        &
           es,nu,dice,hice,depth    )
    endif
    !/
  end function wn_cmplx_v1
  !/ ------------------------------------------------------------------- /
  !/
  function wn_cmplx_hf(sigma,wn_o,es,nu,dice,hice,depth,sigma_last, &
       wn_last)     result(wn)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. shen                 |
    !/                  |           e. rogers               |
    !/                  |                        fortran 90 |
    !/                  | last update :         17-apr-2014 |
    !/                  +-----------------------------------+
    !/
    !/    15-jan-2014 : origination (from wn_cmplxa.f90)      (h. shen)
    !/    17-apr-2014 : import to ww3                         (e. rogers)
    !/
    !  1. purpose :
    !
    !     calculate complex wavenumber for waves in ice.
    !
    !  2. method :
    !
    !     wang and shen (jgr 2010)
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       wn     cmplx dbl o  wave number (imag. part=wave attenuation)
    !       sigma  real dbl  i  wave angular frequency           [in rad]
    !       wn_o   real dbl  i  wave number (open water)
    !       es     real dbl  i  effective shear modulus of ice   [in pa]
    !       nu     real dbl  i  effective viscosity of ice       [in m2/s]
    !       dice   real dbl  i  density of ice                  [in kg/m3]
    !       hice   real dbl  i  thickness of ice                 [in m]
    !       depth  real dbl  i  water depth                      [in m]
    !       sigma_last real dbl i : like sigma, but of last ik
    !       wn_last    real dbl i : wn_o of last ik
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name               type  module   description
    !     ----------------------------------------------------------------
    !      cmplx_root_muller_cheng  func. w3sic3md find root for complex
    !                                         wavenumbers for waves in ice.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3ic3wncg_v1  subr. w3sic3md ice source term.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !   original authors: zhao and shen.
    !     see notes in function wn_cmplx, not repeated here.
    !     new in this function, hayley shen says (jan 15 2014) :
    !     "to speed up the computation, we need to add a new function
    !     wn_cmplxa (attached) into the earlier version of the module
    !     w3sic3md. when wave period t>=10s, we call old function wn_cmplx
    !     directly. when t<10s, call the new function wn_cmplxa with last
    !     calculation step's information: last complex wave number, last
    !     angular wave frequency. the calculation should be from large t
    !     to small t."
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    ! 10. source code :
    !
    use constants, only: tpi
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    double precision, intent(in) :: sigma,wn_o,es,nu,dice,hice,depth
    double precision, intent(in) :: sigma_last
    double complex,   intent(in) :: wn_last
    double complex               :: wn                      ! result
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer            :: i, nsub
    double precision   :: tt, ts, t
    double complex     :: x0, x1, x2, wn0
    !/
    !/ ------------------------------------------------------------------- /
    t    = dble(tpi) / sigma
    ts   = dble(tpi) / sigma_last
    nsub = int((ts-t) * 10.)
    !/
    if (hice<0.001) then
      wn  = cmplx(wn_o,0.)
    else
      x0  = 0.90 * wn_last
      x1  = wn_last
      x2  = 1.1 * wn_last
      wn  = cmplx_root_muller_v1(x0,x1,x2,1,dble(tpi)/ts, &
           es,nu,dice,hice,depth    )
      do i=1,nsub
        x0 = 0.90 * wn
        x1 = wn
        x2 = 1.1 * wn
        tt = ts - (ts-t) / real(nsub) * real(i)
        wn = cmplx_root_muller_v1(x0,x1,x2,1,dble(tpi)/tt, &
             es,nu,dice,hice,depth    )
      enddo
    endif
    !/
  end function wn_cmplx_hf
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  !/
  function cmplx_root_muller_v1(x0, x1, x2, judge, sigma, es, nu, &
       dice, hice, depth)           result(p3)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           e. rogers               |
    !/                  |           s. zieger               |
    !/                  |                        fortran 90 |
    !/                  | last update :         30-oct-2013 |
    !/                  +-----------------------------------+
    !/
    !/    06-may-2013 : origination (port from clarkson.f90)( version 4.10 )
    !/                                                        (e. rogers)
    !/    09-oct-2013 : update to meet ww3 coding standard    (s. zieger)
    !/    30-oct-2013 : clarkson.f90 update added             (s. zieger)
    !/
    !  1. purpose :
    !
    !     find root.
    !
    !  2. method :
    !
    !     muller method for complex equations is a recursive approximation
    !     with initial guess x0, x1, and x2. to the initial guesses a
    !     quadratic parabola is fitted.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       p3    cmplx dbl o  approximation for the root problem
    !       x0    cmplx dbl i  initial guess variable
    !       x1    cmplx dbl i  initial guess variable
    !       x2    cmplx dbl i  initial guess variable
    !       judge integer   i  "switch variable" for f_zhao
    !       sigma double    i  wave angular frequency
    !       es    double    i  effective shear modulus of ice
    !       nu    double    i  effective viscosity of ice       [in m2/s]
    !       dice  double    i  density of ice                   [in kg/m3]
    !       hice  double    i  thickness of ice                 [in m]
    !       depth double    i  water depth                      [in m]
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      f_zhao    func. w3sic3md wrapper function for root finding.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name               type  module   description
    !     ----------------------------------------------------------------
    !      wn_cmplx_v1             find root for complex wave-
    !      wn_cmplx_hf             numbers for waves in ice.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !   original authors: zhao and shen.
    !   this code is based on fortran code provided by hayley shen (clarkson
    !     university) to erick rogers (nrl) on april 19 2013.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3odatmd, only: ndse
    use w3servmd, only: extcde
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    double complex                :: p3                     ! result
    double complex, intent(in)    :: x0,x1,x2
    double precision, intent(in)  :: sigma,es,nu,dice,hice,depth
    integer, intent(in)           :: judge
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                   :: i
    integer, parameter        :: imax = 1000
    double precision          :: dlta,epsi
    double complex            :: p0,p1,p2
    double complex            :: y0,y1,y2,y3
    double complex            :: a,b,c,q,disc,den1,den2
    !/
    !/ ------------------------------------------------------------------- /
    p0      = x0
    p1      = x1
    p2      = x2
    p3      = 0.0
    !
    i       = 0
    epsi    = 1.e-5
    dlta    = 1.e-5
    y0      = f_zhao_v1(p0,judge,sigma,es,nu,dice,hice,depth)
    y1      = f_zhao_v1(p1,judge,sigma,es,nu,dice,hice,depth)
    y2      = f_zhao_v1(p2,judge,sigma,es,nu,dice,hice,depth)
    !
    do i = 1,imax
      q = (p2 - p1) / (p1 - p0)
      a = q * y2 - q * (1.+q) * y1 + q**2. * y0
      b = (2. * q + 1.) * y2 - (1 + q)**2. * y1 + q**2. * y0
      c = (1. + q) * y2
      !
      if ( abs(a).ne.0. ) then
        disc = b**2. - 4 * a * c;
        !
        den1 = ( b + sqrt ( disc ) )
        den2 = ( b - sqrt ( disc ) )
        !
        if ( abs ( den1 ) .lt. abs ( den2 ) )then
          p3 = p2 - (p2 - p1) * (2 * c / den2)
        else
          p3 = p2 - (p2 - p1) * (2 * c / den1)
        endif
        !
      else
        !
        if ( abs(b) .ne. 0. )then
          p3 = p2 - (p2 - p1) * (c / b)
        else
          write(ndse,800)
          write(ndse,801)x0,x1,x2
          write(ndse,802)sigma,es,nu,dice,hice,depth
          write(ndse,803)judge
          call extcde(2)
        endif
      endif
      y3 = f_zhao_v1(p3,judge,sigma,es,nu,dice,hice,depth);
      if ( abs(p3-p2).lt.dlta .or. abs(y3).lt.epsi ) then
        return
      endif
      p0 = p1
      p1 = p2
      p2 = p3
      y0 = y1
      y1 = y2
      y2 = y3
    enddo
    !
    write(ndse,800)
    write(ndse,801)x0,x1,x2
    write(ndse,802)sigma,es,nu,dice,hice,depth
    write(ndse,803)judge
    call extcde(2)
    !
800 format (/' *** wavewatch iii error in w3sic3_cmplx_root_muller'/&
         ' :      muller method failed to find root.'            )
801 format (/'x0,x1,x2 = ',3(1x,'(',f10.5,',',f10.5,')'))
802 format (/'sigma,es,nu,dice,hice,depth = ',6(1x,f10.5))
803 format (/'judge = ',i5)
    !/
  end function cmplx_root_muller_v1
  !/ ------------------------------------------------------------------- /
  !/
  function f_zhao_v1(x,judge,sigma,es,nu,dice,hice,depth) &
       result(fzhao)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           e. rogers               |
    !/                  |           s. zieger               |
    !/                  |                        fortran 90 |
    !/                  | last update :         30-oct-2013 |
    !/                  +-----------------------------------+
    !/
    !/    06-may-2013 : origination (port from clarkson.f90)( version 4.10 )
    !/                                                        (e. rogers)
    !/    09-oct-2013 : update to meet ww3 coding standard    (s. zieger)
    !/    30-oct-2013 : clarkson.f90 update added             (s. zieger)
    !/
    !  1. purpose :
    !
    !     decide whether to call sub-function.
    !
    !  2. method :
    !
    !     decide based on value of integer "judge"
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !      fzhao compl8  o    result (double complex)
    !      x     cmplx8  i    approximate result (double complex)
    !      judge integr  i    switch variable
    !      sigma double  i    wave angular frequency
    !      es    double  i    effective shear modulus
    !      nu    double  i    effective viscosity parameter
    !      dice  double  i    density of ice
    !      hice  double  i    thickness of ice
    !      depth double  i    water depth
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name        type  module   description
    !     ----------------------------------------------------------------
    !      func0_zhao  func. w3sic3md function to find root.
    !      func1_zhao  func. w3sic3md function to find root.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name               type  module   description
    !     ----------------------------------------------------------------
    !      cmplx_root_muller_v1  func. w3sic3md find root for complex wave-
    !                                        numbers for waves in ice.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !   original authors: zhao and shen.
    !   this code is based on fortran code provided by hayley shen (clarkson
    !     university) to erick rogers (nrl) on april 19 2013.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    ! 10. source code :
    !/
    !/ ------------------------------------------------------------------- /
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)          :: judge
    double precision, intent(in) :: sigma,es,nu,dice,hice,depth
    double complex, intent(in)   :: x
    double complex               :: fzhao  ! result
    !/
    !/ ------------------------------------------------------------------- /
    if (judge.eq.0) then
      fzhao = func0_zhao(x,sigma,depth)
    else
      fzhao = func1_zhao(x,sigma,es,nu,dice,hice,depth)
    endif
    !
  end function f_zhao_v1
  !/ ------------------------------------------------------------------- /
  !/
  function func0_zhao(wn, sigma, depth)                result(func0)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           e. rogers               |
    !/                  |           s. zieger               |
    !/                  |                        fortran 90 |
    !/                  | last update :         30-oct-2013 |
    !/                  +-----------------------------------+
    !/
    !/    06-may-2013 : origination (port from clarkson.f90)( version 4.10 )
    !/                                                        (e. rogers)
    !/    09-oct-2013 : update to meet ww3 coding standard    (s. zieger)
    !/    30-oct-2013 : clarkson.f90 update added             (s. zieger)
    !/
    !  1. purpose :
    !
    !     calculate the difference between the left and right side
    !     of the dispersion relation. it is called by the muller method.
    !
    !  2. method :
    !
    !     <text here>
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !      func0  compl dbl  o    result (double complex)
    !      wn     cmplx dbl  i    complex wavenumber
    !      sigma  double     i    wave angular frequency
    !      depth  double     i    water depth [in m]
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !  5. called by :
    !
    !      name    type  module   description
    !     ----------------------------------------------------------------
    !      f_zhao_v1  func. w3sic3md function for computation of complex
    !                             wavenumbers for waves in ice.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !   original authors: zhao and shen.
    !   this code is based on fortran code provided by hayley shen (clarkson
    !     university) to erick rogers (nrl) on april 19 2013.
    !
    !   this function does not get used in update by s. cheng.
    !     it should be removed if/when "v1" routines are removed.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    ! 10. source code :
    !
    !/
    !/ ------------------------------------------------------------------- /
    use constants, only: grav
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    double complex, intent(in)    :: wn
    double precision, intent(in)  :: sigma, depth
    double complex                :: func0                 ! result
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    double complex   :: th
    !/
    !/ ------------------------------------------------------------------- /
    if (real(wn*depth).le.4.) then
      th = (exp(wn*depth)-exp(-wn*depth)) &
           / (exp(wn*depth)+exp(-wn*depth))
      func0 = sigma**2. - th * wn * dble(grav)
    else
      func0 = sigma**2. -      wn * dble(grav)
    end if
    !/
  end function func0_zhao
  !/ ------------------------------------------------------------------- /
  !/
  function func1_zhao(wn,sigma,es,nu,dice,hice,depth)  result(func1)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           e. rogers               |
    !/                  |           s. zieger               |
    !/                  |                        fortran 90 |
    !/                  | last update :         11-oct-2013 |
    !/                  +-----------------------------------+
    !/
    !/    06-may-2013 : origination (port from clarkson.f90)( version 4.10 )
    !/                                                        (e. rogers)
    !/    09-oct-2013 : update to meet ww3 coding standard    (s. zieger)
    !/    30-oct-2013 : clarkson.f90 update added             (s. zieger)
    !/
    !  1. purpose :
    !
    !     <text here>
    !
    !  2. method :
    !
    !     <text here>
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !      func1 cmplx dbl o  result (double complex)
    !      wn    cmplx dbl i  wavenumber (double complex)
    !      w     real  dbl i  wave angular frequency
    !      es    real  dbl i  effective shear modulus on ice
    !      nu    real  dbl i  effective viscosity
    !      dice  real  dbl i  density of ice
    !      hice  real  dbl i  thickness of ice
    !      depth real  dbl i  water depth
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name    type  module    description
    !     ----------------------------------------------------------------
    !      bsdet   func. w3sic3md  calculates the determinant for the
    !                              dispersion relation.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name    type  module   description
    !     ----------------------------------------------------------------
    !      f_zhao_v1  func. w3sic3md function for computation of complex
    !                             wavenumbers for waves in ice.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !   original authors: zhao and shen.
    !   this code is based on fortran code provided by hayley shen (clarkson
    !     university) to erick rogers (nrl) on april 19 2013.
    !
    !   this function does not get used in update by s. cheng.
    !     it should be removed if/when "v1" routines are removed.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    ! 10. source code :
    !/
    !/ ------------------------------------------------------------------- /
    use constants, only: grav, dwat
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    double complex, intent(in)   :: wn
    double precision, intent(in) :: sigma, es, nu, dice, hice, depth
    double complex               :: func1                   ! result
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    double complex   :: ve,alpha,n,m,l,sk,ck,sa,ca,th,thh
    double complex   :: aa(4,4)
    !/
    !/ ------------------------------------------------------------------- /
    ve    = cmplx( nu, es/dice/sigma )
    alpha = sqrt ( wn**2. - sigma/ve * cmplx(0.,1.) )
    n     = sigma + 2. * ve * wn**2. * cmplx(0.,1.)
    l     = 2 * wn * alpha * sigma * ve
    sk    = (exp(wn*hice)-exp(-wn*hice))/2.
    ck    = (exp(wn*hice)+exp(-wn*hice))/2.
    sa    = (exp(alpha*hice)-exp(-alpha*hice))/2.
    ca    = (exp(alpha*hice)+exp(-alpha*hice))/2.
    !
    if (real(wn*depth).le.4.) then
      th  = (exp(wn*depth)-exp(-wn*depth))                        &
           / (exp(wn*depth)+exp(-wn*depth))
      thh = ( exp(wn*(depth-hice)) - exp(-wn*(depth-hice)) )      &
           / ( exp(wn*(depth-hice)) + exp(-wn*(depth-hice)) )
    else
      th  = 1.0
      thh = 1.0
    end if
    !
    m     = (dble(dwat)/dice - 1) * dble(grav) * wn               &
         - dble(dwat) / dice * sigma**2 / th
    !
    if (es.gt.1.e7) then
      aa(1,1) = 0.
      aa(1,2) = 2 * cmplx(0.,1.) * wn**2.
      aa(1,3) = alpha**2. + wn**2.
      aa(1,4) = 0.
      !
      aa(2,1) = n * sigma
      aa(2,2) = -wn * dble(grav)
      aa(2,3) = cmplx(0.,1.) * wn * dble(grav)
      aa(2,4) = l
      !
      aa(3,1) = -2. * cmplx(0.,1.) * wn**2. * sk
      aa(3,2) =  2. * cmplx(0.,1.) * wn**2. * ck
      aa(3,3) =  (alpha**2. + wn**2.) * ca
      aa(3,4) = -(alpha**2. + wn**2.) * sa
      !
      aa(4,1) =   n * sigma * ck - m * sk
      aa(4,2) = - n * sigma * sk + m * ck
      aa(4,3) = -cmplx(0.,1.) * m * ca - l * sa
      aa(4,4) =  cmplx(0.,1.) * m * sa + l * ca
      !
      func1   = bsdet(aa,4)
    else
      func1 = sigma**2. - th*wn*dble(grav) - th*dice/dble(dwat)* &
           (wn**2.*dble(grav)**2.*sk*sa - (n**4. + 16.* &
           ve**4.*wn**6.*alpha**2.)*sk*sa - 8. &
           *wn**3.*alpha*ve**2.*n**2.*(ck*ca-1.))/(4.*wn**3. &
           *alpha*ve**2.*sk*ca+n**2.*sa*ck-dble(grav)*wn*sk*sa)
    endif
    !/
  end function func1_zhao
  !/ ------------------------------------------------------------------- /
  !/
  function bsdet(aa, n)                                  result(det)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           e. rogers               |
    !/                  |           s. zieger               |
    !/                  |                        fortran 90 |
    !/                  | last update :         11-oct-2013 |
    !/                  +-----------------------------------+
    !/
    !/    06-may-2013 : origination (port from clarkson.f90)( version 4.10 )
    !/                                                        (e. rogers)
    !/    09-oct-2013 : update to meet ww3 coding standard    (s. zieger)
    !/
    !  1. purpose :
    !
    !     this subroutine calculates the determinant for the
    !     dispersion relation.
    !
    !  2. method :
    !
    !  3. parameters :
    !
    !      parameter list
    !     ----------------------------------------------------------------
    !      aa    r.a.       i/o  square array type of real
    !      n     int        i    size of array (number of rows/cols)
    !      det   cmplx dble i/o  determinant (double complex)
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !  5. called by :
    !
    !      name        type  module    description
    !     ----------------------------------------------------------------
    !      func1_zhao  func. w3sic3md  function for computation of complex
    !                                  wavenumbers for waves in ice.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !   original authors: zhao and shen.
    !   this code is based on fortran code provided by hayley shen (clarkson
    !     university) to erick rogers (nrl) on april 19 2013.
    !
    !   this function does not get used in update by s. cheng.
    !     it should be removed if/when "v1" routines are removed.
    !
    !  8. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)        :: n
    double complex, intent(in) :: aa(n,n)
    double complex             :: det                       ! result
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                    :: k, i, j, is, js
    double complex             :: f, d, mat(n,n)
    double precision           :: q
    !/
    !/ ------------------------------------------------------------------- /
    mat = aa
    f = 1.0
    det = 1.0
    loop100: do k = 1,n-1
      q = 0.0
      loop10a: do i = k,n
        loop10b: do j = k,n
          if (abs(mat(i,j)).gt.q) then
            q = abs(mat(i,j))
            is = i
            js = j
          end if
        end do loop10b
      end do loop10a
      if (q+1.0.eq.1.0) then
        det = 0.0
        return
      end if
      if (is.ne.k) then
        f = -f
        loop20: do j = k,n
          d         = mat(k,j)
          mat(k,j)  = mat(is,j)
          mat(is,j) = d
        end do loop20
      end if
      if (js.ne.k) then
        f = -f
        loop30: do i = k,n
          d         = mat(i,js)
          mat(i,js) = mat(i,k)
          mat(i,k)  = d
        end do loop30
      end if
      det = det * mat(k,k)
      loop50: do i = k+1,n
        d = mat(i,k) / mat(k,k)
        loop40: do j = k+1,n
          mat(i,j) = mat(i,j) - d * mat(k,j)
        end do loop40
      end do loop50
    end do loop100
    !/
    det =  f * det * mat(n,n)
    !/
    !/ end of bsdet ------------------------------------------------------ /
    !/
  end function bsdet
  !/ ------------------------------------------------------------------- /
  function delta(x) result(dx)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           e. rogers               |
    !/                  |           s. zieger               |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-oct-2013 |
    !/                  +-----------------------------------+
    !/
    !/    06-may-2013 : origination (port from clarkson.f90)( version 4.12 )
    !/    09-oct-2013 : update to meet ww3 coding standard    (s. zieger)
    !/
    !  1. purpose :
    !
    !     this function calculates bin withs for any discretized function.
    !     may be used for numerical integration and differentiation.
    !
    !  2. method :
    !
    !  3. parameters :
    !
    !      parameter list
    !     ----------------------------------------------------------------
    !      x     r.a.      i  array type of real
    !      dx    r.a       o  bin widths if x
    !     ----------------------------------------------------------------
    !
    !  4. remarks :
    !
    !     this function does not get used in update by s. cheng.
    !     it should be removed if/when "v1" routines are removed.
    !
    !  5. called by :
    !     w3ic3wncg_v1
    !
    !  6. source code :
    !/
    implicit none
    !/
    real, intent(in)  :: x(:)
    real, allocatable :: dx(:)
    integer           :: ix, nx
    !/
    nx = size(x,1)
    allocate(dx(nx))
    dx = 0.
    !/
    do ix = 1,nx
      if (ix==1) then
        dx(ix) = (x(ix+1)-x(ix  ))
      else if (ix==nx) then
        dx(ix) = (x(ix  )-x(ix-1))
      else
        dx(ix) = (x(ix+1)-x(ix-1)) / 2.
      end if
    end do
    !/
  end function delta
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  ! start of new codes (or new variants) provided by s. cheng
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine w3ic3wncg_cheng(wn_r,wn_i,cg,ice1,ice2,ice3,ice4,dpt)
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           e. rogers               |
    !/                  |           s. zieger               |
    !/                  |           x. zhao                 |
    !/                  |           s. cheng                |
    !/                  |                        fortran 90 |
    !/                  | last update :         13-jan-2016 |
    !/                  +-----------------------------------+
    !/
    !  1. purpose :
    !
    !     calculation of complex wavenumber for waves in ice. outsourced
    !     from w3sic3 to allow update on wavenumbers and  group
    !     velocities at each time step an ice parameter is updated.
    !
    !  2. method :
    !
    !     <text here>
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       wn_r   r. a.  i/o  wave number (real part)
    !       wn_i   r. a.  i/o  wave number (imag. part=wave attenuation)
    !       cg     r. a.  i/o  group velocity
    !       ice1   real    i   thickness of ice                 [in m]
    !       ice2   real    i   effective viscosity of ice       [in m2/s]
    !       ice3   real    i   density of ice                  [in kg/m3]
    !       ice4   real    i   effective shear modulus of ice   [in pa]
    !       dpt    real    i   water depth                      [in m]
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name    type  module    description
    !     ----------------------------------------------------------------
    !     wavnu1   subr. w3dispmd   wavenumber for waves in open water.
    !     wn_cmplx func. w3sic3md   complex wavenumber for waves in ice.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name    type  module   description
    !     ----------------------------------------------------------------
    !      w3sic3  subr. w3sic3md ice source term.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !     on pre-calculation table, s. cheng says:
    !     instead of interpolation, wn_r of arbitrary hice is approximated
    !     by ic3wn_r in the look up table. ic3wn_r is related to an ice
    !     thickness in the table, which is closest to hice and less than
    !     hice
    !
    !     fix submitted by sukun march 2017:
    !         replace :
    !            i = min(int(hice/ic3_ditk)+1,itknum)
    !         with :
    !            i = min(nint(hice/ic3_ditk),itknum)
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. source code :
    !/
    !/ ------------------------------------------------------------------- /
    !/
    use w3gdatmd, only: nk,sig, ic3pars
    use w3adatmd, only: ic3wn_r, ic3wn_i, ic3cg
    use w3odatmd, only: ndse
    use w3servmd, only: extcde
    use w3dispmd, only: wavnu1
    !/
    implicit none
    !/
    real, intent(in)   :: ice1, ice2, ice3, ice4, dpt
    real, intent(inout):: wn_r(:),wn_i(:),cg(:)
    real, allocatable  :: sigma(:)
    !
    integer     :: i, i1, i2, ik, kl,ku, itknum
    complex(8)  :: wncomplex, x0,x1,x2, wnr, wnl
    real(8)     :: depth, hice, nu, dice, es_mod, rr, k_ocean, &
         cg_ocean
    real        :: ic3hilim,ic3kilim
    ic3hilim=ic3pars(10)
    ic3kilim=ic3pars(11)
    allocate(  sigma( size(cg) ) )
    sigma = 0.
    if (size(wn_r,1).eq.nk) then
      kl    = 1
      ku    = nk
      i1    = 1
      i2    = nk
      sigma = sig(1:nk)
    else if (size(wn_r,1).eq.nk+2) then
      kl    = 1
      ku    = nk+2
      i1    = 0
      i2    = nk+1
      sigma = sig(0:nk+1)
    else
      write(ndse,900)
      call extcde(3)
    end if
    depth   =   dpt     ! water depth
    hice    =   ice1    ! ice thickness
    ! optional: limit ice thickness
    hice=min(dble(ic3hilim),hice)
    nu      =   ice2    ! "effective viscosity" parameter
    dice    =   ice3    ! density of ice
    es_mod  =   ice4    ! effective shear modulus of ice
    !
    itknum  = ceiling(ic3_maxitk/ic3_ditk)
    i       = min(nint(hice/ic3_ditk),itknum)
    ! find values in pre-calculated look-up table
    ! see remarks section.
    wn_r    =   ic3wn_r(i1:i2, i)
    wn_i    =   ic3wn_i(i1:i2, i)
    cg      =   ic3cg(i1:i2, i)
    rr      =   0.01
    !/ --- check if it's shallow water situation, then it needs recalculate
    !              kr,ki,cg
    do ik   =   kl,ku
      if (wn_r(ik)*depth>4.0)then ! exit do-loop
        exit  ! assume kr is proportional to frequency
      else
        x1  = cmplx(wn_r(ik),wn_i(ik))
        x0  = x1*(1-rr)
        x2  = x1*(1+rr)
        wncomplex = cmplx_root_muller_cheng(x0,x1,x2,1, &
             dble(sigma(ik)),es_mod,nu,dice,hice,depth)
        wn_i(ik)  = real(aimag(wncomplex))  !  ki
        wn_r(ik)  = real(wncomplex)         !  kr
      endif
    enddo
    call smooth_k(wn_r,wn_i,sigma,ku-kl+ 1,0)
    ! optional : limit ki
    do ik   =   kl,ku
      wn_i(ik)  = min(wn_i(ik),ic3kilim)
    enddo
!!!     --- update group velocitiy ----
    call cginic3_cheng(cg,sigma,wn_r,ku-kl+1)
    deallocate(sigma)
900 format (/' *** wavewatch iii error in w3sic3_w3ic3wncg : '/&
         '     cannot determine bounds of wavenumber array.')
  end subroutine w3ic3wncg_cheng
  !
  !/ ------------------------------------------------------------------- /
  subroutine ic3table_cheng(ice2,ice3,ice4)
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           e. rogers               |
    !/                  |           s. zieger               |
    !/                  |           x. zhao                 |
    !/                  |           s. cheng                |
    !/                  |                        fortran 90 |
    !/                  | last update :         13-jan-2016 |
    !/                  +-----------------------------------+
    !/
    !  1. purpose :
    !
    !     it's a preprocess part to create a table of wave nubmer,
    !        attenuation and group velocity
    !     for all ice thickness in deep water situation for main
    !        computation.
    !
    !  2. method :
    !
    !  3. parameters :
    !
    !  4. subroutines used :
    !
    !      name               type  module   description
    !     ----------------------------------------------------------------
    !      cmplx_root_muller_cheng  func. w3sic3md find root for complex
    !                                         wavenumbers for waves in ice.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name              type   module   description
    !     ----------------------------------------------------------------
    !      w3wave             subr. w3wavemd
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !   original authors: cheng and shen.
    !   this code is based on fortran code provided by hayley shen (clarkson
    !     university) to erick rogers (nrl) on aug 25 2015
    !
    !   **unresolved bug** this routine should be called again if ice
    !     rheology (visc., elast.) changes (either time or space).
    !     it doesn't. we need to set calledic3table=0 if either parameter is
    !     changed.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: nk,sig
    use w3adatmd, only: ic3wn_r, ic3wn_i, ic3cg
    use w3idatmd, only: inflags2
    use w3odatmd, only: ndse
    use w3servmd, only: extcde
    use constants, only: grav
    !
    implicit none
    real       :: ice1, ice2, ice3, ice4, dpt
    integer ::  i1, i2, jitk, itknum, ik
    dpt    = 999.
    itknum = ceiling(ic3_maxitk/ic3_ditk)
    ic3wn_r(:,0) = sig**2/grav
    ic3wn_i(:,0) = 0
    do jitk = 1,itknum
      ice1 = jitk*ic3_ditk  !hice
      call ic3precalc_cheng(ic3wn_r(:,jitk), ic3wn_i(:,jitk), &
           ic3cg(:,jitk), ice1, ice2, ice3, ice4, dpt)
    enddo
    return
  end subroutine ic3table_cheng
  !/ ------------------------------------------------------------------- /
  subroutine ic3precalc_cheng(wn_r,wn_i,cg,ice1,ice2,ice3,ice4,dpt)
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           e. rogers               |
    !/                  |           s. zieger               |
    !/                  |           x. zhao                 |
    !/                  |           s. cheng                |
    !/                  |                        fortran 90 |
    !/                  | last update :         13-jan-2016 |
    !/                  +-----------------------------------+
    !/
    !  1. purpose :
    !
    !     preprocess part to create a table of wave nubmer, attenuation and
    !     group velocity for all ice thickness in deep water situation for
    !     main computation.
    !
    !  2. method :
    !
    !     calculate them use muller's method
    !
    !  3. parameters :
    !
    !  4. subroutines used :
    !
    !      name               type  module   description
    !     ----------------------------------------------------------------
    !      cmplx_root_muller_cheng  func. w3sic3md find root for complex
    !                                       wavenumbers for waves in ice.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name              type  module   description
    !     ----------------------------------------------------------------
    !      ic3table_cheng      subr. w3sic3md  create a table of kr,ki,cg
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !   original authors: cheng and shen.
    !   this code is based on fortran code provided by hayley shen (clarkson
    !     university) to erick rogers (nrl) on aug 25 2015
    !
    !   sukun cheng in reference to min(wn_i(ik),--):
    !   "this artificial limitation reduces ic3 model’s effect,
    !       though it saves some time.
    !   ki > 2.e-4  is a common truth for angular frequency larger
    !   than the value around 2 or 3 depending on other parameters."
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: nk, sig
    use w3dispmd, only: wavnu1
    use w3odatmd, only: ndse
    use w3servmd, only: extcde
    !/
    implicit none
    !/
    real, intent(inout):: wn_r(0:nk+1),wn_i(0:nk+1), cg(0:nk+1)
    real, intent(in)   :: ice1, ice2, ice3, ice4, dpt
    integer            :: ik, kl,ku,ix,num,switchid
    real               :: k_ocean,cg_ocean
    real(8)            :: kh, k_noice, depth, hice, nu, dice, &
         es_mod,sigmam1
    complex(8)         :: wncomplex, x0,x1,x2,wnm1,wnm2,wn_o
    !
    !     --- input to routine
    depth   =   dpt     ! water depth
    hice    =   ice1    ! ice thickness
    nu      =   ice2    ! "effective viscosity" parameter
    dice    =   ice3    ! density of ice
    es_mod  =   ice4    ! effective shear modulus of ice
    kl    = 0
    ku    = nk+1
    switchid = 0
    do ik = kl,ku
      call wavnu1(sig(ik),dpt,k_ocean,cg_ocean)
      k_noice = k_ocean
      !     --- calculate complex wavenumber ------------------------------- /
      if(ik<kl+2)then
        wnm1     = cmplx(0.,0.)
        wnm2     = cmplx(0.,0.)
        sigmam1  = 0.0
      else
        wnm1     = cmplx(wn_r(ik-1),wn_i(ik-1))
        wnm2     = cmplx(wn_r(ik-2),wn_i(ik-2))
        sigmam1  = sig(ik-1)
      endif
      wn_o = cmplx(k_noice,0.0)
      call wn_precalc_cheng(wncomplex,dble(sig(ik)),dble(sigmam1),wn_o, &
           wnm1,wnm2,es_mod,nu,dice,hice,depth,switchid,ik,ku)
      !     --- output to routine
      wn_r(ik)  = real(wncomplex)         !  kr
      wn_i(ik)  = imag(wncomplex)         !  ki
    enddo
    call smooth_k(wn_r,wn_i,sig,ku-kl+1,switchid)
    call cginic3_cheng(cg,sig,wn_r,ku-kl+1)
    !/
  end subroutine ic3precalc_cheng
  !/ -------------------------------------------------------------------/
  subroutine wn_precalc_cheng(wn,sigma,sigmam1,wn_o,wnm1,wnm2,es_mod, &
       nu,dice,hice,depth,switchid,ik,ku)
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           s. cheng                |
    !/                  |           e. rogers               |
    !/                  |           s. zieger               |
    !/                  |           x. zhao                 |
    !/                  |                        fortran 90 |
    !/                  | last update :         13-jan-2016 |
    !/                  +-----------------------------------+
    !/
    !  1. purpose :
    !
    !     calculate complex wavenumber for waves in ice.
    !
    !  2. method :
    !
    !     wang and shen (jgr 2010)
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       wn     cmplx  o  wave number (imag. part=wave attenuation)
    !       sigma  real   i  wave angular frequency           [in rad]
    !       wn_o   real   i  wave number (open water)
    !       es     real   i  effective shear modulus of ice   [in pa]
    !       nu     real   i  effective viscosity of ice       [in m2/s]
    !       dice   real   i  density of ice                  [in kg/m3]
    !       hice   real   i  thickness of ice                 [in m]
    !       depth  real   i  water depth                      [in m]
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name               type  module   description
    !     ----------------------------------------------------------------
    !      cmplx_root_muller_cheng  func. w3sic3md find root for complex
    !                                         wavenumbers for waves in ice.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      ic3precalc_cheng    xxx  xxx  xxxx
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !   updated authors: cheng and shen.
    !   this code is based on fortran code provided by hayley shen (clarkson
    !     university) to erick rogers (nrl) on aug 25 2015
    !   original authors: zhao and shen.
    !   this code is based on fortran code provided by hayley shen (clarkson
    !     university) to erick rogers (nrl) on april 19 2013.
    !
    !   hayley shen says,
    !     we have determined that it may not be necessary to use curve
    !     fitting or lookup tables to get the group velocity and the
    !     attenuation coefficient. attached is a short report with some
    !     sample numerical solutions. to implement the viscoelastic model,
    !     there are 4 fortran programs. according to xin zhao, the graduate
    !     student, it is very fast to find roots. i suggest that perhaps you
    !     try the pure viscous case by setting g=0 to start with. nu can be
    !     set at 0.05*ice concentration (m^2/s) to begin with, because for
    !     grease ice newyear’s data showed nu to be about 0.02-0.03 m^2/s.
    !     by setting g=0 in you get exactly the keller model for pure
    !     viscous layer.
    !
    !   this routine provides the initial guess according to the parameters
    !   of the present case. t>10s use open water, t<10s cases, calculate
    !   t=10s first using open water as the initial guess.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants, only: tpi
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real(8)       :: sigma,sigmam1,es_mod,nu,dice,hice,depth
    complex(8)    :: wn_o, wnm1, wnm2, wn0, wn1,wn2
    complex(8),intent(out) :: wn ! result
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer       :: i,ix,num,switchid,ik,ku
    real(8)       :: rr, r2, eps, sigma0, kappa, dis
    complex(8)    :: x0, x1, x2, kp, ks, gv
    !/
    !/ ------------------------------------------------------------------- /
    !   compute shear wave and pressure wave modes
    gv = cmplx(es_mod,-sigma*nu*dice)
    kp = sigma/sqrt(4*gv/dice)
    ks = 2*kp
    !   rr,r2 are empirical coefficients
    rr  = 0.2
    r2  = 4
    eps = 1.d-10   ! assuming variable =0, if it is less than this value
    !   compute root 1
    !   initial guesses from wave number of open water
    !
    x1  = wn_o    ! initial guess
    x0  = x1*(1-rr)
    x2  = x1*(1+rr)
    wn1 = cmplx_root_muller_cheng(x0,x1,x2,1,sigma, &
         es_mod,nu,dice,hice,depth)
    !   if root finder failed, or found shear wave mode,
    !   redo searching using simplified dispersion relation form, index 2
    if (real(wn1)<0.or.abs(wn1-ks)/abs(ks)<0.03 .or. &
         abs(wn1-kp)/abs(kp)<0.03.and.es_mod>1.e7*nu)then
      wn1 = cmplx_root_muller_cheng(x0,x1,x2,2,sigma, &
           es_mod,nu,dice,hice,depth)
    endif
    !   similar as wn1, but search with opposite order of inital guesses
    wn2 = cmplx_root_muller_cheng(x2,x1,x0,1,sigma, &
         es_mod,nu,dice,hice,depth)
    if (real(wn2)<0.or.abs(wn2-ks)/abs(ks)<0.03)then
      wn2 = cmplx_root_muller_cheng(x2,x1,x0,2,sigma, &
           es_mod,nu,dice,hice,depth)
    endif
    if(abs(real(wn1)-real(wn_o))<abs(real(wn2)-real(wn_o)))then
      wn0 = wn1
    else
      wn0 = wn2
    endif
    if(sigmam1==0.)then
      wn = wn0
    else
      !      compute root 2
      !      calculate the other wave number based on last frequency
      !      in the frequency array
      r2    = max(abs(wnm1-wnm2)/abs(wnm2), abs((sigma-sigmam1)/sigma))
      do i  = 1,7,2
        if(i<3.and.sigma>4)then
          cycle
        endif
        num = 2**(i-1)
        rr  = min(max(r2/real(num,8),0.001d0),0.5d0)
        x1  = wnm1
        do ix  = 1,num
          x0 = x1*(1-rr)
          x2 = x1*(1+rr)
          sigma0 = sigmam1 + (1.0*ix)/num*(sigma-sigmam1)
          wn = cmplx_root_muller_cheng(x0,x1,x2,1,sigma0, &
               es_mod,nu,dice,hice,depth)
          if(real(wn)<0)then   ! try another searching direction
            wn = cmplx_root_muller_cheng(x2,x1,x0,1,sigma0, &
                 es_mod,nu,dice,hice,depth)
          endif
          if(real(wn)<0)then   ! try another dispersion relation form
            wn = cmplx_root_muller_cheng(x0,x1,x2,3,sigma0, &
                 es_mod,nu,dice,hice,depth)
          endif
          kp = sigma0/sqrt(4*gv/dice)
          ks = 2*kp
          !               set 3 means simple dispersion relation form
          if(abs(wn-ks)/abs(ks)<0.03.or.real(wn)<0)then
            wn = cmplx_root_muller_cheng(x0,x1,x2,2,sigma0, &
                 es_mod,nu,dice,hice,depth)
          endif
          x1 = wn
          if( real(wn)<0.99*real(wnm1).or. abs(wn-wn0)>eps .and. &
               (abs(x1-wn)/abs(wn)>0.3 .or.                  &
               imag(wn)/(imag(x1)+eps)>10.or. real(wn)<0))then
            exit  ! redo with smaller intervals
          endif
          x0 = x1
          x1 = wn
        enddo !            do ix  = 1,num
        if(ix==num+1)then
          exit
        endif
        wn = x1  !/ --- if exit of inner loop: give an approximate wn
      enddo !         do i  = 1,7,2
      ! part 2
      ! assume found two roots, choose from the two condidates.
      kp = sigma/sqrt(4*gv/dice)
      if(real(wn0)>0.and.imag(wn0)>=0.and.abs(wn - wn0)>eps)then
        !do switch at last 3 points is not worth numerically.
        !for v>5.e-2, it is low chance to be wrong at the last 3 points
        ! we suppose to use two mode in the future
        if(nu>0.and.ik>=ku-1.or.   &
             nu>0.and.switchid/=0)then
          ! assume one mode switch for general viscoelastic model
        else
          dis   = abs(real(wn)-real(wn_o))/abs(real(wn0)-real(wn_o))
          kappa = (imag(wn)+ eps)/(imag(wn0) + eps)
          ! wn0 has smaller attenuation and closer to k0
          if ((dis >= 1 .and. kappa>=1 .and.  &
               imag(wn0)>=0.1*imag(wnm1).and. &
               abs(wn-kp)<abs(wn0-kp)) .or.  &
               ! wn0 has larger attenuation and closer to k0
               ( dis>=1 .and. kappa<1 .and. &
               ((kappa> 0.2 .and. imag(wn0)/real(wn0)<0.5).or. &
               abs(real(wn)-real(kp))<abs(real(wn0)-real(kp)))).or.&
               ! wn0 has smaller attenuation and farther to k0,
               ! wn0 could be wrong at high g
               ! (kappa>1 .and. dis<1 .and. dis>  0.8 ))then
               ( kappa>1 .and. dis<1 .and. &
               abs(real(wn)-real(wnm1))>   &
               abs(real(wn0)-real(wnm1)) ))then
            wn = wn0
            switchid = ik
            ! wn0 has lager attenuation and farther to k0
          elseif(dis<1 .and. kappa<1) then
            ! keep wn without change
          endif !  if ((dis >= 1 .and. kappa>=1 .and.  &
        endif !   if(nu>0.and.ik>=ku-2.or.   &
        ! choose dominant mode is farther than pressure wave.
        !but it doens't work for high viscosity.
        if (abs(wn-kp)/abs(kp)<0.03) then
          wn = wn0
          switchid = ik
        endif
        !if (real(wn)<=real(wnm1))then
        !    wn = wn0
        !    switchid = ik
        !endif
        if (real(wn0)>real(wnm1).and.real(wn0)<real(wn).and. &
             abs(imag(wn0)-imag(wnm1))<abs(imag(wn)-imag(wnm1)))then
          ! mainly for pure elastic case has multiple branchs
          wn = wn0
          switchid = ik
        endif
      endif ! if(real(wn0)>0.and.imag(wn0)>=0.and.abs(wn - wn0)....
    endif   !      if(sigmam1==0.)then
    if(real(wn)<0)then
      print*, "muller method failed, es_mod,nu,hice:",es_mod,nu,hice
    endif
    return
  end subroutine wn_precalc_cheng
  !/ -----------------------------------------------------------------
  !/
  subroutine cginic3_cheng(cg,sigma,wn_r,n)
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           e. rogers               |
    !/                  |           s. zieger               |
    !/                  |           x. zhao                 |
    !/                  |           s. cheng                |
    !/                  |                        fortran 90 |
    !/                  | last update :         13-jan-2016 |
    !/                  +-----------------------------------+
    !  1. purpose :
    !
    !     calculate group velocity in ic3 model
    !
    !  2. method :
    !
    !     finite differece
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       cg      r. a.  i/o  group velocity
    !       simga   r. a.  angular frequency
    !       wn_r    r. a.  wave number
    !       n       int.   array size
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      none.
    !
    !  5. called by :
    !
    !      name             type  module   description
    !     ----------------------------------------------------------------
    !      ic3precalc_cheng    subr. w3sic3md create table of kr,ki,cg for
    !                 deep water
    !      w3ic3wncg_cheng     subr. w3sic3md calculate kr,ki,cg
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !      smooth function is used due to jump problem when wave modes
    !             switch
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. source code :
    !/
    !/ ------------------------------------------------------------------- /
    !/
    real, intent(in)  :: sigma(1:n),wn_r(1:n)
    real, intent(out) :: cg(1:n)
    integer           :: n
    !/ local variables
    integer           :: ik, m
    real              :: cg1,cg2,cg3,cg0(1:n)
    !/
    ik = 1
    cg(ik)=(sigma(ik+1)-sigma(ik))/(wn_r(ik+1)-wn_r(ik))
    ik = n
    cg(ik)=(sigma(ik)-sigma(ik-1))/(wn_r(ik)-wn_r(ik-1))
    do ik = 2,n-1
      cg1 = (sigma(ik)-sigma(ik-1))/(wn_r(ik)-wn_r(ik-1))
      cg2 = (sigma(ik+1)-sigma(ik))/(wn_r(ik+1)-wn_r(ik))
      cg(ik) = 2.0/(1./cg1 + 1./cg2)
    end do
    return
  end subroutine cginic3_cheng
  !/ ------------------------------------------------------------------- /
  ! numerically smooth wn_r and wn_i by linear interpolation
  subroutine smooth_k(wn_r,wn_i,sigma,n,switchid)
    real, intent(in)  :: sigma(n)
    real              :: wn_r(n), wn_i(n),diff(n),removeid(n)
    integer           :: n,i,j,switchid
    !
    diff = 0
    ! remove kr in mode switch zone,
    ! if it is a local extremum or suddenly increasing
    do j = 1,3 ! 3 times to guarantee wavenumber increases monotonically
      removeid = 0
      do i = 2,n
        diff(i) = wn_r(i) - wn_r(i-1)
      enddo
      do i = 3,n
        if(diff(i)<=0.or.diff(i)>3*diff(i-1).or.switchid==i)then
          removeid(i) = 1
          removeid(i-1) = 1
        endif
      enddo
      ! fill removed location with kr,ki by interpolation
      do i = 2,n-1
        if (removeid(i) ==1) then
          wn_r(i) = wn_r(i-1) + (wn_r(i+1)-wn_r(i-1))/    &
               (sigma(i+1)-sigma(i-1))*(sigma(i)-sigma(i-1))
          wn_i(i) = wn_i(i-1) + (wn_i(i+1)-wn_i(i-1))/    &
               (sigma(i+1)-sigma(i-1))*(sigma(i)-sigma(i-1))
        endif
      enddo
    enddo
    ! mode switch upward at the last frequencies
    if (diff(n)>3*diff(n-1))then
      i = n
      wn_r(i) = wn_r(i-1) + (wn_r(i-1)-wn_r(i-2))/   &
           (sigma(i-1)-sigma(i-2))*(sigma(i)-sigma(i-1))
      wn_i(i) = wn_i(i-1) + (wn_i(i-1)-wn_i(i-2))/   &
           (sigma(i-1)-sigma(i-2))*(sigma(i)-sigma(i-1))
    endif
    return
  end subroutine smooth_k
  !/ ------------------------------------------------------------------- /
  function cmplx_root_muller_cheng(x0, x1, x2, judge,  &
       sigma,es,nu,dice,hice,depth )           result(p3)
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           e. rogers               |
    !/                  |           s. zieger               |
    !/                  |           x. zhao                 |
    !/                  |           s. cheng                |
    !/                  |                        fortran 90 |
    !/                  | last update :         13-jan-2016 |
    !/                  +-----------------------------------+
    !/
    !/    06-may-2013 : origination (port from clarkson.f90)( version 4.10 )
    !/                                                        (e. rogers)
    !/    09-oct-2013 : update to meet ww3 coding standard    (s. zieger)
    !/    30-oct-2013 : clarkson.f90 update added             (s. zieger)
    !/
    !  1. purpose :
    !
    !     find root.
    !
    !  2. method :
    !
    !     muller method for complex equations is a recursive approximation
    !     with initial guess x0, x1, and x2. to the initial guesses a
    !     quadratic parabola is fitted.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       p3    cmplx dbl o  approximation for the root problem
    !       x0    cmplx dbl i  initial guess variable
    !       x1    cmplx dbl i  initial guess variable
    !       x2    cmplx dbl i  initial guess variable
    !       sigma double    i  wave angular frequency
    !       es    double    i  effective shear modulus of ice
    !       nu    double    i  effective viscosity of ice       [in m2/s]
    !       dice  double    i  density of ice                   [in kg/m3]
    !       hice  double    i  thickness of ice                 [in m]
    !       depth double    i  water depth                      [in m]
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name               type  module   description
    !     ----------------------------------------------------------------
    !      f_zhao_cheng       xxxxx xxxx     xxxx
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !     wn_precalc_cheng xxxxx    xxxx
    !     w3ic3wncg_cheng  xxxxx    xxxx
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !   original authors: zhao and shen.
    !   this code is based on fortran code provided by hayley shen (clarkson
    !     university) to erick rogers (nrl) on april 19 2013.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3odatmd, only: ndse
    use w3servmd, only: extcde
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    complex(8)            :: p3                     ! result
    complex(8), intent(in):: x0,x1,x2
    real(8), intent(in)   :: sigma,es,nu,dice,hice,depth
    integer, intent(in)   :: judge
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer               :: i
    integer, parameter    :: imax = 200
    real(8)               :: dlta,epsi
    complex(8)            :: p0,p1,p2
    complex(8)            :: y0,y1,y2,y3
    complex(8)            :: a,b,c,q,disc,den1,den2
    !/
    !/ ------------------------------------------------------------------- /
    p0      =   x0
    p1      =   x1
    p2      =   x2
    p3      = 0.d0
    epsi    = 1.d-8
    dlta    = 1.d-8   ! relax it may cause error, too rigorous is not good neither.
    y0      = f_zhao_cheng(judge,p0,sigma,es,nu,dice,hice,depth)
    y1      = f_zhao_cheng(judge,p1,sigma,es,nu,dice,hice,depth)
    y2      = f_zhao_cheng(judge,p2,sigma,es,nu,dice,hice,depth)
    do i = 1,imax
      q = (p2 - p1) / (p1 - p0)
      a = q * y2 - q * (1.d0+q) * y1 + q**2.d0 * y0
      b = (2.d0 * q + 1.d0) * y2 - (1.d0 + q)**2.d0 * y1 &
           + q**2.d0 * y0
      c = (1.d0 + q) * y2
      if ( abs(a).ne.0.d0 ) then
        disc = b**2.d0 - 4.d0 * a * c;
        den1 = ( b + sqrt ( disc ) )
        den2 = ( b - sqrt ( disc ) )
        if ( abs ( den1 ) .lt. abs ( den2 ) )then
          p3 = p2 - (p2 - p1) * (2.d0 * c / den2)
        else
          p3 = p2 - (p2 - p1) * (2.d0 * c / den1)
        endif
      else
        if ( abs(b) .ne. 0.d0 )then
          p3 = p2 - (p2 - p1) * (c / b)
        else
          p3 = p2
          return
        endif
      endif
      if (imag(p3).lt.0)then
        p3 = real(p3) - cmplx(0.,1.)*imag(p3)
      endif
      if (real(p3).lt.0)then
        p3 = -real(p3) + cmplx(0.,1.)*imag(p3)
      endif
      if(nu==0)then
        p3 = cmplx(real(p3),0)
      endif
      y3 = f_zhao_cheng(judge,p3,sigma,es,nu,dice,hice,depth);
      if ( abs(p3-p2).lt.dlta .and. abs(y3).lt.epsi ) then
        ! exit before finding a true root,result may not be accurate
        return
      endif
      p0 = p1
      p1 = p2
      p2 = p3
      y0 = y1
      y1 = y2
      y2 = y3
    enddo
    p3 = cmplx(-100.,0)
    return
  end function cmplx_root_muller_cheng
  !/ ------------------------------------------------------------------- /
  !/
  function f_zhao_cheng(judge,x,sigma,es,nu,dice,hice,depth) &
       result(fzhao)
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           e. rogers               |
    !/                  |           s. zieger               |
    !/                  |           x. zhao                 |
    !/                  |           s. cheng                |
    !/                  |                        fortran 90 |
    !/                  | last update :         13-jan-2016 |
    !/                  +-----------------------------------+
    !/
    !/    06-may-2013 : origination (port from clarkson.f90)( version 4.10 )
    !/                                                        (e. rogers)
    !/    09-oct-2013 : update to meet ww3 coding standard    (s. zieger)
    !/    30-oct-2013 : clarkson.f90 update added             (s. zieger)
    !/
    !  1. purpose :
    !
    !     decide whether to call sub-function.
    !
    !  2. method :
    !
    !     decide based on value of integer "judge"
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !      fzhao compl8  o    result (double complex)
    !      x     cmplx8  i    approximate result (double complex)
    !      judge integr  i    switch variable
    !      sigma double  i    wave angular frequency
    !      es    double  i    effective shear modulus
    !      nu    double  i    effective viscosity parameter
    !      dice  double  i    density of ice
    !      hice  double  i    thickness of ice
    !      depth double  i    water depth
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name        type  module   description
    !     ----------------------------------------------------------------
    !      drfun_dble  func. w3sic3md function to find root with double
    !      precision.
    !      drfun_quad  func. w3sic3md function to find root with quadruple
    !      precision.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name               type  module   description
    !     ----------------------------------------------------------------
    !      cmplx_root_muller_cheng  func. w3sic3md find root for complex
    !                                         wavenumbers for waves in ice.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !   updated authors: cheng and shen.
    !   this code is based on fortran code provided by hayley shen (clarkson
    !     university) to erick rogers (nrl) on aug 25 2015
    !   original authors: zhao and shen.
    !   this code is based on fortran code provided by hayley shen (clarkson
    !     university) to erick rogers (nrl) on april 19 2013.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    ! 10. source code :
    !/
    !/ ------------------------------------------------------------------- /
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer,intent(in)           :: judge
    double precision, intent(in) :: sigma,es,nu,dice,hice,depth
    double complex, intent(in)   :: x
    double complex               :: fzhao  ! result
    !/
    !/ ------------------------------------------------------------------- /
    if (judge >0) then
      fzhao = drfun_dble_cheng(x,sigma,es,nu,dice,hice,depth,judge)
    elseif(judge ==0)then
      fzhao = drfun_quad_cheng(x,sigma,es,nu,dice,hice,depth)
    endif
    !
  end function f_zhao_cheng
  !/ ------------------------------------------------------------------- /
  !/
  function drfun_dble_cheng(wn,sigma,es,nu,dice,hice,depth,judge) &
       result(func1)
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           e. rogers               |
    !/                  |           s. zieger               |
    !/                  |           x. zhao                 |
    !/                  |           s. cheng                |
    !/                  |                        fortran 90 |
    !/                  | last update :         13-jan-2016 |
    !/                  +-----------------------------------+
    !/
    !/    06-may-2013 : origination (port from clarkson.f90)( version 4.10 )
    !/                                                        (e. rogers)
    !/    09-oct-2013 : update to meet ww3 coding standard    (s. zieger)
    !/    30-oct-2013 : clarkson.f90 update added             (s. zieger)
    !/
    !  1. purpose :
    !
    !     return dispersion relation function value for root finding
    !
    !  2. method :
    !
    !     function based on dispersion relation derived by wang and shen
    !     2010
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !      func1 cmplx dbl o  result (complex(8))
    !      wn    cmplx dbl i  wavenumber (complex(8))
    !      w     real  dbl i  wave angular frequency
    !      es    real  dbl i  effective shear modulus on ice
    !      nu    real  dbl i  effective viscosity
    !      dice  real  dbl i  density of ice
    !      hice  real  dbl i  thickness of ice
    !      depth real  dbl i  water depth
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name    type  module    description
    !     ----------------------------------------------------------------
    !       none.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name    type  module   description
    !     ----------------------------------------------------------------
    !      f_zhao_cheng   xxx    xxxx      xxxxx
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !   updated authors: cheng and shen.
    !   this code is based on fortran code provided by hayley shen (clarkson
    !     university) to erick rogers (nrl) on aug 25 2015
    !   original authors: zhao and shen.
    !   this code is based on fortran code provided by hayley shen (clarkson
    !     university) to erick rogers (nrl) on april 19 2013.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    ! 10. source code :
    !/
    !/ ------------------------------------------------------------------- /
    use constants, only: grav, dwat
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    complex(8), intent(in)   :: wn
    real(8), intent(in)      :: sigma, es, nu, dice, hice, depth
    complex(8)               :: func1,aa(4,4)                   ! result
    integer                  :: judge
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    complex(8)   :: ve,alpha,n,m,l,sk,ck,sa,ca,th,thh,temp,j1,j2
    !/
    !/ ------------------------------------------------------------------- /
    ve    = cmplx( nu, es/dice/sigma )
    alpha = sqrt ( wn**2. - sigma/ve * cmplx(0.,1.d0) )
    n     = sigma + 2. * ve * wn**2. * cmplx(0.,1.d0)
    temp  = exp(wn*hice)
    sk    = (temp - 1.d0/temp)/2.d0
    ck    = (temp + 1.d0/temp)/2.d0
    temp  = exp(alpha*hice)
    sa    = (temp - 1.d0/temp)/2.d0
    ca    = (temp + 1.d0/temp)/2.d0
    !
    temp  = (wn*depth)
    if ( real(temp).lt.18.d0 ) then
      temp = exp(temp)
      th = (temp - 1./temp)/(temp + 1./temp)
    else
      th = 1.d0
    endif
    !      judge==3 is not used yet
    if ((es>=1.e5.and.judge/=2).or.judge==3) then
      l     = 2 * wn * alpha * sigma * ve
      m     = (dble(dwat)/dice - 1) * dble(grav) * wn             &
           - dble(dwat) / dice * sigma**2 / th
      aa(1,1) = 0.
      aa(1,2) = 2 * cmplx(0.,1.) * wn**2.
      aa(1,3) = alpha**2. + wn**2.
      aa(1,4) = 0.
      !
      aa(2,1) = n * sigma
      aa(2,2) = -wn * dble(grav)
      aa(2,3) = cmplx(0.,1.) * wn * dble(grav)
      aa(2,4) = l
      !
      aa(3,1) = -2. * cmplx(0.,1.) * wn**2. * sk
      aa(3,2) =  2. * cmplx(0.,1.) * wn**2. * ck
      aa(3,3) =  (alpha**2. + wn**2.) * ca
      aa(3,4) = -(alpha**2. + wn**2.) * sa
      !
      aa(4,1) =   n * sigma * ck - m * sk
      aa(4,2) = - n * sigma * sk + m * ck
      aa(4,3) = -cmplx(0.,1.) * m * ca - l * sa
      aa(4,4) =  cmplx(0.,1.) * m * sa + l * ca
      !
      func1   = bsdet(aa,4)
    else
      j1    = dice/dble(dwat)*(wn**2.*dble(grav)**2.*sk*sa - (n**4. &
           + 16.* ve**4.*wn**6.*alpha**2.)*sk*sa - 8. &
           *wn**3.*alpha*ve**2.*n**2.*(ck*ca-1.))
      j2    = (4.*wn**3.*alpha*ve**2.*sk*ca+n**2.*sa*ck &
           -dble(grav)*wn*sk*sa)
      if (judge==2)then
        func1 = (sigma**2. - th*wn*dble(grav)) - th*j1/(j2+1.e-20)
      elseif (judge==1)then
        func1 = (sigma**2. - th*wn*dble(grav))*j2 - th*j1
      endif
    endif
    !/
  end function drfun_dble_cheng
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  !/
  function drfun_quad_cheng(wn,sigma,es,nu,dice,hice,depth) &
       result(func1)
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           e. rogers               |
    !/                  |           s. zieger               |
    !/                  |           x. zhao                 |
    !/                  |           s. cheng                |
    !/                  |                        fortran 90 |
    !/                  | last update :         13-jan-2016 |
    !/                  +-----------------------------------+
    !/
    !/    06-may-2013 : origination (port from clarkson.f90)( version 4.10 )
    !/                                                        (e. rogers)
    !/    09-oct-2013 : update to meet ww3 coding standard    (s. zieger)
    !/    30-oct-2013 : clarkson.f90 update added             (s. zieger)
    !/
    !  1. purpose :
    !
    !     return dispersion relation function value for root finding
    !
    !  2. method :
    !
    !     use quadruple precision for computation
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !      func1 cmplx dbl o  result (complex(8))
    !      wn    cmplx dbl i  wavenumber (complex(8))
    !      w     real  dbl i  wave angular frequency
    !      es    real  dbl i  effective shear modulus on ice
    !      nu    real  dbl i  effective viscosity
    !      dice  real  dbl i  density of ice
    !      hice  real  dbl i  thickness of ice
    !      depth real  dbl i  water depth
    !
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name    type  module    description
    !     ----------------------------------------------------------------
    !      none.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name    type  module   description
    !     ----------------------------------------------------------------
    !      f_zhao_cheng   xxx    xxxx      xxxxx
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !   updated authors: cheng and shen.
    !   this code is based on fortran code provided by hayley shen (clarkson
    !     university) to erick rogers (nrl) on aug 25 2015
    !   original authors: zhao and shen.
    !   this code is based on fortran code provided by hayley shen (clarkson
    !     university) to erick rogers (nrl) on april 19 2013.
    !   er: s. cheng had "complex(16)" for the local parameters. this is not
    !     supported by my compiler (gfortran on linux machine), so i changed
    !     it to "complex(8)"
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    ! 10. source code :
    !/
    !/ ------------------------------------------------------------------- /
    use constants, only: grav, dwat
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    complex(8), intent(in)   :: wn
    real(8), intent(in)      :: sigma, es, nu, dice, hice, depth
    complex(8)               :: func1                   ! result
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    complex(8)   :: ve,alpha,n,m,l,sk,ck,sa,ca,th,thh,temp,j1,j2
    !/
    !/ ------------------------------------------------------------------- /
    ve    = cmplx( nu, es/dice/sigma )
    alpha = sqrt ( wn**2. - sigma/ve * cmplx(0.,1.d0) )
    n     = sigma + 2. * ve * wn**2. * cmplx(0.,1.d0)
    temp  = exp(wn*hice)
    sk    = (temp - 1.d0/temp)/2.d0
    ck    = (temp + 1.d0/temp)/2.d0
    temp  = exp(alpha*hice)
    sa    = (temp - 1.d0/temp)/2.d0
    ca    = (temp + 1.d0/temp)/2.d0
    !
    temp  = (wn*depth)
    if ( real(temp).lt.18.d0 ) then
      temp = exp(temp)
      th = (temp - 1./temp)/(temp + 1./temp)
    else
      th = 1.d0
    endif
    !
    j1    = dice/dble(dwat)*(wn**2.*dble(grav)**2.*sk*sa - (n**4. &
         + 16.* ve**4.*wn**6.*alpha**2.)*sk*sa - 8. &
         *wn**3.*alpha*ve**2.*n**2.*(ck*ca-1.))
    j2    = (4.*wn**3.*alpha*ve**2.*sk*ca+n**2.*sa*ck &
         -dble(grav)*wn*sk*sa)
    func1 = (sigma**2. - th*wn*dble(grav))*j2 - th*j1
    !/
  end function drfun_quad_cheng
  !/ ------------------------------------------------------------------- /
  ! end of new codes (or new variants) provided by s. cheng
  !/ ------------------------------------------------------------------- /
  !/
  !/ end of module w3sic3md -------------------------------------------- /
  !/
end module w3sic3md
