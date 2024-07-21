!> @file w3psmcmd.f90
!> @brief spherical multiple-cell (smc) grid
!>
!> @author jian-guo li
!> @date 23 mar 2020
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
!> @brief spherical multiple-cell (smc) grid routines.
!>
!> @details bundles routines for smc advection (uno2) and diffusion
!>  schemes in single module, including great circile turning and
!>  refraction rotation schemes.
!>
!> @author jian-guo li
!> @date 23 mar 2020
!>
module w3psmcmd
  !/
  !/                  +------------------------------------+
  !/                  | spherical multiple-cell (smc) grid |
  !/                  | adv, gct, rfr, dif subroutines.    |
  !/                  |           jian-guo li              |
  !/                  | first created:     8 nov 2010      |
  !/                  | last modified:    23 mar 2020      |
  !/                  +------------------------------------+
  !/
  !/    08-nov-2010 : coding started by adapting w3pro2md.ftn.
  !/    18-nov-2010 : refraction and gct by rotation and k-shift.
  !/    12-apr-2011 : restore x-advective flux for intermediate update.
  !/     3-jun-2011 : new refraction formulation using cg only.
  !/     8-jun-2011 : optimise classic refraction formulation.
  !/    16-jun-2011 : add refraction limter to gradient direction.
  !/     1-jul-2011 : new refraction using cg and gradient limiter.
  !/    28-jul-2011 : finalise with old refraction scheme and gradient limiter.
  !/     4-nov-2011 : separate x and y obstruction coefficients.
  !/     5-jan-2012 : update to multi-resolution smc grid with sub-time-steps.
  !/     2-feb-2012 : separate single- and multi-resolution advection.
  !/     6-mar-2012 : tidy up code and minor adjustments, clatf.
  !/    12-mar-2012 : remove net flux bug and optimise upstream code.
  !/    16-jan-2013 : adapted for version 4.08, removing facx/y.
  !/    16-sep-2013 : add arctic part for smc grid in ww3 v4.11
  !/     3-jan-2014 : remove bug in smcdhxy for au/v as cell size.
  !/     7-jan-2014 : remove bug in smcgtcrfr for k definition.
  !/    28-jan-2014 : move arctic boundary condition update out.
  !/    18-aug-2015 : new gradient, average and 3rd order advection subs.
  !/     3-sep-2015 : uno3 advection scheme by logical option funo3.
  !/    14-sep-2015 : modify dhdx/y for arctic part refraction term.
  !/     8-aug-2017 : update smcgradn for 0 or 1 boundary conditions.
  !/     9-jan-2018 : parallelization by adding openmp directives.
  !/    19-feb-2020 : additions for omp bit-reproducability (c.bunney)
  !/    23-mar-2020 : add extra parenthese for single atomic line update.
  !/    22-oct-2020 : two new subs for lat-lon points mapping to cells.
  !/
  !  1. purpose :
  !
  !     bundles routines for smc advection (uno2) and diffusion schemes in
  !     single module, including gct and refraction rotation schemes.
  !
  !  2. variables and types :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      trnmin    r.p.  private   minimum transparancy for local
  !     ----------------------------------------------------------------
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3psmc    subr. public   spatial propagation on smc grid.
  !      w3krtn    subr. public   spectral modification by gct and refraction.
  !      smcxuno2  subr. public   irregular grid mid-flux on u-faces by uno2.
  !      smcyuno2  subr. public   irregular grid mid-flux on v-faces by uno2.
  !      smcxuno2r subr. public   regular grid mid-flux on u-faces by uno2.
  !      smcyuno2r subr. public   regular grid mid-flux on v-faces by uno2.
  !      smckuno2  subr. public   shift in k-space due to refraction by uno2.
  !      smcxuno3  subr. public   irregular grid mid-flux on u-faces by uno3.
  !      smcyuno3  subr. public   irregular grid mid-flux on v-faces by uno3.
  !      smcxuno3r subr. public   regular grid 3rd order u-mid-flux by uno3.
  !      smcyuno3r subr. public   regular grid 3rd order v-mid-flux by uno3.
  !      smcgtcrfr subr. public   refraction and gct rotation in theta.
  !      smcdhxy   subr. public   evaluate depth gradient and refraction limiter.
  !      smcgradn  subr. public   evaluate local gradient for sea points.
  !      smcaverg  subr. public   numerical 1-2-1 average of sea point field.
  !      w3gathsmc w3scatsmc      gather and scatter spectral components.
  !      w3smcell  subr. public   calculate cell centre lat-lon for given ids.
  !      w3smcgmp  subr. public   map lat-lon points to smc grid cells.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      strace    subr. w3servmd subroutine tracing.
  !      w3acturn  subr. w3servmd subroutine rotating action spectrum.
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !
  !  6. switches :
  !
  !       !/mgp   correct for motion of grid.
  !       !/s     enable subroutine tracing.
  !       !/tn    enable test output.
  !
  !  7. source code :
  !
  !/ ------------------------------------------------------------------- /
  !/
  !/ use omp_lib for openmp functions if switched on.   jgli10jan2018
  !$       use omp_lib
  !/
  !/ public variables
  !/
  public
  !/
  !/ private data !/
  real, private, parameter:: trnmin = 0.95 !< minimum transparancy for local
  !/
contains
  !/ ------------------------------------------------------------------- /
  !> @brief propagation in phyiscal space for a given spectral component
  !>
  !> @details unstructured smc grid, point-oriented face and cell loops.
  !>  uno2 advection scheme and isotropic ftcs diffusion scheme
  !>
  !> @param[in]     isp     number of spectral bin (ik-1)*nth+ith
  !> @param[in]     dtg     total time step.
  !> @param[inout]  vq      field to propagate.
  !>
  !> @author jian-guo li
  !> @date 18 apr 2018
  !>
  subroutine w3psmc ( isp, dtg, vq )
    !/
    !/                  +------------------------------------+
    !/                  | spherical multiple-cell (smc) grid |
    !/                  | advection and diffusion sub.       |
    !/                  | first created:   jg li  8 nov 2010 |
    !/                  | last modified:   jg li 18 apr 2018 |
    !/                  +------------------------------------+
    !/
    !/    08-nov-2010 : origination.                jgli    ( version 1.00 )
    !/    16-dec-2010 : check u/v cfl values.       jgli    ( version 1.10 )
    !/    18-mar-2011 : check mpi communication.    jgli    ( version 1.20 )
    !/    16-may-2011 : tidy up diagnosis lines.    jgli    ( version 1.30 )
    !/     4 nov-2011 : separate x and y obstruc.   jgli    ( version 1.40 )
    !/     5 jan-2012 : multi-resolution smc grid.  jgli    ( version 1.50 )
    !/     2 feb-2012 : separate single multi adv.  jgli    ( version 1.60 )
    !/     6 mar-2012 : minor adjustments of clatf. jgli    ( version 1.70 )
    !/    12 feb-2012 : remove net flux bug.        jgli    ( version 1.80 )
    !/    16 sep-2013 : add arctic part.            jgli    ( version 2.00 )
    !/     3 sep-2015 : gradient, uno3 and average. jgli    ( version 2.10 )
    !/    26 feb-2016 : update boundary spectra.    jgli    ( version 2.20 )
    !/    23 mar-2016 : add current option.         jgli    ( version 2.30 )
    !/    18 apr-2018 : refined sub-grid blocking.  jgli    ( version 2.40 )
    !/
    !  1. purpose :
    !
    !     propagation in phyiscal space for a given spectral component.
    !
    !  2. method :
    !
    !     unstructured smc grid, point-oriented face and cell loops.
    !     uno2 advection scheme and isotropic ftcs diffusion scheme.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       isp     int.   i   number of spectral bin (ik-1)*nth+ith
    !       facx/y  real   i   factor in propagation velocity (1 or 0 *dt/dx)
    !       dtg     real   i   total time step.
    !       mapsta  i.a.   i   grid point status map.
    !       mapfs   i.a.   i   storage map.
    !       vq      r.a.  i/o  field to propagate.
    !     ----------------------------------------------------------------
    !
    !     local variables.
    !     ----------------------------------------------------------------
    !       ntloc   int   number of local time steps.
    !       dtloc   real  local propagation time step.
    !       cgd     real  deep water group velocity.
    !       dssd, dnnd    deep water diffusion coefficients.
    !       ulcflx  r.a.  local courant numbers in 'x' (norm. velocities)
    !       vlcfly  r.a.  id. in 'y'.
    !       cxtot   r.a.  propagation velocities in physical space.
    !       cytot   r.a.
    !       dfrr    real  relative frequency increment.
    !       dx0i    real  inverted grid incremenent in meters (longitude, eq.).
    !       dy0i    real  inverted grid incremenent in meters (latitude).
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     see module documentation.
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3wave    subr. w3wavemd wave model routine.
    !     ---------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     ---------------------------------------------
    !       1.  preparations
    !         a set constants
    !         b initialize arrays
    !       2.  prepare arrays
    !         a velocities and 'q'
    !         b diffusion coefficients
    !       3.  loop over sub-steps
    !       ----------------------------------------
    !         a propagate
    !         b update boundary conditions
    !         c diffusion correction
    !       ----------------------------------------
    !       4.  store q field in spectra
    !     ---------------------------------------------
    !
    !  9. switches :
    !
    !       !/mgp   correct for motion of grid.
    !
    !       !/tdyn  dynamic increase of dtme
    !       !/dss0  disable diffusion in propagation direction
    !       !/xw0   propagation diffusion only.
    !       !/xw1   growth diffusion only.
    !
    !       !/s     enable subroutine tracing.
    !
    !       !/t     enable general test output.
    !       !/t1    dump of input field and fluxes.
    !       !/t2    dump of output field.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    !
    use w3timemd, only: dsec21
    !
    use w3gdatmd, only: nk, nth, dth, xfr, esin, ecos, sig, nx, ny,  &
         nsea, sx, sy, mapsf, funo3, fverg,           &
         ijkcel, ijkufc, ijkvfc, ncel, nufc, nvfc,    &
         ijkcel3, ijkcel4,                            &
         ijkvfc5, ijkvfc6,ijkufc5,ijkufc6,            &
         nlvcel, nlvufc, nlvvfc, nrlv, mrfct,         &
         dtcfl, clats, dtms, ctrnx, ctrny
    use w3gdatmd, only: nglo, angarc, arctc
    use w3wdatmd, only: time
    use w3adatmd, only: cg, wn, u10, cx, cy, atrnx, atrny, itime
    !
    use w3idatmd, only: flcur
    use w3odatmd, only: ndse, ndst, flbpi, nbi, tbpi0, tbpin,       &
         isbpi, bbpi0, bbpin
    !
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: isp
    real,    intent(in)     :: dtg
    real,    intent(inout)  :: vq(nsea)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ith, ik, ntloc, itloc, isea, ixy,    &
         iy, iy0, ip, ibi, lvr
    integer                 :: i, j, k, l, m, n, ll, mm, nn, lmn,   &
         iuf, juf, ivf, jvf, icl, jcl
    real                    :: cg0, cga, cgn, cgx, cgy, fmr, rd1,   &
         rd2, cxmin, cxmax, cymin, cymax,     &
         cxc, cyc, dtldx, dtldy
    real                    :: dtloc, cgcos, cgsin, futrn, fvtrn,   &
         dfrr, dx0i, dy0i, cgd, dssd,         &
         dnnd, dcell, xwind, tfac, dss, dnn
    real                    :: pcarea, arcth
    logical                 :: yfirst
    !/
    !/ automatic work arrays
    !
    real, dimension(-9:ncel) ::  fcnt, afcn, bcnt, ucfl, vcfl, cq,  &
         cqa, cxtot, cytot
    real, dimension(   nufc) ::  fumd, fudifx, ulcflx
    real, dimension(   nvfc) ::  fvmd, fvdify, vlcfly
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  preparations --------------------------------------------------- *
    !!li  spectral bin direction and frequency indices
    ith    = 1 + mod(isp-1,nth)
    ik     = 1 + (isp-1)/nth
    !!li  maximum group speed for 1st and the transported frequency bin
    !!li  a factor of 1.2 is added to account for the shallow water peak.
    cg0    = 0.6 * grav / sig(1)
    cga    = 0.6 * grav / sig(ik)
    !!li  maximum group speed for given spectral bin. first bin speed is
    !!li  used to avoid zero speed component.
    !     cgx    = abs( cga * ecos(ith) )
    !     cgy    = abs( cga * esin(ith) )
    cgx    =  cga * ecos(ith)
    cgy    =  cga * esin(ith)
    !!li  add maximum current components to maximum group components.
    if ( flcur ) then
      cxmin  = minval ( cx(1:nsea) )
      cxmax  = maxval ( cx(1:nsea) )
      cymin  = minval ( cy(1:nsea) )
      cymax  = maxval ( cy(1:nsea) )
      if ( abs(cgx+cxmin) .gt. abs(cgx+cxmax) ) then
        cgx    = cgx + cxmin
      else
        cgx    = cgx + cxmax
      end if
      if ( abs(cgy+cymin) .gt. abs(cgy+cymax) ) then
        cgy    = cgy + cymin
      else
        cgy    = cgy + cymax
      end if
      cxc    = max ( abs(cxmin) , abs(cxmax) )
      cyc    = max ( abs(cymin) , abs(cymax) )
    else
      cxc    = 0.
      cyc    = 0.
    end if
    !!li  base-cell grid lenth at equator (size-4 on smc625 grid).
    dx0i  = 1.0/(sx * dera * radius)
    dy0i  = 1.0/(sy * dera * radius)
    !!li  miminum time step determined by courant number < 0.8
    !!li  note, minimum x grid length is half the equator value.
    !!li  minimum time step should not be less than sub w3init requirement,
    !!li  where iappro array is initialised for propagation parallization.
    cgn   = 0.9999 * max( abs(cgx), abs(cgy), cxc, cyc, 0.001*cg0 )
    dtloc = dtcfl*cg0/cgn
    ntloc  = 1 + int(dtg/dtloc - 0.001)
    dtloc  = dtg / real(ntloc)
    !!li  group speed component common factors, facx=dtg*dx0i
    !!li  facx and facy are evaluated here directly.  jgli16jan2013
    !     cgcos   = facx * ecos(ith) / real(ntloc)
    !     cgsin   = facy * esin(ith) / real(ntloc)
    cgcos   = ecos(ith)
    cgsin   = esin(ith)
    dtldx   = dtloc * dx0i
    dtldy   = dtloc * dy0i
    !
    yfirst = mod(itime,2) .eq. 0
    !
    !li   homogenous diffusion fourier number dnnd and dssd will be used.
    !li   they have to be divided by base-cell size for size-1 stability.
    !li   so they are equivalent to the fourier number in size-1 cell at
    !li   the sub-time step dtloc/mrfct.
    if ( dtms .gt. 0. ) then
      dfrr   = xfr - 1.
      cgd    = 0.5 * grav / sig(ik)
      dnn    = ((dth*cgd)**2)*dtms / 12.
      dnnd   = dnn*dtloc*(dx0i*dx0i)
      dssd   = dnn*dtloc*(dy0i*dy0i)
    else
      dssd = 0.0
      dnnd = 0.0
    end if
    !
    ! 1.b initialize arrays
    !
    !
    ulcflx = 0.
    vlcfly = 0.
    !li    pass spectral element vq to cq and define size-1 cell cfl
    do isea=1, nsea
      !li  transported variable is divided by cg as in ww3.
      cq(isea) = vq(isea)/cg(ik,isea)
      !li  resetting nanq vq to zero if any.   jgli18mar2013
      if( .not. (cq(isea) .eq. cq(isea)) )  cq(isea) = 0.0
    end do
    !li  add current components if any to wave velocity.
    if ( flcur ) then
      do isea=1, nsea
        cxtot(isea) = (cgcos * cg(ik,isea) + cx(isea))
        cytot(isea) = (cgsin * cg(ik,isea) + cy(isea))
      enddo
    else
      !li   no current case use group speed only.
      do isea=1, nsea
        cxtot(isea) =  cgcos * cg(ik,isea)
        cytot(isea) =  cgsin * cg(ik,isea)
      end do
      !li   end of if( flcur ) block.
    endif
    !li   arctic cell velocity components need to be rotated
    !li   back to local east referenence system for propagation.
    if( arctc ) then
      do isea=nglo+1, nsea
        arcth = angarc(isea-nglo)*dera
        cxc = cxtot(isea)*cos(arcth) + cytot(isea)*sin(arcth)
        cyc = cytot(isea)*cos(arcth) - cxtot(isea)*sin(arcth)
        cxtot(isea) = cxc
        cytot(isea) = cyc
      end do
      !li   polar cell area factor for v-flux update
      pcarea = dy0i/(mrfct*pi*dx0i*float(ijkcel(4,nsea)))
      !li   v-component is reset to zero for polar cell as direction
      !li   is undefined there.
      cytot(nsea) = 0.0
    endif
    !li     convert velocity components into cfl factors.
    do isea=1, nsea
      ucfl(isea) = dtldx*cxtot(isea)/clats(isea)
      vcfl(isea) = dtldy*cytot(isea)
    enddo
    !li  initialise boundary cell cq and velocity values.
    cq(-9:0)=0.0
    ucfl(-9:0)=0.0
    vcfl(-9:0)=0.0
    !
    ! 3.  loop over frequency-dependent sub-steps -------------------------*
    !
    do itloc=1, ntloc
      !
      !     initialise net flux arrays.
      fcnt(-9:ncel) = 0.0
      afcn(-9:ncel) = 0.0
      bcnt(-9:ncel) = 0.0
      !
      !     single-resolution smc grid uses regular grid advection with
      !     partial blocking enabled when nrlv = 1
      if ( nrlv .eq. 1 ) then
        if( funo3 ) then
          !  use 3rd order uno3 scheme.  jgli20aug2015
          call smcxuno3r(1, nufc, cq, ucfl, ulcflx, dnnd, fumd, fudifx)
        else
          !  call smcxuno2 to calculate mfx value
          call smcxuno2r(1, nufc, cq, ucfl, ulcflx, dnnd, fumd, fudifx)
        endif
        !  store conservative flux in fcnt advective one in afcn
        do i=1, nufc
          m=ijkufc5(i)
          n=ijkufc6(i)
          futrn = fumd(i)*ulcflx(i) - fudifx(i)
          !! add sub-grid transparency for input flux update.  jgli16may2011
          !! transparency is also applied on diffusion flux.   jgli12mar2012
          !! replace critical with atomic.  jgli15jan2019
          !! !$omp critical
          !! remove boundary cell flux update or m n > 0.  jgli28mar2019
          if( m > 0 ) then
            if( (ctrnx(m)+ctrnx(n)) .ge. 1.96 )  then
              fcnt(m) = fcnt(m) - futrn
            else if( ulcflx(i) .ge. 0.0 )  then
              fcnt(m) = fcnt(m) - futrn*ctrnx(m)
            else
              fcnt(m) = fcnt(m) - futrn*ctrnx(n)*ctrnx(m)
            endif
            !  also divided by another cell length as ucfl is in basic unit.
            ! chrisb: re-arranged the rhs term below to make it
            ! valid for omp atmoic directive.
            afcn(m) = afcn(m) - (fumd(i)*ucfl(m) - fudifx(i))
          endif
          if( n > 0 ) then
            if( (ctrnx(m)+ctrnx(n)) .ge. 1.96 )  then
              fcnt(n) = fcnt(n) + futrn
            else if( ulcflx(i) .ge. 0.0 )  then
              fcnt(n) = fcnt(n) + futrn*ctrnx(m)*ctrnx(n)
            else
              fcnt(n) = fcnt(n) + futrn*ctrnx(n)
            endif
            !  also divided by another cell length as ucfl is in basic unit.
            afcn(n) = afcn(n) + (fumd(i)*ucfl(n) - fudifx(i))
          endif
          !! !$omp end critical
        enddo
        !  store conservative update in cqa and advective update in cq
        !  the side length in mf value has to be cancelled with cell length
        !  note ulcflx has been divided by the cell size inside smcxuno2.
        do n=1, nsea
          cqa(n)=cq(n) + fcnt(n)/float(ijkcel3(n))
          cq (n)=cq(n) + afcn(n)/float(ijkcel3(n))
        enddo
        !  call advection subs.
        if( funo3 ) then
          !  use 3rd order uno3 scheme.  jgli20aug2015
          call smcyuno3r(1, nvfc, cq, vcfl, vlcfly, dssd, fvmd, fvdify)
        else
          !  call smcyuno2 to calculate mfy value
          call smcyuno2r(1, nvfc, cq, vcfl, vlcfly, dssd, fvmd, fvdify)
        endif
        do j=1, nvfc
          m=ijkvfc5(j)
          n=ijkvfc6(j)
          fvtrn = fvmd(j)*vlcfly(j) - fvdify(j)
          !! add sub-grid transparency for input flux update.  jgli16may2011
          !! transparency is also applied on diffusion flux.   jgli12mar2012
          !! replace critical with atomic.  jgli15jan2019
          !! !$omp critical
          !! remove boundary cell flux update or m n > 0.  jgli28mar2019
          if( m > 0 ) then
            if( (ctrny(m)+ctrny(n)) .ge. 1.96 )  then
              bcnt(m) = bcnt(m) - fvtrn
            else if( vlcfly(j) .ge. 0.0 )  then
              bcnt(m) = bcnt(m) - fvtrn*ctrny(m)
            else
              bcnt(m) = bcnt(m) - fvtrn*ctrny(n)*ctrny(m)
            endif
          endif
          if( n > 0 ) then
            if( (ctrny(m)+ctrny(n)) .ge. 1.96 )  then
              bcnt(n) = bcnt(n) + fvtrn
            else if( vlcfly(j) .ge. 0.0 )  then
              bcnt(n) = bcnt(n) + fvtrn*ctrny(m)*ctrny(n)
            else
              bcnt(n) = bcnt(n) + fvtrn*ctrny(n)
            endif
          endif
          !! !$omp end critical
        enddo
        !  store conservative update of cqa in cq
        !  the v side length in mf value has to be cancelled with cell length
        !! one cosine factor is also needed to be divided for smc grid
        do n=1, nsea
          cq(n)=cqa(n) + bcnt(n)/( clats(n)*float(ijkcel3(n)) )
        enddo
        !   polar cell needs a special area factor, one-level case.
        if( arctc ) cq(nsea) = cqa(nsea) + bcnt(nsea)*pcarea
        !   end of single-resolution advection and diffusion.
      else
        !     multi-resolution smc grid uses irregular grid advection scheme
        !     without partial blocking when nrlv > 1
        !
        ! 3.a    multiresolution sub-steps depend on refined levels mrfct
        do lmn = 1, mrfct
          !
          ! 3.b    loop over all levels, starting from the finest level.
          do ll= 1, nrlv
            !        cell size of this level
            lvr=2**(ll - 1)
            fmr=float( lvr )
            !
            ! 3.c    calculate this level only if size is factor of lmn
            if( mod(lmn, lvr) .eq. 0 ) then
              !
              ! 3.d    select cell and face ranges
              icl=nlvcel(ll-1)+1
              iuf=nlvufc(ll-1)+1
              ivf=nlvvfc(ll-1)+1
              jcl=nlvcel(ll)
              juf=nlvufc(ll)
              jvf=nlvvfc(ll)
              !
              !  use 3rd order uno3 scheme.  jgli03sep2015
              if( funo3 ) then
                call smcxuno3(iuf, juf, cq, ucfl, ulcflx, dnnd, fumd, fudifx, fmr)
              else
                !  call smcxuno2 to calculate finest level (size-1) mfx value
                call smcxuno2(iuf, juf, cq, ucfl, ulcflx, dnnd, fumd, fudifx, fmr)
              endif
              !  store fineset level conservative flux in fcnt advective one in afcn
              do i=iuf, juf
                l=ijkufc5(i)
                m=ijkufc6(i)
                futrn = fumd(i)*ulcflx(i) - fudifx(i)
                !! replace critical with atomic.  jgli15jan2019
                !! !$omp critical
                !! remove boundary cell flux update or l m > 0.  jgli28mar2019
                if( l > 0 ) then
                  !! add sub-grid blocking for refined cells.   jgli18apr2018
                  if( (ctrnx(m)+ctrnx(l)) .ge. 1.96 )  then
                    fcnt(l) = fcnt(l) - futrn
                  else if( ulcflx(i) .ge. 0.0 ) then
                    fcnt(l) = fcnt(l) - futrn*ctrnx(l)
                  else
                    fcnt(l) = fcnt(l) - futrn*ctrnx(l)*ctrnx(m)
                  endif
                  ! chrisb: re-arranged the rhs term below to make it
                  ! valid for omp atmoic directive.
                  afcn(l) = afcn(l) - (fumd(i)*ucfl(l)*fmr - fudifx(i))
                endif
                if( m > 0 ) then
                  !! add sub-grid blocking for refined cells.   jgli18apr2018
                  if( (ctrnx(m)+ctrnx(l)) .ge. 1.96 )  then
                    fcnt(m) = fcnt(m) + futrn
                  else if( ulcflx(i) .ge. 0.0 ) then
                    fcnt(m) = fcnt(m) + futrn*ctrnx(m)*ctrnx(l)
                  else
                    fcnt(m) = fcnt(m) + futrn*ctrnx(m)
                  endif
                  afcn(m) = afcn(m) + (fumd(i)*ucfl(m)*fmr - fudifx(i))
                endif
                !! !$omp end critical
              enddo
              !  store conservative update in cqa and advective update in cq
              !  the side length in mf value has to be cancelled with cell y-length.
              !  also divided by another cell x-size as ucfl is in size-1 unit.
              do n=icl, jcl
                cqa(n)=cq(n) + fcnt(n)/float( ijkcel3(n)*ijkcel4(n) )
                cq (n)=cq(n) + afcn(n)/float( ijkcel3(n)*ijkcel4(n) )
                fcnt(n)=0.0
                afcn(n)=0.0
              enddo
              !
              !  use 3rd order uno3 scheme.  jgli03sep2015
              if( funo3 ) then
                call smcyuno3(ivf, jvf, cq, vcfl, vlcfly, dssd, fvmd, fvdify, fmr)
              else
                !  call smcyuno2 to calculate mfy value
                call smcyuno2(ivf, jvf, cq, vcfl, vlcfly, dssd, fvmd, fvdify, fmr)
              endif
              !
              !  store conservative flux in bcnt
              do j=ivf, jvf
                l=ijkvfc5(j)
                m=ijkvfc6(j)
                fvtrn = fvmd(j)*vlcfly(j) - fvdify(j)
                !! replace critical with atomic.  jgli15jan2019
                !! !$omp critical
                !! remove boundary cell flux update or l m > 0.  jgli28mar2019
                if( l > 0 ) then
                  !! add sub-grid blocking for refined cells.   jgli18apr2018
                  if( (ctrny(m)+ctrny(l)) .ge. 1.96 )  then
                    bcnt(l) = bcnt(l) - fvtrn
                  else if( vlcfly(j) .ge. 0.0 )  then
                    bcnt(l) = bcnt(l) - fvtrn*ctrny(l)
                  else
                    bcnt(l) = bcnt(l) - fvtrn*ctrny(l)*ctrny(m)
                  endif
                endif
                if( m > 0 ) then
                  !! add sub-grid blocking for refined cells.   jgli18apr2018
                  if( (ctrny(m)+ctrny(l)) .ge. 1.96 )  then
                    bcnt(m) = bcnt(m) + fvtrn
                  else if( vlcfly(j) .ge. 0.0 )  then
                    bcnt(m) = bcnt(m) + fvtrn*ctrny(m)*ctrny(l)
                  else
                    bcnt(m) = bcnt(m) + fvtrn*ctrny(m)
                  endif
                endif
                !! !$omp end critical
              enddo
              !  store conservative update of cqa in cq
              !  the v side length in mf value has to be cancelled with x-size.
              !  also divided by cell y-size as vcfl is in size-1 unit.
              !! one cosine factor is also needed to be divided for smc grid.
              do n=icl, jcl
                cq(n)=cqa(n) + bcnt(n)/( clats(n)*            &
                     &             float( ijkcel3(n)*ijkcel4(n) ) )
                bcnt(n)=0.0
              enddo
              !li  polar cell needs a special area factor, multi-level case.
              if( arctc .and. jcl .eq. nsea ) then
                cq(nsea) = cqa(nsea) + bcnt(nsea)*pcarea
              endif
              !
              !  end of refine level if block  mod(lmn, lvr) .eq. 0
            endif
            !  end of refine level loop ll=1, nrlv
          enddo
          !!
          !!    end of multi-resolution sub-step loop lmn = 1, mrfct
        enddo
        !  end of multi-resolution advection else block of nrlv > 1
      endif
      !!   update boundary spectra if any.  jgli26feb2016
      !
      if ( flbpi ) then
        rd1 = dsec21(tbpi0, time)-dtg*real(ntloc-itloc)/real(ntloc)
        rd2 = dsec21(tbpi0, tbpin)
        if ( rd2 .gt. 0.001 ) then
          rd2    = min(1.,max(0.,rd1/rd2))
          rd1    = 1. - rd2
        else
          rd1    = 0.
          rd2    = 1.
        end if
        do ibi=1, nbi
          isea     = isbpi(ibi)
          cq(isea) = (rd1*bbpi0(isp,ibi) + rd2*bbpin(isp,ibi))   &
               /cg(ik,isea)
        end do
      endif
      !
      !!    end of itloc do
    enddo
    !  average with 1-2-1 scheme.  jgli20aug2015
    if(fverg) call smcaverg(cq)
    !
    ! 4.  store results in vq in proper format --------------------------- *
    !
    do isea=1, nsea
      vq(isea) =  max ( 0. , cq(isea)*cg(ik,isea) )
    end do
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3psmc ----------------------------------------------------- /
    !/
  end subroutine w3psmc
  !/
  !/ ------------------------------------------------------------------- /
  !> @brief refraction and great-circle turning by spectral rotation
  !>
  !> @details linear interpolation equivalent to 1st order upstream scheme
  !>  but without restriction on rotation angle.  however, refraction
  !>  is limited towards the depth gradient direction (< 90 degree).
  !>  refraction induced spectral shift in the k-space will remain
  !>  to be advected using the uno2 scheme.
  !>
  !> @param[in]    isea     number of sea point
  !> @param[in]    facth    factor in propagation velocity (th)
  !> @param[in]    fack     factor in propagation velocity (k)
  !> @param[in]    cthg0    factor in great circle refraction term
  !> @param[in]    cg       local group velocities
  !> @param[in]    wn       local wavenumbers
  !> @param[in]    depth    depth
  !> @param[in]    dddx     depth x-gradient
  !> @param[in]    dddy     depth y-gradient
  !> @param[in]    alflmt   refraction limiter
  !> @param[in]    cx       current x-component
  !> @param[in]    cy       current y-component
  !> @param[in]    dcxdx    current gradient (dcx/dx)
  !> @param[in]    dcxdy    current gradient (dcx/dy)
  !> @param[in]    dcydx    current gradient (dcy/dx)
  !> @param[in]    dcydy    current gradient (dcy/dy)
  !> @param[in]    dcdx     phase speed x-gradient
  !> @param[in]    dcdy     phase speed y-gradient
  !> @param[inout] va       spectrum
  !>
  !> @author jian-guo li
  !> @date 06-jun-2018
  !>
  subroutine w3krtn ( isea, facth, fack, cthg0, cg, wn, depth,    &
       dddx, dddy, alflmt, cx, cy, dcxdx, dcxdy,   &
       dcydx, dcydy, dcdx, dcdy, va )
    !/
    !/                  +------------------------------------+
    !/                  | spherical multiple-cell (smc) grid |
    !/                  | refraction and great-cirle turning |
    !/                  |           jian-guo li              |
    !/                  | first created:     8 nov 2010      |
    !/                  | last modified:    06-jun-2018      |
    !/                  +------------------------------------+
    !/
    !/    08-nov-2010 : origination.                        ( version 1.00 )
    !/    10-jun-2011 : new refraction formulation.         ( version 1.10 )
    !/    16-jun-2011 : add refraction limiter to gradient. ( version 1.20 )
    !/    21-jul-2011 : old refraction formula + limiter.   ( version 1.30 )
    !/    26-jul-2011 : tidy up refraction schemes.         ( version 1.40 )
    !/    28-jul-2011 : finalise with old refraction.       ( version 1.50 )
    !/    23-mar-2016 : add current option in refraction.   ( version 2.30 )
    !/    06-jun-2018 : add debugdcxdx                      ( version 6.04 )
    !/
    !/
    !  1. purpose :
    !
    !     refraction and great-circle turning by spectral rotation.
    !
    !  2. method :
    !
    !     linear interpolation equivalent to 1st order upstream scheme
    !     but without restriction on rotation angle.  however, refraction
    !     is limited towards the depth gradient direction (< 90 degree).
    !     refraction induced spectral shift in the k-space will remain
    !     to be advected using the uno2 scheme.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       isea    int.   i   number of sea point.
    !       facth/k real   i   factor in propagation velocity.
    !       cthg0   real   i   factor in great circle refracftion term.
    !       mapxx2  i.a.   i   propagation and storage maps.
    !       cg      r.a.   i   local group velocities.
    !       wn      r.a.   i   local wavenumbers.
    !       depth   r.a.   i   depth.
    !       dddx    real   i   depth gradients.
    !       cx/y    real   i   current components.
    !       dcxdx   real   i   current gradients.
    !       dcdx    real   i   phase speed gradients.
    !       va      r.a.  i/o  spectrum.
    !     ----------------------------------------------------------------
    !
    !     local variables.
    !     ----------------------------------------------------------------
    !       dph2k   r.a.  2*depth*wave_number_k
    !       snh2k   r.a.  sinh(2*depth*wave_number_k)
    !       fdd, fdu, fgc, fcd, fcu
    !               r.a.  directionally varying part of depth, current and
    !                     great-circle refraction terms and of consit.
    !                     of ck term.
    !       cflt-k  r.a.  propagation velocities of local fluxes.
    !       db      r.a.  wavenumber band widths at cell centers.
    !       dm      r.a.  wavenumber band widths between cell centers and
    !                     next cell center.
    !       q       r.a.  extracted spectrum
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !       smcgtcrfr refraction and gct rotation in theta.
    !       smckuno2  refraction shift in k-space by uno2.
    !       strace    service routine.
    !
    !  5. called by :
    !
    !       w3wave   wave model routine.
    !
    !  6. error messages :
    !
    !       none.
    !
    !  8. structure :
    !
    !     -----------------------------------------------------------------
    !       1.  preparations
    !         a initialize arrays
    !         b set constants and counters
    !       2.  point  preparations
    !         a calculate snh2k
    !         b extract spectrum
    !       3.  refraction velocities
    !         a filter level depth reffraction.
    !         b depth refratcion velocity.
    !         c current refraction velocity.
    !       4.  wavenumber shift velocities
    !         a prepare directional arrays
    !         b calcuate velocity.
    !       5.  propagate.
    !       6.  store results.
    !     -----------------------------------------------------------------
    !
    !  9. switches :
    !
    !       !/s     enable subroutine tracing.
    !       !/t     enable general test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    use w3gdatmd, only: nk, nth, nspec, sig, dsip, ecos, esin, &
         ec2, esc, es2, flcth, flck, ctmax, dth
    use w3adatmd, only: itime
    use w3idatmd, only: flcur
    use w3odatmd, only: ndse, ndst
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in) :: isea
    real, intent(in)    :: facth, fack, cthg0, cg(0:nk+1),      &
         wn(0:nk+1), depth, dddx, dddy,       &
         alflmt(nth), cx, cy, dcxdx, dcxdy,   &
         dcydx, dcydy, dcdx(0:nk+1), dcdy(0:nk+1)
    real, intent(inout) :: va(nspec)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer    :: ith, ik, isp
    real       :: fgc, fkd, fks, frk(nk), frg(nk), ddnorm(nth),     &
         fkc(nth), vq(nspec), vcflt(nspec), depth30,       &
         db(0:nk+1), dm(-1:nk+1), cflk(nth,0:nk),          &
         !li   for new refraction scheme using cg.  jgli26jul2011
         !                   dph2k(0:nk+1), snh2k(0:nk+1)
         !li   for old refraction scheme using phase speed.  jgli26jul2011
         sigsnh(0:nk+1)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  preparation for point ------------------------------------------ *
    !     array with partial derivative of sigma versus depth
    !li   use of minimum depth 30 m for refraction factor.  jgli12feb2014
    depth30=max(30.0, depth)
    do ik=0, nk+1
      !li   for old refraction scheme using phase speed.  jgli8jun2011
      !        dph2k(ik) = 2.0*wn(ik)*depth
      !        snh2k(ik) = sinh( dph2k(ik) )
      !li   for new refraction scheme using cg.  jgli3jun2011
      !!        sigsnh(ik) = sig(ik)/sinh(2.0*wn(ik)*depth)
      !!ac  replacing sigsinh with a delimiter to prevent the sinh value from
      !!ac  becoming significantly large. right now set to a max around 1e21
      !         sigsnh(ik) = sig(ik)/sinh(min(2.0*wn(ik)*depth,50.0))
      !li   refraction factor uses minimum depth of 30 m.  jgli12feb2014
      sigsnh(ik) = sig(ik)/sinh(min(2.0*wn(ik)*depth30,50.0))
    end do
    !
    ! 2.  extract spectrum without mapping
    !
    vq = va
    ! 3.  refraction velocities ------------------------------------------ *
    !
    !
    if ( flcth ) then
      !
      ! 3.a set slope filter for depth refraction
      !
      !li   lift theta-refraction limit and use new formulation.   25 nov 2010
      !li   facth  = dtg / dth / real(ntloc), cthg0 = - tan(dera*y) / radius
      fgc    = facth * cthg0
      !
      do ik=1, nk
        !li   new refraction formulation using cg only.  jgli3jun2011
        !         frk(ik) = facth*2.0*sig(ik)*(1.-depth*sig(ik)*sig(ik)/grav)  &
        !     &                                /(dph2k(ik)+snh2k(ik))
        !li   old refraction formulation using phase speed.  jgli8jun2011
        frk(ik) = facth * sigsnh(ik)
        !li
        frg(ik) = fgc * cg(ik)
      end do
      !
      !li   current induced refraction stored in fkc.    jgli30mar2016
      if ( flcur ) then
        do ith=1, nth
          !li   put a ctmax limit on current theta rotation.  jgli02mar2017
          !               fkc(ith) = facth*( dcydx*es2(ith) - dcxdy*ec2(ith) +  &
          fgc      = facth*( dcydx*es2(ith) - dcxdy*ec2(ith) +  &
               (dcxdx - dcydy)*esc(ith) )
          fkc(ith) = sign( min(abs(fgc), ctmax), fgc )
        end do
      else
        fkc(:)=0.0
      end if
      !
      ! 3.b depth refraction and great-circle turning.
      !
      do ith=1, nth
        ddnorm(ith)=esin(ith)*dddx-ecos(ith)*dddy
        do ik=1, nk
          isp = (ik-1)*nth + ith
          !li   apply depth gradient limited refraction, current and gct term
          vcflt(isp)=frg(ik)*ecos(ith) + fkc(ith) +          &
               sign( min(abs(frk(ik)*ddnorm(ith)), alflmt(ith)),  &
               !li   for new refraction scheme using cg.  jgli26jul2011
               !                             frk(ik)*ddnorm(ith) )
               !li   for old refraction scheme using phase speed.  jgli26jul2011
               ddnorm(ith) )
        end do
      end do
    end if
    !
    ! 4.  wavenumber shift velocities due to current refraction ---------- *
    !
    if ( flck ) then
      !
      ! 4.a directionally dependent part
      !
      do ith=1, nth
        !li   depth induced refraction is suspended as it is absorbed in
        !li   the fixed frequency bin used for wave spectrum.  jgli30mar2016
        !           fkc(ith) = ( ecos(ith)*dddx + esin(ith)*dddy )
        fkc(ith) = -dcxdx*ec2(ith) -dcydy*es2(ith)               &
             -(dcxdy + dcydx)*esc(ith)
      end do
      fkd = cx*dddx + cy*dddy
      !
      ! 4.b band widths
      !
      !li  cell and side indices for k-dimension are arranged as
      !    cell:    | -1 | 0 | 1 | 2 | ... | nk | nk+1 | nk+2 |
      !    side:        -1   0   1   2 ...     nk     nk+1
      !li  dsip = sig(k+1) - sig(k), radian frequency increment
      !
      do ik=0, nk
        db(ik) = dsip(ik) / cg(ik)
        dm(ik) = wn(ik+1) - wn(ik)
      end do
      db(nk+1) = dsip(nk+1) / cg(nk+1)
      dm(nk+1) = dm(nk)
      dm(  -1) = dm( 0)
      ! 4.c courant number of k-velocity without dividing by dk
      !!li  fack = dtg / real(ntloc)
      !
      do ik=0, nk
        !li   for new refraction scheme using cg.  jgli3jun2011
        !           fks   = - fack*wn(ik)*sig(ik)/snh2k(ik)
        !li   old refraction formulation using phase speed.  jgli8jun2011
        !           fks   = - fack*wn(ik)*sigsnh(ik)
        !li   current induced k-shift.   jgli30mar2016
        fks = max( 0.0, cg(ik)*wn(ik)-0.5*sig(ik) )*fkd /    &
             ( depth30*cg(ik) )
        do ith=1, nth
          cflk(ith,ik) = fack*( fks + fkc(ith)*wn(ik) )
        end do
      end do
      !li   no cfl limiter is required here as it is applied in smckuno2.
      !
    end if
    !
    ! 5.  propagate ------------------------------------------------------ *
    !
    if ( mod(itime,2) .eq. 0 ) then
      if ( flck ) then
        !!li  refraction caused k-space shift.
        call smckuno2(cflk, vq, db, dm)
      end if
      if ( flcth ) then
        !!li  gct and refraction by rotation in theta direction.
        call smcgtcrfr(vcflt, vq)
      end if
    else
      if ( flcth ) then
        !!li  gct and refraction by rotation in theta direction.
        call smcgtcrfr(vcflt, vq)
      end if
      if ( flck )  then
        !!li  refraction caused k-space shift.
        call smckuno2(cflk, vq, db, dm)
      end if
    end if
    !
    ! 6.  store reults --------------------------------------------------- *
    !
    va = vq
    !
    return
    !
    !/ end of w3krtn ----------------------------------------------------- /
    !/
  end subroutine w3krtn
  !> @brief calculate mid-flux values for x dimension
  !>
  !> @param[in]   nua      start number of u-face list.
  !> @param[in]   nub      end number of u-face list.
  !> @param[in]   cf       transported variable.
  !> @param[in]   uc       veclocity u-component at cell centre.
  !> @param[out]  uflx     mid-flux u-component on u-face.
  !> @param[in]   akdif    diffusion coefficient.
  !> @param[out]  fu       advection mid-flux on u-face.
  !> @param[out]  fx       diffusion mid-flux on u-face.
  !> @param[in]   fts      timestep fraction for sub-timestep.
  !>
  !> @author jian-guo li
  !> @date 03-mar-2022
  !>
  ! subroutine that calculate mid-flux values for x dimension
  subroutine smcxuno2(nua, nub, cf, uc, uflx, akdif, fu, fx, fts)
    use constants
    use w3gdatmd, only: ncel, mrfct, nufc, ijkcel, ijkufc, clats, &
         ijkcel3, ijkcel4
    use w3odatmd, only: ndse, ndst
    implicit none
    integer, intent( in):: nua, nub
    real,    intent( in):: cf(-9:ncel), uc(-9:ncel), akdif, fts
    real,    intent(out):: uflx(nufc), fu(nufc), fx(nufc)
    !
    integer ::  i, j, k, l, m, n, ij
    real:: cnst, cnst0, cnst1, cnst2, cnst3, cnst4, cnst5, cnst6, cnst8, cnst9
    !     two layer of boundary cells are added to each boundary cell face
    !     with all boundary cell values cf(-9:0)=0.0.
    !     diffusion fourier no. at sub-time-step, proportional to face size,
    !     which is also equal to the sub-time-step factor fts.
    cnst0=akdif*fts*fts
    !     uniform diffusion coefficient for all sizes.  jgli24feb2012
    !        cnst0=akdif*mrfct*fts
    !    notice an extra side length l is multiplied to mid-flux to give correct
    !    proportion of flux into the cells.  this length will be removed by the
    !    cell length when the tracer concentration is updated.
    do i=nua, nub
      !    select upstream, central and downstream cells
      k=ijkufc(4,i)
      l=ijkufc(5,i)
      m=ijkufc(6,i)
      n=ijkufc(7,i)
      !    face bounding cell lengths and central gradient
      cnst2=float( ijkcel3(l) )
      cnst3=float( ijkcel3(m) )
      cnst5=(cf(m)-cf(l))/( cnst2 + cnst3 )
      !    courant number in local size-1 cell, arithmetic average.
      cnst6=0.5*( uc(l)+uc(m) )*fts
      uflx(i) = cnst6
      !    multi-resolution smc grid requires flux multiplied by face factor.
      cnst8 = float( ijkufc(3,i) )
      !    diffusion factor in local size-1 cell, plus the cosine factors.
      !    2.0 factor to cancel that in gradient cnst5.  jgli08mar2012
      !    the maximum cell number is used to avoid the boundary cell number
      !    in selection of the cosine factor.
      ij= max(l, m)
      cnst9 = 2.0/( clats( ij )*clats( ij ) )
      !    for positive velocity case
      if(cnst6 >= 0.0)  then
        !    use central cell velocity for boundary flux.  jgli06apr2011
        if( m .le. 0) uflx(i) = uc(l)*fts
        !    upstream cell length and gradient, depending on uflx sign.
        cnst1=float( ijkcel3(k) )
        cnst4=(cf(l)-cf(k))/( cnst2 + cnst1 )
        !    use minimum gradient all region.
        cnst=sign(1.0, cnst5)*min( abs(cnst4), abs(cnst5) )
        !    mid-flux value inside central cell
        fu(i)=(cf(l) + cnst*(cnst2 - uflx(i)))*cnst8
        !    for negative velocity case
      else
        !    use central cell velocity for boundary flux.  jgli06apr2011
        if( l .le. 0) uflx(i) = uc(m)*fts
        !    upstream cell length and gradient, depending on uflx sign.
        cnst1=float( ijkcel3(n) )
        cnst4=(cf(n)-cf(m))/( cnst1 + cnst3 )
        !    use minimum gradient outside monotonic region.
        cnst=sign(1.0, cnst5)*min( abs(cnst4), abs(cnst5) )
        !    mid-flux value inside central cell m
        fu(i)=(cf(m) - cnst*(cnst3+uflx(i)))*cnst8
      endif
      !    diffusion flux by face gradient x dt
      fx(i)=cnst0*cnst5*cnst8*cnst9
    end do
    return
  end subroutine smcxuno2
  !> @brief calculate mid-flux values for y dimension
  !>
  !> @param[in]   nva      start number of v-face list.
  !> @param[in]   nvb      end number of v-face list.
  !> @param[in]   cf       transported variable.
  !> @param[in]   vc       veclocity v-component at cell centre.
  !> @param[out]  vfly     mid-flux v-component on v-face.
  !> @param[in]   akdif    diffusion coefficient.
  !> @param[out]  fv       advection mid-flux on v-face.
  !> @param[out]  fy       diffusion mid-flux on v-face.
  !> @param[in]   fts      timestep fraction for sub-timestep.
  !>
  !> @author jian-guo li
  !> @date 03-mar-2022
  !>
  subroutine smcyuno2(nva, nvb, cf, vc, vfly, akdif, fv, fy, fts)
    use constants
    use w3gdatmd, only: ncel, mrfct, nvfc, ijkcel, ijkvfc, clatf, ijkcel4
    use w3odatmd, only: ndse, ndst
    implicit none
    integer, intent( in):: nva, nvb
    real,    intent( in):: cf(-9:ncel), vc(-9:ncel), akdif, fts
    real,    intent(out):: vfly(nvfc), fv(nvfc), fy(nvfc)
    integer ::  i, j, k, l, m, n, ij
    real:: cnst, cnst0, cnst1, cnst2, cnst3, cnst4, cnst5, cnst6, cnst8
    !     notice an extra side length l is multiplied to mid-flux to give correct
    !     proportion of flux into the cells.  this length will be removed by the
    !     cell length when the tracer concentration is updated.
    !     diffusion fourier no. at sub-time-step, proportional to face size,
    !     which is also equal to the sub-time-step factor fts.
    !        cnst0=akdif*fts*fts
    !     2.0 factor to cancel that in gradient cnst5.  jgli08mar2012
    cnst0=akdif*fts*fts*2.0
    !     uniform diffusion coefficient for all sizes.  jgli24feb2012
    !        cnst0=akdif*mrfct*fts
    do j=nva, nvb
      !    select upstream, central and downstream cells
      k=ijkvfc(4,j)
      l=ijkvfc(5,j)
      m=ijkvfc(6,j)
      n=ijkvfc(7,j)
      !    face bounding cell lengths and gradient
      cnst2=float( ijkcel4(l) )
      cnst3=float( ijkcel4(m) )
      cnst5=(cf(m)-cf(l))/( cnst2 + cnst3 )
      !    courant number in local size-1 cell unit
      !    multiply by multi-resolution time step factor  fts
      cnst6=0.5*( vc(l)+vc(m) )*fts
      vfly(j) = cnst6
      !    face size integer and cosine factor.
      !    clatf is defined on v-face for smc grid.  jgli28feb2012
      cnst8=clatf(j)*float( ijkvfc(3,j) )
      !    for positive velocity case
      if(cnst6 >= 0.0)  then
        !    boundary cell y-size is set equal to central cell y-size
        !    as y-boundary cell sizes are not proportional to refined
        !    inner cells but constant of the base cell y-size, and
        !    use central cell speed for face speed.  jgli06apr2011
        if( m .le. 0 ) then
          vfly(j) = vc(l)*fts
          cnst3   = cnst2
        endif
        !    upstream cell size and irregular grid gradient, depending on vfly.
        cnst1=float( ijkcel4(k) )
        cnst4=(cf(l)-cf(k))/( cnst2 + cnst1 )
        !    use minimum gradient outside monotonic region
        cnst=sign(1.0, cnst5)*min( abs(cnst4), abs(cnst5) )
        !    mid-flux value multiplied by face width and cosine factor
        fv(j)=( cf(l) + cnst*(cnst2 - vfly(j)) )*cnst8
        !    for negative velocity case
      else
        !    set boundary cell y-size equal to central cell y-size and
        !    use central cell speed for flux face speed.  jgli06apr2011
        if( l .le. 0 ) then
          vfly(j) = vc(m)*fts
          cnst2   = cnst3
        endif
        !    upstream cell size and gradient, depending on vfly sign.
        !    side gradients for central cell includs 0.5 factor.
        cnst1=float( ijkcel4(n) )
        cnst4=(cf(n)-cf(m))/( cnst1 + cnst3 )
        !    use minimum gradient outside monotonic region
        cnst=sign(1.0, cnst5)*min( abs(cnst4), abs(cnst5) )
        !    mid-flux value multiplied by face width and cosine factor
        fv(j)=( cf(m) - cnst*(cnst3 + vfly(j)) )*cnst8
      endif
      !    diffusion flux by face gradient x dt x face_width x cos(lat)
      !    multiply by multi-resolution time step factor fts
      fy(j)=cnst0*cnst5*cnst8
    end do
    return
  end subroutine smcyuno2
  !> @brief calculate mid-flux values for x dimension
  !>
  !> @param[in]   nua      start number of u-face list.
  !> @param[in]   nub      end number of u-face list.
  !> @param[in]   cf       transported variable.
  !> @param[in]   uc       veclocity u-component at cell centre.
  !> @param[out]  uflx     mid-flux u-component on u-face.
  !> @param[in]   akdif    diffusion coefficient.
  !> @param[out]  fu       advection mid-flux on u-face.
  !> @param[out]  fx       diffusion mid-flux on u-face.
  !>
  !> @author jian-guo li
  !> @date 03-mar-2022
  !>
  subroutine smcxuno2r(nua, nub, cf, uc, uflx, akdif, fu, fx)
    use constants
    use w3gdatmd, only: nsea, ny, ncel, nufc, ijkcel, ijkufc, clats
    use w3gdatmd, only: ijkcel3
    use w3odatmd, only: ndse, ndst
    implicit none
    integer, intent( in):: nua, nub
    real,    intent( in):: cf(-9:ncel), uc(-9:ncel), akdif
    real,    intent(out):: uflx(nufc), fu(nufc), fx(nufc)
    !
    integer ::  i, j, k, l, m, n, ij
    real:: cnst, cnst0, cnst1, cnst2, cnst3, cnst4, cnst5, cnst6
    !      two layer of boundary cells are added to each boundary cell face
    !      with all boundary cell values cf(-9:0)=0.0.
    !      notice an extra side length l is multiplied to mid-flux to give correct
    !      proportion of flux into the cells.  this length will be removed by the
    !      cell length when the tracer concentration is updated.
    do i=nua, nub
      !    select upstream, central and downstream cells
      k=ijkufc(4,i)
      l=ijkufc(5,i)
      m=ijkufc(6,i)
      n=ijkufc(7,i)
      !    face bounding cell lengths and gradient
      cnst2=float( ijkcel3(l) )
      cnst3=float( ijkcel3(m) )
      cnst5=(cf(m)-cf(l))
      !    averaged courant number for base-level cell face
      cnst6= 0.5*( uc(l)+uc(m) )
      uflx(i) = cnst6
      !    diffusion fourier number in local cell size
      !    to avoid boundary cell number, use maximum of l and m.
      ij= max(l, m)
      cnst0 = 2.0/( clats(ij)*clats(ij) )
      !    for positive velocity case
      if(cnst6 >= 0.0)  then
        !    use central cell velocity for boundary flux.  jgli06apr2011
        if( m .le. 0) uflx(i) = uc(l)
        !    side gradient for upstream cell as regular grid.
        cnst4=(cf(l)-cf(k))
        !    use minimum gradient all region with 0.5 factor
        cnst=sign(0.5, cnst5)*min( abs(cnst4), abs(cnst5) )
        !    mid-flux value inside central cell
        fu(i)=(cf(l) + cnst*(1.0-uflx(i)/cnst2))
        !    for negative velocity case
      else
        !    use central cell velocity for boundary flux.  jgli06apr2011
        if( l .le. 0) uflx(i) = uc(m)
        !    side gradient for upstream cell, depneding on uflx sign.
        cnst4=(cf(n)-cf(m))
        !    use minimum gradient outside monotonic region, include 0.5 factor
        cnst=sign(0.5, cnst5)*min( abs(cnst4), abs(cnst5) )
        !    mid-flux value inside central cell m
        fu(i)=(cf(m) - cnst*(1.0+uflx(i)/cnst3))
      endif
      !    diffusion flux by face gradient x dt
      fx(i)=akdif*cnst0*cnst5/(cnst2 + cnst3)
    end do
    return
  end subroutine smcxuno2r
  !> @brief calculate mid-flux values for y dimension
  !>
  !> @param[in]   nva      start number of v-face list.
  !> @param[in]   nvb      end number of v-face list.
  !> @param[in]   cf       transported variable.
  !> @param[in]   vc       veclocity v-component at cell centre.
  !> @param[out]  vfly     mid-flux v-component on v-face.
  !> @param[in]   akdif    diffusion coefficient.
  !> @param[out]  fv       advection mid-flux on v-face.
  !> @param[out]  fy       diffusion mid-flux on v-face.
  !>
  !> @author jian-guo li
  !> @date 03-mar-2022
  !>
  subroutine smcyuno2r(nva, nvb, cf, vc, vfly, akdif, fv, fy)
    use constants
    use w3gdatmd, only: nsea, ny, ncel, nvfc, ijkcel, ijkvfc, clatf
    use w3odatmd, only: ndse, ndst
    implicit none
    integer, intent( in):: nva, nvb
    real,    intent( in):: cf(-9:ncel), vc(-9:ncel), akdif
    real,    intent(out):: vfly(nvfc), fv(nvfc), fy(nvfc)
    integer ::  i, j, k, l, m, n, ij
    real:: cnst, cnst0, cnst1, cnst2, cnst3, cnst4, cnst5, cnst6, cnst8
    !     notice an extra side length l is multiplied to mid-flux to give correct
    !     proportion of flux into the cells.  this length will be removed by the
    !     cell length when the tracer concentration is updated.
    do j=nva, nvb
      !    select upstream, central and downstream cells
      k=ijkvfc(4,j)
      l=ijkvfc(5,j)
      m=ijkvfc(6,j)
      n=ijkvfc(7,j)
      !    central face gradient.
      cnst5=(cf(m)-cf(l))
      !    courant number in basic cell unit as dy is constant
      cnst6=0.5*( vc(l)+vc(m) )
      vfly(j) = cnst6
      !    face size integer and cosine factor
      !    clatf is defined on v-face for smc grid.  jgli28feb2012
      cnst8=clatf(j)*float( ijkvfc(3,j) )
      !    for positive velocity case
      if(cnst6 >= 0.0)  then
        !    use central cell speed for flux face speed.  jgli06apr2011
        if( m .le. 0 ) vfly(j) = vc(l)
        !    upstream face gradient, depending on vfly sign.
        cnst4=(cf(l)-cf(k))
        !    use minimum gradient, including 0.5 factor and central sign.
        cnst=sign(0.5, cnst5)*min( abs(cnst4), abs(cnst5) )
        !    mid-flux value multiplied by face width and cosine factor
        fv(j)=( cf(l) + cnst*(1.0-vfly(j)) )*cnst8
        !    for negative velocity case
      else
        !    use central cell speed for flux face speed.  jgli06apr2011
        if( l .le. 0 ) vfly(j) = vc(m)
        !    side gradients for upstream face, depending on vfly sign.
        cnst4=(cf(n)-cf(m))
        !    use minimum gradient, including 0.5 factor and central sign.
        cnst=sign(0.5, cnst5)*min( abs(cnst4), abs(cnst5) )
        !    mid-flux value multiplied by face width and cosine factor
        fv(j)=( cf(m) - cnst*(1.0+vfly(j)) )*cnst8
      endif
      !    diffusion flux by face gradient x dt x face_width x cos(lat)
      fy(j)=akdif*cnst5*cnst8
    end do
    return
  end subroutine smcyuno2r
  !> @brief calculate mid-flux values for x dimension with uno3 scheme
  !>
  !> @param[in]   nua      start number of u-face list.
  !> @param[in]   nub      end number of u-face list.
  !> @param[in]   cf       transported variable.
  !> @param[in]   uc       veclocity u-component at cell centre.
  !> @param[out]  uflx     mid-flux u-component on u-face.
  !> @param[in]   akdif    diffusion coefficient.
  !> @param[out]  fu       advection mid-flux on u-face.
  !> @param[out]  fx       diffusion mid-flux on u-face.
  !> @param[in]   fts      timestep fraction for sub-timestep.
  !>
  !> @author jian-guo li
  !> @date 03-mar-2022
  !>
  subroutine smcxuno3(nua, nub, cf, uc, uflx, akdif, fu, fx, fts)
    use constants
    use w3gdatmd, only: ncel, mrfct, nufc, ijkcel, ijkufc, clats
    use w3gdatmd, only: ijkcel3
    use w3odatmd, only: ndse, ndst
    implicit none
    integer, intent( in):: nua, nub
    real,    intent( in):: cf(-9:ncel), uc(-9:ncel), akdif, fts
    real,    intent(out):: uflx(nufc), fu(nufc), fx(nufc)
    !
    integer ::  i, j, k, l, m, n, ij
    real    :: cnst, cnst0, cnst1, cnst2, cnst3, cnst4, cnst5, cnst6,  &
         cnst7, cnst8, cnst9
    !     two layer of boundary cells are added to each boundary cell face
    !     with all boundary cell values cf(-9:0)=0.0.
    !     diffusion fourier no. at sub-time-step, proportional to face size,
    !     which is also equal to the sub-time-step factor fts.
    !         cnst0=akdif*fts*fts
    !     2.0 factor to cancel that in gradient cnst5.  jgli03sep2015
    cnst0=akdif*fts*fts*2.0
    !     notice an extra side length l is multiplied to mid-flux to give correct
    !     proportion of flux into the cells.  this length will be removed by the
    !     cell length when the tracer concentration is updated.
    do i=nua, nub
      !    select upstream, central and downstream cells
      k=ijkufc(4,i)
      l=ijkufc(5,i)
      m=ijkufc(6,i)
      n=ijkufc(7,i)
      !    face bounding cell lengths and central gradient
      cnst2=float( ijkcel3(l) )
      cnst3=float( ijkcel3(m) )
      cnst5=(cf(m)-cf(l))/( cnst2 + cnst3 )
      !    courant number in local size-1 cell, arithmetic average.
      cnst6=0.5*( uc(l)+uc(m) )*fts
      uflx(i) = cnst6
      !    multi-resolution smc grid requires flux multiplied by face factor.
      cnst8 = float( ijkufc(3,i) )
      !    diffusion factor in local size-1 cell, plus the cosine factors.
      !    2.0 factor to cancel that in gradient cnst5.  jgli08mar2012
      !    the maximum cell number is used to avoid the boundary cell number
      !    in selection of the cosine factor.
      ij= max(l, m)
      !    for positive velocity case
      if(cnst6 >= 0.0)  then
        !    use central cell velocity for boundary flux.  jgli06apr2011
        if( m .le. 0) uflx(i) = uc(l)*fts
        !    upstream cell length and gradient, depending on uflx sign.
        cnst1=float( ijkcel3(k) )
        cnst4=(cf(l)-cf(k))/( cnst2 + cnst1 )
        !    second order gradient
        cnst7 = cnst5 - cnst4
        cnst9 = 2.0/( cnst3+cnst2+cnst2+cnst1 )
        !    use 3rd order scheme
        if( abs(cnst7) .lt. 0.6*cnst9*abs(cf(m)-cf(k)) ) then
          cnst= cnst5 - ( cnst3+uflx(i) )*cnst7*cnst9/1.5
          !    use doubled uno2 scheme
        else if( dble(cnst4)*dble(cnst5) .gt. 0.d0 ) then
          cnst=sign(2.0, cnst5)*min( abs(cnst4), abs(cnst5) )
        else
          !    use minimum gradient uno2 scheme
          cnst=sign(1.0, cnst5)*min( abs(cnst4), abs(cnst5) )
        endif
        !    mid-flux value inside central cell
        fu(i)=(cf(l) + cnst*(cnst2 - uflx(i)))*cnst8
        !    for negative velocity case
      else
        !    use central cell velocity for boundary flux.  jgli06apr2011
        if( l .le. 0) uflx(i) = uc(m)*fts
        !    upstream cell length and gradient, depending on uflx sign.
        cnst1=float( ijkcel3(n) )
        cnst4=(cf(n)-cf(m))/( cnst1 + cnst3 )
        !    second order gradient
        cnst7 = cnst4 - cnst5
        cnst9 = 2.0/( cnst2+cnst3+cnst3+cnst1 )
        !    use 3rd order scheme
        if( abs(cnst7) .lt. 0.6*cnst9*abs(cf(n)-cf(l)) ) then
          cnst= cnst5 + ( cnst2-uflx(i) )*cnst7*cnst9/1.5
          !    use doubled uno2 scheme
        else if( dble(cnst4)*dble(cnst5) .gt. 0.d0 ) then
          cnst=sign(2.0, cnst5)*min( abs(cnst4), abs(cnst5) )
        else
          !    use minimum gradient uno2 scheme.
          cnst=sign(1.0, cnst5)*min( abs(cnst4), abs(cnst5) )
        endif
        !    mid-flux value inside central cell m
        fu(i)=(cf(m) - cnst*(cnst3+uflx(i)))*cnst8
      endif
      !    diffusion flux by face gradient x dt
      fx(i)=cnst0*cnst5*cnst8/( clats( ij )*clats( ij ) )
    end do
    return
  end subroutine smcxuno3
  !> @brief calculate mid-flux values for y dimension with uno3 scheme
  !>
  !>
  !> @param[in]   nva      start number of v-face list.
  !> @param[in]   nvb      end number of v-face list.
  !> @param[in]   cf       transported variable.
  !> @param[in]   vc       veclocity v-component at cell centre.
  !> @param[out]  vfly     mid-flux v-component on v-face.
  !> @param[in]   akdif    diffusion coefficient.
  !> @param[out]  fv       advection mid-flux on v-face.
  !> @param[out]  fy       diffusion mid-flux on v-face.
  !> @param[in]   fts      timestep fraction for sub-timestep.
  !>
  !> @author jian-guo li
  !> @date 03-mar-2022
  !>
  subroutine smcyuno3(nva, nvb, cf, vc, vfly, akdif, fv, fy, fts)
    use constants
    use w3gdatmd, only: ncel, mrfct, nvfc, ijkcel, ijkvfc, clatf
    use w3gdatmd, only: ijkcel4
    use w3odatmd, only: ndse, ndst
    implicit none
    integer, intent( in):: nva, nvb
    real,    intent( in):: cf(-9:ncel), vc(-9:ncel), akdif, fts
    real,    intent(out):: vfly(nvfc), fv(nvfc), fy(nvfc)
    integer ::  i, j, k, l, m, n, ij
    real:: cnst, cnst0, cnst1, cnst2, cnst3, cnst4, cnst5, cnst6,  &
         cnst7, cnst8, cnst9
    !     notice an extra side length l is multiplied to mid-flux to give correct
    !     proportion of flux into the cells.  this length will be removed by the
    !     cell length when the tracer concentration is updated.
    !     diffusion fourier no. at sub-time-step, proportional to face size,
    !     which is also equal to the sub-time-step factor fts.
    !        cnst0=akdif*fts*fts
    !     2.0 factor to cancel that in gradient cnst5.  jgli08mar2012
    cnst0=akdif*fts*fts*2.0
    !     uniform diffusion coefficient for all sizes.  jgli24feb2012
    !        cnst0=akdif*mrfct*fts
    do j=nva, nvb
      !    select upstream, central and downstream cells
      k=ijkvfc(4,j)
      l=ijkvfc(5,j)
      m=ijkvfc(6,j)
      n=ijkvfc(7,j)
      !    face bounding cell lengths and gradient
      cnst2=float( ijkcel4(l) )
      cnst3=float( ijkcel4(m) )
      cnst5=(cf(m)-cf(l))/( cnst2 + cnst3 )
      !    courant number in local size-1 cell unit
      !    multiply by multi-resolution time step factor  fts
      cnst6=0.5*( vc(l)+vc(m) )*fts
      vfly(j) = cnst6
      !    face size integer and cosine factor.
      !    clatf is defined on v-face for smc grid.  jgli28feb2012
      cnst8=clatf(j)*float( ijkvfc(3,j) )
      !    for positive velocity case
      if(cnst6 >= 0.0)  then
        !    boundary cell y-size is set equal to central cell y-size
        !    as y-boundary cell sizes are not proportional to refined
        !    inner cells but constant of the base cell y-size, and
        !    use central cell speed for face speed.  jgli06apr2011
        if( m .le. 0 ) then
          vfly(j) = vc(l)*fts
          cnst3   = cnst2
        endif
        !    upstream cell size and irregular grid gradient, depending on vfly.
        cnst1=float( ijkcel4(k) )
        cnst4=(cf(l)-cf(k))/( cnst2 + cnst1 )
        !    second order gradient
        cnst7 = cnst5 - cnst4
        cnst9 = 2.0/( cnst3+cnst2+cnst2+cnst1 )
        !    use 3rd order scheme
        if( abs(cnst7) .lt. 0.6*cnst9*abs(cf(m)-cf(k)) ) then
          cnst= cnst5 - ( cnst3+vfly(j) )*cnst7*cnst9/1.5
          !    use doubled uno2 scheme
        else if( dble(cnst4)*dble(cnst5) .gt. 0.d0 ) then
          cnst=sign(2.0, cnst5)*min( abs(cnst4), abs(cnst5) )
        else
          !    use minimum gradient outside monotonic region
          cnst=sign(1.0, cnst5)*min( abs(cnst4), abs(cnst5) )
        endif
        !    mid-flux value multiplied by face width and cosine factor
        fv(j)=( cf(l) + cnst*(cnst2 - vfly(j)) )*cnst8
        !    for negative velocity case
      else
        !    set boundary cell y-size equal to central cell y-size and
        !    use central cell speed for flux face speed.  jgli06apr2011
        if( l .le. 0 ) then
          vfly(j) = vc(m)*fts
          cnst2   = cnst3
        endif
        !    upstream cell size and gradient, depending on vfly sign.
        !    side gradients for central cell includs 0.5 factor.
        cnst1=float( ijkcel4(n) )
        cnst4=(cf(n)-cf(m))/( cnst1 + cnst3 )
        !    second order gradient
        cnst7 = cnst4 - cnst5
        cnst9 = 2.0/( cnst2+cnst3+cnst3+cnst1 )
        !    use 3rd order scheme
        if( abs(cnst7) .lt. 0.6*cnst9*abs(cf(n)-cf(l)) ) then
          cnst= cnst5 + ( cnst2-vfly(j) )*cnst7*cnst9/1.5
          !    use doubled uno2 scheme
        else if( dble(cnst4)*dble(cnst5) .gt. 0.d0 ) then
          cnst=sign(2.0, cnst5)*min( abs(cnst4), abs(cnst5) )
        else
          !    use minimum gradient outside monotonic region
          cnst=sign(1.0, cnst5)*min( abs(cnst4), abs(cnst5) )
        endif
        !    mid-flux value multiplied by face width and cosine factor
        fv(j)=( cf(m) - cnst*(cnst3 + vfly(j)) )*cnst8
      endif
      !    diffusion flux by face gradient x dt x face_width x cos(lat)
      !    multiply by multi-resolution time step factor fts
      fy(j)=cnst0*cnst5*cnst8
    end do
    return
  end subroutine smcyuno3
  !> @brief calculate mid-flux values for x dimension with uno3
  !>
  !> @param[in]   nua      start number of u-face list.
  !> @param[in]   nub      end number of u-face list.
  !> @param[in]   cf       transported variable.
  !> @param[in]   uc       veclocity u-component at cell centre.
  !> @param[out]  uflx     mid-flux u-component on u-face.
  !> @param[in]   akdif    diffusion coefficient.
  !> @param[out]  fu       advection mid-flux on u-face.
  !> @param[out]  fx       diffusion mid-flux on u-face.
  !>
  !> @author jian-guo li
  !> @date 03-mar-2022
  !>
  subroutine smcxuno3r(nua, nub, cf, uc, uflx, akdif, fu, fx)
    use constants
    use w3gdatmd, only: nsea, ny, ncel, nufc, ijkcel, ijkufc, clats
    use w3gdatmd, only: ijkcel3
    use w3odatmd, only: ndse, ndst
    implicit none
    integer, intent( in):: nua, nub
    real,    intent( in):: cf(-9:ncel), uc(-9:ncel), akdif
    real,    intent(out):: uflx(nufc), fu(nufc), fx(nufc)
    !
    integer ::  i, j, k, l, m, n, ij
    real:: cnst, cnst0, cnst1, cnst2, cnst3, cnst4, cnst5, cnst6, &
         cnst7, cnst8, cnst9
    !    two layer of boundary cells are added to each boundary cell face
    !    with all boundary cell values cf(-9:0)=0.0.
    !    notice an extra side length l is multiplied to mid-flux to give correct
    !    proportion of flux into the cells.  this length will be removed by the
    !    cell length when the tracer concentration is updated.
    do i=nua, nub
      !    select upstream, central and downstream cells
      k=ijkufc(4,i)
      l=ijkufc(5,i)
      m=ijkufc(6,i)
      n=ijkufc(7,i)
      !    face bounding cell lengths and gradient
      cnst2=float( ijkcel3(l) )
      cnst3=float( ijkcel3(m) )
      cnst5=(cf(m)-cf(l))
      !    averaged courant number for base-level cell face
      cnst6= 0.5*( uc(l)+uc(m) )
      uflx(i) = cnst6
      !    diffusion fourier number in local cell size
      !    to avoid boundary cell number, use maximum of l and m.
      ij= max(l, m)
      cnst0 = 2.0/( clats(ij)*clats(ij) )
      !    for positive velocity case
      if(cnst6 >= 0.0)  then
        !    use central cell velocity for boundary flux.  jgli06apr2011
        if( m .le. 0) uflx(i) = uc(l)
        !    side gradient for upstream cell as regular grid.
        cnst4=(cf(l)-cf(k))
        cnst8=(cf(m)-cf(k))
        cnst9=(cnst5-cnst4)
        if( abs(cnst9) <= 0.6*abs(cnst8) )  then
          !    use 3rd order scheme in limited zone, note division by 2 grid sizes
          cnst=0.5*cnst5 - (1.0+uflx(i)/cnst2)*cnst9/6.0
        else if( dble(cnst4)*dble(cnst5) .gt. 0.d0 ) then
          !    use doubled minimum gradient in rest of monotonic region
          cnst=sign(1.0, cnst5)*min( abs(cnst4), abs(cnst5) )
        else
          !    use minimum gradient all region with 0.5 factor
          cnst=sign(0.5, cnst5)*min( abs(cnst4), abs(cnst5) )
        endif
        !    mid-flux value inside central cell
        fu(i)=(cf(l) + cnst*(1.0-uflx(i)/cnst2))
        !    for negative velocity case
      else
        !    use central cell velocity for boundary flux.  jgli06apr2011
        if( l .le. 0) uflx(i) = uc(m)
        !    side gradient for upstream cell, depneding on uflx sign.
        cnst4=(cf(n)-cf(m))
        cnst8=(cf(n)-cf(l))
        cnst9=(cnst4-cnst5)
        if( abs(cnst9) <= 0.6*abs(cnst8) )  then
          !    use 3rd order scheme in limited zone, note division by 2 grid sizes
          cnst=0.5*cnst5 + (1.0-uflx(i)/cnst3)*cnst9/6.0
        else if( dble(cnst4)*dble(cnst5) .gt. 0.d0 ) then
          !    use doubled minimum gradient in rest of monotonic region
          cnst=sign(1.0, cnst5)*min( abs(cnst4), abs(cnst5) )
        else
          !    use minimum gradient outside monotonic region, include 0.5 factor
          cnst=sign(0.5, cnst5)*min( abs(cnst4), abs(cnst5) )
        endif
        !    mid-flux value inside central cell m
        fu(i)=(cf(m) - cnst*(1.0+uflx(i)/cnst3))
      endif
      !    diffusion flux by face gradient x dt
      fx(i)=akdif*cnst0*cnst5/(cnst2 + cnst3)
    end do
    ! 999  print*, ' sub smcxuno3r ended.'
    return
  end subroutine smcxuno3r
  !> @brief calculate mid-flux values for y dimension with uno3
  !>
  !> @param[in]   nva      start number of v-face list.
  !> @param[in]   nvb      end number of v-face list.
  !> @param[in]   cf       transported variable.
  !> @param[in]   vc       veclocity v-component at cell centre.
  !> @param[out]  vfly     mid-flux v-component on v-face.
  !> @param[in]   akdif    diffusion coefficient.
  !> @param[out]  fv       advection mid-flux on v-face.
  !> @param[out]  fy       diffusion mid-flux on v-face.
  !>
  !> @author jian-guo li
  !> @date 03-mar-2022
  !>
  subroutine smcyuno3r(nva, nvb, cf, vc, vfly, akdif, fv, fy)
    use constants
    use w3gdatmd, only: nsea, ny, ncel, nvfc, ijkcel, ijkvfc, clatf
    use w3odatmd, only: ndse, ndst
    implicit none
    integer, intent( in):: nva, nvb
    real,    intent( in):: cf(-9:ncel), vc(-9:ncel), akdif
    real,    intent(out):: vfly(nvfc), fv(nvfc), fy(nvfc)
    integer ::  i, j, k, l, m, n, ij
    real    :: cnst, cnst0, cnst1, cnst2, cnst3, cnst4, cnst5, cnst6, &
         cnst7, cnst8, cnst9
    !     notice an extra side length l is multiplied to mid-flux to give correct
    !     proportion of flux into the cells.  this length will be removed by the
    !     cell length when the tracer concentration is updated.
    do j=nva, nvb
      !    select upstream, central and downstream cells
      k=ijkvfc(4,j)
      l=ijkvfc(5,j)
      m=ijkvfc(6,j)
      n=ijkvfc(7,j)
      !    central face gradient.
      cnst5=(cf(m)-cf(l))
      !    courant number in basic cell unit as dy is constant
      cnst6=0.5*( vc(l)+vc(m) )
      vfly(j) = cnst6
      !    face size integer and cosine factor
      !    clatf is defined on v-face for smc grid.  jgli28feb2012
      cnst7=clatf(j)*float( ijkvfc(3,j) )
      !    for positive velocity case
      if(cnst6 >= 0.0)  then
        !    use central cell speed for flux face speed.  jgli06apr2011
        if( m .le. 0 ) vfly(j) = vc(l)
        !    upstream face gradient, depending on vfly sign.
        cnst4=(cf(l)-cf(k))
        !    second gradient for 3rd scheme
        cnst8=(cf(m)-cf(k))
        cnst9=(cnst5-cnst4)
        if( abs(cnst9) <= 0.6*abs(cnst8) )  then
          !    use 3rd order scheme in limited zone, note division by 2 grid sizes
          cnst=0.5*cnst5-(1.0+vfly(j))*cnst9/6.0
        else if( dble(cnst4)*dble(cnst5) .gt. 0.d0 ) then
          !    use doubled minimum gradient in rest of monotonic region
          cnst=sign(1.0, cnst5)*min( abs(cnst4), abs(cnst5) )
        else
          !    use minimum gradient, including 0.5 factor and central sign.
          cnst=sign(0.5, cnst5)*min( abs(cnst4), abs(cnst5) )
        endif
        !    mid-flux value multiplied by face width and cosine factor
        fv(j)=( cf(l) + cnst*(1.0-vfly(j)) )*cnst7
        !    for negative velocity case
      else
        !    use central cell speed for flux face speed.  jgli06apr2011
        if( l .le. 0 ) vfly(j) = vc(m)
        !    side gradients for upstream face, depending on vfly sign.
        cnst4=(cf(n)-cf(m))
        !    second gradient for 3rd scheme
        cnst8=(cf(n)-cf(l))
        cnst9=(cnst4-cnst5)
        if( abs(cnst9) <= 0.6*abs(cnst8) )  then
          !    use 3rd order scheme in limited zone, note division by 2 grid sizes
          cnst=0.5*cnst5+(1.0-vfly(j))*cnst9/6.0
        else if( dble(cnst4)*dble(cnst5) .gt. 0.d0 ) then
          !    use doubled minimum gradient in rest of monotonic region
          cnst=sign(1.0, cnst5)*min( abs(cnst4), abs(cnst5) )
        else
          !    use minimum gradient, including 0.5 factor and central sign.
          cnst=sign(0.5, cnst5)*min( abs(cnst4), abs(cnst5) )
        endif
        !    mid-flux value multiplied by face width and cosine factor
        fv(j)=( cf(m) - cnst*(1.0+vfly(j)) )*cnst7
      endif
      !    diffusion flux by face gradient x dt x face_width x cos(lat)
      fy(j)=akdif*cnst5*cnst7
    end do
    return
  end subroutine smcyuno3r
  !
  !> @brief evaluate local gradient for sea points.
  !>
  !> @details
  !>  calculate cell centre gradient for any input variable.
  !>  nemerical average is applied to size-changing faces and the gradients
  !>  are along the lat-lon local east-north directions.
  !>
  !>
  !> @param[in]   cvq      input cell values.
  !> @param[out]  grdx     gradient along x-axis.
  !> @param[out]  grdy     gradient along y-axis.
  !> @param[in]   l0r1     zero or 1st-order boundary condiiton.
  !>
  !> @author jian-guo li
  !> @date 08 aug 2017
  !>
  ! add optional zero-gradient bounday conditions.    jgli08aug2017
  !
  subroutine smcgradn(cvq, grdx, grdy, l0r1)
    use constants
    use w3gdatmd, only: nsea,   nufc,   nvfc,   mrfct,       &
         ijkcel, ijkufc, ijkvfc, clats, sx, sy
    use w3gdatmd, only: arctc
    use w3odatmd, only: ndse, ndst
    implicit none
    !!    new boundary conditions depending on user defined l0r1.
    !!    l0r1 = 0 will set zero at land points while l0r1 > 0 invokes
    !!    the zero-gradient boundary condition.    jgli08aug2017
    real,    intent( in)::  cvq(nsea)
    real,    intent(out):: grdx(nsea), grdy(nsea)
    integer, intent( in):: l0r1
    !
    integer :: i, j, k, l, m, n
    real:: cnst, cnst0, cnst1, cnst2, cnst3, cnst4, cnst5, cnst6
    real :: dx0i, dy0i
    !     use a few working arrays
    real,  dimension(-9:nsea):: cvf, aun, avn
    !     two layer of boundary cells are added to each boundary cell face
    !     with all boundary cell default values cvf(-9:0)= 0.0.
    cvf(-9:0)  = 0.0
    cvf(1:nsea)=cvq(1:nsea)
    !!    initialize arrays
    aun = 0.
    avn = 0.
    grdx = 0.
    grdy = 0.
    !!    multi-resolution base-cell size defined by refined levels.
    !!    so the mrfct converts the base cell sx, sy into size-1 cell lenth.
    !!    constant size-1 dy=dy0 and dx on equator dx0, inverted.
    dx0i   = mrfct/ ( sx * dera * radius )
    dy0i   = mrfct/ ( sy * dera * radius )
    !!    calculate x-gradient by averaging u-face gradients.
    do i=1, nufc
      !     select upstream, central and downstream cells
      l=ijkufc(5,i)
      m=ijkufc(6,i)
      !!      for zero-gradient boundary conditions, simply skip boundary faces.
      if( l0r1 .eq. 0 .or. (l > 0 .and. m > 0) ) then
        !         multi-resolution smc grid requires flux multiplied by face factor.
        cnst1=float( ijkufc(3,i) )
        !         face bounding cell lengths and central gradient
        cnst2=float( ijkcel(3,l) )
        cnst3=float( ijkcel(3,m) )
        !         side gradients over 2 cell lengths for central cell.
        !         face size factor is also included for average.
        cnst5=cnst1*(cvf(m)-cvf(l))/(cnst2+cnst3)
        !! replace critical with atomic.  jgli15jan2019
        !! !$omp critical
        !    store side gradient in two neighbouring cells
        !! remove boundary cell flux update or l m > 0.  jgli28mar2019
        if( l > 0 ) then
          aun(l) = aun(l) + cnst5
        endif
        if( m > 0 ) then
          aun(m) = aun(m) + cnst5
        endif
        !! !$omp end critical
      endif
    end do
    !  assign averaged side-gradient to grdx, plus latitude factor
    !  note averaging over 2 times of cell y-width factor but aun
    !  has already been divied by two cell lengths.
    do n=1, nsea
      !       cell y-size ijkcel(4,i) is used to cancel the face size-factor in aun.
      !       plus the actual physical length scale for size-1 cell.
      !       note polar cell (if any) aun = 0.0 as it has no u-face.
      grdx(n)=dx0i*aun(n)/( clats(n)*ijkcel(4,n) )
    enddo
    !!    calculate y-gradient by averaging v-face gradients.
    do j=1, nvfc
      !       select central and downstream cells
      l=ijkvfc(5,j)
      m=ijkvfc(6,j)
      !!      for zero-gradient boundary conditions, simply skip boundary faces.
      if( l0r1 .eq. 0 .or. (l > 0 .and. m > 0) ) then
        !       face size is required for multi-resolution grid.
        cnst1=real( ijkvfc(3,j) )
        !         cell y-length of ucd cells
        cnst2=real( ijkcel(4,l) )
        cnst3=real( ijkcel(4,m) )
        !         side gradients over 2 cell lengths for central cell.
        !         face size factor is also included for average.
        cnst6=cnst1*(cvf(m)-cvf(l))/(cnst2+cnst3)
        !! replace critical with atomic.  jgli15jan2019
        !! !$omp critical
        !! remove boundary cell flux update or l m > 0.  jgli28mar2019
        if( l > 0 ) then
          !    store side gradient in two neighbouring cells
          avn(l) = avn(l) + cnst6
        endif
        if( m > 0 ) then
          avn(m) = avn(m) + cnst6
        endif
        !! !$omp end critical
      endif
    end do
    !  assign averaged side-gradient to grdy.
    do n=1, nsea
      !  av is divided by the cell x-size ijkcel(3,i) to cancel face
      !  size-factor, and physical y-distance of size-1 cell.
      grdy(n)=dy0i*avn(n)/real( ijkcel(3,n) )
    end do
    !!li  y-gradient for polar cell in arctic part is set to zero.
    if( arctc ) grdy(nsea) = 0.0
    return
  end subroutine smcgradn
  !> @brief average sea point values with a 1-2-1 scheme.
  !>
  !> @param[inout]  cvq  input field.
  !>
  !> @author jian-guo li
  !> @date 15-jan-2019
  !>
  subroutine smcaverg(cvq)
    use constants
    use w3gdatmd, only: nsea,   nufc,   nvfc,    &
         ijkcel, ijkufc, ijkvfc,  &
         ijkufc5, ijkufc6
    use w3gdatmd, only: arctc
    use w3odatmd, only: ndse, ndst
    implicit none
    real, intent(inout) :: cvq(-9:nsea)
    !
    integer :: i, j, k, l, m, n
    real :: cnst, cnst0, cnst1, cnst2, cnst3, cnst4, cnst5, cnst6
    !     use a few working arrays
    real, dimension(-9:nsea) :: cvf, aun, avn
    !     two layer of boundary cells are added to each boundary cell face
    !     with all boundary cell values stored in cvf(-9:0).
    cvf=cvq
    !!    initialize arrays
    aun = 0.
    avn = 0.
    !!li  save polar cell value if any.
    cnst0 = cvq(nsea)
    !!    calculate x-gradient by averaging u-face gradients.
    do i=1, nufc
      !    select upstream, central and downstream cells
      l=ijkufc5(i)
      m=ijkufc6(i)
      !       multi-resolution smc grid requires flux multiplied by face factor.
      cnst5=real( ijkufc(3,i) )*(cvf(m)+cvf(l))
      !! replace critical with atomic.  jgli15jan2019
      !! !$omp critical
      !    store side gradient in two neighbouring cells
      !! remove boundary cell flux update or l m > 0.  jgli28mar2019
      if( l > 0 ) then
        aun(l) = aun(l) + cnst5
      endif
      if( m > 0 ) then
        aun(m) = aun(m) + cnst5
      endif
      !! !$omp end critical
    end do
    !!    calculate y-gradient by averaging v-face gradients.
    do j=1, nvfc
      !       select central and downstream cells
      l=ijkvfc(5,j)
      m=ijkvfc(6,j)
      !       face size is required for multi-resolution grid.
      cnst6=real( ijkvfc(3,j) )*(cvf(m)+cvf(l))
      !! replace critical with atomic.  jgli15jan2019
      !! !$omp critical
      !    store side gradient in two neighbouring cells
      !! remove boundary cell flux update or l m > 0.  jgli28mar2019
      if( l > 0 ) then
        avn(l) = avn(l) + cnst6
      endif
      if( m > 0 ) then
        avn(m) = avn(m) + cnst6
      endif
      !! !$omp end critical
    end do
    !  assign averaged value back to cvq.
    do n=1, nsea
      cnst3=0.125/real( ijkcel(3,n) )
      cnst4=0.125/real( ijkcel(4,n) )
      !       aun is divided by the cell y-size ijkcel(4,n) and avn by
      !       the cell x-size ijkcel(3,n) to cancel face size factors.
      cvq(n)= aun(n)*cnst4 + avn(n)*cnst3
    end do
    !!li  polar cell (if any) keep original value.
    if( arctc ) cvq(nsea) = cnst0
    ! 999  print*, ' sub smcaverg ended.'
    return
  end subroutine smcaverg
  !> @brief calculate great circle turning (gct) and refraction.
  !>
  !> @details
  !>  the refraction and gct terms are equivalent to a single rotation by each
  !>  element and does not need to be calculated as advection.  a simple rotation
  !>  scheme similar to the 1st order upstream scheme but without any restriction
  !>  on the rotation angle or the cfl limit by an eulerian advection scheme.
  !>
  !> @param[in]  corfr   courant number for refraction and gct rotation.
  !> @param[in]  spethk  wave spectrum to be rotated and output.
  !>
  !> @author jian-guo li
  !> @date 12 nov 2010
  !>
  subroutine smcgtcrfr(corfr, spethk)
    use constants
    use w3gdatmd, only: nk, nth, dth, ctmax
    implicit none
    real, intent(in)   ::  corfr(nth, nk)
    real, intent(inout):: spethk(nth, nk)
    integer ::  i, j, k, l, m, n
    real, dimension(nth):: spegct, spectr
    real:: cnst, cnst0, cnst1, cnst2, cnst3, cnst4, cnst5, cnst6
    !     loop through nk spectral bins.
    do n=1, nk
      !!      asign cell spectrum to temporary variable spcetr
      spectr=spethk(1:nth,n)
      spegct=0.0
      !!      loop through nth directional bins for each cell spectrum
      do j=1, nth
        !         gct + refraction courant number for this dirctional bin
        cnst6=corfr(j,n)
        !         work out integer number of bins to be skipped.
        !         if k is great than nth, full circle turning is removed.
        cnst5=abs( cnst6 )
        k= mod( int(cnst5), nth )
        !         partitioning faraction of the spectral component
        cnst1=cnst5 - float( int(cnst5) )
        cnst2=1.0 - cnst1
        !         for positive turning case
        if(cnst6 > 0.0)  then
          !           select the upstream and downstream bins to rotate in, wrap at end
          l=j+k
          m=j+k+1
          if( l .gt. nth ) l = l - nth
          if( m .gt. nth ) m = m - nth
          !!          divide the j bin energy by fraction of cnst6 and store in spegct
          spegct(l)=spegct(l)+spectr(j)*cnst2
          spegct(m)=spegct(m)+spectr(j)*cnst1
          !         for negative or no turning case
        else
          !           select the upstream and downstream bins to rotate in, wrap at end
          l=j-k
          m=j-k-1
          if( l .lt. 1 ) l = l + nth
          if( m .lt. 1 ) m = m + nth
          !!          divide the bin energy by fraction of cnst6 and store in spegct
          spegct(l)=spegct(l)+spectr(j)*cnst2
          spegct(m)=spegct(m)+spectr(j)*cnst1
        endif
        !!      end of directional loop j
      end do
      !!      store gct spectrum
      spethk(1:nth,n) = spegct
      !!    end of cell loop n
    end do
    ! 999  print*, ' sub smcgtcrfr ended.'
    return
  end subroutine smcgtcrfr
  !>
  !> @brief calculates refraction induced shift in k-space.
  !>
  !> @details
  !>  the term is equivalent to advection on an irregular k-space grid.
  !>  the uno2 scheme on irregular grid is used for this term.
  !>
  !>  cell and side indices for k-dimension are arranged as:
  !>  @verbatim
  !>    cell:    | -1 | 0 | 1 | 2 | ... | nk | nk+1 | nk+2 |
  !>    side:        -1   0   1   2 ...     nk     nk+1
  !>  @endverbatim
  !>  the wave action in k-space is extended at the high-wavenumber
  !>  (frequency) end by the (m+2)th negative power of frequency for
  !>  boundary conditions.  outside low-wavenumber (frequncy) end, wave
  !>  action is assumed to be zero.
  !>
  !> @param[in]    corfr      courant number for refraction k-shift.
  !> @param[inout] spethk     spectrum to be shifted and output.
  !> @param[in]    dkc        wave number increment at k-bin centre.
  !> @param[in]    dks        wave number increment at k-bin edges.
  !>
  !>
  !> @author jian-guo li
  !> @date 15 nov 2010
  !
  ! fix bug on cfl limiter and add positive filter.  jgli28jun2017
  !
  subroutine smckuno2(corfr, spethk, dkc, dks)
    use constants
    use w3gdatmd, only: nk, nk2, nth, dth, xfr, ctmax
    implicit none
    real, intent(in)   ::  corfr(nth, 0:nk), dkc(0:nk+1), dks(-1:nk+1)
    real, intent(inout):: spethk(nth, nk)
    integer ::  i, j, k, l, m, n
    real, dimension(-1:nk+2):: sperfr, spectr, speflx
    real:: cnst, cnst0, cnst1, cnst2, cnst3, cnst4, cnst5, cnst6
    cnst=xfr**(-7)
    do n=1, nth
      !!      asign cell spectrum to temporary variable spcetr
      spectr(-1)  =0.0
      spectr( 0)  =0.0
      spectr(1:nk)=spethk(n,1:nk)
      spectr(nk+1)=spectr(nk  )*cnst
      spectr(nk+2)=spectr(nk+1)*cnst
      !!      calculate k-space gradient for nk+2 faces by uno2 scheme
      sperfr(-1)= 0.0
      do j=-1, nk+1
        sperfr(j)=(spectr(j+1)-spectr(j))/dks(j)
      enddo
      !!      calculate k-space fluxes for nk+1 faces by uno2 scheme
      do j=0, nk
        !         final refraction courant number for this k-space face
        cnst6=corfr(n,j)
        !!        note corfr is cfl for k but without dividing dk.
        !         for positive case
        if(cnst6 > 0.0)  then
          cnst0 = min( ctmax*dkc(j), cnst6 )
          speflx(j) = cnst0*( spectr(j) + sign(0.5, sperfr(j))*(dkc(j)-cnst0)  &
               *min( abs(sperfr(j-1)), abs(sperfr(j)) ) )
          !         for negative or no turning case
        else
          cnst0 = min( ctmax*dkc(j+1), -cnst6 )
          speflx(j) = -cnst0*( spectr(j+1) - sign(0.5, sperfr(j))*(dkc(j+1)-cnst0) &
               *min( abs(sperfr(j+1)), abs(sperfr(j)) ) )
        endif
        !!      end of flux loop j
      end do
      !!      update spectrum for the given direction
      do j=1, nk
        !         final refraction courant number for this k-space face
        !         spethk(n, j) = spectr(j) + (speflx(j-1) - speflx(j))/dkc(j)
        !         add positive filter in case negative values.  jgli27jun2017
        spethk(n, j) = max( 0.0, spectr(j)+(speflx(j-1)-speflx(j))/dkc(j) )
      end do
      !!    end of directional loop n
    end do
    ! 999   print*, ' sub smckuno2 ended.'
    return
  end subroutine smckuno2
  !> @brief calculates water-depth gradient for refraction.
  !>
  !> @details
  !>  for consistency with the lat-lon grid, full grid dddx, dddy are
  !>  also assigned here.  dhdx, dhdy are used for refraction at present.
  !>  it has to be rotated to map-east system in the arctic part.
  !>
  !> @author jian-guo li
  !> @date 03-mar-2022
  !>
  subroutine smcdhxy
    use constants
    use w3gdatmd, only: nx, ny, nsea, mapsta, mapfs, mrfct, ijkcel,  &
         clats, nth, dth, esin, ecos, refran, dmin
    use w3gdatmd, only: nglo, angarc, arctc
    use w3adatmd, only: dw, dddx, dddy, dhdx, dhdy, dhlmt
    use w3odatmd, only: ndse, ndst
    implicit none
    integer :: i, j, k, l, m, n
    real :: cnst, cnst0, cnst1, cnst2, cnst3, cnst4, cnst5, cnst6
    real, dimension(nsea) :: hcel, grhx, grhy
    !     real, dimension(-9:nsea) :: hcel
    !!    assign water depth to hcel from dw integer values.
    !!    set half the minimum depth dmin for negative cells.
    !     hcel(-9:0) = 0.5*dmin
    hcel(1:nsea)= dw(1:nsea)
    !!    reset shallow water depth with minimum depth
    do k=1, nsea
      if(dw(k) .lt. dmin)  hcel(k)=dmin
    enddo
    !!    initialize full grid gradient arrays
    dddx = 0.
    dddy = 0.
    !!    use zero-gradient boundary condition or l0r1 > 0
    l = 1
    !!    calculate sea point water depth gradient
    call smcgradn(hcel, grhx, grhy, l)
    !!    pass gradient values to dhdx, dhdy
    dhdx(1:nsea) = grhx
    dhdy(1:nsea) = grhy
    !!   apply limiter to depth-gradient and copy to full grid.
    do n=1,nsea
      !  a limiter of gradient <= 0.1 is applied.
      if( abs( dhdx(n) ) .gt.  0.1) dhdx(n)=sign( 0.1, dhdx(n) )
      if( abs( dhdy(n) ) .gt.  0.1) dhdy(n)=sign( 0.1, dhdy(n) )
      !! asign dhdx value to full grid variable dddx
      i= ijkcel(1,n)/mrfct + 1
      j= ijkcel(2,n)/mrfct + 1
      k= max(1, ijkcel(3,n)/mrfct)
      m= max(1, ijkcel(4,n)/mrfct)
      dddx(j:j+m-1,i:i+k-1)  = dhdx(n)
      dddy(j:j+m-1,i:i+k-1)  = dhdy(n)
      !li  depth gradient in the arctic part has to be rotated into
      !li  the map-east system for calculation of refraction.
      if( arctc .and. n .gt. nglo ) then
        cnst0 = angarc(n - nglo)*dera
        cnst1 = dhdx(n)*cos(cnst0) - dhdy(n)*sin(cnst0)
        cnst2 = dhdx(n)*sin(cnst0) + dhdy(n)*cos(cnst0)
        dhdx(n) = cnst1
        dhdy(n) = cnst2
      endif
    end do
    !! calculate the depth gradient limiter for refraction.
    do n=1,nsea
      !li   work out magnitude of depth gradient
      cnst4 = 1.0001*sqrt(dhdx(n)*dhdx(n) + dhdy(n)*dhdy(n))
      !
      !li   directional depedent depth gradient limiter.  jgli16jun2011
      if ( cnst4 .gt. 1.0e-5 ) then
        l = l + 1       !cb - added t switch
        do i=1, nth
          !li       refraction is done only when depth gradient is non-zero.
          !li       note acos returns value between [0, pi), always positive.
          cnst6 = acos(-(dhdx(n)*ecos(i)+dhdy(n)*esin(i))/cnst4 )
          !li   user-defined refraction limiter added.   jgli09jan2012
          dhlmt(i,n)=min(refran, 0.75*min(cnst6,abs(pi-cnst6)))/dth
        end do
        !li   output some values for inspection.  jgli22jul2011
      else
        dhlmt(:,n) = 0.0
      endif
    enddo
    return
  end subroutine smcdhxy
  !> @brief calculates current velocity gradient for refraction.
  !>
  !> @details
  !>  for consistency with the lat-lon grid, full grid dcxdxy, dcydxy are
  !>  assigned here.  they are rotated to map-east system in the arctic part.
  !>
  !> @author jian-guo li
  !> @date 23 mar 2016
  !>
  subroutine smcdcxy
    use constants
    use w3gdatmd, only: nx, ny, nsea, mapsta, mapfs, mrfct, ijkcel
    use w3gdatmd, only: nglo, angarc, arctc
    use w3adatmd, only: cx, cy, dcxdx, dcxdy, dcydx, dcydy
    use w3odatmd, only: ndse, ndst
    implicit none
    integer :: i, j, k, l, m, n
    real :: cnst, cnst0, cnst1, cnst2, cnst3, cnst4, cnst5, cnst6
    real, dimension(nsea) :: cxcy, grhx, grhy
    !     real, dimension(-9:nsea) :: cxcy
    !!    assign current cx speed to cxcy and set negative cells.
    !     cxcy(-9:0) = 0.0
    !!    use zero-gradient boundary condition or l0r1 > 0
    l = 1
    cxcy(1:nsea)= cx(1:nsea)
    !!   initialize full grid gradient arrays
    dcxdx = 0.0
    dcxdy = 0.0
    !!    calculate sea point water depth gradient
    call smcgradn(cxcy, grhx, grhy, l)
    !!   apply limiter to cx-gradient and copy to full grid.
    do n=1,nsea
      !       a limiter of gradient <= 0.01 is applied.
      if( abs( grhx(n) ) .gt.  0.01) grhx(n)=sign( 0.01, grhx(n) )
      if( abs( grhy(n) ) .gt.  0.01) grhy(n)=sign( 0.01, grhy(n) )
      !li     current gradient in the arctic part has to be rotated into
      !li     the map-east system for calculation of refraction.
      if( arctc .and. n .gt. nglo ) then
        cnst0 = angarc(n - nglo)*dera
        cnst1 = grhx(n)*cos(cnst0) - grhy(n)*sin(cnst0)
        cnst2 = grhx(n)*sin(cnst0) + grhy(n)*cos(cnst0)
        grhx(n) = cnst1
        grhy(n) = cnst2
      endif
      !! asign cx gradients to full grid variable dcxdx/y
      i= ijkcel(1,n)/mrfct + 1
      j= ijkcel(2,n)/mrfct + 1
      k= max(1, ijkcel(3,n)/mrfct)
      m= max(1, ijkcel(4,n)/mrfct)
      dcxdx(j:j+m-1,i:i+k-1)  = grhx(n)
      dcxdy(j:j+m-1,i:i+k-1)  = grhy(n)
    end do
    !!    assign current cy speed to cxcy and set negative cells.
    !     cxcy(-9:0) = 0.0
    !!    use zero-gradient boundary condition or l0r1 > 0
    l = 1
    cxcy(1:nsea)= cy(1:nsea)
    !!    initialize full grid gradient arrays
    dcydx = 0.0
    dcydy = 0.0
    !!    calculate sea point water depth gradient
    call smcgradn(cxcy, grhx, grhy, l)
    !!    apply limiter to cx-gradient and copy to full grid.
    do n=1,nsea
      !!      a limiter of gradient <= 0.1 is applied.
      if( abs( grhx(n) ) .gt.  0.01) grhx(n)=sign( 0.01, grhx(n) )
      if( abs( grhy(n) ) .gt.  0.01) grhy(n)=sign( 0.01, grhy(n) )
      !!      current gradient in the arctic part has to be rotated into
      !!      the map-east system for calculation of refraction.
      if( arctc .and. n .gt. nglo ) then
        cnst0 = angarc(n - nglo)*dera
        cnst1 = grhx(n)*cos(cnst0) - grhy(n)*sin(cnst0)
        cnst2 = grhx(n)*sin(cnst0) + grhy(n)*cos(cnst0)
        grhx(n) = cnst1
        grhy(n) = cnst2
      endif
      !!      asign cx gradients to full grid variable dcxdx/y
      i= ijkcel(1,n)/mrfct + 1
      j= ijkcel(2,n)/mrfct + 1
      k= max(1, ijkcel(3,n)/mrfct)
      m= max(1, ijkcel(4,n)/mrfct)
      dcydx(j:j+m-1,i:i+k-1)  = grhx(n)
      dcydy(j:j+m-1,i:i+k-1)  = grhy(n)
    end do
    return
  end subroutine smcdcxy
  !/
  !/ ------------------------------------------------------------------- /
  !> @brief smc version of w3gath
  !>
  !> @details
  !>  gather spectral bin information into a propagation field array.
  !>  direct copy or communication calls (mpp version).
  !>
  !> @remarks
  !>  - the field is extracted but not converted.
  !>  - array field is not initialized.
  !>  - mpi version requires posing of send and receive calls in
  !>    w3wave to match local calls.
  !>  - mpi version does not require an mpi_testall call for the
  !>    posted gather operation as mpi_waitall is mandatory to
  !>    reset persistent communication for next time step.
  !>  - mpi version allows only two new pre-fetch postings per
  !>    call to minimize chances to be slowed down by gathers that
  !>    are not yet needed, while maximizing the pre-loading
  !>    during the early (low-frequency) calls to the routine
  !>    where the amount of calculation needed for proagation is
  !>    the largest.
  !>
  !> @param[in]   ispec   spectral bin considered
  !> @param[out]  field   full field to be propagated
  !>
  !> @author jian-guo li
  !> @date 15 mar 2011
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3gathsmc ( ispec, field )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch-iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         13-jun-2006 |
    !/                  +-----------------------------------+
    !/
    !/    04-jan-1999 : distributed fortran 77 version.     ( version 1.18 )
    !/    13-jan-2000 : upgrade to fortran 90               ( version 2.00 )
    !/                  major changes to logistics.
    !/    29-dec-2004 : multiple grid version.              ( version 3.06 )
    !/    13-jun-2006 : split store in g/sstore             ( version 3.09 )
    !/     9-dec-2010 : adapted for smc grid propagtion. jgli
    !/
    !  1. purpose :
    !
    !     gather spectral bin information into a propagation field array.
    !
    !  2. method :
    !
    !     direct copy or communication calls (mpp version).
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       ispec   int.   i   spectral bin considered.
    !       field   r.a.   o   full field to be propagated.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !
    !      mpi_startall, mpi_waitall
    !                subr. mpif.h   mpi persistent comm. routines (!/mpi).
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3wave    subr. w3wavemd actual wave model routine.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !       none.
    !
    !  7. remarks :
    !
    !     - the field is extracted but not converted.
    !     - array field is not initialized.
    !     - mpi version requires posing of send and receive calls in
    !       w3wave to match local calls.
    !     - mpi version does not require an mpi_testall call for the
    !       posted gather operation as mpi_waitall is mandatory to
    !       reset persistent communication for next time step.
    !     - mpi version allows only two new pre-fetch postings per
    !       call to minimize chances to be slowed down by gathers that
    !       are not yet needed, while maximizing the pre-loading
    !       during the early (low-frequency) calls to the routine
    !       where the amount of calculation needed for proagation is
    !       the largest.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/shrd  switch for message passing method.
    !     !/mpi   id.
    !
    !     !/s     enable subroutine tracing.
    !     !/mpit  mpi test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use w3gdatmd, only: nspec, nx, ny, nsea, nseal, ncel, mapsf
    use w3wdatmd, only: a => va
    use w3adatmd, only: mpibuf, bstat, ibfloc, isploc, bispl, &
         nsploc, nrqsg2, irqsg2, gstore
    use w3odatmd, only: ndst, iaproc, naproc
    !/
    implicit none
    !
    include "mpif.h"
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: ispec
    real,    intent(out)    :: field(ncel)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: status(mpi_status_size,nspec),  &
         ioff, ierr_mpi, jsea, isea,     &
         ixy, is0, ib0, npst, j
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !      field  = 0.
    !
    ! 1.  shared memory version ------------------------------------------ /
    !
    !
    ! 2.  distributed memory version ( mpi ) ----------------------------- /
    ! 2.a update counters
    !
    isploc = isploc + 1
    ibfloc = ibfloc + 1
    if ( ibfloc .gt. mpibuf ) ibfloc = 1
    !
    ! 2.b check status of present buffer
    ! 2.b.1 scatter (send) still in progress, wait to end
    !
    if ( bstat(ibfloc) .eq. 2 ) then
      ioff =  1 + (bispl(ibfloc)-1)*nrqsg2
      if ( nrqsg2 .gt. 0 ) call                              &
           mpi_waitall ( nrqsg2, irqsg2(ioff,2),             &
           status, ierr_mpi )
      bstat(ibfloc) = 0
    end if
    !
    ! 2.b.2 gather (recv) not yet posted, post now
    !
    if ( bstat(ibfloc) .eq. 0 ) then
      bstat(ibfloc) = 1
      bispl(ibfloc) = isploc
      ioff =  1 + (isploc-1)*nrqsg2
      if ( nrqsg2 .gt. 0 ) call                              &
           mpi_startall ( nrqsg2, irqsg2(ioff,1), ierr_mpi )
    end if
    !
    ! 2.c put local spectral densities in store
    !
    do jsea=1, nseal
      gstore(iaproc+(jsea-1)*naproc,ibfloc) = a(ispec,jsea)
    end do
    !
    ! 2.d wait for remote spectral densities
    !
    ioff =  1 + (bispl(ibfloc)-1)*nrqsg2
    if ( nrqsg2 .gt. 0 ) call                                  &
         mpi_waitall ( nrqsg2, irqsg2(ioff,1), status, ierr_mpi )
    !
    ! 2.e convert storage array to field.
    !
    do isea=1, nsea
      field(isea) = gstore(isea,ibfloc)
    end do
    !
    ! 2.f pre-fetch data in available buffers
    !
    is0    = isploc
    ib0    = ibfloc
    npst   = 0
    do j=1, mpibuf-1
      is0    = is0 + 1
      if ( is0 .gt. nsploc ) exit
      ib0    = 1 + mod(ib0,mpibuf)
      if ( bstat(ib0) .eq. 0 ) then
        bstat(ib0) = 1
        bispl(ib0) = is0
        ioff       = 1 + (is0-1)*nrqsg2
        if ( nrqsg2 .gt. 0 ) call                            &
             mpi_startall ( nrqsg2, irqsg2(ioff,1), ierr_mpi )
        npst       = npst + 1
      end if
      if ( npst .ge. 2 ) exit
    end do
    return
    !
    !/ end of w3gathsmc ----------------------------------------------------- /
    !/
  end subroutine w3gathsmc
  !
  !/ ------------------------------------------------------------------- /
  !> @brief smc version of w3gath
  !>
  !> @details
  !> 'scatter' data back to spectral storage after propagation.
  !>  direct copy or communication calls (mpp version).
  !>  see also w3gath.
  !>
  !> @param[in]  ispec   spectral bin considered
  !> @param[in]  mapsta  status map for spatial grid
  !> @param[in]  field   smc grid field to be propagated
  !>
  !> @remarks
  !>  - the field is put back but not converted !
  !>  - mpi persistent communication calls initialize in w3mpii.
  !>  - see w3gath and w3mpii for additional comments on data
  !>    buffering.
  !>
  !> @author jian-guo li
  !> @date 16 jan 2012
  !>
  subroutine w3scatsmc ( ispec, mapsta, field )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch-iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         13-jun-2006 |
    !/                  +-----------------------------------+
    !/
    !/    04-jan-1999 : distributed fortran 77 version.     ( version 1.18 )
    !/    13-jan-2000 : upgrade to fortran 90               ( version 2.00 )
    !/                  major changes to logistics.
    !/    28-dec-2004 : multiple grid version.              ( version 3.06 )
    !/    07-sep-2005 : updated boundary conditions.        ( version 3.08 )
    !/    13-jun-2006 : split store in g/sstore             ( version 3.09 )
    !/     9-dec-2010 : adapted for smc grid propagtion.     jgli09dec2010
    !/    16-jan-2012 : remove mapsta checking for smc grid. jgli16jan2012
    !/
    !/
    !  1. purpose :
    !
    !     'scatter' data back to spectral storage after propagation.
    !
    !  2. method :
    !
    !     direct copy or communication calls (mpp version).
    !     see also w3gath.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       ispec   int.   i   spectral bin considered.
    !       mapsta  i.a.   i   status map for spatial grid.
    !       field   r.a.   i   smc grid field to be propagated.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !
    !      mpi_startall, mpi_waitall, mpi_testall
    !                subr. mpif.h   mpi persistent comm. routines (!/mpi).
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3wave    subr. w3wavemd actual wave model routine.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks :
    !
    !     - the field is put back but not converted !
    !     - mpi persistent communication calls initialize in w3mpii.
    !     - see w3gath and w3mpii for additional comments on data
    !       buffering.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/shrd  switch for message passing method.
    !     !/mpi   id.
    !
    !     !/s     enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use w3gdatmd, only: nspec, nx, ny, nsea, ncel, nseal, mapsf
    use w3wdatmd, only: a => va
    use w3adatmd, only: mpibuf, bstat, ibfloc, isploc, bispl, &
         nsploc, nrqsg2, irqsg2, sstore
    use w3odatmd, only: iaproc, naproc
    use w3odatmd, only: ndst
    !/
    implicit none
    !
    include "mpif.h"
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: ispec, mapsta(ny*nx)
    real,    intent(in)     :: field(ncel)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: isea, ixy, ioff, ierr_mpi, j,   &
         status(mpi_status_size,nspec),  &
         jsea, ib0
    logical                 :: done
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  shared memory version ------------------------------------------ *
    !
    !
    ! 2.  distributed memory version ( mpi ) ----------------------------- *
    ! 2.a initializations
    !
    ! 2.b convert full grid to sea grid, active points only
    !
    do isea=1, nsea
      ixy    = mapsf(isea,3)
      if ( mapsta(ixy) .ge. 1 ) sstore(isea,ibfloc) = field(isea)
    end do
    !
    ! 2.c send spectral densities to appropriate remote
    !
    ioff   = 1 + (isploc-1)*nrqsg2
    if ( nrqsg2 .gt. 0 ) call                                  &
         mpi_startall ( nrqsg2, irqsg2(ioff,2), ierr_mpi )
    bstat(ibfloc) = 2
    !
    ! 2.d save locally stored results
    !
    do jsea=1, nseal
      !!li   isea   = iaproc+(jsea-1)*naproc
      isea   = min( iaproc+(jsea-1)*naproc, nsea )
      a(ispec,jsea) = sstore(isea,ibfloc)
    end do
    !
    ! 2.e check if any sends have finished
    !
    ib0    = ibfloc
    !
    do j=1, mpibuf
      ib0    = 1 + mod(ib0,mpibuf)
      if ( bstat(ib0) .eq. 2 ) then
        ioff   = 1 + (bispl(ib0)-1)*nrqsg2
        if ( nrqsg2 .gt. 0 ) then
          call mpi_testall ( nrqsg2, irqsg2(ioff,2), done,  &
               status, ierr_mpi )
        else
          done   = .true.
        end if
        if ( done .and. nrqsg2.gt.0 ) call                   &
             mpi_waitall ( nrqsg2, irqsg2(ioff,2),       &
             status, ierr_mpi )
        if ( done ) then
          bstat(ib0) = 0
        end if
      end if
    end do
    !
    ! 2.f last component, finish message passing, reset buffer control
    !
    if ( isploc .eq. nsploc ) then
      do ib0=1, mpibuf
        if ( bstat(ib0) .eq. 2 ) then
          ioff   = 1 + (bispl(ib0)-1)*nrqsg2
          if ( nrqsg2 .gt. 0 ) call                        &
               mpi_waitall ( nrqsg2, irqsg2(ioff,2),       &
               status, ierr_mpi )
          bstat(ib0) = 0
        end if
      end do
      isploc = 0
      ibfloc = 0
    end if
    return
    !
    ! formats
    !
    !/
    !/ end of w3scatsmc ----------------------------------------------------- /
    !/
  end subroutine w3scatsmc
  !/
  !/ end of two new subs for smc grid.    jgli 15mar2011
  !/
  !> @brief calculate cell centre lat-lon for given ids.
  !>
  !> @details
  !>  calculate the cell centre longitude and latitude in degree for a given
  !>  list of cell identity or sequential numbers in the imod sub-grid.
  !>
  !>  regular grid sx, sy, x0, y0 and smc grid mrfct and ijkcel arrays
  !>  in w3gdatmd are used to work out smc grid origin and increments.
  !>  then given cell centre coordinates are calculated.  longitude is
  !>  wrapped into [0, 360) range, latitude in in (-90, 90) range.
  !>  the polar cell centre is off the n-pole to avoid singularity but
  !>  its centre values are not used for propagation schemes.
  !>
  !> @param[in]   imod   model number to point to
  !> @param[in]   nc     numcer of cells to be calculated
  !> @param[in]   idcl   list of cell id or sequential numbers
  !> @param[out]  xlon   x-longitude in degree of listed cells
  !> @param[out]  ylat   y-latitude in degree of listed cells
  !>
  !> @author jian-guo li
  !> @date 19 oct 2020
  !>
  subroutine w3smcell( imod, nc, idcl, xlon, ylat )
    !! -------------------------------------------------------------------
    !!
    !!    generated for ww3 multi-grid boundary matching.  jgli19oct2020
    !!
    !! 1. purpose:
    !
    !     calculate the cell centre longitude and latitude in degree for a given
    !     list of cell identity or sequential numbers in the imod sub-grid.
    !
    !! 2. method:
    !
    !     regular grid sx, sy, x0, y0 and smc grid mrfct and ijkcel arrays
    !     in w3gdatmd are used to work out smc grid origin and increments.
    !     then given cell centre coordinates are calculated.  longitude is
    !     wrapped into [0, 360) range, latitude in in (-90, 90) range.
    !     the polar cell centre is off the n-pole to avoid singularity but
    !     its centre values are not used for propagation schemes.
    !
    !! 3. parameters:
    !     ----------------------------------------------------------------
    !     imod    int.   i   model number to point to.
    !     nc      int.   i   numcer of cells to be calculated.
    !     idcl    int.   i   list of cell id or sequential numbers.
    !     xlon    real   o   x-longitude in degree of listed cells.
    !     ylat    real   o   y-latitude in degree of listed cells.
    !     ----------------------------------------------------------------
    !
    !! 4. subroutines used:
    !
    !     none
    !
    !! 5. called by:
    !
    !     wmglow, w3iopp, wmiopp, ww3_gint
    !
    !! 6. error messages:
    !
    !     - error checks on previous setting of variable.
    !
    !! 7. remarks:
    !
    !! 8. structure:
    !
    !! 9. switches:
    !
    !     !/s     enable subroutine tracing.
    !     !/t     enable test output
    !
    ! 10. source code:
    !
    !/ ------------------------------------------------------------------- /
    use constants
    use w3gdatmd
    use w3servmd, only: extcde
    use w3odatmd, only: ndse, ndst
    implicit none
    !/ ------------------------------------------------------------------- /
    ! input/output variables
    integer, intent(in)               :: imod, nc
    integer, intent(in), dimension(nc):: idcl        ! automatic array
    real   , intent(out),dimension(nc):: xlon, ylat
    !/ ------------------------------------------------------------------- /
    ! local variables.
    real       :: xi0, yj0, dxg, dyg, dx1, dy1
    integer    :: i1, i3, j2, j4, mrf, ij, ijp, nsem
    !! 1. convert regular grid parameters into smc grid origin and increments.
    dxg = grids(imod)%sx
    dyg = grids(imod)%sy
    xi0 = grids(imod)%x0 - 0.5*dxg
    yj0 = grids(imod)%y0 - 0.5*dyg
    mrf = grids(imod)%mrfct
    dx1 = dxg/real(mrf)
    dy1 = dyg/real(mrf)
    nsem = grids(imod)%nsea
    !! 2. loop over listed cells and work out their centre coordinates.
    do ij = 1, nc
      ijp = idcl(ij)
      !!li  return south pole lon-lat values for any ids < 1 or > nsea
      !!    so these out of range points will not be matched to any cell.
      if( ijp < 1 .or. ijp > nsem ) then
        xlon(ij) = 0.0
        ylat(ij) = -90.0
      else
        !!    fetch cell array indexes from given sub-grid.
        i1=grids(imod)%ijkcel(1, ijp)
        j2=grids(imod)%ijkcel(2, ijp)
        i3=grids(imod)%ijkcel(3, ijp)
        j4=grids(imod)%ijkcel(4, ijp)
        !!    calculate its cell centre lon-lat values.
        xlon(ij) = xi0 + ( float(i1) + 0.5*float(i3) )*dx1
        ylat(ij) = yj0 + ( float(j2) + 0.5*float(j4) )*dy1
      endif
    end do
    !! 3. wrap negative logitudes into [0, 360) range.
    where( xlon < 0.0 ) xlon = xlon + 360.0
    !
    return
  end subroutine w3smcell
  !!
  !!
  !> @brief map lat-lon points to smc grid cells
  !>
  !> @details
  !>  determine whether a list of points are inside the imod smc sub-grid
  !>  and return the imod sub-grid cell indexes, if any.
  !>
  !>  convert point xlon and ylat values into cell indices i, j.
  !>  match with cell ranges (i,i+di) and (j,j+dj) to see i,j in
  !>  which cell.  return the matched cell number.  otherwise,
  !>  return an index of 0, or no matching cell found.
  !>
  !> @param[in]   imod    model number to point to
  !> @param[in]   xlon    x-longitude in degree of search points
  !> @param[in]   ylat    y-latitude  in degree of search points
  !> @param[in]   nc      number of points to be searched
  !> @param[out]  idcl    model number to point to
  !>
  !> @author jian-guo li
  !> @date 20 oct 2020
  !>
  subroutine w3smcgmp( imod, nc, xlon, ylat, idcl )
    !! -------------------------------------------------------------------
    !!
    !!    generated for ww3 multi-grid boundary matching.  jgli22oct2020
    !!
    !! 1. purpose:
    !
    !     determine whether a list of points are inside the imod smc sub-grid
    !     and return the imod sub-grid cell indexes, if any.
    !
    !! 2. method:
    !
    !     convert point xlon and ylat values into cell indices i, j.
    !     match with cell ranges (i,i+di) and (j,j+dj) to see i,j in
    !     which cell.  return the matched cell number.  otherwide,
    !     return an index of 0, or no matching cell found.
    !
    !! 3. parameters:
    !     ----------------------------------------------------------------
    !     imod    int.   i   model number to point to.
    !     xlon    real   i   x-longitude in degree of search points.
    !     ylat    real   i   y-latitude  in degree of search points.
    !     nc      int.   i   number of points to be searched.
    !     idcl    int.   o   model number to point to.
    !     ----------------------------------------------------------------
    !
    !! 4. subroutines used:
    !
    !     none
    !
    !! 5. called by:
    !
    !     wmglow, w3iopp, wmiopp, ww3_gint
    !
    !! 6. error messages:
    !
    !     - error checks on previous setting of variable.
    !
    !! 7. remarks:
    !
    !! 8. structure:
    !
    !! 9. switches:
    !
    !     !/s     enable subroutine tracing.
    !     !/t     enable test output
    !
    ! 10. source code:
    !
    !/ ------------------------------------------------------------------- /
    use constants
    use w3gdatmd
    use w3servmd, only: extcde
    use w3odatmd, only: ndse, ndst
    implicit none
    !/ ------------------------------------------------------------------- /
    ! iuput/output variables
    integer, intent(in)                :: imod, nc
    real   , intent(in),  dimension(nc):: xlon, ylat
    integer, intent(out), dimension(nc):: idcl
    !/ ------------------------------------------------------------------- /
    ! local variables
    integer, dimension(nc) :: ix1, jy1
    real       :: xi0, yj0, dxg, dyg, dx1, dy1, xlow(nc)
    integer    :: i1, i3, j2, j4, ij, ijp, mrf, nsem, nfund
    !! 1. convert xlon ylat into smc grid indexes in present smc grid.
    dxg = grids(imod)%sx
    dyg = grids(imod)%sy
    xi0 = grids(imod)%x0 - 0.5*dxg
    yj0 = grids(imod)%y0 - 0.5*dyg
    mrf = grids(imod)%mrfct
    dx1 = dxg/real(mrf)
    dy1 = dyg/real(mrf)
    nsem = grids(imod)%nsea
    !!  wrap longitude so they are great than xi0.
    xlow = xlon
    where( xlow < xi0 ) xlow = xlow + 360.0
    !!  convert xlon and ylat into smc indexes.
    ix1 = floor( (xlow - xi0)/dx1 )
    jy1 = floor( (ylat - yj0)/dy1 )
    !!  initialise idcl to be all 0
    idcl = 0
    !! 2. loop over all cells until all input points are found.
    nfund = 0
    ij = 0
    do while( ij < nsem .and. nfund < nc )
      ij = ij + 1
      i1=grids(imod)%ijkcel(1, ij)
      j2=grids(imod)%ijkcel(2, ij)
      i3=grids(imod)%ijkcel(3, ij)
      j4=grids(imod)%ijkcel(4, ij)
      lpnbis: do ijp = 1, nc
        if( idcl(ijp) .eq. 0 ) then
          !!  check if ix1 and jy1 fall inside the cell i,j range.
          if((ix1(ijp) .ge. i1) .and. (ix1(ijp) .lt. i1+i3) .and.  &
               (jy1(ijp) .ge. j2) .and. (jy1(ijp) .lt. j2+j4)) then
            nfund = nfund + 1
            idcl(ijp) = ij
            exit  lpnbis
          endif
        endif
      end do  lpnbis
    end do
    !!  if any idcl element remians to be 0, it means no cell is found
    !!  covering this point.  so check idcl(ij) > 0 to ensure in grid.
    return
  end subroutine w3smcgmp
  !!
  !/ end of module w3psmcmd -------------------------------------------- /
  !/
end module w3psmcmd
