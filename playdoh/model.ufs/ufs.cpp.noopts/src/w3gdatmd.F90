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
!/
!/ ------------------------------------------------------------------- /
!/ macros for enabling test output
!/
!/
!/ ------------------------------------------------------------------- /
module w3gdatmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  !           j. h. alves             !
  !/                  |            f. ardhuin             |
  !/                  |                        fortran 90 |
  !/                  | last update :         15-apr-2020 |
  !/                  +-----------------------------------+
  !/
  !/    24-jun-2005 : origination.                        ( version 3.07 )
  !/    09-nov-2005 : remove soft boundary options.       ( version 3.08 )
  !/    23-jun-2006 : add data for w3sln1.                ( version 3.09 )
  !/    18-jul-2006 : add input grids.                    ( version 3.10 )
  !/    05-oct-2006 : add filter to array pointers.       ( version 3.10 )
  !/    02-feb-2007 : add flagst.                         ( version 3.10 )
  !/    14-apr-2007 : add miche style limiter.            ( version 3.11 )
  !/                  ( j. h. alves )
  !/    25-apr-2007 : adding battjes-janssen sdb.         ( version 3.11 )
  !/                  ( j. h. alves )
  !/    06-aug-2007 : fixing slnp !/seed bug.             ( version 3.13 )
  !/    18-sep-2007 : adding wam4 source terms.           ( version 3.13 )
  !/                  ( f. ardhuin )
  !/    15-apr-2008 : clean up for distribution.          ( version 3.14 )
  !/    27-jun-2008 : expand wam4 variants namelist       ( version 3.14 )
  !/                  ( f. ardhuin )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/    30-oct-2009 : implement run-time grid selection.  ( version 3.14 )
  !/                  (w. e. rogers & t. j. campbell, nrl)
  !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
  !/                  (w. e. rogers & t. j. campbell, nrl)
  !/    29-oct-2010 : implement unstructured grids        ( version 3.14.1 )
  !/                  (a. roland and f. ardhuin)
  !/    06-dec-2010 : change from global (logical) to iclose (integer) to
  !/                  specify index closure for a grid.   ( version 3.14 )
  !/                  (t. j. campbell, nrl)
  !/    23-dec-2010 : fix hpfac and hqfac by including the cos(ygrd)
  !/                  factor with dxdp and dxdq terms.    ( version 3.14 )
  !/                  (t. j. campbell, nrl)
  !/    05-apr-2011 : implement interations for dtmax < 1s( version 3.14.1 )
  !/                  (f. ardhuin)
  !/    01-jul-2011 : movable bed bottom friction bt4     ( version 4.01 )
  !/    03-nov-2011 : bug fix: guginit initialization     ( version 4.04 )
  !/    29-nov-2011 : adding st6 source term option.      ( version 4.04 )
  !/                  (s. zieger)
  !/    14-mar-2012 : add psic for bt4                    ( version 4.04 )
  !/    12-jun-2012 : add /rtd option or rotated grid variables.
  !/                  (jian-guo li)                       ( version 4.06 )
  !/    13-jul-2012 : move data structures gmd (snl3) and nonlinear
  !/                  filter (snls) from 3.15 (hlt).      ( version 4.08 )
  !/    03-sep-2012 : clean up of ug grids                ( version 4.08 )
  !/    12-dec-2012 : adding smc grid.  jg_li             ( version 4.09 )
  !/    16-sep-2013 : add arctic part smc grid.           ( version 4.11 )
  !/    11-nov-2013 : smc and rotated grid incorporated in the main
  !/                  trunk                               ( version 4.13 )
  !/    16-nov-2013 : allows reflection on curvi grids    ( version 4.14 )
  !/    26-jul-2013 : adding ig waves                     ( version 4.16 )
  !/    18-dec-2013 : moving flagll into grid type        ( version 4.16 )
  !/    11-jun-2014 : changed reflection for subgrid      ( version 5.01 )
  !/    10-dec-2014 : add checks for allocate status      ( version 5.04 )
  !/    21-aug-2015 : add smc funo3, fverg options. jgli  ( version 5.09 )
  !/    04-may-2016 : add iicedisp                  gb&fa ( version 5.10 )
  !/    20-jan-2017 : update to new w3gsrumd apis         ( version 6.02 )
  !/    20-jan-2017 : change to preprocessor macros to enable test output.
  !/                  (t.j. campbell, nrl)                ( version 6.02 )
  !/    20-jan-2017 : change calculation of curvilinear grid metric and
  !/                  derivatives calculations to use w3gsrumd:w3cgdm.
  !/                  (t.j. campbell, nrl)                ( version 6.02 )
  !/    07-jan-2018 : generalizes ice100wind to icescales ( version 6.04 )
  !/    26-mar-2018 : add fswnd optional variable.  jgli  ( version 6.02 )
  !/    05-jun-2018 : add pdlib/debuginit and implcit scheme parameters
  !/                  for unstructured grids              ( version 6.04 )
  !/    18-aug-2018 : s_{ice} ic5 (q. liu)                ( version 6.06 )
  !/    20-aug-2018:  extra namelist variables for st6    ( version 6.06)
  !/                  (q. liu, uom)
  !/    26-aug-2018 : uost (mentaschi et al. 2015, 2018)  ( version 6.06 )
  !/    27-aug-2018 : add btbeta parameter                ( version 6.06 )
  !/    22-feb-2020 : add airgb and aircmin               ( version 7.06 )
  !/    15-apr-2020 : adds optional opt-out for cfl on bc ( version 7.08 )
  !/    06-may-2021 : add smctype, arctc options.   jgli  ( version 7.12 )
  !/    07-jun-2021 : the gke module (nl5, q. liu)        ( version 7.12 )
  !/
  !/
  !/    copyright 2009-2013 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     define data structures to set up wave model grids and aliases
  !     to use individual grids transparently. also includes subroutines
  !     to manage data structure and pointing to individual models.
  !     definition of grids and model set up.
  !
  !  2. variables and types :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      ngrids    int.  public   number of grids, initialized at -1
  !                               to check proper model initialization.
  !      nauxgr    int.  public   auxiliary grids.
  !      igrid     int.  public   selected spatial grid, init. at -1.
  !      isgrd     int.  public   selected spectral grid, init. at -1.
  !      ipars     int.  public   selected num. and ph. pars, init. at -1.
  !      rlgtype   i.p.  public   named constant for rectilinear grid type
  !      clgtype   i.p.  public   named constant for curvilinear grid type
  !      ungtype   i.p.  public   named constant for unstructured triangular grid
  !      smctype   i.p.  public   named constant for unstructured smc grid type
  !      flagll    log.  public   flag to indicate coordinate system for all grids
  !                               .true.: spherical (lon/lat in degrees)
  !                               .false.: cartesian (meters)
  !      grid      type  public   data structure defining grid.
  !      grids     grid  public   array of grids.
  !      sgrd      type  public   data structure defining spectral grid.
  !      sgrds     grid  public   array of spectral grids.
  !      mpar      type  public   data structure with all other model
  !                               parameters.
  !      mpars     grid  public   array of mpar.
  !     ----------------------------------------------------------------
  !
  !     all elements of grid are aliased to pointers with the same
  !     name. these pointers are defined as :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      gtype     int.  public   flag for type of grid
  !                               rlgtype: rectilinear grid
  !                               clgtype: curvilinear grid
  !                               ungtype: unstructured triangular grid
  !                               smctype: unstructured smc grid
  !      rstype    int.  public   integer identifyng restart type
  !      iclose    int.  public   parameter indicating type of index closure of grid.
  !                               iclose_none: no grid closure
  !                               iclose_smpl: simple grid closure
  !                                 grid is periodic in the i-index and wraps at
  !                                 i=nx+1. in other words, (nx+1,j) => (1,j).
  !                               iclose_trpl: tripole grid closure
  !                                 grid is periodic in the i-index and and wraps at
  !                                 i=nx+1 and has closure at j=ny+1. in other words,
  !                                 (nx+1,j<=ny) => (1,j) and
  !                                 (i,ny+1) => (mod(nx-i+1,nx)+1,ny). the tripole
  !                                 closure requires that nx be even.
  !      nx, ny    int.  public   discrete dimensions of spatial grid.
  !      nsea(l)   int.  public   number of sea points (local for mpp).
  !      nu/vfc    int.  public   number of u/v faces for smc grid.
  !      nrlv      int.  public   number of refined levels for smc grid.
  !      nglo      int.  public   number of cells in global part for smc grid.
  !      narc      int.  public   number of cells in arctic part for smc grid.
  !      nbac      int.  public   number of boundary cells in arctic part.
  !      nbgl      int.  public   number of boundary cells in global part.
  !      nbsmc     int.  public   number of boundary cells for regional smc grid.
  !      trflag    int.  public   flag for use of transparencies
  !                                0: no sub-grid obstacles.
  !                                1: obstructions at cell boundaries.
  !                                2: obstructions at cell centers.
  !                                3: like 1 with continuous ice.
  !                                4: like 2 with continuous ice.
  !      mapsta    i.a.  public   grid status map.
  !      mapst2    i.a.  public   second grid status map.
  !      mapxx     i.a.  public   storage grid maps.
  !      ijkcel    i.a.  public   cell info array for smc grid.
  !      ijku/vfc  i.a.  public   u/v-face arrays for smc grid.
  !      nlv*      i.a.  public   cell, u/v-face numbers of refine levels.
  !      iclbac    i.a.  public   mapping index for arctic boundary cells.
  !      ismcbp    i.a.  public   list of smc grid input boundary cell indexes.
  !      sx,sy     real  public   spatial (rectilinear) grid increments.
  !      x0,y0     real  public   lower left corner of spatial (rectilinear) grid.
  !      dtcfl     real  public   maximum cfl time step x-y propagation.
  !      dtcfli    real  public   id. intra-spectral.
  !      dtmax     real  public   maximum overall time step.
  !      dtmin     real  public   minimum dynamic time step for source
  !      nitersec1 real  public   number of interations when dtmax < 1s
  !      dmin      real  public   minimum water depth.
  !      ctmax     real  public   maximum cfl number for depth refr.
  !      fice0/n   real  public   cut-off ice conc. for ice coverage.
  !      ficel     real  public   length scale for sea ice damping
  !      iicehmin  real  public   minimum thickness of sea ice
  !      iicehdisp real  public   minimum thickness of sea ice in the dispersion relation before relaxing the conv. criterion
  !      iicehfac  real  public   scale factor for sea ice thickness
  !      iicehinit real  public   initial value of ice thickness
  !      icescales r.a.  publ.    scaling coefficient for source terms in the presence of ice
  !                               default is 1.0, meaning that 100% ice
  !                               concentration result in zero source term
  !                               if set to 0.0, then ice has no direct impact on sln / sin / snl / sds
  !      ic3pars   r.a.  public   various parameters for use in ic4, handled as
  !                               an array for simplicity
  !      ic4_ki    r.a.  public   ki (dissipation rate) values for use in ic4
  !      ic4_fc    r.a.  public   fc (frequency bin separators) for use in ic4
  !      pfmove    real  public   tunable parameter in gse correction
  !                               for moving grids.
  !      gridshift real  public   grid offset for multi-grid w/scrip
  !      cmprtrck  log.  public   true for traditional compression of track output
  !      polat/lon r.a.  public   rotated n-pole standard latitude/longitude.
  !      angld     r.a.  public   rotation angle in degree to turn rotated grid
  !                               back to standard grid.  jgli12jun2012
  !      flagunr   log.  public   true if rotating directions back to true north
  !      stexu     real  public   length-scale (x) for space-time extreme averaging
  !      steyu     real  public   length-scale (y) for space-time extreme averaging
  !      stedu     real  public   time-scale for space-time extreme averaging
  !      zb        r.a.  public   bottom levels on storage grid.
  !      clats(i)  r.a.  public   (inverse) cosine of latitude at sea points.
  !      cthg0s    r.a.  public   constant in great-circle refr. term at sea points.
  !      trnx/y    r.a.  public   transparencies in x/y for sub-grid
  !      ctrnx/y   r.a.  public   sub-grid transparencies for smc grid.
  !      angarc    r.a.  public   rotation angle in degree for arctic cells.
  !      spcbac    r.a.  public   full 2-d spectra for arctic boundary cells.
  !      x/ygrd    r.a.  public   spatial grid coordinate arrays.
  !      sx/sygrd  r.a.  public   spatial grid increment arrays.
  !      ginit     log.  public   flag identifying grid initialization.
  !      fldry     log.  public   flag for 'dry' run (io and data
  !                               processing only).
  !      flcx      log.  public   flags for prop. is different spaces.
  !      flsou     log.  public   flag for source term calculation.
  !      funo3     log.  public   flag for 3rd order uno3 scheme on smc grid.
  !      fverg     log.  public   flag for 1-2-1 averaging smoothing on smc grid.
  !      fswnd     log.  public   flag for sea-point only wind input on smc grid.
  !      arctc     log.  public   flag to include arctic polar part on smc grid.
  !      flagst    l.a.  public   flag for source term computations
  !                               for individual grid points.
  !      iicedisp   log.  public   flag for use of the ice covered dispertion relation.
  !      iicesmooth log.  public   flag to smooth the ice covered dispertion relation in broken ice.
  !
  !
  !      gname     c*30  public   grid name.
  !      filext    c*13  public   extension of wavewatch iii file names
  !                               default in 'ww3'.
  !      btbeta    real  public   the constant used for separating wind sea
  !                               and swell when we estimate wbt
  !     ----------------------------------------------------------------
  !
  !     all elements of sgrd are aliased to pointers with the same
  !     name. these pointers are defined as :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      nk        int.  public   number of discrete wavenumbers.
  !      nk2       int.  public   extended wavenumber range.
  !      nth       int.  public   number of discrete directions.
  !      nspec     int.  public   number of discrete spectral bins.
  !      mapxx     i.a.  public   spectral maps.
  !      dth       real  public   directional increments (radians).
  !      xfr       real  public   frequency multiplication factor.
  !      fr1       real  public   lowest frequency                 (hz)
  !      fte       real  public   factor in tail integration energy.
  !      ftf       real  public   id. frequency.
  !      ftwn      real  public   id. wavenumber.
  !      fttr      real  public   id. wave period.
  !      ftwl      real  public   id. wave length.
  !      factin    real  public   factors for obtaining integer cut-off
  !                               frequency.
  !      fachfx    real  public   factor for tail.
  !      th        r.a   public   directions (radians).
  !      esin      r.a   public   sine of discrete directions.
  !      ecos      r.a   public   cosine of discrete directions.
  !      es2, esc, ec2
  !                r.a   public   sine and cosine products
  !      sig       r.a   public   relative frequencies (invariant
  !                                                     in grid). (rad)
  !      sig2      r.a   public   id. for full 2-d spectrum.
  !      dsip      r.a   public   frequency bandwidths (prop.)    (rad)
  !      dsii      r.a   public   frequency bandwidths (int.)     (rad)
  !      dden      r.a   public   dsii * dth * sig (for integration
  !                               based on energy)
  !      dden2     r.a   public   idem, full spectrum.
  !      sinit     log.  public   flag identifying grid initialization.
  !     ----------------------------------------------------------------
  !
  !     the structure mpar contains all other model parameters for
  !     numerical methods and physical parameterizations. it contains
  !     itself several structures as outlined below.
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      pinit     log.  public   flag identifying initialization.
  !      npars     npar  public   numerical parameters,
  !      props     prop  public   parameters propagatrion schemes.
  !      sflps     sflp  public   parameters for flux computation.
  !      slnps     slnp  public   parameters sln.
  !      srcps     srcp  public   parameters sin and sds.
  !      snlps     snlp  public   parameters snl.
  !      sbtps     sbtp  public   parameters sbt.
  !      sdbps     sdbp  public   parameters sdb.
  !      strps     strp  public   parameters str.
  !      sbsps     sbsp  public   parameters sbs.
  !     ----------------------------------------------------------------
  !
  !     the structure npar contains numerical parameters and is aliased
  !     as above:
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      facp      real  public   constant in maximum par. change in
  !                               dynamic integration scheme (depends
  !                               upon xp).
  !      xrel      real  public   id. relative change.
  !      xflt      real  public   id. filter level.
  !      fxfm      real  public   constant for mean frequency in
  !                               cut-off.                       (!/st1)
  !      fxpm      real  public   id. pm.
  !      xft       real  public   constant for cut-off freq.     (!/st2)
  !      xfc       real  public   id.
  !      facsd     real  public   constant in seeding algorithm.
  !      fhmax     real  public   hs/depth ratio in limiter     (!/mlim)
  !      rwindc    real  public   coefficient for current in relative
  !                               wind                          (!/rwnd)
  !      wwcor     r.a.  public   wind correction factors       (!/wcor)
  !     ----------------------------------------------------------------
  !
  !     the structure prop contains parameters for the propagation
  !     schemes and is aliased as above:
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      dtme      real  public   swell age in disp. corr.      (!/pr2)
  !      clatmn    real  public   id. minimum cosine of lat.    (!/pr2)
  !      dtms      real  public   swell age in disp. corr.      (!/smc)
  !
  !      wdcg      real  public   factors in width of av. cg.   (!/pr3)
  !      wdth      real  public   factors in width of av. th.   (!/pr3)
  !     ----------------------------------------------------------------
  !
  !     the structure sflp contains parameters for the fluxes
  !     and is aliased as above:
  !     ----------------------------------------------------------------
  !                                                            (!/flx2)
  !      nittin    int.  public   number of itterations for drag calc.
  !      cinxsi    real  public   constant in parametric description
  !                                                            (!/flx3)
  !      nittin    int.  public   number of itterations for drag calc.
  !      cap_id    int   public   type of cap used.
  !      cinxsi    real  public   constant in parametric description
  !      cd_max    real  public   cap on cd.
  !                                                            (!/flx4)
  !      flx4a0    real  public   scaling value in parametric description
  !     ----------------------------------------------------------------
  !
  !     the structure slnp contains parameters for the linear input
  !     source terms and is aliased as above:
  !
  !     ----------------------------------------------------------------
  !                                                             (!/ln1)
  !      slnc1     real  public   proportionality and other constants in
  !                               input source term.
  !      fspm      real  public   factor for fpm in filter.
  !      fshf      real  public   factor for fh in filter.
  !     ----------------------------------------------------------------
  !
  !     the structure srcp contains parameters for the input and dis,
  !     source terms and is aliased as above:
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      wwnmeanptail r  public   power of tail for wnmean calculation
  !      sstxftftail  r  public   tail factor for  wnmean calculation
  !                                                             (!/st1)
  !      sinc1     real  public   proportionality and other constants in
  !                               input source term.
  !      sdsc1     real  public   combined constant in dissipation
  !                               source term.
  !                                                             (!/st2)
  !      zwind     real  public   height at which the wind is defined
  !                               of drag.
  !      fswell    real  public   reduction factor of negative input
  !                               for swell.
  !      shstab, ofstab, ccng, ccps, ffng, ffps
  !                real  public   factors in effective wind speed.
  !      cdsan     real  public   constants in high-freq. dis.
  !      sdsaln    real  public   factor for nondimensional 1-d spectrum.
  !      cdsbn     real  public   constants in parameterization of phi.
  !      xfh       real  public   constant for turbulent length scale.
  !      xfn       real  public   constants in combining low and high
  !                               frequency dissipation.
  !                                                             (!/st3)
  !      zzwnd     real  public   height at which the wind is defined
  !      aalpha    real  public   minimum value of charnock parameter
  !      bbeta     real  public   wind-wave coupling coefficient
  !      zzalp     real  public   wave age tuning coefficient in sin
  !      ttauwshelter real  public sheltering coefficient for short waves
  !      zz0max    real  public   maximum value of air-side roughness
  !      zz0rat    real  public   ratio of roughness for mean and
  !                               oscillatory flows
  !      ssinthp   real  public   power in cosine of wind input
  !      sswellf   r.a.  public   swell damping coefficients
  !      ssdscn    real  public   dissipation parameters
  !      ssdsbr    real  public   threshold in saturation spectrum for sds
  !      ssdsp     real  public   power of b(k) in sds
  !      wwnmeanp  real  public   power that defines the mean wavenumber
  !                               in sds
  !      sstxftf, sstxftwn real  public   tail constants
  !      ssdsc4,   real  public   threshold shift in saturation diss.
  !      ssdsc5,   real  public   wave-turbulence dissipation factor
  !      ssdsc6,   real  public   dissipation parameter
  !      ddelta1   real  public   low-frequency dissipation coefficient
  !                               in wam4
  !      ddelta2   real  public   high-frequency dissipation coefficient
  !                               in wam4
  !      ssdsdth   real  public   maximum angular sector for saturation
  !                               spectrum
  !      ssdscos   real  public   power of cosine in saturation integral
  !      ssdsiso   int.  public   choice of definition of the isotropic
  !                               saturation
  !     ----------------------------------------------------------------
  !
  !     the structure snlp contains parameters for the nonl. inter.
  !     source term and is aliased as above:
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !                                                             (!/nl1)
  !      snlc1     real  public   scaled proportionality constant.
  !      lam       real  public   factor defining quadruplet.
  !      kdcon     real  public   conversion factor for relative depth.
  !      kdmn      real  public   minimum relative depth.
  !      snlsn     real  public   constants in shallow water factor.
  !                                                             (!/nl2)
  !      iqtpe     int.  public   type of depth treatment
  !                                1 : deep water
  !                                2 : deep water / wam scaling
  !                                3 : finite water depth
  !      ndpths    int.  public   number of depth for which integration
  !                               space needs to be computed.
  !      nltail    real  public   tail factor for parametric tail.
  !      dpthnl    r.a.  public   depths corresponding to ndpths.
  !                               *** note: this array is not allocated
  !                                         in the w3dimp routine ***
  !                                                             (!/nl3)
  !      nfr       int.  public   number of frequencies or wavenumbers
  !                               in discrete spectral space (nfr=>nk).
  !      nfrmin    int.  public   minimum discrete frequency in the
  !                               expanded frequency space.
  !      nfrmax    int.  public   idem maximum for first part.
  !      nfrcut    int.  public   idem maximum for second part.
  !      nthmax    int.  public   extension of directional space.
  !      nthexp    int   public   number of bins in extended dir. space.
  !      nspmin, nspmax, nspmx2
  !                int.  public   1d spectral space range.
  !      frq       r.a.  public   expanded frequency range (hz).
  !      xsi       r.a.  public   expanded frequency range (rad/s).
  !      nqa       int.  public   number of actual quadruplets.
  !      qst1      i.a.  public   spectral offsets for compuation of
  !                               quadruplet spectral desnities.
  !      qst2      r.a.  public   idem weights.
  !      qst3      r.a.  public   proportionality constants and k factors
  !                               in diagonal strength.
  !      qst4      i.a.  public   spectral offsets for combining of
  !                               interactions and diagonal.
  !      qst5      r.a.  public   idem weights for interactions.
  !      qst6      r.a.  public   idem weights for diagonal.
  !      snlnq     int.  public   number of quadruplet definitions.
  !      snlmsc    real  public   tuning power 'deep' scaling.
  !      snlnsc    real  public   tuning power 'shallow' scaling.
  !      snlsfd    real  public   'deep' nondimensional filer freq.
  !      snlsfs    real  public   'shallow' nondimensional filer freq.
  !      snll      r.a.  public   array with lambda for quadruplet.
  !      snlm      r.a.  public   array with mu for quadruplet.
  !      snlt      r.a.  public   array with dtheta for quadruplet.
  !      snlcd     r.a.  public   array with cd for quadruplet.
  !      snlcs     r.a.  public   array with cs for quadruplet.
  !                                                             (!/nl4)
  !      itsa      int.  public   integer indicating tsa (1) or fbi (0)
  !      ialt      int.  public   integer determining alternating looping
  !                                                             (!/nl5)
  !      qr5dpt    real  public   water depth for the gke module
  !      qr5oml    real  public   λ cut off value for quasi-resonant quartets
  !      qi5dis    int.  public   method to discretize continuous spectrum
  !      qi5kev    int.  public   gke (gs13 or j03)
  !      qi5nnz    int.  public   # of interactive quadruplets
  !      qi5ipl    int.  public   interp. method to get c₄
  !      qi5pmx    int.  public   phase mixing related parameter
  !                                                             (!/nls)
  !      nthx      int.  public   expanded discrete direction range.
  !      nfrx      int.  public   expanded discrete frequency range.
  !      nspl-h    int.  public   range of 1d spectrum.
  !      snsst     r.a.  public   array with interpolation weights.
  !      cnlsa     real  public   a34 in quadruplet definition.
  !      cnlsc     real  public   c in snl definition.
  !      cnlsfm    real  public   maximum relative spectral change.
  !      cnlsc1/3  real  public   constant in frequency filter.
  !     ----------------------------------------------------------------
  !
  !     the structure sbtp contains parameters for the bottom friction
  !     source term and is aliased as above:
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      sbtc1     real  public   proportionality constant.    (!/bt1)
  !      sbtcx     r.a.  public   parameters for bottom fric.  (!/bt4)
  !     ----------------------------------------------------------------
  !
  !     the structure sdbp contains parameters for the depth incduced
  !     breaking source term and is aliased as above:
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      sdbc1     real  public   proportionality constant.    (!/db1)
  !      sdbc2     real  public   hmax/d ratio.                (!/db1)
  !      fdonly    log.  public   flag for checking depth only (!/db1)
  !                               otherwise miche criterion.
  !     ----------------------------------------------------------------
  !
  !     the structure strp contains parameters for the triad interaction
  !     source term and is aliased as above:
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !     ----------------------------------------------------------------
  !
  !     the structure sbsp contains parameters for the bottom scattering
  !     source term and is aliased as above:
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !     ----------------------------------------------------------------
  !
  !     the structure sicp contains parameters for arbitrary source
  !     term and is aliased as above:
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !     is1c1      real  public   scale factor for icecon.     (!/isx)
  !     is1c2      real  public   offset for ice concentration (!/isx)
  !     ----------------------------------------------------------------
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3nmod    subr. public   set number of grids.
  !      w3dimx    subr. public   set dimensions of spatial grid.
  !      w3dims    subr. public   set dimensions of spectral grid.
  !      w3setg    subr. public   point to selected grid / model.
  !      w3gntx    subr. public   construct grid arrays
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      strace    subr. w3servmd subroutine tracing.
  !      extcde    subr. w3servmd abort program with exit code.
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !
  !     - in model versions before 3.06 the parameters in the grid
  !       structure were stored in the module w3iogr.
  !     - no subroutine dimp is provided, instead, arrays are set
  !       one-by-one in w3iogr.
  !
  !  6. switches :
  !
  !     see subroutine documentation.
  !
  !     !/prn  select propagation scheme
  !     !/smc  uno2 propagation on smc grid.
  !
  !     !/lnn  select source terms
  !     !/stn
  !     !/nln
  !     !/btn
  !     !/dbn
  !     !/trn
  !     !/bsn
  !     !/xxn
  !
  !     !/s    enable subroutine tracing.
  !
  !  7. source code :
  !
  !/ ------------------------------------------------------------------- /
  !/
  !/ required modules
  !/
  use w3gsrumd
  !/
  !/ specify default accessibility
  !/
  public
  !/
  !/ module private variable for checking error returns
  !/
  integer, private        :: istat
  !/
  !/ conventional declarations
  !/
  integer                 :: ngrids = -1, igrid = -1, isgrd = -1, &
       ipars = -1, nauxgr
  !
  integer, parameter      :: rlgtype = 1
  integer, parameter      :: clgtype = 2
  integer, parameter      :: ungtype = 3
  integer, parameter      :: smctype = 4
  integer, parameter      :: iclose_none = iclo_none
  integer, parameter      :: iclose_smpl = iclo_smpl
  integer, parameter      :: iclose_trpl = iclo_trpl
  !
  ! dimensions of tables for pre-computing of dissipation
  !
  !/
  !/ data structures
  !/
  !/ grid type
  type grid          ! this is the geographical grid with all associated parameters
    integer          :: gtype
    integer          :: rstype = -1
    integer          :: iclose
    integer          :: nx, ny, nsea, nseal, trflag
    integer, pointer :: mapsta(:,:), mapst2(:,:),            &
         mapfs(:,:), mapsf(:,:)
    !
    !
    real             :: sx, sy, x0, y0, dtcfl, dtcfli, dtmax,      &
         dtmin, dmin, ctmax, fice0, ficen, ficel,   &
         pfmove, stexu, steyu, stedu, iicehmin,     &
         iicehinit, icescales(4), iicehfac, iicehdisp, &
         iiceddisp, iicefdisp, btbeta, aaircmin, aairgb
    real(8)          :: gridshift ! see notes in wmghgh
    real   , pointer :: zb(:)     ! bottom grid, defined on isea
    real   , pointer :: clats(:)  ! cos(lat), defined on sea points
    real   , pointer :: clatis(:) ! inverse of cos(lat) defined on isea
    real   , pointer :: cthg0s(:) ! tan(y)/r, defined on isea
    real   , pointer :: trnx(:,:), trny(:,:) ! transparency information on ix,iy
    real   , pointer :: spcbac(:,:), angarc(:)
    double precision, pointer :: xgrd(:,:), ygrd(:,:) ! x and y defined on ix,iy
    real   , pointer :: dxdp(:,:), dxdq(:,:) ! dx/dp & dx/dq defined on ix,iy
    real   , pointer :: dydp(:,:), dydq(:,:) ! dy/dp & dy/dq defined on ix,iy
    real   , pointer :: dpdx(:,:), dpdy(:,:) ! dp/dx & dp/dy defined on ix,iy
    real   , pointer :: dqdx(:,:), dqdy(:,:) ! dq/dx & dq/dy defined on ix,iy
    real   , pointer :: gsqrt(:,:) ! sqrt(g) defined on ix,iy
    real   , pointer :: hpfac(:,:) ! h_p = sqrt(g_pp) defined on ix,iy
    real   , pointer :: hqfac(:,:) ! h_q = sqrt(g_qq) defined on ix,iy
    logical          :: ginit, fldry, flcx, flcy, flcth, flck, flsou, iicedisp,&
         iicesmooth
    logical          :: flagll
    logical          :: cmprtrck
    logical, pointer :: flagst(:)
    character(len=30):: gname
    character(len=13):: filext
    logical          :: guginit
    integer          :: e3df(3,5), p2msf(3), us3df(3), usspf(2) ! freq. indices for 3d output
    real             :: ussp_wn(25) !max set to 25 decay scales.
    !
    type(t_gsu) :: gsu ! grid search utility object
    !
    real                  :: ffacberg    ! mutiplicative factor for iceberg mask
    logical               :: lmpenabled ! flag to enable li et al. langmuir parameterization
    logical               :: sdtail ! flag to enable high-freq tail in li et al. stokes drift computations
    integer               :: hslmode ! 0 for test (hsl=10m everywhere, 1 for coupler-based hsl)
    !
    ! unstructured data
    !
    integer               :: ntri
    integer, pointer      :: trigp(:,:)
    real(8), pointer      :: len(:,:),si(:), ien(:,:)
    real                  :: maxx, maxy, dxymax
    real, pointer         :: angle(:,:),angle0(:,:)
    integer               :: countri,countot,nnz, nbedge
    integer, pointer      :: ccon(:), countcon(:), ie_cell(:), &
         pos_cell(:),   &
         iaa(:), jaa(:), posi(:,:), index_cell(:),       &
         i_diag(:), ja_ie(:,:,:)
    integer*2, pointer    :: iobp(:)
    integer*1, pointer    :: iobpd(:,:), iobdp(:), iobpa(:)
    integer, pointer      :: edges(:,:), neigh(:,:)
    real(8), pointer      :: tria(:)
    real, pointer         :: crossdiff(:,:)
  end type grid
  !
  type sgrd   ! this is the spectral grid with all parameters that vary with freq. and direction
    integer               :: nk=0, nk2=0, nth=0, nspec=0
    integer, pointer      :: mapwn(:), mapth(:)
    real                  :: dth=0., xfr=0., fr1=0., fte=0., ftf=0., ftwn=0., fttr=0., &
         ftwl=0., facti1=0., facti2=0., fachfa=0., fachfe=0.
    real, pointer         :: th(:), esin(:), ecos(:), es2(:),     &
         esc(:), ec2(:), sig(:), sig2(:),     &
         dsip(:), dsii(:), dden(:), dden2(:)
    logical               :: sinit=.false.
  end type sgrd
  !
  type npar
    real                  :: facp, xrel, xflt, fxfm, fxpm,        &
         xft, xfc, facsd, fhmax
  end type npar
  !
  type prop
  end type prop
  !
  type fldp
    real :: dummy
  end type fldp
  type sflp
  end type sflp
  !
  type slnp
  end type slnp
  !
  type srcp
    real                       :: wwnmeanptail, sstxftftail
    !
    !
  end type srcp
  !
  type snlp
  end type snlp
  !
  type sbtp
  end type sbtp
  !
  type sdbp
  end type sdbp
  !
  type strp
  end type strp
  !
  type sbsp
  end type sbsp
  !
  type sicp
  end type sicp
  ! specific type for unstructured scheme
  type schm
    logical :: fsn          = .false.
    logical :: fspsi        = .false.
    logical :: fsfct        = .false.
    logical :: fsnimp       = .false.
    logical :: fstotalimp   = .false.
    logical :: fstotalexp   = .false.
    logical :: fsrefraction = .false.
    logical :: fsfreqshift  = .false.
    logical :: fssource     = .false.
    logical :: fsbccfl      = .false.
    logical :: do_change_wlv
    real(8) :: solverthr_stp
    real(8) :: crit_dep_stp
    logical :: b_jgs_terminate_maxiter
    logical :: b_jgs_terminate_difference
    logical :: b_jgs_terminate_norm
    logical :: b_jgs_limiter
    logical :: b_jgs_use_jacobi
    logical :: b_jgs_block_gauss_seidel
    integer :: b_jgs_maxiter
    integer :: b_jgs_limiter_func
    real*8  :: b_jgs_pmin
    real*8  :: b_jgs_diff_thr
    real*8  :: b_jgs_norm_thr
    integer :: b_jgs_nlevel
    logical :: b_jgs_source_nonlinear
  end type schm
  !
  !
  type mpar
    logical               :: pinit
    type(npar)            :: npars
    type(prop)            :: props
    type(fldp)            :: fldps
    type(sflp)            :: sflps
    type(slnp)            :: slnps
    type(srcp)            :: srcps
    type(snlp)            :: snlps
    type(sbtp)            :: sbtps
    type(sdbp)            :: sdbps
    type(strp)            :: strps
    type(sbsp)            :: sbsps
    type(sicp)            :: sicps
    type(schm)            :: schms
  end type mpar
  !/
  !/ data storage
  !/
  type(grid), target, allocatable :: grids(:)
  type(sgrd), target, allocatable :: sgrds(:)
  type(mpar), target, allocatable :: mpars(:)
  !/
  !/ data aliases for structure grid(s)
  !/
  integer, pointer :: gtype
  integer, pointer :: rstype
  integer, pointer :: iclose
  integer, pointer        :: nx, ny, nsea, nseal, trflag
  integer, pointer        :: e3df(:,:), p2msf(:), us3df(:), usspf(:)
  real,    pointer        :: ussp_wn(:)
  integer, pointer        :: nbedge
  integer, pointer        :: edges(:,:), neigh(:,:)
  !
  logical, pointer        :: lmpenabled
  logical, pointer        :: sdtail
  integer, pointer        :: hslmode
  !
  ! variables for unstructured grids
  !
  integer, pointer        :: ntri,countri,countot,nnz
  integer                 :: optioncall = 3 ! take care all other options are basically wrong
  integer, pointer        :: trigp(:,:)
  real(8), pointer        :: ien(:,:), len(:,:), si(:)
  real, pointer           :: angle(:,:),angle0(:,:)
  integer,  pointer       :: ccon(:),  countcon(:), ie_cell(:),           &
       pos_cell(:),  &
       iaa(:), jaa(:), posi(:,:),                   &
       i_diag(:), ja_ie(:,:,:),                     &
       index_cell(:)
  integer*2, pointer      :: iobp(:)
  integer*1, pointer      :: iobpd(:,:), iobdp(:), iobpa(:)
  real(8), pointer        :: tria(:)
  real, pointer           :: crossdiff(:,:)
  real,pointer            :: maxx, maxy, dxymax
  logical, pointer        :: guginit
  !
  real,    pointer        :: ffacberg
  integer, pointer        :: mapsta(:,:), mapst2(:,:),            &
       mapfs(:,:), mapsf(:,:)
  !
  !
  real, pointer           :: sx, sy, x0, y0, dtcfl, dtcfli, dtmax, &
       dtmin, dmin, ctmax, fice0, ficen,     &
       ficel, pfmove, stexu, steyu, stedu,   &
       iicehmin, iicehinit, icescales(:),    &
       iicehfac, iicehdisp, iiceddisp, iicefdisp, &
       btbeta, aaircmin, aairgb
  real(8),pointer         :: gridshift ! see notes in wmghgh
  real   , pointer :: zb(:)
  real   , pointer :: clats(:)
  real   , pointer :: clatis(:) ! inverse of cos(lat) defined on isea
  real   , pointer :: cthg0s(:) ! tan(y)/r, defined on isea
  real   , pointer :: trnx(:,:), trny(:,:) ! transparency information on ix,iy
  real   , pointer :: spcbac(:,:), angarc(:)
  double precision, pointer :: xgrd(:,:), ygrd(:,:) ! x and y defined on ix,iy
  real   , pointer :: dxdp(:,:), dxdq(:,:) ! dx/dp & dx/dq defined on ix,iy
  real   , pointer :: dydp(:,:), dydq(:,:) ! dy/dp & dy/dq defined on ix,iy
  real   , pointer :: dpdx(:,:), dpdy(:,:) ! dp/dx & dp/dy defined on ix,iy
  real   , pointer :: dqdx(:,:), dqdy(:,:) ! dq/dx & dq/dy defined on ix,iy
  real   , pointer :: gsqrt(:,:) ! sqrt(g) defined on ix,iy
  real   , pointer :: hpfac(:,:) ! h_p = sqrt(g_pp) defined on ix,iy
  real   , pointer :: hqfac(:,:) ! h_q = sqrt(g_qq) defined on ix,iy
  logical, pointer :: ginit, fldry, flcx, flcy, flcth, flck, flsou, iicedisp,&
       iicesmooth
  logical, pointer :: flagll
  logical, pointer :: cmprtrck
  logical, pointer :: flagst(:)
  character(len=30), pointer :: gname
  character(len=13), pointer :: filext
  type(t_gsu), pointer :: gsu ! grid search utility object
  !/
  !/ data aliasses for structure sgrd(s)
  !/
  integer, pointer        :: nk, nk2, nth, nspec
  integer, pointer        :: mapwn(:), mapth(:)
  real, pointer           :: dth, xfr, fr1, fte, ftf, ftwn, fttr, &
       ftwl, facti1, facti2, fachfa, fachfe
  real, pointer           :: th(:), esin(:), ecos(:), es2(:),     &
       esc(:), ec2(:), sig(:), sig2(:),     &
       dsip(:), dsii(:), dden(:), dden2(:)
  logical, pointer        :: sinit
  !/
  !/ data aliasses for structure mpar(s)
  !/
  logical, pointer        :: pinit
  !/
  !/ data aliasses for structure npar(s)
  !/
  real, pointer           :: facp, xrel, xflt, fxfm, fxpm,        &
       xft, xfc, facsd, fhmax
  !/
  !/ data aliasses for structure prop(s)
  !/
  !/
  !/ data aliasses for structure fldp(s)
  !/
  !/
  !/ data aliasses for structure sflp(s)
  !/
  !/
  !/ data aliasses for structure slnp(s)
  !/
  !/
  !/ data aliasses for structure srcp(s)
  !/
  real, pointer           :: wwnmeanptail, sstxftftail
  !/
  !/ data aliasses for structure snlp(s)
  !/
  !/
  !/ data aliasses for structure sbtp(s)
  !/
  !/
  !/ data aliasses for structure sdbp(s)
  !/
  !/
  !/
  !/ data aliasing for structure schm(s)
  logical, pointer  :: fsn,fspsi,fsfct,fsnimp,fstotalimp,fstotalexp
  logical, pointer  :: fsrefraction, fsfreqshift, fssource, fsbccfl
  logical, pointer  :: do_change_wlv
  real(8), pointer  :: solverthr_stp
  real(8), pointer  :: crit_dep_stp
  logical, pointer :: b_jgs_terminate_maxiter
  logical, pointer :: b_jgs_terminate_difference
  logical, pointer :: b_jgs_terminate_norm
  logical, pointer :: b_jgs_limiter
  logical, pointer :: b_jgs_use_jacobi
  logical, pointer :: b_jgs_block_gauss_seidel
  integer, pointer :: b_jgs_maxiter
  integer, pointer :: b_jgs_limiter_func
  real(8), pointer :: b_jgs_pmin
  real(8), pointer :: b_jgs_diff_thr
  real(8), pointer :: b_jgs_norm_thr
  integer, pointer :: b_jgs_nlevel
  logical, pointer :: b_jgs_source_nonlinear
  !/
  !/ data aliasing for structure sicp(s)
  !/
contains
  !/ ------------------------------------------------------------------- /
  subroutine w3nmod ( number, ndse, ndst, naux )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         10-dec-2014 !
    !/                  +-----------------------------------+
    !/
    !/    24-feb-2004 : origination.                        ( version 3.06 )
    !/    18-jul-2006 : add input grids.                    ( version 3.10 )
    !/    10-dec-2014 : add checks for allocate status      ( version 5.04 )
    !/
    !  1. purpose :
    !
    !     set up the number of grids to be used.
    !
    !  2. method :
    !
    !     store in ngrids and allocate grids.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       number  int.   i   number of grids to be used.
    !       ndse    int.   i   error output unit number.
    !       ndst    int.   i   test output unit number.
    !       naux    int.   i   number of auxiliary grids to be used.
    !                          grids -naux:nubmer are defined, optional
    !                          parameters.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     see module documentation.
    !
    !  5. called by :
    !
    !     any program that uses this grid structure.
    !
    !  6. error messages :
    !
    !     - error checks on previous setting of variable.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !  9. switches :
    !
    !     !/s    enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3servmd, only: extcde
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)           :: number, ndse, ndst
    integer, intent(in), optional :: naux
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: i, nlow
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input and module status
    !
    if ( ngrids .ne. -1 ) then
      write (ndse,1001) ngrids
      call extcde (1)
    end if
    !
    if ( number .lt. 1 ) then
      write (ndse,1002) number
      call extcde (2)
    end if
    !
    if ( present(naux) ) then
      nlow   = -naux
    else
      nlow   = 1
    end if
    !
    if ( nlow .gt. 1 ) then
      write (ndse,1003) -nlow
      call extcde (3)
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 1.  set variable and allocate arrays
    !
    ngrids = number
    nauxgr = - nlow
    allocate ( grids(nlow:number), &
         sgrds(nlow:number), &
         mpars(nlow:number), &
         stat=istat )
    check_alloc_status ( istat )
    !
    ! -------------------------------------------------------------------- /
    ! 2.  initialize ginit and sinit
    !
    do i=nlow, number
      grids(i)%ginit  = .false.
      grids(i)%guginit  = .false.
      sgrds(i)%sinit  = .false.
      mpars(i)%pinit  = .false.
    end do
    !
    return
    !
    ! formats
    !
1001 format (/' *** error w3nmod : grids already initialized *** '/  &
         '                    ngrids = ',i10/)
1002 format (/' *** error w3nmod : illegal number of grids *** '/    &
         '                    number = ',i10/)
1003 format (/' *** error w3nmod : illegal number of aux grids *** '/&
         '                    number = ',i10/)
    !/
    !/ end of w3nmod ----------------------------------------------------- /
    !/
  end subroutine w3nmod
  !/ ------------------------------------------------------------------- /
  subroutine w3dimx  ( imod, mx, my, msea, ndse, ndst   &
       )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         10-dec-2014 |
    !/                  +-----------------------------------+
    !/
    !/    24-jun-2005 : origination.                        ( version 3.07 )
    !/    18-jul-2006 : add input grids.                    ( version 3.10 )
    !/    05-oct-2006 : add filter to array pointers.       ( version 3.10 )
    !/    02-feb-2007 : add flagst.                         ( version 3.10 )
    !/    30-oct-2009 : implement run-time grid selection.  ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    30-oct-2009 : implement unstructured grids        ( version 3.14.1)
    !/    03-sep-2012 : clean up of ug grids                ( version 4.08 )
    !/    10-dec-2014 : add checks for allocate status      ( version 5.04 )
    !/
    !  1. purpose :
    !
    !     initialize an individual spatial grid at the proper dimensions.
    !
    !  2. method :
    !
    !     allocate directly into the structure array grids. note that
    !     this cannot be done through the pointer alias!
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       imod    int.   i   model number to point to.
    !       ndse    int.   i   error output unit number.
    !       ndst    int.   i   test output unit number.
    !       mx, my, msea       like nx, ny, nsea in data structure.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !       see module documentation.
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3iogr    subr. w3iogrmd model definition file io program.
    !      ww3_grid  prog.   n/a    model set up program.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     - check on input parameters.
    !     - check on previous allocation.
    !
    !  7. remarks :
    !
    !     - grid dimensions apre passed through parameter list and then
    !       locally stored to assure consistency between allocation and
    !       data in structure.
    !     - w3setg needs to be called after allocation to point to
    !       proper allocated arrays.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s    enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3servmd, only: extcde
    !
    implicit none
    !
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: imod, mx, my, msea, ndse, ndst
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input and module status
    !
    if ( ngrids .eq. -1 ) then
      write (ndse,1001)
      call extcde (1)
    end if
    !
    if ( imod.lt.-nauxgr .or. imod.gt.ngrids ) then
      write (ndse,1002) imod, -nauxgr, ngrids
      call extcde (2)
    end if
    !
    if ( mx.lt.3 .or. (my.lt.3.and.gtype.ne.ungtype) .or. msea.lt.1 ) then
      write (ndse,1003) mx, my, msea, gtype
      call extcde (3)
    end if
    !
    if ( grids(imod)%ginit ) then
      write (ndse,1004)
      call extcde (4)
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  allocate arrays
    !
    ! nb: some array start at 0 because mapfs(iy,ix)=0 for missing points
    !
    if (gtype .ne. ungtype) then
      allocate ( grids(imod)%zb(msea),  &
           grids(imod)%xgrd(my,mx),    &
           grids(imod)%ygrd(my,mx),    &
           stat=istat                  )
      check_alloc_status ( istat )
    endif
    allocate ( grids(imod)%mapsta(my,mx),  &
         grids(imod)%mapst2(my,mx),  &
         grids(imod)%mapfs(my,mx),   &
         grids(imod)%mapsf(msea,3),  &
         grids(imod)%flagst(msea),   &
         grids(imod)%clats(0:msea),  &
         grids(imod)%clatis(0:msea), &
         grids(imod)%cthg0s(0:msea), &
         grids(imod)%trnx(my,mx),    &
         grids(imod)%trny(my,mx),    &
         grids(imod)%dxdp(my,mx),    &
         grids(imod)%dxdq(my,mx),    &
         grids(imod)%dydp(my,mx),    &
         grids(imod)%dydq(my,mx),    &
         grids(imod)%dpdx(my,mx),    &
         grids(imod)%dpdy(my,mx),    &
         grids(imod)%dqdx(my,mx),    &
         grids(imod)%dqdy(my,mx),    &
         grids(imod)%gsqrt(my,mx),   &
         grids(imod)%hpfac(my,mx),   &
         grids(imod)%hqfac(my,mx),   &
         stat=istat                  )
    check_alloc_status ( istat )
    !
    !
    grids(imod)%flagst = .true.
    grids(imod)%ginit  = .true.
    grids(imod)%mapsf(:,3)=0.
    grids(imod)%clats(0)=1.
    grids(imod)%clatis(0)=1.
    grids(imod)%cthg0s(0)=1.
    !
    !
    !
    ! -------------------------------------------------------------------- /
    ! 2.  update counters in grid
    !
    grids(imod)%nx     = mx
    grids(imod)%ny     = my
    grids(imod)%nsea   = msea
    !
    ! -------------------------------------------------------------------- /
    ! 3.  point to allocated arrays
    !
    call w3setg ( imod, ndse, ndst )
    !
    return
    !
    ! formats
    !
1001 format (/' *** error w3dimx : grids not initialized *** '/      &
         '                    run w3nmod first '/)
1002 format (/' *** error w3dimx : illegal model number *** '/       &
         '                    imod   = ',i10/                   &
         '                    nauxgr = ',i10/                   &
         '                    ngrids = ',i10/)
1003 format (/' *** error w3dimx : illegal grid dimension(s) *** '/  &
         '                    input = ',4i10 /)
1004 format (/' *** error w3dimx : array(s) already allocated *** ')
    !/
    !/ end of w3dimx ----------------------------------------------------- /
    !/
  end subroutine w3dimx
  !/ ------------------------------------------------------------------- /
  subroutine w3dims  ( imod, mk, mth, ndse, ndst )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         10-dec-2014 !
    !/                  +-----------------------------------+
    !/
    !/    19-feb-2004 : origination.                        ( version 3.06 )
    !/    18-jul-2006 : add input grids.                    ( version 3.10 )
    !/    05-oct-2006 : add filter to array pointers.       ( version 3.10 )
    !/    10-dec-2014 : add checks for allocate status      ( version 5.04 )
    !/
    !  1. purpose :
    !
    !     initialize an individual spatial grid at the proper dimensions.
    !
    !  2. method :
    !
    !     allocate directly into the structure array grids. note that
    !     this cannot be done through the pointer alias!
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       imod    int.   i   model number to point to.
    !       ndse    int.   i   error output unit number.
    !       mk,mth  int.   i   spectral dimensions.
    !       ndst    int.   i   test output unit number.
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
    !      w3iogr    subr. w3iogrmd model definition file io program.
    !      ww3_grid  prog.   n/a    model set up program.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     - check on input parameters.
    !     - check on previous allocation.
    !
    !  7. remarks :
    !
    !     - grid dimensions apre passed through parameter list and then
    !       locally stored to assure consistency between allocation and
    !       data in structure.
    !     - w3setg needs to be called after allocation to point to
    !       proper allocated arrays.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s    enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3servmd, only: extcde
    !
    implicit none
    !
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: imod, mk, mth, ndse, ndst
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer, save           :: mk2, mspec
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input and module status
    !
    if ( ngrids .eq. -1 ) then
      write (ndse,1001)
      call extcde (1)
    end if
    !
    if ( imod.lt.-nauxgr .or. imod.gt.ngrids ) then
      write (ndse,1002) imod, -nauxgr, ngrids
      call extcde (2)
    end if
    !
    if ( mk.lt.3 .or. mth.lt.4 ) then
      write (ndse,1003) mk, mth
      call extcde (3)
    end if
    !
    if ( sgrds(imod)%sinit ) then
      write (ndse,1004)
      call extcde (4)
    end if
    !
    mk2    = mk + 2
    mspec  = mk * mth
    !
    ! -------------------------------------------------------------------- /
    ! 2.  allocate arrays
    !
    allocate ( sgrds(imod)%mapwn(mspec+mth),                        &
         sgrds(imod)%mapth(mspec+mth),                        &
         sgrds(imod)%th(mth),                                 &
         sgrds(imod)%esin(mspec+mth),                         &
         sgrds(imod)%ecos(mspec+mth),                         &
         sgrds(imod)%es2(mspec+mth),                          &
         sgrds(imod)%esc(mspec+mth),                          &
         sgrds(imod)%ec2(mspec+mth),                          &
         sgrds(imod)%sig(0:mk+1),                             &
         sgrds(imod)%sig2(mspec),                             &
         sgrds(imod)%dsip(0:mk+1),                            &
         sgrds(imod)%dsii(mk),                                &
         sgrds(imod)%dden(mk),                                &
         sgrds(imod)%dden2(mspec),                            &
         stat=istat                                           )
    check_alloc_status ( istat )
    sgrds(imod)%mapwn(:)=0.
    sgrds(imod)%mapth(:)=0.
    sgrds(imod)%th(:)=0.
    sgrds(imod)%esin(:)=0.
    sgrds(imod)%ecos(:)=0.
    sgrds(imod)%es2(:)=0.
    sgrds(imod)%esc(:)=0.
    sgrds(imod)%ec2(:)=0.
    sgrds(imod)%sig(:)=0.
    sgrds(imod)%sig2(:)=0.
    sgrds(imod)%dsip(:)=0.
    sgrds(imod)%dsii(:)=0.
    sgrds(imod)%dden(:)=0.
    sgrds(imod)%dden2(:)=0.
    !
    sgrds(imod)%sinit  = .true.
    !
    ! -------------------------------------------------------------------- /
    ! 3.  point to allocated arrays
    !
    call w3setg ( imod, ndse, ndst )
    !
    ! -------------------------------------------------------------------- /
    ! 4.  update counters in grid
    !
    nk     = mk
    nk2    = mk + 2
    nth    = mth
    nspec  = mk * mth
    !
    return
    !
    ! formats
    !
1001 format (/' *** error w3dims : grids not initialized *** '/      &
         '                    run w3nmod first '/)
1002 format (/' *** error w3dims : illegal model number *** '/       &
         '                    imod   = ',i10/                   &
         '                    nauxgr = ',i10/                   &
         '                    ngrids = ',i10/)
1003 format (/' *** error w3dims : illegal grid dimension(s) *** '/  &
         '                    input = ',4i10/)
1004 format (/' *** error w3dims : array(s) already allocated *** ')
    !/
    !/ end of w3dims ----------------------------------------------------- /
    !/
  end subroutine w3dims
  !/ ------------------------------------------------------------------- /
  subroutine w3setg ( imod, ndse, ndst )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  !           j. h. alves             !
    !/                  |                        fortran 90 |
    !/                  | last update :         03-sep-2012 |
    !/                  +-----------------------------------+
    !/
    !/    24-jun-2005 : origination.                        ( version 3.07 )
    !/    09-nov-2005 : remove soft boundary options.       ( version 3.08 )
    !/    23-jun-2006 : add data for w3sln1.                ( version 3.09 )
    !/    18-jul-2006 : add input grids.                    ( version 3.10 )
    !/    05-oct-2006 : add filter to array pointers.       ( version 3.10 )
    !/    02-feb-2007 : add flagst.                         ( version 3.10 )
    !/    14-apr-2007 : add miche style limiter.            ( version 3.11 )
    !/                  ( j. h. alves )
    !/    25-apr-2007 : adding battjes-janssen sdb.         ( version 3.11 )
    !/                  ( j. h. alves )
    !/    18-sep-2007 : adding wam4 source terms.           ( version 3.13 )
    !/                  ( f. ardhuin )
    !/    27-jun-2008 : expand wam4 variants namelist       ( version 3.14 )
    !/                  ( f. ardhuin )
    !/    30-oct-2009 : implement run-time grid selection.  ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    06-dec-2010 : change from global (logical) to iclose (integer) to
    !/                  specify index closure for a grid.   ( version 3.14 )
    !/                  (t. j. campbell, nrl)
    !/    13-jul-2012 : move data structures gmd (snl3) and nonlinear
    !/                  filter (snls) from 3.15 (hlt).      ( version 4.08 )
    !/    03-sep-2012 : clean up of ug grids                ( version 4.08 )
    !/
    !  1. purpose :
    !
    !     select one of the wavewatch iii grids / models.
    !
    !  2. method :
    !
    !     point pointers to the proper variables in the proper element of
    !     the grids array.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       imod    int.   i   model number to point to.
    !       ndse    int.   i   error output unit number.
    !       ndst    int.   i   test output unit number.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     see module documentation.
    !
    !  5. called by :
    !
    !     many subroutines in eth wavewatch system.
    !
    !  6. error messages :
    !
    !     checks on parameter list imod.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !  9. switches :
    !
    !     !/prn  select propagation scheme
    !
    !     !/stn  select source terms
    !     !/nln
    !     !/btn
    !
    !     !/s    enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3servmd, only: extcde
    !
    implicit none
    !
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: imod, ndse, ndst
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input and module status
    !
    if ( ngrids .eq. -1 ) then
      write (ndse,1001)
      call extcde (1)
    end if
    !
    if ( imod.lt.-nauxgr .or. imod.gt.ngrids ) then
      write (ndse,1002) imod, -nauxgr, ngrids
      call extcde (2)
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  set model numbers
    !
    igrid  = imod
    isgrd  = imod
    ipars  = imod
    !
    ! -------------------------------------------------------------------- /
    ! 3.  set pointers in structure grid
    !
    gtype  => grids(imod)%gtype
    rstype => grids(imod)%rstype
    iclose => grids(imod)%iclose
    !
    nx     => grids(imod)%nx
    ny     => grids(imod)%ny
    nsea   => grids(imod)%nsea
    nseal  => grids(imod)%nseal
    trflag => grids(imod)%trflag
    flagll => grids(imod)%flagll
    !
    !
    e3df   => grids(imod)%e3df
    p2msf  => grids(imod)%p2msf
    us3df  => grids(imod)%us3df
    usspf  => grids(imod)%usspf
    ussp_wn => grids(imod)%ussp_wn
    ffacberg => grids(imod)%ffacberg
    !
    lmpenabled => grids(imod)%lmpenabled
    sdtail => grids(imod)%sdtail
    hslmode => grids(imod)%hslmode
    sx     => grids(imod)%sx
    sy     => grids(imod)%sy
    x0     => grids(imod)%x0
    y0     => grids(imod)%y0
    !
    dtcfl  => grids(imod)%dtcfl
    dtcfli => grids(imod)%dtcfli
    dtmax  => grids(imod)%dtmax
    dtmin  => grids(imod)%dtmin
    dmin   => grids(imod)%dmin
    ctmax  => grids(imod)%ctmax
    fice0  => grids(imod)%fice0
    gridshift  => grids(imod)%gridshift
    cmprtrck => grids(imod)%cmprtrck
    ficen  => grids(imod)%ficen
    ficel  => grids(imod)%ficel
    iicehmin  => grids(imod)%iicehmin
    iicehdisp  => grids(imod)%iicehdisp
    iicefdisp  => grids(imod)%iicefdisp
    iiceddisp  => grids(imod)%iiceddisp
    iicehfac  => grids(imod)%iicehfac
    iicehinit  => grids(imod)%iicehinit
    icescales  => grids(imod)%icescales
    pfmove => grids(imod)%pfmove
    stexu  => grids(imod)%stexu
    steyu  => grids(imod)%steyu
    stedu  => grids(imod)%stedu
    btbeta => grids(imod)%btbeta
    aairgb => grids(imod)%aairgb
    aaircmin => grids(imod)%aaircmin
    !
    ginit  => grids(imod)%ginit
    guginit  => grids(imod)%guginit
    fldry  => grids(imod)%fldry
    flcx   => grids(imod)%flcx
    flcy   => grids(imod)%flcy
    flcth  => grids(imod)%flcth
    flck   => grids(imod)%flck
    flsou  => grids(imod)%flsou
    iicedisp => grids(imod)%iicedisp
    iicesmooth => grids(imod)%iicesmooth
    !
    gname  => grids(imod)%gname
    filext => grids(imod)%filext
    trigp  => grids(imod)%trigp
    ntri     => grids(imod)%ntri
    countri     => grids(imod)%countri
    si     => grids(imod)%si
    countot    => grids(imod)%countot
    ien     => grids(imod)%ien
    len     => grids(imod)%len
    angle     => grids(imod)%angle
    angle0     => grids(imod)%angle0
    ccon     => grids(imod)%ccon
    countcon     => grids(imod)%countcon
    index_cell  => grids(imod)%index_cell
    ie_cell     => grids(imod)%ie_cell
    pos_cell     => grids(imod)%pos_cell
    iobp     => grids(imod)%iobp
    iaa      => grids(imod)%iaa
    jaa      => grids(imod)%jaa
    posi     => grids(imod)%posi
    i_diag     => grids(imod)%i_diag
    ja_ie     => grids(imod)%ja_ie
    nbedge    => grids(imod)%nbedge
    edges     => grids(imod)%edges
    neigh     => grids(imod)%neigh
    nnz      => grids(imod)%nnz
    iobpd     => grids(imod)%iobpd
    iobdp     => grids(imod)%iobdp
    iobpa     => grids(imod)%iobpa
    tria     => grids(imod)%tria
    crossdiff => grids(imod)%crossdiff
    maxx     => grids(imod)%maxx
    maxy     => grids(imod)%maxy
    dxymax   => grids(imod)%dxymax
    xgrd   => grids(imod)%xgrd
    ygrd   => grids(imod)%ygrd
    zb     => grids(imod)%zb
    !
    if ( ginit ) then
      !
      mapsta => grids(imod)%mapsta
      mapst2 => grids(imod)%mapst2
      mapfs  => grids(imod)%mapfs
      mapsf  => grids(imod)%mapsf
      flagst => grids(imod)%flagst
      !
      clats  => grids(imod)%clats
      clatis => grids(imod)%clatis
      cthg0s => grids(imod)%cthg0s
      trnx   => grids(imod)%trnx
      trny   => grids(imod)%trny
      !
      dxdp   => grids(imod)%dxdp
      dxdq   => grids(imod)%dxdq
      dydp   => grids(imod)%dydp
      dydq   => grids(imod)%dydq
      dpdx   => grids(imod)%dpdx
      dpdy   => grids(imod)%dpdy
      dqdx   => grids(imod)%dqdx
      dqdy   => grids(imod)%dqdy
      gsqrt  => grids(imod)%gsqrt
      hpfac  => grids(imod)%hpfac
      hqfac  => grids(imod)%hqfac
      !
      !
      !
      gsu  => grids(imod)%gsu
      !
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 4.  set pointers in structure sgrd
    !
    nk     => sgrds(imod)%nk
    nk2    => sgrds(imod)%nk2
    nth    => sgrds(imod)%nth
    nspec  => sgrds(imod)%nspec
    !
    dth    => sgrds(imod)%dth
    xfr    => sgrds(imod)%xfr
    fr1    => sgrds(imod)%fr1
    fte    => sgrds(imod)%fte
    ftf    => sgrds(imod)%ftf
    ftwn   => sgrds(imod)%ftwn
    fttr   => sgrds(imod)%fttr
    ftwl   => sgrds(imod)%ftwl
    facti1 => sgrds(imod)%facti1
    facti2 => sgrds(imod)%facti2
    fachfa => sgrds(imod)%fachfa
    fachfe => sgrds(imod)%fachfe
    !
    sinit  => sgrds(imod)%sinit
    !
    if ( sinit ) then
      !
      mapwn  => sgrds(imod)%mapwn
      mapth  => sgrds(imod)%mapth
      !
      th     => sgrds(imod)%th
      esin   => sgrds(imod)%esin
      ecos   => sgrds(imod)%ecos
      es2    => sgrds(imod)%es2
      esc    => sgrds(imod)%esc
      ec2    => sgrds(imod)%ec2
      sig    => sgrds(imod)%sig
      sig2   => sgrds(imod)%sig2
      dsip   => sgrds(imod)%dsip
      dsii   => sgrds(imod)%dsii
      dden   => sgrds(imod)%dden
      dden2  => sgrds(imod)%dden2
      !
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 5.  set pointers in structure mpar
    !
    pinit  => mpars(imod)%pinit
    !
    !     structure npars
    !
    facp   => mpars(imod)%npars%facp
    xrel   => mpars(imod)%npars%xrel
    xflt   => mpars(imod)%npars%xflt
    fxfm   => mpars(imod)%npars%fxfm
    fxpm   => mpars(imod)%npars%fxpm
    xft    => mpars(imod)%npars%xft
    xfc    => mpars(imod)%npars%xfc
    facsd  => mpars(imod)%npars%facsd
    fhmax  => mpars(imod)%npars%fhmax
    !
    !     structure props
    !
    !
    !     structure fldp
    !
    !
    !     structure sflps
    !
    !
    !     structure slnps
    !
    !
    !     structure srcps
    !
    wwnmeanptail=> mpars(imod)%srcps%wwnmeanptail
    sstxftftail => mpars(imod)%srcps%sstxftftail
    !
    !
    !
    !
    !     structure srnls
    !
    !
    !     structure sbtps
    !
    !
    !     structure sdbps
    !
    !
    !
    !
    !     structure sicps
    !
    !
    !    structure schm
    fsbccfl => mpars(imod)%schms%fsbccfl
    fsn => mpars(imod)%schms%fsn
    fspsi => mpars(imod)%schms%fspsi
    fsfct => mpars(imod)%schms%fsfct
    fsnimp => mpars(imod)%schms%fsnimp
    fstotalimp => mpars(imod)%schms%fstotalimp
    fstotalexp => mpars(imod)%schms%fstotalexp
    fsrefraction => mpars(imod)%schms%fsrefraction
    fsfreqshift => mpars(imod)%schms%fsfreqshift
    fssource => mpars(imod)%schms%fssource
    do_change_wlv => mpars(imod)%schms%do_change_wlv
    solverthr_stp => mpars(imod)%schms%solverthr_stp
    crit_dep_stp => mpars(imod)%schms%crit_dep_stp
    b_jgs_terminate_maxiter => mpars(imod)%schms%b_jgs_terminate_maxiter
    b_jgs_terminate_difference => mpars(imod)%schms%b_jgs_terminate_difference
    b_jgs_terminate_norm => mpars(imod)%schms%b_jgs_terminate_norm
    b_jgs_limiter => mpars(imod)%schms%b_jgs_limiter
    b_jgs_use_jacobi => mpars(imod)%schms%b_jgs_use_jacobi
    b_jgs_block_gauss_seidel => mpars(imod)%schms%b_jgs_block_gauss_seidel
    b_jgs_maxiter => mpars(imod)%schms%b_jgs_maxiter
    b_jgs_limiter_func => mpars(imod)%schms%b_jgs_limiter_func
    b_jgs_pmin => mpars(imod)%schms%b_jgs_pmin
    b_jgs_diff_thr => mpars(imod)%schms%b_jgs_diff_thr
    b_jgs_norm_thr => mpars(imod)%schms%b_jgs_norm_thr
    b_jgs_nlevel => mpars(imod)%schms%b_jgs_nlevel
    b_jgs_source_nonlinear => mpars(imod)%schms%b_jgs_source_nonlinear
    return
    !
    ! formats
    !
1001 format (/' *** error w3setg : grids not initialized *** '/      &
         '                    run w3nmod first '/)
1002 format (/' *** error w3setg : illegal model number *** '/       &
         '                    imod   = ',i10/                   &
         '                    nauxgr = ',i10/                   &
         '                    ngrids = ',i10/)
    !/
    !/ end of w3setg ----------------------------------------------------- /
    !/
  end subroutine w3setg
  !/ ------------------------------------------------------------------- /
  subroutine w3gntx ( imod, ndse, ndst )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch-iii           noaa/ncep |
    !/                  |           t. j. campbell          |
    !/                  |                        fortran 90 |
    !/                  | last update :         20-jul-2011 |
    !/                  +-----------------------------------+
    !/
    !/    30-oct-2009 : origination.                        ( version 3.13 )
    !/    06-dec-2010 : change from global (logical) to iclose (integer) to
    !/                  specify index closure for a grid.   ( version 3.14 )
    !/                  (t. j. campbell, nrl)
    !/    23-dec-2010 : fix hpfac and hqfac by including the cos(ygrd)
    !/                  factor with dxdp and dxdq terms.    ( version 3.14 )
    !/                  (t. j. campbell, nrl)
    !/    20-jul-2011 : hpfac and hqfac are now calculated using w3dist.
    !/                  result should be very similar except near pole.
    !/                  due to precision issues, hpfac and hqfac revert
    !/                  to sx and sy in case of regular grids.
    !/                  (w. e. rogers, nrl)                 ( version 3.14 )
    !/    20-jan-2017 : update to new w3gsrumd apis         ( version 6.02 )
    !/    20-jan-2017 : change calculation of curvilinear grid metric and
    !/                  derivatives calculations to use w3gsrumd:w3cgdm.
    !/                  (t.j. campbell, nrl)                ( version 6.02 )
    !/
    !  1. purpose :
    !
    !     construct required spatial grid quantities for curvilinear grids.
    !
    !  2. method :
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       imod    int.   i   model number to point to.
    !       ndse    int.   i   error output unit number.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     see module documentation.
    !
    !  5. called by :
    !
    !     any program that uses this grid structure.
    !
    !  6. error messages :
    !
    !     - check on previous initialization of grids.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !  9. switches :
    !
    !     !/s    enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3servmd, only: extcde
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: imod, ndse, ndst
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer, parameter :: nfd    = 4
    logical, parameter :: ptiled = .false.
    logical, parameter :: qtiled = .false.
    logical, parameter :: ijg    = .false.
    logical, parameter :: sphere = .false.
    integer :: prange(2), qrange(2)
    integer :: lbi(2), ubi(2), lbo(2), ubo(2), istat
    real   , allocatable :: cosa(:,:)
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input and module status
    !
    if ( ngrids .eq. -1 ) then
      write (ndse,1001)
      call extcde (1)
    end if
    !
    if ( imod.lt.-nauxgr .or. imod.gt.ngrids ) then
      write (ndse,1002) imod, -nauxgr, ngrids
      call extcde (2)
    end if
    !
    select case ( grids(imod)%gtype )
    case ( rlgtype )
    case ( clgtype )
    case ( smctype )
    case default
      write (ndse,1003) grids(imod)%gtype
      call extcde (3)
    end select
    !
    ! -------------------------------------------------------------------- /
    ! 2.  create grid search utility object
    !
    grids(imod)%gsu = w3gsuc( ijg, flagll, grids(imod)%iclose, &
         grids(imod)%xgrd, grids(imod)%ygrd )
    !
    ! -------------------------------------------------------------------- /
    ! 3.  reset grid pointers
    !
    call w3setg ( imod, ndse, ndst )
    !
    ! -------------------------------------------------------------------- /
    ! 4.  construct curvilinear grid derivatives and metric
    !     note that in the case of lon/lat grids, these quantities do not
    !     include the spherical coordinate metric (sphere=.false.).
    !
    prange = (/ 1,nx/)
    qrange = (/ 1,ny/)
    lbi = (/ 1, 1/)
    ubi = (/ny,nx/)
    lbo = (/ 1, 1/)
    ubo = (/ny,nx/)
    select case ( gtype )
      !!li  smc grid shares the settings with rectilinear grid. jgli12oct2020
    case ( rlgtype, smctype )
      call w3cgdm( ijg, flagll, iclose, ptiled, qtiled,            &
           prange, qrange, lbi, ubi, lbo, ubo, real(xgrd), real(ygrd), &
           nfd=nfd, sphere=sphere, dx=sx, dy=sy,           &
           dxdp=dxdp, dydp=dydp, dxdq=dxdq, dydq=dydq,     &
           dpdx=dpdx, dpdy=dpdy, dqdx=dqdx, dqdy=dqdy,     &
           hpfc=hpfac, hqfc=hqfac, gsqr=gsqrt,             &
           rc=istat )
      if ( istat.ne.0 ) then
        write (ndse,1004) gtype
        call extcde (4)
      end if
    case ( clgtype )
      call w3cgdm( ijg, flagll, iclose, ptiled, qtiled,            &
           prange, qrange, lbi, ubi, lbo, ubo, real(xgrd), real(ygrd), &
           nfd=nfd, sphere=sphere,                         &
           dxdp=dxdp, dydp=dydp, dxdq=dxdq, dydq=dydq,     &
           dpdx=dpdx, dpdy=dpdy, dqdx=dqdx, dqdy=dqdy,     &
           hpfc=hpfac, hqfc=hqfac, gsqr=gsqrt,             &
           rc=istat )
      if ( istat.ne.0 ) then
        write (ndse,1004) gtype
        call extcde (4)
      end if
    end select
    !
    !
    ! formats
    !
1001 format (/' *** error w3gntx : grids not initialized *** '/      &
         '                    run w3nmod first '/)
1002 format (/' *** error w3gntx : illegal model number *** '/       &
         '                    imod   = ',i10/                   &
         '                    nauxgr = ',i10/                   &
         '                    ngrids = ',i10/)
1003 format (/' *** error w3gntx : unsupported type of grid *** '/   &
         '                    gtype  = ',i10/)
1004 format (/' *** error w3gntx : error occured in w3cgdm *** '/    &
         '                    gtype  = ',i10/)
    !
    !/
    !/ end of w3gntx ----------------------------------------------------- /
    !/
  end subroutine w3gntx
  !/ ------------------------------------------------------------------- /
  subroutine w3dimug  ( imod, mtri, mx, countota, nnz, ndse, ndst )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch-iii           noaa/ncep |
    !/                  |             f.ardhuin             |
    !/                  |                        fortran 90 |
    !/                  | last update :         15-mar-2007 !
    !/                  +-----------------------------------+
    !/
    !/    15-mar-2007 : origination.                        ( version 3.14 )
    !/    11-may-2015 : updates to 2-ways nestings for ug   ( version 5.08 )
    !/
    !  1. purpose :
    !
    !     initialize an individual spatial grid at the proper dimensions.
    !
    !  2. method :
    !
    !     allocate directly into the structure array grids. note that
    !     this cannot be done through the pointer alias!
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       imod    int.   i   model number to point to.
    !       ndse    int.   i   error output unit number.
    !       ndst    int.   i   test output unit number.
    !       mx, mtri, msea       like nx, ntri, nsea in data structure.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !       see module documentation.
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3iogr    subr. w3iogrmd model definition file io program.
    !      ww3_grid  prog.   n/a    model set up program.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     - check on input parameters.
    !     - check on previous allocation.
    !
    !  7. remarks :
    !
    !     - grid dimensions apre passed through parameter list and then
    !       locally stored to assure consistency between allocation and
    !       data in structure.
    !     - w3setg needs to be called after allocation to point to
    !       proper allocated arrays.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s    enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3servmd, only: extcde
    !
    implicit none
    !
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: imod, mtri, mx, countota, nnz, ndse, ndst
    integer                 :: iaproc = 1
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input and module status
    !
    if ( ngrids .eq. -1 ) then
      write (ndse,1001)
      call extcde (1)
    end if
    !
    if ( imod.lt.-nauxgr .or. imod.gt.ngrids ) then
      write (ndse,1002) imod, ngrids
      call extcde (2)
    end if
    if ( grids(imod)%guginit ) then
      write (ndse,1004)
      call extcde (4)
    end if
    !
    !
    ! -------------------------------------------------------------------- /
    ! 2.  allocate arrays
    !
    allocate ( grids(imod)%trigp(3,mtri),                         &
         grids(imod)%si(mx),                                &
         grids(imod)%xgrd(1,mx),                            &
         grids(imod)%ygrd(1,mx),                            &
         grids(imod)%zb(mx),                                &
         grids(imod)%tria(mtri),                            &
         grids(imod)%crossdiff(6,mtri),                     &
         grids(imod)%ien(mtri,6),                           &
         grids(imod)%len(mtri,3),                           &
         grids(imod)%angle(mtri,3),                         &
         grids(imod)%angle0(mtri,3),                        &
         grids(imod)%ccon(mx),                              &
         grids(imod)%countcon(mx),                          &
         grids(imod)%index_cell(mx+1),                      &
         grids(imod)%ie_cell(countota),                     &
         grids(imod)%pos_cell(countota),                    &
         grids(imod)%iaa(nx+1),                             &
         grids(imod)%jaa(nnz),                              &
         grids(imod)%posi(3,countota),                      &
         grids(imod)%i_diag(nx),                            &
         grids(imod)%ja_ie(3,3,mtri),                       &
         grids(imod)%iobp(mx),                              &
         grids(imod)%iobpd(nth,mx),                         &
         grids(imod)%iobdp(mx),                             &
         grids(imod)%iobpa(mx),                             &
         stat=istat                                         )
    check_alloc_status ( istat )
    !
    grids(imod)%iobp(:)=1
    !
    !some segmentation troubles can appear, they are related with the allocation of
    !normal(1st dimension) and the nesting of the triangulated grid.
    ! -------------------------------------------------------------------- /
    ! 3.  point to allocated arrays
    !
    call w3setg ( imod, ndse, ndst )
    !
    ! -------------------------------------------------------------------- /
    ! 4.  update counters in grid
    !     note that in the case of lon/lat grids, these quantities do not
    !     include the spherical coordinate metric (sphere=.false.).
    !
    ntri   = mtri
    countot=countota
    grids(imod)%guginit  = .true.
    return
    !
    ! formats
    !
1001 format (/' *** error w3dimug : grids not initialized *** '/      &
         '                    run w3nmod first '/)
1002 format (/' *** error w3dimug : illegal model number *** '/       &
         '                    imod   = ',i10/                   &
         '                    ngrids = ',i10/)
1004 format (/' *** error w3dimug : array(s) already allocated *** ')
    !/
    !/ end of w3dimug ----------------------------------------------------- /
    !/
  end subroutine w3dimug
  !/ ------------------------------------------------------------------- /
  subroutine w3setref
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           f. ardhuin              |
    !/                  |                        fortran 90 |
    !/                  | last update :         13-nov-2013 |
    !/                  +-----------------------------------+
    !/
    !/    13-nov-2013 : origination.                        ( version 4.13 )
    !/
    !  1. purpose :
    !
    !     update reflection directions at shoreline.
    !
    !  2. method :
    !
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       none
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
    !      ww3_grid  prog. ww3_grid grid preprocessor
    !      w3ulev    subr. w3updtmd water level update
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !       !/s      enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/
    integer                 :: isea, ix, iy, ixy, ixn, ixp, iyn, iyp
    integer                 :: j, k, neigh1(0:7)
    integer                 :: ilev, nlev
    real                    :: trix(ny*nx), triy(ny*nx), dx, dy,    &
         cosavg, sinavg, thavg, angles(0:7), clat
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  preparations --------------------------------------------------- *
    !
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3setref ----------------------------------------------------- /
    !/
  end subroutine w3setref
  !/
  !/ end of module w3gdatmd -------------------------------------------- /
  !/
end module w3gdatmd
