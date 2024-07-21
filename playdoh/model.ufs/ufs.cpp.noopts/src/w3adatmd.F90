!> @file
!> @brief define data structures to set up wave model auxiliary data
!>  for several models simultaneously.
!>
!> @author h. l. tolman
!> @date   22-mar-2021
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
!> @brief define data structures to set up wave model auxiliary data
!>  for several models simultaneously.
!>
!> @details the number of grids is taken from w3gdatmd, and needs to be
!>  set first with w3dimg.
!>
!> @author h. l. tolman
!> @date   22-mar-2021
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3adatmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         22-mar-2021 |
  !/                  +-----------------------------------+
  !/
  !/    28-dec-2004 : origination.                        ( version 3.06 )
  !/    04-may-2005 : adding mpi_comm_wave.               ( version 3.07 )
  !/    20-jul-2005 : adding output fields.               ( version 3.07 )
  !/    09-nov-2005 : removing soft boundary option.      ( version 3.08 )
  !/    13-jun-2006 : splitting store in g/sstore.        ( version 3.09 )
  !/    04-oct-2006 : add filter to array pointers.       ( version 3.10 )
  !/    28_mar-2007 : add partitioned data arrays.        ( version 3.11 )
  !/                  add aditional undefined arrays.
  !/    22-feb-2008 ; modify mapth2 declaration.          ( version 3.13 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/    29-oct-2010 : adding unstructured grid data.      ( version 3.14 )
  !/                  (a. roland and f. ardhuin)
  !/    31-oct-2010 : adding output parameters            ( version 3.14 )
  !/    12-dec-2012 : adding smc grid.  jg_li             ( version 4.08 )
  !/    26-dec-2012 : memory reduction for outputs.       ( version 4.11 )
  !/                  add w3xeta.
  !/    28-jun-2013 : bug fix initialization p2sms.       ( version 4.11 )
  !/    11-nov-2013 : smc and rotated grid incorporated in the main
  !/                  trunk                               ( version 4.13 )
  !/    14-nov-2013 : move orphaned arrays as scalar to w3srce.
  !/                  here update of documentation only.
  !/                  (z0s, cds, emn, fmn, wnm, amx)      ( version 4.13 )
  !/    30-apr-2014 : memory reduction for group3.        ( version 5.00 )
  !/    10-dec-2014 : add checks for allocate status      ( version 5.04 )
  !/    01-may-2017 : adds directional mss parameters     ( version 6.02 )
  !/    30-jul-2017 : adds tws parameter                  ( version 6.02 )
  !/    05-jun-2018 : adds pdlib and memcheck             ( version 6.04 )
  !/    21-aug-2018 : add wbt parameter                   ( version 6.06 )
  !/    22-mar-2021 : adds taua, wnmean, tauoc parameters ( version 7.13 )
  !/    06-may-2021 : smc shares variables with pr2/3.    ( version 7.13 )
  !
  !/
  !/    copyright 2009-2013 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     define data structures to set up wave model auxiliary data for
  !     several models simultaneously.
  !
  !  2. variables and types :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      nadata    int.  public   number of models in array dim.
  !      iadata    int.  public   selected model for output, init. at -1.
  !      mpibuf    i.p.  public   number of buffer arrays for 'hidden'
  !                               mpi communications (no hiding for
  !                               mpibuf = 1).
  !      wadat     type  public   basic data structure.
  !      wadats    wadat public   array of data structures.
  !     ----------------------------------------------------------------
  !
  !     all elements of wadat are aliased to pointers with the same
  !     name. these pointers are defined as :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !     internal model definition:
  !
  !      cg        r.a.  public   group velocities for all wave model
  !                               sea points and frequencies.
  !      wn        r.a.  public   idem, wavenumbers.
  !
  !     aux. arrays for model input:
  !
  !      ca0-i     r.a.  public   absolute current velocity (initial
  !                               and inc.) in w3ucur.
  !      cd0-i     r.a.  public   current direction (initial and
  !                               increment) in w3ucur.
  !      ua0-i     r.a.  public   absolute wind speeds (initial and
  !                               incr.) in w3uwnd                (m/s)
  !      ud0-i     r.a.  public   wind direction (initial and incr.)
  !                               in w3uwnd                       (rad)
  !      as0-i     r.a.  public   stability par. (initial and incr.)
  !                               in w3uwnd                      (degr)
  !      ma0-i     r.a.  public   absolute atmospheric momentum (initial
  !                               and inc.) in w3utau.
  !      ra0-i     r.a.  public   absolute air density (initial and inc.)
  !                               in w3urho.
  !      md0-i     r.a.  public   atmospheric momentum direction (initial and
  !                               increment) in w3utau.
  !      atrnx/y   r.a.  public   actual transparency info.
  !
  !     fields of mean wave parameters:
  !
  !      dw        r.a.  public   water depths.
  !      ua        r.a.  public   absolute wind speeds.
  !      ud        r.a.  public   absolute wind direction.
  !      u10       r.a.  public   wind speed used.
  !      u10d      r.a.  public   wind direction used.
  !      as        r.a.  public   stability parameter.
  !      cx/y      r.a.  public   current components.
  !      taua      r.a.  public   absolute atmospheric momentum.
  !      tauadir   r.a.  public   absolute atmospheric momentum direction.
  !
  !      hs        r.a.  public   wave height.
  !      wlm       r.a.  public   mean wave length.
  !      t02       r.a.  public   mean wave period (m0,2).
  !      t0m1      r.a.  public   mean wave period (m0,-1).
  !      t01       r.a.  public   mean wave period (m0,1).
  !      fp0       r.a.  public   peak frequency.
  !      thm       r.a.  public   mean wave direction.
  !      ths       r.a.  public   mean directional spread.
  !      thp0      r.a.  public   peak direction.
  !      hsig      r.a.  public   height of infragravity waves
  !      stmaxe    r.a.  public   expected maximum surface elevation (crest)
  !      stmaxd    r.a.  public   std of maximum surface elevation
  !      hmaxe     r.a.  public   expected maximum wave height (from covariance)
  !      hmaxd     r.a.  public   std of hmaxe
  !      hcmaxe    r.a.  public   expected maximum wave height (from crest)
  !      hcmaxd    r.a.  public   std of hcmaxe
  !      wbt       r.a.  public   dominant wave breaking probability
  !                               (b_t in babanin et al. (2001, jgr))
  !      wnmean    r.a.  public   mean wave number
  !
  !      charn     r.a.  public   charnock parameter for air-sea friction.
  !      tws       r.a.  public   wind sea period (used for flux parameterizations)
  !      cge       r.a.  public   energy flux.
  !      phiaw     r.a.  public   wind to wave energy flux.
  !      tauwix/y  r.a.  public   wind to wave energy flux.
  !      tauwnx/y  r.a.  public   wind to wave energy flux.
  !      whitecap  r.a.  public    1 : whitecap coverage
  !                                2 : whitecap thickness
  !                                3 : mean breaking height
  !                                4 : mean breaking height
  !
  !      sxx       r.a.  public   radiation stresses.
  !      tauox/y   r.a.  public   wave-ocean momentum flux.
  !      bhd       r.a.  public   wave-induced pressure (j term, smith jpo 2006)
  !      phioc     r.a.  public   waves to ocean energy flux.
  !      tusx/y    r.a.  public   volume transport associated to stokes drift.
  !      ussx/y    r.a.  public   surface stokes drift.
  !      tauocx/y  r.a.  public   total ocean momentum flux
  !      tauice    r.a.  public   wave-ice momentum flux.
  !      phice     r.a.  public   waves to ice energy flux.
  !
  !      us3d      r.a.  public   3d stokes drift.
  !      ussp      r.a.  public   partitioned surface stokes drift
  !
  !      usshx/y   r.a.  public   surface layer averaged stokes drift.
  !
  !      aba       r.a.  public   near-bottom rms wave ex. amplitude.
  !      abd       r.a.  public   corresponding direction.
  !      uba       r.a.  public   near-bottom rms wave velocity.
  !      ubd       r.a.  public   corresponding direction.
  !      bedforms  r.a.  public   bed for parameters
  !      phibbl    r.a.  public   energy loss in wbbl.
  !      taubbl    r.a.  public   momentum loss in wbbl.
  !
  !      mssx/y    r.a.  public   surface mean square slopes in x and y direction.
  !      mscx/y    r.a.  public   phillips constant.
  !      mssd      r.a.  public   direction of mssx
  !      mscd      r.a.  public   direction of mscx
  !      qp        r.a.  public   goda peakedness parameter.
  !
  !      dtdyn     r.a.  public   mean dynamic time step (raw).
  !      fcut      r.a.  public   cut-off frequency for tail.
  !      cflxymax  r.a.  public   max. cfl number for spatial advection.
  !      cflthmax  r.a.  public   max. cfl number for refraction.
  !      cflkmax   r.a.  public   max. cfl number for wavenumber shift.
  !
  !    orphans, commented out here, now automatic arrays in w3wave, ....
  !
  !      drat      r.a.  public   density ration air/water. was
  !                               placeholder only. now scalar in w3srce,
  !      tauwx/y   r.a.  public   stresses.
  !
  !    derivatives in space ....
  !
  !      dddx      r.a.  public   spatial derivatives of the depth.
  !      dcxdx     r.a.  public   spatial dirivatives of the current.
  !
  !    mean parameters from partitiones spectra, 2d array with el.
  !    0 holding wind sea data, and 1:noswll holding swell fields.
  !    last two arrays are regular single-entry arrays.
  !
  !      phs       r.a.  public   wave height of partition.
  !      ptp       r.a.  public   peak period of partition.
  !      plp       r.a.  public   peak wave leingth of partition.
  !      pdir      r.a.  public   mean direction of partition.
  !      psi       r.a.  public   mean spread of partition.
  !      pws       r.a.  public   wind sea fraction of partition.
  !
  !      pwst      r.a.  public   total wind sea fraction.
  !      pnr       r.a.  public   number of partitions found.
  !
  !      pthp0     r.a.  public   peak wave direction of partition.
  !      pqp       r.a.  public   goda peakdedness parameter of partition.
  !      ppe       r.a.  public   jonswap peak enhancement factor of partition.
  !      pgw       r.a.  public   gaussian frequency width of partition.
  !      psw       r.a.  public   spectral width of partition.
  !      ptm1      r.a.  public   mean wave period (m-1,0) of partition.
  !      pt1       r.a.  public   mean wave period (m0,1) of partition.
  !      pt2       r.a.  public   mean wave period (m0,2) of partition.
  !      pep       r.a.  public   peak spectral density of partition.
  !
  !     empty dummy fields (noextr)
  !
  !      usero     r.a.  public   empty output arrays than can be
  !                               used by users as a simple means to
  !                               add output.
  !
  !     map data for propagation schemes (1up).
  !
  !      is0/2     i.a.  public   spectral propagation maps.
  !      facvx/y   r.a.  public   spatial propagation factor map.
  !
  !     map data for propagation schemes (uq).
  !
  !      nmxn      int.  public    counters for mapx2, see w3map3.
  !      nmyn      int.  public
  !      nmxy      int.  public    dimension of mapxy.
  !      nactn     int.  public    dimension of mapaxy.
  !      ncent     int.  public    dimension of mapaxy.
  !      mapx2     i.a.  public    map for prop. in 'x' (longitude) dir.
  !      mapy2     i.a.  public    idem in y' (latitude) direction.
  !      mapxy     i.a.  public
  !      mapaxy    i.a.  public    list of active points used in w3qck1.
  !      mapcxy    i.a.  public    list of central points used in avg.
  !      mapth2    i.a.  public    like mapx2 for refraction (rotated
  !                                and shifted, see w3ktp3). like mapaxy.
  !      mapwn2    i.a.  public    like mapx2 for wavenumber shift.
  !      maptrn    l.a.  public    map to block out gse mitigation in
  !                                proper grid points.
  !
  !     nonlinear interactions ( !/nl1 ) :
  !
  !      nfr       int.  public   nuber of frequencies ( nfr = nk )
  !      nfrhgh    int.  public   auxiliary frequency counter.
  !      nfrchg    int.  public   id.
  !      nspecx-y  int.  public   auxiliary spectral counter.
  !      ipnn      i.a.  public   spectral address for snl.
  !      imnn      i.a.  public   id.
  !      icnn      i.a.  public   id.
  !      daln      real  public   lambda dependend weight factors.
  !      awgn      real  public   interpolation weights for snl.
  !      swgn      real  public   interpolation weights for diag. term.
  !      af11      r.a.  public   scaling array (f**11)
  !      nlinit    log.  public   flag for initialization.
  !
  !     mpp / mpi variables :
  !
  !      iappro    i.a.  public   processor numbers for propagation calc.
  !                               for each spectral component.
  !      mpi_comm_wave
  !                int.  public   communicator used in the wave model.
  !      mpi_comm_wcmp
  !                int.  public   idem, computational proc. only.
  !      ww3_field_vec, ww3_spec_vec
  !                int.  public   mpi derived vecor types.
  !      nrqsg1    int.  public   number of handles in irqsg1.
  !      nrqsg2    int.  public   number of handles in irqsg2.
  !      ibfloc    int.  public   present active buffer number.
  !      isploc    int.  public   corresponding local spectral bin number
  !                               (1,nsploc,1).
  !      nsploc    int.  public   total number of spectral bins for which
  !                               prop. is performed on present cpu.
  !      bstat     i.a.  public   status of buffer (size mpibuf):
  !                                 0: inactive.
  !                                 1: a --> store (active or finished).
  !                                 2: store --> a (active or finished).
  !      bispl     i.a.  public   local spectral bin number for buffer
  !                               (size mpibuf).
  !      irqsg1    i.a.  public   mpi request handles for scatters and
  !                               gathers to a() (persistent).
  !      irqsg2    i.a.  public   mpi request handles for gathers and
  !                               scatters to store (persistent).
  !    g/sstore    r.a.  public   communication buffer (nsea,mpibuf).
  !      sppnt     r.a.  public   point output buffer.
  !
  !     other:
  !
  !      itime     int.  public   discrete time step counter.
  !      ipass     int.  public   pass counter for log file.
  !      idlast    int.  public   last day id for log file.
  !      nsealm    int.  public   maximum number of local sea points.
  !      alpha     r.a.  public   phillips' alpha.
  !      flcold    log.  public   flag for 'cold start' of model.
  !      fliwnd    log.  public   flag for initialization of model
  !                               based on wind.
  !      ainit(2)  log.  public   flag for array initialization.
  !      fl_all    log.  public   flag for all/partial  initialization.
  !     ----------------------------------------------------------------
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3naux    subr. public   set number of grids/models.
  !      w3dima    subr. public   set dimensions of arrays.
  !      w3dmnl    subr. public   set dimensions of arrays.   ( !/nl1 )
  !      w3seta    subr. public   point to selected grid / model.
  !      w3xeta    subr. public   like w3seta for expanded output arrays.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      w3setg    subr. w3gdatmd point to proper model grid.
  !      strace    subr. w3servmd subroutine tracing.
  !      extcde    subr. w3servmd abort program with exit code.
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !
  !     - the number of grids is taken from w3gdatmd, and needs to be
  !       set first with w3dimg.
  !
  !  6. switches :
  !
  !     !/shrd, !/dist, !/mpi
  !              shared / distributed memory model
  !
  !     !/prn    propagation scheme selection.
  !
  !     !/s      enable subroutine tracing.
  !     !/t      enable test output
  !
  !  7. source code :
  !
  !/ ------------------------------------------------------------------- /
  use w3servmd, only : print_memcheck
  ! module default
  implicit none
  public
  !/
  !/ module private variable for checking error returns
  !/
  integer, private        :: istat
  !/
  !/ conventional declarations
  !/
  integer                 :: nadata = -1, iadata = -1
  integer, parameter      :: mpibuf = 6
  !/
  !/ data structure wadat
  !/
  type wadat
    !
    ! the grid
    !
    real, pointer         :: cg(:,:), wn(:,:)
    !
    ! arrays for processing model input
    !
    real, pointer         :: ca0(:), cai(:), cd0(:), cdi(:),      &
         ua0(:), uai(:), ud0(:), udi(:),      &
         ma0(:), mai(:), ra0(:), rai(:),      &
         md0(:), mdi(:), as0(:), asi(:),      &
         atrnx(:,:), atrny(:,:)
    !
    ! output fields group 1)
    !
    real, pointer         :: dw(:), ua(:), ud(:), u10(:), u10d(:),&
         as(:), cx(:), cy(:), taua(:), tauadir(:)
    !
    ! output fields group 2)
    !
    real, pointer         :: hs(:),  wlm(:),  t02(:), t0m1(:),   &
         t01 (:),  fp0(:),  thm(:),          &
         ths(:),  thp0(:),                   &
         hsig(:), stmaxe(:), stmaxd(:),      &
         hmaxe(:), hcmaxe(:), hmaxd(:),      &
         hcmaxd(:), qp(:), wbt(:), wnmean(:)
    real, pointer         :: xhs(:), xwlm(:), xt02(:), xt0m1(:),  &
         xt01 (:), xfp0(:), xthm(:),          &
         xths(:), xthp0(:),                   &
         xhsig(:), xstmaxe(:), xstmaxd(:),    &
         xhmaxe(:), xhcmaxe(:), xhmaxd(:),    &
         xhcmaxd(:), xqp(:), xwbt(:),         &
         xwnmean(:)
    !
    ! output fields group 3)
    !
    real, pointer         ::  ef(:,:),  th1m(:,:),  sth1m(:,:),   &
         th2m(:,:),  sth2m(:,:) !, wn(:,:)
    real, pointer         ::  xef(:,:),  xth1m(:,:),  xsth1m(:,:),&
         xth2m(:,:),  xsth2m(:,:) !, xwn(:,:)
    !
    ! output fields group 4)
    !
    real, pointer         :: phs(:,:),  ptp(:,:),  plp(:,:),      &
         pdir(:,:),  psi(:,:),  pws(:,:),     &
         pwst(:),  pnr(:), pgw(:,:),          &
         pthp0(:,:), pqp(:,:), ppe(:,:),      &
         psw(:,:), ptm1(:,:), pt1(:,:),       &
         pt2(:,:), pep(:,:)
    real, pointer         :: xphs(:,:), xptp(:,:), xplp(:,:),     &
         xpdir(:,:), xpsi(:,:), xpws(:,:),    &
         xpwst(:), xpnr(:), xpgw(:,:),        &
         xpthp0(:,:), xpqp(:,:), xppe(:,:),   &
         xpsw(:,:), xptm1(:,:), xpt1(:,:),    &
         xpt2(:,:), xpep(:,:)
    !
    ! output fields group 5)
    !
    real, pointer         ::  charn(:),  cge(:),  phiaw(:),       &
         tauwix(:),  tauwiy(:),  tauwnx(:),  &
         tauwny(:),  whitecap(:,:), tws(:)
    real, pointer         :: xcharn(:), xcge(:), xphiaw(:),       &
         xtauwix(:), xtauwiy(:), xtauwnx(:),  &
         xtauwny(:), xwhitecap(:,:), xtws(:)
    !
    ! output fields group 6)
    !
    real, pointer         ::  sxx(:),  syy(:),  sxy(:),  tauox(:),&
         tauoy(:),  bhd(:),  phioc(:),       &
         tusx(:),  tusy(:),  ussx(:),        &
         ussy(:), tauocx(:), tauocy(:),      &
         prms(:),  tpms(:), phice(:),        &
         tauice(:,:)
    real, pointer         ::  p2sms(:,:),  us3d(:,:), ussp(:,:)
    real, pointer         :: xsxx(:), xsyy(:), xsxy(:), xtauox(:),&
         xtauoy(:), xbhd(:), xphioc(:),       &
         xtusx(:), xtusy(:), xussx(:),        &
         xussy(:), xtauocx(:), xtauocy(:),    &
         xprms(:), xtpms(:), xphice(:),       &
         xtauice(:,:)
    real, pointer         :: xp2sms(:,:), xus3d(:,:), xussp(:,:)
    real, pointer         :: xusshx(:), xusshy(:)
    !
    ! output fields group 7)
    !
    real, pointer         ::  aba(:),  abd(:),  uba(:),  ubd(:),  &
         bedforms(:,:),  phibbl(:),          &
         taubbl(:,:)
    real, pointer         :: xaba(:), xabd(:), xuba(:), xubd(:),  &
         xbedforms(:,:), xphibbl(:),          &
         xtaubbl(:,:)
    !
    ! output fields group 8)
    !
    real, pointer         ::  mssx(:),  mssy(:),  mssd(:),        &
         mscx(:),  mscy(:),  mscd(:)
    real, pointer         ::  xmssx(:), xmssy(:), xmssd(:),       &
         xmscx(:), xmscy(:), xmscd(:)
    !
    ! output fields group 9)
    !
    real, pointer         ::  dtdyn(:),  fcut(:),  cflxymax(:),   &
         cflthmax(:),  cflkmax(:)
    real, pointer         :: xdtdyn(:), xfcut(:), xcflxymax(:),   &
         xcflthmax(:), xcflkmax(:)
    !
    ! output fields group 10)
    !
    real, pointer         ::  usero(:,:)
    real, pointer         :: xusero(:,:)
    ! output fileds for langmuir mixing parameterization
    real, pointer         :: usshx(:), usshy(:)
    !
    ! spatial derivatives
    !
    real, pointer         :: dddx(:,:), dddy(:,:), dcxdx(:,:),    &
         dcydx(:,:), dcxdy(:,:), dcydy(:,:)
    real, pointer         :: dcdx(:,:,:), dcdy(:,:,:)
    !
    !
    !
    !
    ! warning defined but not set if ugtype .eq. .t.
    integer, pointer      :: iter(:,:)
    !
    !
    integer, pointer      :: iappro(:)
    integer               :: mpi_comm_wave, mpi_comm_wcmp,        &
         ww3_field_vec, ww3_spec_vec,         &
         nrqsg1 = 0, nrqsg2, ibfloc, isploc,  &
         nsploc
    integer               :: bstat(mpibuf), bispl(mpibuf)
    integer, pointer      :: irqsg1(:,:), irqsg2(:,:)
    real, pointer         :: gstore(:,:), sstore(:,:)
    real, pointer         :: sppnt(:,:,:)
    !
    integer               :: itime, ipass, idlast, nsealm
    real, pointer         :: alpha(:,:)
    logical               :: ainit, ainit2, fl_all, flcold, fliwnd
    !
  end type wadat
  !/
  !/ data storage
  !/
  type(wadat), target, allocatable :: wadats(:)
  !/
  !/ data aliases for structure wadat(s)
  !/
  real, pointer :: usshx(:), usshy(:)
  !
  real, pointer           :: cg(:,:), wn(:,:)
  real, pointer           :: ic3wn_r(:,:), ic3wn_i(:,:), ic3cg(:,:)
  !
  real, pointer           :: ca0(:), cai(:), cd0(:), cdi(:),      &
       ua0(:), uai(:), ud0(:), udi(:),      &
       ma0(:), mai(:), ra0(:), rai(:),      &
       md0(:), mdi(:), as0(:), asi(:),      &
       atrnx(:,:), atrny(:,:)
  !
  real, pointer           :: dw(:), ua(:), ud(:), u10(:), u10d(:),&
       as(:), cx(:), cy(:), taua(:), tauadir(:)
  !
  real, pointer           :: hs(:), wlm(:),  t02(:), t0m1(:),     &
       t01 (:), fp0(:), thm(:), ths(:),     &
       thp0(:), hsig(:),                    &
       stmaxe(:), stmaxd(:), hmaxe(:),      &
       hcmaxe(:), hmaxd(:), hcmaxd(:),      &
       qp(:), wbt(:), wnmean(:)
  !
  real, pointer           :: ef(:,:), th1m(:,:), sth1m(:,:),      &
       th2m(:,:), sth2m(:,:)
  !
  real, pointer           :: phs(:,:), ptp(:,:), plp(:,:),        &
       pdir(:,:), psi(:,:), pws(:,:),       &
       pwst(:), pnr(:), pgw(:,:), psw(:,:), &
       pthp0(:,:), pqp(:,:), ppe(:,:),      &
       ptm1(:,:), pt1(:,:), pt2(:,:),pep(:,:)
  !
  real, pointer           :: charn(:), cge(:), phiaw(:),          &
       tauwix(:), tauwiy(:), tauwnx(:),     &
       tauwny(:), whitecap(:,:), tws(:)
  !
  real, pointer           :: sxx(:), syy(:), sxy(:), tauox(:),    &
       tauoy(:), bhd(:), phioc(:),          &
       tusx(:), tusy(:), ussx(:), ussy(:),  &
       tauocx(:), tauocy(:), prms(:),       &
       tpms(:), phice(:), tauice(:,:)
  real, pointer           :: p2sms(:,:), us3d(:,:), ussp(:,:)
  !
  real, pointer           :: aba(:), abd(:), uba(:), ubd(:),      &
       bedforms(:,:), phibbl(:), taubbl(:,:)
  !
  real, pointer           :: mssx(:), mssy(:), mssd(:),           &
       mscx(:), mscy(:), mscd(:)
  !
  real, pointer           :: dtdyn(:), fcut(:), cflxymax(:),      &
       cflthmax(:), cflkmax(:)
  !
  real, pointer           :: usero(:,:)
  !
  !     real, pointer           :: tauwx(:), tauwy(:)
  !
  real, pointer           :: dddx(:,:), dddy(:,:), dcxdx(:,:),    &
       dcydx(:,:), dcxdy(:,:), dcydy(:,:)
  real, pointer           :: dcdx(:,:,:), dcdy(:,:,:)
  !
  !
  !
  !
  integer, pointer        :: iter(:,:)
  !
  !
  integer, pointer        :: iappro(:)
  integer, pointer        :: mpi_comm_wave, mpi_comm_wcmp,        &
       ww3_field_vec, ww3_spec_vec,         &
       nrqsg1, nrqsg2, ibfloc, isploc,      &
       nsploc
  integer, pointer        :: bstat(:), bispl(:)
  integer, pointer        :: irqsg1(:,:), irqsg2(:,:)
  real, pointer           :: gstore(:,:), sstore(:,:)
  real, pointer           :: sppnt(:,:,:)
  !
  integer, pointer        :: itime, ipass, idlast, nsealm
  real, pointer           :: alpha(:,:)
  logical, pointer        :: ainit, ainit2, fl_all, flcold, fliwnd
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief set up the number of grids to be used.
  !>
  !> @details use data stored in ngrids in w3gdatmd.
  !>
  !> @param[in] ndse error output unit number.
  !> @param[in] ndst test output unit number.
  !>
  !> @author h. l. tolman
  !> @date   10-dec-2014
  !>
  subroutine w3naux ( ndse, ndst )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         10-dec-2014 !
    !/                  +-----------------------------------+
    !/
    !/    14-dec-2004 : origination.                        ( version 3.06 )
    !/    04-oct-2006 : add filter to array pointers.       ( version 3.10 )
    !/    10-dec-2014 : add checks for allocate status      ( version 5.04 )
    !/
    !  1. purpose :
    !
    !     set up the number of grids to be used.
    !
    !  2. method :
    !
    !     use data stored in ngrids in w3gdatmd.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
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
    !     any program that uses this grid structure.
    !
    !  6. error messages :
    !
    !     - error checks on previous setting of variable ngrids.
    !
    !  7. remarks :
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
    use w3gdatmd, only: ngrids
    use w3servmd, only: extcde
    use w3odatmd, only: iaproc
    !
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: ndse, ndst
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: i
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input and module status
    !
    if ( ngrids .eq. -1 ) then
      write (ndse,1001) ngrids
      call extcde (1)
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  set variable and allocate arrays
    !
    allocate ( wadats(ngrids), stat=istat )
    check_alloc_status ( istat )
    nadata = ngrids
    !
    ! -------------------------------------------------------------------- /
    ! 3.  initialize parameters
    !
    do i=1, ngrids
      wadats(i)%itime  = 0
      wadats(i)%ipass  = 0
      wadats(i)%idlast = 0
      wadats(i)%nsealm = 0
      wadats(i)%flcold = .false.
      wadats(i)%fliwnd = .false.
      wadats(i)%ainit  = .false.
      wadats(i)%ainit2 = .false.
      wadats(i)%fl_all = .false.
    end do
    !
    !
    return
    !
    ! formats
    !
1001 format (/' *** error w3naux : ngrids not yet set *** '/         &
         '                    ngrids = ',i10/                   &
         '                    run w3nmod first'/)
    !
    !/
    !/ end of w3naux ----------------------------------------------------- /
    !/
  end subroutine w3naux
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief initialize an individual data grid at the proper dimensions.
  !>
  !> @details allocate directly into the structure array. note that
  !>  this cannot be done through the pointer alias!
  !>
  !> @param[in] imod   model number to point to.
  !> @param[in] ndse   error output unit number.
  !> @param[in] ndst   test output unit number.
  !> @param[in] d_only flag for initializing data arrays only.
  !>
  !> @author h. l. tolman
  !> @date   22-mar-2021
  !>
  subroutine w3dima  ( imod, ndse, ndst, d_only )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 !
    !/                  +-----------------------------------+
    !/
    !/    28-dec-2004 : origination.                        ( version 3.06 )
    !/    20-jul-2005 : adding output fields.               ( version 3.07 )
    !/    04-oct-2006 : add filter to array pointers.       ( version 3.10 )
    !/    28-mar-2007 : add partitioned data arrays.        ( version 3.11 )
    !/                  add additional undefined arrays.
    !/    22-feb-2008 ; modify mapth2 declaration.          ( version 3.14 )
    !/    31-oct-2010 : added initialization of cx,cy,dw    ( version 3.14 )
    !/    25-dec-2012 : memory reduction for outputs.       ( version 4.11 )
    !/    28-jul-2013 : bug fix initialization p2sms.       ( version 4.11 )
    !/    30-apr-2014 : memory reduction for group3.        ( version 5.00 )
    !/    10-dec-2014 : add checks for allocate status      ( version 5.04 )
    !/    22-mar-2021 : adds taua, wnmean, tauoc parameters ( version 7.13 )
    !/
    !  1. purpose :
    !
    !     initialize an individual data grid at the proper dimensions.
    !
    !  2. method :
    !
    !     allocate directly into the structure array. note that
    !     this cannot be done through the pointer alias!
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       imod    int.   i   model number to point to.
    !       ndse    int.   i   error output unit number.
    !       ndst    int.   i   test output unit number.
    !       d_only  l.o.   i   flag for initializing data arrays only.
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
    !      w3iogo    subr. w3iogomd grid output io routine.
    !      ww3_shel  prog.   n/a    wave model driver.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     - check on input parameters.
    !     - check on previous allocation.
    !
    !  7. remarks :
    !
    !     - w3seta needs to be called after allocation to point to
    !       proper allocated arrays.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/shrd, !/dist
    !              shared / distributed memory model
    !
    !     !/prn    propagation scheme selection.
    !
    !     !/s      enable subroutine tracing.
    !     !/t    enable test output
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants, only : lpdlib
    use w3gdatmd, only: ngrids, igrid, w3setg, nk, nx, ny, nsea,    &
         nseal, nspec, nth, e3df, p2msf, us3df,      &
         usspf, gtype, ungtype
    use w3odatmd, only: iaproc, naproc, ntproc, napfld,             &
         noswll, noextr, undef, flogrd, flogr2
    use w3idatmd, only: flcur, flwind, fltaua, flrhoa
    use w3servmd, only: extcde
    !
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)           :: imod, ndse, ndst
    logical, intent(in), optional :: d_only
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: jgrid, nxxx, nseal_tmp
    integer :: memunit
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input and module status
    !
    memunit = 30000+iaproc
    call print_memcheck(memunit, 'memcheck_____:'//' w3dima 0')
    if ( present(d_only) ) then
      fl_all = .not. d_only
    else
      fl_all = .true.
    end if
    !
    if ( ngrids .eq. -1 ) then
      write (ndse,1001)
      call extcde (1)
    end if
    !
    if ( imod.lt.1 .or. imod.gt.nadata ) then
      write (ndse,1002) imod, nadata
      call extcde (2)
    end if
    !
    if ( wadats(imod)%ainit ) then
      write (ndse,1003)
      call extcde (3)
    end if
    !
    !
    jgrid  = igrid
    if ( jgrid .ne. imod ) call w3setg ( imod, ndse, ndst )
    call print_memcheck(memunit, 'memcheck_____:'//' w3dima 1')
    !
    ! -------------------------------------------------------------------- /
    ! 2.  allocate arrays
    !     call w3seta to assure of pointes flcur, flwnd, and fltaua
    !
    call w3seta ( imod, ndse, ndst )
    !
    !ar: check this below more ...
    nxxx   = nsealm * naproc
    !
    !     output and input parameteres by output type
    !
    !  1) forcing fields (these arrays are always needed)
    !
    allocate ( wadats(imod)%dw(0:nsea) , stat=istat )
    check_alloc_status ( istat )
    wadats(imod)%dw(:)=0.
    !
    allocate ( wadats(imod)%cx(0:nsea) , wadats(imod)%cy(0:nsea) , &
         stat=istat )
    check_alloc_status ( istat )
    wadats(imod)%cx(:)=0.
    wadats(imod)%cy(:)=0.
    !
    allocate ( wadats(imod)%ua(0:nsea) , wadats(imod)%ud(0:nsea) , &
         wadats(imod)%u10(nsea)  , wadats(imod)%u10d(nsea) , &
         wadats(imod)%as(0:nsea) , stat=istat )
    check_alloc_status ( istat )
    !
    allocate ( wadats(imod)%taua(0:nsea) ,                         &
         wadats(imod)%tauadir(0:nsea), stat=istat )
    check_alloc_status ( istat )
    wadats(imod)%taua(:)   =0.
    wadats(imod)%tauadir(:)=0.
    call print_memcheck(memunit, 'memcheck_____:'//' w3dima 2')
    !
    !     water level wlv stored in w3wdatmd
    !     ice concentration ice stored in w3wdatmd
    !     ice floe sizes icef and icedmax stored in w3wdatmd
    !     iceberg damping berg stored in w3wdatmd
    !
    ! 2) standard mean wave parameters
    !    here, all short arrays are always allocated to reduce logical
    !    checks in all computations. the coresponding full size arrays
    !    are allocated in w3mpio only as needed to keep the memory
    !    footprint down.
    !
    if (nsealm .eq. 0) then
      nsealm=nsea
    end if
    allocate ( wadats(imod)%hs (nsealm), wadats(imod)%wlm (nsealm), &
         wadats(imod)%t02  (nsealm), wadats(imod)%t0m1(nsealm),     &
         wadats(imod)%t01  (nsealm), wadats(imod)%fp0 (nsealm),     &
         wadats(imod)%thm  (nsealm), wadats(imod)%ths (nsealm),     &
         wadats(imod)%thp0 (nsealm), wadats(imod)%hsig(nsealm),     &
         wadats(imod)%stmaxe (nsealm),                              &
         wadats(imod)%stmaxd(nsealm),                               &
         wadats(imod)%hmaxe(nsealm), wadats(imod)%hmaxd(nsealm),    &
         wadats(imod)%hcmaxe(nsealm),                               &
         wadats(imod)%hcmaxd(nsealm), wadats(imod)%qp(nsealm),      &
         wadats(imod)%wbt(nsealm),                                  &
         wadats(imod)%wnmean(nsealm),                               &
         stat=istat )
    check_alloc_status ( istat )
    !
    wadats(imod)%hs     = undef
    wadats(imod)%wlm    = undef
    wadats(imod)%t02    = undef
    wadats(imod)%t0m1   = undef
    wadats(imod)%t01    = undef
    wadats(imod)%fp0    = undef
    wadats(imod)%thm    = undef
    wadats(imod)%ths    = undef
    wadats(imod)%thp0   = undef
    wadats(imod)%hsig   = undef
    wadats(imod)%stmaxe = undef
    wadats(imod)%stmaxd = undef
    wadats(imod)%hmaxe  = undef
    wadats(imod)%hmaxd  = undef
    wadats(imod)%hcmaxe = undef
    wadats(imod)%hcmaxd = undef
    wadats(imod)%qp     = undef
    wadats(imod)%wbt    = undef
    wadats(imod)%wnmean = undef
    call print_memcheck(memunit, 'memcheck_____:'//' w3dima 3')
    !
    ! 3) frequency-dependent standard parameters
    !
    ! for the 3d arrays: the allocation is performed only if these arrays are allowed
    !                    by specific variables defined through the mod_def file
    !                    and read by w3iogr, which is called before w3dima.
    if (  e3df(1,1).gt.0 ) then
      allocate(wadats(imod)%ef(nsealm,e3df(2,1):e3df(3,1)),    &
           stat=istat )
      check_alloc_status ( istat )
    end if
    if (  e3df(1,2).gt.0 ) then
      allocate(wadats(imod)%th1m(nsealm,e3df(2,2):e3df(3,2)),  &
           stat=istat )
      check_alloc_status ( istat )
    end if
    if (  e3df(1,3).gt.0 ) then
      allocate(wadats(imod)%sth1m(nsealm,e3df(2,3):e3df(3,3)), &
           stat=istat )
      check_alloc_status ( istat )
    end if
    if (  e3df(1,4).gt.0 ) then
      allocate(wadats(imod)%th2m(nsealm,e3df(2,4):e3df(3,4)),  &
           stat=istat )
      check_alloc_status ( istat )
    end if
    if (  e3df(1,5).gt.0 ) then
      allocate(wadats(imod)%sth2m(nsealm,e3df(2,5):e3df(3,5)), &
           stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if (  e3df(1,1).gt.0 ) wadats(imod)%ef      = undef
    if (  e3df(1,2).gt.0 ) wadats(imod)%th1m    = undef
    if (  e3df(1,3).gt.0 ) wadats(imod)%sth1m   = undef
    if (  e3df(1,4).gt.0 ) wadats(imod)%th2m    = undef
    if (  e3df(1,5).gt.0 ) wadats(imod)%sth2m   = undef
    call print_memcheck(memunit, 'memcheck_____:'//' w3dima 4')
    !
    ! 4) spectral partitions parameters
    !
    allocate ( wadats(imod)%phs(nsealm,0:noswll),                   &
         wadats(imod)%ptp(nsealm,0:noswll),                   &
         wadats(imod)%plp(nsealm,0:noswll),                   &
         wadats(imod)%pdir(nsealm,0:noswll),                  &
         wadats(imod)%psi(nsealm,0:noswll),                   &
         wadats(imod)%pws(nsealm,0:noswll),                   &
         wadats(imod)%pwst(nsealm),                           &
         wadats(imod)%pnr(nsealm),                            &
         wadats(imod)%pthp0(nsealm,0:noswll),                 &
         wadats(imod)%pqp(nsealm,0:noswll),                   &
         wadats(imod)%ppe(nsealm,0:noswll),                   &
         wadats(imod)%pgw(nsealm,0:noswll),                   &
         wadats(imod)%psw(nsealm,0:noswll),                   &
         wadats(imod)%ptm1(nsealm,0:noswll),                  &
         wadats(imod)%pt1(nsealm,0:noswll),                   &
         wadats(imod)%pt2(nsealm,0:noswll),                   &
         wadats(imod)%pep(nsealm,0:noswll),                   &
         stat=istat )
    check_alloc_status ( istat )
    !
    wadats(imod)%phs    = undef
    wadats(imod)%ptp    = undef
    wadats(imod)%plp    = undef
    wadats(imod)%pdir   = undef
    wadats(imod)%psi    = undef
    wadats(imod)%pws    = undef
    wadats(imod)%pwst   = undef
    wadats(imod)%pnr    = undef
    wadats(imod)%pthp0  = undef
    wadats(imod)%pqp    = undef
    wadats(imod)%ppe    = undef
    wadats(imod)%pgw    = undef
    wadats(imod)%psw    = undef
    wadats(imod)%ptm1   = undef
    wadats(imod)%pt1    = undef
    wadats(imod)%pt2    = undef
    wadats(imod)%pep    = undef
    !
    ! 5) atmosphere-waves layer
    !
    !    friction velocity ust and ustdir in w3wdatmd
    !
    allocate ( wadats(imod)%charn   (nsealm),                       &
         wadats(imod)%tws     (nsealm),                       &
         wadats(imod)%cge     (nsealm),                       &
         wadats(imod)%phiaw   (nsealm),                       &
         wadats(imod)%tauwix  (nsealm),                       &
         wadats(imod)%tauwiy  (nsealm),                       &
         wadats(imod)%tauwnx  (nsealm),                       &
         wadats(imod)%tauwny  (nsealm),                       &
         wadats(imod)%whitecap(nsealm,4),                     &
         stat=istat )
    check_alloc_status ( istat )
    !
    wadats(imod)%charn    = undef
    wadats(imod)%tws      = undef
    wadats(imod)%cge      = undef
    wadats(imod)%phiaw    = undef
    wadats(imod)%tauwix   = undef
    wadats(imod)%tauwiy   = undef
    wadats(imod)%tauwnx   = undef
    wadats(imod)%tauwny   = undef
    wadats(imod)%whitecap = undef
    call print_memcheck(memunit, 'memcheck_____:'//' w3dima 5')
    !
    ! 6) wave-ocean layer
    !
    allocate ( wadats(imod)%sxx   (nsealm) ,                  &
         wadats(imod)%syy   (nsealm) ,                        &
         wadats(imod)%sxy   (nsealm) ,                        &
         wadats(imod)%tauox (nsealm) ,                        &
         wadats(imod)%tauoy (nsealm) ,                        &
         wadats(imod)%bhd   (nsealm) ,                        &
         wadats(imod)%phioc (nsealm) ,                        &
         wadats(imod)%tusx  (nsealm) ,                        &
         wadats(imod)%tusy  (nsealm) ,                        &
         wadats(imod)%ussx  (nsealm) ,                        &
         wadats(imod)%ussy  (nsealm) ,                        &
         wadats(imod)%tauocx(nsealm) ,                        &
         wadats(imod)%tauocy(nsealm) ,                        &
         wadats(imod)%prms  (nsealm) ,                        &
         wadats(imod)%tpms  (nsealm) ,                        &
         wadats(imod)%phice (nsealm) ,                        &
         wadats(imod)%tauice(nsealm,2),                       &
         wadats(imod)%usshx(nsealm),                          &
         wadats(imod)%usshy(nsealm),                          &
         stat=istat )
    check_alloc_status ( istat )
    !
    ! for the 3d arrays: the allocation is performed only if these arrays are allowed
    !                    by specific variables defined through the mod_def file
    !                    and read by w3iogr, which is called before w3dima.
    if (  p2msf(1).gt.0 ) then
      allocate(wadats(imod)%p2sms(nsealm,p2msf(2):p2msf(3)), stat=istat )
      check_alloc_status ( istat )
    end if
    if (  us3df(1).gt.0 ) then ! maybe use us3df(2:3)
      allocate(wadats(imod)%us3d(nsealm,nk*2), stat=istat )
      check_alloc_status ( istat )
    end if
    if ( usspf(1).gt.0 ) then
      allocate(wadats(imod)%ussp(nsealm,nk*2), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    wadats(imod)%sxx    = undef
    wadats(imod)%syy    = undef
    wadats(imod)%sxy    = undef
    wadats(imod)%tauox  = undef
    wadats(imod)%tauoy  = undef
    wadats(imod)%bhd    = undef
    wadats(imod)%phioc  = undef
    wadats(imod)%tusx   = undef
    wadats(imod)%tusy   = undef
    wadats(imod)%ussx   = undef
    wadats(imod)%ussy   = undef
    wadats(imod)%tauocx = undef
    wadats(imod)%tauocy = undef
    wadats(imod)%prms   = undef
    wadats(imod)%tpms   = undef
    wadats(imod)%phice  = undef
    wadats(imod)%tauice = undef
    wadats(imod)%usshx  = undef
    wadats(imod)%usshy  = undef
    if (  p2msf(1).gt.0 ) wadats(imod)%p2sms  = undef
    if (  us3df(1).gt.0 ) wadats(imod)%us3d   = undef
    if (  usspf(1).gt.0 ) wadats(imod)%ussp   = undef
    call print_memcheck(memunit, 'memcheck_____:'//' w3dima 6')
    !
    ! 7) wave-bottom layer
    !
    allocate ( wadats(imod)%aba(nsealm) , wadats(imod)%abd(nsealm) , &
         wadats(imod)%uba(nsealm) , wadats(imod)%ubd(nsealm) ,       &
         wadats(imod)%bedforms(nsealm,3),                            &
         wadats(imod)%phibbl  (nsealm)  ,                            &
         wadats(imod)%taubbl  (nsealm,2), stat=istat           )
    check_alloc_status ( istat )
    !
    wadats(imod)%aba    = undef
    wadats(imod)%abd    = undef
    wadats(imod)%uba    = undef
    wadats(imod)%ubd    = undef
    wadats(imod)%bedforms = undef
    wadats(imod)%phibbl = undef
    wadats(imod)%taubbl = undef
    call print_memcheck(memunit, 'memcheck_____:'//' w3dima 7')
    !
    ! 8) spectrum parameters
    !
    allocate ( wadats(imod)%mssx(nsealm), wadats(imod)%mssy(nsealm), &
         wadats(imod)%mscx(nsealm), wadats(imod)%mscy(nsealm), &
         wadats(imod)%mssd(nsealm), wadats(imod)%mscd(nsealm), &
         stat=istat )
    check_alloc_status ( istat )
    !
    wadats(imod)%mssx   = undef
    wadats(imod)%mssy   = undef
    wadats(imod)%mssd   = undef
    wadats(imod)%mscx   = undef
    wadats(imod)%mscy   = undef
    wadats(imod)%mscd   = undef
    call print_memcheck(memunit, 'memcheck_____:'//' w3dima 8')
    !
    ! 9) numerical diagnostics
    !
    !
    allocate ( wadats(imod)%dtdyn   (nsealm) , &
         wadats(imod)%fcut    (nsealm) ,       &
         wadats(imod)%cflxymax(nsealm) ,       &
         wadats(imod)%cflthmax(nsealm) ,       &
         wadats(imod)%cflkmax (nsealm) , stat=istat            )
    check_alloc_status ( istat )
    !
    wadats(imod)%dtdyn    = undef
    wadats(imod)%fcut     = undef
    wadats(imod)%cflxymax = undef
    wadats(imod)%cflthmax = undef
    wadats(imod)%cflkmax  = undef
    call print_memcheck(memunit, 'memcheck_____:'//' w3dima 9')
    !
    ! 10) user defined
    !
    allocate ( wadats(imod)%usero(nsealm,noextr), stat=istat )
    check_alloc_status ( istat )
    !
    wadats(imod)%usero  = undef
    !
    allocate ( wadats(imod)%wn(0:nk+1,0:nsea), stat=istat )
    check_alloc_status ( istat )
    call print_memcheck(memunit, 'memcheck_____:'//' w3dima 10')
    !
    if ( fl_all ) then
      !
      allocate ( wadats(imod)%cg(0:nk+1,0:nsea), stat=istat )
      check_alloc_status ( istat )
      !
      if ( flcur  ) then
        allocate ( wadats(imod)%ca0(nsea) , &
             wadats(imod)%cai(nsea) ,       &
             wadats(imod)%cd0(nsea) ,       &
             wadats(imod)%cdi(nsea) ,       &
             stat=istat )
        check_alloc_status ( istat )
      end if
      !
      if ( flwind ) then
        allocate ( wadats(imod)%ua0(nsea) , &
             wadats(imod)%uai(nsea) ,       &
             wadats(imod)%ud0(nsea) ,       &
             wadats(imod)%udi(nsea) ,       &
             wadats(imod)%as0(nsea) ,       &
             wadats(imod)%asi(nsea) ,       &
             stat=istat )
        check_alloc_status ( istat )
      end if
      !
      if ( fltaua  ) then
        allocate ( wadats(imod)%ma0(nsea) , &
             wadats(imod)%mai(nsea) ,       &
             wadats(imod)%md0(nsea) ,       &
             wadats(imod)%mdi(nsea) ,       &
             stat=istat )
        check_alloc_status ( istat )
      end if
      !
      if ( flrhoa  ) then
        allocate ( wadats(imod)%ra0(nsea) , &
             wadats(imod)%rai(nsea) ,       &
             stat=istat )
        check_alloc_status ( istat )
      end if
      !
      allocate ( wadats(imod)%atrnx(ny*nx,-1:1) ,                 &
           wadats(imod)%atrny(ny*nx,-1:1) , stat=istat      )
      check_alloc_status ( istat )
      !
      if (.not. lpdlib) then
        allocate ( wadats(imod)%dddx(ny,nx)  ,    &
             wadats(imod)%dddy(ny,nx)  ,          &
             wadats(imod)%dcdx(0:nk+1,ny,nx)  ,   &
             wadats(imod)%dcdy(0:nk+1,ny,nx)  ,   &
             wadats(imod)%dcxdx(ny,nx) ,          &
             wadats(imod)%dcydx(ny,nx) ,          &
             wadats(imod)%dcxdy(ny,nx) ,          &
             wadats(imod)%dcydy(ny,nx) , stat=istat           )
      else
        allocate ( wadats(imod)%dddx(1,nseal)  ,  &
             wadats(imod)%dddy(1,nseal)  ,        &
             wadats(imod)%dcdx(0:nk+1,1,nseal)  , &
             wadats(imod)%dcdy(0:nk+1,1,nseal)  , &
             wadats(imod)%dcxdx(1,nseal) ,        &
             wadats(imod)%dcydx(1,nseal) ,        &
             wadats(imod)%dcxdy(1,nseal) ,        &
             wadats(imod)%dcydy(1,nseal) ,        &
             stat=istat           )
      endif
      check_alloc_status ( istat )
      wadats(imod)%dddx = 0.
      wadats(imod)%dddy = 0.
      wadats(imod)%dcdx = 0.
      wadats(imod)%dcdy = 0.
      wadats(imod)%dcxdx = 0.
      wadats(imod)%dcydx = 0.
      wadats(imod)%dcxdy = 0.
      wadats(imod)%dcydy = 0.
      !
      !
      allocate ( wadats(imod)%alpha(nk,nseal) , stat=istat )
      check_alloc_status ( istat )
      !
      !
      !
      if (gtype .eq. ungtype) then
        allocate( wadats(imod)%iter(nk,nth), stat=istat )
        check_alloc_status ( istat )
      end if
      !
      !
      allocate ( wadats(imod)%iappro(nspec) ,          &
           wadats(imod)%sppnt(nth,nk,4), stat=istat         )
      check_alloc_status ( istat )
      !
    end if
    !
    wadats(imod)%ainit  = .true.
    call print_memcheck(memunit, 'memcheck_____:'//' w3dima 11')
    !
    !
    ! -------------------------------------------------------------------- /
    ! 3.  point to allocated arrays
    !
    call w3seta ( imod, ndse, ndst )
    call print_memcheck(memunit, 'memcheck_____:'//' w3dima 12')
    !
    !
    ! -------------------------------------------------------------------- /
    ! 4.  update counters in grid
    !
    !
    ! -------------------------------------------------------------------- /
    ! 5.  restore previous grid setting if necessary
    !
    if ( jgrid .ne. imod ) call w3setg ( jgrid, ndse, ndst )
    call print_memcheck(memunit, 'memcheck_____:'//' w3dima end')
    !
    return
    !
    ! formats
    !
1001 format (/' *** error w3dima : grids not initialized *** '/ &
         '                    run w3nmod first '/)
1002 format (/' *** error w3dima : illegal model number *** '/  &
         '                    imod   = ',i10/                   &
         '                    nadata = ',i10/)
1003 format (/' *** error w3dima : array(s) already allocated *** ')
    !
    !/
    !/ end of w3dima ----------------------------------------------------- /
    !/
  end subroutine w3dima
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief version of w3dimx for extended ouput arrays only.
  !>
  !> @param[in] imod     model number to point to.
  !> @param[in] ndse     error output unit number.
  !> @param[in] ndst     test output unit number.
  !> @param[in] outflags
  !>
  !> @author h. l. tolman
  !> @date   22-mar-2021
  !>
  subroutine w3xdma  ( imod, ndse, ndst, outflags )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 !
    !/                  +-----------------------------------+
    !/
    !/    26-dec-2012 : origination.                        ( version 3.06 )
    !/    10-dec-2014 : add checks for allocate status      ( version 5.04 )
    !/    22-mar-2021 : adds wnmean, tauoc parameters       ( version 7.13 )
    !/
    !  1. purpose :
    !
    !     version of w3dimx for extended ouput arrays only.
    !
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: ngrids, igrid, w3setg, nk, nx, ny, nsea,    &
         nseal, nspec, nth, e3df, p2msf, us3df,      &
         usspf, gtype, ungtype
    use w3odatmd, only: iaproc, naproc, ntproc, napfld,             &
         noswll, noextr, undef, flogrd, flogr2,      &
         nogrp, ngrpp
    use w3servmd, only: extcde
    !
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)           :: imod, ndse, ndst
    logical, intent(in)           :: outflags(nogrp,ngrpp)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: jgrid, nxxx, i
    integer :: memunit
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input and module status
    !
    memunit = 30000+iaproc
    if ( ngrids .eq. -1 ) then
      write (ndse,1001)
      call extcde (1)
    end if
    !
    if ( imod.lt.1 .or. imod.gt.nadata ) then
      write (ndse,1002) imod, nadata
      call extcde (2)
    end if
    !
    if ( wadats(imod)%ainit2 ) then
      write (ndse,1003)
      call extcde (3)
    end if
    !
    !
    jgrid  = igrid
    if ( jgrid .ne. imod ) call w3setg ( imod, ndse, ndst )
    !
    ! -------------------------------------------------------------------- /
    ! 2.  allocate arrays
    !
    nxxx   = nsealm * naproc
    !
    if ( outflags( 2, 1) ) then
      allocate ( wadats(imod)%xhs(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xhs(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 2, 2) ) then
      allocate ( wadats(imod)%xwlm(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xwlm(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 2, 3) ) then
      allocate ( wadats(imod)%xt02(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xt02(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 2, 4) ) then
      allocate ( wadats(imod)%xt0m1(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xt0m1(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 2, 5) ) then
      allocate ( wadats(imod)%xt01 (nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xt01 (1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 2, 6) .or. outflags( 2,18) ) then
      ! tp output shares fp0 internal field with fp
      allocate ( wadats(imod)%xfp0(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xfp0(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 2, 7) ) then
      allocate ( wadats(imod)%xthm(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xthm(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 2, 8) ) then
      allocate ( wadats(imod)%xths(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xths(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 2, 9) ) then
      allocate ( wadats(imod)%xthp0(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xthp0(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 2, 10) ) then
      allocate ( wadats(imod)%xhsig(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xhsig(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 2, 11) ) then
      allocate ( wadats(imod)%xstmaxe(nxxx) )
    else
      allocate ( wadats(imod)%xstmaxe(1) )
    end if
    !
    if ( outflags( 2, 12) ) then
      allocate ( wadats(imod)%xstmaxd(nxxx) )
    else
      allocate ( wadats(imod)%xstmaxd(1) )
    end if
    !
    if ( outflags( 2, 13) ) then
      allocate ( wadats(imod)%xhmaxe(nxxx) )
    else
      allocate ( wadats(imod)%xhmaxe(1) )
    end if
    !
    if ( outflags( 2, 14) ) then
      allocate ( wadats(imod)%xhcmaxe(nxxx) )
    else
      allocate ( wadats(imod)%xhcmaxe(1) )
    end if
    !
    !
    if ( outflags( 2, 15) ) then
      allocate ( wadats(imod)%xhmaxd(nxxx) )
    else
      allocate ( wadats(imod)%xhmaxd(1) )
    end if
    !
    if ( outflags( 2, 16) ) then
      allocate ( wadats(imod)%xhcmaxd(nxxx) )
    else
      allocate ( wadats(imod)%xhcmaxd(1) )
    end if
    !
    if ( outflags( 2, 17) ) then
      allocate ( wadats(imod)%xwbt (nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xwbt (1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 2, 19) ) then
      allocate ( wadats(imod)%xwnmean(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xwnmean(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    wadats(imod)%xhs    = undef
    wadats(imod)%xwlm   = undef
    wadats(imod)%xt02   = undef
    wadats(imod)%xt0m1  = undef
    wadats(imod)%xt01   = undef
    wadats(imod)%xfp0   = undef
    wadats(imod)%xthm   = undef
    wadats(imod)%xths   = undef
    wadats(imod)%xthp0  = undef
    wadats(imod)%xhsig  = undef
    wadats(imod)%xstmaxe= undef
    wadats(imod)%xstmaxd= undef
    wadats(imod)%xhmaxe = undef
    wadats(imod)%xhmaxd = undef
    wadats(imod)%xhcmaxe= undef
    wadats(imod)%xhcmaxd= undef
    wadats(imod)%xwbt   = undef
    wadats(imod)%xwnmean= undef
    !
    if ( outflags( 3, 1) ) then
      allocate ( wadats(imod)%xef(nxxx,e3df(2,1):e3df(3,1)), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xef(1,1), stat=istat )
      check_alloc_status ( istat )
    end if
    if ( outflags( 3, 2) ) then
      allocate ( wadats(imod)%xth1m(nxxx,e3df(2,2):e3df(3,2)), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xth1m(1,1), stat=istat )
      check_alloc_status ( istat )
    end if
    if ( outflags( 3, 3) ) then
      allocate ( wadats(imod)%xsth1m(nxxx,e3df(2,3):e3df(3,3)), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xsth1m(1,1), stat=istat )
      check_alloc_status ( istat )
    end if
    if ( outflags( 3, 4) ) then
      allocate ( wadats(imod)%xth2m(nxxx,e3df(2,4):e3df(3,4)), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xth2m(1,1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 3, 5) ) then
      allocate ( wadats(imod)%xsth2m(nxxx,e3df(2,5):e3df(3,5)), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xsth2m(1,1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    wadats(imod)%xef     = undef
    wadats(imod)%xth1m   = undef
    wadats(imod)%xsth1m  = undef
    wadats(imod)%xth2m   = undef
    wadats(imod)%xsth2m  = undef
    !
    if ( outflags( 4, 1) ) then
      allocate ( wadats(imod)%xphs(nxxx,0:noswll), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xphs(1,1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 4, 2) ) then
      allocate ( wadats(imod)%xptp(nxxx,0:noswll), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xptp(1,1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 4, 3) ) then
      allocate ( wadats(imod)%xplp(nxxx,0:noswll), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xplp(1,1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 4, 4) ) then
      allocate ( wadats(imod)%xpdir(nxxx,0:noswll), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xpdir(1,1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 4, 5) ) then
      allocate ( wadats(imod)%xpsi(nxxx,0:noswll), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xpsi(1,1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 4, 6) ) then
      allocate ( wadats(imod)%xpws(nxxx,0:noswll), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xpws(1,1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 4, 7) ) then
      allocate ( wadats(imod)%xpthp0(nxxx,0:noswll), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xpthp0(1,1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 4, 8) ) then
      allocate ( wadats(imod)%xpqp(nxxx,0:noswll), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xpqp(1,1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 4, 9) ) then
      allocate ( wadats(imod)%xppe(nxxx,0:noswll), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xppe(1,1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 4,10) ) then
      allocate ( wadats(imod)%xpgw(nxxx,0:noswll), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xpgw(1,1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 4,11) ) then
      allocate ( wadats(imod)%xpsw(nxxx,0:noswll), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xpsw(1,1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 4,12) ) then
      allocate ( wadats(imod)%xptm1(nxxx,0:noswll), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xptm1(1,1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 4,13) ) then
      allocate ( wadats(imod)%xpt1(nxxx,0:noswll), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xpt1(1,1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 4,14) ) then
      allocate ( wadats(imod)%xpt2(nxxx,0:noswll), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xpt2(1,1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 4,15) ) then
      allocate ( wadats(imod)%xpep(nxxx,0:noswll), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xpep(1,1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 4,16) ) then
      allocate ( wadats(imod)%xpwst(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xpwst(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 4,17) ) then
      allocate ( wadats(imod)%xpnr(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xpnr(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    wadats(imod)%xphs   = undef
    wadats(imod)%xptp   = undef
    wadats(imod)%xplp   = undef
    wadats(imod)%xpdir  = undef
    wadats(imod)%xpsi   = undef
    wadats(imod)%xpws   = undef
    wadats(imod)%xpwst  = undef
    wadats(imod)%xpnr   = undef
    wadats(imod)%xpthp0 = undef
    wadats(imod)%xpqp   = undef
    wadats(imod)%xppe   = undef
    wadats(imod)%xpgw   = undef
    wadats(imod)%xpsw   = undef
    wadats(imod)%xptm1  = undef
    wadats(imod)%xpt1   = undef
    wadats(imod)%xpt2   = undef
    wadats(imod)%xpep   = undef
    !
    if ( outflags( 5, 2) ) then
      allocate ( wadats(imod)%xcharn(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xcharn(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 5, 3) ) then
      allocate ( wadats(imod)%xcge(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xcge(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 5, 4) ) then
      allocate ( wadats(imod)%xphiaw(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xphiaw(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 5, 5) ) then
      allocate ( wadats(imod)%xtauwix(nxxx), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xtauwiy(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xtauwix(1), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xtauwiy(1) )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 5, 6) ) then
      allocate ( wadats(imod)%xtauwnx(nxxx), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xtauwny(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xtauwnx(1), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xtauwny(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 5, 7) .or. outflags( 5, 8) .or.           &
         outflags( 5, 9) .or. outflags( 5,10)) then
      allocate ( wadats(imod)%xwhitecap(nxxx,4), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xwhitecap(1,4), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 5, 11) ) then
      allocate ( wadats(imod)%xtws(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xtws(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    wadats(imod)%xcharn    = undef
    wadats(imod)%xtws      = undef
    wadats(imod)%xcge      = undef
    wadats(imod)%xphiaw    = undef
    wadats(imod)%xtauwix   = undef
    wadats(imod)%xtauwiy   = undef
    wadats(imod)%xtauwnx   = undef
    wadats(imod)%xtauwny   = undef
    wadats(imod)%xwhitecap = undef
    !
    if ( outflags( 6, 1) ) then
      allocate ( wadats(imod)%xsxx(nxxx), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xsyy(nxxx), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xsxy(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xsxx(1), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xsyy(1), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xsxy(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 6, 2) ) then
      allocate ( wadats(imod)%xtauox(nxxx), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xtauoy(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xtauox(1), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xtauoy(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 6, 3) ) then
      allocate ( wadats(imod)%xbhd(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xbhd(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 6, 4) ) then
      allocate ( wadats(imod)%xphioc(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xphioc(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 6, 5) ) then
      allocate ( wadats(imod)%xtusx(nxxx), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xtusy(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xtusx(1), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xtusy(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 6, 6) ) then
      allocate ( wadats(imod)%xussx(nxxx), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xussy(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xussx(1), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xussy(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 6, 7) ) then
      allocate ( wadats(imod)%xprms(nxxx), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xtpms(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xprms(1), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xtpms(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 6, 8) ) then
      allocate ( wadats(imod)%xus3d(nxxx,2*nk), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xus3d(1,1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 6, 9) ) then
      allocate ( wadats(imod)%xp2sms(nxxx,p2msf(2):p2msf(3)), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xp2sms(1,1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 6,10) ) then
      allocate ( wadats(imod)%xtauice(nxxx,2), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xtauice(1,2), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 6,11) ) then
      allocate ( wadats(imod)%xphice(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xphice(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 6, 12) ) then
      allocate ( wadats(imod)%xussp(nxxx,2*nk), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xussp(1,1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 6, 13) ) then
      allocate ( wadats(imod)%xtauocx(nxxx), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xtauocy(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xtauocx(1), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xtauocy(1), stat=istat )
      check_alloc_status ( istat )
    end if
    if ( outflags( 6, 14) ) then
      allocate ( wadats(imod)%xusshx(nxxx), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xusshy(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xusshx(1), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xusshy(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    wadats(imod)%xsxx    = undef
    wadats(imod)%xsyy    = undef
    wadats(imod)%xsxy    = undef
    wadats(imod)%xtauox  = undef
    wadats(imod)%xtauoy  = undef
    wadats(imod)%xbhd    = undef
    wadats(imod)%xphioc  = undef
    wadats(imod)%xtusx   = undef
    wadats(imod)%xtusy   = undef
    wadats(imod)%xussx   = undef
    wadats(imod)%xussy   = undef
    wadats(imod)%xprms   = undef
    wadats(imod)%xtpms   = undef
    wadats(imod)%xus3d   = undef
    wadats(imod)%xp2sms  = undef
    wadats(imod)%xphice  = undef
    wadats(imod)%xtauice = undef
    wadats(imod)%xussp   = undef
    wadats(imod)%xtauocx = undef
    wadats(imod)%xtauocy = undef
    wadats(imod)%xusshx   = undef
    wadats(imod)%xusshy   = undef
    !
    if ( outflags( 7, 1) ) then
      allocate ( wadats(imod)%xaba(nxxx), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xabd(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xaba(1), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xabd(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 7, 2) ) then
      allocate ( wadats(imod)%xuba(nxxx), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xubd(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xuba(1), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xubd(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 7, 3) ) then
      allocate ( wadats(imod)%xbedforms(nxxx,3), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xbedforms(1,3), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 7, 4) ) then
      allocate ( wadats(imod)%xphibbl(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xphibbl(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 7, 5) ) then
      allocate ( wadats(imod)%xtaubbl(nxxx,2), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xtaubbl(1,2), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    wadats(imod)%xaba    = undef
    wadats(imod)%xabd    = undef
    wadats(imod)%xuba    = undef
    wadats(imod)%xubd    = undef
    wadats(imod)%xbedforms = undef
    wadats(imod)%xphibbl = undef
    wadats(imod)%xtaubbl = undef
    !
    if ( outflags( 8, 1) ) then
      allocate ( wadats(imod)%xmssx(nxxx), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xmssy(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xmssx(1), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xmssy(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 8, 2) ) then
      allocate ( wadats(imod)%xmscx(nxxx), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xmscy(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xmscx(1), stat=istat )
      check_alloc_status ( istat )
      allocate ( wadats(imod)%xmscy(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 8, 3) ) then
      allocate ( wadats(imod)%xmssd(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xmssd(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 8, 4) ) then
      allocate ( wadats(imod)%xmscd(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xmscd(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 8,  5) ) then
      allocate ( wadats(imod)%xqp(nxxx) )
    else
      allocate ( wadats(imod)%xqp(1) )
    end if
    !
    wadats(imod)%xmssx   = undef
    wadats(imod)%xmssy   = undef
    wadats(imod)%xmssd   = undef
    wadats(imod)%xmscx   = undef
    wadats(imod)%xmscy   = undef
    wadats(imod)%xmscd   = undef
    wadats(imod)%xqp(1)  = undef
    !
    if ( outflags( 9, 1) ) then
      allocate ( wadats(imod)%xdtdyn(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xdtdyn(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 9, 2) ) then
      allocate ( wadats(imod)%xfcut(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xfcut(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 9, 3) ) then
      allocate ( wadats(imod)%xcflxymax(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xcflxymax(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 9, 4) ) then
      allocate ( wadats(imod)%xcflthmax(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xcflthmax(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( outflags( 9, 5) ) then
      allocate ( wadats(imod)%xcflkmax(nxxx), stat=istat )
      check_alloc_status ( istat )
    else
      allocate ( wadats(imod)%xcflkmax(1), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    wadats(imod)%xdtdyn    = undef
    wadats(imod)%xfcut     = undef
    wadats(imod)%xcflxymax = undef
    wadats(imod)%xcflthmax = undef
    wadats(imod)%xcflkmax  = undef
    !
    do i=1, noextr
      if ( outflags(10, i) ) then
        allocate ( wadats(imod)%xusero(nxxx,i), stat=istat )
        check_alloc_status ( istat )
      else
        allocate ( wadats(imod)%xusero(1,i), stat=istat )
        check_alloc_status ( istat )
      end if
    end do
    !
    wadats(imod)%xusero  = undef
    !
    wadats(imod)%ainit2 = .true.
    !
    !
    ! -------------------------------------------------------------------- /
    ! 5.  restore previous grid setting if necessary
    !
    if ( jgrid .ne. imod ) call w3setg ( jgrid, ndse, ndst )
    call print_memcheck(memunit, 'memcheck_____:'//' w3xdma')
    !
    return
    !
    ! formats
    !
1001 format (/' *** error w3xdma : grids not initialized *** '/ &
         '                    run w3nmod first '/)
1002 format (/' *** error w3xdma : illegal model number *** '/  &
         '                    imod   = ',i10/                   &
         '                    nadata = ',i10/)
1003 format (/' *** error w3xdma : array(s) already allocated *** ')
    !
    !/
    !/ end of w3xdma ----------------------------------------------------- /
    !/
  end subroutine w3xdma
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief initialize an individual data grid at the proper dimensions (dia).
  !>
  !> @details allocate directly into the structure array. note that
  !>  this cannot be done through the pointer alias!
  !>
  !> @param[in] imod model number to point to.
  !> @param[in] ndse error output unit number.
  !> @param[in] ndst test output unit number.
  !> @param[in] nsp  array dimensions.
  !> @param[in] nspx array dimensions.
  !>
  !> @author h. l. tolman
  !> @date   10-dec-2014
  !>
  subroutine w3dmnl  ( imod, ndse, ndst, nsp, nspx )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         10-dec-2014 |
    !/                  +-----------------------------------+
    !/
    !/    24-dec-2004 : origination.                        ( version 3.06 )
    !/    04-oct-2006 : add filter to array pointers.       ( version 3.10 )
    !/    10-dec-2014 : add checks for allocate status      ( version 5.04 )
    !/
    !  1. purpose :
    !
    !     initialize an individual data grid at the proper dimensions (dia).
    !
    !  2. method :
    !
    !     allocate directly into the structure array. note that
    !     this cannot be done through the pointer alias!
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       imod    int.   i   model number to point to.
    !       ndse    int.   i   error output unit number.
    !       ndst    int.   i   test output unit number.
    !       nsp(x)  int.   i   array dimensions.
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
    !      insnl1    subr. w3snl1md traditional dia approach to snl.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     - check on input parameters.
    !     - check on previous allocation.
    !
    !  7. remarks :
    !
    !     - w3seta needs to be called after allocation to point to
    !       proper allocated arrays.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s      enable subroutine tracing.
    !     !/t    enable test output
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: ngrids, igrid, nk, nx, ny, nsea, nseal,     &
         nspec, nth, gtype, ungtype
    use w3odatmd, only: naproc
    use w3servmd, only: extcde
    !
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)           :: imod, ndse, ndst, nsp, nspx
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
    if ( imod.lt.1 .or. imod.gt.nadata ) then
      write (ndse,1002) imod, nadata
      call extcde (2)
    end if
    !
    !
    !
    ! -------------------------------------------------------------------- /
    ! 2.  allocate arrays
    !
    !
    !
    ! -------------------------------------------------------------------- /
    ! 3.  point to allocated arrays
    !
    call w3seta ( imod, ndse, ndst )
    !
    !
    ! -------------------------------------------------------------------- /
    ! 4.  update counters in grid
    !
    !
    !
    return
    !
    ! formats
    !
1001 format (/' *** error w3dmnl : grids not initialized *** '/ &
         '                    run w3nmod first '/)
1002 format (/' *** error w3dmnl : illegal model number *** '/  &
         '                    imod   = ',i10/                   &
         '                    nadata = ',i10/)
    !
    !/
    !/ end of w3dmnl ----------------------------------------------------- /
    !/
  end subroutine w3dmnl
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief select one of the wavewatch iii grids / models.
  !>
  !> @details point pointers to the proper variables in the proper element
  !>  of the grids array.
  !>
  !> @param[in] imod model number to point to.
  !> @param[in] ndse error output unit number.
  !> @param[in] ndst test output unit number.
  !>
  !> @author h. l. tolman
  !> @date   22-mar-2021
  !>
  subroutine w3seta ( imod, ndse, ndst )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    28-dec-2004 : origination.                        ( version 3.06 )
    !/    04-may-2005 : adding mpi_comm_wave.               ( version 3.07 )
    !/    20-jul-2005 : adding output fields.               ( version 3.07 )
    !/    09-nov-2005 : removing soft boundary option.      ( version 3.08 )
    !/    13-jun-2006 : splitting store in g/sstore.        ( version 3.09 )
    !/    04-oct-2006 : add filter to array pointers.       ( version 3.10 )
    !/    28_mar-2007 : add partitioned data arrays.        ( version 3.11 )
    !/                  add aditional undefined arrays.
    !/    22-mar-2021 : adds taua, wnmean, tauoc parameters ( version 7.13 )
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
    !     see module documentation below.
    !
    !  5. called by :
    !
    !     many subroutines in the wavewatch system.
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
    !     !/mpi  paralllel model environment.
    !
    !     !/prn    propagation scheme selection.
    !
    !     !/s    enable subroutine tracing.
    !     !/t    enable test output
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    use w3idatmd, only: inputs
    use w3gdatmd, only: e3df, p2msf, us3df, usspf, gtype, ungtype
    !
    use w3servmd, only: extcde
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
    if ( nadata .eq. -1 ) then
      write (ndse,1001)
      call extcde (1)
    end if
    !
    if ( imod.lt.1 .or. imod.gt.nadata ) then
      write (ndse,1002) imod, nadata
      call extcde (2)
    end if
    !
    !
    ! -------------------------------------------------------------------- /
    ! 2.  set model numbers
    !
    iadata = imod
    !
    ! -------------------------------------------------------------------- /
    ! 3.  set pointers
    !
    itime  => wadats(imod)%itime
    ipass  => wadats(imod)%ipass
    idlast => wadats(imod)%idlast
    nsealm => wadats(imod)%nsealm
    flcold => wadats(imod)%flcold
    fliwnd => wadats(imod)%fliwnd
    ainit  => wadats(imod)%ainit
    ainit2 => wadats(imod)%ainit2
    fl_all => wadats(imod)%fl_all
    !
    !
    !
    !
    mpi_comm_wave => wadats(imod)%mpi_comm_wave
    mpi_comm_wcmp => wadats(imod)%mpi_comm_wcmp
    ww3_field_vec => wadats(imod)%ww3_field_vec
    ww3_spec_vec => wadats(imod)%ww3_spec_vec
    nrqsg1 => wadats(imod)%nrqsg1
    nrqsg2 => wadats(imod)%nrqsg2
    ibfloc => wadats(imod)%ibfloc
    isploc => wadats(imod)%isploc
    nsploc => wadats(imod)%nsploc
    bstat  => wadats(imod)%bstat
    bispl  => wadats(imod)%bispl
    !
    if ( ainit ) then
      !
      dw     => wadats(imod)%dw
      ua     => wadats(imod)%ua
      ud     => wadats(imod)%ud
      u10    => wadats(imod)%u10
      u10d   => wadats(imod)%u10d
      as     => wadats(imod)%as
      cx     => wadats(imod)%cx
      cy     => wadats(imod)%cy
      taua   => wadats(imod)%taua
      tauadir=> wadats(imod)%tauadir
      !
      hs     => wadats(imod)%hs
      wlm    => wadats(imod)%wlm
      t02    => wadats(imod)%t02
      t0m1   => wadats(imod)%t0m1
      t01    => wadats(imod)%t01
      fp0    => wadats(imod)%fp0
      thm    => wadats(imod)%thm
      ths    => wadats(imod)%ths
      thp0   => wadats(imod)%thp0
      hsig   => wadats(imod)%hsig
      stmaxe => wadats(imod)%stmaxe
      stmaxd => wadats(imod)%stmaxd
      hmaxe  => wadats(imod)%hmaxe
      hmaxd  => wadats(imod)%hmaxd
      hcmaxe => wadats(imod)%hcmaxe
      hcmaxd => wadats(imod)%hcmaxd
      qp     => wadats(imod)%qp
      wbt    => wadats(imod)%wbt
      wnmean => wadats(imod)%wnmean
      !
      ef     => wadats(imod)%ef
      th1m   => wadats(imod)%th1m
      sth1m  => wadats(imod)%sth1m
      th2m   => wadats(imod)%th2m
      sth2m  => wadats(imod)%sth2m
      !
      phs    => wadats(imod)%phs
      ptp    => wadats(imod)%ptp
      plp    => wadats(imod)%plp
      pdir   => wadats(imod)%pdir
      psi    => wadats(imod)%psi
      pws    => wadats(imod)%pws
      pwst   => wadats(imod)%pwst
      pnr    => wadats(imod)%pnr
      pthp0  => wadats(imod)%pthp0
      pqp    => wadats(imod)%pqp
      ppe    => wadats(imod)%ppe
      pgw    => wadats(imod)%pgw
      psw    => wadats(imod)%psw
      ptm1   => wadats(imod)%ptm1
      pt1    => wadats(imod)%pt1
      pt2    => wadats(imod)%pt2
      pep    => wadats(imod)%pep
      !
      charn    => wadats(imod)%charn
      tws      => wadats(imod)%tws
      cge      => wadats(imod)%cge
      phiaw    => wadats(imod)%phiaw
      tauwix   => wadats(imod)%tauwix
      tauwiy   => wadats(imod)%tauwiy
      tauwnx   => wadats(imod)%tauwnx
      tauwny   => wadats(imod)%tauwny
      whitecap => wadats(imod)%whitecap
      !
      sxx    => wadats(imod)%sxx
      syy    => wadats(imod)%syy
      sxy    => wadats(imod)%sxy
      tauox  => wadats(imod)%tauox
      tauoy  => wadats(imod)%tauoy
      bhd    => wadats(imod)%bhd
      phioc  => wadats(imod)%phioc
      tusx   => wadats(imod)%tusx
      tusy   => wadats(imod)%tusy
      ussx   => wadats(imod)%ussx
      ussy   => wadats(imod)%ussy
      prms   => wadats(imod)%prms
      tpms   => wadats(imod)%tpms
      p2sms  => wadats(imod)%p2sms
      us3d   => wadats(imod)%us3d
      phice  => wadats(imod)%phice
      tauice => wadats(imod)%tauice
      ussp   => wadats(imod)%ussp
      tauocx => wadats(imod)%tauocx
      tauocy => wadats(imod)%tauocy
      !
      aba    => wadats(imod)%aba
      abd    => wadats(imod)%abd
      uba    => wadats(imod)%uba
      ubd    => wadats(imod)%ubd
      bedforms=> wadats(imod)%bedforms
      phibbl => wadats(imod)%phibbl
      taubbl => wadats(imod)%taubbl
      !
      mssx   => wadats(imod)%mssx
      mssy   => wadats(imod)%mssy
      mssd   => wadats(imod)%mssd
      mscx   => wadats(imod)%mscx
      mscy   => wadats(imod)%mscy
      mscd   => wadats(imod)%mscd
      !
      dtdyn    => wadats(imod)%dtdyn
      fcut     => wadats(imod)%fcut
      cflxymax => wadats(imod)%cflxymax
      cflthmax => wadats(imod)%cflthmax
      cflkmax =>  wadats(imod)%cflkmax
      !
      usero  => wadats(imod)%usero
      !
      wn     => wadats(imod)%wn
      usshx  => wadats(imod)%usshx
      usshy  => wadats(imod)%usshy
      !
      if ( fl_all ) then
        !
        cg     => wadats(imod)%cg
        !
        atrnx  => wadats(imod)%atrnx
        atrny  => wadats(imod)%atrny
        !
        dddx   => wadats(imod)%dddx
        dddy   => wadats(imod)%dddy
        dcdx   => wadats(imod)%dcdx
        dcdy   => wadats(imod)%dcdy
        dcxdx  => wadats(imod)%dcxdx
        dcydx  => wadats(imod)%dcydx
        dcxdy  => wadats(imod)%dcxdy
        dcydy  => wadats(imod)%dcydy
        !
        !
        alpha  => wadats(imod)%alpha
        !
        if ( inputs(imod)%inflags1(2) ) then
          ca0    => wadats(imod)%ca0
          cai    => wadats(imod)%cai
          cd0    => wadats(imod)%cd0
          cdi    => wadats(imod)%cdi
        end if
        !
        if ( inputs(imod)%inflags1(3) ) then
          ua0    => wadats(imod)%ua0
          uai    => wadats(imod)%uai
          ud0    => wadats(imod)%ud0
          udi    => wadats(imod)%udi
          as0    => wadats(imod)%as0
          asi    => wadats(imod)%asi
        end if
        !
        if ( inputs(imod)%inflags1(5) ) then
          ma0    => wadats(imod)%ma0
          mai    => wadats(imod)%mai
          md0    => wadats(imod)%md0
          mdi    => wadats(imod)%mdi
        end if
        !
        if ( inputs(imod)%inflags1(6) ) then
          ra0    => wadats(imod)%ra0
          rai    => wadats(imod)%rai
        end if
        !
        !
        !
        !
        if (gtype .eq. ungtype) iter   => wadats(imod)%iter
        !
        iappro => wadats(imod)%iappro
        sppnt  => wadats(imod)%sppnt
        !
      end if
      !
    end if
    !
    if ( nrqsg1 .ne. 0 ) then
      irqsg1 => wadats(imod)%irqsg1
      irqsg2 => wadats(imod)%irqsg2
    end if
    gstore => wadats(imod)%gstore
    sstore => wadats(imod)%sstore
    !
    return
    !
    ! formats
    !
1001 format (/' *** error w3seta : grids not initialized *** '/ &
         '                    run w3nmod first '/)
1002 format (/' *** error w3seta : illegal model number *** '/  &
         '                    imod   = ',i10/                   &
         '                    nadata = ',i10/)
    !
    !/
    !/ end of w3seta ----------------------------------------------------- /
    !/
  end subroutine w3seta
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief reduced version of w3seta to point to expended output arrays.
  !>
  !> @param[in] imod model number to point to.
  !> @param[in] ndse error output unit number.
  !> @param[in] ndst test output unit number.
  !>
  !> @author h. l. tolman
  !> @date   22-mar-2021
  !>
  subroutine w3xeta ( imod, ndse, ndst )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    25-dec-2012 : origination.                        ( version 4.11 )
    !/    30-apr-2014 : add s/th1-2m                        ( version 5.01 )
    !/    22-mar-2021 : adds wnmean, tauoc parameters       ( version 7.13 )
    !/
    !  1. purpose :
    !
    !     reduced version of w3seta to point t expended output arrays.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    use w3idatmd, only: inputs
    use w3gdatmd, only: e3df, p2msf, us3df, usspf, gtype, ungtype
    !
    use w3servmd, only: extcde
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
    if ( nadata .eq. -1 ) then
      write (ndse,1001)
      call extcde (1)
    end if
    !
    if ( imod.lt.1 .or. imod.gt.nadata ) then
      write (ndse,1002) imod, nadata
      call extcde (2)
    end if
    !
    !
    ! -------------------------------------------------------------------- /
    ! 2.  set model numbers
    !
    iadata = imod
    !
    ! -------------------------------------------------------------------- /
    ! 3.  set pointers
    !
    if ( ainit2 ) then
      !
      hs     => wadats(imod)%xhs
      wlm    => wadats(imod)%xwlm
      t02    => wadats(imod)%xt02
      t0m1   => wadats(imod)%xt0m1
      t01    => wadats(imod)%xt01
      fp0    => wadats(imod)%xfp0
      thm    => wadats(imod)%xthm
      ths    => wadats(imod)%xths
      thp0   => wadats(imod)%xthp0
      hsig   => wadats(imod)%xhsig
      stmaxe => wadats(imod)%xstmaxe
      stmaxd => wadats(imod)%xstmaxd
      hmaxe  => wadats(imod)%xhmaxe
      hmaxd  => wadats(imod)%xhmaxd
      hcmaxe => wadats(imod)%xhcmaxe
      hcmaxd => wadats(imod)%xhcmaxd
      qp     => wadats(imod)%xqp
      wbt    => wadats(imod)%xwbt
      wnmean => wadats(imod)%xwnmean
      !
      ef     => wadats(imod)%xef
      th1m   => wadats(imod)%xth1m
      sth1m  => wadats(imod)%xsth1m
      th2m   => wadats(imod)%xth2m
      sth2m  => wadats(imod)%xsth2m
      !
      phs    => wadats(imod)%xphs
      ptp    => wadats(imod)%xptp
      plp    => wadats(imod)%xplp
      pdir   => wadats(imod)%xpdir
      psi    => wadats(imod)%xpsi
      pws    => wadats(imod)%xpws
      pwst   => wadats(imod)%xpwst
      pnr    => wadats(imod)%xpnr
      pthp0  => wadats(imod)%xpthp0
      pqp    => wadats(imod)%xpqp
      ppe    => wadats(imod)%xppe
      pgw    => wadats(imod)%xpgw
      psw    => wadats(imod)%xpsw
      ptm1   => wadats(imod)%xptm1
      pt1    => wadats(imod)%xpt1
      pt2    => wadats(imod)%xpt2
      pep    => wadats(imod)%xpep
      !
      charn    => wadats(imod)%xcharn
      tws      => wadats(imod)%xtws
      cge      => wadats(imod)%xcge
      phiaw    => wadats(imod)%xphiaw
      tauwix   => wadats(imod)%xtauwix
      tauwiy   => wadats(imod)%xtauwiy
      tauwnx   => wadats(imod)%xtauwnx
      tauwny   => wadats(imod)%xtauwny
      whitecap => wadats(imod)%xwhitecap
      !
      sxx    => wadats(imod)%xsxx
      syy    => wadats(imod)%xsyy
      sxy    => wadats(imod)%xsxy
      tauox  => wadats(imod)%xtauox
      tauoy  => wadats(imod)%xtauoy
      bhd    => wadats(imod)%xbhd
      phioc  => wadats(imod)%xphioc
      tusx   => wadats(imod)%xtusx
      tusy   => wadats(imod)%xtusy
      ussx   => wadats(imod)%xussx
      ussy   => wadats(imod)%xussy
      prms   => wadats(imod)%xprms
      tpms   => wadats(imod)%xtpms
      p2sms  => wadats(imod)%xp2sms
      us3d   => wadats(imod)%xus3d
      phice  => wadats(imod)%xphice
      tauice => wadats(imod)%xtauice
      ussp   => wadats(imod)%xussp
      tauocx => wadats(imod)%xtauocx
      tauocy => wadats(imod)%xtauocy
      aba    => wadats(imod)%xaba
      abd    => wadats(imod)%xabd
      uba    => wadats(imod)%xuba
      ubd    => wadats(imod)%xubd
      bedforms=> wadats(imod)%xbedforms
      phibbl => wadats(imod)%xphibbl
      taubbl => wadats(imod)%xtaubbl
      !
      mssx   => wadats(imod)%xmssx
      mssy   => wadats(imod)%xmssy
      mssd   => wadats(imod)%xmssd
      mscx   => wadats(imod)%xmscx
      mscy   => wadats(imod)%xmscy
      mscd   => wadats(imod)%xmscd
      !
      dtdyn    => wadats(imod)%xdtdyn
      fcut     => wadats(imod)%xfcut
      cflxymax => wadats(imod)%xcflxymax
      cflthmax => wadats(imod)%xcflthmax
      cflkmax =>  wadats(imod)%xcflkmax
      !
      usero  => wadats(imod)%xusero
      !
      usshx   => wadats(imod)%xusshx
      usshy   => wadats(imod)%xusshy
      !
    end if
    !
    return
    !
    ! formats
    !
1001 format (/' *** error w3xeta : grids not initialized *** '/ &
         '                    run w3nmod first '/)
1002 format (/' *** error w3xeta : illegal model number *** '/  &
         '                    imod   = ',i10/                   &
         '                    nadata = ',i10/)
    !
    !/
    !/ end of w3xeta ----------------------------------------------------- /
    !/
  end subroutine w3xeta
  !/
  !/ end of module w3adatmd -------------------------------------------- /
  !/
end module w3adatmd
