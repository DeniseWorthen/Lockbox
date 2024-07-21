!> @file
!> @brief generic shallow-water boltzmann integral (fbi or tsa).
!>
!> @author bash toulany
!> @author michael casey
!> @author william perrie
!> @date   12-apr-2016
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
!/
!>
!> @brief generic shallow-water boltzmann integral (fbi or tsa).
!>
!> @details interface module for tsa type nonlinear interactions.
!>  based on resio and perrie (2008) and perrie and resio (2009).
!>
!> @author bash toulany
!> @author michael casey
!> @author william perrie
!> @date   12-apr-2016
!>
module w3snl4md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii                 bio |
  !/                  |           bash toulany            |
  !/                  |           michael casey           |
  !/                  |           william perrie          |
  !/                  |                        fortran 90 |
  !/                  | last update :         12-apr-2016 |
  !/                  +-----------------------------------+
  !/
  !/    01-mar-2016 : origination.                        ( version 5.13 )
  !/
  !/    copyright 2009 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !!
  !!    -----------------------------------------------------------------#
  !!                                                                     !
  !!    generic shallow-water boltzmann integral (fbi or tsa)            !
  !!                                                                     !
  !!    -----------------------------------------------------------------#
  !!
  !!
  !! 1. purpose :
  !!
  !!    interface module for tsa type nonlinear interactions.
  !!    based on resio and perrie (2008) and perrie and resio (2009)
  !!
  !! 2. variables and types :
  !!
  !!     name      type  scope    description
  !!    ------------------------------------------------------------------
  !!    ------------------------------------------------------------------
  !!
  !! 3. subroutines and functions :
  !!
  !!     name      type  scope    description
  !!    ------------------------------------------------------------------
  !!     insnl4    subr. w3snl4md corresponding initialization routine.
  !!     ------
  !!     w3snl4    subr. w3snl4md main interface for tsa subroutines.
  !!     ------                   replaces main program "sboltz" in
  !!                              "sbtsa-0-norm-dec15-08.f" with
  !!                              initialization done in subr. insnl4
  !!     gridsetr  subr. w3snl4md setup geometric integration grid
  !!     shloxr    subr. w3snl4md general locus solution
  !!     shlocr    subr. w3snl4md locus solving routine - must converges
  !!     cplshr    subr. w3snl4md computes boltzmann coupling coefficient
  !!     ------
  !!op2
  !!     bash; sections starting & ending with !!op2 are related to subr. optsa2
  !!     optsa2    subr. w3snl4md converts the 2d energy density (f,theta)
  !!     ------                   to polar action density (k,theta) norm. (in k)
  !!                              then splits it into large and small scale
  !!     --- - - - - - - - - - - - - - - - - - - - - - - - - - - - - !!op2
  !!     snlr_tsa  subr. w3snl4md computes dn(k,theta)/dt for tsa
  !!     --------                 due to wave-wave inter. (set itsa = 1)
  !!     snlr_fbi  subr. w3snl4md computes dn(k,theta)/dt for fbi
  !!     --------                 due to wave-wave inter. (set itsa = 0)
  !!
  !!     wkfnc     fnc.  w3snl4md compute wave number "k" for given
  !!                              freq "f" (hz) and water depth "d" (m)
  !!                              or can use subr. wavnu2
  !!     cgfnc     fnc.  w3snl4md compute group velocity "cg" for given
  !!                              freq "f" (hz), water depth "d" (m)
  !!                              and phase speed "cvel" (m/s)
  !!    ------------------------------------------------------------------
  !!
  !! 4. subroutines and functions used :
  !!
  !!     see subroutine documentation.
  !!
  !! 5. remarks :
  !!
  !! 6. switches :
  !!
  !!      !/s      enable subroutine tracing.
  !!      !/t(n)   test output, see subroutines.
  !!
  !! 7. source code :
  !/
  !!    --------------------------------------------------------------- &
  !!    ----------------------------------------------------------------72
  !!    ==================================================================
  !!
  !!
  public
  !!
  !!
  !!    ------------------------------------------------------------------
  !!
  !!
  !!-0  set these important run parameters here and declare them as public
  !! ac   itsa, ialt are set in mod_def and read here
  !!      integer, parameter  :: itsa  =  1  !* = 1 for "snlr_tsa" or tsa
  !!                           ****        !* = 0 for "snlr_fbi" or fbi
  !!      integer, parameter  :: ialt  =  2  !* = 2 do    alternate in snlr's
  !!                           ****        !* = 1 don't alternate in snlr's
  integer, parameter  :: ismo  =  1  !* = 1 do    smooth in interp2
  !!                           ****        !* = 0 don't smooth in interp2
  !!                                       !* interp2 is called only if ialt=2
  integer, parameter  :: npts  = 30  !* # of points on the locus
  !!                           ****        !* can reduce npts for speed
  integer, parameter  :: ndep  = 37  !* # of depths in look-up tables
  !!                           ****        !* can reduce ndep for speed
  !!    ------------------------------------------------------------------
  !!
  !!
  !!-1  declare freq. related arrays & variables dim (nrng) and
  !!            angle related arrays & variables dim (nang) as public
  integer                            :: nrng, nzz, kzone, nb2fp
  integer                            :: nang, na2p1
  integer                            :: np2p1
  real                               :: dfrq, f0
  real                               :: ainc, twopi
  real,    allocatable, dimension(:) :: frqa, oma
  real,    allocatable, dimension(:) :: angl, sinan, cosan
  real,    allocatable, dimension(:) :: dep_tbl
  !!    ------------------------------------------------------------------
  !!
  !!
  !!-2  declare gridsetr 11 look-up tables arrays dim (npts,nang,nzz,ndep)
  !!    plus    pha_tbl array dim=(nrng,ndep) as public
  integer, allocatable, dimension(:,:,:,:) :: kref2_tbl, kref4_tbl
  integer, allocatable, dimension(:,:,:,:) :: jref2_tbl, jref4_tbl
  real,    allocatable, dimension(:,:,:,:) :: wtk2_tbl,  wtk4_tbl
  real,    allocatable, dimension(:,:,:,:) :: wta2_tbl,  wta4_tbl
  real,    allocatable, dimension(:,:,:,:) :: tfac2_tbl, tfac4_tbl
  real,    allocatable, dimension(:,:,:,:) :: grad_tbl
  real,    allocatable, dimension(:,:)     :: pha_tbl
  !!    ------------------------------------------------------------------
  !!
  !!
  !!-3  declare gridsetr 11 returned arrays dim (npts,nang,nzz) as public
  integer, allocatable, dimension(:,:,:) :: kref2, kref4
  integer, allocatable, dimension(:,:,:) :: jref2, jref4
  real,    allocatable, dimension(:,:,:) :: wtk2,  wtk4
  real,    allocatable, dimension(:,:,:) :: wta2,  wta4
  real,    allocatable, dimension(:,:,:) :: tfac2, tfac4
  real,    allocatable, dimension(:,:,:) :: grad
  !!    ------------------------------------------------------------------
  !!
  !!
  !!-4  declare shloxr/shlocr 5 returned arrays dim (npts) as public
  real,    allocatable, dimension(:)     :: wk2x, wk2y
  real,    allocatable, dimension(:)     :: wk4x, wk4y, ds
  !!    ------------------------------------------------------------------
  !!
  !!
  !!-5  declare w3snl4/optsa2 2 shared arrays dim (nrng,nang) & (nrng) as public
  !!    - ef2(nrng,nang) 'ww3' 2d energy
  !!    - ef1(nrng)      'ww3' 1d energy from ef2()
  real,    allocatable, dimension(:,:)   :: ef2
  real,    allocatable, dimension(:)     :: ef1
  !!    ------------------------------------------------------------------
  !!
  !!
  !!-6  declare optsa2 2 returned arrays dim (nrng,nang) as public
  !!    - dens1(nrng,nang)  'ww3' 2d broad scale action
  !!    - dens2(nrng,nang)  'ww3' 2d small scale action
  real,    allocatable, dimension(:,:)   :: dens1, dens2
  !!    ------------------------------------------------------------------
  !!
  !!
  !!-7  declare snlr's 4 returned arrays dim (nrng,nang) as public
  !!    tsa, diag  used for -tsa
  !!    fbi, diag2 used for -fbi
  real,    allocatable, dimension(:,:)   :: tsa, diag
  real,    allocatable, dimension(:,:)   :: fbi, diag2
  !!    ------------------------------------------------------------------
  !!    ==================================================================
  !!
  !!
contains
  !!
  !!
  !!==============================================================================
  !!
  !!    ------------------------------------------------------------------
  !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !>
  !> @brief it reads look-up tables (generated by gridsetr) if file exists
  !>  otherwise it must generate the look-up tables file.
  !>
  !> @author bash toulany
  !> @author michael casey
  !> @author william perrie
  !> @date   12-apr-2016
  !>
  subroutine insnl4
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ------------------------------------------------------------------
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii                 bio |
    !/                  |           bash toulany            |
    !/                  |           michael casey           |
    !/                  |           william perrie          |
    !/                  |                        fortran 90 |
    !/                  | last update :         12-apr-2016 |
    !/                  +-----------------------------------+
    !/
    !/    01-mar-2016 : origination.                        ( version 5.13 )
    !/
    !!
    !!    it returns: 11 look-up tables arrays dim=(npts,nang,nzz,ndep)
    !!                kref2_tbl, kref4_tbl, jref2_tbl, jref4_tbl,
    !!                wtk2_tbl,  wtk4_tbl,  wta2_tbl,  wta4_tbl,
    !!                tfac2_tbl, tfac4_tbl & grad_tbl
    !!                plus       pha_tbl  dim=(nrng,ndep)
    !!                and        dep_tbl  dim=(ndep)
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    !! 1. purpose :
    !!    it reads look-up tables (generated by gridsetr) if file exists
    !!    otherwise it must generate the look-up tables file
    !!
    !! 2. method :
    !!    see subr gridsetr and subr w3snl4 (or subr. w3iogr)
    !!
    !! 3. parameters :
    !!
    !!    parameter list
    !!    ------------------------------------------------------------------
    !!    name     type   scope    i/o  description
    !!    ------------------------------------------------------------------
    !!    nrng      int.  public    i   # of freq. or rings
    !!    nang      int.  public    i   # of angles
    !!    npts      int.  public    i   # of points on the locus
    !!    ndep      int.  public    i   # of depths in look-up tables
    !!    dfrq      real  public    i   frequency multiplier for log freq. spacing
    !!    dep_tbl   r.a.  public    o   depthes in look-up tables arrays dim=(ndep)
    !!    grdfname  chr.  local     -   look-up tables filename (c*80)
    !!    ------------------------------------------------------------------
    !!
    !!    *** the 11 look-up tables for grid integration geometry arrays
    !!    *** at all selected 'ndep' depths defined in dep_tbl(ndep)' array
    !!    *** from gridsetr.            dim=(npts,nang,nzz,ndep)
    !!    kref2_tbl i.a.  public    o   index of reference wavenumber for k2
    !!    kref4_tbl i.a.  public    o   idem for k4
    !!    jref2_tbl i.a.  public    o   index of reference angle      for k2
    !!    jref4_tbl i.a.  public    o   idem for k4
    !!    wtk2_tbl  r.a.  public    o   k2 interpolation weigth along wavenumbers
    !!    wtk4_tbl  r.a.  public    o   idem for k4
    !!    wta2_tbl  r.a.  public    o   k2 interpolation weigth along angles
    !!    wta4_tbl  r.a.  public    o   idem for k4
    !!    tfac2_tbl r.a.  public    o   norm. for interp action density at k2
    !!    tfac4_tbl r.a.  public    o   idem for k4
    !!    grad_tbl  r.a.  public    o   coupling and gradient term in integral
    !!                                  grad = c * h * g**2 * ds / |dw/dn|
    !!    ------------------------------------------------------------------
    !!
    !!    *** the 11 grid integration geometry arrays at one given depth
    !!    *** from gridsetr.            dim=(npts,nang,nzz,ndep)
    !!    kref2     i.a.  public    o   index of reference wavenumber for k2
    !!    kref4     i.a.  public    o   idem for k4
    !!    jref2     i.a.  public    o   index of reference angle      for k2
    !!    jref4     i.a.  public    o   idem for k4
    !!    wtk2      r.a.  public    o   k2 interpolation weigth along wavenumbers
    !!    wtk4      r.a.  public    o   idem for k4
    !!    wta2      r.a.  public    o   k2 interpolation weigth along angles
    !!    wta4      r.a.  public    o   idem for k4
    !!    tfac2     r.a.  public    o   norm. for interp action density at k2
    !!    tfac4     r.a.  public    o   idem for k4
    !!    grad      r.a.  public    o   coupling and gradient term in integral
    !!                                  grad = c * h * g**2 * ds / |dw/dn|
    !!    ------------------------------------------------------------------
    !!
    !! 4. subroutines used :
    !!
    !!     name      type  module   description
    !!    ------------------------------------------------------------------
    !!     gridsetr  subr. w3servmd calc. the 11 grid geometry arrays for one depth
    !!    ------------------------------------------------------------------
    !!
    !! 5. called by :
    !!
    !!     name      type  module   description
    !!    ------------------------------------------------------------------
    !!     strace    subr. w3servmd subroutine tracing.
    !!     w3snl4    subr. w3snl4md interface for tsa  nonlinear interactions
    !!     - or -    the option below was not used
    !!     w3iogr    subr. w3initmd initialization (called by w3shel or wminit)
    !!    ------------------------------------------------------------------
    !!
    !! 6. error messages :
    !!      none.
    !!
    !! 7. remarks :
    !!
    !! 8. structure :
    !!
    !!    see source code.
    !!
    !! 9. switches :
    !!    !/s  enable subroutine tracing.
    !!
    !!10. source code :
    !!
    !!    --------------------------------------------------------------- &
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ----------------------------------------------------------------72
    !!    ==================================================================
    !!
    !!
    use w3odatmd, only: ndse, ndst, ndso
    use wmmdatmd, only: nmpscr, improc
    use constants, only: file_endian
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    implicit none
    !!
    !!    ==================================================================
    !!
    !!    local variables & parameters
    !!    ----------------------------
    !!
    integer              :: irng               !* dummy integer
    integer              :: nd                 !* dummy integer
    !!
    logical              :: unavail = .true.
    logical              :: file_exists
    character            :: grdfname*80
    integer              :: io_unit
    !!
    !!
    !!    dimension wv# array and
    !!    declare local var. dep2, cvel & cgnrng at nrng & depth 'nd'
    real                 :: wka2(nrng)  !* wv# array at depth nd (local)
    real                 :: pha2(nrng)  !* pha array at depth nd (local)
    real                 :: dep2        !* = dep_tbl(nd) depth at nd
    real                 :: cvel        !* phase velocity at (nrng,nd)
    real                 :: cgnrng      !* group velocity at (nrng,nd)
    real                 :: dwka        !* dummy storage for dk
    !!    ---------------------::-----------------------------------------72
    !!    ##################################################################
    !!------------------------------------------------------------------------------
    !!==============================================================================
    !!
    !!
    !!
    !!    ==================================================================
    !!
    !!
    !!-1  make-up the filename from the main parameters
    !!    ------------------------------------------------------------------
    !b    example filename; grdfname = 'grd_1.1025_35_36_30_37.dat'
    !!    grdfname = 'grd_dfrq_nr_na_np_nd.dat'
    !!       where    fm = freq. mult. (dfrq) ex. dfrq = 1.1025 (f6.4)
    !!                nr = # of rings  (nrng) ex. nrng = 35     (i2.2)
    !!                na = # of angles (nang) ex. nang = 36     (i2.2)
    !!                np = # of points (npts) ex. npts = 30     (i2.2)
    !!                nd = # of depths (ndep) ex. nd   = 37     (i2.2)
    write(grdfname,'(a,f6.4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a)')        &
         'grd_', dfrq,'_', nrng,'_', nang,'_',  npts,'_', ndep, '.dat'
    !!    ==================================================================
    !!
    !!
    !!-2  check if the propre gridsetr look-up tables file is available.
    !!    if available read it and if not must generate it (by calling gridsetr)
    !!    ------------------------------------------------------------------
    inquire ( file=grdfname, exist=file_exists )
    !!    assign an unused unit number to io_unit.
    !!    note; it's important to look for an available unused number
    io_unit   = 60
    do while (unavail)
      io_unit = io_unit + 1
      inquire ( io_unit, opened=unavail )
    enddo
    !prt  print *, 'io_unit = ', io_unit
    !!    ==================================================================
    !!
    !!
    !!
    !!
    if ( file_exists ) then
      !!
      !!-3    file exists open it and read it
      !!
      if ( improc .eq. nmpscr ) then
        write ( ndso, 900 ) grdfname
      end if
      !!
      open (unit=io_unit, file=grdfname, status='old',              &
           access='sequential', action='read', form='unformatted', convert=file_endian)
      read (io_unit)  kref2_tbl, kref4_tbl, jref2_tbl, jref4_tbl,   &
           wtk2_tbl,  wtk4_tbl,  wta2_tbl,  wta4_tbl,    &
           tfac2_tbl, tfac4_tbl, grad_tbl,               &
           pha_tbl,   dep_tbl
      close (io_unit)
      !!      ----------------------------------------------------------------
      !!
    else      !* else if ( file_exists )
      !!
      !!
      !!-4    file does not exist, create it here
      !!
      if ( improc .eq. nmpscr ) then
        write ( ndso, 901 ) grdfname
      end if
      !!      ----------------------------------------------------------------
      !!
      !!-4a   define look-up tables depth array 'dep_tbl(ndep)' for ndep=37
      !!      with depths are +ve values
      !!      ----------------------------------------------------------------
      dep_tbl(1:ndep) =                                             &
           (/  2.,  4.,  6.,  8., 10., 12., 14., 16., 18., 20.,   &
           25., 30., 35., 40., 45., 50., 55., 60., 65., 70.,   &
           80., 90.,100.,110.,120.,130.,140.,150.,160.,170.,   &
           220.,270.,320.,370.,420.,470.,520.  /)
      !prt    print *, ' ndep = ', ndep
      !prt    print *, ' dep_tbl(1:ndep) = ', dep_tbl
      !!      ----------------------------------------------------------------
      !!      ================================================================
      !!
      !!
      do nd = 1,ndep
        !!
        !!
        !!-4b     for given new depth dep2 = dep_tbl(nd) calculate
        !!        a new array wka2(:) & new cgnrng corresp. to this depth
        dep2 = dep_tbl(nd)
        do irng=1,nrng
          wka2(irng) = wkfnc(frqa(irng),dep2)
        end do
        cvel    = oma(nrng)/wka2(nrng)         !* phase vel. at (nrng,nd)
        cgnrng  = cgfnc(frqa(nrng),dep2,cvel)  !* group vel. at (nrng,nd)
        !!        --------------------------------------------------------------
        !!        ==============================================================
        !!
        !!
        !!-4c     call gridsetr for this depth at nd
        !!        --------------------------------------------------------------
        !!        -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        call gridsetr ( dep2, wka2, cgnrng )
        !!        -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        !!        it returns: 11 gridsetr arrays which are declared public
        !!                    kref2,kref4, jref2,jref4, wtk2,wtk4, wta2,wta4,
        !!                    tfac2,tfac4  and   grad   all dim=(npts,nang,nzz)
        !!        -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        !!        --------------------------------------------------------------
        !!        ==============================================================
        !!
        !!-4d     store in look-up tables arrays at depth bin # 'nd'
        kref2_tbl(:,:,:,nd) = kref2(:,:,:)
        kref4_tbl(:,:,:,nd) = kref4(:,:,:)
        jref2_tbl(:,:,:,nd) = jref2(:,:,:)
        jref4_tbl(:,:,:,nd) = jref4(:,:,:)
        wtk2_tbl(:,:,:,nd)  = wtk2(:,:,:)
        wtk4_tbl(:,:,:,nd)  = wtk4(:,:,:)
        wta2_tbl(:,:,:,nd)  = wta2(:,:,:)
        wta4_tbl(:,:,:,nd)  = wta4(:,:,:)
        tfac2_tbl(:,:,:,nd) = tfac2(:,:,:)
        tfac4_tbl(:,:,:,nd) = tfac4(:,:,:)
        grad_tbl(:,:,:,nd)  = grad(:,:,:)
        !!        --------------------------------------------------------------
        !!        ==============================================================
        !!
        !!
        !!-4e     calculate pha2(:) at nd depth and store it in pha_tbl(:,:)
        !!        pha2()=k*dk*dtheta, the base area at a grid intersection
        !!        for use in integration of 2-d density functions.
        !!        --------------------------------------------------------------
        !!        below: variable dwka = dk centered at ring 1 (between 0 & 2)
        !!        and computed    pha2(1) = k*dk*dtheta at ring 1
        !!        with wkfnc(frqa(1)/dfrq,dep2) is like wka2(0)
        !!        --assuming frqa(1)/dfrq is like frqa(0)
        dwka    = ( wka2(2) - wkfnc(frqa(1)/dfrq,dep2) ) / 2.
        pha2(1) = wka2(1)*dwka*ainc
        !!
        do irng=2,nrng-1
          !!          below: variable dwka = dk centered at irng (between irng-1 & irng+1)
          !!          and computed    pha2(irng) = k*dk*dtheta at irng
          dwka       = ( wka2(irng+1) - wka2(irng-1) ) / 2.
          pha2(irng) = wka2(irng)*dwka*ainc
        end do
        !!
        !!        below: variable dwka = dk centered at nrng (between nrng-1 & nrng+1)
        !!        and computed    pha2(nrng) = k*dk*dtheta at nrng
        !!        with wkfnc(dfrq*frqa(nrng),dep2) is like wka2(nrng+1)
        !!        --assuming dfrq*frqa(nrng) is like frqa(nrng+1)
        dwka       = ( wkfnc(dfrq*frqa(nrng),dep2) - wka2(nrng-1) ) / 2.
        pha2(nrng) = wka2(nrng)*dwka*ainc
        !!        --------------------------------------------------------------
        !!        ==============================================================
        !!
        !!
        !!-4f     store pha2(:) at nd in  pha_tbl(:,nd) to be added to look-up tables
        pha_tbl(1:nrng, nd) = pha2(1:nrng)
        !!        --------------------------------------------------------------
        !!        ==============================================================
        !!
        !!
      end do ! nd = 1,ndep
      !!      ----------------------------------------------------------------
      !!      ================================================================
      !!
      !!
      !!-5    ounce the look-up tables arrays are full write it out to 'io_unit'
      !!
      if ( improc .eq. nmpscr ) then
        write( ndso,902 )
        open (unit=io_unit, file=grdfname, status='new',              &
             access='sequential', action='write', form='unformatted', convert=file_endian)
        write (io_unit) kref2_tbl, kref4_tbl, jref2_tbl, jref4_tbl,   &
             wtk2_tbl,  wtk4_tbl,  wta2_tbl,  wta4_tbl,    &
             tfac2_tbl, tfac4_tbl, grad_tbl,               &
             pha_tbl,   dep_tbl
        close (io_unit)
        write( ndso,903 ) grdfname
      end if
      !!      ----------------------------------------------------------------
      !!      ================================================================
      !!
    endif     !* end  if ( file_exists )
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    !!
    return
    !!
900 format ( '  grdfname does exist = ',a/                          &
         '  open, read & close file ' )
    !!
901 format ( '  grdfname does not exist = ',a/                      &
         '  generate look-up table arrays ' )
    !!
902 format ( '  done generating look-up table arrays ----------- ' )
    !!
903 format ( '  done writing & closing grdfname ', a )
    !!
  end subroutine insnl4
  !!
  !!==============================================================================
  !!
  !!    ------------------------------------------------------------------
  !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !>
  !> @brief interface module for tsa type nonlinear interactions.
  !>
  !> @details based on resio and perrie (2008) and perrie and resio (2009).
  !>
  !> @param[inout] a       2d action density a(nth,nk) as function of
  !>                       direction (rad) and wavenumber (theta,k).
  !> @param[inout] cg      group velocities.
  !> @param[inout] wn      wavenumbers.
  !> @param[inout] depth   water depth (m).
  !> @param[inout] s       source term.
  !> @param[inout] d       diagonal term of derivative.
  !>
  !> @author bash toulany
  !> @author michael casey
  !> @author william perrie
  !> @date   12-apr-2016
  !>
  subroutine w3snl4 ( a, cg, wn, depth,  s, d )
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ------------------------------------------------------------------
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii                 bio |
    !/                  |           bash toulany            |
    !/                  |           michael casey           |
    !/                  |           william perrie          |
    !/                  |                        fortran 90 |
    !/                  | last update :         12-apr-2016 |
    !/                  +-----------------------------------+
    !/
    !/    01-mar-2016 : origination.                        ( version 5.13 )
    !/
    !!    ------------------------------------------------------------------
    !!
    !!    it returns: s & d  dim = (nth,nk) = (nang,nrng)
    !!
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    !! 1. purpose :
    !!
    !!    interface module for tsa type nonlinear interactions.
    !!    based on resio and perrie (2008) and perrie and resio (2009)
    !!
    !! 2. method :
    !!
    !! 3. parameters :
    !!
    !!    parameter list
    !!    ------------------------------------------------------------------
    !!    name     type   scope    i/o  description
    !!    ------------------------------------------------------------------
    !!    a         r.a.            i   2d action density a(nth,nk) as function of
    !!                                  direction (rad) and wavenumber (theta,k)
    !!    cg        r.a.            i   group velocities             dim=nk
    !!    wn        r.a.            i   wavenumbers                  dim=nk
    !!    depth     real            i   water depth (m)
    !!    s         r.a.            o   source term.                 dim=(nth,nk)
    !!    d         r.a.            o   diagonal term of derivative. dim=(nth,nk)
    !!    ------------------------------------------------------------------
    !!
    !!    nrng      int.  public    o   # of freq. or rings
    !!    nang      int.  public    o   # of angles
    !!    npts      int.  public    i   # of points on the locus
    !!    ndep      int.  public    i   # of depths in look-up tables
    !!    nzz       int.  public    o   linear irngxkrng = (nk*(nk+1))/2
    !!    kzone     int.  public    o   zone of influence = int(alog(4.0)/alog(dfrq))
    !!    nb2fp     int.  public    o   # of bins over fp => dfrq**nb2fp)*fp ~ 2.*fp
    !!                                                    = int(alog(2.0)/alog(dfrq))
    !!    na2p1     int.  public    o   = nang/2 + 1
    !!    np2p1     int.  public    o   = npts/2 + 1
    !!    dfrq      real  public    o   frequency multiplier for log freq. spacing
    !!    f0        real  public    o   = frqa(1); first freq. (hz)
    !!    ainc      real  public    o   = dth; ww3 angle increment (radians)
    !!    twopi     real  public    o   = tpi; ww3i 2*pi = 8.*atan(1.) (radians)
    !!    oma       r.a.  public    o   = sig(1:nk)  ww3 waveumber array   dim=(nrng)
    !!    frqa      r.a.  public    o   = oma(:)/twopi ww3 frequency array dim=(nrng)
    !!    angl      r.a.  public    o   = th(1:nth);   ww3 angles array       dim=(nang)
    !!    sinan     r.a.  public    o   = esin(1:nth); ww3 sin(angl(:)) array dim=(nang)
    !!    cosan     r.a.  public    o   = ecos(1:nth); ww3 cos(angl(:)) array dim=(nang)
    !!    dep_tbl   r.a.  public    i   depthes in look-up tables arrays dim=(ndep)
    !!    ------------------------------------------------------------------
    !!
    !!    *** the 11 look-up tables for grid integration geometry arrays
    !!    *** at all selected 'ndep' depths defined in dep_tbl(ndep)' array
    !!    *** from gridsetr.            dim=(npts,nang,nzz,ndep)
    !!    kref2_tbl i.a.  public    i   index of reference wavenumber for k2
    !!    kref4_tbl i.a.  public    i   idem for k4
    !!    jref2_tbl i.a.  public    i   index of reference angle      for k2
    !!    jref4_tbl i.a.  public    i   idem for k4
    !!    wtk2_tbl  r.a.  public    i   k2 interpolation weigth along wavenumbers
    !!    wtk4_tbl  r.a.  public    i   idem for k4
    !!    wta2_tbl  r.a.  public    i   k2 interpolation weigth along angles
    !!    wta4_tbl  r.a.  public    i   idem for k4
    !!    tfac2_tbl r.a.  public    i   norm. for interp action density at k2
    !!    tfac4_tbl r.a.  public    i   idem for k4
    !!    grad_tbl  r.a.  public    i   coupling and gradient term in integral
    !!                                  grad = c * h * g**2 * ds / |dw/dn|
    !!    ------------------------------------------------------------------
    !!
    !!    *** the 11 grid integration geometry arrays at one given depth
    !!    *** from gridsetr.            dim=(npts,nang,nzz,ndep)
    !!    kref2     i.a.  public    o   index of reference wavenumber for k2
    !!    kref4     i.a.  public    o   idem for k4
    !!    jref2     i.a.  public    o   index of reference angle      for k2
    !!    jref4     i.a.  public    o   idem for k4
    !!    wtk2      r.a.  public    o   k2 interpolation weigth along wavenumbers
    !!    wtk4      r.a.  public    o   idem for k4
    !!    wta2      r.a.  public    o   k2 interpolation weigth along angles
    !!    wta4      r.a.  public    o   idem for k4
    !!    tfac2     r.a.  public    o   norm. for interp action density at k2
    !!    tfac4     r.a.  public    o   idem for k4
    !!    grad      r.a.  public    o   coupling and gradient term in integral
    !!                                  grad = c * h * g**2 * ds / |dw/dn|
    !!    ------------------------------------------------------------------
    !!
    !!    ef2       r.a.  public    o   2d energy density spectrum ef2(theta,f)
    !!                                  = a(theta,k) * 2*pi*oma(f)/cga(f)
    !!                                                     dim=(nrng,nang)
    !!    ef1       r.a.  public    o   1d energy density spectrum ef1(f)
    !!                                                     dim=(nrng)
    !!
    !!    dens1     r.a.  public    o   large-scale action density (k,theta)
    !!                                                     dim=(nrng,nang)
    !!    dens2     r.a.  public    o   small-scale action density (k,theta)
    !!                                                     dim=(nrng,nang)
    !!    ------------------------------------------------------------------
    !!
    !!    for -tsa; the 2 returned arrays tsa & diag dim=(nrng,nang)
    !!    tsa       r.a.  public    o   snl-tsa = sumint + sumintsa
    !!    diag      r.a.  public    o   snl-tsa diagonal term = [dn/dn1]
    !!    ------------------------------------------------------------------
    !!
    !!    for -fbi; the 2 returned arrays fbi & diag2 dim=(nrng,nang)
    !!    fbi       r.a.  public    o   snl-fbi = sumint + sumintp  + sumintx
    !!    diag2     r.a.  public    o   snl-fbi diagonal term = [dn/dn1]
    !!    ------------------------------------------------------------------
    !!
    !! 4. subroutines used :
    !!
    !!     name      type  module   description
    !!    ------------------------------------------------------------------
    !!     strace    subr. w3servmd subroutine tracing.
    !!     optsa2    subr. w3servmd converts the 2d energy density (f,theta)
    !!     ------                   to polar action density (k,theta) norm. (in k)
    !!                              then splits it into large and small scale
    !!     snlr_tsa  subr. w3servmd computes dn(k,theta)/dt for tsa
    !!     --------                 due to wave-wave inter. (set itsa = 1)
    !!     snlr_fbi  subr. w3servmd computes dn(k,theta)/dt for fbi
    !!     --------                 due to wave-wave inter. (set itsa = 0)
    !!    ------------------------------------------------------------------
    !!
    !! 5. called by :
    !!
    !!     name      type  module   description
    !!    ------------------------------------------------------------------
    !!     w3srce    subr. w3srcemd source term integration.
    !!     w3expo    subr. ww3_outp point output post-processor.
    !!     w3exnc    subr. ww3_ounp netcdf point output post-processor.
    !!     gxexpo    subr. gx_outp  grads  point output post-processor.
    !!    ------------------------------------------------------------------
    !!
    !! 6. error messages :
    !!
    !!      none.
    !!
    !! 7. remarks :
    !!
    !! 8. structure :
    !!
    !!    see source code.
    !!
    !! 9. switches :
    !!
    !!    !/s   enable subroutine tracing.
    !!
    !!10. source code :
    !!
    !!    --------------------------------------------------------------- &
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ----------------------------------------------------------------72
    !!    ==================================================================
    !!
    !!
    use constants, only: tpi
    use w3gdatmd,  only: nk,  nth,  xfr, dth,  sig, th, ecos, esin, &
         itsa, ialt
    !!    dimension: sig(0:nk+1),th(nth), ecos(nspec+nth), esin(nspec+nth)
    !!
    use w3servmd, only: extcde
    use w3odatmd, only: ndse, ndst, ndso
    !!    ==================================================================
    !!
    implicit none
    !!
    !!    parameter list
    !!    --------------
    real,    intent(in)  :: a(nth,nk), cg(nk), wn(nk), depth
    real,    intent(out) :: s(nth,nk), d(nth,nk)
    !!
    logical, save        :: first_tsa = .true.
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ------------------------------------------------------------------
    !!
    !!    local parameters & variables
    !!    -----------------------------
    integer              :: irng, iang
    integer              :: nd3         !* bin # corresp. to ww3 dep
    real                 :: dep         !* depth (m), get it from ww3 depth
    real                 :: wka(nk)     !* from ww3 wn(1:nk) corresp. to "depth"
    real                 :: cga(nk)     !* from ww3 cg(1:nk) corresp. to "depth"
    real                 :: pha(nk)     !* k*dk*dtheta array corresp. to "depth"
    real                 :: fac         !* twopi*oma()/cga()
    real                 :: sum1        !* dummy variable
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!
    integer              :: npk         !* bin# at peak frequency fpk
    integer              :: npk2        !* bin# of second peak frequency
    integer              :: npk0        !* dummy int. used in the shuffle of npk's
    integer              :: nsep        !* min # of bins that separates npk & npk2
    !* set nsep = 2
    integer              :: npeaks      !* # of peaks (=0, 1, or 2)
    integer              :: nfs         !* bin# of freq. separation
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!
    integer              :: nbins       !* actual # of bins > npk  (incl. nfs) or
    !!                                        !* actual # of bins > npk2 (incl. nrng)
    !!                                        !* to guarantee a min 1 bin in equi. range
    real                 :: fpk         !* peak frequency (hz)
    real                 :: fpk2        !* second peak frequency (hz)
    real                 :: e1max       !* 1d energy at 1st peak 'fpk'
    real                 :: e1max2      !* 1d energy at 2nd peak 'fpk2'
    real                 :: sumd1       !* sum dens1+dens2 at nfs
    real                 :: sumd2       !* sum dens1+dens2 at nfs+1
    real                 :: densat1     !* averaged dens1  at nfs
    real                 :: densat2     !* averaged dens1  at nfs+1
    !!    --------------------------------------------------------------- &
    !!    ---------------------::-----------------------------------------72
    !!    ##################################################################
    !!------------------------------------------------------------------------------
    !!==============================================================================
    !!
    !!
    !!
    !!ini
    !!    initialization of the output arrays
    !!    before calling tsa subroutines.
    !!    -----------------------------------
    s(:,:) = 0.0
    d(:,:) = 0.0
    !!ini---
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!------------------------------------------------------------------------------
    !!==============================================================================
    !!
    !!
    !!
    if ( first_tsa ) then
      !!
      !!
      !!-0    set parameters & constants
      !!      ---------------------------
      nrng  = nk                     !* nrng = nk  must be odd   <---
      nzz   = (nk * (nk+1)) / 2      !* linear irng, krng
      nang  = nth                    !* nang = nth must be even  <---
      na2p1 = nang/2 + 1             !* mid-angle or angle opposite to 1
      np2p1 = npts/2 + 1             !* mid-index of locus array
      twopi = tpi                    !* twopi = 8.*atan(1.)
      !* get it from ww3 tpi
      !!      ----------------------------------------------------------------
      !!      ================================================================
      !!
      !!
      !!-1    allocate freq & angle related array declared as public
      if ( allocated (frqa) )    deallocate (frqa)
      if ( allocated (oma) )     deallocate (oma)
      if ( allocated (angl) )    deallocate (angl)
      if ( allocated (sinan) )   deallocate (sinan)
      if ( allocated (cosan) )   deallocate (cosan)
      if ( allocated (dep_tbl) ) deallocate (dep_tbl)
      allocate(frqa(nrng))
      allocate(oma(nrng))
      allocate(angl(nang))
      allocate(sinan(nang))
      allocate(cosan(nang))
      allocate(dep_tbl(ndep))
      !!      ----------------------------------------------------------------
      !!
      !!-1a   initialize frequency arrays and related parameters
      !!      --------------------------------------------------
      oma(:)   = sig(1:nk)           !* get it from ww3 sig(1:nk)
      frqa(:)  = oma(:) / twopi
      f0       = frqa(1)
      dfrq     = xfr                 !* ww3 freq mult. for log freq
      !* get it from ww3 xfr
      !!      ----------------------------------------------------------------
      !!
      !!-1b   initialize direction arrays and related parameters
      !!      --------------------------------------------------
      angl(:)  = th(1:nth)           !* get it from ww3 th(1:nth)
      cosan(:) = ecos(1:nth)         !* get it from ww3 ecos(1:nth)
      sinan(:) = esin(1:nth)         !* get it from ww3 esin(1:nth)
      ainc     = dth                 !* ww3 angle increment (radians)
      !* get it from ww3 dth
      !!      ----------------------------------------------------------------
      !!
      !!-1c   define kzone & nb2fp
      !!kz
      !!      kzone = zone of freq influence, function of dfrq
      !!      for different values of x = 2,3,4 & 5
      !!      so,    kzone(x) = int( alog(x)/alog(dfrq) )
      !!      +--------+----------+----------+----------+----------+
      !!      | dfrq   | kzone(2) | kzone(3) | kzone(4) | kzone(5) |
      !!      +--------+----------+----------+----------+----------+
      !!      | 1.05   |    14    |    22    |    28    |    33    |
      !!      +--------+----------+----------+----------+----------+
      !!      | 1.07   |    10    |    16    |    20    |    24    |
      !!      +--------+----------+----------+----------+----------+
      !!      | 1.10   |     7    |    11    |    14    |    17    |
      !!      +--------+----------+----------+----------+----------+
      kzone = int( alog(2.0)/alog(dfrq) )  !* bash; faster without loss of accuracy
      !kz     kzone = int( alog(3.0)/alog(dfrq) )  !* as in gridsetr & snlr_'s
      !kz     kzone = int( alog(4.0)/alog(dfrq) )  !* as in gridsetr & snlr_'s
      !kz     kzone = int( alog(5.0)/alog(dfrq) )  !* as in gridsetr & snlr_'s
      !!kz---
      !!
      !!op2
      !!      nb2fp = # of bins over fp (not incl. fp) - this depends on dfrq
      !!              so that (dfrq**nb2fp)*fp ~ 2.*fp  (like kzone(2))
      !!              used in 1 bin equi. range
      nb2fp = int( alog(2.0)/alog(dfrq) )  !* for equi. range near 2*fp
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - !!op2
      !!      ================================================================
      !!
      !!
      !!-2    allocate gridsetr 11 look-up tables arrays
      !!      plus     pha_tbl array dim=(nrng,ndep) declared as public
      if ( allocated (kref2_tbl) ) deallocate (kref2_tbl)
      if ( allocated (kref4_tbl) ) deallocate (kref4_tbl)
      allocate(kref2_tbl(npts,nang,nzz,ndep))
      allocate(kref4_tbl(npts,nang,nzz,ndep))
      !!
      if ( allocated (jref2_tbl) ) deallocate (jref2_tbl)
      if ( allocated (jref4_tbl) ) deallocate (jref4_tbl)
      allocate(jref2_tbl(npts,nang,nzz,ndep))
      allocate(jref4_tbl(npts,nang,nzz,ndep))
      !!
      if ( allocated (wtk2_tbl) ) deallocate (wtk2_tbl)
      if ( allocated (wtk4_tbl) ) deallocate (wtk4_tbl)
      allocate(wtk2_tbl(npts,nang,nzz,ndep))
      allocate(wtk4_tbl(npts,nang,nzz,ndep))
      !!
      if ( allocated (wta2_tbl) ) deallocate (wta2_tbl)
      if ( allocated (wta4_tbl) ) deallocate (wta4_tbl)
      allocate(wta2_tbl(npts,nang,nzz,ndep))
      allocate(wta4_tbl(npts,nang,nzz,ndep))
      !!
      if ( allocated (tfac2_tbl) ) deallocate (tfac2_tbl)
      if ( allocated (tfac4_tbl) ) deallocate (tfac4_tbl)
      allocate(tfac2_tbl(npts,nang,nzz,ndep))
      allocate(tfac4_tbl(npts,nang,nzz,ndep))
      !!
      if ( allocated (grad_tbl) ) deallocate (grad_tbl)
      allocate(grad_tbl(npts,nang,nzz,ndep))
      !!
      if ( allocated (pha_tbl) ) deallocate (pha_tbl)
      allocate(pha_tbl(nrng,ndep))
      !!      ----------------------------------------------------------------
      !!      ================================================================
      !!
      !!
      !!-3    allocate gridsetr 11 returned arrays declared as public
      if ( allocated (kref2) ) deallocate (kref2)
      if ( allocated (kref4) ) deallocate (kref4)
      allocate(kref2(npts,nang,nzz))
      allocate(kref4(npts,nang,nzz))
      !!
      if ( allocated (jref2) ) deallocate (jref2)
      if ( allocated (jref4) ) deallocate (jref4)
      allocate(jref2(npts,nang,nzz))
      allocate(jref4(npts,nang,nzz))
      !!
      if ( allocated (wtk2) ) deallocate (wtk2)
      if ( allocated (wtk4) ) deallocate (wtk4)
      allocate(wtk2(npts,nang,nzz))
      allocate(wtk4(npts,nang,nzz))
      !!
      if ( allocated (wta2) ) deallocate (wta2)
      if ( allocated (wta4) ) deallocate (wta4)
      allocate(wta2(npts,nang,nzz))
      allocate(wta4(npts,nang,nzz))
      !!
      if ( allocated (tfac2) ) deallocate (tfac2)
      if ( allocated (tfac4) ) deallocate (tfac4)
      allocate(tfac2(npts,nang,nzz))
      allocate(tfac4(npts,nang,nzz))
      !!
      if ( allocated (grad) ) deallocate (grad)
      allocate(grad(npts,nang,nzz))
      !!      ----------------------------------------------------------------
      !!      ================================================================
      !!
      !!
      !!-4    allocate shloxr/shlocr 5 returned arrays declared as public
      if ( allocated (wk2x) ) deallocate (wk2x)
      if ( allocated (wk2y) ) deallocate (wk2y)
      allocate(wk2x(npts))
      allocate(wk2y(npts))
      !!
      if ( allocated (wk4x) ) deallocate (wk4x)
      if ( allocated (wk4y) ) deallocate (wk4y)
      allocate(wk4x(npts))
      allocate(wk4y(npts))
      !!
      if ( allocated (ds) ) deallocate (ds)
      allocate(ds(npts))
      !!      ----------------------------------------------------------------
      !!      ================================================================
      !!
      !!
      !!-5    allocate w3snlx/optsa2 2 shared arrays declared as public
      if ( allocated (ef2) ) deallocate (ef2)
      if ( allocated (ef1) ) deallocate (ef1)
      allocate(ef2(nrng,nang))
      allocate(ef1(nrng))
      !!      ----------------------------------------------------------------
      !!      ================================================================
      !!
      !!
      !!-6    allocate optsa2 2 returned arrays declared as public
      if ( allocated (dens1) ) deallocate (dens1)
      if ( allocated (dens2) ) deallocate (dens2)
      allocate(dens1(nrng,nang))
      allocate(dens2(nrng,nang))
      !!      ----------------------------------------------------------------
      !!      ================================================================
      !!
      !!
      !!-7    allocate snlr_??? 2 returned arrays declared as public
      if ( itsa .eq. 1) then
        !!        allocate tsa, diag  used for -tsa
        if ( allocated (tsa) ) deallocate (tsa)
        if ( allocated (diag) ) deallocate (diag)
        allocate(tsa(nrng,nang))
        allocate(diag(nrng,nang))
      elseif ( itsa .eq. 0) then
        !!        allocate fbi, diag2 used for -fbi
        if ( allocated (fbi) ) deallocate (fbi)
        if ( allocated (diag2) ) deallocate (diag2)
        allocate(fbi(nrng,nang))
        allocate(diag2(nrng,nang))
      else
        write ( ndse,1000 ) itsa
        call extcde ( 115 )
      endif
      !!      ----------------------------------------------------------------
      !!      ================================================================
      !!
      !!
      !!-8    get the 11 look-up table arrays by calling insnl4
      !!      ----------------------------------------------------------------
      !!
      !!      ----------------------------------------------------------------
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      call insnl4
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !!
      !!      it returns: 11 look-up tables arrays dim=(npts,nang,nzz,ndep)
      !!                  kref2_tbl, kref4_tbl, jref2_tbl, jref4_tbl,
      !!                  wtk2_tbl,  wtk4_tbl,  wta2_tbl,  wta4_tbl,
      !!                  tfac2_tbl, tfac4_tbl & grad_tbl
      !!                  plus       pha_tbl  dim=(nrng,ndep)
      !!                  and        dep_tbl  dim=(ndep)
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !!      ----------------------------------------------------------------
      !!      ================================================================
      !!
      !!
      first_tsa  = .false.
      !!
      !!
    endif    !! if ( first_tsa ) then
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!    ##################################################################
    !!
    !!
    !!
    !!*i1 map input ww3 "depth" to "dep" and find corresp. depth bin # "nd3"
    !!    ------------------------------------------------------------------
    dep = depth                !* ww3 depth at a given time & loc.
    nd3 = minloc( abs(dep - dep_tbl(:)), dim=1 )
    !prt  print *, 'depth, corresp depth bin # (nd3)  = ', depth, nd3
    !!    ------------------------------------------------------------------
    !!
    !!
    !!*i2 map from look-up tables the 11 gridsetr arrays corresp. to "nd3"
    !!    kref2(:,:,:) -> grad(:,:,:) are used in subrs. "snlr_*"
    !!    ------------------------------------------------------------------
    kref2(:,:,:) = kref2_tbl(:,:,:,nd3)
    kref4(:,:,:) = kref4_tbl(:,:,:,nd3)
    jref2(:,:,:) = jref2_tbl(:,:,:,nd3)
    jref4(:,:,:) = jref4_tbl(:,:,:,nd3)
    wtk2(:,:,:)  = wtk2_tbl(:,:,:,nd3)
    wtk4(:,:,:)  = wtk4_tbl(:,:,:,nd3)
    wta2(:,:,:)  = wta2_tbl(:,:,:,nd3)
    wta4(:,:,:)  = wta4_tbl(:,:,:,nd3)
    tfac2(:,:,:) = tfac2_tbl(:,:,:,nd3)
    tfac4(:,:,:) = tfac4_tbl(:,:,:,nd3)
    grad(:,:,:)  = grad_tbl(:,:,:,nd3)
    pha(:)       = pha_tbl(:,nd3)
    !!    ------------------------------------------------------------------
    !!
    !!
    !!*i3 map input ww3 arrays "wn(:)" & "cg(:)" to "wka(:)" & "cga(:)"
    !!    note; arrays wka(:) & cga(:) corresp to ww3 "depth" & to be used in "optsa2"
    !!    ------------------------------------------------------------------
    wka(:)  = wn(1:nk)               !* wavenumber array at ww3 "depth"
    cga(:)  = cg(1:nk)               !* group velocity array at ww3 "depth"
    !!    ------------------------------------------------------------------
    !!
    !!
    !!*i4 convert input ww3 2d action density spectrum "a(theta,k)"
    !!    to 2d energy density spectrum "ef2(theta,f)" & reverse indices
    !!    ==>  ef2(f,theta) = a(theta,k) * 2*pi*oma(f)/cga(f)
    !!    ------------------------------------------------------------------
    do irng=1,nrng
      fac = twopi*oma(irng)/cga(irng)
      do  iang=1,nang
        ef2(irng,iang) = a(iang,irng) * fac
      end do
    end do
    !!    ------------------------------------------------------------------
    !!
    !!
    !!*i5 calculte the 1d energy density "ef1(f)"
    !!    ------------------------------------------------------------------
    do irng=1,nrng
      sum1 = 0.0
      do iang=1,nang
        sum1 = sum1 + ef2(irng,iang)
      end do
      ef1(irng) = sum1 * ainc
    end do
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!------------------------------------------------------------------------------
    !!==============================================================================
    !!
    !!
    !!op2
    !!*   bash;
    !!*   find 1 or 2 peaks that satisfy tsa min condition (below) ------- *
    !!*   before calling tsa subrs. otherwise bailout (return) ----------- *
    !!*   bailout & return with init. values of s & d = 0.0 -------------- *
    !!*   nsep   = min # of bins that separates between npk & npk2 (set=2) *
    !!*   nbins  = actual # of bins > npk  (incl. nfs)  -- or --           *
    !!*            actual # of bins > npk2 (incl. nrng)                    *
    !!*            to guarantee a min 1 bin in equi. range                 *
    !!*                                                                    *
    !!*   ===>  in case of just 1 peak the tsa min condition ------------- *  *****
    !!*   ===>  is relative to nrng and is satisfied when ---------------- *  <<<<<
    !!*   ===>  npk.le.nrng-1, to guarantee min 1 bin (incl nrng) > npk -- *  <<<<<
    !!*   ===>  we only need 1 bin in optsa2 to be in the equi. range ---- *  <<<<<
    !!*   ===>  skip if condition is not met ie if  npk.gt.nrng-1  ------- *  <<<<<
    !!*   ---------------------------------------------------------------- *
    !!*                                                                    *
    !!*   ===>  in case of 2 peaks the tsa min condition is applied twice: *  *****
    !!*   ===>   *1) at low freq peak (npk), *2) at high freq peak (npk2)  *  *****
    !!*   ===> *1) tsa min condition for the low freq peak (npk) --------- *  *****
    !!*   ===>  is relative to nfs and is satisfied when ----------------- *  <<<<<
    !!*   ===>  npk.le.nfs-1, to guarantee min 1 bin (incl nfs) > npk2 --- *  <<<<<
    !!*   ===>  we only need 1 bin in optsa2 to be in the equi. range ---- *  <<<<<
    !!*   ===>  skip if condition is not met ie if  npk.gt.nfs-1  -------- *  <<<<<
    !!*                                                                    *
    !!*   ===> *2) tsa min condition for the high freq peak (npk2) ------- *  *****
    !!*   ===>  is relative to nrng and is satisfied when ---------------- *  <<<<<
    !!*   ===>  npk2.le.nrng-1 to guarantee min 1 bin (incl nrng) > npk2   *  <<<<<
    !!*   ===>  we only need 1 bin in optsa2 to be in the equi. range ---- *  <<<<<
    !!*   ===>  skip if condition is not met ie if  npk2.gt.nrng-1 ------- *  <<<<<
    !!*   ---------------------------------------------------------------- *
    !!    ------------------------------------------------------------ !!op2
    !!    ==================================================================
    !!
    !!op2 ctd
    !!    first find the overall peak in ef1(:) with e1max must be > 0.000001
    !!    starting from low freq. find the energy max "e1max" and
    !!    corresp. peak freq. "fpk" and its freq. number "npk".
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    npk    = 0
    fpk    = 0.0
    e1max  = 0.0
    npeaks = 0
    !!    look in the freq range that works for tsa call (see condition below)
    do irng=2,nrng-1             !* last peak loc. is at nrng-1      <<<<<
      !!      pick the 1st local abs. max in [2,nrng-1] using (ef1(irng).gt.e1max)
      !!      so that if 2 equal adj. peaks are found it will pick the 1st e1max
      !!      encountered (i.e. the lower freq. one)
      !!      --------------------------------------------------------------!*  <<<<<
      if ( ef1(irng).gt.ef1(irng-1) .and. ef1(irng).gt.ef1(irng+1)  &
           .and. ef1(irng).gt.e1max ) then
        !!      --------------------------------------------------------------!*  <<<<<
        npk    = irng               !* update npk
        fpk    = frqa(npk)          !* update fpk
        e1max  = ef1(npk)           !* update e1max
        npeaks = 1
      endif
    end do
    !!    ------------------------------------------------------------------
    !!
    !!b   if a 1st peak is not found (npeaks=0 & e1max=0.0 < eps) or
    !!b   if a 1st peak is found with a tiny peak energy (e1max < eps) or
    !!b   if tsa min condition is not met rel. to nrng (npk.gt.nrng-1)   <<<<<
    !!b   this spectrum is not suitable for tsa, so don't call tsa
    !!b   just return (don't stop) with init. values of s(:,:) and d(:,:)=0.0
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if ( e1max.lt.0.000001 ) return
    !!    ------------------------------------------------------------ !!op2
    !!    ==================================================================
    !!
    !!
    !!op2 ctd
    !!    bash; if we are here (i.e. we did not return) then we must
    !!    have found the 1st good peak (= overall peak) with e1max > eps
    !!
    !!    now we look for a new 2nd peak that is at least 'nsep' bins away from
    !!    the 1st peak (nsep=2) (i.e. iabs(irng-npk).gt.nsep) before
    !!    calling new "optsa2" (the 2nd peak will have e1max2 < e1max)
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    nsep   = 2
    npk2   = 0
    fpk2   = 0.0
    e1max2 = 0.0
    !!    again look in the freq range that is in line with tsa min condition
    !!    and find the 2nd highest peak with  eps < e1max2 < e1max
    do irng=2,nrng-1           !* last peak loc. is at nrng-1      <<<<<
      !!      pick the 2nd local abs. max in [2,nrng-1] that is at least 'nsep'
      !!      bins away from the 1st peak using (ef1(irng).ge.e1max2) so that
      !!      if 2 equal adj. peaks are found it will pick the 2nd e1max2
      !!      encountered (i.e. the higher freq. one)
      !!      --------------------------------------------------------------!*  <<<<<
      if ( ef1(irng).gt.ef1(irng-1) .and. ef1(irng).gt.ef1(irng+1)  &
           .and. ef1(irng).ge.e1max2 .and. iabs(irng-npk).gt.nsep )  then
        !!      --------------------------------------------------------------!*  <<<<<
        npk2   = irng             !* update npk2
        fpk2   = frqa(npk2)       !* update fpk2
        e1max2 = ef1(npk2)        !* update e1max2
        npeaks = 2
      endif
    end do
    !!    ------------------------------------------------------------------
    !!
    !!b   if a 2nd peak is not found (npeaks=1 & e1max2=0.0 < eps)
    !!b   if a 2nd peak is found with a tiny peak energy (e1max2 < eps) or
    !!b   if tsa min condition is not met rel. to nrng (npk2.gt.nrng-1)   <<<<<
    !!b   this 2nd peak is not suitable for tsa, drop it and stay with just 1st peak.
    if ( e1max2.lt.0.000001 ) then
      npeaks = 1
      goto 200           !* skip the remaings tests goto 200
    endif
    !!    ------------------------------------------------------------ !!op2
    !!    ==================================================================
    !!
    !!
    !!
    !!
    !!op2 ctd
    if ( npeaks.eq.2 ) then
      !!-1    shuffle the 2 peaks (if necessary) to keep npk to be always < npk2
      !!      this says nothing about which peak is the dominant peak
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      if ( npk2.lt.npk ) then
        npk0   = npk2
        npk2   = npk
        npk    = npk0                 !*  this way  npk < npk2  always
        fpk    = frqa(npk)
        fpk2   = frqa(npk2)
      endif
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !!
      !!-2    here we have 2 peaks (npeaks=2) with  npk < npk2
      !!      find the freq. separation "nfs" (that divide the freq. regime into 2)
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      nfs = int ( (npk+npk2)   / 2.0 )  !* take the lower  bin # to be nfs
      !b      nfs = int ( (npk+npk2+1) / 2.0 )  !* take the higher bin # to be nfs
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    endif   !! if ( npeaks.eq.2 )
    !!
200 continue
    !!    ------------------------------------------------------------ !!op2
    !!    ==================================================================
    !!
    !!
    !!    bash; with the new "optsa2" you are allowed one call (if 1 peak)
    !!          or 2 calls (if 2 peaks) o account for spectra with double peaks.
    !!    note; when nrmn=1 & nrmx=nrng ==> optsa2 = the old optsa
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!
    if ( npeaks.eq.1 ) then
      !!-1    one call to optsa2 for the whole freq. regime ( 1 --> nrng )
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      nbins = nrng - npk    !* # of bins in (npk, nrng] not incl. npk
      if ( nbins.gt.nb2fp ) nbins=nb2fp !* limit equi. range to ~2.0*fp
      !!      ----------------------------------------------------------------
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      call optsa2 ( 1,nrng,       npk, fpk,  nbins, wka, cga )
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !!      it returns variables dens1(nrng,nang) and dens2(nrng,nang)
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !!      ----------------------------------------------------------------
    endif   !! if ( npeaks.eq.1 )
    !!    ==================================================================
    !!
    !!
    if ( npeaks.eq.2 ) then
      !!
      !!-2    now make two calls to new "optsa2" one for each freq regime.
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      nbins = nfs - npk     !* # of bins in (npk, nfs] not incl. npk
      if ( nbins.gt.nb2fp ) nbins=nb2fp  !* limit equi. range to ~2.0*fp
      !!      ---------------------------------------------------------------
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      call optsa2 ( 1,nfs,        npk, fpk,  nbins, wka, cga )
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !!      it returns variables dens1(nrng,nang) and dens2(nrng,nang)
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !!      ----------------------------------------------------------------
      !!
      nbins = nrng - npk2   !* # of bins in (npk2, nrng] not incl. npk2
      if ( nbins.gt.nb2fp ) nbins=nb2fp  !* limit equi. range to ~2.0*fp
      !!      ----------------------------------------------------------------
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      call optsa2 ( nfs+1,nrng,   npk2,fpk2, nbins, wka, cga )
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !!      it returns variables dens1(nrng,nang) and dens2(nrng,nang)
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !!      ----------------------------------------------------------------
      !!      ================================================================
      !!
      !!-3    remove the step like jump (if exists) in dens1() between nfs & nfs+1
      do iang=1,nang
        sumd1 = dens1(nfs,iang)   + dens2(nfs,iang)   !* sum at nfs
        sumd2 = dens1(nfs+1,iang) + dens2(nfs+1,iang) !* sum at nfs+1
        !!
        !!        do 3 bin average for dens1() at nfs   and store in densat1
        densat1 = ( dens1(nfs-1,iang) + dens1(nfs,iang) +           &
             dens1(nfs+1,iang) ) / 3.
        !!        do 3 bin average for dens1() at nfs+1 and store in densat2
        densat2 = ( dens1(nfs,iang)   + dens1(nfs+1,iang) +         &
             dens1(nfs+2,iang) ) / 3.
        !!
        !!        subtitute back into dens1(nfs,iang) & dens1(nfs+1,iang)
        dens1(nfs,iang)   = densat1             ! dens1 at nfs
        dens1(nfs+1,iang) = densat2             ! dens1 at nfs+1
        !!
        !!        recalculate dens2(nfs,iang) & dens2(nfs+1,iang)
        dens2(nfs,iang)   = sumd1 - densat1     ! dens2 at nfs
        dens2(nfs+1,iang) = sumd2 - densat2     ! dens2 at nfs+1
      end do
      !!
    endif   !! if ( npeaks.eq.2 )
    !!    ==================================================================
    !!
    !!
    !!
    !!    -----------------------------------------------------------------#
    !!                                                                     !
    !!    get snl source term and its diagonal term  from "snlr"           !
    !!    for -tsa  only       use "snlr_tsa"       itsa = 1               !
    !!    for -fbi  only       use "snlr_fbi"       itsa = 0               !
    !!                                                                     !
    !!    -----------------------------------------------------------------#
    !!
    !!
    if ( itsa .eq. 1) then
      !!
      !!      ----------------------------------------------------------------
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      call snlr_tsa ( pha, ialt )
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !!      it returns tsa(nrng,nang) & diag(nrng,nang)
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !!      ----------------------------------------------------------------
      !!
      !!      pack results in proper format ---------------------------------- *
      !!      s() & d() arrays are to be returned to ww3 in (k,theta) space
      do irng=1,nrng
        do iang=1,nang
          !!        convert the norm. (in k) polar tsa(k,theta) to polar s(theta,k)
          !!        and  reverse indices back to (iang,irng) as in ww3
          s(iang,irng) = tsa(irng,iang) * wka(irng)   !* <=============
          d(iang,irng) = diag(irng,iang)
          !!        ---------------------------
        end do
      end do
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !!
      !!
    elseif ( itsa .eq. 0) then
      !!
      !!
      !!      ----------------------------------------------------------------
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      call snlr_fbi ( pha, ialt )
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !!      it returns fbi(nrng,nang) & diag2(nrng,nang)
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !!      ----------------------------------------------------------------
      !!
      !!      pack results in proper format ---------------------------------- *
      !!      s() & d() arrays are to be returned to ww3 in (k,theta) space
      do irng=1,nrng
        do iang=1,nang
          !!        convert the norm. (in k) polar fbi(k,theta) to polar s(theta,k)
          !!        and  reverse indices back to (iang,irng) as in ww3
          s(iang,irng) = fbi(irng,iang) * wka(irng)   !* <=============
          d(iang,irng) = diag2(irng,iang)
          !!        --------------------------------
        end do
      end do
      !!      -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !!
    else
      !!
      write( ndse,1000 ) itsa
      call extcde ( 130 )
      !!       --- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      !!
    endif
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    return
    !!
1000 format ( ' w3snl4 error : bad itsa value ',i4)
    !!
  end subroutine w3snl4
  !!
  !!==============================================================================
  !!
  !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !>
  !> @brief this routine sets up the geometric part of the boltzmann integral.
  !>
  !> @details this routine sets up the geometric part of the boltzmann integral
  !>  based on a grid of wave frequencies and directions, with wave-
  !>  numbers related to frequency and depth by linear dispersion.  it
  !>  is adapted from don's original code with changes to modify the
  !>  indexing so there are fewer unused elements, and a number of algo-
  !>  rithmic changes that are mathematically equivalent to don's but
  !>  take advantage of intrinsic functions to form smooth results with
  !>  less reliance on if statements.
  !>
  !>  it calls locus-solving routines shloxr and shlocr and coupling
  !>  coefficient routine cplshr.  if shlocr does not converge, ierr_gr
  !>  will be something other than 0 and the routine will terminate,
  !>  returning ierr_gr to the calling program (see shlocr).
  !>
  !>  it returns array grad(,,), which is an estimate of the product
  !>  c(k1,k2,k3,k4)*h(k1,k3,k4)*ds/|dw/dn| (where n and the k's are all
  !>  vectors) as given, for example, by eq.(7) of 'nonlinear energy
  !>  fluxes and the finite depth equilibrium range in wave spectra,'
  !>  by resio, pihl, tracy and vincent (2001, jgr, 106(c4), p. 6985),
  !>  as well as arrays for indexing, interpolating and weighting locus-
  !>  based wavenumber vectors within the discrete solution grid.
  !>
  !> @param[in] dep      depth (m)
  !> @param[in] wka1     wavenumber array at one depth dim=(nrng)
  !> @param[in] cgnrng1  group velocity at nrng at one depth.
  !>
  !> @author bash toulany
  !> @author michael casey
  !> @author william perrie
  !> @date   12-apr-2016
  !>
  subroutine gridsetr ( dep, wka1, cgnrng1 )
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii                 bio |
    !/                  |           bash toulany            |
    !/                  |           michael casey           |
    !/                  |           william perrie          |
    !/                  |                        fortran 90 |
    !/                  | last update :         12-apr-2016 |
    !/                  +-----------------------------------+
    !/
    !/    01-mar-2016 : origination.                        ( version 5.13 )
    !/
    !!    ------------------------------------------------------------------
    !!
    !!    it returns: kref2,kref4, jref2,jref4, wtk2,wtk4, wta2,wta4,
    !!                tfac2,tfac4  and    grad       all dim=(npts,nang,nzz)
    !!
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    !!
    !! 1. purpose :
    !!
    !!    -------------------------------------------------------------------------#
    !!                                                                             !
    !!    this routine sets up the geometric part of the boltzmann integral        !
    !!    based on a grid of wave frequencies and directions, with wave-           !
    !!    numbers related to frequency and depth by linear dispersion.  it         !
    !!    is adapted from don's original code with changes to modify the           !
    !!    indexing so there are fewer unused elements, and a number of algo-       !
    !!    rithmic changes that are mathematically equivalent to don's but          !
    !!    take advantage of intrinsic functions to form smooth results with        !
    !!    less reliance on if statements.                                          !
    !!                                                                             !
    !!    it calls locus-solving routines shloxr and shlocr and coupling           !
    !!    coefficient routine cplshr.  if shlocr does not converge, ierr_gr        !
    !!    will be something other than 0 and the routine will terminate,           !
    !!    returning ierr_gr to the calling program (see shlocr).                   !
    !!                                                                             !
    !!    it returns array grad(,,), which is an estimate of the product           !
    !!    c(k1,k2,k3,k4)*h(k1,k3,k4)*ds/|dw/dn| (where n and the k's are all       !
    !!    vectors) as given, for example, by eq.(7) of 'nonlinear energy           !
    !!    fluxes and the finite depth equilibrium range in wave spectra,'          !
    !!    by resio, pihl, tracy and vincent (2001, jgr, 106(c4), p. 6985),         !
    !!    as well as arrays for indexing, interpolating and weighting locus-       !
    !!    based wavenumber vectors within the discrete solution grid.              !
    !!    -------------------------------------------------------------------------#
    !!
    !!
    !! 2. method :
    !!
    !! 3. parameters :
    !!
    !!    parameter list
    !!    ------------------------------------------------------------------
    !!    name     type   scope    i/o  description
    !!    ------------------------------------------------------------------
    !!    nrng      int.  public    i   # of freq. or rings
    !!    nang      int.  public    i   # of angles
    !!    npts      int.  public    i   # of points on the locus
    !!    nzz       int.  public    i   linear irngxkrng = (nk*(nk+1))/2
    !!    kzone     int.  public    i   zone of influence = int(alog(4.0)/alog(dfrq))
    !!    na2p1     int.  public    i   = nang/2 + 1
    !!    np2p1     int.  public    i   = npts/2 + 1
    !!    ------------------------------------------------------------------
    !!
    !!    dfrq      real  public    i   frequency multiplier for log freq. spacing
    !!    f0        real  public    i   = frqa(1); first freq. (hz)
    !!    twopi     real  public    i   = tpi; ww3i 2*pi = 8.*atan(1.) (radians)
    !!    ainc      real  public    i   = dth; ww3 angle increment (radians)
    !!    dep       real  local     i   = depth (m)
    !!    frqa      r.a.  public    i   = oma(:)/twopi ww3 frequency array dim=(nrng)
    !!    angl      r.a.  public    i   = th(1:nth);   ww3 angles array       dim=(nang)
    !!    sinan     r.a.  public    i   = esin(1:nth); ww3 sin(angl(:)) array dim=(nang)
    !!    cosan     r.a.  public    i   = ecos(1:nth); ww3 cos(angl(:)) array dim=(nang)
    !!    wka1      r.a.  local     i   = wavenumber array at one depth dim=(nrng)
    !!    cgnrng1   real  local     i   = group vel. at nrng at one depth
    !!    ------------------------------------------------------------------
    !!
    !!    *** the 11 grid integration geometry arrays at one given depth
    !!    *** from gridsetr.            dim=(npts,nang,nzz,ndep)
    !!    kref2     i.a.  public    o   index of reference wavenumber for k2
    !!    kref4     i.a.  public    o   idem for k4
    !!    jref2     i.a.  public    o   index of reference angle      for k2
    !!    jref4     i.a.  public    o   idem for k4
    !!    wtk2      r.a.  public    o   k2 interpolation weigth along wavenumbers
    !!    wtk4      r.a.  public    o   idem for k4
    !!    wta2      r.a.  public    o   k2 interpolation weigth along angles
    !!    wta4      r.a.  public    o   idem for k4
    !!    tfac2     r.a.  public    o   norm. for interp action density at k2
    !!    tfac4     r.a.  public    o   idem for k4
    !!    grad      r.a.  public    o   coupling and gradient term in integral
    !!                                  grad = c * h * g**2 * ds / |dw/dn|
    !!    ------------------------------------------------------------------
    !!
    !! 4. subroutines used :
    !!
    !!     name      type  module   description
    !!    ------------------------------------------------------------------
    !!     shloxr    subr. w3servmd general locus solution for input vectors
    !!                              k1 & k3 when |k1| .eq. |k3|
    !!     shlocr    subr. w3servmd general locus solution for input vectors
    !!                              k1 & k3 when |k1| .ne. |k3|
    !!     cplshr    subr. w3servmd calculates boltzmann coupling coefficient
    !!                              in shallow water
    !!    ------------------------------------------------------------------
    !!
    !! 5. called by :
    !!
    !!     name      type  module   description
    !!    ------------------------------------------------------------------
    !!     insnl4    subr. w3snl4md initialize the grid geometry
    !!    ------------------------------------------------------------------
    !!
    !! 6. error messages :
    !!
    !!      none.
    !!
    !! 7. remarks :
    !!
    !! 8. structure :
    !!
    !!    see source code.
    !!
    !! 9. switches :
    !!
    !!    !/s  enable subroutine tracing.
    !!
    !!10. source code :
    !!
    !!    --------------------------------------------------------------- &
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ----------------------------------------------------------------72
    !!    ==================================================================
    !!
    !!
    implicit none
    !!
    !!    parameter list
    !!    --------------
    real,    intent(in)  :: dep
    real,    intent(in)  :: wka1(nrng), cgnrng1  !* use new names locally
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ------------------------------------------------------------------
    !!
    !!
    !!    local parameters & variables
    !!    -----------------------------
    integer              :: irng,krng, iang,kang, ipt
    integer              :: iizz, izz, ir, i
    integer              :: kmax               !* = min(irng+kzone, nrng)
    !!
    real                 :: g, gsq
    real                 :: alf0,aldfrq, wk1x,wk1y, wk3x,wk3y
    !!
    real                 :: wn2,th2, wn2d,tnh2, om2,f2,cg2, tt2,w2
    real                 :: wn4,th4, wn4d,tnh4, om4,f4,cg4, tt4,w4
    real                 :: dwdnsq,dwdn, dif13,dif14, er
    !!
    !!hv  bash; with !hv on, move heaviside section up & don't use var heaviside
    !hv   real                 :: heaviside
    !!hv---
    !!
    real                 :: csq
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ---------------------::-----------------------------------------72
    !!    ##################################################################
    !!------------------------------------------------------------------------------
    !!==============================================================================
    !!
    !!
    !!    initial constants
    !!    ------------------
    g      = 9.806                              !* set = grav as in constants
    gsq    = 96.157636                          !* set = grav**2
    !!
    alf0   = alog(frqa(1))                      !* ln(f0) for ir calc. below
    aldfrq = alog(dfrq)                         !* ln(dfrq)       "
    !!
    !!ini
    !!    initialize array grad
    !!    ----------------------
    grad(:,:,:)  = 0.0
    kref2(:,:,:) = 0
    kref4(:,:,:) = 0
    jref2(:,:,:) = 0
    jref4(:,:,:) = 0
    wtk2(:,:,:)  = 0.0
    wtk4(:,:,:)  = 0.0
    wta2(:,:,:)  = 0.0
    wta4(:,:,:)  = 0.0
    tfac2(:,:,:) = 0.0
    tfac4(:,:,:) = 0.0
    !!ini---
    !!------------------------------------------------------------------------------
    !!
    !!
    !!    irng and iang are k1 parameters; krng and kang are k3 parameters
    iang = 1                               !* set = 1 and will remain = 1
    !!
    !!20
    do 20 irng=1,nrng
      !!kz
      kmax = min(irng+kzone, nrng)   !* bash; sometimes a locus pt is outside nrng
      !kz     kmax = min(irng+kzone, nrng-1) !* bash; taking 1 out will not affect kzone, try it
      !!kz---
      !!kz---
      !!
      wk1x   = wka1(irng)
      wk1y   = 0.0                         !* set = 0.0 and will remain = 0.0
      iizz = (nrng-1)*(irng-1)-((irng-2)*(irng-1))/2
      !!30
      !!kz
      do 30 krng=irng,kmax
        !!kz---
        !kz     do 30 krng=irng,nrng
        !!
        !!        bash; check1 - change this ratio from > 4 to > 3   and
        !!              make it consistent with similar test done in subr. snlr_'s
        !kz       if ( frqa(krng)/frqa(irng) .gt. 2. ) go to 30  !* bash; use .gt. 2 for speed
        !kz       if ( frqa(krng)/frqa(irng) .gt. 3. ) go to 30  !* original snlr_'s
        !kz       if ( frqa(krng)/frqa(irng) .gt. 4. ) go to 30  !* original gridsetr
        !!kz---
        izz = krng+iizz
        !!40
        do 40 kang=1,nang
          !!
          wk3x = wka1(krng)*cosan(kang)
          wk3y = wka1(krng)*sinan(kang)
          !!
          if ( krng.eq.irng ) then              !* wn3 = wn1
            !!
            !!ba1         bash; skip k1 but keep the opposite angle to k1 - orig setting
            !!ba1               remember here iang = 1
            if ( kang .eq. 1 ) go to 40         !* th3 = th1
            !!ba1---
            !!            ----------------------------------------------------------
            !!            -- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            call shloxr ( dep,       wk1x,wk1y,wk3x,wk3y )
            !!            -- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            !!            it returns: wk2x, wk2y, wk4x, wk4y & ds all dim=(npts)
            !!                        and all are public
            !!            -- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            !!            ----------------------------------------------------------
            !!
          else                                  !* wn3 > wn1
            !!
            !!            ----------------------------------------------------------
            !!            -- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            call shlocr ( dep,       wk1x,wk1y,wk3x,wk3y )
            !!            -- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            !!            it returns: wk2x, wk2y, wk4x, wk4y & ds all dim=(npts)
            !!                        and all are public
            !!            -- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            !!            ----------------------------------------------------------
            !!
          end if   !!  if ( krng.eq.irng )
          !!
          !!          set the heaviside coefficient
          !b          dif13 = (wk1x-wk3x)**2   +  (wk1y-wk3y)**2        !* wk1y = 0.0
          !b          dif13 = (wk1x-wk3x)**2   +       (wk3y)**2
          dif13 = (wk1x-wk3x)*(wk1x-wk3x) + wk3y*wk3y
          !!50
          do 50 ipt=1,npts
            !!
            !!xlc1        bash; skip k1 but keep the opposite angle to k1 - original setting
            if ( kang.eq.1 ) then                        !* th3=+th1, iang=1
              if (ipt.eq.1 .or. ipt.eq.np2p1) go to 50   !* skip x-axis loci
            end if
            !!xlc1---
            !!            ----------------------------------------------------------
            !!
            !!hv          bash; with !hv on, move heaviside section from below to here
            !!            bash moved this section here. *** check first compute after ***
            !!            skip first then compute only if heaviside=1, without using it
            !!            i.e. compute only if dif13.le.dif14 with heaviside=1 omitted.
            !!            note; with !hv option is on, you don't need to turn options
            !!            ----  !k19p1 nor !cp4 on.ayou only need one of the three.
            !!            ----------------------------------------------------------
            !!            set the heaviside coefficient
            !b            dif14 = (wk1x-wk4x(ipt))**2 + (wk1y-wk4y(ipt))**2 !* wk1y=0.0
            !b            dif14 = (wk1x-wk4x(ipt))**2 +      (wk4y(ipt))**2
            dif14 = (wk1x-wk4x(ipt))*(wk1x-wk4x(ipt)) +             &
                 wk4y(ipt)*wk4y(ipt)
            !!
            if ( dif13 .gt. dif14 ) go to 50    !* skip, don't compute
            !!
            !b            if ( dif13 .gt. dif14 ) then
            !b               heaviside = 0.                   !* eq(12) of rptv
            !b               go to 50
            !b            else
            !b               heaviside = 1.                   !* eq(11) of rptv
            !b            end if
            !!hv---
            !!            ----------------------------------------------------------
            !!
            !!            set the coupling coefficient for ipt'th locus position
            !!            ----------------------------------------------------------
            !!            -- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            call cplshr ( wk4x(ipt),wk4y(ipt), wk3x,wk3y,           &
                 wk2x(ipt),wk2y(ipt), dep, csq,            &
                 irng,krng,kang,ipt )
            !!            -- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            !!            it returns: the coupling coefficient  csq
            !!            -- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            !!            ----------------------------------------------------------
            !!
            !!wn2         set parameters related to ipt'th locus wavenumber vector k2
            !!            ----------------------------------------------------------
            !b            wn2  = sqrt(wk2x(ipt)**2 + wk2y(ipt)**2)               !* k2
            wn2  = sqrt(wk2x(ipt)*wk2x(ipt) + wk2y(ipt)*wk2y(ipt)) !* k2
            th2  = atan2(wk2y(ipt),wk2x(ipt))                !* k2 direction
            if ( th2 .lt. 0. ) th2 = th2 + twopi             !* +ve in radians
            wn2d = wn2*dep                                   !* k2*depth
            tnh2 = tanh(wn2d)                                !* tanh(k2*depth)
            om2  = sqrt(g*wn2*tnh2)                          !* omega2 (rad)
            !b            cg2  = 0.5*(om2/wn2)*(1.+wn2d*(1.-tnh2**2)/tnh2) !* group velocity
            cg2  = 0.5*(om2/wn2)*(1.+wn2d*((1./tnh2)-tnh2))  !* group velocity
            f2   = om2/twopi                                 !* f2 (hz)
            !!            ----------------------------------------------------------
            !!
            !!wn4         set parameters related to ipt'th locus wavenumber vector k4
            !!            ----------------------------------------------------------
            !b            wn4  = sqrt(wk4x(ipt)**2 + wk4y(ipt)**2)
            wn4  = sqrt(wk4x(ipt)*wk4x(ipt) + wk4y(ipt)*wk4y(ipt))
            th4  = atan2(wk4y(ipt),wk4x(ipt))
            if ( th4 .lt. 0. ) th4 = th4 + twopi
            wn4d = wn4*dep
            tnh4 = tanh(wn4d)
            om4  = sqrt(g*wn4*tnh4)
            !b            cg4  = 0.5*(om4/wn4)*(1.+wn4d*(1.-tnh4**2)/tnh4)
            cg4  = 0.5*(om4/wn4)*(1.+wn4d*((1./tnh4)-tnh4))
            f4   = om4/twopi
            !!            ----------------------------------------------------------
            !!
            !!
            !!hv          bash; with !hv on, move heaviside section up
            !!            bash moved this section up. check first compute after.
            !!            ----------------------------------------------------------
            !!            set the heaviside coefficient
            !b            dif14 = (wk1x-wk4x(ipt))**2 + (wk1y-wk4y(ipt))**2 !* wk1y=0.0
            !b            dif14 = (wk1x-wk4x(ipt))**2 +      (wk4y(ipt))**2
            !hv           dif14 = (wk1x-wk4x(ipt))*(wk1x-wk4x(ipt)) +             &
            !hv                                          wk4y(ipt)*wk4y(ipt)
            !hv           if ( dif13 .gt. dif14 ) then
            !hv              heaviside = 0.                   !* eq(12) of rptv
            !hv           else
            !hv              heaviside = 1.                   !* eq(11) of rptv
            !hv           end if
            !!hv---
            !!            ----------------------------------------------------------
            !!
            !!
            !!            dwdn is the same as sqrt(zzsum) in don's code, here reduced to a
            !!            simpler but mathematically equivalent form that should vary
            !!            smoothly between deep and intermediate water owing to identities
            !!            using the computer's tanh() function
            !!            ----------------------------------------------------------
            !!
            !!            set grad(,,);
            !!            looks like the g^2 goes with csq (webb'1978, eq. a2)
            !!            ----------------------------------------------------------
            !!
            !b            dwdnsq = cg2**2  - 2.*cg2*cg4 * cos(th2-th4) + cg4**2
            dwdnsq = cg2*cg2 - 2.*cg2*cg4 * cos(th2-th4) + cg4*cg4
            !!            ----------------------------------------------------------
            !!
            dwdn               = sqrt(dwdnsq)
            !!            ----------------------------------------------------------
            !!
            !!hv          bash; with !hv on, don't use var heaviside (by here it's = 1.0)
            grad(ipt,kang,izz) =           ds(ipt)*csq*gsq/dwdn
            !!hv---
            !hv           grad(ipt,kang,izz) = heaviside*ds(ipt)*csq*gsq/dwdn
            !!hv---
            !!            ----------------------------------------------------------
            !!            ==========================================================
            !!
            !!
            !!            set interpolation, indexing and weight parameters for
            !!            computations along wavenumber radials
            !!            ----------------------------------------------------------
            !!
            !!f2          --------------------
            if ( f2 .lt. f0 ) then
              wtk2(ipt,kang,izz)  = 1.
              tfac2(ipt,kang,izz) = 0.
              kref2(ipt,kang,izz) = 1
            else
              ir = 1+int((alog(f2)-alf0)/aldfrq)
              if ( ir+1 .gt. nrng ) then
                wtk2(ipt,kang,izz)  = 0.
                er = (wka1(nrng)/wn2)**(2.5)
                tt2= er*(cg2/cgnrng1)*(frqa(nrng)/f2)*(wka1(nrng)/wn2)
                tfac2(ipt,kang,izz) = tt2
                kref2(ipt,kang,izz) = nrng - 1
              else
                w2 = (f2-frqa(ir))/(frqa(ir+1)-frqa(ir))
                wtk2(ipt,kang,izz)  = 1. - w2
                tfac2(ipt,kang,izz) = 1.
                kref2(ipt,kang,izz) = ir
              end if
            end if
            !!            ----------------------------------------------------------
            !!
            !!f4          --------------------
            if ( f4 .lt. f0 ) then
              wtk4(ipt,kang,izz)  = 1.
              tfac4(ipt,kang,izz) = 0.
              kref4(ipt,kang,izz) = 1
            else
              ir = 1+int((alog(f4)-alf0)/aldfrq)
              if ( ir+1 .gt. nrng ) then
                wtk4(ipt,kang,izz)  = 0.
                er = (wka1(nrng)/wn4)**2.5
                tt4= er*(cg4/cgnrng1)*(frqa(nrng)/f4)*(wka1(nrng)/wn4)
                tfac4(ipt,kang,izz) = tt4
                kref4(ipt,kang,izz) = nrng - 1
              else
                w2 = (f4-frqa(ir))/(frqa(ir+1)-frqa(ir))
                wtk4(ipt,kang,izz)  = 1. - w2
                tfac4(ipt,kang,izz) = 1.
                kref4(ipt,kang,izz) = ir
              end if
            end if
            !!            ----------------------------------------------------------
            !!
            !!
            !!            set indexing and weight parameters for computations around
            !!            azimuths; it appears that jref2 and jref4 should be bounded
            !!            between 0 and nang-1 so that when iang (=1,nang) is added in
            !!            the integration section, the proper bin index will arise;
            !!            the weights wta2 and wta4 seem to be the fractional bin
            !!            widths between th2 or th4 and the next increasing
            !!            directional bin boundary
            !!            ----------------------------------------------------------
            !!
            i = int(th2/ainc)
            wta2(ipt,kang,izz)  = 1. - abs(th2-i*ainc)/ainc
            if ( i .ge. nang )  i = i - nang
            jref2(ipt,kang,izz) = i
            !mpc          jref2(ipt,kang,izz) = mod(i,nang) !* is this better that the above two lines?
            !!
            i = int(th4/ainc)
            wta4(ipt,kang,izz)  = 1. - abs(th4-i*ainc)/ainc
            if ( i .ge. nang )  i = i - nang
            jref4(ipt,kang,izz) = i
            !mpc          jref4(ipt,kang,izz) = mod(i,nang) !* is this better that the above two lines?
            !!
50        end do                              !* end of ipt loop
          !!
40      end do                                !* end of kang loop
        !!
30    end do                                  !* end of krng loop
      !!
20  end do                                    !* end of irng loop
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    return
    !!
  end subroutine gridsetr
  !!
  !!==============================================================================
  !!
  !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !>
  !> @brief general locus solution for input vectors (wk1x,wk1y).
  !>
  !> @verbatim
  !>    general locus solution for input vectors (wk1x,wk1y) and
  !>    (wk3x,wk3y) of the same magnitude but not in the same direction
  !>    (or singularness will occur), output vectors (wk2x,wk2y) and
  !>    (wk4x,wk4y), and element length ds along locus curve:
  !>
  !>    with wavenumber vector n identified by (wknx,wkny), its magnitude
  !>    given by wkn = sqrt(wknx**2+wkny**2) and its associated radian
  !>    frequency given by sign = sqrt[g*wkn*tanh(wkn*dep)], where g is
  !>    gravitational acceleration and dep is water depth, the four-wave
  !>    resonance condition is satisfied along a locus of pts defined by
  !>
  !>    [1]  (wk1x,wk1y) + (wk2x,wk2y) - (wk3x,wk3y) - (wk4x,wk4y) = 0
  !>
  !>    [2]  sig1 + sig2 - sig3 - sig4 = 0
  !>
  !>    in the case where k1 [= sqrt(wk1x**2+wk1y**2)] is equal to k3
  !>    [= sqrt(wk3x**2+wk3y**2)], we have by dispersion,
  !>
  !>    [3]  sig1 = sqrt[g*k1*tanh(k1*h)] = sqrt[g*k3*tanh(k3*h)] = sig3
  !>
  !>    so sig1 - sig3 = 0 and [2] becomes sig2 = sig4, where, again by
  !>    dispersion,
  !>
  !>    [4]  sig2 = sqrt(g*k2*tanh(k2*h)] = sig4 = sqrt(g*k4*tanh(k4*h)]
  !>
  !>    and consequently k2 = k4.  this simplifies the locus solution
  !>    considerably, and it can be shown that the (wk2x,wk2y) locus is
  !>    along the perpendicular bisector of the (px,py) vector given by
  !>
  !>    [5]  (px,py) = (wk3x-wk1x,wk3y-wk1y)
  !>
  !>    and thereby from [1]
  !>    [6]  (wk4x,wk4y) = (wk2x,wk2y) - (px,py)
  !>
  !>    we note that these loci are independent of depth, although depth
  !>    is used to set the length of the locus line by requiring that its
  !>    range on either side of the p vector correspond to a wave with a
  !>    wkx freq four times that of the k1 vector (the locus line is made
  !>    up of npts segments of length ds; the outer edges of the terminal
  !>    segments satisfy the length constraint; vectors k2 and k4 extend
  !>    to segment centers and will sufficiently approximate the length
  !>    constraint).  as compared to srshlocr.f, we can do all
  !>    calculations here in dimensional space.
  !> @endverbatim
  !>
  !> @param[in] dep
  !> @param[in] wk1x
  !> @param[in] wk1y
  !> @param[in] wk3x
  !> @param[in] wk3y
  !>
  !> @author bash toulany
  !> @author michael casey
  !> @author william perrie
  !> @date   12-apr-2016
  !>
  subroutine shloxr ( dep, wk1x,wk1y, wk3x,wk3y )
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii                 bio |
    !/                  |           bash toulany            |
    !/                  |           michael casey           |
    !/                  |           william perrie          |
    !/                  |                        fortran 90 |
    !/                  | last update :         12-apr-2016 |
    !/                  +-----------------------------------+
    !/
    !/    01-mar-2016 : origination.                        ( version 5.13 )
    !/
    !!    ------------------------------------------------------------------
    !!
    !!    it returns: wk2x, wk2y, wk4x, wk4y & ds all dim=(npts)
    !!
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    !!
    !! 1. purpose :
    !!
    !!    -------------------------------------------------------------------------#
    !!                                                                             !
    !!    general locus solution for input vectors (wk1x,wk1y) and                 !
    !!    (wk3x,wk3y) of the same magnitude but not in the same direction          !
    !!    (or singularness will occur), output vectors (wk2x,wk2y) and             !
    !!    (wk4x,wk4y), and element length ds along locus curve:                    !
    !!                                                                             !
    !!    with wavenumber vector n identified by (wknx,wkny), its magnitude        !
    !!    given by wkn = sqrt(wknx**2+wkny**2) and its associated radian           !
    !!    frequency given by sign = sqrt[g*wkn*tanh(wkn*dep)], where g is          !
    !!    gravitational acceleration and dep is water depth, the four-wave         !
    !!    resonance condition is satisfied along a locus of pts defined by         !
    !!                                                                             !
    !!    [1]  (wk1x,wk1y) + (wk2x,wk2y) - (wk3x,wk3y) - (wk4x,wk4y) = 0           !
    !!                                                                             !
    !!    [2]  sig1 + sig2 - sig3 - sig4 = 0                                       !
    !!                                                                             !
    !!    in the case where k1 [= sqrt(wk1x**2+wk1y**2)] is equal to k3            !
    !!    [= sqrt(wk3x**2+wk3y**2)], we have by dispersion,                        !
    !!                                                                             !
    !!    [3]  sig1 = sqrt[g*k1*tanh(k1*h)] = sqrt[g*k3*tanh(k3*h)] = sig3         !
    !!                                                                             !
    !!    so sig1 - sig3 = 0 and [2] becomes sig2 = sig4, where, again by          !
    !!    dispersion,                                                              !
    !!                                                                             !
    !!    [4]  sig2 = sqrt(g*k2*tanh(k2*h)] = sig4 = sqrt(g*k4*tanh(k4*h)]         !
    !!                                                                             !
    !!    and consequently k2 = k4.  this simplifies the locus solution            !
    !!    considerably, and it can be shown that the (wk2x,wk2y) locus is          !
    !!    along the perpendicular bisector of the (px,py) vector given by          !
    !!                                                                             !
    !!    [5]  (px,py) = (wk3x-wk1x,wk3y-wk1y)                                     !
    !!                                                                             !
    !!    and thereby from [1]                                                     !
    !!    [6]  (wk4x,wk4y) = (wk2x,wk2y) - (px,py)                                 !
    !!                                                                             !
    !!    we note that these loci are independent of depth, although depth         !
    !!    is used to set the length of the locus line by requiring that its        !
    !!    range on either side of the p vector correspond to a wave with a         !
    !!    wkx freq four times that of the k1 vector (the locus line is made        !
    !!    up of npts segments of length ds; the outer edges of the terminal        !
    !!    segments satisfy the length constraint; vectors k2 and k4 extend         !
    !!    to segment centers and will sufficiently approximate the length          !
    !!    constraint).  as compared to srshlocr.f, we can do all                   !
    !!    calculations here in dimensional space.                                  !
    !!    -------------------------------------------------------------------------#
    !!
    !! 2. method :
    !!
    !! 3. parameters :
    !!
    !!    parameter list
    !!    ------------------------------------------------------------------
    !!    name     type   scope    i/o  description
    !!    ------------------------------------------------------------------
    !!    npts      int.  public    i   # of points on the locus
    !!    np2p1     int.  public    i   = npts/2 + 1
    !!    ----------------------------------------------------------------
    !!
    !!    *** arrays wk2x,wk2y, wk4x,wk4y & ds are related to locus solutioni
    !!        for given vectors k1 & k3        all have dim=(npts)
    !!    wk2x      r.a.  public    o   x_cmp of vector k2 solution on the locus
    !!    wk2y      r.a.  public    o   y_cmp of vector k2 solution on the locus
    !!    wk4x      r.a.  public    o   x_cmp of vector k4 solution on the locus
    !!    wk4y      r.a.  public    o   y_cmp of vector k4 solution on the locus
    !!    ds        r.a.  public    o   element length along the locus
    !!                                  (npts*ds circles the whole locus)
    !!    ----------------------------------------------------------------
    !!
    !! 4. subroutines used :
    !!
    !!     name      type  module   description
    !!    ----------------------------------------------------------------
    !!    ----------------------------------------------------------------
    !!
    !! 5. called by :
    !!
    !!     name      type  module   description
    !!    ----------------------------------------------------------------
    !!     gridsetr  subr. w3snl4md setup geometric integration grid
    !!    ----------------------------------------------------------------
    !!
    !! 6. error messages :
    !!
    !!      none.
    !!
    !! 7. remarks :
    !!
    !! 8. structure :
    !!
    !!    see source code.
    !!
    !! 9. switches :
    !!
    !!    !/s  enable subroutine tracing.
    !!
    !!10. source code :
    !!
    !!    --------------------------------------------------------------- &
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ----------------------------------------------------------------72
    !!    ==================================================================
    !!
    !!
    !!wvn
    !wvn  use w3dispmd, only: wavnu2
    !!wvn---
    !!
    implicit none
    !!
    !!    parameter list
    !!    --------------
    real,    intent(in)  :: dep
    real,    intent(in)  :: wk1x,wk1y, wk3x,wk3y
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ------------------------------------------------------------------
    !!
    !!
    !!    local parameters & variables
    !!    -----------------------------
    integer              :: m, n
    real                 :: g
    real                 :: wk1, f1, fx
    real                 :: wkx, db, px, py, p, thp, halfp
    real                 :: a, b, dth, a_halfp
    !a    real                 :: wkfnc      !* real function or use wavnu2
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ---------------------::-----------------------------------------72
    !!    ##################################################################
    !!------------------------------------------------------------------------------
    !!==============================================================================
    !!
    !!
    !!    initial constants
    !!    ------------------
    g     = 9.806                            !* set = grav as in constants
    !!
    !!ini
    !!    initial all returned arrays before they are computed
    !!    -----------------------------------------------------
    wk2x(:) = 0.0
    wk2y(:) = 0.0
    wk4x(:) = 0.0
    wk4y(:) = 0.0
    ds(:)   = 0.0
    !!ini---
    !!    ------------------------------------------------------------------
    !!
    !!wkx bash; try use wkx = wka(nrng) instead of  wkx = wkfnc(4.*f1,dep)
    !b    wk1   = sqrt(wk1x**2+wk1y**2)           !* k1=wk1x since wk1y=0.0
    wk1   = wk1x
    f1    = sqrt(g*wk1*tanh(wk1*dep))/twopi !* f1=f(k1,dep)
    fx    = 4. * f1                         !* fx = 4*f1
    !!
    !!wvn bash; try use subr wavnu2 instead of function wkfnc
    !wvn  call wavnu2(twopi*fx,dep,wkx,cgx)       !* => wkx=k(4*f1,dep) & cgx=cg(4*f1,dep)
    wkx   = wkfnc(fx,dep)                   !* +> wkx=k(4*f1,dep)  (cgx not needed)
    !!wvn---
    !!    ------------------------------------------------------------------
    !!
    db    = wkx/float(np2p1-1)              !* locus length increment
    !!
    !!
    px    = wk3x - wk1x
    py    = wk3y - wk1y                     !* wk1y=0.0 can be omitted
    !b    p     = sqrt(px**2 + py**2)             !* argument never = 0.0
    p     = sqrt(px*px + py*py)
    thp   = atan2(py,px)
    halfp = 0.5*p
    !!
    !!
    do n=np2p1,npts                      !* for npts = 30
      !!                                            !* n = 16 --> 30
      !!
      !b       b   = 0.5 * db  + float(n-np2p1) * db
      b   = db * (0.5 + float(n-np2p1))
      !b       a   = sqrt(1. + (2.*b/p)**2)
      a   = sqrt(1. + (2.*b/p)*(2.*b/p))
      dth = acos(1./a)
      a_halfp = a*halfp
      !!
      !b       wk2x(n) = a*halfp * cos(thp+dth)
      !b       wk2y(n) = a*halfp * sin(thp+dth)
      wk2x(n) = a_halfp * cos(thp+dth)
      wk2y(n) = a_halfp * sin(thp+dth)
      wk4x(n) = wk2x(n) - px
      wk4y(n) = wk2y(n) - py
      ds(n)   = db
      !!
      m       = npts - n + 1                !* m = 15 --> 1
      !b       wk2x(m) = a*halfp * cos(thp-dth)
      !b       wk2y(m) = a*halfp * sin(thp-dth)
      wk2x(m) = a_halfp * cos(thp-dth)
      wk2y(m) = a_halfp * sin(thp-dth)
      wk4x(m) = wk2x(m) - px
      wk4y(m) = wk2y(m) - py
      ds(m)   = db
      !!
    end do !  do n=np2p1,npts
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    return
    !!
  end subroutine shloxr
  !!
  !!==============================================================================
  !!
  !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !>
  !> @brief n/a
  !>
  !> @verbatim
  !>  with wavenumber vector n identified by (wknx,wkny), its magnitude
  !>  given by wkn = sqrt(wknx**2+wkny**2) and its associated radian
  !>  frequency given by sign = sqrt[g*wkn*tanh(wkn*dep)], where g is
  !>  gravitational acceleration and dep is water depth, the four-wave
  !>  resonance condition is satisfied along a locus of pts defined by
  !>
  !>  [1]  (wk1x,wk1y) + (wk2x,wk2y) - (wk3x,wk3y) - (wk4x,wk4y) = 0
  !>
  !>  [2]  sig1 + sig2 - sig3 - sig4 = 0
  !>
  !>  because of the influence of depth, it is convenient to define new
  !>  vectors (wnx,wny) = (wknx*dep,wkny*dep) with magnitudes wn =
  !>  sqrt(wnx**2+wny**2) = wkn*dep such that a dimensionless frequency
  !>  is sign*sqrt(dep/g) = sqrt[wkn*dep*tanh(wkn*dep)]
  !>                      = sqrt[wn*tanh(wn)].
  !>  with these definitions and vectors (wk1x,wk1y) and (wk3x,wk3y)
  !>  given as input, we can write (with some rearrangement
  !>  of [1] and [2])
  !>
  !>  [3]  w3x - w1x = px = w2x - w4x
  !>  [4]  w3y - w1y = py = w2y - w4y
  !>  [5]  sqrt[w3*tanh(w3)] - sqrt[w1*tanh(w1)] = q
  !>       = sqrt[w2*tanh(w2)] - sqrt[w4*tanh(w4)]
  !>
  !>  with dimensionless vector (px,py) = (w3x-w1x,w3y-w1y) [magnitude
  !>  p = sqrt(px**2+py**2), direction atan2(py,px)] and dimensionless
  !>  frequency difference q = sqrt(w3*tanh(w3)] - sqrt(w1*tanh(w1)]
  !>  defined by input parameters, we see from [3] and [4] that
  !>  (w4x,w4y) = (w2x-px,w2y-py) [magnitude w4 = sqrt((w2x-px)**2 +
  !>  (w2y-py)**2)] and thus from [5] we must basically find elements
  !>  w2x and w2y that satisfy
  !>
  !>  [6] sqrt[sqrt(w2x**2+w2y**2)*tanh(sqrt(w2x**2+w2y**2))] -
  !>      sqrt[sqrt((w2x-px)**2+(w2y-py)**2) *
  !>           tanh(sqrt((w2x-px)**2+(w2y-py)**2] = q
  !>
  !>  the locus curve defined by the set of pts (w2x,w2y) crosses the
  !>  p-vector axis at two points; one with magnitude w2=rmin*p with
  !>  0.5 < rmin < 1.0 and one with magnitude w2=rmax*p with rmax > 1.
  !>  we first isolate rmin, rmax using various iterative algorithms,
  !>  and then find locus pts that are not on the p-vector axis with
  !>  another iterative scheme.  at the end, we un-normalize the w2
  !>  and w4 vectors to find the wk2 and wk4 vectors.
  !> @endverbatim
  !>
  !> @param[in] dep
  !> @param[in] wk1x
  !> @param[in] wk1y
  !> @param[in] wk3x
  !> @param[in] wk3y
  !>
  !> @author bash toulany
  !> @author michael casey
  !> @author william perrie
  !> @date   12-apr-2016
  !>
  subroutine shlocr ( dep, wk1x,wk1y, wk3x,wk3y )
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii                 bio |
    !/                  |           bash toulany            |
    !/                  |           michael casey           |
    !/                  |           william perrie          |
    !/                  |                        fortran 90 |
    !/                  | last update :         12-apr-2016 |
    !/                  +-----------------------------------+
    !/
    !/    01-mar-2016 : origination.                        ( version 5.13 )
    !/
    !!    ------------------------------------------------------------------
    !!
    !!    it returns: wk2x, wk2y, wk4x, wk4y & ds all dim=(npts)
    !!
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    !!
    !! 1. purpose :
    !!
    !!    -----------------------------------------------------------------#
    !!                                                                     !
    !!    with wavenumber vector n identified by (wknx,wkny), its magnitude!
    !!    given by wkn = sqrt(wknx**2+wkny**2) and its associated radian   !
    !!    frequency given by sign = sqrt[g*wkn*tanh(wkn*dep)], where g is  !
    !!    gravitational acceleration and dep is water depth, the four-wave !
    !!    resonance condition is satisfied along a locus of pts defined by !
    !!                                                                     !
    !!    [1]  (wk1x,wk1y) + (wk2x,wk2y) - (wk3x,wk3y) - (wk4x,wk4y) = 0   !
    !!                                                                     !
    !!    [2]  sig1 + sig2 - sig3 - sig4 = 0                               !
    !!                                                                     !
    !!    because of the influence of depth, it is convenient to define new!
    !!    vectors (wnx,wny) = (wknx*dep,wkny*dep) with magnitudes wn =     !
    !!    sqrt(wnx**2+wny**2) = wkn*dep such that a dimensionless frequency!
    !!    is sign*sqrt(dep/g) = sqrt[wkn*dep*tanh(wkn*dep)]                !
    !!                        = sqrt[wn*tanh(wn)].                         !
    !!    with these definitions and vectors (wk1x,wk1y) and (wk3x,wk3y)   !
    !!    given as input, we can write (with some rearrangement            !
    !!    of [1] and [2])                                                  !
    !!                                                                     !
    !!    [3]  w3x - w1x = px = w2x - w4x                                  !
    !!    [4]  w3y - w1y = py = w2y - w4y                                  !
    !!    [5]  sqrt[w3*tanh(w3)] - sqrt[w1*tanh(w1)] = q                   !
    !!         = sqrt[w2*tanh(w2)] - sqrt[w4*tanh(w4)]                     !
    !!                                                                     !
    !!    with dimensionless vector (px,py) = (w3x-w1x,w3y-w1y) [magnitude !
    !!    p = sqrt(px**2+py**2), direction atan2(py,px)] and dimensionless !
    !!    frequency difference q = sqrt(w3*tanh(w3)] - sqrt(w1*tanh(w1)]   !
    !!    defined by input parameters, we see from [3] and [4] that        !
    !!    (w4x,w4y) = (w2x-px,w2y-py) [magnitude w4 = sqrt((w2x-px)**2 +   !
    !!    (w2y-py)**2)] and thus from [5] we must basically find elements  !
    !!    w2x and w2y that satisfy                                         !
    !!                                                                     !
    !!    [6] sqrt[sqrt(w2x**2+w2y**2)*tanh(sqrt(w2x**2+w2y**2))] -        !
    !!        sqrt[sqrt((w2x-px)**2+(w2y-py)**2) *                         !
    !!             tanh(sqrt((w2x-px)**2+(w2y-py)**2] = q                  !
    !!                                                                     !
    !!    the locus curve defined by the set of pts (w2x,w2y) crosses the  !
    !!    p-vector axis at two points; one with magnitude w2=rmin*p with   !
    !!    0.5 < rmin < 1.0 and one with magnitude w2=rmax*p with rmax > 1. !
    !!    we first isolate rmin, rmax using various iterative algorithms,  !
    !!    and then find locus pts that are not on the p-vector axis with   !
    !!    another iterative scheme.  at the end, we un-normalize the w2    !
    !!    and w4 vectors to find the wk2 and wk4 vectors.                  !
    !!    -----------------------------------------------------------------#
    !!
    !! 2. method :
    !!
    !! 3. parameters :
    !!
    !!    parameter list
    !!    ------------------------------------------------------------------
    !!    name     type   scope    i/o  description
    !!    ------------------------------------------------------------------
    !!    npts      int.  public    i   # of points on the locus
    !!    np2p1     int.  public    i   = npts/2 + 1
    !!    ----------------------------------------------------------------
    !!
    !!    *** arrays wk2x,wk2y, wk4x,wk4y & ds are related to locus solutioni
    !!        for given vectors k1 & k3        all have dim=(npts)
    !!    wk2x      r.a.  public    o   x_cmp of vector k2 solution on the locus
    !!    wk2y      r.a.  public    o   y_cmp of vector k2 solution on the locus
    !!    wk4x      r.a.  public    o   x_cmp of vector k4 solution on the locus
    !!    wk4y      r.a.  public    o   y_cmp of vector k4 solution on the locus
    !!    ds        r.a.  public    o   element length along the locus
    !!                                  (npts*ds circles the whole locus)
    !!    ----------------------------------------------------------------
    !!
    !! 4. subroutines used :
    !!
    !!     name      type  module   description
    !!    ----------------------------------------------------------------
    !!    ----------------------------------------------------------------
    !!
    !! 5. called by :
    !!
    !!     name      type  module   description
    !!    ----------------------------------------------------------------
    !!     gridsetr  subr. w3snl4md setup geometric integration grid
    !!    ----------------------------------------------------------------
    !!
    !! 6. error messages :
    !!
    !!      none.
    !!
    !! 7. remarks :
    !!
    !! 8. structure :
    !!
    !!    see source code.
    !!
    !! 9. switches :
    !!
    !!    !/s  enable subroutine tracing.
    !!
    !!10. source code :
    !!
    !!    --------------------------------------------------------------- &
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ----------------------------------------------------------------72
    !!    ==================================================================
    !!
    !!
    use w3servmd, only: extcde
    use w3odatmd, only: ndse
    !!
    !!
    implicit none
    !!
    !!    parameter list
    !!    --------------
    real,    intent(in)  :: dep
    real,    intent(in)  :: wk1x,wk1y, wk3x,wk3y
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ------------------------------------------------------------------
    !!
    !!
    !!    local parameters & variables
    !!    -----------------------------
    integer              :: n, np, nnp, nplace
    integer              :: ierr_gr
    !!
    real                 :: p,  px,  py,  q,    qrtp,  qsqp,        &
         dr, dth, thp, dphi, cphi,               &
         w1, w1x, w1y, wk1,  w3, w3x, w3y, wk3,  &
         rold, rold1, rold2, rnew, rnew1, rnew2, &
         pxod, pyod,  zpod,                      &
         t, t1, t2, t3, tm, tp, ds1, ds2,        &
         rmin, rmax, rcenter, rradius
    !!
    double precision     :: dbt3,dbt4,dbt5,dbt6, dbz, dbp, dbqrtp,  &
         cdthold, cdthnew, wate1, wate2
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ---------------------::-----------------------------------------72
    !!    ##################################################################
    !!------------------------------------------------------------------------------
    !!==============================================================================
    !!
    !!
    !!ini
    !!    initial all returned arrays before they are computed
    !!    -----------------------------------------------------
    wk2x(:) = 0.0
    wk2y(:) = 0.0
    wk4x(:) = 0.0
    wk4y(:) = 0.0
    ds(:)   = 0.0
    !!ini---
    !!    ------------------------------------------------------------------
    !!
    !!
    !b    wk1   = sqrt(wk1x**2 + wk1y**2)         !* k1=wk1x since wk1y=0.0
    wk1   = wk1x
    !b    wk3   = sqrt(wk3x**2 + wk3y**2)
    wk3   = sqrt(wk3x*wk3x + wk3y*wk3y)
    !!
    w1    = wk1  * dep
    w1x   = wk1x * dep
    !b    w1y   = wk1y * dep
    w1y   = wk1y                            !* wk1y=0.0
    !!
    w3    = wk3  * dep
    w3x   = wk3x * dep
    w3y   = wk3y * dep
    !!
    px    = w3x - w1x
    py    = w3y - w1y
    !b    p     = sqrt(px**2 + py**2)             !* argument never = 0.0
    p     = sqrt(px*px + py*py)             !* argument never = 0.0
    thp   = atan2(py,px)
    q     = sqrt(w3*tanh(w3)) - sqrt(w1*tanh(w1))
    qrtp  = q / sqrt(p)
    qsqp  = qrtp*qrtp
    !!
    !!
    !!    -----------------------------------------------------------------#
    !!                                                                     !
    !!    for (w2x,w2y) = rmin*(px,py) (locus crossing the p-vector axis   !
    !!    nearest the origin), we have (w4x,w4y) = (w2x,w2y) - (px,py)     !
    !!    = rmin*(px,py) - (px,py) = (rmin - 1)*(px,py); note that because !
    !!    rmin < 1, the length of (w4x,w4y) is w4 = (1 - rmin)*p; then [6] !
    !!    takes the simpler form                                           !
    !!                                                                     !
    !!    [7] sqrt(rmin*p*tanh(rmin*p)) - sqrt[(1-rmin)*p*tanh((1-rmin)*p)]!
    !!        = q                                                          !
    !!                                                                     !
    !!    assuming the tanh() functions are slowly varying and can be      !
    !!    treated as separate entities, [7] can be written as a quadratic  !
    !!    in sqrt(rmin), i.e.,                                             !
    !!                                                                     !
    !!               2*qrtp*sqrt(rmin*p)                                   !
    !!    [8] rmin - ------------------- sqrt(rmin) +                      !
    !!                        t                                            !
    !!                                                                     !
    !!                                   qsqp-p*tanh((1-rmin)*p)           !
    !!                                   -----------------------  =  0,    !
    !!                                              t                      !
    !!                                                                     !
    !!    where   t    = tanh(rmin*p) + tanh((1-rmin)*p),                  !
    !!            qrtp = q/sqrt(p)  and qsqp = qrtp**2                     !
    !!                                                                     !
    !!    the square of the most positive root of [8] can (with some       !
    !!    algebra) be written                                              !
    !!                                                                     !
    !!    [9] rmin =                                                       !
    !!   (1/t**2)*{qsqp*[tanh(rmin*p)-tanh((1-rmin)*p)]+t*tanh((1-rmin)*p) !
    !!           +2*qrtp*sqrt[tanh(rmin*p)*tanh((1-rmin)*p)]*sqrt(t-qsqp)} !
    !!                                                                     !
    !!    setting rnew=rmin on the lhs and rold=rmin on the rhs in all     !
    !!    instances allows the creation of an iterative algorithm for rmin;!
    !!    convergence can be slow in general, so a coarse search for the   !
    !!    crossing of [9] with the rnew=rold line is conducted first, then !
    !!    a weighted iterative replacement loop is executed until the      !
    !!    desired accuracy is achieved;                                    !
    !!    note that if p is sufficiently large, all tanh() -> 1            !
    !!    and [9] becomes the analytic expression                          !
    !!            rmin = 0.5*[1 + qrtp*sqrt(2-qsqp)]                       !
    !!                                                                     !
    !!    following is the coarse search using rold1 = 0.5,0.1,1.0,        !
    !!                                         rold2 = rold1 + 0.1:        !
    !!                                                                     !
    !!    -----------------------------------------------------------------#
    !!
    !!
    ierr_gr = 0
    !!
    rold1 = 0.5
    tp    = tanh(rold1 * p)
    tm    = tanh((1.-rold1) * p)
    t     = tp + tm
    t1    = qsqp * (tp-tm)
    t2    = t  * tm
    t3    = 2. * qrtp * sqrt(tp*tm) * sqrt(t-qsqp)
    rnew1 = (t1 + t2 + t3) / (t*t)
    !!
    !!
    do n=1,4
      rold2 = rold1 + 0.1
      tp    = tanh(rold2 * p)
      tm    = tanh((1.-rold2) * p)
      t     = tp + tm
      t1    = qsqp * (tp-tm)
      t2    = t  * tm
      t3    = 2. * qrtp * sqrt(tp*tm) * sqrt(t-qsqp)
      rnew2 = (t1 + t2 + t3) / (t*t)
      if ( rnew2 .lt. rold2 ) then
        rold = (rold2*rnew1-rold1*rnew2)/(rold2-rold1-rnew2+rnew1)
        go to 11
      end if
      rold1 = rold2
      rnew1 = rnew2
    end do  ! do n=1,4
    rold = 0.9                       !* default if not otherwise found
11  continue
    !!    ------------------------------------------------------------------
    !!
    !!
    !!    iterative replacement search for rmin
    do n=1,50
      tp   = tanh(rold * p)
      tm   = tanh((1.-rold) * p)
      t    = tp + tm
      t1   = qsqp * (tp-tm)
      t2   = t  * tm
      t3   = 2. * qrtp*sqrt(tp*tm)*sqrt(t-qsqp)
      rnew = (t1 + t2 + t3) / (t*t)
      if ( abs(rnew-rold) .lt. 0.00001 ) then
        rmin = rnew
        go to 21
      end if
      rold = 0.5 * (rold + rnew)
    end do
    ierr_gr = ierr_gr + 1  !* set 1's flag in ierr_gr if no convergence
    rmin = rnew
21  continue
    !!    ------------------------------------------------------------------
    !!
    !!    set (dimensional) wavenumber components for this point on locus
    wk2x(1) =  rmin * px / dep
    wk2y(1) =  rmin * py / dep
    wk4x(1) = (rmin-1.) * px / dep
    wk4y(1) = (rmin-1.) * py / dep
    !!
    !!
    !!    -----------------------------------------------------------------#
    !!                                                                     !
    !!    for (w2x,w2y) = rmax*(px,py) (locus crossing the p-vector axis   !
    !!    farthest from the origin), we have (w4x,w4y)=(w2x,w2y) - (px,py) !
    !!    = rmax*(px,py) - (px,py) = (rmax - 1)*(px,py);                   !
    !!    here, because rmax > 1, the length of (w4x,w4y) is               !
    !!    w4 = (rmax - 1)*p; then [6] takes the form                       !
    !!                                                                     !
    !!    [10] sqrt(rmax*p*tanh(rmax*p))-sqrt[(rmax-1)*p*tanh((rmax-1)*p)] !
    !!         = q                                                         !
    !!                                                                     !
    !!    rearranging terms, squaring both sides and again rearranging     !
    !!    terms yields                                                     !
    !!                                                                     !
    !!    [11] rmax*p*[tanh(rmax*p) - tanh((rmax-1)*p)]                    !
    !!         = 2*q*sqrt(tanh(rmax*p))*sqrt(rmax*p) +                     !
    !!           q**2 + p*tanh((rmax-1)*p)                                 !
    !!                                                                     !
    !!    because the difference of the two tanh()'s on the lhs tend to    !
    !!    make the whole term small, we solve for rmax from the rapidly    !
    !!    varying part of the first term on the rhs, i.e.,                 !
    !!                                                                     !
    !!                         [tanh((rmax-1)*p)+qsqp + rmax*t]**2         !
    !!    [12]          rmax = ----------------------------------- ,       !
    !!                                 4*qsqp*tanh(rmax*p)                 !
    !!                                                                     !
    !!    where, in this algorithm, t = tanh(rmax*p) - tanh((rmax-1)*p);   !
    !!    as for rmin in [9], setting rnew=rmax on the lhs and rold=rmax   !
    !!    in all instances on the rhs allows the formation of an iterative !
    !!    algorithm; initially, we only know rmax > 1 so we do a coarse    !
    !!    search in the 10's place out to some reasonably big number to    !
    !!    try to find the place where [12] crosses the rnew = rold line    !
    !!    (if this fails, we set an error flag); in refining the estimate, !
    !!    it appears that [12] can get a little squirrely, so we do a      !
    !!    brute force successive decimation search to nplace decimal places!
    !!    to home in on the answer; note that if p is big enough for the   !
    !!    tanh()'s to reach unity, [12] becomes exact and                  !
    !!    rmax = [(1 + qsqp)**2]/(4*qsqp)                                  !
    !!    following is the coarse search with rold = 1,10,2001:            !
    !!                                                                     !
    !!    -----------------------------------------------------------------#
    !!
    rold = 1.0
    do n=1,200
      rold = rold + 10.
      tp   = tanh(rold * p)
      tm   = tanh((rold-1.) * p)
      t    = tp - tm
      t1   = tm + qsqp
      t2   = 4. * tp * qsqp
      rnew = ((t1+rold*t)**2) / t2
      if ( rnew .lt. rold ) then
        rold = rold - 10.
        go to 31
      end if
    end do
    ierr_gr = ierr_gr + 10    !* set 10's place in ierr_gr if no sol'n
31  continue
    !!    ------------------------------------------------------------------
    !!
    !!
    !!    successive decimation search to refine rmax
    dr = 10.
    do nplace=1,6
      dr = dr/10.
      do n=1,10
        rold = rold + dr
        tp   = tanh(rold * p)
        tm   = tanh((rold-1.) * p)
        t    = tp - tm
        t1   = tm + qsqp
        t2   = 4. * tp * qsqp
        rnew = ((t1+rold*t)**2) / t2
        if ( rnew .lt. rold ) then
          rold = rold - dr
          go to 51
        end if
      end do
51    continue
    end do
    !!
    rmax = rold
    !!
    !!    set (dimensional) wavenumber components for this locus point
    !!
    wk2x(np2p1) =  rmax * px / dep
    wk2y(np2p1) =  rmax * py / dep
    wk4x(np2p1) = (rmax-1.) * px / dep
    wk4y(np2p1) = (rmax-1.) * py / dep
    !!
    !!
    !!    -----------------------------------------------------------------#
    !!                                                                     !
    !!    search for cos(dth) for off-p-vector solutions; use a circle     !
    !!    centered on the p-vector axis at a distance                      !
    !!    rcenter = 0.5*(rmax+rmin)  from the  origin with a               !
    !!    radius  = 0.5*(rmax-rmin); radii from the center of the circle   !
    !!    at successive angle increments np*dphi intersect the circle at   !
    !!    distances r*p from the origin of the p vector such that          !
    !!                                                                     !
    !!    [13]  r**2 = rradius**2 + rcenter**2 -                           !
    !!                 2*rcenter*rradius*cos(np*dphi)                      !
    !!                                                                     !
    !!    and makes an angle dth with the p vector satisfying              !
    !!                                                                     !
    !!    [14]   cdth = cos(dth) = (rcenter/r) - (rradius/r)*cos(np*dphi)  !
    !!                                                                     !
    !!    we then rotate this vector, holding its length=r*p constant and  !
    !!    successively estimating cdth (using the above equation as an     !
    !!    initial guess) until it intersects the locus curve; some algebra !
    !!    yields the estimation equation as                                !
    !!                                                                     !
    !!                  r**2 + 1      [sqrt(r*tanh(rp)) - q/sqrt(p)]**4    !
    !!    [15] cdthnew= ------- - ---------------------------------------- !
    !!                    2*r     2*r*[tanh(p*sqrt(r**2-2*r*cdthold+1))]**2!
    !!                                                                     !
    !!    we use a weighted new estimate of cdthold with the weights based !
    !!    on the argument of the tanh() function in the denominator        !
    !!    (if the argument is big, tanh() -> 1 and cdthnew is found in one !
    !!    pass; for small arguments, convergence is faster with equal      !
    !!    weighting of old and new estimates; all this is empirical to try !
    !!    to increase speed); double precision is used to gain enough      !
    !!    accuracy when the arccos is taken; note that if p is big enough  !
    !!    for all tanh()'s -> 1, [15] is exact and                         !
    !!    cdthnew = cdth = [r**2 + 1 - (sqrt(r) - qrtp)**4]/(2*r)          !
    !!                                                                     !
    !!    -----------------------------------------------------------------#
    !!
    !!
    rcenter = 0.5 * (rmax + rmin)
    rradius = 0.5 * (rmax - rmin)
    t1      = rradius**2 + rcenter**2
    t2      = 2. * rradius * rcenter
    dphi    = 6.283185308 / float(npts)
    pxod    = px / dep
    pyod    = py / dep
    !!
    dbp     = dble(p)
    dbqrtp  = dble(qrtp)
    !!
    !!
    do np=2,npts/2                        !* np = 2 --> 15
      !!
      cphi    = cos(float(np-1)*dphi)
      dbz     = dsqrt(dble(t1-t2*cphi))
      cdthold = dble(rcenter-rradius*cphi) / dbz
      dbt3    = (dbz*dbz) + 1.d0
      dbt4    = dbt3 / (2.d0*dbz)
      dbt5    = ((dsqrt(dbz*dtanh(dbz*dbp))-dbqrtp)**4)/(2.d0*dbz)
      dbt6    = dbp * dsqrt(dbt3-2.d0*dbz*cdthold)
      !!
      if ( dbt6 .gt. 0.55d0 ) then
        wate1 = dtanh(dbt6)
        wate2 = 1.d0 - wate1
      else
        wate1 = 0.5d0
        wate2 = 0.5d0
      end if
      !!
      do  n=1,25
        cdthnew = dbt4 - dbt5 / ((dtanh(dbt6))**2)
        if ( dabs(cdthnew-cdthold) .lt. 0.0000001d0 ) go to 71
        cdthold = wate1 * cdthnew + wate2 * cdthold
        dbt6    = dbp * dsqrt(dbt3-2.d0*dbz*cdthold)
      end do
      ierr_gr = ierr_gr + 100   !* add to 100's place for every failure
71    continue
      !!
      dth  = sngl(dacos(cdthnew))
      zpod = sngl(dbz) * p / dep
      !!
      wk2x(np) = zpod * cos(thp+dth)
      wk2y(np) = zpod * sin(thp+dth)
      wk4x(np) = wk2x(np) - pxod
      wk4y(np) = wk2y(np) - pyod
      !!
      nnp = npts-np+2                        !* for npts = 30
      !!                                             !* nnp = 30 --> 17
      !!
      wk2x(nnp) = zpod * cos(thp-dth)
      wk2y(nnp) = zpod * sin(thp-dth)
      wk4x(nnp) = wk2x(nnp) - pxod
      wk4y(nnp) = wk2y(nnp) - pyod
      !!
    end do ! do np=2,npts/2
    !!    ------------------------------------------------------------------
    !!
    !!
    if ( ierr_gr .ne. 0 ) then
      write ( ndse,1000 ) ierr_gr
      call extcde ( 60 )
    endif
    !!    ------------------------------------------------------------------
    !!
    !!
    !!    set arc length ds as the sum of half the segment lengths on either
    !!    side of a given point
    !!
    ds1   = sqrt((wk2x(2)-wk2x(1))**2+(wk2y(2)-wk2y(1))**2)
    ds(1) = ds1
    do np=3,npts/2+1
      ds2   = sqrt((wk2x(np)-wk2x(np-1))**2+(wk2y(np)-wk2y(np-1))**2)
      ds(np-1)      = 0.5*(ds1+ds2)
      ds(npts-np+3) = ds(np-1)
      ds1           = ds2
    end do
    ds(npts/2+1)    = ds2
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    return
    !!
1000 format ( ' w3snl4 error : in shlocr. error from gridset ',i10)
    !!
  end subroutine shlocr
  !!
  !!==============================================================================
  !!
  !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !>
  !> @brief calculates four-wave boltzmann coupling coefficient in shallow
  !>  water.
  !>
  !> @details calculates four-wave boltzmann coupling coefficient in shallow
  !>  water given k1,k2,k3 and following at least hasselmann (1962)
  !>  and probably herterich and hasselmann (1982).  dimensional
  !>  wavenumbers are (wnx0,wny0), n = 1,3, h = depth, csq = coupling
  !>  coefficient.  this is the same as don's cplesh, except within
  !>  the algorithm, wavenumbers are made dimensionless with h and
  !>  frequencies with sqrt(h/g), g = gravitational acceleration (the
  !>  idea is to simplify and speed up the calculations while keeping
  !>  a reasonable machine resolution of the result).  at the end,
  !>  dimensionless csqhat is redimensioned as csq = csqhat/(h**6)
  !>  so it is returned as a dimensional entity.
  !>
  !>  this calculation can be a touchy bird, so we use double precision
  !>  for internal calculations, using single precision for input and
  !>  output.
  !>
  !> @param[in]  w1x0
  !> @param[in]  w1y0
  !> @param[in]  w2x0
  !> @param[in]  w2y0
  !> @param[in]  w3x0
  !> @param[in]  w3y0
  !> @param[in]  h
  !> @param[out] csq
  !> @param[in]  irng
  !> @param[in]  krng
  !> @param[in]  kang
  !> @param[in]  ipt
  !>
  !> @author bash toulany
  !> @author michael casey
  !> @author william perrie
  !> @date   12-apr-2016
  !>
  subroutine cplshr ( w1x0,w1y0, w2x0,w2y0, w3x0,w3y0,            &
       h, csq, irng,krng, kang,ipt )
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii                 bio |
    !/                  |           bash toulany            |
    !/                  |           michael casey           |
    !/                  |           william perrie          |
    !/                  |                        fortran 90 |
    !/                  | last update :         12-apr-2016 |
    !/                  +-----------------------------------+
    !/
    !/    01-mar-2016 : origination.                        ( version 5.13 )
    !/
    !!
    !!    it returns: the coupling coefficient  csq
    !!
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    !!
    !! 1. purpose :
    !!
    !!    -----------------------------------------------------------------#
    !!                                                                     !
    !!    calculates four-wave boltzmann coupling coefficient in shallow   !
    !!    water given k1,k2,k3 and following at least hasselmann (1962)    !
    !!    and probably herterich and hasselmann (1982).  dimensional       !
    !!    wavenumbers are (wnx0,wny0), n = 1,3, h = depth, csq = coupling  !
    !!    coefficient.  this is the same as don's cplesh, except within    !
    !!    the algorithm, wavenumbers are made dimensionless with h and     !
    !!    frequencies with sqrt(h/g), g = gravitational acceleration (the  !
    !!    idea is to simplify and speed up the calculations while keeping  !
    !!    a reasonable machine resolution of the result).  at the end,     !
    !!    dimensionless csqhat is redimensioned as csq = csqhat/(h**6)     !
    !!    so it is returned as a dimensional entity.                       !
    !!                                                                     !
    !!    this calculation can be a touchy bird, so we use double precision!
    !!    for internal calculations, using single precision for input and  !
    !!    output.                                                          !
    !!                                                                     !
    !!    -----------------------------------------------------------------#
    !!
    !! 2. method :
    !!
    !! 3. parameters :
    !!
    !!    parameter list
    !!    ------------------------------------------------------------------
    !!    name     type   scope    i/o  description
    !!    ------------------------------------------------------------------
    !!    ------------------------------------------------------------------
    !!
    !! 4. subroutines used :
    !!
    !!     name      type  module   description
    !!    ------------------------------------------------------------------
    !!    ------------------------------------------------------------------
    !!
    !! 5. called by :
    !!
    !!     name      type  module   description
    !!    ------------------------------------------------------------------
    !!     gridsetr  subr. w3snl4md setup geometric integration grid
    !!    ------------------------------------------------------------------
    !!
    !! 6. error messages :
    !!
    !!      none.
    !!
    !! 7. remarks :
    !!
    !! 8. structure :
    !!
    !!    see source code.
    !!
    !! 9. switches :
    !!
    !!    !/s  enable subroutine tracing.
    !!
    !!10. source code :
    !!
    !!    --------------------------------------------------------------- &
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ----------------------------------------------------------------72
    !!    ==================================================================
    !!
    !!
    implicit none
    !!
    !!    parameter list
    !!    --------------
    integer, intent(in)  :: irng,krng, kang,ipt
    real,    intent(in)  :: w1x0,w1y0, w2x0,w2y0, w3x0,w3y0
    real,    intent(in)  :: h          !* depth 'dep'
    real,    intent(out) :: csq
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ------------------------------------------------------------------
    !!
    !!
    !!    local parameters & variables
    !!    -----------------------------
    integer              :: ipass
    double precision     :: hh
    double precision     :: s1,    s2,    s3
    double precision     :: k1x,   k2x,   k3x
    double precision     :: k1y,   k2y,   k3y
    double precision     :: k1,    k2,    k3
    double precision     :: om1,   om2,   om3
    double precision     :: som1,  som2,  som3
    double precision     :: om1sq, om2sq, om3sq
    double precision     :: k23,   k23x,  k23y
    double precision     :: dot23, dot123
    double precision     :: omsq23
    !!mpc
    double precision     :: k1sq, k2sq, k3sq, k23sq
    double precision     :: tanh_k1, tanh_k2, tanh_k3, tanh_k23
    !!mpc---
    double precision     :: k1x0,  k2x0,  k3x0,  k1zx
    double precision     :: k1y0,  k2y0,  k3y0,  k1zy
    double precision     :: di,    e
    double precision     :: p1,    p2,    p3,    p4
    double precision     :: t1,    t2,    t3,    t4,    t5
    !!
    double precision     :: csqhatd, csqd
    double precision     :: scple
    double precision     :: pi4
    !!
    !!eps bash; added +eps to avoid dividing by 0.0. dividing by 0.0 causes nan
    !eps  double precision     :: eps
    !!
    !!    bash; added domsq23 = denominator of  t1      in cplshr
    !!          and   sumom   = denominator of  csqhatd in cplshr
    double precision     :: domsq23, sumom
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ---------------------::-----------------------------------------72
    !!    ##################################################################
    !!------------------------------------------------------------------------------
    !!==============================================================================
    !!
    !!
    !!    initial constants
    !!    ------------------
    hh    = dble(h)                     !* single to dbl precision
    pi4   = 0.785398175d0               !* set = pi/4 as in constants
    !eps  eps   = 1.d-12                      !* set eps to a very small number
    scple = 0.d0                        !* initialize accumulator
    !!ini
    !!    initialize returned variable 'csq'
    !!    ----------------------------------
    csq   = 0.d0
    !!ini---
    !!    ------------------------------------------------------------------
    !!
    do ipass=1,3
      !p1
      if (ipass .eq. 1) then            !* initial pass (+1,+1,-1)
        s1   =  1.d0
        s2   =  1.d0
        s3   = -1.d0
        k1x0 = dble(w1x0) * hh          !* norm. k elements with h
        k1y0 = dble(w1y0) * hh
        k2x0 = dble(w2x0) * hh
        k2y0 = dble(w2y0) * hh
        k3x0 = dble(w3x0) * hh
        k3y0 = dble(w3y0) * hh
        !p1
        !p2
      else if (ipass .eq. 2) then       !* 1st permutation (+1,-1,+1)
        s1   =  1.d0
        s2   = -1.d0
        s3   =  1.d0
        k1zx = k1x0
        k1zy = k1y0
        k1x0 = k2x0
        k1y0 = k2y0
        k2x0 = k3x0
        k2y0 = k3y0
        k3x0 = k1zx
        k3y0 = k1zy
        !p2
        !p3
      else                              !* 2nd permutation (-1,+1,+1)
        s1   = -1.d0
        s2   =  1.d0
        s3   =  1.d0
        k1zx = k1x0
        k1zy = k1y0
        k1x0 = k2x0
        k1y0 = k2y0
        k2x0 = k3x0
        k2y0 = k3y0
        k3x0 = k1zx
        k3y0 = k1zy
        !p3
      end if
      !!k19p1
      !!k19p1 note: na2p1=nang/2+1   !* this is the angle index opposite to iang=1
      !k19p1  if (krng.ne.irng .and. kang.eq.na2p1 .and. ipt.eq.1 .and.     &
      !k19p1                                        ipass.eq.1) go to 10
      !!k19p1---
      !!
      k1x = s1 * k1x0                    !* sign the norm'ed k parts
      k1y = s1 * k1y0
      k2x = s2 * k2x0
      k2y = s2 * k2y0
      k3x = s3 * k3x0
      k3y = s3 * k3y0
      !!mpc
      !mpc    k1    = dsqrt(k1x**2 + k1y**2)     !* normalized |k|
      !mpc    k2    = dsqrt(k2x**2 + k2y**2)
      !mpc    k3    = dsqrt(k3x**2 + k3y**2)
      !!mpc---
      k1sq  = (k1x*k1x + k1y*k1y)        !* normalized |k| **2
      k2sq  = (k2x*k2x + k2y*k2y)
      k3sq  = (k3x*k3x + k3y*k3y)
      k1    = dsqrt(k1sq)                !* normalized |k|
      k2    = dsqrt(k2sq)
      k3    = dsqrt(k3sq)
      !!mpc---
      !!
      !!mpc
      !mpc    om1   = dsqrt(k1*dtanh(k1))        !* norm. omega (by sqrt(h/g))
      !mpc    om2   = dsqrt(k2*dtanh(k2))
      !mpc    om3   = dsqrt(k3*dtanh(k3))
      !mpc    om1sq = om1**2
      !mpc    om2sq = om2**2
      !mpc    om3sq = om3**2
      !!mpc---
      tanh_k1 = dtanh(k1)
      tanh_k2 = dtanh(k2)
      tanh_k3 = dtanh(k3)
      om1sq   = k1*tanh_k1
      om2sq   = k2*tanh_k2
      om3sq   = k3*tanh_k3
      om1     = dsqrt(om1sq)             !* norm. omega (by sqrt(h/g))
      om2     = dsqrt(om2sq)
      om3     = dsqrt(om3sq)
      !!mpc---
      !!
      som1 = s1 * om1                    !* sign the norm'ed omega's
      som2 = s2 * om2
      som3 = s3 * om3
      !!      ----------------------------------------------------------------
      !!      ================================================================
      !!
      !!
      dot23  = k2x*k3x + k2y*k3y        !*  vector k2 dot vector k3
      !!
      k23x   = k2x + k3x                !* (vector k2  +  vector k3)_x
      k23y   = k2y + k3y                !* (vector k2  +  vector k3)_y
      !!
      !!mpc
      !mpc    k23    = dsqrt(k23x**2+k23y**2)   !* |vector k2  +  vector k3|
      !!mpc---
      k23sq  = (k23x*k23x + k23y*k23y)
      k23    = dsqrt(k23sq)             !* |vector k2  +  vector k3|
      !!mpc---
      !!
      !!mpc
      !mpc    omsq23 = k23 * dtanh(k23)         !* norm sq frq of v.k2+v.k3
      !!mpc---
      tanh_k23 = dtanh(k23)
      omsq23   = k23 * tanh_k23         !* norm sq frq of v.k2+v.k3
      !!mpc---
      !!
      dot123 = k1x*k23x + k1y*k23y      !* v.k1 dot (v.k2 + v.k3)
      !!      ----------------------------------------------------------------
      !!
      !!      note: the "i**2" factor from some reference is included in this term
      !!
      !!mpc
      !mpc    di = -(som2+som3)*(om2sq*om3sq-dot23)+0.5d0 *                 &
      !mpc          (som2*(k3**2-om3sq**2)+som3*(k2**2-om2sq**2))
      !!mpc---
      di = -(som2+som3)*(om2sq*om3sq-dot23)+0.5d0 *                 &
           (som2*(k3sq-om3sq*om3sq)+som3*(k2sq-om2sq*om2sq))
      !!mpc---
      !!
      e  = 0.5d0*(dot23-som2*som3*(om2sq+om3sq+som2*som3))
      !!
      p1 = 2.d0 * (som1+som2+som3) * (om1sq*omsq23 - dot123)
      !!
      !!mpc
      !mpc    p2 = -som1 * (k23**2 - omsq23**2)
      !mpc    p3 = -(som2+som3) * (k1**2 - om1sq**2)
      !mpc    p4 = k1**2 - om1sq**2
      !!mpc---
      !!      equation  p2 rewritten to preserve numerical precision
      !!      equations p3, p4 rearranged to avoid recomputations.
      p2 = -som1 * (k23sq*(1 - tanh_k23*tanh_k23))
      p4 = (k1sq*(1-tanh_k1*tanh_k1))
      p3 = -(som2+som3) * p4
      !!mpc---
      !!      ----------------------------------------------------------------
      !!
      !!      bash; added & used  variable domsq23 = denominator of t1
      domsq23 = omsq23 - ((som2+som3)**2)  !* bash; needed for test below
      !!      ----------------------------------------------------------------
      !!
      !!cp4   bash; with !cp4 on, test if ( domsq23 .eq. 0.d0 )
      !cp4    if ( domsq23 .eq. 0.d0 ) then     !* bash; this test was needed
      !!                                        !* when !k19p1 & !hv were off
      !!        domsq23=0.0 dividing by 0.0 causes nan; here we avoid it
      !cp4      t1 = 0.d0
      !eps      t1 = di * (p1+p2+p3) / (domsq23+eps)    !* add eps to denominator
      !!                                                !* and may be to numerator
      !cp4    endif
      !!cp4---
      !!      bash; with !cp4 off, don't test if ( domsq23 .eq. 0.d0 )
      !!      domsq23 is not = 0.0 (when !k19p1 & !hv were off)
      !b      t1 = di * (p1+p2+p3) / (omsq23 - ((som2+som3)**2))
      t1 = di * (p1+p2+p3) / (domsq23)
      !!cp4---
      !!      ----------------------------------------------------------------
      !!
      t2 = -di * som1 * (om1sq+omsq23)
      t3 = e * ((som1**3) * (som2+som3) - dot123 - p4)
      t4 = 0.5d0 * som1 * dot23 *                                   &
           ((som1+som2+som3) * (om2sq+om3sq) + som2*som3*(som2+som3))
      !!
      !!mpc
      !mpc    t5 = -0.5d0 * som1 *                                          &
      !mpc        (om2sq * (k3**2) * (som1+som2 + 2.d0 * som3)  +           &
      !mpc         om3sq * (k2**2) * (som1+som3 + 2.d0 * som2))
      !!mpc---
      t5 = -0.5d0 * som1 *                                          &
           (om2sq * (k3sq) * (som1+som2 + 2.d0 * som3)  +            &
           om3sq * (k2sq) * (som1+som3 + 2.d0 * som2))
      !!mpc---
      !!
      scple = scple + t1 + t2 + t3 + t4 + t5
      !!
    end do  ! do ipass=1,3
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    !!as  hh did division by 3 after adding 3 terms
    !as   scple = scple/3.d0
    !!as---
    !!
    !!    bash; added sumom   = denominator of  csqhatd in cplshr
    sumom = om1*om2*om3*(om2+om3-om1)
    !b    csqhatd = scple*scple*pi4/(om1*om2*om3*(om2+om3-om1))  !* bash; ok
    csqhatd = scple*scple*pi4/(sumom)
    !!    ------------------------------------------------------------------
    !!
    csqd    = csqhatd / (hh**6)
    csq     = sngl(csqd)                 !* from dbl to single precision
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    return
    !!
  end subroutine cplshr
  !!
  !!==============================================================================
  !!
  !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !>
  !> @brief splits the action density into two parts.
  !>
  !> @verbatim
  !>    (1) large-scale part dens1(nrng,nang)  and
  !>    (2) small-scale part dens2(nrng,nang)
  !>    dens1 & dens2 in polar action density (k,theta) space norm. (in k)
  !> @endverbatim
  !>
  !> @param[in] nrmn   number of first frequency bin in [1,nrng-1].
  !> @param[in] nrmx   number of last  frequency bin in [2,nrng].
  !> @param[in] npk    number of peak frequency  in [2,nrng-1].
  !> @param[in] fpk    peak frequency [hz] of initial frequency spectrum.
  !> @param[in] nbins  number of bins (see more below).
  !> @param[in] wka    wavenumbers array [1/m].
  !> @param[in] cga    group velocities array [m/s].
  !>
  !> @author bash toulany
  !> @author michael casey
  !> @author william perrie
  !> @date   12-apr-2016
  !>
  subroutine optsa2 ( nrmn,nrmx,    npk,fpk, nbins, wka, cga )
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii                 bio |
    !/                  |           bash toulany            |
    !/                  |           michael casey           |
    !/                  |           william perrie          |
    !/                  |                        fortran 90 |
    !/                  | last update :         12-apr-2016 |
    !/                  +-----------------------------------+
    !/
    !/    01-mar-2016 : origination.                        ( version 5.13 )
    !/
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    !!    ------------------------------------------------------------------
    !!
    !!    it returns variables dens1(nrng,nang) and dens2(nrng,nang)
    !!
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    !!
    !! 1. purpose :
    !!
    !!    splits the action density into two parts:
    !!    (1) large-scale part dens1(nrng,nang)  and
    !!    (2) small-scale part dens2(nrng,nang)
    !!    dens1 & dens2 in polar action density (k,theta) space norm. (in k)
    !!
    !! 2. method :
    !!
    !! 3. parameters :
    !!
    !!    parameter list
    !!    ------------------------------------------------------------------
    !!    name     type   scope    i/o  description
    !!    ------------------------------------------------------------------
    !!op2
    !!    nrmn      int.  local     i   number of first freq. bin in [1,nrng-1]
    !!    nrmx      int.  local     i   number of last  freq. bin in [2,nrng]
    !!    npk       int.  local     i   number of peak frequency  in [2,nrng-1]
    !!    nbins     int.  local     i   actual # of bins > npk  (incl. nfs)  or
    !!                                  actual # of bins > npk2 (incl. nrng)
    !!                                  to guarantee a min 1 bin in equi. range
    !!                                  (see subr. w3snl4)
    !!    ------------------------------------------------------------ !!op2
    !!
    !!    nrng      int.  public    i   # of freq. or rings
    !!    nang      int.  public    i   # of angles
    !!
    !!    dfrq      real  public    i   freq mult. for log freq spacing
    !!    fpk       real  public    i   peak freq. [hz] of initial freq spectrum
    !!    oma       r.a.  public    i   rel. freq. array (rad*hz) ----- dim=(nrng)
    !!    frqa      r.a.  public    i   radian frequencies (hz) ------- dim=(nrng)
    !!
    !!    ainc      real  public    i   angle increment (radians)
    !!    angl      r.a.  public    i   dir. array (rad) (full circle); dim=(nrng)
    !!    cosan     r.a.  public    i   cosine angles array ----------- dim=(nang)
    !!    sinan     r.a.  public    i   sine   angles array ----------- dim=(nang)
    !!    ------------------------------------------------------------------
    !!
    !!    wka       r.a.  local     i   wavenumbers array [1/m] ------- dim=(nrng)
    !!    cga       r.a.  local     i   group velocities array [m/s] -- dim=(nrng)
    !!                                  wka & cga arrays are corrsp. to depth 'dep'
    !!    ------------------------------------------------------------------
    !!
    !!    ef2       r.a.  public    i   2d energy density spectrum ef2(theta,f)
    !!                                  = a(theta,k)*2*pi*oma(f)/cga(f) dim=(nrng,nang)
    !!    ef1       r.a.  public    i   1d energy density spectrum ef1(f) dim=(nrng)
    !!    ------------------------------------------------------------------
    !!
    !!    dens1     r.a.  public    o   large-scale action density (k,theta)
    !!                                                     dim=(nrng,nang)
    !!    dens2     r.a.  public    o   small-scale action density (k,theta)
    !!                                                     dim=(nrng,nang)
    !!    ------------------------------------------------------------------
    !!
    !! 4. subroutines used :
    !!
    !!     name      type  module   description
    !!    ------------------------------------------------------------------
    !!    ------------------------------------------------------------------
    !!
    !! 5. called by :
    !!
    !!     name      type  module   description
    !!    ------------------------------------------------------------------
    !!     gridsetr  subr. w3snl4md setup geometric integration grid
    !!    ------------------------------------------------------------------
    !!
    !! 6. error messages :
    !!
    !!      none.
    !!
    !! 7. remarks :
    !!
    !! 8. structure :
    !!
    !!    see source code.
    !!
    !! 9. switches :
    !!
    !!    !/s  enable subroutine tracing.
    !!
    !!10. source code :
    !!
    !!    --------------------------------------------------------------- &
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ----------------------------------------------------------------72
    !!    ==================================================================
    !!
    !!
    !!
    implicit none
    !!
    !!
    !!
    !!    parameter list
    !!    --------------
    !!op2 bash; new for optsa2
    integer, intent(in)  :: nrmn, nrmx, nbins
    !!    ------------------------------------------------------------ !!op2
    !!
    integer, intent(in)  :: npk
    real,    intent(in)  :: fpk
    real,    intent(in)  :: wka(nrng),   cga(nrng)
    !!    ------------------------------------------------------------------
    !!
    !!
    !!    local parameters & variables
    !!    -----------------------------
    integer              :: irng, iang
    !!
    !!p2
    !!    bash; uses of original "psi2(:)", it was very bad (see below)
    !p2   integer              :: n1,  n2,  m,   mm
    !p2   integer              :: nn1, nn2, ii, idif
    !p2   real                 :: q(16)
    !p2   real                 :: emax
    !p2   real                 :: y, qmin, adif
    !!p2---
    !!
    !!p3
    !!    bash; this is an attempt to replace the original psi2(:)
    !!          with a distr. based on sin()**mm  with 'newmaxang'
    !!          - not good enough (see below)
    !!          !!p4 is an override of !!p3 with mm=4
    !p3   integer              :: n1,  n2,  m,   mm
    !p3   real                 :: q(16)
    !p3   real                 :: y, qmin, adif
    !!    the var. below are needed to find 'newmaxang' used in !p3 & !p4
    integer              :: maxang,    newmaxang
    integer              :: maxangshift
    integer              :: halfangl,  halfangu
    real                 :: ef2maxrow(nang)
    real                 :: ef2shift(nang)
    real                 :: halfmax
    !!p3---
    !!
    !!p4
    !!    bash; !!p4 is an override of !!p3 with mm=4
    integer              :: n1,  n2
    real                 :: q4
    !!p4---
    !!
    !!eq
    !!    bash; use variable equi. range suitable to tsa min condition
    !!          simplifed to one point equi. range nearest to 2.*fp
    !eq   integer              :: neq
    !eq   real                 :: fovfp
    !!eq---
    !!
    integer              :: igam
    real                 :: sum1, fac
    real                 :: beta, gam
    real                 :: fdenp, fr, ratio, z, ddd
    real                 :: sigz               !* sigz  = 0.109
    real                 :: fk(nrng),    fknrm(nrng)
    real                 :: bscl1(nrng), fkscl1(nrng)
    real                 :: psi2(nang)
    real                 :: act2d(nrng,nang)
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ---------------------::-----------------------------------------72
    !!    ##################################################################
    !!------------------------------------------------------------------------------
    !!==============================================================================
    !!
    !!
    !!ini
    !!    bash; initialize psi2() array here
    psi2(:)    = 0.0
    !!
    !!    initialize all the 1d & 2d arrays that are being used
    !!    and especially those that are being returned
    fk(:)      = 0.0
    fknrm(:)   = 0.0
    fkscl1(:)  = 0.0
    bscl1(:)   = 0.0
    act2d(:,:) = 0.0
    dens1(:,:) = 0.0
    dens2(:,:) = 0.0
    !!ini---
    !!    ------------------------------------------------------------------
    !!
    !!
    !!*   convert 2d energy density ef2(f,theta)
    !!         to 2d polar action density act2d(k,theta) norm. (in k)
    do irng=nrmn,nrmx
      fac   = cga(irng)/(twopi*oma(irng)*wka(irng))
      do iang=1,nang
        act2d(irng,iang) = ef2(irng,iang) * fac
      end do
      !!
      !!*     convert ef1(f) to fk(k); both are 1d energy density
      fk(irng)    = cga(irng)*ef1(irng)/twopi  !* fk(k) energy
      !!
      !!*     normalize the 1d wavenumber energy density fk(k) to give fknrm(k)
      fknrm(irng) = fk(irng)*wka(irng)**2.5    !* fknrm(k) = norm. fk(k)
    end do
    !!    ------------------------------------------------------------------
    !!
    !!
    !!    fit parameters to spectrum
    !!    --------------------------
    !!eq
    !eq   sum1 = 0.
    !eq   neq  = 0
    !eq   do 26 irng=nrmn,nrmx
    !eq     fovfp = frqa(irng)/fpk
    !!      bash; check2 test equilibrium range
    !b      if ( fovfp.ge.1.55.and.fovfp.le.2.45 ) then !* orig   equi range
    !b      if ( fovfp.ge.1.20.and.fovfp.le.2.20 ) then !* wide   equi range
    !b      if ( fovfp.ge.1.90.and.fovfp.le.2.20 ) then !* narrow equi range
    !!      --------------------------------------------------------------!*  <<<<<
    !!      bash; select variable equi. range suitable to tsa min condition
    !eq     if ( fovfp.ge.(dfrq**(nbins))-0.005 .and.                     &
    !eq          fovfp.le.(dfrq**(nbins))+0.005 ) then  !* narrow equi range  <<<<<
    !!      --------------------------------------------------------------!*  <<<<<
    !eq       sum1 = sum1 + fknrm(irng)
    !eq       neq  = neq + 1
    !eq     endif
    ! 26  end do
    !eq   beta = sum1 / neq
    !eq   gam  = fknrm(npk) / beta
    !!eq---
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    simplify
    beta = fknrm(npk+nbins)
    gam  = fknrm(npk) / beta
    !!eq---
    !!    ------------------------------------------------------------------
    !!
    do irng=nrmn,nrmx
      fknrm(irng) = fknrm(irng) / beta
    end do
    !!    ==================================================================
    !!
    !!
    !!p2
    !!    construct directional distribution "psi2(:)" - original option
    !!    ------------------------------------------------------------------
    !!
    !!    solve for normalizing coefficient for integral [1.0/(cos**m)]
    !!    note: n1, n2 spans half circle (from -pi/2 to +pi/2 going through 0.)
    !p2   n1 = -nang/4 + 1
    !p2   n2 =  nang/4 + 1
    !p2   do m=1,16
    !p2     sum1 = 0.
    !p2     do iang=n1,n2
    !p2       ii = iang
    !p2       if ( iang .lt. 1 ) ii = iang + nang
    !p2       sum1 = sum1 + cosan(ii)**m
    !       end do
    !p2     q(m) = 1./(sum1*ainc)
    !     end do
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!
    !!    find peak direction "maxang" in ef2() at "npk" the peak in ef1()
    !!    needed to define the energy spreading factor y=ef2(npk,maxang)/ef1(npk)
    !!    bash; note; this original "psi2(:)" was simply very bad because the drift
    !!          in "maxang" location causing the 2d snl to lose symmetry
    !p2   emax   = 0.
    !p2   maxang = 0
    !p2   do iang=1,nang
    !p2     if ( ef2(npk,iang).gt.emax ) then
    !p2       emax   = ef2(npk,iang)
    !p2       maxang = iang                          !* in [1,nang]
    !p2     endif
    !     end do
    !p2   y  = ef2(npk,maxang)/ef1(npk)              !* bash; energy spread
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!
    !!    compare value of peak with q-array for closest fit to cos()**m at peak
    !p2   mm   = 1
    !p2   qmin = abs(q(1)-y)
    !p2   do m=2,16
    !p2     adif = abs(q(m)-y)
    !p2     if ( adif.lt.qmin ) then
    !p2       qmin = adif
    !p2       mm   = m
    !p2     endif
    !     end do
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!
    !p2   nn1 = maxang - nang/4           !* nn1 in [-8, 27], -ve/+ve (incl. 0)
    !p2   nn2 = maxang + nang/4           !* nn2 in [10, 45], all +ve  (no  0)
    !p2   do iang=nn1,nn2              !* bash; nn1 -> nn2 covers half circle
    !p2     ii = iang                                !* ii always in range [1,nang]
    !p2     if ( ii .lt.    1 ) ii = ii + nang       !* ""
    !p2     if ( ii .gt. nang ) ii = ii - nang       !* ""
    !p2     idif = iabs(maxang-iang) + 1             !* =10,9,..,2,1,2,..,9,10
    !p2     psi2(ii) = q(mm) * cos(angl(idif))**mm   !* normalized psi2 distr.
    !     end do
    !!p2---
    !!    ==================================================================
    !!
    !!
    !!p3
    !!    construct new directional distribution "psi2(:)"
    !!    in an attempt to replace the original psi2(:) with
    !!    a distribution based on sin()**mm with 'newmaxang' - not good enough
    !!    ------------------------------------------------------------------
    !!
    !!    solve for normalizing coefficient for integral [1.0/(sin()**m)]
    !!    note: n1, n2 spans half circle (from 0 to +pi)
    !p3   n1 =  1
    !p3   n2 =  nang/2 + 1
    !p3   do m=1,16
    !p3     sum1 = 0.
    !p3     do iang=n1,n2
    !p3       sum1 = sum1 + sinan(iang)**m
    !       end do
    !p3     q(m) = 1./(sum1*ainc)
    !     end do
    !!p3---
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!
    !!p3
    ef2maxrow(:) = ef2(npk,:)
    maxang       = maxloc(ef2maxrow,1)
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!
    !!    shift the row so that the max is at location of 90 degress
    !!          negative shift is to the right, postive to the left
    !!          halfangl - lower angular limit of the half maximum
    !!          halfangu - upper angular limit of the half maximum
    ef2shift(:) = cshift( ef2maxrow(:), (maxang-1-nang/4) )
    halfangu    = nang/4+2
    halfangl    = nang/4
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!
    halfmax = 0.5 * ef2(npk,maxang)
    do while((ef2shift(halfangu).gt.halfmax).and.(halfangu.lt.nang/2))
      halfangu = halfangu + 1
    enddo
    do while((ef2shift(halfangl).gt.halfmax).and.(halfangl.gt.1))
      halfangl = halfangl - 1
    enddo
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!
    !!    convert angles indices with respect to peak
    !!       e.g. halfangl should go to halfangl - (nang/4+1)
    !!            halfangu should go to halfangu - (nang/4+1)
    halfangl = halfangl - (nang/4+1)
    halfangu = halfangu - (nang/4+1)
    !!
    !!    now average the positions, round to nearest integer.
    !!    -ve result means the centre is one greater than it should be.
    maxangshift = nint( 0.5 * (halfangl + halfangu) )
    newmaxang   = maxang + maxangshift
    if (newmaxang .lt.    1) newmaxang = newmaxang + nang
    if (newmaxang .gt. nang) newmaxang = newmaxang - nang
    !!p3---
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!
    !!p3
    !!    bash; need this section if you want to try sin()**mm with 'newmaxang'
    !p3   y = ef2(npk,newmaxang) / ef1(npk)          !* new energy spread
    !!
    !!    compare value of peak with q-array for closest fit to sin()**m at peak
    !!    this !p3 section is needed for use with  sin()**mm
    !!    bash; note; this new "psi2(:)" although better than original "psi2(:)"
    !!          it was still not good enough:  the 2d energy was ok but
    !!          the 2d snl, now with better symmetry, didn't always have the side lobes.
    !p3   mm   = 1
    !p3   qmin = abs(q(1)-y)
    !p3   do m=2,16
    !p3     adif = abs(q(m)-y)
    !p3     if ( adif.lt.qmin ) then
    !p3       qmin = adif
    !p3       mm   = m
    !p3     endif
    !     end do
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!
    !!    final step, use 'mm' for sin()**mm
    !p3   psi2(n1:n2) = (sinan(n1:n2))**mm           !* un-norm. psi2 distr.
    !p3   psi2(n1:n2) = q(mm) * psi2(n1:n2)          !* norm. psi2 distr.
    !!    rotate peak to correct angle
    !p3   psi2(:) = cshift( psi2(:), newmaxang-1+nang/4 )
    !!p3---
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    !!
    !!p4
    !!    !!p4 is an override of !!p3 with mm=4, so go straight to final step
    !!    note; all you need from !!p3 is the  "newmaxang"
    !!    so it's a sin()**4 distr. shifted to "newmaxang" - worked very well
    !!    ------------------------------------------------------------------
    !!
    !!    solve for normalizing coefficient for integral [1.0/(sin()**4)]
    !!    note: n1, n2 spans half circle (from 0 to +pi)
    n1 =  1
    n2 =  nang/2 + 1
    !p4   sum1 = 0.
    !p4   do iang=n1,n2
    !p4     sum1 = sum1 + sinan(iang)**4
    !     end do
    !p4   q4 = 1.0/(sum1*ainc)
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!
    !!    change the angles that aren't zero  (0 deg to +180 deg)
    psi2(n1:n2) = (sinan(n1:n2))**4                 !* un-norm. psi2 distr.
    q4          = 1.0/(sum(psi2(n1:n2))*ainc)
    psi2(n1:n2) = q4 * psi2(n1:n2)                  !* norm. psi2 distr.
    !!
    !!    rotate peak to correct angle
    psi2(:) = cshift( psi2(:), newmaxang-1+nang/4 )
    !!p4---
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    !!
    !!
    !!    estimate parametric spectrum and deviation from parametric spectrum
    !!    ------------------------------------------------------------------
    igam  = (gam-0.4)*10 + 0.5
    sigz  = 0.109
    gam   = igam/10. + 0.4
    fdenp = gam * beta / wka(npk)**2.5
    !!
    !!
    do irng=nrmn,nrmx
      fr = frqa(irng) / fpk
      if ( fr.le.1.0001 ) then
        if ( fr.ge.0.85 ) then
          ratio = 1.-(1.-fr)*0.7/0.15
        else
          ratio = 0.3*exp(-17.3*(0.85-fr))
        endif
        fkscl1(irng) = fdenp*ratio
        bscl1(irng)  = fkscl1(irng)/oma(irng)
      else
        z = 0.5*((fr-1.)/sigz)**1.2
        if ( z.gt.6. ) z = 6.
        ratio = 1.+exp(-z)*(gam-1.)
        fkscl1(irng) = beta*ratio/wka(irng)**2.5
        bscl1(irng)  = fkscl1(irng)/oma(irng)
      endif
      !!
      do iang=1,nang
        ddd = bscl1(irng) * psi2(iang) / wka(irng)  !* large-scale
        dens1(irng,iang) = ddd                      !* large-scale
        dens2(irng,iang) = act2d(irng,iang) - ddd   !* small-scale
      end do
    end do ! do irng=nrmn,nrmx
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    return
    !!
  end subroutine optsa2
  !!
  !!==============================================================================
  !!
  !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !>
  !> @brief for a given action density array dens1(k,theta), computes the
  !>  rate-of-change array sumint(k,theta) = dn(k,theta)/dt owing to
  !>  wave-wave interaction.
  !>
  !> @details for a given action density array dens1(k,theta), computes the
  !>  rate-of-change array sumint(k,theta) = dn(k,theta)/dt owing to
  !>  wave-wave interaction, as well as some ancillary arrays relating to
  !>  positive and negative fluxes and their integrals.
  !>
  !> @param[in] pha   pha = k*dk*dtheta.
  !> @param[in] ialt  integer switch.
  !>
  !> @author bash toulany
  !> @author michael casey
  !> @author william perrie
  !> @date   12-apr-2016
  !>
  subroutine snlr_fbi ( pha, ialt )
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii                 bio |
    !/                  |           bash toulany            |
    !/                  |           michael casey           |
    !/                  |           william perrie          |
    !/                  |                        fortran 90 |
    !/                  | last update :         12-apr-2016 |
    !/                  +-----------------------------------+
    !/
    !/    01-mar-2016 : origination.                        ( version 5.13 )
    !/
    !!    ------------------------------------------------------------------
    !!
    !!    it returns: fbi & diag2 all dim=(nrng,nang)
    !!
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    !!
    !! 1. purpose :
    !!
    !!    -----------------------------------------------------------------#
    !!                                                                     !
    !!    for a given action density array dens1(k,theta), computes the    !
    !!    rate-of-change array sumint(k,theta) = dn(k,theta)/dt owing to   !
    !!    wave-wave interaction, as well as some ancillary arrays          !
    !!    relating to positive and negative fluxes and their integrals.    !
    !!                                                                     !
    !!    -----------------------------------------------------------------#
    !!                                                                     !
    !!    compute:                                                         !
    !!    --------                                                         !
    !!    for both -tsa and -fbi                                           !
    !!    + sumint    contains scale 1 contibution for snl -tsa & snl -fbi !
    !!                                                                     !
    !!    for -tsa                                                         !
    !!    + sumintsa  contains tsa approximation to snl -tsa               !
    !!                                                                     !
    !!    for -fbi                                                         !
    !!    + sumintp   contains scale 2 contribution to snl -fbi            !
    !!    + sumintx   contains cross interactions between scales 1 and 2   !
    !!    -----------------------------------------------------------------#
    !!
    !! 2. method :
    !!
    !! 3. parameters :
    !!
    !!    parameter list
    !!    ------------------------------------------------------------------
    !!    name     type   scope    i/o  description
    !!    ------------------------------------------------------------------
    !!    nrng      int.  public    i   # of freq. or rings
    !!    nang      int.  public    i   # of angles
    !!    npts      int.  public    i   # of points on the locus
    !!    nzz       int.  public    i   linear irng x krng = (nk*(nk+1))/2
    !!    ialt      int.  public    i   integer switch ialt=2; do alternate
    !!                                                 ialt=1; do not alternate
    !!    kzone     int.  public    i   zone of influence = int(alog(4.0)/alog(dfrq))
    !!    na2p1     int.  public    i   = nang/2 + 1
    !!    np2p1     int.  public    i   = npts/2 + 1
    !!    dfrq      real  public    i   frequency multiplier for log freq. spacing
    !!    frqa      r.a.  public    i   radian frequencies (hz);   dim=(nrng)
    !!    pha       r.a.  local     i   pha = k*dk*dtheta      ;   dim=(nrng)
    !!    ------------------------------------------------------------------
    !!
    !!    *** the 11 grid integration geometry arrays at one given depth
    !!    *** from gridsetr.            dim=(npts,nang,nzz,ndep)
    !!    kref2     i.a.  public    i   index of reference wavenumber for k2
    !!    kref4     i.a.  public    i   idem for k4
    !!    jref2     i.a.  public    i   index of reference angle      for k2
    !!    jref4     i.a.  public    i   idem for k4
    !!    wtk2      r.a.  public    i   k2 interpolation weigth along wavenumbers
    !!    wtk4      r.a.  public    i   idem for k4
    !!    wta2      r.a.  public    i   k2 interpolation weigth along angles
    !!    wta4      r.a.  public    i   idem for k4
    !!    tfac2     r.a.  public    i   norm. for interp action density at k2
    !!    tfac4     r.a.  public    i   idem for k4
    !!    grad      r.a.  public    i   coupling and gradient term in integral
    !!                                  grad = c * h * g**2 * ds / |dw/dn|
    !!    ------------------------------------------------------------------
    !!
    !!    *** large & small scale action density from optsa dim=(nrng,nang)
    !!    dens1     r.a.  public    i   lrg-scl action density (k,theta);
    !!    dens2     r.a.  public    i   sml-scl action density (k,theta);
    !!    ------------------------------------------------------------------
    !!
    !!    for both -tsa and -fbi
    !!    sumint    r.a.  local     o   contains scale 1 contribution to snl
    !!                                                       dim=(nrng,nang)
    !!    for -tsa
    !!    sumintsa  r.a.  local     o   contains tsa approximation to snl -tsa
    !!                                                       dim=(nrng,nang)
    !!    for -fbi
    !!    sumintp   r.a.  local     o   contains scale 2 contribution to snl -fbi
    !!                                                       dim=(nrng,nang)
    !!    sumintx   r.a.  local     o   contains cross interactions " "   "  -fbi
    !!                                                       dim=(nrng,nang)
    !!    ------------------------------------------------------------------
    !!
    !!    for -tsa; the 2 returned arrays tsa & diag dim=(nrng,nang)
    !!    tsa       r.a.  public    o   snl-tsa = sumint + sumintsa
    !!    diag      r.a.  public    o   snl-tsa diagonal term = [dn/dn1]
    !!    ------------------------------------------------------------------
    !!
    !!    for -fbi; the 2 returned arrays fbi & diag2 dim=(nrng,nang)
    !!    fbi       r.a.  public    o   snl-fbi = sumint + sumintp  + sumintx
    !!    diag2     r.a.  public    o   snl-fbi diagonal term = [dn/dn1]
    !!    ------------------------------------------------------------------
    !!
    !! 4. subroutines used :
    !!
    !!     name      type  module   description
    !!    ----------------------------------------------------------------
    !!    ----------------------------------------------------------------
    !!
    !! 5. called by :
    !!
    !!     name      type  module   description
    !!    ----------------------------------------------------------------
    !!     gridsetr  subr. w3snl4md setup geometric integration grid
    !!    ----------------------------------------------------------------
    !!
    !! 6. error messages :
    !!
    !!      none.
    !!
    !! 7. remarks :
    !!
    !! 8. structure :
    !!
    !!    see source code.
    !!
    !! 9. switches :
    !!
    !!    !/s  enable subroutine tracing.
    !!
    !!10. source code :
    !!
    !!    --------------------------------------------------------------- &
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ----------------------------------------------------------------72
    !!    ==================================================================
    !!
    !!
    implicit none
    !!
    !!    parameter list
    !!    --------------
    real,    intent(in)  :: pha(nrng)
    integer, intent(in)  :: ialt
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ------------------------------------------------------------------
    !!
    !!
    !!    local parameters & variables
    !!    -----------------------------
    !!    for both -tsa and -fbi
    integer              :: irng,krng, iang,kang
    integer              :: ipt, iizz, izz
    integer              :: kmax
    integer              :: ia2, ia2p, k2, k2p
    integer              :: ia4, ia4p, k4, k4p
    integer              :: nref
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!
    !!    for -tsa
    !tsa  integer              :: nklimit, nalimit
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!
    !!    for both -tsa and -fbi
    real                 :: d1,   d3,   d2,   d4
    real                 :: dp1,  dp3
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!
    !!    for both -tsa and -fbi
    !!    but for -tsa they are being calc. inside  if/endif if test is successful
    !!    and for -fbi they are being calc. outside if/endif always
    real                 :: dz4,  dz5
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!
    !!    for both -tsa and -fbi
    real                 :: dx13, ds13, dxp13, dsp13
    real                 :: dgm,  t31,  tr31
    real                 :: w2,   w2p,  wa2,  wa2p,  d2a,  d2b,  tt2
    real                 :: w4,   w4p,  wa4,  wa4p,  d4a,  d4b,  tt4
    real                 :: sumint(nrng,nang)
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!
    !!    for -tsa
    !tsa  real                 :: dz2a, dz3a,  ttsa,   trtsa
    !tsa  real                 :: ddn1, ddn3,  diagk1, diagk3
    !tsa  real                 :: sumintsa(nrng,nang)
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!
    !!    for -fbi
    real                 :: dp2,   dp4
    real                 :: d2pa,  d4pa
    real                 :: d2pb,  d4pb
    real                 :: dz1,   dz2,   dz3,   dz6,   dz7,   dz8
    real                 :: dgmp,  tp31,  trp31, dzsum, txp31, trx31
    !!
    !!    for -fbi; bash added 4 new terms for a full expression of diag2 term
    real                 :: ddp1,  ddp2,  ddp3,  ddp4   !* ddpi=di+dpi for i=1,4
    real                 :: dd2n1, dd2n3, diag2k1, diag2k3
    real                 :: sumintp(nrng,nang)
    real                 :: sumintx(nrng,nang)
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ---------------------::-----------------------------------------72
    !!    ##################################################################
    !!------------------------------------------------------------------------------
    !!==============================================================================
    !!
    !!
    !!    for -tsa
    !!    bash; hardwire these two parameters
    !tsa  nklimit = 6
    !tsa  nalimit = 6
    !!
    !!
    !!ini
    !!    bash; move initialization of all returned arrays from below to here
    !!    ------------------------------------------------------------------
    !!    for both -tsa and -fbi
    !!    sumint is now initialized here instead of below!
    sumint(:,:)   = 0.0
    !!
    !!    for -tsa
    !!    sumintsa are now initialized here instead of below!
    !tsa  sumintsa(:,:) = 0.0
    !tsa  tsa(:,:)      = 0.0
    !tsa  diag(:,:)     = 0.0
    !!
    !!    for -fbi
    !!    sumintp and sumintx are now initialized here instead of below!
    sumintp(:,:)  = 0.0
    sumintx(:,:)  = 0.0
    fbi(:,:)      = 0.0
    diag2(:,:)    = 0.0
    !!ini---
    !!    ------------------------------------------------------------------
    !!    ------------------------------------------------------------------
    !!    ##################################################################
    !!
    !!
    !!    for -tsa
    !tsa  ddn1    = 0.0                           !* for -tsa diag  [dn/dn1]
    !tsa  ddn3    = 0.0                           !* for -tsa diag  [dn/dn3]
    !!
    !!    for -fbi
    dd2n1   = 0.0                           !* for -fbi diag2 [dn/dn1]
    dd2n3   = 0.0                           !* for -fbi diag2 [dn/dn3]
    !!
    !!
    !!
    !!50
    do 50 irng=1,nrng,ialt
      !!kz
      kmax = min(irng+kzone, nrng)   !* bash; sometimes a locus pt is outside nrng
      !kz     kmax = min(irng+kzone, nrng-1) !* bash; taking 1 out will not affect kzone, try it
      !!kz---
      !!
      iizz = (nrng-1)*(irng-1) - ((irng-2)*(irng-1))/2
      !!      ----------------------------------------------------------------
      !!
      !!
      !!60
      do 60 iang=1,nang,ialt
        !!
        !!        for both -tsa and -fbi
        d1   = dens1(irng,iang)
        dp1  = dens2(irng,iang)
        !!
        !!        for -fbi
        ddp1 = d1+dp1             !! for full expression of diag2 term
        !!
        !!70
        !!kz
        !kz       do 70 krng=irng,nrng
        do 70 krng=irng,kmax,ialt
          !!
          !!          for both -tsa and -fbi
          !!          bash; check5  be consistent with gridsetr
          !!          moved here from below (was after do 80 kang=1,nang)
          !!          and changed go to 80 into go to 70 (i.e. go to next krng)
          !kz         if ( frqa(krng)/frqa(irng) .gt. 4. ) go to 70  !* original gridsetr
          !kz         if ( frqa(krng)/frqa(irng) .gt. 3. ) go to 70  !* original snlr_'s
          !kz         if ( frqa(krng)/frqa(irng) .gt. 2. ) go to 70  !* bash; use .gt. 2
          !!kz---
          !!
          izz = krng + iizz
          !!          ------------------------------------------------------------
          !!
          !!80
          do 80 kang=1,nang,ialt
            !!
            !!            for both -tsa and -fbi
            !!ba1         bash; remove self interaction
            !!                  skip k1 but keep the opposite angle to k1 - original setting
            if ( krng.eq.irng ) then              !* wn3 = wn1
              if ( kang.eq.iang ) go to 80        !* th3 = th1
            endif
            !!ba1---
            !!            ----------------------------------------------------------
            !!
            !!            for both -tsa and -fbi
            d3   = dens1(krng,kang)
            dp3  = dens2(krng,kang)
            !!
            !!            for -fbi
            ddp3 = d3+dp3         !! for full expression of diag2 term
            !!
            !!
            !!            for both -tsa and -fbi
            nref = kang - iang + 1
            if ( nref .lt. 1 ) nref = nref + nang
            !!
            !!
            !!            for both -tsa and -fbi
            !!            bash; check5  be consistent with gridsetr
            !!                  and move this test above right after do 70 krng=irng,nrng
            !x            if ( frqa(krng)/frqa(irng) .gt. 4. ) go to 80  !* gridsetr
            !b            if ( frqa(krng)/frqa(irng) .gt. 3. ) go to 80  !* original
            !!
            !!
            !!            for both -tsa and -fbi
            t31     = 0.0             !* must be reset to 0.0
            !!
            !!            for -tsa
            !tsa          ttsa    = 0.0             !* must be reset to 0.0
            !tsa          diagk1  = 0.0             !* must be reset to 0.0
            !tsa          diagk3  = 0.0             !* must be reset to 0.0
            !!
            !!            for -fbi
            tp31    = 0.0             !* must be reset to 0.0
            txp31   = 0.0             !* must be reset to 0.0
            diag2k1 = 0.0             !* must be reset to 0.0
            diag2k3 = 0.0             !* must be reset to 0.0
            !!
            !!            for both -tsa and -fbi
            dx13  = d1*d3
            ds13  = d3-d1
            dxp13 = dp1*dp3
            dsp13 = dp3-dp1
            !!            ----------------------------------------------------------
            !!
            !!90
            do 90 ipt=1,npts
              !!
              !!              for both -tsa and -fbi
              !!              save time by skipping insignificant contributions
              !!e-30
              !e-30           if ( grad(ipt,nref,izz) .lt. 1.e-30 ) go to 90
              !!e-30---
              if ( grad(ipt,nref,izz) .lt. 1.e-15 ) go to 90
              !!e-30---
              !!              --------------------------------------------------------
              !!
              !!xlc1          bash; skip k1 but keep the opposite angle to k1 - original setting
              !xlc1           if ( kang.eq.iang ) then                     !* th3=+th1
              !xlc1             if (ipt.eq.1 .or. ipt.eq.np2p1) go to 90   !* skip x-axis loci
              !xlc1           end if
              !!xlc1---
              !!              --------------------------------------------------------
              !!
              !!
              !!2             estimation of density for wave #2
              !!
              !!              for both -tsa and -fbi
              k2  = kref2(ipt,nref,izz)
              k2p = k2 + 1
              w2  = wtk2(ipt,nref,izz)
              w2p = 1. - w2
              !!
              !!              for both -tsa and -fbi
              ia2 = iang + jref2(ipt,nref,izz)
              if ( ia2 .gt. nang )  ia2  = ia2  - nang
              !!
              !!              for both -tsa and -fbi
              ia2p = ia2 + 1
              if ( ia2p .gt. nang ) ia2p = ia2p - nang
              !!
              !!              for both -tsa and -fbi
              wa2  = wta2(ipt,nref,izz)
              wa2p = 1. - wa2
              d2a  = w2 * dens1(k2,ia2)  + w2p * dens1(k2p,ia2)
              d2b  = w2 * dens1(k2,ia2p) + w2p * dens1(k2p,ia2p)
              tt2  = tfac2(ipt,nref,izz)
              d2   = (wa2*d2a  + wa2p*d2b)  * tt2
              !!
              !!              for -fbi
              d2pa = w2 * dens2(k2,ia2)  + w2p * dens2(k2p,ia2)
              d2pb = w2 * dens2(k2,ia2p) + w2p * dens2(k2p,ia2p)
              !!
              !!              for -fbi
              dp2  = (wa2*d2pa + wa2p*d2pb) * tt2   !* for -fbi
              ddp2 = d2+dp2       !! for full expression of diag2 term
              !!              ========================================================
              !!
              !!
              !!4             estimation of density for wave #4
              !!
              !!              for both -tsa and -fbi
              k4  = kref4(ipt,nref,izz)
              k4p = k4 + 1
              w4  = wtk4(ipt,nref,izz)
              w4p = 1. - w4
              !!
              !!              for both -tsa and -fbi
              ia4 = iang + jref4(ipt,nref,izz)
              if ( ia4 .gt. nang )  ia4  = ia4  - nang
              !!
              !!              for both -tsa and -fbi
              ia4p= ia4 + 1
              if ( ia4p .gt. nang ) ia4p = ia4p - nang
              !!
              !!              for both -tsa and -fbi
              wa4  = wta4(ipt,nref,izz)
              wa4p = 1. - wa4
              d4a  = w4*dens1(k4,ia4)  + w4p*dens1(k4p,ia4)
              d4b  = w4*dens1(k4,ia4p) + w4p*dens1(k4p,ia4p)
              tt4  = tfac4(ipt,nref,izz)
              d4   = (wa4*d4a  + wa4p*d4b)  * tt4
              !!
              !!              for -fbi
              d4pa = w4*dens2(k4,ia4)  + w4p*dens2(k4p,ia4)
              d4pb = w4*dens2(k4,ia4p) + w4p*dens2(k4p,ia4p)
              !!
              !!              for -fbi
              dp4  = (wa4*d4pa + wa4p*d4pb) * tt4   !* for -fbi
              ddp4 = d4+dp4       !! for full expression of diag2 term
              !!              ========================================================
              !!
              !!
              !!              for both -tsa and -fbi
              dgm  = dx13*(d4-d2) + ds13*d4*d2        !* dgm=b of r&p'08 eqn(8)
              !!                                         !* represents broad scale interactions
              t31  = t31  + dgm  * grad(ipt,nref,izz)
              !!              --------------------------------------------------------
              !!
              !!              for -fbi
              dgmp = dxp13*(dp4-dp2) + dsp13*dp4*dp2  !* dgmp=l of r&p'08 eqn(8)
              !!                                          !* represents local scale interactions
              tp31 = tp31 + dgmp * grad(ipt,nref,izz)
              !!              --------------------------------------------------------
              !!              ========================================================
              !!
              !!
              !!              for -tsa : -diag
              !!              use this expression for the diagonal term
              !!              whose derivation neglect "dp2" & "dp4"
              !tsa            ddn1   = (d3+dp3)*(d4-d2) - d4*d2              !* dn/dn1
              !tsa            ddn3   = (d1+dp1)*(d4-d2) + d4*d2              !* dn/dn3
              !tsa            diagk1 = diagk1 + ddn1 * grad(ipt,nref,izz)
              !tsa            diagk3 = diagk3 + ddn3 * grad(ipt,nref,izz)
              !!              --------------------------------------------------------
              !!
              !!              for -fbi : -diag2
              !!              use the full expression for the diagonal terms
              !!              whose derivation keeps all large + small scale
              dd2n1   = ddp3*(ddp4-ddp2) - ddp4*ddp2         !* dn/dn1
              dd2n3   = ddp1*(ddp4-ddp2) + ddp4*ddp2         !* dn/dn3
              diag2k1 = diag2k1 + dd2n1 * grad(ipt,nref,izz)
              diag2k3 = diag2k3 + dd2n3 * grad(ipt,nref,izz)
              !!              --------------------------------------------------------
              !!              ========================================================
              !!
              !!
              !!              for -fbi
              dz1  = dx13    * (dp4-dp2)
              dz2  = d1*dp3  * ((d4-d2)+(dp4-dp2))
              dz3  = d3*dp1  * ((d4-d2)+(dp4-dp2))
              !!
              !!              for -fbi (calc. dz4 & dz5 here)
              dz4  = dxp13   * (d4-d2)
              dz5  = d2*d4   *  dsp13
              !!
              !!              for -tsa
              !!              cross-interactions between parametric and perturbation
              !!              that occur only when k3 is close enough to k1
              !!              bash; added an extra check on (nang-nalimit)
              !b              if ( iabs(irng-krng).lt.nklimit .and.                 &
              !b                   iabs(iang-kang).lt.nalimit )    then        !* original
              !!
              !tsa            if (     (krng-irng).lt.nklimit .and.                 &
              !tsa               ( iabs(kang-iang).lt.nalimit .or.                  &
              !tsa                 iabs(kang-iang).gt.(nang-nalimit) ) )  then !* bash
              !!
              !!                for -tsa (calc. dz4 & dz5 here)
              !tsa              dz4  = dxp13   * (d4-d2)
              !tsa              dz5  = d2*d4   *  dsp13
              !tsa              dz2a = d1*dp3 * (d4-d2)
              !tsa              dz3a = d3*dp1 * (d4-d2)
              !!
              !tsa              ttsa = ttsa + (dz4+dz5+dz2a+dz3a)*grad(ipt,nref,izz)
              !!
              !tsa            endif
              !!              --------------------------------------------------------
              !!
              !!              for -fbi
              dz6   = d2*dp4  * (ds13+dsp13)
              dz7   = d4*dp2  * (ds13+dsp13)
              dz8   = dp2*dp4 *  ds13
              dzsum = dz1 + dz2 + dz3 + dz4 + dz5 + dz6 + dz7 + dz8
              txp31 = txp31 + dzsum * grad(ipt,nref,izz)
              !!              --------------------------------------------------------
              !!              ========================================================
              !!
              !!
90          end do                        !* end of ipt (locus) loop
            !!            ----------------------------------------------------------
            !!
            !!
            !!            multiply the following components by factor 2. in here
            !!
            !!            for both -tsa and -fbi
            tr31  = 2. * t31
            !!
            !!            for -tsa
            !tsa          trtsa = 2. * ttsa
            !!
            !!            for -fbi
            trp31 = 2. * tp31
            trx31 = 2. * txp31
            !!
            !!            for -tsa : -diag
            !tsa          diagk1  = 2. * diagk1
            !tsa          diagk3  = 2. * diagk3
            !!
            !!            for -fbi : -diag2
            diag2k1 = 2. * diag2k1
            diag2k3 = 2. * diag2k3
            !!            ----------------------------------------------------------
            !!
            !!            for both -tsa and -fbi
            sumint(irng,iang)  = sumint(irng,iang)  + tr31*pha(krng)
            sumint(krng,kang)  = sumint(krng,kang)  - tr31*pha(irng)
            !!            ----------------------------------------------------------
            !!
            !!            for -tsa
            !tsa          sumintsa(irng,iang)= sumintsa(irng,iang)+ trtsa*pha(krng)
            !tsa          sumintsa(krng,kang)= sumintsa(krng,kang)- trtsa*pha(irng)
            !!            ----------------------------------------------------------
            !!
            !!            for -fbi
            sumintp(irng,iang) = sumintp(irng,iang) + trp31*pha(krng)
            sumintp(krng,kang) = sumintp(krng,kang) - trp31*pha(irng)
            !!
            !!            for -fbi
            sumintx(irng,iang) = sumintx(irng,iang) + trx31*pha(krng)
            sumintx(krng,kang) = sumintx(krng,kang) - trx31*pha(irng)
            !!            ----------------------------------------------------------
            !!
            !!            for -tsa : -diag
            !tsa          diag(irng,iang) = diag(irng,iang)  + diagk1*pha(krng)
            !tsa          diag(krng,kang) = diag(krng,kang)  - diagk3*pha(irng)
            !!            ----------------------------------------------------------
            !!
            !!            for -fbi : -diag2
            diag2(irng,iang) = diag2(irng,iang) + diag2k1*pha(krng)
            diag2(krng,kang) = diag2(krng,kang) - diag2k3*pha(irng)
            !!            ----------------------------------------------------------
            !!
80        end do                              !* end of kang loop
          !!
70      end do                                !* end of krng loop
        !!
60    end do                                  !* end of iang loop
      !!
50  end do                                    !* end of irng loop
    !!------------------------------------------------------------------------------
    !!==============================================================================
    !!
    !!
    !!    final sum-up to get snl and diag. term to be returned
    !!
    !!    for -tsa
    !tsa  tsa(:,:)   = sumint(:,:) + sumintsa(:,:)
    !b    diag(:,:)  = diag(:,:)   !* is ok, already summed up
    !!
    !!    for -fbi
    fbi(:,:)   = sumint(:,:) + sumintp(:,:) + sumintx(:,:)
    !b    diag2(:,:) = diag2(:,:)  !* is ok, already summed up
    !!    --------------------------------------------------------------------------
    !!    ==========================================================================
    !!
    !!
    !!alt call interp2 only if ialt=2,
    !!    interpolate bi-linearly to fill in tsa/fbi & diag/diag2 arrays
    !!    after alternating the irng, iang, krng & kang loops above
    !!    ------------------------------------------------------------------
    if ( ialt.eq.2 ) then
      !!      for -tsa
      !tsa    call interp2 ( tsa )
      !tsa    call interp2 ( diag )
      !!
      !!      for -fbi
      call interp2 ( fbi )
      call interp2 ( diag2 )
    endif
    !!alt---
    !!    --------------------------------------------------------------------------
    !!    ==========================================================================
    !!
    return
    !!
  end subroutine snlr_fbi
  !!
  !!==============================================================================
  !!
  !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !>
  !> @brief for a given action density array dens1(k,theta), computes the
  !>  rate-of-change array sumint(k,theta) = dn(k,theta)/dt owing to
  !>  wave-wave interaction.
  !>
  !> @details for a given action density array dens1(k,theta), computes the
  !>  rate-of-change array sumint(k,theta) = dn(k,theta)/dt owing to
  !>  wave-wave interaction, as well as some ancillary arrays
  !>  relating to positive and negative fluxes and their integrals.
  !>
  !> @param[in] pha   pha = k*dk*dtheta.
  !> @param[in] ialt  integer switch.
  !>
  !> @author bash toulany
  !> @author michael casey
  !> @author william perrie
  !> @date   12-apr-2016
  !>
  subroutine snlr_tsa ( pha, ialt )
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii                 bio |
    !/                  |           bash toulany            |
    !/                  |           michael casey           |
    !/                  |           william perrie          |
    !/                  |                        fortran 90 |
    !/                  | last update :         12-apr-2016 |
    !/                  +-----------------------------------+
    !/
    !/    01-mar-2016 : origination.                        ( version 5.13 )
    !/
    !!    ------------------------------------------------------------------
    !!
    !!    it returns: tsa & diag  all dim=(nrng,nang)
    !!
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    !!
    !! 1. purpose :
    !!
    !!    -----------------------------------------------------------------#
    !!                                                                     !
    !!    for a given action density array dens1(k,theta), computes the    !
    !!    rate-of-change array sumint(k,theta) = dn(k,theta)/dt owing to   !
    !!    wave-wave interaction, as well as some ancillary arrays          !
    !!    relating to positive and negative fluxes and their integrals.    !
    !!                                                                     !
    !!    -----------------------------------------------------------------#
    !!                                                                     !
    !!    compute:                                                         !
    !!    --------                                                         !
    !!    for both -tsa and -fbi                                           !
    !!    + sumint    contains scale 1 contibution for snl -tsa & snl -fbi !
    !!                                                                     !
    !!    for -tsa                                                         !
    !!    + sumintsa  contains tsa approximation to snl -tsa               !
    !!                                                                     !
    !!    for -fbi                                                         !
    !!    + sumintp   contains scale 2 contribution to snl -fbi            !
    !!    + sumintx   contains cross interactions between scales 1 and 2   !
    !!    -----------------------------------------------------------------#
    !!
    !! 2. method :
    !!
    !! 3. parameters :
    !!
    !!    parameter list
    !!    ------------------------------------------------------------------
    !!    name     type   scope    i/o  description
    !!    ------------------------------------------------------------------
    !!    nrng      int.  public    i   # of freq. or rings
    !!    nang      int.  public    i   # of angles
    !!    npts      int.  public    i   # of points on the locus
    !!    nzz       int.  public    i   linear irng x krng = (nk*(nk+1))/2
    !!    ialt      int.  public    i   integer switch ialt=2; do alternate
    !!                                                 ialt=1; do not alternate
    !!    kzone     int.  public    i   zone of influence = int(alog(4.0)/alog(dfrq))
    !!    na2p1     int.  public    i   = nang/2 + 1
    !!    np2p1     int.  public    i   = npts/2 + 1
    !!    dfrq      real  public    i   frequency multiplier for log freq. spacing
    !!    frqa      r.a.  public    i   radian frequencies (hz);   dim=(nrng)
    !!    pha       r.a.  local     i   pha = k*dk*dtheta      ;   dim=(nrng)
    !!    ------------------------------------------------------------------
    !!
    !!    *** the 11 grid integration geometry arrays at one given depth
    !!    *** from gridsetr.            dim=(npts,nang,nzz,ndep)
    !!    kref2     i.a.  public    i   index of reference wavenumber for k2
    !!    kref4     i.a.  public    i   idem for k4
    !!    jref2     i.a.  public    i   index of reference angle      for k2
    !!    jref4     i.a.  public    i   idem for k4
    !!    wtk2      r.a.  public    i   k2 interpolation weigth along wavenumbers
    !!    wtk4      r.a.  public    i   idem for k4
    !!    wta2      r.a.  public    i   k2 interpolation weigth along angles
    !!    wta4      r.a.  public    i   idem for k4
    !!    tfac2     r.a.  public    i   norm. for interp action density at k2
    !!    tfac4     r.a.  public    i   idem for k4
    !!    grad      r.a.  public    i   coupling and gradient term in integral
    !!                                  grad = c * h * g**2 * ds / |dw/dn|
    !!    ------------------------------------------------------------------
    !!
    !!    *** large & small scale action density from optsa dim=(nrng,nang)
    !!    dens1     r.a.  public    i   lrg-scl action density (k,theta);
    !!    dens2     r.a.  public    i   sml-scl action density (k,theta);
    !!    ------------------------------------------------------------------
    !!
    !!    for both -tsa and -fbi
    !!    sumint    r.a.  local     o   contains scale 1 contribution to snl
    !!                                                       dim=(nrng,nang)
    !!    for -tsa
    !!    sumintsa  r.a.  local     o   contains tsa approximation to snl -tsa
    !!                                                       dim=(nrng,nang)
    !!    for -fbi
    !!    sumintp   r.a.  local     o   contains scale 2 contribution to snl -fbi
    !!                                                       dim=(nrng,nang)
    !!    sumintx   r.a.  local     o   contains cross interactions " "   "  -fbi
    !!                                                       dim=(nrng,nang)
    !!    ------------------------------------------------------------------
    !!
    !!    for -tsa; the 2 returned arrays tsa & diag dim=(nrng,nang)
    !!    tsa       r.a.  public    o   snl-tsa = sumint + sumintsa
    !!    diag      r.a.  public    o   snl-tsa diagonal term = [dn/dn1]
    !!    ------------------------------------------------------------------
    !!
    !!    for -fbi; the 2 returned arrays fbi & diag2 dim=(nrng,nang)
    !!    fbi       r.a.  public    o   snl-fbi = sumint + sumintp  + sumintx
    !!    diag2     r.a.  public    o   snl-fbi diagonal term = [dn/dn1]
    !!    ------------------------------------------------------------------
    !!
    !! 4. subroutines used :
    !!
    !!     name      type  module   description
    !!    ----------------------------------------------------------------
    !!    ----------------------------------------------------------------
    !!
    !! 5. called by :
    !!
    !!     name      type  module   description
    !!    ----------------------------------------------------------------
    !!     gridsetr  subr. w3snl4md setup geometric integration grid
    !!    ----------------------------------------------------------------
    !!
    !! 6. error messages :
    !!
    !!      none.
    !!
    !! 7. remarks :
    !!
    !! 8. structure :
    !!
    !!    see source code.
    !!
    !! 9. switches :
    !!
    !!    !/s  enable subroutine tracing.
    !!
    !!10. source code :
    !!
    !!    --------------------------------------------------------------- &
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ----------------------------------------------------------------72
    !!    ==================================================================
    !!
    !!
    implicit none
    !!
    !!    parameter list
    !!    --------------
    real,    intent(in)  :: pha(nrng)
    integer, intent(in)  :: ialt
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ------------------------------------------------------------------
    !!
    !!
    !!    local parameters & variables
    !!    -----------------------------
    !!    for both -tsa and -fbi
    integer              :: irng,krng, iang,kang
    integer              :: ipt, iizz, izz
    integer              :: kmax
    integer              :: ia2, ia2p, k2, k2p
    integer              :: ia4, ia4p, k4, k4p
    integer              :: nref
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!
    !!    for -tsa
    integer              :: nklimit, nalimit
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!
    !!    for both -tsa and -fbi
    real                 :: d1,   d3,   d2,   d4
    real                 :: dp1,  dp3
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!
    !!    for both -tsa and -fbi
    !!    but for -tsa they are being calc. inside  if/endif if test is successful
    !!    and for -fbi they are being calc. outside if/endif always
    real                 :: dz4,  dz5
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!
    !!    for both -tsa and -fbi
    real                 :: dx13, ds13, dxp13, dsp13
    real                 :: dgm,  t31,  tr31
    real                 :: w2,   w2p,  wa2,  wa2p,  d2a,  d2b,  tt2
    real                 :: w4,   w4p,  wa4,  wa4p,  d4a,  d4b,  tt4
    real                 :: sumint(nrng,nang)
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!
    !!    for -tsa
    real                 :: dz2a, dz3a,  ttsa,   trtsa
    real                 :: ddn1, ddn3,  diagk1, diagk3
    real                 :: sumintsa(nrng,nang)
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!
    !!    for -fbi
    !fbi  real                 :: dp2,   dp4
    !fbi  real                 :: d2pa,  d4pa
    !fbi  real                 :: d2pb,  d4pb
    !fbi  real                 :: dz1,   dz2,   dz3,   dz6,   dz7,   dz8
    !fbi  real                 :: dgmp,  tp31,  trp31, dzsum, txp31, trx31
    !!
    !!    for -fbi; bash added 4 new terms for a full expression of diag2 term
    !fbi  real                 :: ddp1,  ddp2,  ddp3,  ddp4   !* ddpi=di+dpi for i=1,4
    !fbi  real                 :: dd2n1, dd2n3, diag2k1, diag2k3
    !fbi  real                 :: sumintp(nrng,nang)
    !fbi  real                 :: sumintx(nrng,nang)
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ---------------------::-----------------------------------------72
    !!    ##################################################################
    !!------------------------------------------------------------------------------
    !!==============================================================================
    !!
    !!
    !!    for -tsa
    !!    bash; hardwire these two parameters
    nklimit = 6
    nalimit = 6
    !!
    !!
    !!ini
    !!    bash; move initialization of all returned arrays from below to here
    !!    ------------------------------------------------------------------
    !!    for both -tsa and -fbi
    !!    sumint is now initialized here instead of below!
    sumint(:,:)   = 0.0
    !!
    !!    for -tsa
    !!    sumintsa are now initialized here instead of below!
    sumintsa(:,:) = 0.0
    tsa(:,:)      = 0.0
    diag(:,:)     = 0.0
    !!
    !!    for -fbi
    !!    sumintp and sumintx are now initialized here instead of below!
    !fbi  sumintp(:,:)  = 0.0
    !fbi  sumintx(:,:)  = 0.0
    !fbi  fbi(:,:)      = 0.0
    !fbi  diag2(:,:)    = 0.0
    !!ini---
    !!    ------------------------------------------------------------------
    !!    ------------------------------------------------------------------
    !!    ##################################################################
    !!
    !!
    !!    for -tsa
    ddn1    = 0.0                           !* for -tsa diag  [dn/dn1]
    ddn3    = 0.0                           !* for -tsa diag  [dn/dn3]
    !!
    !!    for -fbi
    !fbi  dd2n1   = 0.0                           !* for -fbi diag2 [dn/dn1]
    !fbi  dd2n3   = 0.0                           !* for -fbi diag2 [dn/dn3]
    !!
    !!
    !!
    !!50
    do 50 irng=1,nrng,ialt
      !!kz
      kmax = min(irng+kzone, nrng)   !* bash; sometimes a locus pt is outside nrng
      !kz     kmax = min(irng+kzone, nrng-1) !* bash; taking 1 out will not affect kzone, try it
      !!kz---
      !!
      iizz = (nrng-1)*(irng-1) - ((irng-2)*(irng-1))/2
      !!      ----------------------------------------------------------------
      !!
      !!
      !!60
      do 60 iang=1,nang,ialt
        !!
        !!        for both -tsa and -fbi
        d1   = dens1(irng,iang)
        dp1  = dens2(irng,iang)
        !!
        !!        for -fbi
        !fbi      ddp1 = d1+dp1             !! for full expression of diag2 term
        !!
        !!70
        !!kz
        !kz       do 70 krng=irng,nrng
        do 70 krng=irng,kmax,ialt
          !!
          !!          for both -tsa and -fbi
          !!          bash; check5  be consistent with gridsetr
          !!          moved here from below (was after do 80 kang=1,nang)
          !!          and changed go to 80 into go to 70 (i.e. go to next krng)
          !kz         if ( frqa(krng)/frqa(irng) .gt. 4. ) go to 70  !* original gridsetr
          !kz         if ( frqa(krng)/frqa(irng) .gt. 3. ) go to 70  !* original snlr_'s
          !kz         if ( frqa(krng)/frqa(irng) .gt. 2. ) go to 70  !* bash; use .gt. 2
          !!kz---
          !!
          izz = krng + iizz
          !!          ------------------------------------------------------------
          !!
          !!80
          do 80 kang=1,nang,ialt
            !!
            !!            for both -tsa and -fbi
            !!ba1         bash; remove self interaction
            !!                  skip k1 but keep the opposite angle to k1 - original setting
            if ( krng.eq.irng ) then              !* wn3 = wn1
              if ( kang.eq.iang ) go to 80        !* th3 = th1
            endif
            !!ba1---
            !!            ----------------------------------------------------------
            !!
            !!            for both -tsa and -fbi
            d3   = dens1(krng,kang)
            dp3  = dens2(krng,kang)
            !!
            !!            for -fbi
            !fbi          ddp3 = d3+dp3         !! for full expression of diag2 term
            !!
            !!
            !!            for both -tsa and -fbi
            nref = kang - iang + 1
            if ( nref .lt. 1 ) nref = nref + nang
            !!
            !!
            !!            for both -tsa and -fbi
            !!            bash; check5  be consistent with gridsetr
            !!                  and move this test above right after do 70 krng=irng,nrng
            !x            if ( frqa(krng)/frqa(irng) .gt. 4. ) go to 80  !* gridsetr
            !b            if ( frqa(krng)/frqa(irng) .gt. 3. ) go to 80  !* original
            !!
            !!
            !!            for both -tsa and -fbi
            t31     = 0.0             !* must be reset to 0.0
            !!
            !!            for -tsa
            ttsa    = 0.0             !* must be reset to 0.0
            diagk1  = 0.0             !* must be reset to 0.0
            diagk3  = 0.0             !* must be reset to 0.0
            !!
            !!            for -fbi
            !fbi          tp31    = 0.0             !* must be reset to 0.0
            !fbi          txp31   = 0.0             !* must be reset to 0.0
            !fbi          diag2k1 = 0.0             !* must be reset to 0.0
            !fbi          diag2k3 = 0.0             !* must be reset to 0.0
            !!
            !!            for both -tsa and -fbi
            dx13  = d1*d3
            ds13  = d3-d1
            dxp13 = dp1*dp3
            dsp13 = dp3-dp1
            !!            ----------------------------------------------------------
            !!
            !!90
            do 90 ipt=1,npts
              !!
              !!              for both -tsa and -fbi
              !!              save time by skipping insignificant contributions
              !!e-30
              !e-30           if ( grad(ipt,nref,izz) .lt. 1.e-30 ) go to 90
              !!e-30---
              if ( grad(ipt,nref,izz) .lt. 1.e-15 ) go to 90
              !!e-30---
              !!              --------------------------------------------------------
              !!
              !!xlc1          bash; skip k1 but keep the opposite angle to k1 - original setting
              !xlc1           if ( kang.eq.iang ) then                     !* th3=+th1
              !xlc1             if (ipt.eq.1 .or. ipt.eq.np2p1) go to 90   !* skip x-axis loci
              !xlc1           end if
              !!xlc1---
              !!              --------------------------------------------------------
              !!
              !!
              !!2             estimation of density for wave #2
              !!
              !!              for both -tsa and -fbi
              k2  = kref2(ipt,nref,izz)
              k2p = k2 + 1
              w2  = wtk2(ipt,nref,izz)
              w2p = 1. - w2
              !!
              !!              for both -tsa and -fbi
              ia2 = iang + jref2(ipt,nref,izz)
              if ( ia2 .gt. nang )  ia2  = ia2  - nang
              !!
              !!              for both -tsa and -fbi
              ia2p = ia2 + 1
              if ( ia2p .gt. nang ) ia2p = ia2p - nang
              !!
              !!              for both -tsa and -fbi
              wa2  = wta2(ipt,nref,izz)
              wa2p = 1. - wa2
              d2a  = w2 * dens1(k2,ia2)  + w2p * dens1(k2p,ia2)
              d2b  = w2 * dens1(k2,ia2p) + w2p * dens1(k2p,ia2p)
              tt2  = tfac2(ipt,nref,izz)
              d2   = (wa2*d2a  + wa2p*d2b)  * tt2
              !!
              !!              for -fbi
              !fbi            d2pa = w2 * dens2(k2,ia2)  + w2p * dens2(k2p,ia2)
              !fbi            d2pb = w2 * dens2(k2,ia2p) + w2p * dens2(k2p,ia2p)
              !!
              !!              for -fbi
              !fbi            dp2  = (wa2*d2pa + wa2p*d2pb) * tt2   !* for -fbi
              !fbi            ddp2 = d2+dp2       !! for full expression of diag2 term
              !!              ========================================================
              !!
              !!
              !!4             estimation of density for wave #4
              !!
              !!              for both -tsa and -fbi
              k4  = kref4(ipt,nref,izz)
              k4p = k4 + 1
              w4  = wtk4(ipt,nref,izz)
              w4p = 1. - w4
              !!
              !!              for both -tsa and -fbi
              ia4 = iang + jref4(ipt,nref,izz)
              if ( ia4 .gt. nang )  ia4  = ia4  - nang
              !!
              !!              for both -tsa and -fbi
              ia4p= ia4 + 1
              if ( ia4p .gt. nang ) ia4p = ia4p - nang
              !!
              !!              for both -tsa and -fbi
              wa4  = wta4(ipt,nref,izz)
              wa4p = 1. - wa4
              d4a  = w4*dens1(k4,ia4)  + w4p*dens1(k4p,ia4)
              d4b  = w4*dens1(k4,ia4p) + w4p*dens1(k4p,ia4p)
              tt4  = tfac4(ipt,nref,izz)
              d4   = (wa4*d4a  + wa4p*d4b)  * tt4
              !!
              !!              for -fbi
              !fbi            d4pa = w4*dens2(k4,ia4)  + w4p*dens2(k4p,ia4)
              !fbi            d4pb = w4*dens2(k4,ia4p) + w4p*dens2(k4p,ia4p)
              !!
              !!              for -fbi
              !fbi            dp4  = (wa4*d4pa + wa4p*d4pb) * tt4   !* for -fbi
              !fbi            ddp4 = d4+dp4       !! for full expression of diag2 term
              !!              ========================================================
              !!
              !!
              !!              for both -tsa and -fbi
              dgm  = dx13*(d4-d2) + ds13*d4*d2        !* dgm=b of r&p'08 eqn(8)
              !!                                         !* represents broad scale interactions
              t31  = t31  + dgm  * grad(ipt,nref,izz)
              !!              --------------------------------------------------------
              !!
              !!              for -fbi
              !fbi            dgmp = dxp13*(dp4-dp2) + dsp13*dp4*dp2  !* dgmp=l of r&p'08 eqn(8)
              !!                                          !* represents local scale interactions
              !fbi            tp31 = tp31 + dgmp * grad(ipt,nref,izz)
              !!              --------------------------------------------------------
              !!              ========================================================
              !!
              !!
              !!              for -tsa : -diag
              !!              use this expression for the diagonal term
              !!              whose derivation neglect "dp2" & "dp4"
              ddn1   = (d3+dp3)*(d4-d2) - d4*d2              !* dn/dn1
              ddn3   = (d1+dp1)*(d4-d2) + d4*d2              !* dn/dn3
              diagk1 = diagk1 + ddn1 * grad(ipt,nref,izz)
              diagk3 = diagk3 + ddn3 * grad(ipt,nref,izz)
              !!              --------------------------------------------------------
              !!
              !!              for -fbi : -diag2
              !!              use the full expression for the diagonal terms
              !!              whose derivation keeps all large + small scale
              !fbi            dd2n1   = ddp3*(ddp4-ddp2) - ddp4*ddp2         !* dn/dn1
              !fbi            dd2n3   = ddp1*(ddp4-ddp2) + ddp4*ddp2         !* dn/dn3
              !fbi            diag2k1 = diag2k1 + dd2n1 * grad(ipt,nref,izz)
              !fbi            diag2k3 = diag2k3 + dd2n3 * grad(ipt,nref,izz)
              !!              --------------------------------------------------------
              !!              ========================================================
              !!
              !!
              !!              for -fbi
              !fbi            dz1  = dx13    * (dp4-dp2)
              !fbi            dz2  = d1*dp3  * ((d4-d2)+(dp4-dp2))
              !fbi            dz3  = d3*dp1  * ((d4-d2)+(dp4-dp2))
              !!
              !!              for -fbi (calc. dz4 & dz5 here)
              !fbi            dz4  = dxp13   * (d4-d2)
              !fbi            dz5  = d2*d4   *  dsp13
              !!
              !!              for -tsa
              !!              cross-interactions between parametric and perturbation
              !!              that occur only when k3 is close enough to k1
              !!              bash; added an extra check on (nang-nalimit)
              !b              if ( iabs(irng-krng).lt.nklimit .and.                 &
              !b                   iabs(iang-kang).lt.nalimit )    then        !* original
              !!
              if (     (krng-irng).lt.nklimit .and.                 &
                   ( iabs(kang-iang).lt.nalimit .or.                  &
                   iabs(kang-iang).gt.(nang-nalimit) ) )  then !* bash
                !!
                !!                for -tsa (calc. dz4 & dz5 here)
                dz4  = dxp13   * (d4-d2)
                dz5  = d2*d4   *  dsp13
                dz2a = d1*dp3 * (d4-d2)
                dz3a = d3*dp1 * (d4-d2)
                !!
                ttsa = ttsa + (dz4+dz5+dz2a+dz3a)*grad(ipt,nref,izz)
                !!
              endif
              !!              --------------------------------------------------------
              !!
              !!              for -fbi
              !fbi            dz6   = d2*dp4  * (ds13+dsp13)
              !fbi            dz7   = d4*dp2  * (ds13+dsp13)
              !fbi            dz8   = dp2*dp4 *  ds13
              !fbi            dzsum = dz1 + dz2 + dz3 + dz4 + dz5 + dz6 + dz7 + dz8
              !fbi            txp31 = txp31 + dzsum * grad(ipt,nref,izz)
              !!              --------------------------------------------------------
              !!              ========================================================
              !!
              !!
90          end do                        !* end of ipt (locus) loop
            !!            ----------------------------------------------------------
            !!
            !!
            !!            multiply the following components by factor 2. in here
            !!
            !!            for both -tsa and -fbi
            tr31  = 2. * t31
            !!
            !!            for -tsa
            trtsa = 2. * ttsa
            !!
            !!            for -fbi
            !fbi          trp31 = 2. * tp31
            !fbi          trx31 = 2. * txp31
            !!
            !!            for -tsa : -diag
            diagk1  = 2. * diagk1
            diagk3  = 2. * diagk3
            !!
            !!            for -fbi : -diag2
            !fbi          diag2k1 = 2. * diag2k1
            !fbi          diag2k3 = 2. * diag2k3
            !!            ----------------------------------------------------------
            !!
            !!            for both -tsa and -fbi
            sumint(irng,iang)  = sumint(irng,iang)  + tr31*pha(krng)
            sumint(krng,kang)  = sumint(krng,kang)  - tr31*pha(irng)
            !!            ----------------------------------------------------------
            !!
            !!            for -tsa
            sumintsa(irng,iang)= sumintsa(irng,iang)+ trtsa*pha(krng)
            sumintsa(krng,kang)= sumintsa(krng,kang)- trtsa*pha(irng)
            !!            ----------------------------------------------------------
            !!
            !!            for -fbi
            !fbi          sumintp(irng,iang) = sumintp(irng,iang) + trp31*pha(krng)
            !fbi          sumintp(krng,kang) = sumintp(krng,kang) - trp31*pha(irng)
            !!
            !!            for -fbi
            !fbi          sumintx(irng,iang) = sumintx(irng,iang) + trx31*pha(krng)
            !fbi          sumintx(krng,kang) = sumintx(krng,kang) - trx31*pha(irng)
            !!            ----------------------------------------------------------
            !!
            !!            for -tsa : -diag
            diag(irng,iang) = diag(irng,iang)  + diagk1*pha(krng)
            diag(krng,kang) = diag(krng,kang)  - diagk3*pha(irng)
            !!            ----------------------------------------------------------
            !!
            !!            for -fbi : -diag2
            !fbi          diag2(irng,iang) = diag2(irng,iang) + diag2k1*pha(krng)
            !fbi          diag2(krng,kang) = diag2(krng,kang) - diag2k3*pha(irng)
            !!            ----------------------------------------------------------
            !!
80        end do                              !* end of kang loop
          !!
70      end do                                !* end of krng loop
        !!
60    end do                                  !* end of iang loop
      !!
50  end do                                    !* end of irng loop
    !!------------------------------------------------------------------------------
    !!==============================================================================
    !!
    !!
    !!    final sum-up to get snl and diag. term to be returned
    !!
    !!    for -tsa
    tsa(:,:)   = sumint(:,:) + sumintsa(:,:)
    !b    diag(:,:)  = diag(:,:)   !* is ok, already summed up
    !!
    !!    for -fbi
    !fbi  fbi(:,:)   = sumint(:,:) + sumintp(:,:) + sumintx(:,:)
    !b    diag2(:,:) = diag2(:,:)  !* is ok, already summed up
    !!    --------------------------------------------------------------------------
    !!    ==========================================================================
    !!
    !!
    !!alt call interp2 only if ialt=2,
    !!    interpolate bi-linearly to fill in tsa/fbi & diag/diag2 arrays
    !!    after alternating the irng, iang, krng & kang loops above
    !!    ------------------------------------------------------------------
    if ( ialt.eq.2 ) then
      !!      for -tsa
      call interp2 ( tsa )
      call interp2 ( diag )
      !!
      !!      for -fbi
      !fbi    call interp2 ( fbi )
      !fbi    call interp2 ( diag2 )
    endif
    !!alt---
    !!    --------------------------------------------------------------------------
    !!    ==========================================================================
    !!
    return
    !!
  end subroutine snlr_tsa
  !!
  !!==============================================================================
  !!
  !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !>
  !> @brief interpolate bi-linearly to fill in tsa/fbi & diag/diag2 arrays.
  !>
  !> @details optionally smooth the interior and the corners
  !>  after alternating the irng, iang, krng & kang loops in snlr's.
  !>
  !> @param[inout] x  array to be interpolated and smoothed.
  !>
  !> @author bash toulany
  !> @author michael casey
  !> @author william perrie
  !> @date   12-apr-2016
  !>
  subroutine interp2 ( x )
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii                 bio |
    !/                  |           bash toulany            |
    !/                  |           michael casey           |
    !/                  |           william perrie          |
    !/                  |                        fortran 90 |
    !/                  | last update :         12-apr-2016 |
    !/                  +-----------------------------------+
    !/
    !/    01-mar-2016 : origination.                        ( version 5.13 )
    !/
    !!
    !! 1. purpose :
    !!
    !!    interpolate bi-linearly to fill in tsa/fbi & diag/diag2 arrays
    !!    and then (optional) smooth the interior and the corners
    !!    after alternating the irng, iang, krng & kang loops in snlr's
    !!
    !! 2. method :
    !!
    !! 3. parameters :
    !!
    !!    parameter list
    !!    ------------------------------------------------------------------
    !!    name     type   scope    i/o  description
    !!    ------------------------------------------------------------------
    !!    nrng      int.  public    i   # of freq. or rings
    !!    nang      int.  public    i   # of angles
    !!    ismo      int.  local     i   switch;  ismo=0 skip smoothing
    !!                                           ismo.ne.0 do smoothing
    !!    x         r.a.  local    i/o  array to be ineterp. & smoothing
    !!                                  its returned to snlr_tsa as tsa or diag
    !!                                           and to snlr_fbi as fbi or diag2
    !!                                           dim=(nrng,nang)
    !!    ------------------------------------------------------------------
    !!
    !! 4. subroutines used :
    !!
    !!     name      type  module   description
    !!    ----------------------------------------------------------------
    !!    ----------------------------------------------------------------
    !!
    !! 5. called by :
    !!
    !!     name      type  module   description
    !!    ----------------------------------------------------------------
    !!     snlr_tsa  subr. w3snl4md computes dn(k,theta)/dt for tsa
    !!     --------                 due to wave-wave inter. (set itsa = 1)
    !!     snlr_fbi  subr. w3snl4md computes dn(k,theta)/dt for fbi
    !!     --------                 due to wave-wave inter. (set itsa = 0)
    !!    ----------------------------------------------------------------
    !!
    !! 6. error messages :
    !!
    !!      none.
    !!
    !! 7. remarks :
    !!
    !! 8. structure :
    !!
    !!    see source code.
    !!
    !! 9. switches :
    !!
    !!    !/s  enable subroutine tracing.
    !!
    !!10. source code :
    !!
    !!    --------------------------------------------------------------- &
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ----------------------------------------------------------------72
    !!    ==================================================================
    !!
    !!
    implicit none
    !!
    !!
    !!    parameter list
    !!    --------------
    real,    intent(inout)  :: x(nrng,nang)
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ------------------------------------------------------------------
    !!
    !!
    !!    local parameters
    !!    ----------------
    integer              :: irng, iang
    real                 :: y(nrng,nang)  !* dummy array used in smoothing
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ---------------------::-----------------------------------------72
    !!    ##################################################################
    !!------------------------------------------------------------------------------
    !!==============================================================================
    !!
    !!
    !!-0  initial y(:,:) array before it's computed
    !!ini
    y(:,:) = 0.0
    !!ini---
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    !!
    !!-1  interpolate using simple 2 point averaging to fill in x
    !!    remeber: nang must be an even number ==> nang-2 is an even number
    !!    and      nrng must be an odd  number ==> nrng-1 is an even number
    !!    example numbers used here are for nrng=35 & nang=36
    !!    ------------------------------------------------------------------
    !!
    !!-1a for every calculated iang (1,3,5,..,nang-1=35)
    !!    fill in missing irng's    (2,4,6,..,nrng-1=34)
    do iang=1,nang-1,2      !* = 1,3,5,...,nang-1=35
      do  irng=2,nrng-1,2      !* = 2,4,6,...,nrng-1=34
        x(irng,iang) = 0.5 * ( x(irng-1,iang) + x(irng+1,iang) )
      end do
    end do
    !!    ------------------------------------------------------------------
    !!
    !!-1b now, for every irng (1,2,3,..,nrng  =35)
    !!    fill missing iang's (2,4,6,..,nang-2=34)
    do irng=1,nrng          !* 1,2,3,..,nrng  =35
      do iang=2,nang-2,2      !* 2,4,6,..,nang-2=34
        x(irng,iang) = 0.5 * ( x(irng,iang-1) + x(irng,iang+1) )
      end do
    end do
    !!    ------------------------------------------------------------------
    !!
    !!-1c for iang = nang (special case since nang is an even number)
    do irng=1,nrng
      x(irng,nang) = 0.5 * ( x(irng,nang-1) + x(irng,1) )
    end do
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    !!
    !!
    !!    skip smoothing only if ismo = 0
    !!
    !!
    if ( ismo.eq.0 ) goto 99
    !!
    !!
    !!
    !!-2  smoothing the 2d array x into array y
    !!
    !!-2a smoothing the interior [2;nrng-1] x [2:nang-1]
    !!-   using 9 points averaged with equal weights.
    !!-   here use the dummy array so we don't spoil the original array.
    do irng=2,nrng-1
      do iang=2,nang-1
        y(irng,iang)=(x(irng-1,iang-1)+x(irng-1,iang)+x(irng-1,iang+1) + &
             x(irng,  iang-1)+x(irng,  iang)+x(irng,  iang+1) + &
             x(irng+1,iang-1)+x(irng+1,iang)+x(irng+1,iang+1))/9.
      end do
    end do
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    !!
    !!-3  smooth first & last line at iang=1 & iang=nang  (special cases)
    !!
    !!-3a smooth line at iang = 1     (special case)
    !!-   using 9 points averaged with equal weights.
    do irng=2,nrng-1
      y(irng, 1) = (x(irng-1,nang) + x(irng-1, 1) + x(irng-1, 2) +  &
           x(irng,  nang) + x(irng,   1) + x(irng,   2) +  &
           x(irng+1,nang) + x(irng+1, 1) + x(irng+1, 2) )/9.
    end do
    !!    ------------------------------------------------------------------
    !!
    !!-3b smooth line at iang = nang  (special case)
    !!-   using 9 points averaged with equal weights.
    do irng=2,nrng-1
      y(irng,nang)=(x(irng-1,nang-1) +x(irng-1,nang) +x(irng-1,1) + &
           x(irng,  nang-1) +x(irng,  nang) +x(irng,  1) + &
           x(irng+1,nang-1) +x(irng+1,nang) +x(irng+1,1))/9.
    end do
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    !!
    !!-4  smooth first & last col. at irng=1 & irng=nrng  (special cases)
    !!
    !!-4a smooth col. at irng = 1     (low frq. can be skipped)
    !!-   using 6 points averaged with equal weights.
    do iang=2,nang-1
      y(1,iang)   = (x(1,iang-1) + x(1,iang) + x(1,iang+1) +        &
           x(2,iang-1) + x(2,iang) + x(2,iang+1) )/6.
    end do
    !!    ------------------------------------------------------------------
    !!
    !!-4b smooth col. at irng = nrng  (high frq. can be skipped)
    !!-   using 6 points averaged with equal weights.
    do iang=2,nang-1
      y(nrng,iang)=(x(nrng-1,iang-1)+x(nrng-1,iang)+x(nrng-1,iang+1)+ &
           x(nrng, iang-1)+x(nrng, iang)+x(nrng, iang+1) )/6.
    end do
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    !!
    !!-5  smooth the 4 corners (optional):  <== skip no sig. effect
    !!-   using 6 points averaged with equal weights
    !!
    !!-5a corner (1, 1)
    y(1, 1)      =( x(1,nang) + x(1, 1) + x(1, 2) +                 &
         x(2,nang) + x(2, 1) + x(2, 2) )/6.0
    !!    ------------------------------------------------------------------
    !!
    !!-5b corner (nrng,1)
    y(nrng,1)    =( x(nrng-1,nang) + x(nrng-1,1) + x(nrng-1,2) +    &
         x(nrng,  nang) + x(nrng,  1) + x(nrng,  2) )/6.0
    !!    ------------------------------------------------------------------
    !!
    !!-5c corner (1,nang)
    y(1,nang)    =( x(1,nang-1) + x(1,nang) + x(1, 1) +             &
         x(2,nang-1) + x(2,nang) + x(2, 1) ) / 6.
    !!    ------------------------------------------------------------------
    !!
    !!-5d corner (nrng,nang)
    y(nrng,nang) =( x(nrng-1,nang-1) +x(nrng-1,nang) +x(nrng-1,1) + &
         x(nrng,  nang-1) +x(nrng,  nang) +x(nrng,  1) )/6.
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    !!
    !!-6  final, dump smoothed array y(:,:) into x(:,:) to be returned
    !!
    !!-6a done with x(:,:) re-initial before it's replaced by y(:,:)
    !!ini
    x(:,:) = 0.0
    !!ini---
    !!
    !!-6b dump smoothed array y(:,:) into x(:,:) to be returned
    do iang=1,nang
      do irng=1,nrng
        x(irng,iang) = y(irng,iang)
      end do
    end do
    !!    bash; can simplify in one line
    !b    x(1:nrng, 1:nang) = y(1:nrng, 1:nang)
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
99  continue
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    !!
    return
    !!
  end subroutine interp2
  !!
  !!==============================================================================
  !!
  !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !>
  !> @brief calculate the wavenumber k (rad/m) as function of
  !>  frequency 'f' (hz) and depth 'dep' (m).
  !>
  !> @verbatim
  !>  using what looks like a "pade approximation" of an inversion
  !>  of the linear wave dispersion relation.
  !>
  !>      sigma^2 = gk*tanh(kd),  sigma = 2*pi*f
  !>      wavenumber k (rad/m) is returned in "wkfnc"
  !>
  !> @endverbatim
  !>
  !> @param   f      frequency (hz).
  !> @param   dep    depth (m).
  !> @returns wkfnc  wavenumber 'k'
  !>
  !> @author bash toulany
  !> @author michael casey
  !> @author william perrie
  !> @date   12-apr-2016
  !>
  real function wkfnc ( f, dep )
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii                 bio |
    !/                  |           bash toulany            |
    !/                  |           michael casey           |
    !/                  |           william perrie          |
    !/                  |                        fortran 90 |
    !/                  | last update :         12-apr-2016 |
    !/                  +-----------------------------------+
    !/
    !/    01-mar-2016 : origination.                        ( version 5.13 )
    !/
    !!
    !!    it returns: wavenumber 'k' in wkfnc
    !!
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    !! 1. purpose :
    !!
    !!    calculate the wavenumber k (rad/m) as function of
    !!    frequency 'f' (hz) and depth 'dep' (m).
    !!
    !! 2. method :
    !!
    !!    using what looks like a "pade approximation" of an inversion
    !!    of the linear wave dispersion relation.
    !!        sigma^2 = gk*tanh(kd),  sigma = 2*pi*f
    !!        wavenumber k (rad/m) is returned in "wkfnc"
    !!
    !! 3. parameters :
    !!
    !!    parameter list
    !!    ------------------------------------------------------------------
    !!    name     type   scope    i/o  description
    !!    ------------------------------------------------------------------
    !!    twopi     real  public    i   = tpi; ww3 2*pi=8.*atan(1.) (radians)
    !!    ------------------------------------------------------------------
    !!
    !!    --------------------------------------------------------------- &
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ----------------------------------------------------------------72
    !!    ==================================================================
    !!
    !!
    implicit none
    !!
    !!    parameter list
    !!    --------------
    real,    intent(in)  :: f, dep
    !!
    !!    local variables
    !!    ---------------
    real(kind=8)         :: g, y, x
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ---------------------::-----------------------------------------72
    !!    ##################################################################
    !!------------------------------------------------------------------------------
    !!==============================================================================
    !!
    !!
    g     = 9.806                   !* set = grav as in constants
    !!
    y     = ( (twopi*f)**2 ) * dep / g   !* sigma^2 d/g
    !!
    !!    --------------------------------------------------------------- &
    x     = y * ( y +                                               &
         1./(1.00000+y*(0.66667+y*(0.35550+y*(0.16084+y*(0.06320   &
         +y*(0.02174+y*(0.00654+y*(0.00171+y*(0.00039+y*0.00011)   &
         )))))))))
    !!    --------------------------------------------------------------- &
    !!
    x     = sqrt(x)                 !* kd
    !!
    wkfnc = x / dep                 !* k
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    return
    !!
  end function wkfnc
  !!
  !!==============================================================================
  !!
  !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !>
  !> @brief calculate the group velocity cg (m/s) as function of
  !>  frequency 'f' (hz), depth 'dep' (m) and phase speed 'cvel' (m/s).
  !>
  !> @verbatim
  !>  this routine uses the identity
  !>         sinh(2x) = 2*tanh(x)/(1-tanh(x)**2)
  !>    to avoid extreme sinh(2x) for large x.
  !>    thus,    2kd/sinh(2kd) = kd(1-tanh(kd)**2)/tanh(kd)
  !>    group velocity cg (m/s) is returned in "cgfnc".
  !> @endverbatim
  !>
  !> @param   f      frequency (hz).
  !> @param   dep    depth (m).
  !> @param   cvel   phase speed (m/s).
  !> @returns cgfnc  group velocity (m/s).
  !>
  !> @author bash toulany
  !> @author michael casey
  !> @author william perrie
  !> @date   12-apr-2016
  !>
  real function cgfnc ( f, dep, cvel )
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii                 bio |
    !/                  |           bash toulany            |
    !/                  |           michael casey           |
    !/                  |           william perrie          |
    !/                  |                        fortran 90 |
    !/                  | last update :         12-apr-2016 |
    !/                  +-----------------------------------+
    !/
    !/    01-mar-2016 : origination.                        ( version 5.13 )
    !/
    !!    it returns:  group velocity (m/s) in cgfnc
    !!
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    !!
    !! 1. purpose :
    !!
    !!    calculate the group velocity cg (m/s) as function of
    !!    frequency 'f' (hz), depth 'dep' (m) and phase speed 'cvel' (m/s)
    !!
    !! 2. method :
    !!
    !!    this routine uses the identity
    !!         sinh(2x) = 2*tanh(x)/(1-tanh(x)**2)
    !!    to avoid extreme sinh(2x) for large x.
    !!    thus,    2kd/sinh(2kd) = kd(1-tanh(kd)**2)/tanh(kd)
    !!    group velocity cg (m/s) is returned in "cgfnc"
    !!
    !! 3. parameters :
    !!
    !!    parameter list
    !!    ------------------------------------------------------------------
    !!    name     type   scope    i/o  description
    !!    ------------------------------------------------------------------
    !!    twopi     real  public    i   = tpi; ww3 2*pi=8.*atan(1.) (radians)
    !!    ------------------------------------------------------------------
    !!
    !!    --------------------------------------------------------------- &
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ----------------------------------------------------------------72
    !!    ==================================================================
    !!
    !!
    implicit none
    !!
    !!    parameter list
    !!    --------------
    real,    intent(in)  :: f, dep, cvel
    !!
    !!    local variables
    !!    ---------------
    real                 :: wkd, tkd
    !!    -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !!    ---------------------::-----------------------------------------72
    !!    ##################################################################
    !!------------------------------------------------------------------------------
    !!==============================================================================
    !!
    !!
    wkd   = twopi * f*dep/cvel           !* kd
    tkd   = tanh(wkd)                    !* tanh(kd)
    cgfnc = 0.5*cvel*(1.+wkd*(1.-tkd**2)/tkd)
    !!    ------------------------------------------------------------------
    !!    ==================================================================
    !!
    return
    !!
  end function cgfnc
  !!
  !!==============================================================================
  !!
  !!
end module w3snl4md
!!
!!==============================================================================
