!/ =================================================================== /
!/ define the enable_ww3 macro when compiling within wavewatch iii.
!/
!/ when compiling outside of wavewatch iii the enable_ww3 must be
!/ undefined and the enable_mpi macro must be defined if compiling
!/ with mpi.
!/ =================================================================== /
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
!/ =================================================================== /
module w3gsrumd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |        t. j. campbell, nrl        |
  !/                  |                        fortran 90 |
  !/                  | last update :         25-jan-2017 |
  !/                  +-----------------------------------+
  !/
  !/    30-oct-2009 : origination.                        ( version 3.14 )
  !/    14-jun-2010 : fix for acos argument > 1 in w3dist ( version 3.14 )
  !/    12-nov-2010 : change t_nns, w3nn*, w3sort, w3isrt to public.
  !/                  add w3gfij (public). implement r4 & r8 interfaces.
  !/                  add subcell check for grid cell that includes a pole.
  !/                  change to number of search buckets based on
  !/                  dimensions of input grid.           ( version 3.14 )
  !/    01-dec-2010 : assign cells to buckets based on overlap. the
  !/                  nearest-neighbor bucket search is removed (no longer
  !/                  needed). add support for tripole grids (jclo).
  !/                  add w3gfcd (public). some cleanup. add ouput of
  !/                  approximate memory usage.           ( version 3.14 )
  !/    01-dec-2010 : add check for target point coincident with a cell
  !/                  vertex in w3rmbl.  change to error exit when unable
  !/                  to determine local (i,j).           ( version 3.14 )
  !/    06-dec-2010 : remove restriction on longitude range. change iclo
  !/                  to integer and remove jclo. implement support for
  !/                  r4 and r8 source grids.             ( version 3.14 )
  !/    15-jun-2012 : fixed various format statements that gave compile
  !/                  warnings with intel compiler on ncep r&d machine
  !/                  zeus (h. l. tolman)                 ( version 4.07 )
  !/    20-jan-2017 : moved all record of changes from subroutines to
  !/                  the top of the module and consolidate source code
  !/                  for procedure interfaces            ( version 6.02 )
  !/    20-jan-2017 : generalize index bounds for source  ( version 6.02 )
  !/    20-jan-2017 : fix tripole grid index mapping and implement
  !/                  additional index closure types.     ( version 6.02 )
  !/    20-jan-2017 : add small non-zero tolerance to bounding box checks,
  !/                  point coincidence checks and checks for points that
  !/                  lie exactly on a cell side          ( version 6.02 )
  !/    20-jan-2017 : add option to w3gfcl, w3grmp, w3gfpt, and w3gfij to
  !/                  allow target outside of source grid ( version 6.02 )
  !/    20-jan-2017 : implement more accurate sin(d/2) equation in w3dist
  !/                  for computing angular distance      ( version 6.02 )
  !/    20-jan-2017 : implement stereographic projection for remapping
  !/                  from cells near a pole              ( version 6.02 )
  !/    20-jan-2017 : add routine for computing metric and derivatives
  !/                  for a curvilinear grid and routines for computing
  !/                  gradient and divergence of fields defined on a
  !/                  curvilinear grid                    ( version 6.02 )
  !/    20-jan-2017 : add routine for computing computing bounding box
  !/                  for a curvilinear grid              ( version 6.02 )
  !/    20-jan-2017 : add w3grmc as generic routine for computing
  !/                  remapping coefficients              ( version 6.02 )
  !/    25-jan-2017 : fix index offsets for mask in w3grmp and w3grmc.
  !/                  change redist to nearpt in w3grmc.  ( version 6.03 )
  !/    31-oct-2017 : add optional mask input for w3cgdm. ( version 6.03 )
  !/    18-jul-2018 : add fall back to nfd = 2 in w3cgdm for metric
  !/                  calculations where gsqrl < 0.       ( version 6.05 )
  !/
  !  1. purpose :
  !
  !     search, regrid, and miscellaneous utilities (data structures and
  !     associated methods) for logically rectangular grids.
  !
  !     the grid-search-utility (gsu) object can be used for rapid searching
  !     of the associated grid to identify a grid cell that encloses a target
  !     point and to compute interpolation weights.  the gsu object maintains
  !     internal pointers to the associated grid coordinate arrays.  rapid
  !     searching is done using a bucket search algorithm.  the search buckets
  !     are based on the bounding box for the associated grid and an optional
  !     user defined approximate number of grid cells per search bucket.
  !
  !     grid cells are identified by the cell's lower-left corner grid point.
  !     the vertices (grid points) associated with a grid cell are assigned a
  !     sequential index in a counterclockwise order beginning with the cell's
  !     lower-left corner grid point.  that is, when moving from vertex 1 to
  !     vertex 2 to vertex 3, etc., the grid cell interior is always to the left.
  !     note that though cell will be counterclockwise w.r.t. indices, this does
  !     not necessarily mean that the cell will be counterclockwise geographically,
  !     specifically in situation of curvilinear grid.
  !
  !                  (x4,y4)               (x3,y3)
  !                      _____________________
  !                     /                    /
  !                    /                    /
  !                   /                    /
  !                  /                    /
  !                 /____________________/
  !             (x1,y1)              (x2,y2)
  !
  !
  !     a simple interpolation example:
  !
  !          -----------------------------------------------------------
  !          ! define data
  !          type(t_gsu) :: gsu
  !          logical :: ijg  = .true.
  !          logical :: llg  = .true.
  !          logical :: iclo = iclo_none
  !          real, pointer :: xs(:,:), ys(:,:)  !source grid coordinates
  !          real :: fs(:,:)                    !source field
  !          integer :: nt                      !number of target points
  !          real :: xt(nt), yt(nt), ft(nt)     !target coordinates and field
  !          integer :: is(4), js(4)            !interpolation points
  !          real :: rw(4)                      !interpolation weights
  !
  !          ! setup source grid and field and target points
  !          < ... >
  !
  !          ! create grid-search-utility object for source grid
  !          gsu = w3gsuc( ijg, llg, iclo, xs, ys )
  !
  !          ! interpolate source field to target points
  !          do k=1,nt
  !            ft(k) = 0
  !            if ( w3grmp( gsu, xt(k), yt(k), is, js, rw ) ) then
  !                do l=1,4
  !                  ft(k) = ft(k) + rw(l)*fs(is(l),js(l))
  !                  end do
  !              end if
  !            end do
  !
  !          ! destroy grid-search-utility object
  !          call w3gsud( gsu )
  !          -----------------------------------------------------------
  !
  !  2. variables and types :
  !
  !     all module variables and types are scoped private by default.
  !     the private module variables and types are not listed in this section.
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      mskc_none i.p.  public   named constant identifying a non-masked
  !                               enclosing grid cell
  !      mskc_part i.p.  public   named constant identifying a partially
  !                               masked enclosing grid cell
  !      mskc_full i.p.  public   named constant identifying a fully
  !                               masked enclosing grid cell
  !      iclo_none i.p.  public   named constant identifying a grid with
  !                               no closure in index space
  !      iclo_smpl i.p.  public   synonym for iclo_grdi
  !      iclo_grdi i.p.  public   named constant identifying a grid with
  !                               closure in i-index: (ubx+1, j) => (lbx, j)
  !      iclo_grdj i.p.  public   named constant identifying a grid with
  !                               closure in j-index: (i, uby+1) => (i, lby)
  !      iclo_trdl i.p.  public   named constant identifying a grid with
  !                               toroidal closure: (ubx+1, j) => (lbx, j) and
  !                                                 (i, uby+1) => (i, lby)
  !      iclo_trpl i.p.  public   named constant identifying a grid with
  !                               tripole closure: (ubx+1, lby<=j<=uby) => (lbx, j)
  !                               and (lbx<=i<=ubx, uby+1) => (ubx+lbx-i, uby)
  !      t_gsu     type  public   grid-search-utility type (opaque)
  !      t_nns     type  public   nearest-neighbor grid-point search type
  !     ----------------------------------------------------------------
  !
  !  3. subroutines and functions :
  !
  !     all module subroutines and functions are scoped private by default.
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3gsuc    func. public   create grid-search-utility object.
  !      w3gsud    subr. public   destroy grid-search-utility object.
  !      w3gsup    subr. public   print grid-search-utility object to stdout.
  !      w3bbox    subr. public   get bounding box associated with grid.
  !      w3gfcl    func. public   find grid cell that encloses target point (bucket search).
  !      w3gfcd    func. public   find grid cell that encloses target point (direct search).
  !      w3gfpt    func. public   find grid point that is closest to target point.
  !      w3gfij    func. public   compute coord of target point in source grid index space
  !      w3grmp    func. public   compute bilinear interpolation coeff. from grid.
  !      w3grmc    func. public   compute remapping coeff. from grid.
  !      w3ckcl    func. public   check if point lies within grid cell.
  !      w3cgdm    func. public   compute curvilinear grid derivatives and metric
  !      w3grd0    func. public   compute gradient of scalar field
  !      w3div1    func. public   compute divergence of a vector field
  !      w3div2    func. public   compute divergence of a tensor field
  !      w3dist    func. public   compute distance between two points.
  !      w3splx    func. public   compute cartesian coord using stereographic projection
  !      w3spxl    func. public   compute (lon,lat) coord using stereographic projection
  !      w3trll    func. public   compute (lon,lat) in rotated coordinate system
  !      w3llaz    func. public   compute azimuth for pair of (lon,lat) points
  !      w3fdwt    func. public   compute finite-difference weights.
  !      w3nnsc    func. public   create nearest-neighbor-search object.
  !      w3nnsd    subr. public   destroy nearest-neighbor-search object.
  !      w3nnsp    subr. public   print nearest-neighbor-search object to stdout.
  !      w3sort    subr. public   sort input arrays in increasing order.
  !      w3isrt    subr. public   insert data into array.
  !      w3inan    func. public   check if input is infinite or nan.
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
  !     - the gsu object is an "opaque" object.  this means that the
  !       internals of the object are not accessible outside this module.
  !     - the burden is upon the user to invoke the destroy method when
  !       finished with a gsu object.  if created gsu objects are
  !       not properly destroyed, then memory leaks may be introduced.
  !
  !  6. switches :
  !
  !     !/s    enable subroutine tracing.
  !
  !  7. source code :
  !
  !/ =================================================================== /
  !/
  !/ use associated modules
  !/
  use w3servmd, only: extcde
  !/
  !/ specify default data typing
  !/
  implicit none
  !/
  !/ specify default accessibility
  !/
  private
  !/
  !/ public module methods
  !/
  public w3gsuc
  public w3gsud
  public w3gsup
  public w3bbox
  public w3gfcl
  public w3gfcd
  public w3gfpt
  public w3gfij
  public w3grmp
  public w3grmc
  public w3ckcl
  public w3cgdm
  public w3grd0
  public w3div1
  public w3div2
  public w3dist
  public w3splx
  public w3spxl
  public w3trll
  public w3llaz
  public w3fdwt
  public w3nnsc
  public w3nnsd
  public w3nnsp
  public w3sort
  public w3isrt
  public w3inan
  !/
  !/ public return codes
  !/
  integer, parameter, public :: mskc_none = 0
  integer, parameter, public :: mskc_part = 1
  integer, parameter, public :: mskc_full = 2
  !/
  !/ public index closure types (for lat/lon grids only)
  !/   iclo_none : no closure in index space
  !/   iclo_smpl : synonym for iclo_grdi
  !/   iclo_grdi : closure in i-index at i=ubx+1: (ubx+1, j) => (lbx, j)
  !/   iclo_grdj : closure in j-index at j=uby+1: (i, uby+1) => (i, lby)
  !/   iclo_trdl : toroidal grid closure: (ubx+1, j) => (lbx, j) and
  !/                                      (i, uby+1) => (i, lby)
  !/   iclo_trpl : tripole grid closure: (ubx+1, lby<=j<=uby) => (lbx, j) and
  !/                                     (lbx<=i<=ubx, uby+1) => (ubx+lbx-i, uby)
  !/
  !/   note that simple i-index closure types are set to multiples of 2.
  !/   note that simple j-index closure types are set to multiples of 3.
  !/   these settings are used in the gsu methods to simplify checking.
  !/
  !/   implementation notes on index closure:
  !/   simple closure in i-index means that a given integer i' is mapped to the
  !/   range [lbx,ubx].  when i' >= lbx, the function i = lbx + mod(i'-lbx,nx)
  !/   maps i' to i in [lbx,ubx] (where, nx = ubx - lbx + 1).  the function
  !/   i = ubx + mod(i'-lbx+1,nx) maps any integer i' to i in [lbx,inf).  hence,
  !/   the following composition is used to map any integer i' to [lbx,ubx].
  !/           i = lbx + mod(nx - 1 + mod(i' - lbx + 1, nx), nx)
  !/   similarly, for simple closure in j-index, the following composition is used
  !/   to map any integer j' to [lby,uby].
  !/           j = lby + mod(ny - 1 + mod(j' - lby + 1, ny), ny)
  !/   for tripole type index closure, the simple closure in i-index is appied
  !/   prior to computing the appropriate i and j-index mapping for closure across
  !/   the seam at j = uby.  the j-index closure for i' in [lbx,ubx] and j' > uby
  !/   is computed as i = ubx + lbx - i' and j = 2*uby - j' + 1.
  !/
  integer, parameter, public :: iclo_none = -1
  integer, parameter, public :: iclo_smpl =  2
  integer, parameter, public :: iclo_grdi =  iclo_smpl
  integer, parameter, public :: iclo_grdj =  3
  integer, parameter, public :: iclo_trdl =  6
  integer, parameter, public :: iclo_trpl =  8
  !/
  !/ public grid-search-utility type
  !/ this is an opaque type -- that is, it's internals are private and only
  !/ accessible to subroutines in this module where the type is declared.
  !/
  type, public :: t_gsu
    private
    type(class_gsu), pointer :: ptr => null()
  end type t_gsu
  !/
  !/ private grid-search-utility class
  !/
  type :: class_gsu
    logical :: ijg  ! grid array ordering flag: t = (nx,ny), f = (ny,nx)
    logical :: llg  ! spherical coordinate flag of associated grid
    integer :: iclo ! parameter indicating type of index space closure
    ! this flag must be set by the user
    logical :: lclo ! flag indicating longitudinal periodicity
    ! this flag is calculated internally
    ! llg & iclo != iclo_none => lclo = t
    logical :: l360 ! flag indicating longitude range:
    !   t = [0:360],  f = [-180:180]
    integer :: gkind  ! kind (precision: 4 or 8) of associated grid
    integer :: lbx, lby ! lower-bounds of associated grid
    integer :: ubx, uby ! upper-bounds of associated grid
    integer :: nx, ny   ! dimensions of associated grid
    real(4), pointer :: xg4(:,:), yg4(:,:) ! coordinates of associated grid (r4)
    real(8), pointer :: xg8(:,:), yg8(:,:) ! coordinates of associated grid (r8)
    type(t_nns), pointer :: nnp  ! nearest-neighbor point search indices object
    integer :: nbx, nby  ! number of buckets in each spatial direction
    real(8) :: dxb, dyb  ! spatial extent of each search bucket
    real(8) :: xmin, ymin, xmax, ymax ! bounding box of search domain
    type(t_bkt), pointer :: b(:,:) ! array of search buckets
    type(t_nns), pointer :: nnb  ! nearest-neighbor bucket search indices object
  end type class_gsu
  !/
  !/ private search bucket type
  !/
  type :: t_bkt
    integer :: n  ! number of cells in bucket
    integer, pointer :: i(:)  ! i-index of cell c
    integer, pointer :: j(:)  ! j-index of cell c
  end type t_bkt
  !/
  !/ public nearest-neighbor grid-point search type
  !/
  type, public :: t_nns
    integer :: nlvl  ! number of nnbr levels
    integer :: nnbr  ! total number of nnbr's
    integer, pointer :: n1(:)  ! starting nearest-nbr loop index for level l
    integer, pointer :: n2(:)  ! ending nearest-nbr loop index for level l
    integer, pointer :: di(:)  ! i-index delta for nearest-nbr n
    integer, pointer :: dj(:)  ! j-index delta for nearest-nbr n
  end type t_nns
  !/
  !/ private module parameters
  !/
  real(8), parameter :: pi = 3.14159265358979323846d0
  real(8), parameter :: pi2 = 2d0*pi
  real(8), parameter :: pi3h = 3d0*pi/2d0
  real(8), parameter :: pio2 = pi/2d0
  real(8), parameter :: pio4 = pi/4d0
  real(8), parameter :: d2r = pi/180d0
  real(8), parameter :: r2d = 1d0/d2r
  real(8), parameter :: d360 = 360d0
  real(8), parameter :: d270 = 270d0
  real(8), parameter :: d180 = 180d0
  real(8), parameter :: d90  =  90d0
  real(8), parameter :: zero = 0.0d0
  real(8), parameter :: half = 0.5d0
  real(8), parameter :: one  = 1.0d0
  real(8), parameter :: two  = 2.0d0
  real(8), parameter :: four = 4.0d0
  real(8), parameter :: rearth = 4.d7/pi2 !this gives d2m = 111111.111111
  real(8), parameter :: d2m  = rearth*d2r
  real(8), parameter :: m2d  = 1d0/d2m
  !     default small non-zero tolerance used to check if
  !     target point is in domain and for point coincidence.
  real(8), parameter :: eps_default  = 1.0d-6
  !     distance (deg) from pole to consider a cell "near the pole"
  real(8), parameter :: near_pole = 10.0d0
  !     default number of grid cells (in each direction) per search bucket.
  integer, parameter :: ncb_default = 10
  !     default maximum number of nearest-neighbor grid point search levels.
  integer, parameter :: nnp_default = 2
  !     max number of non-empty levels for bucket search when target point
  !     is outside source domain
  integer, parameter :: max_fncl_level = 3
  !     default finite-difference order
  integer, parameter :: nfd_default = 4
  !/
  !/ module interfaces
  !/
  interface w3gsuc
    module procedure w3gsuc_ptr_r4
    module procedure w3gsuc_ptr_r8
    module procedure w3gsuc_tgt_r4
    module procedure w3gsuc_tgt_r8
  end interface w3gsuc
  interface w3bbox
    module procedure w3bbox_gsu
    module procedure w3bbox_grd_ptr_r4
    module procedure w3bbox_grd_ptr_r8
    module procedure w3bbox_grd_tgt_r4
    module procedure w3bbox_grd_tgt_r8
  end interface w3bbox
  interface w3gfcl
    module procedure w3gfcl_r4
    module procedure w3gfcl_r8
  end interface w3gfcl
  interface w3gfcd
    module procedure w3gfcd_r4
    module procedure w3gfcd_r8
  end interface w3gfcd
  interface w3gfpt
    module procedure w3gfpt_r4
    module procedure w3gfpt_r8
  end interface w3gfpt
  interface w3gfij
    module procedure w3gfij_r4
    module procedure w3gfij_r8
  end interface w3gfij
  interface w3grmp
    module procedure w3grmp_r4
    module procedure w3grmp_r8
  end interface w3grmp
  interface w3grmc
    module procedure w3grmc_r4
    module procedure w3grmc_r8
  end interface w3grmc
  interface w3cgdm
    module procedure w3cgdm_r4
    module procedure w3cgdm_r8
  end interface w3cgdm
  interface w3grd0
    module procedure w3grd0_r4
    module procedure w3grd0_r8
  end interface w3grd0
  interface w3div1
    module procedure w3div1_r4
    module procedure w3div1_r8
  end interface w3div1
  interface w3div2
    module procedure w3div2_r4
    module procedure w3div2_r8
  end interface w3div2
  interface w3dist
    module procedure w3dist_r4
    module procedure w3dist_r8
  end interface w3dist
  interface w3splx
    module procedure w3splx_0d_r4
    module procedure w3splx_0d_r8
    module procedure w3splx_1d_r4
    module procedure w3splx_1d_r8
    module procedure w3splx_2d_r4
    module procedure w3splx_2d_r8
  end interface w3splx
  interface w3spxl
    module procedure w3spxl_0d_r4
    module procedure w3spxl_0d_r8
    module procedure w3spxl_1d_r4
    module procedure w3spxl_1d_r8
    module procedure w3spxl_2d_r4
    module procedure w3spxl_2d_r8
  end interface w3spxl
  interface w3trll
    module procedure w3trll_0d_r4
    module procedure w3trll_0d_r8
    module procedure w3trll_1d_r4
    module procedure w3trll_1d_r8
    module procedure w3trll_2d_r4
    module procedure w3trll_2d_r8
  end interface w3trll
  interface w3llaz
    module procedure w3llaz_r4
    module procedure w3llaz_r8
  end interface w3llaz
  interface w3fdwt
    module procedure w3fdwt_r4
    module procedure w3fdwt_r8
  end interface w3fdwt
  interface w3ckcl
    module procedure w3ckcl_r4
    module procedure w3ckcl_r8
  end interface w3ckcl
  interface w3sort
    module procedure w3sort_r4
    module procedure w3sort_r8
  end interface w3sort
  interface w3isrt
    module procedure w3isrt_r4
    module procedure w3isrt_r8
  end interface w3isrt
  interface w3inan
    module procedure w3inan_r4
    module procedure w3inan_r8
  end interface w3inan
  !/
contains
  !/
  !/ =================================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    function w3gsuc( ijg, llg, iclo, xg, yg, &
  !/                     ncb, nnp, debug ) result(gsu)
  !/      or
  !/    function w3gsuc( ijg, llg, iclo, lb, ub, xg, yg, &
  !/                     ncb, nnp, debug ) result(gsu)
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     create grid-search-utility (gsu) object for a logically rectangular
  !     grid defined by the input coordinates.
  !
  !  2. method :
  !
  !  3. parameters :
  !
  !     return parameter
  !     ----------------------------------------------------------------
  !       gsu     type   o   created grid-search-utility object.
  !     ----------------------------------------------------------------
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !       ijg     log.   i   logical flag indicating ordering of input
  !                          coord. arrays: t = (nx,ny) and f = (ny,nx).
  !       llg     log.   i   logical flag indicating the coordinate system:
  !                          t = spherical lat/lon (degrees) and f = cartesian.
  !       iclo    int.   i   parameter indicating type of index space closure
  !
  !     inputs (for w3gsuc_ptr):
  !       xg      r.a.   i   pointer to array of x-coordinates of input grid.
  !       yg      r.a.   i   pointer to array of y-coordinates of input grid.
  !
  !     inputs (for w3gsuc_tgt):
  !       lb      i.a.   i   lower bounds of xg and yg arrays
  !       ub      i.a.   i   upper bounds of xg and yg arrays
  !       xg      r.a.   i   array of x-coordinates of input grid.
  !       yg      r.a.   i   array of y-coordinates of input grid.
  !
  !       ncb     int.   i   optional (approximate) number of cells (in each
  !                          direction) per search bucket. (default is ncb_default)
  !                          ncb >= 1 is required.  ncb = 1 gives most efficient
  !                          searching, but uses more memory. increasing ncb leads
  !                          to fewer buckets (less memory) but slower searching.
  !       nnp     int.   i   optional maximum number of nearest-neighbor grid
  !                          point search levels. (default is nnp_default)
  !       debug   log.   i   optional logical flag to turn on debug mode.
  !                          default is false.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
  !
  !     - check on correct coordinate system with global grid.
  !     - check on association of input grid coordinate array pointers.
  !
  !  7. remarks :
  !
  !     - lclo is calculated internally.
  !     - llg & iclo != iclo_none => lclo = t.
  !     - periodic cartesian grids are not allowed.
  !
  !  8. structure :
  !
  !     -----------------------------------------------------------------
  !      1.  test input
  !      2.  allocate object and set grid related data and pointers
  !      3.  create nearest-neighbor point search object
  !      4.  construct bucket search "object"
  !      5.  set return parameter
  !     -----------------------------------------------------------------
  !
  !  9. switches :
  !
  !     !/s    enable subroutine tracing.
  !
  ! 10. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function w3gsuc_ptr_r4( ijg, llg, iclo, xg, yg, &
       ncb, nnp, debug ) result(gsu)
    !     single precision pointer interface
    type(t_gsu)         :: gsu
    logical, intent(in) :: ijg
    logical, intent(in) :: llg
    integer, intent(in) :: iclo
    real(4), pointer    :: xg(:,:)
    real(4), pointer    :: yg(:,:)
    integer, intent(in), optional :: ncb
    integer, intent(in), optional :: nnp
    logical, intent(in), optional :: debug
    !     local parameters
    integer :: lb(2), ub(2)
    !
    lb(1) = lbound(xg,1); lb(2) = lbound(xg,2)
    ub(1) = ubound(xg,1); ub(2) = ubound(xg,2)
    gsu = gsu_create( ijg, llg, iclo, lb, ub, xg4=xg, yg4=yg, &
         ncb=ncb, nnp=nnp, debug=debug)
  end function w3gsuc_ptr_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function w3gsuc_ptr_r8( ijg, llg, iclo, xg, yg, &
       ncb, nnp, debug ) result(gsu)
    !     double precision pointer interface
    type(t_gsu)         :: gsu
    logical, intent(in) :: ijg
    logical, intent(in) :: llg
    integer, intent(in) :: iclo
    real(8), pointer    :: xg(:,:)
    real(8), pointer    :: yg(:,:)
    integer, intent(in), optional :: ncb
    integer, intent(in), optional :: nnp
    logical, intent(in), optional :: debug
    !     local parameters
    integer :: lb(2), ub(2)
    !
    lb(1) = lbound(xg,1); lb(2) = lbound(xg,2)
    ub(1) = ubound(xg,1); ub(2) = ubound(xg,2)
    gsu = gsu_create( ijg, llg, iclo, lb, ub, xg8=xg, yg8=yg, &
         ncb=ncb, nnp=nnp, debug=debug)
  end function w3gsuc_ptr_r8
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function w3gsuc_tgt_r4( ijg, llg, iclo, lb, ub, xg, yg, &
       ncb, nnp, debug ) result(gsu)
    !     single precision target interface
    type(t_gsu)         :: gsu
    logical, intent(in) :: ijg
    logical, intent(in) :: llg
    integer, intent(in) :: iclo
    integer, intent(in) :: lb(2)
    integer, intent(in) :: ub(2)
    real(4), target     :: xg(lb(1):ub(1),lb(2):ub(2))
    real(4), target     :: yg(lb(1):ub(1),lb(2):ub(2))
    integer, intent(in), optional :: ncb
    integer, intent(in), optional :: nnp
    logical, intent(in), optional :: debug
    !     local parameters
    !
    gsu = gsu_create( ijg, llg, iclo, lb, ub, xg4=xg, yg4=yg, &
         ncb=ncb, nnp=nnp, debug=debug)
  end function w3gsuc_tgt_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function w3gsuc_tgt_r8( ijg, llg, iclo, lb, ub, xg, yg, &
       ncb, nnp, debug ) result(gsu)
    !     double precision target interface
    type(t_gsu)         :: gsu
    logical, intent(in) :: ijg
    logical, intent(in) :: llg
    integer, intent(in) :: iclo
    integer, intent(in) :: lb(2)
    integer, intent(in) :: ub(2)
    real(8), target     :: xg(lb(1):ub(1),lb(2):ub(2))
    real(8), target     :: yg(lb(1):ub(1),lb(2):ub(2))
    integer, intent(in), optional :: ncb
    integer, intent(in), optional :: nnp
    logical, intent(in), optional :: debug
    !     local parameters
    !
    gsu = gsu_create( ijg, llg, iclo, lb, ub, xg8=xg, yg8=yg, &
         ncb=ncb, nnp=nnp, debug=debug)
  end function w3gsuc_tgt_r8
  !/
  !/ end of w3gsuc ===================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    subroutine w3gsud( gsu )
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     destroy grid search utility (gsu) object.
  !
  !  2. method :
  !
  !  3. parameters :
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !       gsu     type   i   grid-search-utility object.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
  !
  !     - check on previous creation of grid-search-utility object.
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
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3gsud( gsu )
    type(t_gsu), intent(inout) :: gsu
    !     local parameters
    integer :: ib, jb
    !
    if ( associated(gsu%ptr) ) then
      !
      call w3nnsd(gsu%ptr%nnp)
      !
      if ( associated(gsu%ptr%b) ) then
        do ib=1,gsu%ptr%nbx
          do jb=1,gsu%ptr%nby
            if ( gsu%ptr%b(jb,ib)%n .gt. 0 ) then
              deallocate(gsu%ptr%b(jb,ib)%i)
              nullify(gsu%ptr%b(jb,ib)%i)
              deallocate(gsu%ptr%b(jb,ib)%j)
              nullify(gsu%ptr%b(jb,ib)%j)
            end if
          end do
        end do
        deallocate(gsu%ptr%b)
        nullify(gsu%ptr%b)
      end if
      !
      call w3nnsd(gsu%ptr%nnb)
      !
      deallocate(gsu%ptr)
      nullify(gsu%ptr)
      !
    end if
  end subroutine w3gsud
  !/
  !/ end of w3gsud ===================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    subroutine w3gsup( gsu, iunit, lfull )
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     print grid-search-utility (gsu) object to iunit.
  !
  !  2. method :
  !
  !  3. parameters :
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !       gsu     type   i   grid-search-utility object.
  !       iunit   int.   i   optional unit for output. default is stdout.
  !       lfull   log.   i   optional logical flag to turn on full-output
  !                          mode.  default is false.  when full-output
  !                          is enabled the search bucket cell lists and
  !                          nearest-neighbor point search indices are output.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
  !
  !     - check on previous creation of grid-search-utility object.
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
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3gsup( gsu, iunit, lfull )
    type(t_gsu), intent(in) :: gsu
    integer, optional, intent(in) :: iunit
    logical, optional, intent(in) :: lfull
    !     local parameters
    integer, parameter :: nbyte_ptr=4
    integer, parameter :: nbyte_int=4
    type(class_gsu), pointer :: ptr
    integer :: ndst, k, ib, jb, nbyte
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input
    !
    if ( .not.associated(gsu%ptr) ) then
      write(0,'(/1a,1a/)') 'w3gsup error -- ', &
           'grid search utility object not created'
      call extcde (1)
    end if
    if ( present(iunit) ) then
      ndst = iunit
    else
      ndst = 6
    end if
    ptr => gsu%ptr
    !
    ! -------------------------------------------------------------------- /
    ! 2.  compute approximate memory usage
    !
    nbyte = (nbyte_int+nbyte_ptr*2)*size(ptr%b)
    do ib=1,ptr%nbx
      do jb=1,ptr%nby
        nbyte = nbyte + nbyte_int*2*ptr%b(jb,ib)%n
      end do
    end do
    !
    ! -------------------------------------------------------------------- /
    ! 3.  output
    !
    write(ndst,'(//80a)') ('-',k=1,80)
    write(ndst,'(a)') 'report on grid search utility object'
    write(ndst,'( 80a)') ('-',k=1,80)
    write(ndst,'(a,1l2)') 'grid  ijg:',ptr%ijg
    write(ndst,'(a,1l2)') 'grid  llg:',ptr%llg
    write(ndst,'(a,1i2)') 'grid iclo:',ptr%iclo
    write(ndst,'(a,1l2)') 'grid lclo:',ptr%lclo
    write(ndst,'(a,1i2)') 'grid precision:',ptr%gkind
    write(ndst,'(a,2i6)') 'grid lbx,lby:',ptr%lbx,ptr%lby
    write(ndst,'(a,2i6)') 'grid ubx,uby:',ptr%ubx,ptr%uby
    write(ndst,'(a,2i6)') 'grid  nx, ny:',ptr%nx,ptr%ny
    if ( present(lfull) ) then
      if ( lfull ) then
        write(ndst,'( 80a)') ('-',k=1,80)
        write(ndst,'(a)') 'nearest-neighbor point search indices'
        write(ndst,'( 80a)') ('-',k=1,80)
        call w3nnsp(ptr%nnp,ndst)
      end if
    end if
    write(ndst,'( 80a)') ('-',k=1,80)
    write(ndst,'(a)') 'bucket-search object'
    write(ndst,'( 80a)') ('-',k=1,80)
    write(ndst,'(a,4e24.16)') 'spatial grid search domain: ', &
         ptr%xmin,ptr%ymin,ptr%xmax,ptr%ymax
    write(ndst,'(a,2i6)') 'nbx,nby:',ptr%nbx,ptr%nby
    write(ndst,'(a,2e24.16)') 'dxb,dyb:',ptr%dxb,ptr%dyb
    write(ndst,'(a,1f10.1)') 'approximate memory usage (mb):', &
         real(nbyte)/2**20
    if ( present(lfull) ) then
      if ( lfull ) then
        write(ndst,'( 80a)') ('-',k=1,80)
        write(ndst,'(a)') 'search bucket bounds:'
        write(ndst,'( 80a)') ('-',k=1,80)
        write(ndst,'(2a4,4a24)') 'ib','jb','x1','y1','x2','y2'
        do ib=1,ptr%nbx
          do jb=1,ptr%nby
            write(ndst,'(2i4,4e24.16)') ib,jb, &
                 ptr%xmin+(ib-1)*ptr%dxb,ptr%ymin+(jb-1)*ptr%dyb, &
                 ptr%xmin+(ib  )*ptr%dxb,ptr%ymin+(jb  )*ptr%dyb
          end do
        end do
        write(ndst,'( 80a)') ('-',k=1,80)
        write(ndst,'(a)') 'number of cells in each search bucket:'
        write(ndst,'( 80a)') ('-',k=1,80)
        do jb=ptr%nby,1,-1
          write(ndst,'(500i4)') (ptr%b(jb,ib)%n,ib=1,ptr%nbx)
        end do
        write(ndst,'( 80a)') ('-',k=1,80)
        write(ndst,'(a)') 'search bucket cell lists:'
        write(ndst,'( 80a)') ('-',k=1,80)
        write(ndst,'(3a4,a)') 'ib','jb','nc',': ( ic, jc), ...'
        do jb=1,ptr%nby
          do ib=1,ptr%nbx
            write(ndst,'(3i4,a,500(a,i3,a,i3,a))') ib,jb, &
                 ptr%b(jb,ib)%n, ': ', &
                 ( '(',ptr%b(jb,ib)%i(k),',',ptr%b(jb,ib)%j(k),') ', &
                 k=1,ptr%b(jb,ib)%n )
          end do
        end do
        write(ndst,'( 80a)') ('-',k=1,80)
        write(ndst,'(a)') 'nearest-neighbor bucket search indices'
        write(ndst,'( 80a)') ('-',k=1,80)
        call w3nnsp(ptr%nnb,ndst)
      end if !lfull
    end if !present(lfull)
    write(ndst,'( 80a)') ('-',k=1,80)
    write(ndst,'( 80a)') ('-',k=1,80)
  end subroutine w3gsup
  !/
  !/ end of w3gsup ===================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    subroutine w3bbox( gsu, xmin, ymin, xmax, ymax )
  !/      or
  !/    subroutine w3bbox( ijg, llg, iclo, xg, yg, xmin, ymin, xmax, ymax )
  !/      or
  !/    subroutine w3bbox( ijg, llg, iclo, lb, ub, xg, yg, xmin, ymin, xmax, ymax )
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     get bounding box associated with grid.
  !
  !  2. method :
  !
  !  3. parameters :
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !     inputs (for w3bbox_gsu):
  !       gsu     type   i   grid-search-utility object
  !
  !     inputs (for w3bbox_grd_ptr):
  !       ijg     log.   i   logical flag indicating ordering of input
  !                          coord. arrays: t = (nx,ny) and f = (ny,nx).
  !       llg     log.   i   logical flag indicating the coordinate system:
  !                          t = spherical lat/lon (degrees) and f = cartesian.
  !       iclo    int.   i   parameter indicating type of index space closure
  !       xg      r.a.   i   pointer to array of x-coordinates of input grid.
  !       yg      r.a.   i   pointer to array of y-coordinates of input grid.
  !
  !     inputs (for w3bbox_grd_tgt):
  !       ijg     log.   i   logical flag indicating ordering of input
  !                          coord. arrays: t = (nx,ny) and f = (ny,nx).
  !       llg     log.   i   logical flag indicating the coordinate system:
  !                          t = spherical lat/lon (degrees) and f = cartesian.
  !       iclo    int.   i   parameter indicating type of index space closure
  !       lb      i.a.   i   lower bounds of xg and yg arrays
  !       ub      i.a.   i   upper bounds of xg and yg arrays
  !       xg      r.a.   i   array of x-coordinates of input grid.
  !       yg      r.a.   i   array of y-coordinates of input grid.
  !
  !     outputs:
  !       xmin    int.   o   minimum x-coord of bounding box
  !       ymin    int.   o   minimum y-coord of bounding box
  !       xmax    int.   o   maximum x-coord of bounding box
  !       ymax    int.   o   maximum y-coord of bounding box
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
  !
  !     - check on previous creation of grid-search-utility object.
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
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3bbox_gsu( gsu, xmin, ymin, xmax, ymax )
    type(t_gsu), intent(in) :: gsu
    real(8), intent(out)    :: xmin, ymin, xmax, ymax
    !     local parameters
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input
    !
    if ( .not.associated(gsu%ptr) ) then
      write(0,'(/1a,1a/)') 'w3bbox_gsu error -- ', &
           'grid search utility object not created'
      call extcde (1)
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  set bounding box
    !
    xmin = gsu%ptr%xmin
    ymin = gsu%ptr%ymin
    xmax = gsu%ptr%xmax
    ymax = gsu%ptr%ymax
  end subroutine w3bbox_gsu
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3bbox_grd_ptr_r4( ijg, llg, iclo, xg, yg, &
       xmin, ymin, xmax, ymax )
    logical, intent(in)     :: ijg
    logical, intent(in)     :: llg
    integer, intent(in)     :: iclo
    real(4), pointer        :: xg(:,:)
    real(4), pointer        :: yg(:,:)
    real(8), intent(out)    :: xmin, ymin, xmax, ymax
    !     local parameters
    type(t_gsu) :: gsu
    integer :: lb(2), ub(2)
    !
    ! -------------------------------------------------------------------- /
    ! 1.  set bounding box
    !
    lb(1) = lbound(xg,1); lb(2) = lbound(xg,2)
    ub(1) = ubound(xg,1); ub(2) = ubound(xg,2)
    gsu = gsu_create( ijg, llg, iclo, lb, ub, xg4=xg, yg4=yg, bbox_only=.true. )
    xmin = gsu%ptr%xmin
    ymin = gsu%ptr%ymin
    xmax = gsu%ptr%xmax
    ymax = gsu%ptr%ymax
    call w3gsud( gsu )
  end subroutine w3bbox_grd_ptr_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3bbox_grd_ptr_r8( ijg, llg, iclo, xg, yg, &
       xmin, ymin, xmax, ymax )
    logical, intent(in)     :: ijg
    logical, intent(in)     :: llg
    integer, intent(in)     :: iclo
    real(8), pointer        :: xg(:,:)
    real(8), pointer        :: yg(:,:)
    real(8), intent(out)    :: xmin, ymin, xmax, ymax
    !     local parameters
    type(t_gsu) :: gsu
    integer :: lb(2), ub(2)
    !
    ! -------------------------------------------------------------------- /
    ! 1.  set bounding box
    !
    lb(1) = lbound(xg,1); lb(2) = lbound(xg,2)
    ub(1) = ubound(xg,1); ub(2) = ubound(xg,2)
    gsu = gsu_create( ijg, llg, iclo, lb, ub, xg8=xg, yg8=yg, bbox_only=.true. )
    xmin = gsu%ptr%xmin
    ymin = gsu%ptr%ymin
    xmax = gsu%ptr%xmax
    ymax = gsu%ptr%ymax
    call w3gsud( gsu )
  end subroutine w3bbox_grd_ptr_r8
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3bbox_grd_tgt_r4( ijg, llg, iclo, lb, ub, xg, yg, &
       xmin, ymin, xmax, ymax )
    logical, intent(in)     :: ijg
    logical, intent(in)     :: llg
    integer, intent(in)     :: iclo
    integer, intent(in)     :: lb(2), ub(2)
    real(4), target         :: xg(lb(1):ub(1),lb(2):ub(2))
    real(4), target         :: yg(lb(1):ub(1),lb(2):ub(2))
    real(8), intent(out)    :: xmin, ymin, xmax, ymax
    !     local parameters
    type(t_gsu) :: gsu
    !
    ! -------------------------------------------------------------------- /
    ! 1.  set bounding box
    !
    gsu = gsu_create( ijg, llg, iclo, lb, ub, xg4=xg, yg4=yg, bbox_only=.true. )
    xmin = gsu%ptr%xmin
    ymin = gsu%ptr%ymin
    xmax = gsu%ptr%xmax
    ymax = gsu%ptr%ymax
    call w3gsud( gsu )
  end subroutine w3bbox_grd_tgt_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3bbox_grd_tgt_r8( ijg, llg, iclo, lb, ub, xg, yg, &
       xmin, ymin, xmax, ymax )
    logical, intent(in)     :: ijg
    logical, intent(in)     :: llg
    integer, intent(in)     :: iclo
    integer, intent(in)     :: lb(2), ub(2)
    real(8), target         :: xg(lb(1):ub(1),lb(2):ub(2))
    real(8), target         :: yg(lb(1):ub(1),lb(2):ub(2))
    real(8), intent(out)    :: xmin, ymin, xmax, ymax
    !     local parameters
    type(t_gsu) :: gsu
    !
    ! -------------------------------------------------------------------- /
    ! 1.  set bounding box
    !
    gsu = gsu_create( ijg, llg, iclo, lb, ub, xg8=xg, yg8=yg, bbox_only=.true. )
    xmin = gsu%ptr%xmin
    ymin = gsu%ptr%ymin
    xmax = gsu%ptr%xmax
    ymax = gsu%ptr%ymax
    call w3gsud( gsu )
  end subroutine w3bbox_grd_tgt_r8
  !/
  !/ end of w3bbox ===================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    function w3gfcl( gsu, xt, yt, is, js, xs, ys, &
  !/                     pole, eps, fncl, debug ) result(ingrid)
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     find cell in grid, associated with the input grid-search-utility
  !     object (gsu), that encloses the target point (xt,yt).
  !
  !  2. method :
  !
  !  3. parameters :
  !
  !     return parameter
  !     ----------------------------------------------------------------
  !       ingrid  log.   o   logical flag indicating if target point lies
  !                          within the source grid domain.
  !     ----------------------------------------------------------------
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !       gsu     type   i   grid-search-utility object.
  !       xt      real   i   x-coordinate of target point.
  !       yt      real   i   y-coordinate of target point.
  !       is,js   i.a.   o   (i,j) indices of vertices of enclosing grid cell.
  !       xs,ys   r.a.   o   (x,y) coord. of vertices of enclosing grid cell.
  !       pole    log.   o   optional logical flag to indicate whether or not
  !                          the enclosing grid cell includes a pole.
  !       eps     real   i   optional small non-zero tolerance used to check if
  !                          target point is in domain and for point coincidence.
  !       fncl    log.   i   optional logical flag to enable finding cell that
  !                          is shortest distance from target point when the
  !                          target point is not located in the source grid.
  !                          default is false.
  !       debug   log.   i   optional logical flag to turn on debug mode.
  !                          default is false.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
  !
  !     - check on previous creation of grid-search-utility object.
  !
  !  7. remarks :
  !
  !     - the target point coordinates may be modified by this routine.
  !     - the target point longitude will be shifted to the source grid
  !       longitudinal range.
  !     - if enclosing cell includes a branch cut, then the coordinates of
  !       of the cell vertices and the target point will be adjusted so
  !       that the branch cut is shifted 180 degrees.
  !
  !  8. structure :
  !
  !     -----------------------------------------------------------------
  !      1.  test input
  !      2.  initialize search
  !      3.  search for enclosing cell in central and nearest nbr buckets
  !      4.  if not in grid and find nearest cell is enabled, then
  !          identify cell closest to target point
  !     -----------------------------------------------------------------
  !
  !  9. switches :
  !
  !     !/s    enable subroutine tracing.
  !
  ! 10. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function w3gfcl_r4( gsu, xt, yt, is, js, xs, ys, &
       pole, eps, fncl, debug ) result(ingrid)
    !     single precision interface
    logical                 :: ingrid
    type(t_gsu), intent(in) :: gsu
    real(4), intent(inout)  :: xt
    real(4), intent(inout)  :: yt
    integer, intent(inout)  :: is(4), js(4)
    real(4), intent(inout)  :: xs(4), ys(4)
    logical, intent(out),optional :: pole
    real(4), intent(in), optional :: eps
    logical, intent(in), optional :: fncl
    logical, intent(in), optional :: debug
    !     local parameters
    real(8) :: xt8, yt8, xs8(4), ys8(4), eps8
    !
    !-----set inputs
    xt8 = xt; yt8 = yt;
    if ( present(eps) ) then
      eps8 = eps
    else
      eps8 = eps_default
    end if
    !
    !-----call double precision method
    ingrid = w3gfcl( gsu, xt8, yt8, is, js, xs8, ys8, pole=pole, &
         eps=eps8, fncl=fncl, debug=debug )
    !
    !-----set outputs
    xt = xt8; yt = yt8;
    xs = xs8; ys = ys8;
  end function w3gfcl_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function w3gfcl_r8( gsu, xt, yt, is, js, xs, ys, &
       pole, eps, fncl, debug ) result(ingrid)
    !     double precision interface
    logical                 :: ingrid
    type(t_gsu), intent(in) :: gsu
    real(8), intent(inout)  :: xt
    real(8), intent(inout)  :: yt
    integer, intent(inout)  :: is(4), js(4)
    real(8), intent(inout)  :: xs(4), ys(4)
    logical, intent(out),optional :: pole
    real(8), intent(in), optional :: eps
    logical, intent(in), optional :: fncl
    logical, intent(in), optional :: debug
    !     local parameters
    real(8) :: leps
    logical :: ldbg, lplc, lfncl, incell
    integer :: i, j, k, l, n, ib, jb
    logical :: ijg, llg, lclo, l360
    integer :: iclo, gkind
    integer :: lbx, lby, ubx, uby, nx, ny
    real(4), pointer :: xg4(:,:), yg4(:,:)
    real(8), pointer :: xg8(:,:), yg8(:,:)
    integer :: nbx, nby
    real(8) :: dxb, dyb, xmin, xmax, ymin, ymax
    type(t_bkt), pointer :: b(:,:)
    type(t_nns), pointer :: nnb
    logical :: found
    integer :: nlevel, lvl, lvl1, n1, ib0, jb0, ib1, jb1, k1
    integer :: is1(4), js1(4)
    real(8) :: xs1(4), ys1(4), xsm, ysm, dd, dd1
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input
    !
    if ( .not.associated(gsu%ptr) ) then
      write(0,'(/2a/)') 'w3gfcl_r8 error -- ', &
           'grid search utility object not created'
      call extcde (1)
    end if
    if ( present(eps) ) then
      if ( eps .lt. zero ) then
        write(0,'(/2a/)') 'w3gfcl_r8 error -- ', &
             'eps parameter must be >= 0'
        call extcde (1)
      end if
      leps = eps
    else
      leps = eps_default
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  initialize search
    !
    if ( present(fncl) ) then
      lfncl = fncl
    else
      lfncl = .false.
    end if
    if ( present(debug) ) then
      ldbg = debug
    else
      ldbg = .false.
    end if
    !
    !  local pointers to grid search utility object data
    ijg = gsu%ptr%ijg
    llg = gsu%ptr%llg
    iclo = gsu%ptr%iclo
    lclo = gsu%ptr%lclo
    l360 = gsu%ptr%l360
    gkind = gsu%ptr%gkind
    lbx = gsu%ptr%lbx;  lby = gsu%ptr%lby;
    ubx = gsu%ptr%ubx;  uby = gsu%ptr%uby;
    nx = gsu%ptr%nx;  ny = gsu%ptr%ny;
    if ( gkind.eq.4 ) then
      xg4 => gsu%ptr%xg4;  yg4 => gsu%ptr%yg4;
    else
      xg8 => gsu%ptr%xg8;  yg8 => gsu%ptr%yg8;
    end if
    nbx = gsu%ptr%nbx;  nby = gsu%ptr%nby;
    dxb = gsu%ptr%dxb;  dyb = gsu%ptr%dyb;
    xmin = gsu%ptr%xmin;  ymin = gsu%ptr%ymin;
    xmax = gsu%ptr%xmax;  ymax = gsu%ptr%ymax;
    b => gsu%ptr%b
    nnb => gsu%ptr%nnb
    !
    ingrid = .false.
    !
    !  shift target to appropriate longitude range
    if ( llg ) then
      xt = mod(xt,real(d360,8))
      if ( lclo .or. l360 ) then
        if ( xt.lt.zero ) xt = xt + d360
      else
        if ( xt.gt.d180 ) xt = xt - d360
      end if
    end if
    if ( ldbg ) write(*,'(/a,2e24.16)') 'w3gfcl_r8 - target point:',xt,yt
    !
    !  target point must lie within search domain
    if ( .not.lfncl ) then
      if ( xt.lt.xmin-leps .or. xt.gt.xmax+leps .or. &
           yt.lt.ymin-leps .or. yt.gt.ymax+leps ) then
        if ( ldbg ) write(*,'(a)') &
             'w3gfcl_r8 - target point outside search domain'
        return
      end if
    end if
    !
    !  search bucket that contains the target point.
    ib = max(int((xt-xmin)/dxb)+1,1);  if ( .not.lclo ) ib = min(ib,nbx);
    jb = max(int((yt-ymin)/dyb)+1,1);                   jb = min(jb,nby);
    !
    ! -------------------------------------------------------------------- /
    ! 3.  search for enclosing cell in bucket
    !
    if ( ldbg ) &
         write(*,'(a,3i6,4e24.16)') &
         'w3gfcl_r8 - bucket search:',ib,jb,b(jb,ib)%n, &
         xmin+(ib-1)*dxb,ymin+(jb-1)*dyb,xmin+ib*dxb,ymin+jb*dyb
    cell_loop: do k=1,b(jb,ib)%n
      !---------setup cell corner indices
      is(1) = b(jb,ib)%i(k)  ;  js(1) = b(jb,ib)%j(k)  ;
      is(2) = b(jb,ib)%i(k)+1;  js(2) = b(jb,ib)%j(k)  ;
      is(3) = b(jb,ib)%i(k)+1;  js(3) = b(jb,ib)%j(k)+1;
      is(4) = b(jb,ib)%i(k)  ;  js(4) = b(jb,ib)%j(k)+1;
      !---------setup cell corner coordinates and adjust for periodicity
      do l=1,4
        !-------------apply index closure
        if ( mod(iclo,2).eq.0 ) &
             is(l) = lbx + mod(nx - 1 + mod(is(l) - lbx + 1, nx), nx)
        if ( mod(iclo,3).eq.0 ) &
             js(l) = lby + mod(ny - 1 + mod(js(l) - lby + 1, ny), ny)
        if ( iclo.eq.iclo_trpl .and. js(l).gt.uby ) then
          is(l) = ubx + lbx - is(l)
          js(l) = 2*uby - js(l) + 1
        end if
        !-------------copy cell vertex coordinates into local variables
        if ( ijg ) then
          if ( gkind.eq.4 ) then
            xs(l) = xg4(is(l),js(l));  ys(l) = yg4(is(l),js(l));
          else
            xs(l) = xg8(is(l),js(l));  ys(l) = yg8(is(l),js(l));
          end if
        else
          if ( gkind.eq.4 ) then
            xs(l) = xg4(js(l),is(l));  ys(l) = yg4(js(l),is(l));
          else
            xs(l) = xg8(js(l),is(l));  ys(l) = yg8(js(l),is(l));
          end if
        end if
        !-------------shift longitudes to same range
        if ( llg ) then
          xs(l) = mod(xs(l),real(d360,8))
          if ( lclo .or. l360 ) then
            if ( xs(l).lt.zero ) xs(l) = xs(l) + d360
          else
            if ( xs(l).gt.d180 ) xs(l) = xs(l) - d360
          end if
        end if
      end do !l
      if ( ldbg ) &
           write(*,'(a,3i6,4(/a,1i1,a,2i6,2e24.16))') &
           'w3gfcl_r8 - check cell:',ib,jb,k, &
           ('          corner(',l,'):',is(l),js(l),xs(l),ys(l),l=1,4)
      !---------check if point is enclosed in cell defined by xs(1:4) & ys(1:4)
      incell = w3ckcl(llg,xt,yt,4,xs,ys,lplc,leps,ldbg)
      if ( ldbg ) write(*,'(a,1l2)')'w3gfcl_r8 - incell:',incell
      if ( incell ) then
        !-------------exit search
        if ( ldbg ) &
             write(*,'(a,3i6,4(2i6))') &
             'w3gfcl_r8 - enclosing cell:',ib,jb,k,(is(l),js(l),l=1,4)
        if ( present(pole) ) pole = lplc
        ingrid = .true.
        exit cell_loop
      end if !point in cell
    end do cell_loop
    if ( ingrid ) return
    if ( .not.lfncl ) return
    !
    ! -------------------------------------------------------------------- /
    ! 4.  if not in grid, then identify cell closest to target point
    !
    !-----find closest cell by searching nearest-neighbor buckets
    nlevel = 0
    dd1 = huge(xt)
    ib0 = ib;  jb0 = jb;
    ib1 = ib;  jb1 = jb;
    nnb = w3nnsc(nint(half*max(nbx,nby)))
    if ( ldbg ) write(*,'(a,3i6)') &
         'w3gfcl_r8 - closest cell search:',ib0,jb0,nnb%nlvl
    level_loop: do lvl=0,nnb%nlvl
      found = .false.
      nnbr_loop: do n=nnb%n1(lvl),nnb%n2(lvl)
        ib = ib0 + nnb%di(n);  jb = jb0 + nnb%dj(n);
        if ( ib.lt.1 .or. ib.gt.nbx ) cycle nnbr_loop
        if ( jb.lt.1 .or. jb.gt.nby ) cycle nnbr_loop
        if ( ldbg ) write(*,'(a,5i6)') &
             'w3gfcl_r8 - check bucket:',lvl,n,ib,jb,b(jb,ib)%n
        cell_loop2: do k=1,b(jb,ib)%n
          !-----------------setup cell corner indices
          is(1) = b(jb,ib)%i(k)  ;  js(1) = b(jb,ib)%j(k)  ;
          is(2) = b(jb,ib)%i(k)+1;  js(2) = b(jb,ib)%j(k)  ;
          is(3) = b(jb,ib)%i(k)+1;  js(3) = b(jb,ib)%j(k)+1;
          is(4) = b(jb,ib)%i(k)  ;  js(4) = b(jb,ib)%j(k)+1;
          !-----------------setup cell corner coordinates and adjust for periodicity
          do l=1,4
            !---------------------apply index closure
            if ( mod(iclo,2).eq.0 ) &
                 is(l) = lbx + mod(nx - 1 + mod(is(l) - lbx + 1, nx), nx)
            if ( mod(iclo,3).eq.0 ) &
                 js(l) = lby + mod(ny - 1 + mod(js(l) - lby + 1, ny), ny)
            if ( iclo.eq.iclo_trpl .and. js(l).gt.uby ) then
              is(l) = ubx + lbx - is(l)
              js(l) = 2*uby - js(l) + 1
            end if
            !---------------------copy cell vertex coordinates into local variables
            if ( ijg ) then
              if ( gkind.eq.4 ) then
                xs(l) = xg4(is(l),js(l));  ys(l) = yg4(is(l),js(l));
              else
                xs(l) = xg8(is(l),js(l));  ys(l) = yg8(is(l),js(l));
              end if
            else
              if ( gkind.eq.4 ) then
                xs(l) = xg4(js(l),is(l));  ys(l) = yg4(js(l),is(l));
              else
                xs(l) = xg8(js(l),is(l));  ys(l) = yg8(js(l),is(l));
              end if
            end if
            !---------------------shift longitudes to same range
            if ( llg ) then
              xs(l) = mod(xs(l),real(d360,8))
              if ( lclo .or. l360 ) then
                if ( xs(l).lt.zero ) xs(l) = xs(l) + d360
              else
                if ( xs(l).gt.d180 ) xs(l) = xs(l) - d360
              end if
            end if
          end do !l
          !-----------------check cell distance from target point
          xsm = sum(xs)/four; ysm = sum(ys)/four;
          dd = w3dist(llg,xt,yt,xsm,ysm)
          if ( ldbg ) &
               write(*,'(a,5i6,3e24.16,4(/a,1i1,a,2i6,2e24.16))') &
               'w3gfcl_r8 - check cell:',lvl,n,ib,jb,k,xsm,ysm,dd, &
               ('          corner(',l,'):',is(l),js(l),xs(l),ys(l),l=1,4)
          if (dd.lt.dd1) then
            lvl1   = lvl
            n1     = n
            ib1    = ib
            jb1    = jb
            k1     = k
            dd1    = dd
            is1(:) = is(:)
            js1(:) = js(:)
            xs1(:) = xs(:)
            ys1(:) = ys(:)
          endif
          found = .true.
        end do cell_loop2
      end do nnbr_loop
      if ( found ) nlevel = nlevel + 1
      if ( nlevel .ge. max_fncl_level ) exit level_loop
    end do level_loop
    !
    !-----return cell that is shortest distance from target point
    is(:) = is1(:)
    js(:) = js1(:)
    xs(:) = xs1(:)
    ys(:) = ys1(:)
    if ( ldbg ) &
         write(*,'(a,5i6,1e24.16,4(/a,1i1,a,2i6,2e24.16))') &
         'w3gfcl_r8 - closest cell:',lvl1,n1,ib1,jb1,k1,dd1, &
         ('          corner(',l,'):',is(l),js(l),xs(l),ys(l),l=1,4)
    !
    !-----check if cell includes a pole or branch cut
    if ( llg ) then
      n = 0
      !---------count longitudinal branch cut crossings
      do i=1,4
        j = mod(i,4) + 1
        if ( abs(xs(j)-xs(i)) .gt. d180 ) n = n + 1
      end do
      !---------single longitudinal branch cut crossing
      !         or single vertex at 90 degrees => cell includes pole
      lplc = n.eq.1 .or. count(abs(ys).eq.d90).eq.1
      if ( lplc .and. ldbg ) &
           write(*,'(a)') 'w3gfcl_r8 - cell includes a pole'
    else
      lplc = .false.
    end if
    if ( present(pole) ) pole = lplc
  end function w3gfcl_r8
  !/
  !/ end of w3gfcl ===================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    function w3gfcd_r4( gsu, xt, yt, is, js, xs, ys, pole, eps, debug ) &
  !/    result(ingrid)
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     find cell in grid, associated with the input grid-search-utility
  !     object (gsu), that encloses the target point (xt,yt), using direct
  !     grid search (i.e., no bucket search).
  !
  !  2. method :
  !
  !  3. parameters :
  !
  !     return parameter
  !     ----------------------------------------------------------------
  !       ingrid  log.   o   logical flag indicating if target point lies
  !                          within the source grid domain.
  !     ----------------------------------------------------------------
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !       gsu     type   i   grid-search-utility object.
  !       xt      real   i   x-coordinate of target point.
  !       yt      real   i   y-coordinate of target point.
  !       is,js   i.a.   o   (i,j) indices of vertices of enclosing grid cell.
  !       xs,ys   r.a.   o   (x,y) coord. of vertices of enclosing grid cell.
  !       pole    log.   o   optional logical flag to indicate whether or not
  !                          the enclosing grid cell includes a pole.
  !       eps     real   i   optional small non-zero tolerance used to check if
  !                          target point is in domain and for point coincidence.
  !       debug   log.   i   optional logical flag to turn on debug mode.
  !                          default is false.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
  !
  !     - check on previous creation of grid-search-utility object.
  !
  !  7. remarks :
  !
  !     - the target point coordinates may be modified by this routine.
  !     - the target point longitude will be shifted to the source grid
  !       longitudinal range.
  !     - if enclosing cell includes a branch cut, then the coordinates of
  !       of the cell vertices and the target point will be adjusted so
  !       that the branch cut is shifted 180 degrees.
  !
  !  8. structure :
  !
  !     -----------------------------------------------------------------
  !      1.  test input
  !      2.  initialize search
  !      3.  search for enclosing cell
  !     -----------------------------------------------------------------
  !
  !  9. switches :
  !
  !     !/s    enable subroutine tracing.
  !
  ! 10. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function w3gfcd_r4( gsu, xt, yt, is, js, xs, ys, &
       pole, eps, debug ) result(ingrid)
    !     single precision interface
    logical                 :: ingrid
    type(t_gsu), intent(in) :: gsu
    real(4), intent(inout)  :: xt
    real(4), intent(inout)  :: yt
    integer, intent(inout)  :: is(4), js(4)
    real(4), intent(inout)  :: xs(4), ys(4)
    logical, intent(out),optional :: pole
    real(4), intent(in), optional :: eps
    logical, intent(in), optional :: debug
    !     local parameters
    real(8) :: xt8, yt8, xs8(4), ys8(4), eps8
    !
    !-----set inputs
    xt8 = xt; yt8 = yt;
    if ( present(eps) ) then
      eps8 = eps
    else
      eps8 = eps_default
    end if
    !
    !-----call double precision method
    ingrid = w3gfcd( gsu, xt8, yt8, is, js, xs8, ys8, pole=pole, &
         eps=eps8, debug=debug )
    !
    !-----set outputs
    xt = xt8; yt = yt8;
    xs = xs8; ys = ys8;
  end function w3gfcd_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function w3gfcd_r8( gsu, xt, yt, is, js, xs, ys, &
       pole, eps, debug ) result(ingrid)
    !     double precision interface
    logical                 :: ingrid
    type(t_gsu), intent(in) :: gsu
    real(8), intent(inout)  :: xt
    real(8), intent(inout)  :: yt
    integer, intent(inout)  :: is(4), js(4)
    real(8), intent(inout)  :: xs(4), ys(4)
    logical, intent(out),optional :: pole
    real(8), intent(in), optional :: eps
    logical, intent(in), optional :: debug
    !     local parameters
    real(8) :: leps
    logical :: ldbg, lplc
    integer :: i, j, l
    logical :: ijg, llg, lclo, l360
    integer :: iclo, gkind
    integer :: lbx, lby, ubx, uby, nx, ny
    integer :: lxc, lyc, uxc, uyc
    real(4), pointer :: xg4(:,:), yg4(:,:)
    real(8), pointer :: xg8(:,:), yg8(:,:)
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input
    !
    if ( .not.associated(gsu%ptr) ) then
      write(0,'(/2a/)') 'w3gfcd_r8 error -- ', &
           'grid search utility object not created'
      call extcde (1)
    end if
    if ( present(eps) ) then
      if ( eps .lt. zero ) then
        write(0,'(/2a/)') 'w3gfcd_r8 error -- ', &
             'eps parameter must be >= 0'
        call extcde (1)
      end if
      leps = eps
    else
      leps = eps_default
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  initialize search
    !
    if ( present(debug) ) then
      ldbg = debug
    else
      ldbg = .false.
    end if
    !
    !  local pointers to grid search utility object data
    ijg = gsu%ptr%ijg
    llg = gsu%ptr%llg
    iclo = gsu%ptr%iclo
    lclo = gsu%ptr%lclo
    l360 = gsu%ptr%l360
    gkind = gsu%ptr%gkind
    lbx = gsu%ptr%lbx;  lby = gsu%ptr%lby;
    ubx = gsu%ptr%ubx;  uby = gsu%ptr%uby;
    nx = gsu%ptr%nx;  ny = gsu%ptr%ny;
    if ( gkind.eq.4 ) then
      xg4 => gsu%ptr%xg4;  yg4 => gsu%ptr%yg4;
    else
      xg8 => gsu%ptr%xg8;  yg8 => gsu%ptr%yg8;
    end if
    !
    ingrid = .false.
    !
    !  shift target to appropriate longitude range
    if ( llg ) then
      xt = mod(xt,real(d360,8))
      if ( lclo .or. l360 ) then
        if ( xt.lt.zero ) xt = xt + d360
      else
        if ( xt.gt.d180 ) xt = xt - d360
      end if
    end if
    if ( ldbg ) &
         write(*,'(/a,2e24.16)') 'w3gfcd_r8 - target point:',xt,yt
    !-----number of cells
    lxc = lbx;  lyc = lby;
    select case ( iclo )
    case ( iclo_none )
      uxc = ubx-1;  uyc = uby-1;
    case ( iclo_grdi )
      uxc = ubx;    uyc = uby-1;
    case ( iclo_grdj )
      uxc = ubx-1;  uyc = uby;
    case ( iclo_trdl )
      uxc = ubx;    uyc = uby;
    case ( iclo_trpl )
      uxc = ubx;    uyc = uby;
    end select
    !
    ! -------------------------------------------------------------------- /
    ! 3.  search for enclosing cell
    !
    cell_loop: do i=lxc,uxc
      do j=lyc,uyc
        !-------------create list of cell vertices
        is(1) = i  ;  js(1) = j  ;
        is(2) = i+1;  js(2) = j  ;
        is(3) = i+1;  js(3) = j+1;
        is(4) = i  ;  js(4) = j+1;
        !-------------setup cell corner coordinates and adjust for periodicity
        do l=1,4
          !-----------------apply index closure
          if ( mod(iclo,2).eq.0 ) &
               is(l) = lbx + mod(nx - 1 + mod(is(l) - lbx + 1, nx), nx)
          if ( mod(iclo,3).eq.0 ) &
               js(l) = lby + mod(ny - 1 + mod(js(l) - lby + 1, ny), ny)
          if ( iclo.eq.iclo_trpl .and. js(l).gt.uby ) then
            is(l) = ubx + lbx - is(l)
            js(l) = 2*uby - js(l) + 1
          end if
          !-----------------copy cell vertex coordinates into local variables
          if ( ijg ) then
            if ( gkind.eq.4 ) then
              xs(l) = xg4(is(l),js(l));  ys(l) = yg4(is(l),js(l));
            else
              xs(l) = xg8(is(l),js(l));  ys(l) = yg8(is(l),js(l));
            end if
          else
            if ( gkind.eq.4 ) then
              xs(l) = xg4(js(l),is(l));  ys(l) = yg4(js(l),is(l));
            else
              xs(l) = xg8(js(l),is(l));  ys(l) = yg8(js(l),is(l));
            end if
          end if
          !-----------------shift longitudes to same range
          if ( llg ) then
            xs(l) = mod(xs(l),real(d360,8))
            if ( lclo .or. l360 ) then
              if ( xs(l).lt.zero ) xs(l) = xs(l) + d360
            else
              if ( xs(l).gt.d180 ) xs(l) = xs(l) - d360
            end if
          end if
        end do !l
        if ( ldbg ) &
             write(*,'(a,4(/a,1i1,a,2i6,2e24.16))') &
             'w3gfcd_r8 - check cell:', &
             ('          corner(',l,'):',is(l),js(l),xs(l),ys(l),l=1,4)
        !-------------check if point is enclosed in cell defined by xs(1:4) & ys(1:4)
        ingrid = w3ckcl(llg,xt,yt,4,xs,ys,lplc,leps,ldbg)
        if ( ldbg ) write(*,'(a,1l2)')'w3gfcd_r8 - ingrid:',ingrid
        if ( ingrid ) then
          !-----------------exit search
          if ( ldbg ) &
               write(*,'(a,4(2i6))') &
               'w3gfcd_r8 - enclosing cell:',(is(l),js(l),l=1,4)
          if ( present(pole) ) pole = lplc
          exit cell_loop
        end if !point in cell
      end do !j
    end do cell_loop
  end function w3gfcd_r8
  !/
  !/ end of w3gfcd ===================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    function w3gfpt( gsu, xtin, ytin, ix, iy, eps, dcin, debug ) &
  !/    result(ingrid)
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     find point in grid, associated with the input grid-search-utility
  !     object (gsu), that is closest to the target point (xtin,ytin).
  !
  !  2. method :
  !
  !  3. parameters :
  !
  !     return parameter
  !     ----------------------------------------------------------------
  !       ingrid  log.   o   logical flag indicating if target point lies
  !                          within the source grid domain.
  !     ----------------------------------------------------------------
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !       gsu     type   i   grid-search-utility object.
  !       xtin    real   i   x-coordinate of target point.
  !       ytin    real   i   y-coordinate of target point.
  !       ix,jx   i.a.   o   (i,j) indices of nearest grid point.
  !       eps     real   i   optional small non-zero tolerance used to check if
  !                          target point is in domain and for point coincidence.
  !       dcin    real   i   optional distance outside of source grid in
  !                          units of cell width to treat target point as
  !                          inside the source grid.
  !                          default is 0.
  !       debug   log.   i   optional logical flag to turn on debug mode.
  !                          default is false.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
  !
  !     - check on previous initialization of grid search utility object.
  !
  !  7. remarks :
  !
  !  8. structure :
  !
  !     -----------------------------------------------------------------
  !      1.  test input
  !      2.  initialize search
  !      3.  find enclosing cell and compute closest point
  !     -----------------------------------------------------------------
  !
  !  9. switches :
  !
  !     !/s    enable subroutine tracing.
  !
  ! 10. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function w3gfpt_r4( gsu, xtin, ytin, ix, iy, eps, dcin, debug ) &
       result(ingrid)
    !     single precision interface
    logical                 :: ingrid
    type(t_gsu), intent(in) :: gsu
    real(4), intent(in)     :: xtin
    real(4), intent(in)     :: ytin
    integer, intent(out)    :: ix, iy
    real(4), intent(in), optional :: eps
    real(4), intent(in), optional :: dcin
    logical, intent(in), optional :: debug
    !     local parameters
    real(8) :: xt8, yt8, eps8, dcin8
    !
    !-----set inputs
    xt8 = xtin; yt8 = ytin;
    if ( present(eps) ) then
      eps8 = eps
    else
      eps8 = eps_default
    end if
    if ( present(dcin) ) then
      dcin8 = dcin
    else
      dcin8 = zero
    end if
    !
    !-----call double precision method
    ingrid = w3gfpt( gsu, xt8, yt8, ix, iy, eps=eps8, dcin=dcin8, &
         debug=debug )
  end function w3gfpt_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function w3gfpt_r8( gsu, xtin, ytin, ix, iy, eps, dcin, debug ) &
       result(ingrid)
    !     single precision interface
    logical                 :: ingrid
    type(t_gsu), intent(in) :: gsu
    real(8), intent(in)     :: xtin
    real(8), intent(in)     :: ytin
    integer, intent(out)    :: ix, iy
    real(8), intent(in), optional :: eps
    real(8), intent(in), optional :: dcin
    logical, intent(in), optional :: debug
    !     local parameters
    real(8), parameter :: big = 1d16
    real(8) :: leps, ldcin
    logical :: ldbg, fncl
    integer :: i, l
    integer :: is(4), js(4)
    real(8) :: xt, yt, xs(4), ys(4)
    real(8) :: xtc, ytc, xsc(4), ysc(4)
    real(8) :: ixr, jxr, dd, lon0, lat0, dmin
    logical :: ijg, llg
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input
    !
    if ( .not.associated(gsu%ptr) ) then
      write(0,'(/2a/)') 'w3gfpt_r8 error -- ', &
           'grid search utility object not created'
      call extcde (1)
    end if
    if ( present(eps) ) then
      if ( eps .lt. zero ) then
        write(0,'(/2a/)') 'w3gfpt_r8 error -- ', &
             'eps parameter must be >= 0'
        call extcde (1)
      end if
      leps = eps
    else
      leps = eps_default
    end if
    if ( present(dcin) ) then
      if ( dcin .lt. zero ) then
        write(0,'(/2a/)') 'w3gfpt_r8 error -- ', &
             'dcin parameter must be >= 0'
        call extcde (1)
      end if
      ldcin = dcin
    else
      ldcin = zero
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  initialize search
    !
    if ( present(debug) ) then
      ldbg = debug
    else
      ldbg = .false.
    end if
    !
    !  local pointers to grid search utility object data
    ijg = gsu%ptr%ijg
    llg = gsu%ptr%llg
    !
    ingrid = .false.
    ix = gsu%ptr%lbx-1
    iy = gsu%ptr%lby-1
    !
    xt = xtin;  yt = ytin;
    if ( ldbg ) &
         write(*,'(/a,2e24.16)') 'w3gfpt_r8 - target point:',xt,yt
    !
    ! -------------------------------------------------------------------- /
    ! 3.  find enclosing cell and compute closest point
    !
    fncl = ldcin .gt. zero
    ingrid = w3gfcl( gsu, xt, yt, is, js, xs, ys, eps=leps, fncl=fncl, debug=ldbg )
    if ( .not.ingrid .and. .not.fncl ) return
    !
    !-----set in grid if point is within dcin cell width distance of closest cell
    if ( .not.ingrid .and. fncl ) then
      !-------compute cell relative index space location
      lon0 = sum(xs)/four; lat0 = sum(ys)/four;
      if ( d90-abs(lat0).gt.near_pole ) then
        !-----------non-pole cell: compute relative location using (lon,lat)
        call getpqr(xt,yt,xs,ys,ixr,jxr,eps=leps,debug=ldbg)
      else
        !-----------pole cell: compute relative location using stereographic projection
        call w3splx(lon0,lat0,zero,xt,yt,xtc,ytc)
        do i=1,4
          call w3splx(lon0,lat0,zero,xs(i),ys(i),xsc(i),ysc(i))
        end do
        call getpqr(xtc,ytc,xsc,ysc,ixr,jxr,eps=leps,debug=ldbg)
      endif
      dd = half + ldcin
      ingrid = abs(ixr-half).le.dd .and. abs(jxr-half).le.dd
    end if
    !
    !-----compute indices of closest point in cell
    if ( ingrid ) then
      dmin = big
      do l=1,4
        dd = w3dist( llg, xt, yt, xs(l), ys(l) )
        if ( dd .lt. dmin ) then
          dmin = dd;  ix = is(l);  iy = js(l);
        end if
      end do !l
    end if
  end function w3gfpt_r8
  !/
  !/ end of w3gfpt ===================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    function w3gfij( gsu, xtin, ytin, ix, jx, eps, dcin, debug ) &
  !/    result(ingrid)
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     compute coordinates ( ix, jx ) of target point ( xtin, ytin ) in
  !     source grid index space from source grid associated with the input
  !     grid search utility object (gsu).
  !
  !  2. method :
  !
  !  3. parameters :
  !
  !     return parameter
  !     ----------------------------------------------------------------
  !       ingrid  log.   o   logical flag indicating if target point lies
  !                          within the source grid domain.
  !     ----------------------------------------------------------------
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !       gsu     type   i   grid-search-utility object.
  !       xtin    real   i   x-coordinate of target point.
  !       ytin    real   i   y-coordinate of target point.
  !       ix      real   o   x-coordinate of target point in source grid
  !                          index space.
  !       jx      real   o   y-coordinate of target point in source grid
  !                          index space.
  !       eps     real   i   optional small non-zero tolerance used to check if
  !                          target point is in domain and for point coincidence.
  !       dcin    real   i   optional distance outside of source grid in
  !                          units of cell width to treat target point as
  !                          inside the source grid.
  !                          default is 0.
  !       debug   log.   i   optional logical flag to turn on debug mode.
  !                          default is false.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
  !
  !     - check on previous initialization of grid search utility object.
  !     - check on appropriate input of optional arguments.
  !
  !  7. remarks :
  !
  !  8. structure :
  !
  !     -----------------------------------------------------------------
  !      1.  test input
  !      2.  initialize search
  !      3.  find enclosing cell and compute index coordinates
  !     -----------------------------------------------------------------
  !
  !  9. switches :
  !
  !     !/s    enable subroutine tracing.
  !
  ! 10. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function w3gfij_r4( gsu, xtin, ytin, ix, jx, eps, dcin, debug ) &
       result(ingrid)
    !     single precision interface
    logical                 :: ingrid
    type(t_gsu), intent(in) :: gsu
    real(4), intent(in)     :: xtin
    real(4), intent(in)     :: ytin
    real(4), intent(out)    :: ix
    real(4), intent(out)    :: jx
    real(4), intent(in), optional :: eps
    real(4), intent(in), optional :: dcin
    logical, intent(in), optional :: debug
    !     local parameters
    real(8) :: xt8, yt8, ix8, jx8, eps8, dcin8
    !
    !-----set inputs
    xt8 = xtin; yt8 = ytin;
    if ( present(eps) ) then
      eps8 = eps
    else
      eps8 = eps_default
    end if
    if ( present(dcin) ) then
      dcin8 = dcin
    else
      dcin8 = zero
    end if
    !
    !-----call double precision method
    ingrid = w3gfij( gsu, xt8, yt8, ix8, jx8, eps=eps8, dcin=dcin8, &
         debug=debug )
    !
    !-----set outputs
    ix = ix8; jx = jx8;
  end function w3gfij_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function w3gfij_r8( gsu, xtin, ytin, ix, jx, eps, dcin, debug ) &
       result(ingrid)
    !     double precision interface
    logical                 :: ingrid
    type(t_gsu), intent(in) :: gsu
    real(8), intent(in)     :: xtin
    real(8), intent(in)     :: ytin
    real(8), intent(out)    :: ix
    real(8), intent(out)    :: jx
    real(8), intent(in), optional :: eps
    real(8), intent(in), optional :: dcin
    logical, intent(in), optional :: debug
    !     local parameters
    real(8) :: leps, ldcin
    integer :: i
    logical :: ldbg, fncl, pole
    integer :: is(4), js(4)
    real(8) :: xt, yt, xs(4), ys(4)
    real(8) :: xtc, ytc, xsc(4), ysc(4)
    real(8) :: ixr, jxr, dd, lon0, lat0
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input
    !
    if ( .not.associated(gsu%ptr) ) then
      write(0,'(/2a/)') 'w3gfij_r8 error -- ', &
           'grid search utility object not created'
      call extcde (1)
    end if
    if ( present(eps) ) then
      if ( eps .lt. zero ) then
        write(0,'(/2a/)') 'w3gfij_r8 error -- ', &
             'eps parameter must be >= 0'
        call extcde (1)
      end if
      leps = eps
    else
      leps = eps_default
    end if
    if ( present(dcin) ) then
      if ( dcin .lt. zero ) then
        write(0,'(/2a/)') 'w3gfij_r8 error -- ', &
             'dcin parameter must be >= 0'
        call extcde (1)
      end if
      ldcin = dcin
    else
      ldcin = zero
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  initialize search
    !
    if ( present(debug) ) then
      ldbg = debug
    else
      ldbg = .false.
    end if
    !
    xt = xtin;  yt = ytin;
    if ( ldbg ) write(*,'(/a,2e24.16)') 'w3gfij_r8 - target point:',xt,yt
    !
    ! -------------------------------------------------------------------- /
    ! 3.  find enclosing cell and compute point location
    !
    fncl = ldcin .gt. zero
    ingrid = w3gfcl(gsu,xt,yt,is,js,xs,ys,pole=pole,eps=leps,fncl=fncl,debug=ldbg)
    if ( .not.ingrid .and. .not.fncl ) return
    !
    !-----compute cell relative index space location
    lon0 = sum(xs)/four; lat0 = sum(ys)/four;
    if ( d90-abs(lat0).gt.near_pole ) then
      !---------non-pole cell: compute relative location using (lon,lat)
      call getpqr(xt,yt,xs,ys,ixr,jxr,eps=leps,debug=ldbg)
    else
      !---------pole cell: compute relative location using stereographic projection
      call w3splx(lon0,lat0,zero,xt,yt,xtc,ytc)
      do i=1,4
        call w3splx(lon0,lat0,zero,xs(i),ys(i),xsc(i),ysc(i))
      end do
      call getpqr(xtc,ytc,xsc,ysc,ixr,jxr,eps=leps,debug=ldbg)
    endif
    if ( ldbg ) &
         write(*,'(a,2l2,2e24.16)') 'w3gfij_r8 - relative:',ingrid,fncl,ixr,jxr
    !
    !-----set in grid if point is within dcin cell width distance of closest cell
    if ( .not.ingrid .and. fncl ) then
      dd = half + ldcin
      ingrid = abs(ixr-half).le.dd .and. abs(jxr-half).le.dd
    end if
    !
    !-----compute absolute index space location
    ix = is(1)+ixr;  jx = js(1)+jxr;
    if ( ldbg ) &
         write(*,'(a,2l2,2e24.16)') 'w3gfij_r8 - absolute:',ingrid,fncl,ix,jx
  end function w3gfij_r8
  !/
  !/ end of w3gfij ===================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    function w3grmp( gsu, xtin, ytin, is, js, rw, eps, &
  !/                     dcin, mask, mskc, nnbr, debug ) result(ingrid)
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     compute bilinear remapping for target point (xtin,ytin) from source
  !     grid associated with the input grid search utility object (gsu).
  !     the indices of the source points used for remapping are returned in
  !     is(1:4) and js(1:4).  the remapping weights are returned in rw(1:4).
  !
  !  2. method :
  !
  !  3. parameters :
  !
  !     return parameter
  !     ----------------------------------------------------------------
  !       ingrid  log.   o   logical flag indicating if target point lies
  !                          within the source grid domain.
  !     ----------------------------------------------------------------
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !       gsu     type   i   grid-search-utility object.
  !       xtin    real   i   x-coordinate of target point.
  !       ytin    real   i   y-coordinate of target point.
  !       is,js   i.a.   o   (i,j) indices of vertices of enclosing grid cell.
  !       rw      r.a.   o   array of interpolation weights.
  !       eps     real   i   optional small non-zero tolerance used to check if
  !                          target point is in domain and for point coincidence.
  !       dcin    real   i   optional distance outside of source grid in
  !                          units of cell width to treat target point as
  !                          inside the source grid.  default is 0.
  !       mask    l.a.   i   optional logical mask for source grid.
  !       mskc    int.   o   optional output integer parameter indicating how
  !                          the enclosing cell is masked.  possible values
  !                          are mskc_none, mskc_part and mskc_full.
  !                          mskc is required when mask is specified.
  !       nnbr    int.   i/o optional integer parameter indicating the number
  !                          of nearest-neighbor non-masked points used for
  !                          distance-weighted averaging.
  !                          input:  requested number of nearest-neighbor
  !                                  non-masked points (0 < nnbr <= 4).
  !                          output: actual number of nearest-neighbor
  !                                  non-masked points used.
  !       debug   log.   i   optional logical flag to turn on debug mode.
  !                          default is false.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
  !
  !     - check on previous initialization of grid search utility object.
  !     - check on appropriate input of optional arguments.
  !
  !  7. remarks :
  !
  !  8. structure :
  !
  !     -----------------------------------------------------------------
  !      1.  test input
  !      2.  initialize search
  !      3.  find enclosing cell and compute remapping weights
  !          - if enclosing cell does not includes a pole, then
  !            compute bilinear remapping
  !          - if enclosing cell includes a pole, then
  !            compute distance weighted remapping
  !      4.  handle case of target point located within a partially masked cell.
  !      5.  handle case of target point located within a fully masked cell.
  !     -----------------------------------------------------------------
  !
  !  9. switches :
  !
  !     !/s    enable subroutine tracing.
  !
  ! 10. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function w3grmp_r4( gsu, xtin, ytin, is, js, rw, eps, &
       dcin, mask, mskc, nnbr, debug ) result(ingrid)
    !     single precision interface
    logical                 :: ingrid
    type(t_gsu), intent(in) :: gsu
    real(4), intent(in)     :: xtin
    real(4), intent(in)     :: ytin
    integer, intent(out)    :: is(4)
    integer, intent(out)    :: js(4)
    real(4), intent(out)    :: rw(4)
    real(4), intent(in)   , optional :: eps
    real(4), intent(in)   , optional :: dcin
    logical, intent(in)   , optional :: mask(:,:)
    integer, intent(out)  , optional :: mskc
    integer, intent(inout), optional :: nnbr
    logical, intent(in)   , optional :: debug
    !     local parameters
    real(8) :: xt8, yt8, rw8(4), eps8, dcin8
    !
    !-----set inputs
    xt8 = xtin; yt8 = ytin;
    if ( present(eps) ) then
      eps8 = eps
    else
      eps8 = eps_default
    end if
    if ( present(dcin) ) then
      dcin8 = dcin
    else
      dcin8 = zero
    end if
    !
    !-----call double precision method
    ingrid = w3grmp( gsu, xt8, yt8, is, js, rw8, &
         eps=eps8, dcin=dcin8, &
         mask=mask, mskc=mskc, nnbr=nnbr, debug=debug )
    !
    !-----set outputs
    rw = rw8
  end function w3grmp_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function w3grmp_r8( gsu, xtin, ytin, is, js, rw, eps, &
       dcin, mask, mskc, nnbr, debug ) result(ingrid)
    !     double precision interface
    logical                 :: ingrid
    type(t_gsu), intent(in) :: gsu
    real(8), intent(in)     :: xtin
    real(8), intent(in)     :: ytin
    integer, intent(out)    :: is(4)
    integer, intent(out)    :: js(4)
    real(8), intent(out)    :: rw(4)
    real(8), intent(in)   , optional :: eps
    real(8), intent(in)   , optional :: dcin
    logical, intent(in)   , optional :: mask(:,:)
    integer, intent(out)  , optional :: mskc
    integer, intent(inout), optional :: nnbr
    logical, intent(in)   , optional :: debug
    !     local parameters
    real(8), parameter :: big = 1d16
    real(8), parameter :: small = 1d-6
    real(8) :: leps
    logical :: ldbg, fncl, pole
    integer :: i, j, l
    logical :: m, msk(4)
    integer :: lvl, n, ns, icc, jcc
    real(8) :: xt, yt, xs(4), ys(4), dw(4)
    real(8) :: xtc, ytc, xsc(4), ysc(4)
    real(8) :: ldcin, ixr, jxr, x, y, d(4), dd, dmin, dsum, lon0, lat0
    logical :: ijg, llg, lclo
    integer :: iclo, gkind
    integer :: lbx, lby, ubx, uby, nx, ny
    real(4), pointer :: xg4(:,:), yg4(:,:)
    real(8), pointer :: xg8(:,:), yg8(:,:)
    type(t_nns), pointer :: nnp
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input
    !
    if ( .not.associated(gsu%ptr) ) then
      write(0,'(/2a/)') 'w3grmp_r8 error -- ', &
           'grid search utility object not created'
      call extcde (1)
    end if
    !
    if ( present(eps) ) then
      if ( eps .lt. zero ) then
        write(0,'(/2a/)') 'w3grmp_r8 error -- ', &
             'eps parameter must be >= 0'
        call extcde (1)
      end if
      leps = eps
    else
      leps = eps_default
    end if
    !
    if ( present(dcin) ) then
      if ( dcin .lt. zero ) then
        write(0,'(/2a/)') 'w3grmp_r4 error -- ', &
             'dcin parameter must be >= 0'
        call extcde (1)
      end if
      ldcin = dcin
    else
      ldcin = zero
    end if
    !
    if ( present(mask) ) then
      if ( .not.present(mskc) ) then
        write(0,'(/2a/)') 'w3grmp_r8 error -- ', &
             'mskc must be specified with mask'
        call extcde (1)
      end if
      if ( present(nnbr) ) then
        if ( .not.associated(gsu%ptr%nnp) ) then
          write(0,'(/3a/)') 'w3grmp_r8 error -- ', &
               'mask and nnbr input specified, ', &
               'but grid point-search object not created'
          call extcde (1)
        end if
        if ( nnbr .le. 0 .or. nnbr .gt. 4 ) then
          write(0,'(/2a/)') 'w3grmp_r8 error -- ', &
               'nnbr must be >= 1 and <= 4'
          call extcde (1)
        end if
      end if
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  initialize search
    !
    if ( present(debug) ) then
      ldbg = debug
    else
      ldbg = .false.
    end if
    !
    !  local pointers to grid search utility object data
    ijg = gsu%ptr%ijg
    llg = gsu%ptr%llg
    iclo = gsu%ptr%iclo
    lclo = gsu%ptr%lclo
    gkind = gsu%ptr%gkind
    lbx = gsu%ptr%lbx;  lby = gsu%ptr%lby;
    ubx = gsu%ptr%ubx;  uby = gsu%ptr%uby;
    nx = gsu%ptr%nx;  ny = gsu%ptr%ny;
    if ( gkind.eq.4 ) then
      xg4 => gsu%ptr%xg4;  yg4 => gsu%ptr%yg4;
    else
      xg8 => gsu%ptr%xg8;  yg8 => gsu%ptr%yg8;
    end if
    nnp => gsu%ptr%nnp
    !
    if ( present(mask) ) then
      if ( ijg ) then
        if ( .not.(ubound(mask,1).eq.nx.and. &
             ubound(mask,2).eq.ny) ) then
          write(0,'(/2a/)') 'w3grmp_r8 error -- ', &
               'mask array size does not agree with gsu index bounds'
          call extcde (1)
        end if
      else
        if ( .not.(ubound(mask,2).eq.nx.and. &
             ubound(mask,1).eq.ny) ) then
          write(0,'(/2a/)') 'w3grmp_r8 error -- ', &
               'mask array size does not agree with gsu index bounds'
          call extcde (1)
        end if
      end if
    end if
    !
    rw = zero;
    !
    xt = xtin;  yt = ytin;
    if ( ldbg ) write(*,'(/a,2e24.16)') 'w3grmp_r8 - target point:',xt,yt
    !
    ! -------------------------------------------------------------------- /
    ! 3.  find enclosing cell and compute remapping
    !
    fncl = ldcin .gt. zero
    ingrid = w3gfcl(gsu,xt,yt,is,js,xs,ys,pole=pole,eps=leps,fncl=fncl,debug=ldbg)
    if ( .not.ingrid .and. .not.fncl ) return
    !
    !-----compute remapping
    lon0 = sum(xs)/four; lat0 = sum(ys)/four;
    if ( d90-abs(lat0).gt.near_pole ) then
      !---------non-pole cell: compute remapping using (lon,lat)
      call getpqr(xt,yt,xs,ys,ixr,jxr,eps=leps,debug=ldbg)
    else
      !---------pole cell: compute remapping using stereographic projection
      call w3splx(lon0,lat0,zero,xt,yt,xtc,ytc)
      do i=1,4
        call w3splx(lon0,lat0,zero,xs(i),ys(i),xsc(i),ysc(i))
      end do
      call getpqr(xtc,ytc,xsc,ysc,ixr,jxr,eps=leps,debug=ldbg)
    endif
    dw(1) = (one-ixr)*(one-jxr)
    dw(2) = ixr*(one-jxr)
    dw(3) = ixr*jxr
    dw(4) = (one-ixr)*jxr
    rw = dw
    if ( ldbg ) then
      write(*,'(a,2e24.16)') 'w3grmp_r8 - remap (tgt):',xt,yt
      do l=1,4
        write(*,'(a,3i6,e24.16)') 'w3grmp_r8 - remap (src):', &
             l,is(l),js(l),dw(l)
      end do
    end if !ldbg
    !
    !-----set in grid if point is within dcin cell width distance of closest cell
    if ( .not.ingrid .and. fncl ) then
      dd = half + ldcin
      ingrid = abs(ixr-half).le.dd .and. abs(jxr-half).le.dd
    end if
    if ( .not.ingrid ) return
    !
    if ( .not.present(mask) ) return
    !
    ! -------------------------------------------------------------------- /
    ! 4.  handle case of target point located within a partially masked cell.
    !
    !-----copy cell mask values according to array ordering
    if ( ijg ) then
      do l=1,4
        msk(l) = mask(is(l)-lbx+1,js(l)-lby+1)
      end do
    else
      do l=1,4
        msk(l) = mask(js(l)-lby+1,is(l)-lbx+1)
      end do
    end if
    !
    !-----adjust weights for a partially masked cell
    dsum = zero
    ns = 4
    do l=1,4
      if ( msk(l) ) then
        ns = ns - 1
        dw(l) = zero
      end if
      dsum = dsum + dw(l)
    end do
    if ( ns .eq. 4 ) then
      mskc = mskc_none
      return
    end if
    if ( ns .gt. 0 .and. dsum .gt. small ) then
      dw = dw / dsum
      rw = dw
      if ( ldbg ) &
           write(*,'(a,2e24.16,4(2i6,e24.16))') &
           'w3grmp_r8 - partial masked cell:', &
           xt,yt,(is(l),js(l),dw(l),l=1,4)
      mskc = mskc_part
      return
    else
      mskc = mskc_full
      if ( .not.present(nnbr) ) return
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 5.  handle case of target point located within a fully masked cell.
    !
    !  choose closest point in enclosing land cell to be the central point
    dmin = big
    do l=1,4
      dd = w3dist(llg,xt,yt,xs(l),ys(l))
      if ( dd .lt. dmin ) then
        dmin = dd;  icc = is(l);  jcc = js(l);
      end if
    end do !l
    !
    !  search nearest-neighbor source points for closest nnbr un-masked
    !  points and compute distance-weighted average remapping.
    if ( ldbg ) &
         write(*,'(a,2i6)') &
         'w3grmp_r8 - begin point nnbr search:',icc,jcc
    ns = 0;  d(:) = big;
    level_loop: do lvl=0,nnp%nlvl
      nnbr_loop: do n=nnp%n1(lvl),nnp%n2(lvl)
        i = icc + nnp%di(n);  j = jcc + nnp%dj(n);
        if ( iclo.eq.iclo_none ) then
          if ( i.lt.lbx .or. i.gt.ubx ) cycle nnbr_loop
          if ( j.lt.lby .or. j.gt.uby ) cycle nnbr_loop
        end if
        !-------------apply index closure
        if ( mod(iclo,2).eq.0 ) &
             i = lbx + mod(nx - 1 + mod(i - lbx + 1, nx), nx)
        if ( mod(iclo,3).eq.0 ) &
             j = lby + mod(ny - 1 + mod(j - lby + 1, ny), ny)
        if ( iclo.eq.iclo_trpl .and. j.gt.uby ) then
          i = ubx + lbx - i
          j = 2*uby - j + 1
        end if
        !-------------set mask
        if ( ijg ) then
          m = mask(i-lbx+1,j-lby+1)
        else
          m = mask(j-lby+1,i-lbx+1)
        end if
        if ( ldbg ) &
             write(*,'(a,4i6,1l6)') &
             'w3grmp_r8 - point nnbr search:',lvl,n,i,j,m
        !-------------if masked point, then skip
        if ( m ) cycle nnbr_loop
        !-------------compute distance
        if ( ijg ) then
          if ( gkind.eq.4 ) then
            x = xg4(i,j);  y = yg4(i,j);
          else
            x = xg8(i,j);  y = yg8(i,j);
          end if
        else
          if ( gkind.eq.4 ) then
            x = xg4(j,i);  y = yg4(j,i);
          else
            x = xg8(j,i);  y = yg8(j,i);
          end if
        end if
        dd = w3dist(llg,xt,yt,x,y)
        !-------------still need nnbr points
        if ( ns .lt. nnbr ) then
          !-----------------add to list
          ns = ns + 1
          is(ns) = i;  js(ns) = j;  d(ns) = dd;
          !-----------------once list is full sort according to increasing distance
          if ( ns .eq. nnbr ) call w3sort(ns,is,js,d)
          !---------------we have found nnbr points
        else !list is full
          !-----------------insert into list if the newest point is closer
          call w3isrt(i,j,dd,ns,is,js,d)
        end if !list is full
        if ( ldbg ) &
             write(*,'(a,i2,i3,i6,4(2i6,e24.16))') &
             'w3grmp_r8 - point nnbr list:', &
             lvl,n,ns,(is(l),js(l),d(l),l=1,ns)
      end do nnbr_loop
      !---------if we have found nnbr_rqd points, then exit the search
      if ( ns .eq. nnbr ) exit level_loop
    end do level_loop
    nnbr = ns
    !
    !  if zero unmasked points found, then return nnbr=0 as error indicator
    if ( nnbr .eq. 0 ) return
    !
    !  compute distance-weighted remapping for nnbr points
    dsum = zero
    do l=1,nnbr
      dsum = dsum + one/(d(l)+small)
    end do
    dw(1:nnbr) = one/(d(1:nnbr)+small)/dsum
    rw = dw
    if ( ldbg ) then
      write(*,'(a,2e24.16,i6)') &
           'w3grmp_r8 - fully masked cell (tgt):',xt,yt,nnbr
      do l=1,nnbr
        write(*,'(a,3i6,e24.16)') &
             'w3grmp_r8 - fully masked cell (src):', &
             l,is(l),js(l),dw(l)
      end do
    end if !ldbg
  end function w3grmp_r8
  !/
  !/ end of w3grmp ===================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    function w3grmc( gsu, xtin, ytin, rtyp, ns, is, js, cs, eps, &
  !/                     dcin, wdth, mask, nmsk, debug ) result(ingrid)
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     compute remapping coefficients for target point (xtin,ytin) from
  !     source grid associated with the input grid search utility object
  !     (gsu). the type of remapping is specified by rtyp.  the indices
  !     of the source points used for remapping are returned in is(1:ns)
  !     and js(1:ns).  the remapping coefficients are returned in cs(1:ns).
  !
  !  2. method :
  !
  !  3. parameters :
  !
  !     return parameter
  !     ----------------------------------------------------------------
  !       ingrid  log.   o   logical flag indicating if target point lies
  !                          within the source grid domain.
  !     ----------------------------------------------------------------
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !       gsu     type   i   grid-search-utility object.
  !       xtin    real   i   x-coordinate of target point.
  !       ytin    real   i   y-coordinate of target point.
  !       rtyp    str.   i   remap type: 'nearpt', 'bilinr', 'bicubc',
  !                                      'filter'
  !       ns      int.   o   number of vertices for remapping
  !       is,js   i.a.   o   (i,j) indices of vertices for remapping
  !       cs      r.a.   o   array of remapping coefficients
  !       eps     real   i   optional small non-zero tolerance used to check if
  !                          target point is in domain and for point coincidence.
  !       dcin    real   i   optional distance outside of source grid in
  !                          units of cell width to treat target point as
  !                          inside the source grid.  default is 0.
  !       wdth    real   i   optional width for gaussian filter in units of
  !                          source grid cell width.  required if rtyp='filter'.
  !                          actual width used is min(wdth,1.5).
  !       mask    l.a.   i   optional logical mask for source grid.
  !                          (t = invalid, f = valid)
  !                          dimension must be same as gsu coordinate arrays.
  !       nmsk    int.   i   optional maximum number of masked points for
  !                          treating an enclosing source grid cell as partially
  !                          masked. must be >= 0 and < 4.  default is 2.
  !       debug   log.   i   optional logical flag to turn on debug mode.
  !                          default is false.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
  !
  !     - check on previous initialization of grid search utility object.
  !     - check on appropriate input of optional arguments.
  !
  !  7. remarks :
  !
  !  8. structure :
  !
  !     -----------------------------------------------------------------
  !      1.  test input
  !      2.  initialize search
  !      3.  find enclosing cell and compute relative index space location
  !      4.  compute source grid points and remapping coefficients
  !      5.  adjust for partially masked cell and enforce normalization
  !      6.  load into return arrays and release work arrays
  !     -----------------------------------------------------------------
  !
  !  9. switches :
  !
  !     !/s    enable subroutine tracing.
  !
  ! 10. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function w3grmc_r4( gsu, xtin, ytin, rtyp, ns, is, js, cs, eps, &
       dcin, wdth, mask, nmsk, debug ) result(ingrid)
    !     single precision interface
    logical                 :: ingrid
    type(t_gsu), intent(in) :: gsu
    real(4), intent(in)     :: xtin
    real(4), intent(in)     :: ytin
    character(6), intent(in):: rtyp
    integer, intent(out)    :: ns
    integer, intent(inout), pointer :: is(:)
    integer, intent(inout), pointer :: js(:)
    real(4), intent(inout), pointer :: cs(:)
    real(4), intent(in)   , optional :: eps
    real(4), intent(in)   , optional :: dcin
    real(4), intent(in)   , optional :: wdth
    logical, intent(in)   , optional :: mask(:,:)
    integer, intent(in)   , optional :: nmsk
    logical, intent(in)   , optional :: debug
    !     local parameters
    real(8) :: leps, ldcin, lwdth=zero
    real(8) :: xt, yt
    real(8), pointer :: cs8(:) => null()
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input
    !
    if ( .not.associated(gsu%ptr) ) then
      write(0,'(/2a/)') 'w3grmc_r4 error -- ', &
           'grid search utility object not created'
      call extcde (1)
    end if
    !
    select case (rtyp)
    case ('nearpt')
    case ('bilinr')
    case ('bicubc')
    case ('filter')
      if ( .not.present(wdth) ) then
        write(0,'(/2a/)') 'w3grmc_r4 error -- ', &
             'wdth parameter is required with rtyp = filter'
        call extcde (1)
      else
        lwdth = wdth
      end if
    case default
      write(0,'(/2a/)') 'w3grmc_r4 error -- ', &
           'rtyp = '//rtyp//' not supported'
      call extcde (1)
    end select
    !
    if ( present(eps) ) then
      if ( eps .lt. zero ) then
        write(0,'(/2a/)') 'w3grmc_r4 error -- ', &
             'eps parameter must be >= 0'
        call extcde (1)
      end if
      leps = eps
    else
      leps = eps_default
    end if
    !
    if ( present(dcin) ) then
      if ( dcin .lt. zero ) then
        write(0,'(/2a/)') 'w3grmc_r4 error -- ', &
             'dcin parameter must be >= 0'
        call extcde (1)
      end if
      ldcin = dcin
    else
      ldcin = zero
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  call into double precision method
    !
    xt = xtin;  yt = ytin;
    ingrid = w3grmc( gsu, xt, yt, rtyp, ns, is, js, cs8, &
         eps=leps, dcin=ldcin, wdth=lwdth, &
         mask=mask, nmsk=nmsk, debug=debug )
    if ( ns.gt.0 ) then
      allocate( cs(ns) )
      cs(:) = cs8(:)
      deallocate( cs8 )
    end if
  end function w3grmc_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function w3grmc_r8( gsu, xtin, ytin, rtyp, ns, is, js, cs, eps, &
       dcin, wdth, mask, nmsk, debug ) result(ingrid)
    !     double precision interface
    logical                 :: ingrid
    type(t_gsu), intent(in) :: gsu
    real(8), intent(in)     :: xtin
    real(8), intent(in)     :: ytin
    character(6), intent(in):: rtyp
    integer, intent(out)    :: ns
    integer, intent(inout), pointer :: is(:)
    integer, intent(inout), pointer :: js(:)
    real(8), intent(inout), pointer :: cs(:)
    real(8), intent(in)   , optional :: eps
    real(8), intent(in)   , optional :: dcin
    real(8), intent(in)   , optional :: wdth
    logical, intent(in)   , optional :: mask(:,:)
    integer, intent(in)   , optional :: nmsk
    logical, intent(in)   , optional :: debug
    !     local parameters
    logical, parameter :: lcmp = .true.
    integer, parameter :: nmsk_default = 2
    real(8), parameter :: big = 1d16
    real(8) :: leps, lwdth=zero
    logical :: ldbg, fncl, pole, doblc, lmsk
    integer :: i, ii, jj, k, kk, mcs, mcsmax
    integer :: ic(4), jc(4)
    real(8) :: xt, yt, xc(4), yc(4)
    real(8) :: xtc, ytc, xsc(4), ysc(4)
    real(8) :: ldcin, ixr, jxr, dd, lon0, lat0, dmin
    real(8) :: ix, jx, czs
    integer :: nz
    logical, pointer :: lz(:)=>null()
    integer, pointer :: iz(:)=>null(), jz(:)=>null()
    real(8), pointer :: cz(:)=>null()
    logical :: ijg, llg, lclo
    integer :: iclo, gkind
    integer :: lbx, lby, ubx, uby, nx, ny
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input
    !
    if ( .not.associated(gsu%ptr) ) then
      write(0,'(/2a/)') 'w3grmc_r8 error -- ', &
           'grid search utility object not created'
      call extcde (1)
    end if
    !
    select case (rtyp)
    case ('nearpt')
    case ('bilinr')
    case ('bicubc')
    case ('filter')
      if ( .not.present(wdth) ) then
        write(0,'(/2a/)') 'w3grmc_r8 error -- ', &
             'wdth parameter is required with rtyp = filter'
        call extcde (1)
      else
        lwdth = wdth
      end if
    case default
      write(0,'(/2a/)') 'w3grmc_r8 error -- ', &
           'rtyp = '//rtyp//' not supported'
      call extcde (1)
    end select
    !
    if ( present(eps) ) then
      if ( eps .lt. zero ) then
        write(0,'(/2a/)') 'w3grmc_r8 error -- ', &
             'eps parameter must be >= 0'
        call extcde (1)
      end if
      leps = eps
    else
      leps = eps_default
    end if
    !
    if ( present(dcin) ) then
      if ( dcin .lt. zero ) then
        write(0,'(/2a/)') 'w3grmc_r8 error -- ', &
             'dcin parameter must be >= 0'
        call extcde (1)
      end if
      ldcin = dcin
    else
      ldcin = zero
    end if
    !
    if ( present(nmsk) ) then
      if ( nmsk .lt. zero .or. nmsk .ge. 4 ) then
        write(0,'(/2a/)') 'w3grmc_r8 error -- ', &
             'nmsk parameter must be >= 0 and < 4'
        call extcde (1)
      end if
      mcsmax = nmsk
    else
      mcsmax = nmsk_default
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  initialize search
    !
    if ( present(debug) ) then
      ldbg = debug
    else
      ldbg = .false.
    end if
    !
    !  local pointers to grid search utility object data
    ijg = gsu%ptr%ijg
    llg = gsu%ptr%llg
    iclo = gsu%ptr%iclo
    lclo = gsu%ptr%lclo
    gkind = gsu%ptr%gkind
    lbx = gsu%ptr%lbx;  lby = gsu%ptr%lby;
    ubx = gsu%ptr%ubx;  uby = gsu%ptr%uby;
    nx = gsu%ptr%nx;  ny = gsu%ptr%ny;
    !
    if ( present(mask) ) then
      if ( ijg ) then
        if ( .not.(ubound(mask,1).eq.nx.and. &
             ubound(mask,2).eq.ny) ) then
          write(0,'(/2a/)') 'w3grmc_r8 error -- ', &
               'mask array size does not agree with gsu index bounds'
          call extcde (1)
        end if
      else
        if ( .not.(ubound(mask,2).eq.nx.and. &
             ubound(mask,1).eq.ny) ) then
          write(0,'(/2a/)') 'w3grmc_r8 error -- ', &
               'mask array size does not agree with gsu index bounds'
          call extcde (1)
        end if
      end if
    end if
    !
    ns = 0
    if ( associated(is) ) then
      deallocate( is )
      nullify( is )
    end if
    if ( associated(js) ) then
      deallocate( js )
      nullify( js )
    end if
    if ( associated(cs) ) then
      deallocate( cs )
      nullify( cs )
    end if
    !
    xt = xtin;  yt = ytin;
    if ( ldbg ) write(*,'(/a,2e24.16)') 'w3grmc_r8 - target point:',xt,yt
    !
    ! -------------------------------------------------------------------- /
    ! 3.  find enclosing cell and compute relative index space location
    !
    fncl = ldcin .gt. zero
    ingrid = w3gfcl(gsu,xt,yt,ic,jc,xc,yc,pole=pole,eps=leps,fncl=fncl,debug=ldbg)
    if ( .not.ingrid .and. .not.fncl ) return
    !
    !-----compute cell relative index space location
    lon0 = sum(xc)/four; lat0 = sum(yc)/four;
    if ( d90-abs(lat0).gt.near_pole ) then
      !---------non-pole cell: compute relative location using (lon,lat)
      call getpqr(xt,yt,xc,yc,ixr,jxr,eps=leps,debug=ldbg)
    else
      !---------pole cell: compute relative location using stereographic projection
      call w3splx(lon0,lat0,zero,xt,yt,xtc,ytc)
      do i=1,4
        call w3splx(lon0,lat0,zero,xc(i),yc(i),xsc(i),ysc(i))
      end do
      call getpqr(xtc,ytc,xsc,ysc,ixr,jxr,eps=leps,debug=ldbg)
    endif
    if ( ldbg ) &
         write(*,'(a,2l2,2e24.16)') 'w3grmc_r8 - relative:',ingrid,fncl,ixr,jxr
    !
    !-----set in grid if point is within dcin cell width distance of closest cell
    if ( .not.ingrid .and. fncl ) then
      dd = half + ldcin
      ingrid = abs(ixr-half).le.dd .and. abs(jxr-half).le.dd
    end if
    if ( .not.ingrid ) return
    !
    !-----compute absolute index space location
    ix = ic(1) + ixr;  jx = jc(1) + jxr;
    !
    !-----determine if target point is coincident with an
    !     unmasked source grid cell point (kk)
    kk_loop: do kk=1,4
      if ( abs(ic(kk)-ix).le.leps .and. &
           abs(jc(kk)-jx).le.leps ) then
        if ( present(mask) ) then
          if ( ijg ) then
            if ( .not.mask(ic(kk)-lbx+1,jc(kk)-lby+1) ) exit kk_loop
          else
            if ( .not.mask(jc(kk)-lby+1,ic(kk)-lbx+1) ) exit kk_loop
          end if
        else
          exit kk_loop
        end if
      end if
    end do kk_loop
    !
    !-----count number of masked points in source cell
    mcs = 0
    if ( present(mask) ) then
      do k=1,4
        if ( ijg ) then
          if ( mask(ic(k)-lbx+1,jc(k)-lby+1) ) mcs = mcs+1
        else
          if ( mask(jc(k)-lby+1,ic(k)-lbx+1) ) mcs = mcs+1
        end if
      end do
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 4.  compute source grid points and remapping coefficients
    !
    select case (rtyp)
    case ('nearpt')
      ! *** nearest point ***
      dmin = big
      do k=1,4
        dd = (ix - ic(k))**2 + (jx - jc(k))**2
        if ( dd .lt. dmin ) then
          dmin = dd;  ii = ic(k);  jj = jc(k);
        end if
      end do
      nz = 1
      if ( present(mask) ) then
        if ( ijg ) then
          if ( mask(ii-lbx+1,jj-lby+1) ) nz = 0
        else
          if ( mask(jj-lby+1,ii-lbx+1) ) nz = 0
        end if
      end if
      if ( nz.eq.1 ) then
        ! nearest point is unmasked
        ! set number of points to one and coefficient to one
        allocate( lz(nz), iz(nz), jz(nz), cz(nz) )
        lz(nz) = .true.
        iz(nz) = ii
        jz(nz) = jj
        cz(nz) = one
      else
        ! nearest point is masked
        ! set number of points to zero and return
        ns = 0
        return
      end if
    case ('bilinr')
      ! *** bilinear interpolation ***
      if ( kk.le.4 ) then
        ! coincident with unmasked point kk
        ! set number of points to one and coefficient to one
        nz = 1
        allocate( lz(nz), iz(nz), jz(nz), cz(nz) )
        lz(nz) = .true.
        iz(nz) = ic(kk)
        jz(nz) = jc(kk)
        cz(nz) = one
      else
        ! no coincident points
        if ( mcs.le.mcsmax ) then
          ! unmasked or partially masked cell
          ! set bilinear interpolation
          call getblc( gsu, ic(1), jc(1), ixr, jxr, &
               lcmp, nz, lz, iz, jz, cz )
        else
          ! fully masked cell
          ! set number of points to zero and return
          ns = 0
          return
        end if
      end if
    case ('bicubc')
      ! *** bicubic interpolation ***
      if ( kk.le.4 ) then
        ! coincident with unmasked point kk
        ! set number of points to one and coefficient to one
        nz = 1
        allocate( lz(nz), iz(nz), jz(nz), cz(nz) )
        lz(nz) = .true.
        iz(nz) = ic(kk)
        jz(nz) = jc(kk)
        cz(nz) = one
      else
        ! no coincident points
        if ( mcs.eq.0 ) then
          ! unmasked cell
          ! get bicubic interpolation
          call getbcc( gsu, ic(1), jc(1), ixr, jxr, &
               lcmp, nz, lz, iz, jz, cz )
          ! check for masked points in bicubic stencil
          doblc = .false.
          if ( present(mask) ) then
            check: do k=1,nz
              if ( lz(k) ) then
                if ( ijg ) then
                  lmsk = mask(iz(k)-lbx+1,jz(k)-lby+1)
                else
                  lmsk = mask(jz(k)-lby+1,iz(k)-lbx+1)
                end if
                if ( lmsk ) then
                  doblc = .true.
                  exit check
                end if
              end if
            end do check
          end if
          if ( doblc ) then
            ! masked points in bicubic stencil
            ! set bilinear interpolation
            call getblc( gsu, ic(1), jc(1), ixr, jxr, &
                 lcmp, nz, lz, iz, jz, cz )
          end if
        else if ( mcs.le.mcsmax ) then
          ! partially masked cell
          ! set bilinear interpolation
          call getblc( gsu, ic(1), jc(1), ixr, jxr, &
               lcmp, nz, lz, iz, jz, cz )
        else
          ! fully masked cell
          ! set number of points to zero and return
          ns = 0
          return
        end if
      end if
    case ('filter')
      ! *** gaussian filter ***
      if ( mcs.le.mcsmax ) then
        ! unmasked or partially masked cell
        ! get gaussian filter
        call getgfc( gsu, ic(1), jc(1), ixr, jxr, &
             lwdth, lcmp, nz, lz, iz, jz, cz )
      else
        ! fully masked cell
        ! set number of points to zero and return
        ns = 0
        return
      end if
    end select
    !
    ! -------------------------------------------------------------------- /
    ! 5.  adjust for partially masked cell and enforce normalization
    !
    if ( nz .gt. 1 ) then
      czs = zero
      do k=1,nz
        if ( lz(k) ) then
          if ( present(mask) ) then
            if ( ijg ) then
              lmsk = mask(iz(k)-lbx+1,jz(k)-lby+1)
            else
              lmsk = mask(jz(k)-lby+1,iz(k)-lbx+1)
            end if
            if ( lmsk ) then
              lz(k) = .false.
              cz(k) = zero
            else
              czs = czs + cz(k)
            end if
          else
            czs = czs + cz(k)
          end if
        end if
      end do
      if ( czs .gt. zero ) then
        do k=1,nz
          if ( lz(k) ) cz(k) = cz(k)/czs
        enddo
      end if
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 6.  load into return arrays and release work arrays
    !
    ns = 0
    do k=1,nz
      if ( lz(k) ) ns = ns + 1
    end do
    if ( ns.gt.0 ) then
      allocate( is(ns), js(ns), cs(ns) )
      ns = 0
      do k=1,nz
        if ( lz(k) ) then
          ns = ns + 1
          is(ns) = iz(k)
          js(ns) = jz(k)
          cs(ns) = cz(k)
        end if
      end do
    end if
    deallocate( lz, iz, jz, cz )
  end function w3grmc_r8
  !/
  !/ end of w3grmc ===================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    function w3ckcl( llg, xt, yt, ns, xs, ys, pole, eps, debug ) &
  !/    result(incell)
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     check if point lies within grid cell.
  !
  !  2. method :
  !
  !     calculates cross products for vertex to vertex (i.e. cell side)
  !     vs vertex to target. if all cross products have the same sign,
  !     the point is considered to be within the cell. since they can
  !     be "all positive" *or* "all negative", there are no pre-conditions
  !     that the order of specification of the vertices be clockwise vs.
  !     counter-clockwise geographically.  the logical variable pole is
  !     set to true if the grid cell includes a pole.
  !
  !  3. parameters :
  !
  !     return parameter
  !     ----------------------------------------------------------------
  !       incell  log.   o   logical flag indicating point is in the cell
  !     ----------------------------------------------------------------
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !       llg     log.   i   logical flag indicating the coordinate system:
  !                          t = spherical lat/lon (degrees) and f = cartesian.
  !       xt      real   i   x-coordinate of target point.
  !       yt      real   i   y-coordinate of target point.
  !       xs      r.a.   i   x-coordinates of source cell vertices.
  !       ys      r.a.   i   y-coordinates of source cell vertices.
  !       pole    log.   o   optional output logical flag to indicate
  !                          the source cell contains a pole.
  !       eps     real   i   optional small non-zero tolerance used to check
  !                          for point coincidence.
  !       debug   log.   i   optional logical flag to turn on debug mode.
  !                          default is false.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
  !
  !  7. remarks :
  !
  !     - for ll grids, this method assumes that the longitudes of point
  !       and grid cell vertices lie in the same range (i.e., both in [0:360]
  !       or [-180:180]).  if the longitudes are not in the same range, then
  !       this method may result in a false positive.  the burden is upon the
  !       caller to ensure that the longitude range of the point is the same
  !       as that of the grid cell vertices.
  !     - if enclosing cell includes a branch cut, then the coordinates of
  !       of the cell vertices and the target point will be adjusted so
  !       that the branch cut is shifted 180 degrees.
  !     - if the enclosing cell includes a pole, then the cross-product check
  !       is performed using coordinates in a stereographic projection.
  !
  !  8. structure :
  !
  !  9. switches :
  !
  !     !/s    enable subroutine tracing.
  !
  ! 10. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function w3ckcl_r4( llg, xt, yt, ns, xs, ys, pole, eps, debug ) &
       result(incell)
    !     single precision interface
    logical                :: incell
    logical, intent(in)    :: llg
    real(4), intent(inout) :: xt, yt
    integer, intent(in)    :: ns
    real(4), intent(inout) :: xs(ns), ys(ns)
    logical, intent(out)   :: pole
    real(4), intent(in), optional :: eps
    logical, intent(in), optional :: debug
    !     local parameters
    real(8) :: xt8, yt8, xs8(ns), ys8(ns), eps8
    !
    !-----set inputs
    xt8 = xt;  xs8 = xs;
    yt8 = yt;  ys8 = ys;
    if ( present(eps) ) then
      eps8 = eps
    else
      eps8 = eps_default
    end if
    !
    !-----call double precision method
    incell = w3ckcl( llg, xt8, yt8, ns, xs8, ys8, pole, &
         eps=eps8, debug=debug )
    !
    !-----return branch cut shifted coordinates
    xt = xt8;  xs = xs8;
  end function w3ckcl_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function w3ckcl_r8( llg, xt, yt, ns, xs, ys, pole, eps, debug ) &
       result(incell)
    !     double precision interface
    logical                :: incell
    logical, intent(in)    :: llg
    real(8), intent(inout) :: xt, yt
    integer, intent(in)    :: ns
    real(8), intent(inout) :: xs(ns), ys(ns)
    logical, intent(out)   :: pole
    real(8), intent(in), optional :: eps
    logical, intent(in), optional :: debug
    !     local parameters
    real(8) :: leps
    logical :: ldbg, lsbc, bcut
    integer :: i, j, k, n
    real(8) :: xxt, yyt, xxs(ns), yys(ns)
    real(8) :: xct, yct, xcs(ns), ycs(ns)
    real(8) :: v1x, v1y, v2x, v2y, s90
    real(8) :: cross
    real(8) :: sign1
    incell = .true.
    !
    !-----must have >= 3 points to be a cell
    if ( ns .lt. 3 ) then
      incell = .false.
      return
    end if
    !
    if ( present(eps) ) then
      if ( eps .lt. zero ) then
        write(0,'(/2a/)') 'w3ckcl_r8 error -- ', &
             'eps parameter must be >= 0'
        call extcde (1)
      end if
      leps = eps
    else
      leps = eps_default
    end if
    if ( present(debug) ) then
      ldbg = debug
    else
      ldbg = .false.
    end if
    !
    !-----set local copies
    xxt = xt;  xxs = xs;
    yyt = yt;  yys = ys;
    !
    !-----check if cell includes a pole or branch cut
    if ( llg ) then
      n = 0
      !---------count longitudinal branch cut crossings
      do i=1,ns
        j = mod(i,ns) + 1
        if ( abs(xxs(j)-xxs(i)) .gt. d180 ) n = n + 1
      end do
      !---------multiple longitudinal branch cut crossing => cell includes branch cut
      bcut = n.gt.1
      !---------single longitudinal branch cut crossing
      !         or single vertex at 90 degrees => cell includes pole
      pole = n.eq.1 .or. count(abs(d90-abs(yys)).le.leps).eq.1
    else
      pole = .false.
      bcut = .false.
    end if
    !
    !-----shift branch cut if necessary
    if ( bcut ) then
      if ( minval(xxs) .ge. zero ) then
        where ( xxs .gt. d180 ) xxs = xxs - d360
        if ( xxt .gt. d180 ) xxt = xxt - d360
      else
        where ( xxs .lt. zero ) xxs = xxs + d360
        if ( xxt .lt. zero ) xxt = xxt + d360
      end if
      if ( ldbg ) then
        write(*,'(a)') 'w3ckcl_r8 - cell includes a branch cut'
        write(*,'(a,2e24.16,4(/a,1i1,a,2e24.16))') &
             'w3ckcl_r8 - shift branch cut:',xxt,yyt, &
             ('          corner(',k,'):',xxs(k),yys(k),k=1,4)
      end if
    end if
    !
    !-----check for coincidence with a cell vertex
    do i=1,ns
      !---------if target point is coincident a cell vertex, then
      !         flag as in cell and return
      if ( abs(xxt-xxs(i)).le.leps .and. abs(yyt-yys(i)).le.leps ) then
        if ( ldbg ) &
             write(*,'(a,i1,a,2e24.16)') &
             'w3ckcl_r8 - coincident with corner(',i,'): ', &
             abs(xxt-xxs(i)),abs(yyt-yys(i))
        !-------------return branch cut shifted coordinates
        if ( bcut ) then
          xt = xxt;  xs = xxs;
        end if
        incell = .true.
        return
      end if
    end do
    !
    !-----handle cell that includes a pole
    if ( pole ) then
      !---------perform cross-product check for each subcell
      if ( ldbg ) &
           write(*,'(a)') 'w3ckcl_r8 - cell includes a pole'
      s90 = d90; if ( maxval(ys).lt.zero ) s90 = -d90;
      subcell_loop: do i=1,ns
        lsbc = .true.
        j = mod(i,ns) + 1
        sign1 = 0.0
        do k=1,4
          select case (k)
          case (1)
            !---------------------vector from (xi,yi) to (xj,yj)
            v1x = xxs(j) - xxs(i)
            v1y = yys(j) - yys(i)
            !---------------------vector from (xi,yi) to (xt,yt)
            v2x = xxt    - xxs(i)
            v2y = yyt    - yys(i)
          case (2)
            !---------------------vector from (xj,yj) to (xj,90)
            v1x = xxs(j) - xxs(j)
            v1y = s90    - yys(j)
            !---------------------vector from (xj,yj) to (xt,yt)
            v2x = xxt    - xxs(j)
            v2y = yyt    - yys(j)
          case (3)
            !---------------------vector from (xj,90) to (xi,90)
            v1x = xxs(i) - xxs(j)
            v1y = s90    - s90
            !---------------------vector from (xj,90) to (xt,yt)
            v2x = xxt    - xxs(j)
            v2y = yyt    - s90
          case (4)
            !---------------------vector from (xi,90) to (xi,yi)
            v1x = xxs(i) - xxs(i)
            v1y = yys(i) - s90
            !---------------------vector from (xi,90) to (xt,yt)
            v2x = xxt    - xxs(i)
            v2y = yyt    - s90
          end select
          !-----------------check for longitudinal branch cut crossing
          if ( abs(v1x) .gt. d180 ) then
            v1x = v1x - sign(d360,v1x)
          end if
          if ( abs(v2x) .gt. d180 ) then
            v2x = v2x - sign(d360,v2x)
          end if
          !-----------------cross product
          cross = v1x*v2y - v1y*v2x
          !-----------------handle point that lies exacly on side or zero length side
          if ( abs(cross) .lt. leps ) cross = zero
          if ( ldbg ) &
               write(*,'(a,3(i1,a),5e24.16)') 'w3ckcl_r8 - cross(', &
               i,',',j,',',k,'):',v1x,v1y,v2x,v2y,cross
          !-----------------if sign of cross product is not "unanimous" among the
          !                 subcell sides, then target is outside the subcell
          if ( abs(sign1) .le. leps ) then
            if (abs(cross) .gt. leps) sign1 = sign(one,cross)
          else
            ! if point lies along a border, the cross product
            ! is zero and its sign is not well defined
            if ( abs(cross) .gt. leps ) then
              if ( sign(one,cross) .ne. sign1 ) then
                lsbc = .false.
                cycle subcell_loop
              end if
            end if
          end if
        end do !k
        if ( lsbc ) return
      end do subcell_loop
      incell = .false.
      return
    else
      !---------use input coordinates
      xct = xxt;  yct = yyt;
      xcs = xxs;  ycs = yys;
    end if !pole
    !
    !-----perform cross-product cell check
    sign1 = 0.0
    do i=1,ns
      j = mod(i,ns) + 1
      !---------vector from (xi,yi) to (xj,yj)
      v1x = xcs(j) - xcs(i)
      v1y = ycs(j) - ycs(i)
      !---------vector from (xi,yi) to (xt,yt)
      v2x = xct    - xcs(i)
      v2y = yct    - ycs(i)
      !---------cross product
      cross = v1x*v2y - v1y*v2x
      !---------handle point that lies exacly on side or zero length side
      if ( abs(cross) .lt. leps ) cross = zero
      if ( ldbg ) &
           write(*,'(a,2(i1,a),5e24.16)') 'w3ckcl_r8 - cross(', &
           i,',',j,'):',v1x,v1y,v2x,v2y,cross
      !---------if sign of cross product is not "unanimous" among the cell sides,
      !         then target is outside the cell
      if ( abs(sign1) .le. leps ) then
        if (abs(cross) .gt. leps) sign1 = sign(one,cross)
      else
        ! if point lies along a border, the cross product
        ! is zero and its sign is not well defined
        if ( abs(cross) .gt. leps ) then
          if ( sign(one,cross) .ne. sign1 ) then
            incell = .false.
            return
          end if
        end if
      end if
    end do
    !
    !-----return branch cut shifted coordinates
    if ( bcut ) then
      xt = xxt;  xs = xxs;
    end if
  end function w3ckcl_r8
  !/
  !/ end of w3ckcl ===================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    subroutine w3cgdm( ijg, llg, iclo, ptiled, qtiled, &
  !/                       prange, qrange, lbi, ubi, lbo, ubo, x, y, &
  !/                       mask, nfd, sphere, radius, dx, dy, &
  !/                       gppc, gqqc, gpqc, gsqr, &
  !/                       hpfc, hqfc, appc, aqqc, apqc, &
  !/                       dxdp, dydp, dxdq, dydq, &
  !/                       dpdx, dpdy, dqdx, dqdy, &
  !/                       cosa, cosc, sinc, angl, rc )
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     compute curvilinear grid derivatives and metric.
  !
  !  2. method :
  !
  !     curvilinear grid is defined by the input coordinates as a function
  !     of the (p,q) index coordinates:
  !
  !         x = x(p,q),  y = y(p,q),  dp = dq = 1.
  !
  !     when using spherical coordinates (llg=t) x = longitude and
  !     y = latitude in degrees.  the optional sphere input (default is true)
  !     controls whether or not the spherical coordinate metric is applied.
  !     if sphere is true, then the spherical coordinate metric is applied
  !     to the coordinate derivatives with respect to p & q.  in other words,
  !
  !         dx/dp <= d2r*radius*cos(y)*(dx/dp),
  !         dx/dq <= d2r*radius*cos(y)*(dx/dq),
  !         dy/dp <= d2r*radius*(dy/dp), and
  !         dy/dq <= d2r*radius*(dy/dq).
  !
  !     the default radius is rearth.
  !
  !     the covariant metric tensor components are
  !
  !         g_pp = (dx/dp)*(dx/dp) + (dy/dp)*(dy/dp),
  !         g_qq = (dx/dq)*(dx/dq) + (dy/dq)*(dy/dq),
  !         g_pq = (dx/dp)*(dx/dq) + (dy/dp)*(dy/dq).
  !
  !     the contravariant (associated) metric tensor components are
  !
  !         g^pp = (dp/dx)*(dp/dx) + (dp/dy)*(dp/dy),
  !         g^qq = (dq/dx)*(dq/dx) + (dq/dy)*(dq/dy),
  !         g^pq = (dp/dx)*(dq/dx) + (dp/dy)*(dq/dy).
  !
  !     the curvilinear scale factors are h_p = sqrt(g_pp) and h_q = sqrt(g_qq).
  !     the square root of determinant of metric tensor is
  !
  !         sqrt(|g|) = sqrt( g_pp*g_qq - g_pq^2 )
  !                   = (dx/dp)(dy/dq) - (dx/dq)(dy/dp)
  !                   = h_p*h_q*sqrt(sin(alpha))
  !                   = cell area.
  !
  !     the curvilinear derivatives are computed as
  !
  !         dp/dx =  (1/sqrt(g))*(dy/dq),
  !         dp/dy = -(1/sqrt(g))*(dx/dq),
  !         dq/dx = -(1/sqrt(g))*(dy/dp),
  !         dq/dy =  (1/sqrt(g))*(dx/dp).
  !
  !     orthogonality of grid can be checked by computing angle between the
  !     curvilinear coordinate unit vectors:
  !
  !         cos(alpha) = g_pq/(h_p*h_q) = uvec_p \dot uvec_q,
  !
  !     where
  !
  !         uvec_p = (1/h_p)*(dx/dp)*uvec_x + (1/h_p)*(dy/dp)*uvec_y,
  !         uvec_q = (1/h_q)*(dx/dq)*uvec_x + (1/h_q)*(dy/dq)*uvec_y.
  !
  !     the local cell rotation angle is (assuming orthogonal):
  !
  !         cos(theta) = (1/h_p)*dx/dp,
  !         sin(theta) = (1/h_q)*dy/dp,
  !         theta      = atan2((1/h_q)*dy/dp,(1/h_p)*dx/dp).
  !
  !  3. parameters :
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !       ijg     log.   i   logical flag indicating ordering of input
  !                          coord. arrays: t = (np,nq) and f = (np,nq)
  !       llg     log.   i   spherical coordinate (lon,lat) flag
  !       iclo    int.   i   parameter indicating type of index space closure
  !       ptiled  log.   i   logical flag indicating that input arrays are tiled
  !                          in p-axis with halos of width >= nfd/2
  !       qtiled  log.   i   logical flag indicating that input arrays are tiled
  !                          in q-axis with halos of width >= nfd/2
  !       prange  i.a.   i   range of p index coordinate: p in [prange(1),prange(2)]
  !       qrange  i.a.   i   range of q index coordinate: q in [qrange(1),qrange(2)]
  !       lbi     i.a.   i   lower-bound of  input arrays, dimension(2)
  !       ubi     i.a.   i   upper-bound of  input arrays, dimension(2)
  !       lbo     i.a.   i   lower-bound of output arrays, dimension(2)
  !       ubo     i.a.   i   upper-bound of output arrays, dimension(2)
  !       x       r.a.   i   gridded x-coordinates, dimension(lbi(1):ubi(1),lbi(2):ubi(2))
  !       y       r.a.   i   gridded y-coordinates, dimension(lbi(1):ubi(1),lbi(2):ubi(2))
  !       mask    l.a.   i   optional logical mask (t = invalid, f = valid)
  !                          dimension(lbi(1):ubi(1),lbi(2):ubi(2))
  !       nfd     int.   i   optional finite-difference order (even), default is nfd_default.
  !       sphere  log.   i   optional apply spherical coord metric if llg, default is t
  !       radius  real   i   optional radius for sphere.  default is rearth
  !       dx      real   i   optional constant spacing in x-direction
  !       dy      real   i   optional constant spacing in y-direction
  !       gppc    r.a.   o   optional g_pp,       dimension(lbo(1):ubo(1),lbo(2):ubo(2))
  !       gqqc    r.a.   o   optional g_qq,       dimension(lbo(1):ubo(1),lbo(2):ubo(2))
  !       gpqc    r.a.   o   optional g_pq,       dimension(lbo(1):ubo(1),lbo(2):ubo(2))
  !       gsqr    r.a.   o   optional sqrt(|g|),  dimension(lbo(1):ubo(1),lbo(2):ubo(2))
  !       hpfc    r.a.   o   optional h_p,        dimension(lbo(1):ubo(1),lbo(2):ubo(2))
  !       hqfc    r.a.   o   optional h_q,        dimension(lbo(1):ubo(1),lbo(2):ubo(2))
  !       appc    r.a.   o   optional g^pp,       dimension(lbo(1):ubo(1),lbo(2):ubo(2))
  !       aqqc    r.a.   o   optional g^qq,       dimension(lbo(1):ubo(1),lbo(2):ubo(2))
  !       apqc    r.a.   o   optional g^pq,       dimension(lbo(1):ubo(1),lbo(2):ubo(2))
  !       dxdp    r.a.   o   optional dx/dp,      dimension(lbo(1):ubo(1),lbo(2):ubo(2))
  !       dydp    r.a.   o   optional dy/dp,      dimension(lbo(1):ubo(1),lbo(2):ubo(2))
  !       dxdq    r.a.   o   optional dx/dq,      dimension(lbo(1):ubo(1),lbo(2):ubo(2))
  !       dydq    r.a.   o   optional dy/dq,      dimension(lbo(1):ubo(1),lbo(2):ubo(2))
  !       dpdx    r.a.   o   optional dp/dx,      dimension(lbo(1):ubo(1),lbo(2):ubo(2))
  !       dpdy    r.a.   o   optional dp/dy,      dimension(lbo(1):ubo(1),lbo(2):ubo(2))
  !       dqdx    r.a.   o   optional dq/dx,      dimension(lbo(1):ubo(1),lbo(2):ubo(2))
  !       dqdy    r.a.   o   optional dq/dy,      dimension(lbo(1):ubo(1),lbo(2):ubo(2))
  !       cosa    r.a.   o   optional cos(alpha), dimension(lbo(1):ubo(1),lbo(2):ubo(2))
  !       cosc    r.a.   o   optional cos(theta), dimension(lbo(1):ubo(1),lbo(2):ubo(2))
  !       sinc    r.a.   o   optional sin(theta), dimension(lbo(1):ubo(1),lbo(2):ubo(2))
  !       angl    r.a.   o   optional theta,      dimension(lbo(1):ubo(1),lbo(2):ubo(2))
  !       rc      int.   o   optional return code (!= 0 if error occurs)
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
  !
  !  7. remarks :
  !
  !     - the derivatives and metric will be computed using the constant
  !       spacing dx and/or dy if they are specified.  dx & dy are assumed
  !       to be in degrees when llg = t.
  !     - the grid derivatives (dx/dp, dy/dp, dx/dq, dy/dq) are computed
  !       using a finite difference method.
  !     - when llg = t, the finite differences are done in a polar
  !       stereographic projection.
  !     - if rc is not provided and an error occurs, then the routine will
  !       report error to stderr and attempt to abort the calling program.
  !
  !  8. structure :
  !
  !  9. switches :
  !
  !     !/s    enable subroutine tracing.
  !
  ! 10. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3cgdm_r4( ijg, llg, iclo, ptiled, qtiled, &
       prange, qrange, lbi, ubi, lbo, ubo, x, y, &
       mask, nfd, sphere, radius, dx, dy, &
       gppc, gqqc, gpqc, gsqr, &
       hpfc, hqfc, appc, aqqc, apqc, &
       dxdp, dydp, dxdq, dydq, &
       dpdx, dpdy, dqdx, dqdy, &
       cosa, cosc, sinc, angl, rc )
    !     single precision interface
    logical, intent(in)   :: ijg
    logical, intent(in)   :: llg
    integer, intent(in)   :: iclo
    logical, intent(in)   :: ptiled, qtiled
    integer, intent(in)   :: prange(2), qrange(2)
    integer, intent(in)   :: lbi(2), ubi(2)
    integer, intent(in)   :: lbo(2), ubo(2)
    real(4), intent(in)   :: x(lbi(1):ubi(1),lbi(2):ubi(2))
    real(4), intent(in)   :: y(lbi(1):ubi(1),lbi(2):ubi(2))
    logical, intent(in),  optional :: mask(lbi(1):ubi(1),lbi(2):ubi(2))
    integer, intent(in),  optional :: nfd
    logical, intent(in),  optional :: sphere
    real(4), intent(in),  optional :: radius
    real(4), intent(in),  optional :: dx, dy
    real(4), intent(out), optional :: gppc(lbo(1):ubo(1),lbo(2):ubo(2))
    real(4), intent(out), optional :: gqqc(lbo(1):ubo(1),lbo(2):ubo(2))
    real(4), intent(out), optional :: gpqc(lbo(1):ubo(1),lbo(2):ubo(2))
    real(4), intent(out), optional :: gsqr(lbo(1):ubo(1),lbo(2):ubo(2))
    real(4), intent(out), optional :: hpfc(lbo(1):ubo(1),lbo(2):ubo(2))
    real(4), intent(out), optional :: hqfc(lbo(1):ubo(1),lbo(2):ubo(2))
    real(4), intent(out), optional :: appc(lbo(1):ubo(1),lbo(2):ubo(2))
    real(4), intent(out), optional :: aqqc(lbo(1):ubo(1),lbo(2):ubo(2))
    real(4), intent(out), optional :: apqc(lbo(1):ubo(1),lbo(2):ubo(2))
    real(4), intent(out), optional :: dxdp(lbo(1):ubo(1),lbo(2):ubo(2))
    real(4), intent(out), optional :: dydp(lbo(1):ubo(1),lbo(2):ubo(2))
    real(4), intent(out), optional :: dxdq(lbo(1):ubo(1),lbo(2):ubo(2))
    real(4), intent(out), optional :: dydq(lbo(1):ubo(1),lbo(2):ubo(2))
    real(4), intent(out), optional :: dpdx(lbo(1):ubo(1),lbo(2):ubo(2))
    real(4), intent(out), optional :: dpdy(lbo(1):ubo(1),lbo(2):ubo(2))
    real(4), intent(out), optional :: dqdx(lbo(1):ubo(1),lbo(2):ubo(2))
    real(4), intent(out), optional :: dqdy(lbo(1):ubo(1),lbo(2):ubo(2))
    real(4), intent(out), optional :: cosa(lbo(1):ubo(1),lbo(2):ubo(2))
    real(4), intent(out), optional :: cosc(lbo(1):ubo(1),lbo(2):ubo(2))
    real(4), intent(out), optional :: sinc(lbo(1):ubo(1),lbo(2):ubo(2))
    real(4), intent(out), optional :: angl(lbo(1):ubo(1),lbo(2):ubo(2))
    integer, intent(out), optional :: rc
    !     local parameters
    integer, parameter :: m = 1 ! order of derivative
    real(8), parameter :: small = 1d-15
    integer :: istat=0, n, np, nq, i1, i2, p, q
    logical :: sphr
    real(8) :: r, facx, facy
    integer, allocatable :: k(:,:,:), k2(:,:,:)
    real(8), allocatable :: c(:,:,:), c2(:,:,:)
    real(8) :: gppcl, gqqcl, gpqcl
    real(8) :: gsqrl, hpfcl, hqfcl
    real(8) :: appcl, aqqcl, apqcl
    real(8) :: dxdpl, dydpl, dxdql, dydql
    real(8) :: dpdxl, dpdyl, dqdxl, dqdyl
    real(8) :: cosal, sinal, costp, sintp, coscl, sincl, angll
    ! -------------------------------------------------------------------- /
    ! 1.  check and setup inputs
    !
    if ( present(rc) ) rc = 0
    if ( present(nfd) ) then
      n = nfd
    else
      n = nfd_default
    end if
    if ( n.le.0 .or. mod(n,2).ne.0 ) then
      write(0,'(/1a,1a/)') 'w3cgdm error -- ', &
           'nfd must be even and greater than zero'
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    np = prange(2) - prange(1) + 1
    nq = qrange(2) - qrange(1) + 1
    select case ( iclo )
    case ( iclo_none, iclo_grdi, iclo_grdj, iclo_trdl, iclo_trpl )
      continue
    case default
      write(0,'(/1a,1a,1i2/)') 'w3cgdm error -- ', &
           'unsupported iclo: ',iclo
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end select
    if ( iclo.eq.iclo_trpl .and. mod(np,2).ne.0 ) then
      write(0,'(/1a,1a/)') 'w3cgdm error -- ', &
           'tripole grid closure requires np even'
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    if ( present(sphere) ) then
      sphr = sphere
    else
      sphr = .true.
    end if
    if ( present(radius) ) then
      r = radius
    else
      r = rearth
    end if
    facy = r*d2r
    if ( present(dx) ) then
      if ( dx.le.zero ) then
        write(0,'(/1a,1a/)') 'w3cgdm error -- ','dx must be > 0'
        istat = 1
        if ( present(rc) ) then
          rc = istat
          return
        else
          call extcde (istat)
        end if
      end if
    end if
    if ( present(dy) ) then
      if ( dy.le.zero ) then
        write(0,'(/1a,1a/)') 'w3cgdm error -- ','dy must be > 0'
        istat = 1
        if ( present(rc) ) then
          rc = istat
          return
        else
          call extcde (istat)
        end if
      end if
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  setup finite difference coefficients
    !
    allocate ( k(0:n,0:n,1:n), c(0:n,0:n,1:n), stat=istat )
    if ( istat .ne. 0 ) then
      write(0,'(/1a,1a/)') 'w3cgdm error -- ', &
           'finite difference coeff allocation failed'
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    call get_fdw3 ( n, m, k, c )
    allocate ( k2(0:2,0:2,1:2), c2(0:2,0:2,1:2), stat=istat )
    if ( istat .ne. 0 ) then
      write(0,'(/1a,1a/)') 'w3cgdm error -- ', &
           'finite difference coeff allocation for n=2 failed'
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    call get_fdw3 ( 2, m, k2, c2 )
    !
    ! -------------------------------------------------------------------- /
    ! 3.  compute optional return quantities
    !
    do i2 = lbo(2), ubo(2)
      do i1 = lbo(1), ubo(1)
        if ( ijg ) then
          p = i1
          q = i2
        else
          p = i2
          q = i1
        end if
        if ( present(dx) ) then
          dxdpl = dx
          dydpl = zero
        else
          call dxydp( n, k, c, ijg, llg, iclo, ptiled, qtiled, &
               prange, qrange, lbi, ubi, p, q, dxdpl, dydpl, &
               mask=mask, x4=x, y4=y, rc=istat )
          if ( istat .ne. 0 ) then
            if ( present(rc) ) then
              rc = istat
              return
            else
              call extcde (istat)
            end if
          end if
        end if
        if ( present(dy) ) then
          dxdql = zero
          dydql = dy
        else
          call dxydq( n, k, c, ijg, llg, iclo, ptiled, qtiled, &
               prange, qrange, lbi, ubi, p, q, dxdql, dydql, &
               mask=mask, x4=x, y4=y, rc=istat )
          if ( istat .ne. 0 ) then
            if ( present(rc) ) then
              rc = istat
              return
            else
              call extcde (istat)
            end if
          end if
        end if
        if ( llg .and. sphr ) then
          facx = facy*cos(real(y(i1,i2),8)*d2r)
          dxdpl = dxdpl*facx
          dydpl = dydpl*facy
          dxdql = dxdql*facx
          dydql = dydql*facy
        end if
        gsqrl = dxdpl*dydql - dxdql*dydpl
        if ( gsqrl .lt. zero .and. n .gt. 2 ) then
          !                 write(0,'(1a,1i0,1a,1i0,1a,1i0,2a)') &
          !                 'w3cgdm warning -- nfd = ',n, &
          !                 ' yields gsqrl < 0 at (',p,',',q,'):', &
          !                 ' computing metrics using nfd = 2'
          if ( present(dx) ) then
            dxdpl = dx
            dydpl = zero
          else
            call dxydp( 2, k2, c2, ijg, llg, iclo, ptiled, qtiled, &
                 prange, qrange, lbi, ubi, p, q, dxdpl, dydpl, &
                 mask=mask, x4=x, y4=y, rc=istat )
            if ( istat .ne. 0 ) then
              if ( present(rc) ) then
                rc = istat
                return
              else
                call extcde (istat)
              end if
            end if
          end if
          if ( present(dy) ) then
            dxdql = zero
            dydql = dy
          else
            call dxydq( 2, k2, c2, ijg, llg, iclo, ptiled, qtiled, &
                 prange, qrange, lbi, ubi, p, q, dxdql, dydql, &
                 mask=mask, x4=x, y4=y, rc=istat )
            if ( istat .ne. 0 ) then
              if ( present(rc) ) then
                rc = istat
                return
              else
                call extcde (istat)
              end if
            end if
          end if
          if ( llg .and. sphr ) then
            facx = facy*cos(real(y(i1,i2),8)*d2r)
            dxdpl = dxdpl*facx
            dydpl = dydpl*facy
            dxdql = dxdql*facx
            dydql = dydql*facy
          end if
          gsqrl = dxdpl*dydql - dxdql*dydpl
        end if
        if ( gsqrl .lt. zero ) then
          istat = 1
          write(0,'(/1a,1a)') 'w3cgdm error -- ', &
               'input coordinates do not define a '// &
               'right-handed coordinate system'
          write(0,'(1a,2a6,5a16)') 'w3cgdm error --', &
               'p','q','gsqrl','dxdpl','dydql','dxdql','dydpl'
          write(0,'(1a,2i6,5e16.8/)') 'w3cgdm error --', &
               p,q,gsqrl,dxdpl,dydql,dxdql,dydpl
          if ( present(rc) ) then
            rc = istat
            return
          else
            call extcde (istat)
          end if
        end if
        gppcl = dxdpl*dxdpl + dydpl*dydpl
        gqqcl = dxdql*dxdql + dydql*dydql
        gpqcl = dxdpl*dxdql + dydpl*dydql
        gsqrl = max(gsqrl,small)
        gppcl = max(gppcl,small)
        gqqcl = max(gqqcl,small)
        dpdxl = dydql/gsqrl
        dpdyl =-dxdql/gsqrl
        dqdxl =-dydpl/gsqrl
        dqdyl = dxdpl/gsqrl
        appcl = dpdxl*dpdxl + dpdyl*dpdyl
        aqqcl = dqdxl*dqdxl + dqdyl*dqdyl
        apqcl = dpdxl*dqdxl + dpdyl*dqdyl
        hpfcl = sqrt(gppcl)
        hqfcl = sqrt(gqqcl)
        cosal = gpqcl/(hpfcl*hqfcl)
        sinal = gsqrl**2/(gppcl*gqqcl)
        costp = dxdpl/hpfcl
        sintp = dydpl/hqfcl
        coscl = sinal*costp + cosal*sintp
        sincl = sinal*sintp - cosal*costp
        angll = atan2(sincl,coscl)*r2d
        if (present(gppc)) gppc(i1,i2) = gppcl
        if (present(gqqc)) gqqc(i1,i2) = gqqcl
        if (present(gpqc)) gpqc(i1,i2) = gpqcl
        if (present(appc)) appc(i1,i2) = appcl
        if (present(aqqc)) aqqc(i1,i2) = aqqcl
        if (present(apqc)) apqc(i1,i2) = apqcl
        if (present(gsqr)) gsqr(i1,i2) = gsqrl
        if (present(hpfc)) hpfc(i1,i2) = hpfcl
        if (present(hqfc)) hqfc(i1,i2) = hqfcl
        if (present(dxdp)) dxdp(i1,i2) = dxdpl
        if (present(dydp)) dydp(i1,i2) = dydpl
        if (present(dxdq)) dxdq(i1,i2) = dxdql
        if (present(dydq)) dydq(i1,i2) = dydql
        if (present(dpdx)) dpdx(i1,i2) = dpdxl
        if (present(dpdy)) dpdy(i1,i2) = dpdyl
        if (present(dqdx)) dqdx(i1,i2) = dqdxl
        if (present(dqdy)) dqdy(i1,i2) = dqdyl
        if (present(cosa)) cosa(i1,i2) = cosal
        if (present(cosc)) cosc(i1,i2) = coscl
        if (present(sinc)) sinc(i1,i2) = sincl
        if (present(angl)) angl(i1,i2) = angll
      end do !i1
    end do !i2
    !
    ! -------------------------------------------------------------------- /
    ! 4.  clean up
    !
    deallocate ( k, c, k2, c2 )
  end subroutine w3cgdm_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3cgdm_r8( ijg, llg, iclo, ptiled, qtiled, &
       prange, qrange, lbi, ubi, lbo, ubo, x, y, &
       mask, nfd, sphere, radius, dx, dy, &
       gppc, gqqc, gpqc, gsqr, &
       hpfc, hqfc, appc, aqqc, apqc, &
       dxdp, dydp, dxdq, dydq, &
       dpdx, dpdy, dqdx, dqdy, &
       cosa, cosc, sinc, angl, rc )
    !     double precision interface
    logical, intent(in)   :: ijg
    logical, intent(in)   :: llg
    integer, intent(in)   :: iclo
    logical, intent(in)   :: ptiled, qtiled
    integer, intent(in)   :: prange(2), qrange(2)
    integer, intent(in)   :: lbi(2), ubi(2)
    integer, intent(in)   :: lbo(2), ubo(2)
    real(8), intent(in)   :: x(lbi(1):ubi(1),lbi(2):ubi(2))
    real(8), intent(in)   :: y(lbi(1):ubi(1),lbi(2):ubi(2))
    logical, intent(in),  optional :: mask(lbi(1):ubi(1),lbi(2):ubi(2))
    integer, intent(in),  optional :: nfd
    logical, intent(in),  optional :: sphere
    real(8), intent(in),  optional :: radius
    real(8), intent(in),  optional :: dx, dy
    real(8), intent(out), optional :: gppc(lbo(1):ubo(1),lbo(2):ubo(2))
    real(8), intent(out), optional :: gqqc(lbo(1):ubo(1),lbo(2):ubo(2))
    real(8), intent(out), optional :: gpqc(lbo(1):ubo(1),lbo(2):ubo(2))
    real(8), intent(out), optional :: gsqr(lbo(1):ubo(1),lbo(2):ubo(2))
    real(8), intent(out), optional :: hpfc(lbo(1):ubo(1),lbo(2):ubo(2))
    real(8), intent(out), optional :: hqfc(lbo(1):ubo(1),lbo(2):ubo(2))
    real(8), intent(out), optional :: appc(lbo(1):ubo(1),lbo(2):ubo(2))
    real(8), intent(out), optional :: aqqc(lbo(1):ubo(1),lbo(2):ubo(2))
    real(8), intent(out), optional :: apqc(lbo(1):ubo(1),lbo(2):ubo(2))
    real(8), intent(out), optional :: dxdp(lbo(1):ubo(1),lbo(2):ubo(2))
    real(8), intent(out), optional :: dydp(lbo(1):ubo(1),lbo(2):ubo(2))
    real(8), intent(out), optional :: dxdq(lbo(1):ubo(1),lbo(2):ubo(2))
    real(8), intent(out), optional :: dydq(lbo(1):ubo(1),lbo(2):ubo(2))
    real(8), intent(out), optional :: dpdx(lbo(1):ubo(1),lbo(2):ubo(2))
    real(8), intent(out), optional :: dpdy(lbo(1):ubo(1),lbo(2):ubo(2))
    real(8), intent(out), optional :: dqdx(lbo(1):ubo(1),lbo(2):ubo(2))
    real(8), intent(out), optional :: dqdy(lbo(1):ubo(1),lbo(2):ubo(2))
    real(8), intent(out), optional :: cosa(lbo(1):ubo(1),lbo(2):ubo(2))
    real(8), intent(out), optional :: cosc(lbo(1):ubo(1),lbo(2):ubo(2))
    real(8), intent(out), optional :: sinc(lbo(1):ubo(1),lbo(2):ubo(2))
    real(8), intent(out), optional :: angl(lbo(1):ubo(1),lbo(2):ubo(2))
    integer, intent(out), optional :: rc
    !     local parameters
    integer, parameter :: m = 1 ! order of derivative
    real(8), parameter :: small = 1d-15
    integer :: istat=0, n, np, nq, i1, i2, p, q
    logical :: sphr
    real(8) :: r, facx, facy
    integer, allocatable :: k(:,:,:), k2(:,:,:)
    real(8), allocatable :: c(:,:,:), c2(:,:,:)
    real(8) :: gppcl, gqqcl, gpqcl
    real(8) :: gsqrl, hpfcl, hqfcl
    real(8) :: appcl, aqqcl, apqcl
    real(8) :: dxdpl, dydpl, dxdql, dydql
    real(8) :: dpdxl, dpdyl, dqdxl, dqdyl
    real(8) :: cosal, sinal, costp, sintp, coscl, sincl, angll
    ! -------------------------------------------------------------------- /
    ! 1.  check and setup inputs
    !
    if ( present(rc) ) rc = 0
    if ( present(nfd) ) then
      n = nfd
    else
      n = nfd_default
    end if
    if ( n.le.0 .or. mod(n,2).ne.0 ) then
      write(0,'(/1a,1a/)') 'w3cgdm error -- ', &
           'nfd must be even and greater than zero'
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    np = prange(2) - prange(1) + 1
    nq = qrange(2) - qrange(1) + 1
    select case ( iclo )
    case ( iclo_none, iclo_grdi, iclo_grdj, iclo_trdl, iclo_trpl )
      continue
    case default
      write(0,'(/1a,1a,1i2/)') 'w3cgdm error -- ', &
           'unsupported iclo: ',iclo
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end select
    if ( iclo.eq.iclo_trpl .and. mod(np,2).ne.0 ) then
      write(0,'(/1a,1a/)') 'w3cgdm error -- ', &
           'tripole grid closure requires np even'
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    if ( present(sphere) ) then
      sphr = sphere
    else
      sphr = .true.
    end if
    if ( present(radius) ) then
      r = radius
    else
      r = rearth
    end if
    facy = r*d2r
    if ( present(dx) ) then
      if ( dx.le.zero ) then
        write(0,'(/1a,1a/)') 'w3cgdm error -- ','dx must be > 0'
        istat = 1
        if ( present(rc) ) then
          rc = istat
          return
        else
          call extcde (istat)
        end if
      end if
    end if
    if ( present(dy) ) then
      if ( dy.le.zero ) then
        write(0,'(/1a,1a/)') 'w3cgdm error -- ','dy must be > 0'
        istat = 1
        if ( present(rc) ) then
          rc = istat
          return
        else
          call extcde (istat)
        end if
      end if
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  setup finite difference coefficients
    !
    allocate ( k(0:n,0:n,1:n), c(0:n,0:n,1:n), stat=istat )
    if ( istat .ne. 0 ) then
      write(0,'(/1a,1a/)') 'w3cgdm error -- ', &
           'finite difference coeff allocation failed'
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    call get_fdw3 ( n, m, k, c )
    allocate ( k2(0:2,0:2,1:2), c2(0:2,0:2,1:2), stat=istat )
    if ( istat .ne. 0 ) then
      write(0,'(/1a,1a/)') 'w3cgdm error -- ', &
           'finite difference coeff allocation for n=2 failed'
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    call get_fdw3 ( 2, m, k2, c2 )
    !
    ! -------------------------------------------------------------------- /
    ! 3.  compute optional return quantities
    !
    do i2 = lbo(2), ubo(2)
      do i1 = lbo(1), ubo(1)
        if ( ijg ) then
          p = i1
          q = i2
        else
          p = i2
          q = i1
        end if
        if ( present(dx) ) then
          dxdpl = dx
          dydpl = zero
        else
          call dxydp( n, k, c, ijg, llg, iclo, ptiled, qtiled, &
               prange, qrange, lbi, ubi, p, q, dxdpl, dydpl, &
               mask=mask, x8=x, y8=y, rc=istat )
          if ( istat .ne. 0 ) then
            if ( present(rc) ) then
              rc = istat
              return
            else
              call extcde (istat)
            end if
          end if
        end if
        if ( present(dy) ) then
          dxdql = zero
          dydql = dy
        else
          call dxydq( n, k, c, ijg, llg, iclo, ptiled, qtiled, &
               prange, qrange, lbi, ubi, p, q, dxdql, dydql, &
               mask=mask, x8=x, y8=y, rc=istat )
          if ( istat .ne. 0 ) then
            if ( present(rc) ) then
              rc = istat
              return
            else
              call extcde (istat)
            end if
          end if
        end if
        if ( llg .and. sphr ) then
          facx = facy*cos(real(y(i1,i2),8)*d2r)
          dxdpl = dxdpl*facx
          dydpl = dydpl*facy
          dxdql = dxdql*facx
          dydql = dydql*facy
        end if
        gsqrl = dxdpl*dydql - dxdql*dydpl
        if ( gsqrl .lt. zero .and. n .gt. 2 ) then
          !                 write(0,'(1a,1i0,1a,1i0,1a,1i0,2a)') &
          !                 'w3cgdm warning -- nfd = ',n, &
          !                 ' yields gsqrl < 0 at (',p,',',q,'):', &
          !                 ' computing metrics using nfd = 2'
          if ( present(dx) ) then
            dxdpl = dx
            dydpl = zero
          else
            call dxydp( 2, k2, c2, ijg, llg, iclo, ptiled, qtiled, &
                 prange, qrange, lbi, ubi, p, q, dxdpl, dydpl, &
                 mask=mask, x8=x, y8=y, rc=istat )
            if ( istat .ne. 0 ) then
              if ( present(rc) ) then
                rc = istat
                return
              else
                call extcde (istat)
              end if
            end if
          end if
          if ( present(dy) ) then
            dxdql = zero
            dydql = dy
          else
            call dxydq( 2, k2, c2, ijg, llg, iclo, ptiled, qtiled, &
                 prange, qrange, lbi, ubi, p, q, dxdql, dydql, &
                 mask=mask, x8=x, y8=y, rc=istat )
            if ( istat .ne. 0 ) then
              if ( present(rc) ) then
                rc = istat
                return
              else
                call extcde (istat)
              end if
            end if
          end if
          if ( llg .and. sphr ) then
            facx = facy*cos(real(y(i1,i2),8)*d2r)
            dxdpl = dxdpl*facx
            dydpl = dydpl*facy
            dxdql = dxdql*facx
            dydql = dydql*facy
          end if
          gsqrl = dxdpl*dydql - dxdql*dydpl
        end if
        if ( gsqrl .lt. zero ) then
          istat = 1
          write(0,'(/1a,1a)') 'w3cgdm error -- ', &
               'input coordinates do not define a '// &
               'right-handed coordinate system'
          write(0,'(1a,2a6,5a16)') 'w3cgdm error --', &
               'p','q','gsqrl','dxdpl','dydql','dxdql','dydpl'
          write(0,'(1a,2i6,5e16.8/)') 'w3cgdm error --', &
               p,q,gsqrl,dxdpl,dydql,dxdql,dydpl
          if ( present(rc) ) then
            rc = istat
            return
          else
            call extcde (istat)
          end if
        end if
        gppcl = dxdpl*dxdpl + dydpl*dydpl
        gqqcl = dxdql*dxdql + dydql*dydql
        gpqcl = dxdpl*dxdql + dydpl*dydql
        gsqrl = max(gsqrl,small)
        gppcl = max(gppcl,small)
        gqqcl = max(gqqcl,small)
        dpdxl = dydql/gsqrl
        dpdyl =-dxdql/gsqrl
        dqdxl =-dydpl/gsqrl
        dqdyl = dxdpl/gsqrl
        appcl = dpdxl*dpdxl + dpdyl*dpdyl
        aqqcl = dqdxl*dqdxl + dqdyl*dqdyl
        apqcl = dpdxl*dqdxl + dpdyl*dqdyl
        hpfcl = sqrt(gppcl)
        hqfcl = sqrt(gqqcl)
        cosal = gpqcl/(hpfcl*hqfcl)
        sinal = gsqrl**2/(gppcl*gqqcl)
        costp = dxdpl/hpfcl
        sintp = dydpl/hqfcl
        coscl = sinal*costp + cosal*sintp
        sincl = sinal*sintp - cosal*costp
        angll = atan2(sincl,coscl)*r2d
        if (present(gppc)) gppc(i1,i2) = gppcl
        if (present(gqqc)) gqqc(i1,i2) = gqqcl
        if (present(gpqc)) gpqc(i1,i2) = gpqcl
        if (present(appc)) appc(i1,i2) = appcl
        if (present(aqqc)) aqqc(i1,i2) = aqqcl
        if (present(apqc)) apqc(i1,i2) = apqcl
        if (present(gsqr)) gsqr(i1,i2) = gsqrl
        if (present(hpfc)) hpfc(i1,i2) = hpfcl
        if (present(hqfc)) hqfc(i1,i2) = hqfcl
        if (present(dxdp)) dxdp(i1,i2) = dxdpl
        if (present(dydp)) dydp(i1,i2) = dydpl
        if (present(dxdq)) dxdq(i1,i2) = dxdql
        if (present(dydq)) dydq(i1,i2) = dydql
        if (present(dpdx)) dpdx(i1,i2) = dpdxl
        if (present(dpdy)) dpdy(i1,i2) = dpdyl
        if (present(dqdx)) dqdx(i1,i2) = dqdxl
        if (present(dqdy)) dqdy(i1,i2) = dqdyl
        if (present(cosa)) cosa(i1,i2) = cosal
        if (present(cosc)) cosc(i1,i2) = coscl
        if (present(sinc)) sinc(i1,i2) = sincl
        if (present(angl)) angl(i1,i2) = atan2(sincl,coscl)*r2d
        if (present(angl)) angl(i1,i2) = angll
      end do !i1
    end do !i2
    !
    ! -------------------------------------------------------------------- /
    ! 4.  clean up
    !
    deallocate ( k, c, k2, c2 )
  end subroutine w3cgdm_r8
  !/
  !/ end of w3cgdm ===================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    subroutine w3grd0( nfd, ijg, iclo, ptiled, qtiled, &
  !/                       prange, qrange, lbi, ubi, lbo, ubo, &
  !/                       dpdx, dpdy, dqdx, dqdy, &
  !/                       f, dfdx, dfdy, mask, rc )
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     compute gradient of a scalar field f(x,y) defined on a
  !     curvilinear coordinate grid (x(p,q),y(p,q)).
  !
  !  2. method :
  !
  !     compute derivatives using finite-difference method.
  !     apply curvilinear grid metric.
  !
  !  3. parameters :
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !       nfd     int.   i   finite-difference order (even)
  !       ijg     log.   i   logical flag indicating ordering of input
  !                          coord. arrays: t = (np,nq) and f = (np,nq)
  !       iclo    int.   i   parameter indicating type of index space closure.
  !       ptiled  log.   i   logical flag indicating that input arrays are tiled
  !                          in p-axis with halos of width >= nfd/2
  !       qtiled  log.   i   logical flag indicating that input arrays are tiled
  !                          in q-axis with halos of width >= nfd/2
  !       prange  i.a.   i   range of p index coordinate: p in [prange(1),prange(2)]
  !       qrange  i.a.   i   range of q index coordinate: q in [qrange(1),qrange(2)]
  !       lbi     i.a.   i   lower-bound of  input arrays, dimension(2)
  !       ubi     i.a.   i   upper-bound of  input arrays, dimension(2)
  !       lbo     i.a.   i   lower-bound of output arrays, dimension(2)
  !       ubo     i.a.   i   upper-bound of output arrays, dimension(2)
  !       dpdx    r.a.   i   dp/dx, dimension(lbi(1):ubi(1),lbi(2):ubi(2))
  !       dpdy    r.a.   i   dp/dy, dimension(lbi(1):ubi(1),lbi(2):ubi(2))
  !       dqdx    r.a.   i   dq/dx, dimension(lbi(1):ubi(1),lbi(2):ubi(2))
  !       dqdy    r.a.   i   dq/dy, dimension(lbi(1):ubi(1),lbi(2):ubi(2))
  !       f       r.a.   i   scalar input field, dimension(lbi(1):ubi(1),lbi(2):ubi(2))
  !       dfdx    r.a.   o   df/dx, dimension(lbo(1):ubo(1),lbo(2):ubo(2))
  !       dfdy    r.a.   o   df/dy, dimension(lbo(1):ubo(1),lbo(2):ubo(2))
  !       mask    l.a.   i   optional logical mask (t = invalid, f = valid)
  !                          dimension(lbi(1):ubi(1),lbi(2):ubi(2))
  !       rc      int.   o   optional return code (!= 0 if error occurs)
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
  !
  !  7. remarks :
  !
  !     - if rc is not provided and an error occurs, then the routine will
  !       report error to stderr and attempt to abort the calling program.
  !     - when mask is specified, points that are masked are excluded from
  !       the finite-difference stencil.  in order to avoid reaching across
  !       masked regions, the stencil is modified to one-sided and/or the
  !       finite-difference order is reduced.  if the masking results in a
  !       single point wide channel, then the derivative in the direction
  !       across the channel is set to zero.
  !
  !  8. structure :
  !
  !  9. switches :
  !
  !     !/s    enable subroutine tracing.
  !
  ! 10. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3grd0_r4( nfd, ijg, iclo, ptiled, qtiled, &
       prange, qrange, lbi, ubi, lbo, ubo, &
       dpdx, dpdy, dqdx, dqdy, &
       f, dfdx, dfdy, mask, rc )
    !     single precision interface
    integer, intent(in)   :: nfd
    logical, intent(in)   :: ijg
    integer, intent(in)   :: iclo
    logical, intent(in)   :: ptiled, qtiled
    integer, intent(in)   :: prange(2), qrange(2)
    integer, intent(in)   :: lbi(2), ubi(2)
    integer, intent(in)   :: lbo(2), ubo(2)
    real(4), intent(in)   :: dpdx(lbi(1):ubi(1),lbi(2):ubi(2))
    real(4), intent(in)   :: dpdy(lbi(1):ubi(1),lbi(2):ubi(2))
    real(4), intent(in)   :: dqdx(lbi(1):ubi(1),lbi(2):ubi(2))
    real(4), intent(in)   :: dqdy(lbi(1):ubi(1),lbi(2):ubi(2))
    real(4), intent(in)   ::    f(lbi(1):ubi(1),lbi(2):ubi(2))
    real(4), intent(out)  :: dfdx(lbo(1):ubo(1),lbo(2):ubo(2))
    real(4), intent(out)  :: dfdy(lbo(1):ubo(1),lbo(2):ubo(2))
    logical, intent(in),  optional :: mask(lbi(1):ubi(1),lbi(2):ubi(2))
    integer, intent(out), optional :: rc
    !     local parameters
    integer, parameter :: m = 1 ! order of derivative
    integer :: np, nq, i1, i2, p, q
    integer :: istat=0
    integer :: k(0:nfd,0:nfd,1:nfd)
    real(8) :: c(0:nfd,0:nfd,1:nfd)
    real(8) :: dfdp, dfdq
    ! -------------------------------------------------------------------- /
    ! 1.  check and setup inputs
    !
    if ( present(rc) ) rc = 0
    if ( nfd.le.0 .or. mod(nfd,2).ne.0 ) then
      write(0,'(/1a,1a/)') 'w3grd0 error -- ', &
           'nfd must be even and greater than zero'
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    np = prange(2) - prange(1) + 1
    nq = qrange(2) - qrange(1) + 1
    select case ( iclo )
    case ( iclo_none, iclo_grdi, iclo_grdj, iclo_trdl, iclo_trpl )
      continue
    case default
      write(0,'(/1a,1a,1i2/)') 'w3grd0 error -- ', &
           'unsupported iclo: ',iclo
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end select
    if ( iclo.eq.iclo_trpl .and. mod(np,2).ne.0 ) then
      write(0,'(/1a,1a/)') 'w3grd0 error -- ', &
           'tripole grid closure requires np even'
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  setup finite difference coefficients
    !
    call get_fdw3 ( nfd, m, k, c )
    !
    ! -------------------------------------------------------------------- /
    ! 3.  compute df/dx & df/dy
    !
    do i2 = lbo(2), ubo(2)
      do i1 = lbo(1), ubo(1)
        if ( present(mask) ) then
          if ( mask(i1,i2) ) cycle
        end if
        if ( ijg ) then
          p = i1
          q = i2
        else
          p = i2
          q = i1
        end if
        call dfdpq ( nfd, k, c, ijg, iclo, ptiled, qtiled, &
             prange, qrange, lbi, ubi, p, q, &
             f4=f, dfdp=dfdp, dfdq=dfdq, &
             mask=mask, rc=istat )
        if ( istat .ne. 0 ) then
          if ( present(rc) ) then
            rc = istat
            return
          else
            call extcde (istat)
          end if
        end if
        dfdx(i1,i2) = dfdp*dpdx(i1,i2) + dfdq*dqdx(i1,i2)
        dfdy(i1,i2) = dfdp*dpdy(i1,i2) + dfdq*dqdy(i1,i2)
      end do !i1
    end do !i2
  end subroutine w3grd0_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3grd0_r8( nfd, ijg, iclo, ptiled, qtiled, &
       prange, qrange, lbi, ubi, lbo, ubo, &
       dpdx, dpdy, dqdx, dqdy, &
       f, dfdx, dfdy, mask, rc )
    !     double precision interface
    integer, intent(in)   :: nfd
    logical, intent(in)   :: ijg
    integer, intent(in)   :: iclo
    logical, intent(in)   :: ptiled, qtiled
    integer, intent(in)   :: prange(2), qrange(2)
    integer, intent(in)   :: lbi(2), ubi(2)
    integer, intent(in)   :: lbo(2), ubo(2)
    real(8), intent(in)   :: dpdx(lbi(1):ubi(1),lbi(2):ubi(2))
    real(8), intent(in)   :: dpdy(lbi(1):ubi(1),lbi(2):ubi(2))
    real(8), intent(in)   :: dqdx(lbi(1):ubi(1),lbi(2):ubi(2))
    real(8), intent(in)   :: dqdy(lbi(1):ubi(1),lbi(2):ubi(2))
    real(8), intent(in)   ::    f(lbi(1):ubi(1),lbi(2):ubi(2))
    real(8), intent(out)  :: dfdx(lbo(1):ubo(1),lbo(2):ubo(2))
    real(8), intent(out)  :: dfdy(lbo(1):ubo(1),lbo(2):ubo(2))
    logical, intent(in),  optional :: mask(lbi(1):ubi(1),lbi(2):ubi(2))
    integer, intent(out), optional :: rc
    !     local parameters
    integer, parameter :: m = 1 ! order of derivative
    integer :: np, nq, i1, i2, p, q
    integer :: istat=0
    integer :: k(0:nfd,0:nfd,1:nfd)
    real(8) :: c(0:nfd,0:nfd,1:nfd)
    real(8) :: dfdp, dfdq
    ! -------------------------------------------------------------------- /
    ! 1.  check and setup inputs
    !
    if ( present(rc) ) rc = 0
    if ( nfd.le.0 .or. mod(nfd,2).ne.0 ) then
      write(0,'(/1a,1a/)') 'w3grd0 error -- ', &
           'nfd must be even and greater than zero'
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    np = prange(2) - prange(1) + 1
    nq = qrange(2) - qrange(1) + 1
    select case ( iclo )
    case ( iclo_none, iclo_grdi, iclo_grdj, iclo_trdl, iclo_trpl )
      continue
    case default
      write(0,'(/1a,1a,1i2/)') 'w3grd0 error -- ', &
           'unsupported iclo: ',iclo
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end select
    if ( iclo.eq.iclo_trpl .and. mod(np,2).ne.0 ) then
      write(0,'(/1a,1a/)') 'w3grd0 error -- ', &
           'tripole grid closure requires np even'
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  setup finite difference coefficients
    !
    call get_fdw3 ( nfd, m, k, c )
    !
    ! -------------------------------------------------------------------- /
    ! 3.  compute df/dx & df/dy
    !
    do i2 = lbo(2), ubo(2)
      do i1 = lbo(1), ubo(1)
        if ( present(mask) ) then
          if ( mask(i1,i2) ) cycle
        end if
        if ( ijg ) then
          p = i1
          q = i2
        else
          p = i2
          q = i1
        end if
        call dfdpq ( nfd, k, c, ijg, iclo, ptiled, qtiled, &
             prange, qrange, lbi, ubi, p, q, &
             f8=f, dfdp=dfdp, dfdq=dfdq, &
             mask=mask, rc=istat )
        if ( istat .ne. 0 ) then
          if ( present(rc) ) then
            rc = istat
            return
          else
            call extcde (istat)
          end if
        end if
        dfdx(i1,i2) = dfdp*dpdx(i1,i2) + dfdq*dqdx(i1,i2)
        dfdy(i1,i2) = dfdp*dpdy(i1,i2) + dfdq*dqdy(i1,i2)
      end do !i1
    end do !i2
  end subroutine w3grd0_r8
  !/
  !/ end of w3grd0 ===================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    subroutine w3div1( nfd, ijg, iclo, ptiled, qtiled, &
  !/                       prange, qrange, lbi, ubi, lbo, ubo, &
  !/                       dpdx, dpdy, dqdx, dqdy, &
  !/                       vx, vy, divv, mask, rc )
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     compute divergence of a vector field (v_x,v_y) defined
  !     on a curvilinear coordinate grid (x(p,q),y(p,q)).
  !
  !  2. method :
  !
  !     compute derivatives using finite-difference method.
  !     apply curvilinear grid metric.
  !
  !  3. parameters :
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !       nfd     int.   i   finite-difference order (even)
  !       ijg     log.   i   logical flag indicating ordering of input
  !                          coord. arrays: t = (np,nq) and f = (np,nq)
  !       iclo    int.   i   parameter indicating type of index space closure.
  !       ptiled  log.   i   logical flag indicating that input arrays are tiled
  !                          in p-axis with halos of width >= nfd/2
  !       qtiled  log.   i   logical flag indicating that input arrays are tiled
  !                          in q-axis with halos of width >= nfd/2
  !       prange  i.a.   i   range of p index coordinate: p in [prange(1),prange(2)]
  !       qrange  i.a.   i   range of q index coordinate: q in [qrange(1),qrange(2)]
  !       lbi     i.a.   i   lower-bound of  input arrays, dimension(2)
  !       ubi     i.a.   i   upper-bound of  input arrays, dimension(2)
  !       lbo     i.a.   i   lower-bound of output arrays, dimension(2)
  !       ubo     i.a.   i   upper-bound of output arrays, dimension(2)
  !       dpdx    r.a.   i   dp/dx, dimension(lbi(1):ubi(1),lbi(2):ubi(2))
  !       dpdy    r.a.   i   dp/dy, dimension(lbi(1):ubi(1),lbi(2):ubi(2))
  !       dqdx    r.a.   i   dq/dx, dimension(lbi(1):ubi(1),lbi(2):ubi(2))
  !       dqdy    r.a.   i   dq/dy, dimension(lbi(1):ubi(1),lbi(2):ubi(2))
  !       vx      r.a.   i   x-component of input vector field,
  !                          dimension(lbi(1):ubi(1),lbi(2):ubi(2))
  !       vy      r.a.   i   y-component of input vector field,
  !                          dimension(lbi(1):ubi(1),lbi(2):ubi(2))
  !       divv    r.a.   o   div(v), dimension(lbo(1):ubo(1),lbo(2):ubo(2))
  !       mask    l.a.   i   optional logical mask (t = invalid, f = valid)
  !                          dimension(lbi(1):ubi(1),lbi(2):ubi(2))
  !       rc      int.   o   optional return code (!= 0 if error occurs)
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
  !
  !  7. remarks :
  !
  !     - if rc is not provided and an error occurs, then the routine will
  !       report error to stderr and attempt to abort the calling program.
  !     - when mask is specified, points that are masked are excluded from
  !       the finite-difference stencil.  in order to avoid reaching across
  !       masked regions, the stencil is modified to one-sided and/or the
  !       finite-difference order is reduced.  if the masking results in a
  !       single point wide channel, then the derivative in the direction
  !       across the channel is set to zero.
  !
  !  8. structure :
  !
  !  9. switches :
  !
  !     !/s    enable subroutine tracing.
  !
  ! 10. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3div1_r4( nfd, ijg, iclo, ptiled, qtiled, &
       prange, qrange, lbi, ubi, lbo, ubo, &
       dpdx, dpdy, dqdx, dqdy, &
       vx, vy, divv, mask, rc )
    !     single precision interface
    integer, intent(in)   :: nfd
    logical, intent(in)   :: ijg
    integer, intent(in)   :: iclo
    logical, intent(in)   :: ptiled, qtiled
    integer, intent(in)   :: prange(2), qrange(2)
    integer, intent(in)   :: lbi(2), ubi(2)
    integer, intent(in)   :: lbo(2), ubo(2)
    real(4), intent(in)   :: dpdx(lbi(1):ubi(1),lbi(2):ubi(2))
    real(4), intent(in)   :: dpdy(lbi(1):ubi(1),lbi(2):ubi(2))
    real(4), intent(in)   :: dqdx(lbi(1):ubi(1),lbi(2):ubi(2))
    real(4), intent(in)   :: dqdy(lbi(1):ubi(1),lbi(2):ubi(2))
    real(4), intent(in)   ::   vx(lbi(1):ubi(1),lbi(2):ubi(2))
    real(4), intent(in)   ::   vy(lbi(1):ubi(1),lbi(2):ubi(2))
    real(4), intent(out)  :: divv(lbo(1):ubo(1),lbo(2):ubo(2))
    logical, intent(in),  optional :: mask(lbi(1):ubi(1),lbi(2):ubi(2))
    integer, intent(out), optional :: rc
    !     local parameters
    integer, parameter :: m = 1 ! order of derivative
    integer :: np, nq, i1, i2, p, q
    integer :: istat=0
    integer :: k(0:nfd,0:nfd,1:nfd)
    real(8) :: c(0:nfd,0:nfd,1:nfd)
    real(8) :: dvxdp, dvxdq, dvydp, dvydq
    real(8) :: dvxdx, dvydy
    ! -------------------------------------------------------------------- /
    ! 1.  check and setup inputs
    !
    if ( present(rc) ) rc = 0
    if ( nfd.le.0 .or. mod(nfd,2).ne.0 ) then
      write(0,'(/1a,1a/)') 'w3div1 error -- ', &
           'nfd must be even and greater than zero'
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    np = prange(2) - prange(1) + 1
    nq = qrange(2) - qrange(1) + 1
    select case ( iclo )
    case ( iclo_none, iclo_grdi, iclo_grdj, iclo_trdl, iclo_trpl )
      continue
    case default
      write(0,'(/1a,1a,1i2/)') 'w3div1 error -- ', &
           'unsupported iclo: ',iclo
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end select
    if ( iclo.eq.iclo_trpl .and. mod(np,2).ne.0 ) then
      write(0,'(/1a,1a/)') 'w3div1 error -- ', &
           'tripole grid closure requires np even'
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  setup finite difference coefficients
    !
    call get_fdw3 ( nfd, m, k, c )
    !
    ! -------------------------------------------------------------------- /
    ! 3.  compute div(v) = dv_x/dx + dv_y/dy
    !
    do i2 = lbo(2), ubo(2)
      do i1 = lbo(1), ubo(1)
        if ( present(mask) ) then
          if ( mask(i1,i2) ) cycle
        end if
        if ( ijg ) then
          p = i1
          q = i2
        else
          p = i2
          q = i1
        end if
        call dfdpq ( nfd, k, c, ijg, iclo, ptiled, qtiled, &
             prange, qrange, lbi, ubi, p, q, &
             f4=vx, dfdp=dvxdp, dfdq=dvxdq, &
             g4=vy, dgdp=dvydp, dgdq=dvydq, &
             mask=mask, rc=istat )
        if ( istat .ne. 0 ) then
          if ( present(rc) ) then
            rc = istat
            return
          else
            call extcde (istat)
          end if
        end if
        dvxdx = dvxdp*dpdx(i1,i2) + dvxdq*dqdx(i1,i2)
        dvydy = dvydp*dpdy(i1,i2) + dvydq*dqdy(i1,i2)
        divv(i1,i2) = dvxdx + dvydy
      end do !i1
    end do !i2
  end subroutine w3div1_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3div1_r8( nfd, ijg, iclo, ptiled, qtiled, &
       prange, qrange, lbi, ubi, lbo, ubo, &
       dpdx, dpdy, dqdx, dqdy, &
       vx, vy, divv, mask, rc )
    !     double precision interface
    integer, intent(in)   :: nfd
    logical, intent(in)   :: ijg
    integer, intent(in)   :: iclo
    logical, intent(in)   :: ptiled, qtiled
    integer, intent(in)   :: prange(2), qrange(2)
    integer, intent(in)   :: lbi(2), ubi(2)
    integer, intent(in)   :: lbo(2), ubo(2)
    real(8), intent(in)   :: dpdx(lbi(1):ubi(1),lbi(2):ubi(2))
    real(8), intent(in)   :: dpdy(lbi(1):ubi(1),lbi(2):ubi(2))
    real(8), intent(in)   :: dqdx(lbi(1):ubi(1),lbi(2):ubi(2))
    real(8), intent(in)   :: dqdy(lbi(1):ubi(1),lbi(2):ubi(2))
    real(8), intent(in)   ::   vx(lbi(1):ubi(1),lbi(2):ubi(2))
    real(8), intent(in)   ::   vy(lbi(1):ubi(1),lbi(2):ubi(2))
    real(8), intent(out)  :: divv(lbo(1):ubo(1),lbo(2):ubo(2))
    logical, intent(in),  optional :: mask(lbi(1):ubi(1),lbi(2):ubi(2))
    integer, intent(out), optional :: rc
    !     local parameters
    integer, parameter :: m = 1 ! order of derivative
    integer :: np, nq, i1, i2, p, q
    integer :: istat=0
    integer :: k(0:nfd,0:nfd,1:nfd)
    real(8) :: c(0:nfd,0:nfd,1:nfd)
    real(8) :: dvxdp, dvxdq, dvydp, dvydq
    real(8) :: dvxdx, dvydy
    ! -------------------------------------------------------------------- /
    ! 1.  check and setup inputs
    !
    if ( present(rc) ) rc = 0
    if ( nfd.le.0 .or. mod(nfd,2).ne.0 ) then
      write(0,'(/1a,1a/)') 'w3grd0 error -- ', &
           'nfd must be even and greater than zero'
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    np = prange(2) - prange(1) + 1
    nq = qrange(2) - qrange(1) + 1
    select case ( iclo )
    case ( iclo_none, iclo_grdi, iclo_grdj, iclo_trdl, iclo_trpl )
      continue
    case default
      write(0,'(/1a,1a,1i2/)') 'w3grd0 error -- ', &
           'unsupported iclo: ',iclo
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end select
    if ( iclo.eq.iclo_trpl .and. mod(np,2).ne.0 ) then
      write(0,'(/1a,1a/)') 'w3grd0 error -- ', &
           'tripole grid closure requires np even'
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  setup finite difference coefficients
    !
    call get_fdw3 ( nfd, m, k, c )
    !
    ! -------------------------------------------------------------------- /
    ! 3.  compute div(v) = dv_x/dx + dv_y/dy
    !
    do i2 = lbo(2), ubo(2)
      do i1 = lbo(1), ubo(1)
        if ( present(mask) ) then
          if ( mask(i1,i2) ) cycle
        end if
        if ( ijg ) then
          p = i1
          q = i2
        else
          p = i2
          q = i1
        end if
        call dfdpq ( nfd, k, c, ijg, iclo, ptiled, qtiled, &
             prange, qrange, lbi, ubi, p, q, &
             f8=vx, dfdp=dvxdp, dfdq=dvxdq, &
             g8=vy, dgdp=dvydp, dgdq=dvydq, &
             mask=mask, rc=istat )
        if ( istat .ne. 0 ) then
          if ( present(rc) ) then
            rc = istat
            return
          else
            call extcde (istat)
          end if
        end if
        dvxdx = dvxdp*dpdx(i1,i2) + dvxdq*dqdx(i1,i2)
        dvydy = dvydp*dpdy(i1,i2) + dvydq*dqdy(i1,i2)
        divv(i1,i2) = dvxdx + dvydy
      end do !i1
    end do !i2
  end subroutine w3div1_r8
  !/
  !/ end of w3div1 ===================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    subroutine w3div2( nfd, ijg, iclo, ptiled, qtiled, &
  !/                       prange, qrange, lbi, ubi, lbo, ubo, &
  !/                       dpdx, dpdy, dqdx, dqdy, &
  !/                       sxx, syy, sxy, dsx, dsy, mask, rc )
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     compute divergence of a rank 2 symmetric tensor field (s_xx,s_yy,s_xy)
  !     defined on a curvilinear coordinate grid (x(p,q),y(p,q)).
  !
  !  2. method :
  !
  !     compute derivatives using finite-difference method.
  !     apply curvilinear grid metric.
  !
  !  3. parameters :
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !       nfd     int.   i   finite-difference order (even)
  !       ijg     log.   i   logical flag indicating ordering of input
  !                          coord. arrays: t = (np,nq) and f = (np,nq)
  !       iclo    int.   i   parameter indicating type of index space closure.
  !       ptiled  log.   i   logical flag indicating that input arrays are tiled
  !                          in p-axis with halos of width >= nfd/2
  !       qtiled  log.   i   logical flag indicating that input arrays are tiled
  !                          in q-axis with halos of width >= nfd/2
  !       prange  i.a.   i   range of p index coordinate: p in [prange(1),prange(2)]
  !       qrange  i.a.   i   range of q index coordinate: q in [qrange(1),qrange(2)]
  !       lbi     i.a.   i   lower-bound of  input arrays, dimension(2)
  !       ubi     i.a.   i   upper-bound of  input arrays, dimension(2)
  !       lbo     i.a.   i   lower-bound of output arrays, dimension(2)
  !       ubo     i.a.   i   upper-bound of output arrays, dimension(2)
  !       dpdx    r.a.   i   dp/dx, dimension(lbi(1):ubi(1),lbi(2):ubi(2))
  !       dpdy    r.a.   i   dp/dy, dimension(lbi(1):ubi(1),lbi(2):ubi(2))
  !       dqdx    r.a.   i   dq/dx, dimension(lbi(1):ubi(1),lbi(2):ubi(2))
  !       dqdy    r.a.   i   dq/dy, dimension(lbi(1):ubi(1),lbi(2):ubi(2))
  !       sxx     r.a.   i   xx-component of input tensor field,
  !                          dimension(lbi(1):ubi(1),lbi(2):ubi(2))
  !       syy     r.a.   i   yy-component of input vector field,
  !                          dimension(lbi(1):ubi(1),lbi(2):ubi(2))
  !       sxy     r.a.   i   xy-component of input vector field,
  !                          dimension(lbi(1):ubi(1),lbi(2):ubi(2))
  !       dsx     r.a.   o   div(s)_x, dimension(lbo(1):ubo(1),lbo(2):ubo(2))
  !       dsy     r.a.   o   div(s)_y, dimension(lbo(1):ubo(1),lbo(2):ubo(2))
  !       mask    l.a.   i   optional logical mask (t = invalid, f = valid)
  !                          dimension(lbi(1):ubi(1),lbi(2):ubi(2))
  !       rc      int.   o   optional return code (!= 0 if error occurs)
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
  !
  !  7. remarks :
  !
  !     - if rc is not provided and an error occurs, then the routine will
  !       report error to stderr and attempt to abort the calling program.
  !     - when mask is specified, points that are masked are excluded from
  !       the finite-difference stencil.  in order to avoid reaching across
  !       masked regions, the stencil is modified to one-sided and/or the
  !       finite-difference order is reduced.  if the masking results in a
  !       single point wide channel, then the derivative in the direction
  !       across the channel is set to zero.
  !
  !  8. structure :
  !
  !  9. switches :
  !
  !     !/s    enable subroutine tracing.
  !
  ! 10. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3div2_r4( nfd, ijg, iclo, ptiled, qtiled, &
       prange, qrange, lbi, ubi, lbo, ubo, &
       dpdx, dpdy, dqdx, dqdy, &
       sxx, syy, sxy, dsx, dsy, mask, rc )
    !     single precision interface
    integer, intent(in)   :: nfd
    logical, intent(in)   :: ijg
    integer, intent(in)   :: iclo
    logical, intent(in)   :: ptiled, qtiled
    integer, intent(in)   :: prange(2), qrange(2)
    integer, intent(in)   :: lbi(2), ubi(2)
    integer, intent(in)   :: lbo(2), ubo(2)
    real(4), intent(in)   :: dpdx(lbi(1):ubi(1),lbi(2):ubi(2))
    real(4), intent(in)   :: dpdy(lbi(1):ubi(1),lbi(2):ubi(2))
    real(4), intent(in)   :: dqdx(lbi(1):ubi(1),lbi(2):ubi(2))
    real(4), intent(in)   :: dqdy(lbi(1):ubi(1),lbi(2):ubi(2))
    real(4), intent(in)   ::  sxx(lbi(1):ubi(1),lbi(2):ubi(2))
    real(4), intent(in)   ::  syy(lbi(1):ubi(1),lbi(2):ubi(2))
    real(4), intent(in)   ::  sxy(lbi(1):ubi(1),lbi(2):ubi(2))
    real(4), intent(out)  ::  dsx(lbo(1):ubo(1),lbo(2):ubo(2))
    real(4), intent(out)  ::  dsy(lbo(1):ubo(1),lbo(2):ubo(2))
    logical, intent(in),  optional :: mask(lbi(1):ubi(1),lbi(2):ubi(2))
    integer, intent(out), optional :: rc
    !     local parameters
    integer, parameter :: m = 1 ! order of derivative
    integer :: np, nq, i1, i2, p, q
    integer :: istat=0
    integer :: k(0:nfd,0:nfd,1:nfd)
    real(8) :: c(0:nfd,0:nfd,1:nfd)
    real(8) :: dxxdp, dxxdq, dyydp, dyydq, dxydp, dxydq
    real(8) :: dxxdx, dyydy, dxydx, dxydy
    ! -------------------------------------------------------------------- /
    ! 1.  check and setup inputs
    !
    if ( present(rc) ) rc = 0
    if ( nfd.le.0 .or. mod(nfd,2).ne.0 ) then
      write(0,'(/1a,1a/)') 'w3div2 error -- ', &
           'nfd must be even and greater than zero'
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    np = prange(2) - prange(1) + 1
    nq = qrange(2) - qrange(1) + 1
    select case ( iclo )
    case ( iclo_none, iclo_grdi, iclo_grdj, iclo_trdl, iclo_trpl )
      continue
    case default
      write(0,'(/1a,1a,1i2/)') 'w3div2 error -- ', &
           'unsupported iclo: ',iclo
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end select
    if ( iclo.eq.iclo_trpl .and. mod(np,2).ne.0 ) then
      write(0,'(/1a,1a/)') 'w3div2 error -- ', &
           'tripole grid closure requires np even'
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  setup finite difference coefficients
    !
    call get_fdw3 ( nfd, m, k, c )
    !
    ! -------------------------------------------------------------------- /
    ! 3.  compute div(s) = (ds_xx/dx + ds_xy/dy, ds_xy/dx + ds_yy/dy)
    !
    do i2 = lbo(2), ubo(2)
      do i1 = lbo(1), ubo(1)
        if ( present(mask) ) then
          if ( mask(i1,i2) ) cycle
        end if
        if ( ijg ) then
          p = i1
          q = i2
        else
          p = i2
          q = i1
        end if
        call dfdpq ( nfd, k, c, ijg, iclo, ptiled, qtiled, &
             prange, qrange, lbi, ubi, p, q, &
             f4=sxx, dfdp=dxxdp, dfdq=dxxdq, &
             g4=syy, dgdp=dyydp, dgdq=dyydq, &
             h4=sxy, dhdp=dxydp, dhdq=dxydq, &
             mask=mask, rc=istat )
        if ( istat .ne. 0 ) then
          if ( present(rc) ) then
            rc = istat
            return
          else
            call extcde (istat)
          end if
        end if
        dxxdx = dxxdp*dpdx(i1,i2) + dxxdq*dqdx(i1,i2)
        dyydy = dyydp*dpdy(i1,i2) + dyydq*dqdy(i1,i2)
        dxydx = dxydp*dpdx(i1,i2) + dxydq*dqdx(i1,i2)
        dxydy = dxydp*dpdy(i1,i2) + dxydq*dqdy(i1,i2)
        dsx(i1,i2) = dxxdx + dxydy
        dsy(i1,i2) = dxydx + dyydy
      end do !i1
    end do !i2
  end subroutine w3div2_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3div2_r8( nfd, ijg, iclo, ptiled, qtiled, &
       prange, qrange, lbi, ubi, lbo, ubo, &
       dpdx, dpdy, dqdx, dqdy, &
       sxx, syy, sxy, dsx, dsy, mask, rc )
    !     double precision interface
    integer, intent(in)   :: nfd
    logical, intent(in)   :: ijg
    integer, intent(in)   :: iclo
    logical, intent(in)   :: ptiled, qtiled
    integer, intent(in)   :: prange(2), qrange(2)
    integer, intent(in)   :: lbi(2), ubi(2)
    integer, intent(in)   :: lbo(2), ubo(2)
    real(8), intent(in)   :: dpdx(lbi(1):ubi(1),lbi(2):ubi(2))
    real(8), intent(in)   :: dpdy(lbi(1):ubi(1),lbi(2):ubi(2))
    real(8), intent(in)   :: dqdx(lbi(1):ubi(1),lbi(2):ubi(2))
    real(8), intent(in)   :: dqdy(lbi(1):ubi(1),lbi(2):ubi(2))
    real(8), intent(in)   ::  sxx(lbi(1):ubi(1),lbi(2):ubi(2))
    real(8), intent(in)   ::  syy(lbi(1):ubi(1),lbi(2):ubi(2))
    real(8), intent(in)   ::  sxy(lbi(1):ubi(1),lbi(2):ubi(2))
    real(8), intent(out)  ::  dsx(lbo(1):ubo(1),lbo(2):ubo(2))
    real(8), intent(out)  ::  dsy(lbo(1):ubo(1),lbo(2):ubo(2))
    logical, intent(in),  optional :: mask(lbi(1):ubi(1),lbi(2):ubi(2))
    integer, intent(out), optional :: rc
    !     local parameters
    integer, parameter :: m = 1 ! order of derivative
    integer :: np, nq, i1, i2, p, q
    integer :: istat=0
    integer :: k(0:nfd,0:nfd,1:nfd)
    real(8) :: c(0:nfd,0:nfd,1:nfd)
    real(8) :: dxxdp, dxxdq, dyydp, dyydq, dxydp, dxydq
    real(8) :: dxxdx, dyydy, dxydx, dxydy
    ! -------------------------------------------------------------------- /
    ! 1.  check and setup inputs
    !
    if ( present(rc) ) rc = 0
    if ( nfd.le.0 .or. mod(nfd,2).ne.0 ) then
      write(0,'(/1a,1a/)') 'w3div2 error -- ', &
           'nfd must be even and greater than zero'
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    np = prange(2) - prange(1) + 1
    nq = qrange(2) - qrange(1) + 1
    select case ( iclo )
    case ( iclo_none, iclo_grdi, iclo_grdj, iclo_trdl, iclo_trpl )
      continue
    case default
      write(0,'(/1a,1a,1i2/)') 'w3div2 error -- ', &
           'unsupported iclo: ',iclo
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end select
    if ( iclo.eq.iclo_trpl .and. mod(np,2).ne.0 ) then
      write(0,'(/1a,1a/)') 'w3div2 error -- ', &
           'tripole grid closure requires np even'
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  setup finite difference coefficients
    !
    call get_fdw3 ( nfd, m, k, c )
    !
    ! -------------------------------------------------------------------- /
    ! 3.  compute div(s) = (ds_xx/dx + ds_xy/dy, ds_xy/dx + ds_yy/dy)
    !
    do i2 = lbo(2), ubo(2)
      do i1 = lbo(1), ubo(1)
        if ( present(mask) ) then
          if ( mask(i1,i2) ) cycle
        end if
        if ( ijg ) then
          p = i1
          q = i2
        else
          p = i2
          q = i1
        end if
        call dfdpq ( nfd, k, c, ijg, iclo, ptiled, qtiled, &
             prange, qrange, lbi, ubi, p, q, &
             f8=sxx, dfdp=dxxdp, dfdq=dxxdq, &
             g8=syy, dgdp=dyydp, dgdq=dyydq, &
             h8=sxy, dhdp=dxydp, dhdq=dxydq, &
             mask=mask, rc=istat )
        if ( istat .ne. 0 ) then
          if ( present(rc) ) then
            rc = istat
            return
          else
            call extcde (istat)
          end if
        end if
        dxxdx = dxxdp*dpdx(i1,i2) + dxxdq*dqdx(i1,i2)
        dyydy = dyydp*dpdy(i1,i2) + dyydq*dqdy(i1,i2)
        dxydx = dxydp*dpdx(i1,i2) + dxydq*dqdx(i1,i2)
        dxydy = dxydp*dpdy(i1,i2) + dxydq*dqdy(i1,i2)
        dsx(i1,i2) = dxxdx + dxydy
        dsy(i1,i2) = dxydx + dyydy
      end do !i1
    end do !i2
  end subroutine w3div2_r8
  !/
  !/ end of w3div2 ===================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    function w3dist( llg, xt, yt, xs, ys ) result(dist)
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     compute distance between two points.  if spherical grid, then
  !     distance is the angle (in degrees) between the two points.
  !
  !  2. method :
  !
  !     map projections -- a working manual, john p. snyder
  !     u.s. geological survey professional paper; 1395
  !     chapter 5. transformation of map graticules
  !
  !  3. parameters :
  !
  !     return parameter
  !     ----------------------------------------------------------------
  !       dist    real   o   distance
  !     ----------------------------------------------------------------
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !       llg     log.   i   logical flag indicating the coordinate system:
  !                          t = spherical lat/lon (degrees) and f = cartesian.
  !       xt      real   i   x-coordinate of target point.
  !       yt      real   i   y-coordinate of target point.
  !       xs      real   i   x-coordinate of source point.
  !       ys      real   i   y-coordinate of source point.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
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
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function w3dist_r4( llg, xt, yt, xs, ys ) result(dist)
    !     single precision interface
    real(4)             :: dist
    logical, intent(in) :: llg
    real(4), intent(in) :: xt, yt
    real(4), intent(in) :: xs, ys
    !     local parameters
    real(8) :: xt8, yt8, xs8, ys8
    !
    !-----set inputs
    xt8 = xt; yt8 = yt;
    xs8 = xs; ys8 = ys;
    !
    !-----call double precision method
    dist = w3dist( llg, xt8, yt8, xs8, ys8 )
  end function w3dist_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function w3dist_r8( llg, xt, yt, xs, ys ) result(dist)
    !     double precision interface
    real(8)             :: dist
    logical, intent(in) :: llg
    real(8), intent(in) :: xt, yt
    real(8), intent(in) :: xs, ys
    !     local parameters
    real(8) :: dx, dy, slam, sphi, argd
    !
    !-----compute displacements
    dx = xt - xs
    dy = yt - ys
    if ( llg ) then !spherical coordinates
      !---------check for longitudinal branch cut crossing
      if ( abs(dx) .gt. d270 ) then
        dx = dx - sign(d360,dx)
      end if
      !---------compute angular distance using sin(d/2)
      !         (this equation is more accurate than cos(d))
      slam = sin(half*dx*d2r)
      sphi = sin(half*dy*d2r)
      argd = sqrt( cos(yt*d2r)*cos(ys*d2r)*slam*slam + sphi*sphi )
      dist = r2d*two*asin( argd )
    else !cartesian coordinates
      !---------compute cartesian distance
      dist = sqrt( dx**2 + dy**2 )
    end if !cartesian coordinates
  end function w3dist_r8
  !/
  !/ end of w3dist ===================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    subroutine w3splx( lam0, phi0, c0, lam, phi, x, y )
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     compute cartesian coordinates from input longitude and latitude
  !     using stereographic projection with center at (lam0,phi0) and
  !     "standard circle" of angular distance c0 (in degrees) from the
  !     center.
  !
  !  2. method :
  !
  !     map projections -- a working manual, john p. snyder
  !     u.s. geological survey professional paper; 1395
  !     chapter 21. stereographic projection
  !
  !  3. parameters :
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !       lam0    real   i   longitude of center of projection.
  !       phi0    real   i   latitude of center of projection.
  !       c0      real   i   angular distance from center of projection
  !                          where the scale factor is one.
  !       lam     real   i   longitude of input point.
  !       phi     real   i   latitude of input point.
  !       x       real   o   cartesian x-coordinate of input point.
  !       y       real   o   cartesian y-coordinate of input point.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
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
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3splx_0d_r4( lam0, phi0, c0, lam, phi, x, y )
    !     single precision point interface
    real(4), intent(in) :: lam0, phi0, c0
    real(4), intent(in) :: lam, phi
    real(4), intent(out):: x, y
    !     local parameters
    real(8) :: k, k0, clam, slam, cphi0, cphi, sphi0, sphi
    clam  = cos((lam-lam0)*d2r)
    slam  = sin((lam-lam0)*d2r)
    cphi0 = cos(phi0*d2r)
    cphi  = cos(phi*d2r)
    sphi0 = sin(phi0*d2r)
    sphi  = sin(phi*d2r)
    k0    = cos(half*c0*d2r)**2
    k     = two*k0*rearth/(one+sphi0*sphi+cphi0*cphi*clam)
    x     = k*cphi*slam
    y     = k*(cphi0*sphi-sphi0*cphi*clam)
  end subroutine w3splx_0d_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3splx_0d_r8( lam0, phi0, c0, lam, phi, x, y )
    !     double precision point interface
    real(8), intent(in) :: lam0, phi0, c0
    real(8), intent(in) :: lam, phi
    real(8), intent(out):: x, y
    !     local parameters
    real(8) :: k, k0, clam, slam, cphi0, cphi, sphi0, sphi
    clam  = cos((lam-lam0)*d2r)
    slam  = sin((lam-lam0)*d2r)
    cphi0 = cos(phi0*d2r)
    cphi  = cos(phi*d2r)
    sphi0 = sin(phi0*d2r)
    sphi  = sin(phi*d2r)
    k0    = cos(half*c0*d2r)**2
    k     = two*k0*rearth/(one+sphi0*sphi+cphi0*cphi*clam)
    x     = k*cphi*slam
    y     = k*(cphi0*sphi-sphi0*cphi*clam)
  end subroutine w3splx_0d_r8
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3splx_1d_r4( lam0, phi0, c0, lam, phi, x, y )
    !     single precision 1d array interface
    real(4), intent(in) :: lam0, phi0, c0
    real(4), intent(in) :: lam(:), phi(:)
    real(4), intent(out):: x(:), y(:)
    !     local parameters
    integer :: i
    do i = lbound(lam,1),ubound(lam,1)
      call w3splx( lam0, phi0, c0, lam(i), phi(i), x(i), y(i) )
    enddo
  end subroutine w3splx_1d_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3splx_1d_r8( lam0, phi0, c0, lam, phi, x, y )
    !     double precision 1d array interface
    real(8), intent(in) :: lam0, phi0, c0
    real(8), intent(in) :: lam(:), phi(:)
    real(8), intent(out):: x(:), y(:)
    !     local parameters
    integer :: i
    do i = lbound(lam,1),ubound(lam,1)
      call w3splx( lam0, phi0, c0, lam(i), phi(i), x(i), y(i) )
    enddo
  end subroutine w3splx_1d_r8
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3splx_2d_r4( lam0, phi0, c0, lam, phi, x, y )
    !     single precision 2d array interface
    real(4), intent(in) :: lam0, phi0, c0
    real(4), intent(in) :: lam(:,:), phi(:,:)
    real(4), intent(out):: x(:,:), y(:,:)
    !     local parameters
    integer :: i, j
    do j = lbound(lam,2),ubound(lam,2)
      do i = lbound(lam,1),ubound(lam,1)
        call w3splx( lam0, phi0, c0, lam(i,j), phi(i,j), x(i,j), y(i,j) )
      enddo
    enddo
  end subroutine w3splx_2d_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3splx_2d_r8( lam0, phi0, c0, lam, phi, x, y )
    !     double precision 2d array interface
    real(8), intent(in) :: lam0, phi0, c0
    real(8), intent(in) :: lam(:,:), phi(:,:)
    real(8), intent(out):: x(:,:), y(:,:)
    !     local parameters
    integer :: i, j
    do j = lbound(lam,2),ubound(lam,2)
      do i = lbound(lam,1),ubound(lam,1)
        call w3splx( lam0, phi0, c0, lam(i,j), phi(i,j), x(i,j), y(i,j) )
      enddo
    enddo
  end subroutine w3splx_2d_r8
  !/
  !/ end of w3splx ===================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    subroutine w3spxl( lam0, phi0, x, y, lam, phi )
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     compute longitude and latitude coordinates from input cartesian
  !     coordinates using stereographic projection with center at (lam0,phi0)
  !     and "standard circle" of angular distance c0 (in degrees) from the
  !     center.
  !
  !  2. method :
  !
  !     map projections -- a working manual, john p. snyder
  !     u.s. geological survey professional paper; 1395
  !     chapter 21. stereographic projection
  !
  !  3. parameters :
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !       lam0    real   i   longitude of center of projection.
  !       phi0    real   i   latitude of center of projection.
  !       c0      real   i   angular distance from center of projection
  !                          where the scale factor is one.
  !       x       real   i   cartesian x-coordinate of input point.
  !       y       real   i   cartesian y-coordinate of input point.
  !       lam     real   o   longitude of input point.
  !       phi     real   o   latitude of input point.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
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
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3spxl_0d_r4( lam0, phi0, c0, x, y, lam, phi )
    !     single precision point interface
    real(4), intent(in) :: lam0, phi0, c0
    real(4), intent(in) :: x, y
    real(4), intent(out):: lam, phi
    !     local parameters
    real(8) :: k0, rho, c, cosc, sinc, cphi0, sphi0
    k0    = cos(half*c0*d2r)**2
    rho   = sqrt(x*x+y*y)
    c     = two*atan2(rho,two*rearth*k0)
    cosc  = cos(c)
    sinc  = sin(c)
    cphi0 = cos(phi0*d2r)
    sphi0 = sin(phi0*d2r)
    phi   = asin(cosc*sphi0+y*sinc*cphi0/rho)*r2d
    lam   = lam0 + atan2(x*sinc,rho*cphi0*cosc-y*sphi0*sinc)*r2d
  end subroutine w3spxl_0d_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3spxl_0d_r8( lam0, phi0, c0, x, y, lam, phi )
    !     double precision point interface
    real(8), intent(in) :: lam0, phi0, c0
    real(8), intent(in) :: x, y
    real(8), intent(out):: lam, phi
    !     local parameters
    real(8) :: k0, rho, c, cosc, sinc, cphi0, sphi0
    k0    = cos(half*c0*d2r)**2
    rho   = sqrt(x*x+y*y)
    c     = two*atan2(rho,two*rearth*k0)
    cosc  = cos(c)
    sinc  = sin(c)
    cphi0 = cos(phi0*d2r)
    sphi0 = sin(phi0*d2r)
    phi   = asin(cosc*sphi0+y*sinc*cphi0/rho)*r2d
    lam   = lam0 + atan2(x*sinc,rho*cphi0*cosc-y*sphi0*sinc)*r2d
  end subroutine w3spxl_0d_r8
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3spxl_1d_r4( lam0, phi0, c0, x, y, lam, phi )
    !     single precision 1d array interface
    real(4), intent(in) :: lam0, phi0, c0
    real(4), intent(in) :: x(:), y(:)
    real(4), intent(out):: lam(:), phi(:)
    !     local parameters
    integer :: i
    do i = lbound(x,1),ubound(x,1)
      call w3spxl( lam0, phi0, c0, x(i), y(i), lam(i), phi(i) )
    enddo
  end subroutine w3spxl_1d_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3spxl_1d_r8( lam0, phi0, c0, x, y, lam, phi )
    !     double precision 1d array interface
    real(8), intent(in) :: lam0, phi0, c0
    real(8), intent(in) :: x(:), y(:)
    real(8), intent(out):: lam(:), phi(:)
    !     local parameters
    integer :: i
    do i = lbound(x,1),ubound(x,1)
      call w3spxl( lam0, phi0, c0, x(i), y(i), lam(i), phi(i) )
    enddo
  end subroutine w3spxl_1d_r8
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3spxl_2d_r4( lam0, phi0, c0, x, y, lam, phi )
    !     single precision 2d array interface
    real(4), intent(in) :: lam0, phi0, c0
    real(4), intent(in) :: x(:,:), y(:,:)
    real(4), intent(out):: lam(:,:), phi(:,:)
    !     local parameters
    integer :: i, j
    do j = lbound(x,2),ubound(x,2)
      do i = lbound(x,1),ubound(x,1)
        call w3spxl( lam0, phi0, c0, x(i,j), y(i,j), lam(i,j), phi(i,j) )
      enddo
    enddo
  end subroutine w3spxl_2d_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3spxl_2d_r8( lam0, phi0, c0, x, y, lam, phi )
    !     double precision 2d array interface
    real(8), intent(in) :: lam0, phi0, c0
    real(8), intent(in) :: x(:,:), y(:,:)
    real(8), intent(out):: lam(:,:), phi(:,:)
    !     local parameters
    integer :: i, j
    do j = lbound(x,2),ubound(x,2)
      do i = lbound(x,1),ubound(x,1)
        call w3spxl( lam0, phi0, c0, x(i,j), y(i,j), lam(i,j), phi(i,j) )
      enddo
    enddo
  end subroutine w3spxl_2d_r8
  !/
  !/ end of w3spxl ===================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    subroutine w3trll( lam0, phi0, lam1, phi1, lam, phi )
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     compute longitude and latitude for input coordinates in a
  !     coordinate system with the north pole placed at a latitude
  !     phi0 on a meridian lam0 east of the central meridian.
  !
  !  2. method :
  !
  !     map projections -- a working manual, john p. snyder
  !     u.s. geological survey professional paper; 1395
  !     chapter 5. transformation of map graticules
  !
  !  3. parameters :
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !       lam0    real   i   longitude of north pole
  !       phi0    real   i   latitude of north pole
  !       lam1    real   i   input longitude
  !       phi1    real   i   input latitude
  !       lam     real   o   transformed longitude
  !       phi     real   o   transformed latitude
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
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
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3trll_0d_r4( lam0, phi0, lam1, phi1, lam, phi )
    !     single precision point interface
    real(4), intent(in) :: lam0, phi0
    real(4), intent(in) :: lam1, phi1
    real(4), intent(out):: lam, phi
    !     local parameters
    real(8) :: clam, slam, calp, salp, cphi, sphi
    clam  = cos((lam1-lam0)*d2r)
    slam  = sin((lam1-lam0)*d2r)
    calp  = cos(phi0*d2r)
    salp  = sin(phi0*d2r)
    cphi  = cos(phi1*d2r)
    sphi  = sin(phi1*d2r)
    lam   = lam0 + atan2(cphi*slam,salp*cphi*clam+calp*sphi)*r2d
    phi   = asin(salp*sphi-calp*cphi*clam)*r2d
  end subroutine w3trll_0d_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3trll_0d_r8( lam0, phi0, lam1, phi1, lam, phi )
    !     double precision point interface
    real(8), intent(in) :: lam0, phi0
    real(8), intent(in) :: lam1, phi1
    real(8), intent(out):: lam, phi
    !     local parameters
    real(8) :: clam, slam, calp, salp, cphi, sphi
    clam  = cos((lam1-lam0)*d2r)
    slam  = sin((lam1-lam0)*d2r)
    calp  = cos(phi0*d2r)
    salp  = sin(phi0*d2r)
    cphi  = cos(phi1*d2r)
    sphi  = sin(phi1*d2r)
    lam   = lam0 + atan2(cphi*slam,salp*cphi*clam+calp*sphi)*r2d
    phi   = asin(salp*sphi-calp*cphi*clam)*r2d
  end subroutine w3trll_0d_r8
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3trll_1d_r4( lam0, phi0, lam1, phi1, lam, phi )
    !     single precision 1d array interface
    real(4), intent(in) :: lam0, phi0
    real(4), intent(in) :: lam1(:), phi1(:)
    real(4), intent(out):: lam(:), phi(:)
    !     local parameters
    integer :: i
    do i = lbound(lam1,1),ubound(lam1,1)
      call w3trll( lam0, phi0, lam1(i), phi1(i), lam(i), phi(i) )
    enddo
  end subroutine w3trll_1d_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3trll_1d_r8( lam0, phi0, lam1, phi1, lam, phi )
    !     double precision 1d array interface
    real(8), intent(in) :: lam0, phi0
    real(8), intent(in) :: lam1(:), phi1(:)
    real(8), intent(out):: lam(:), phi(:)
    !     local parameters
    integer :: i
    do i = lbound(lam1,1),ubound(lam1,1)
      call w3trll( lam0, phi0, lam1(i), phi1(i), lam(i), phi(i) )
    enddo
  end subroutine w3trll_1d_r8
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3trll_2d_r4( lam0, phi0, lam1, phi1, lam, phi )
    !     single precision 2d array interface
    real(4), intent(in) :: lam0, phi0
    real(4), intent(in) :: lam1(:,:), phi1(:,:)
    real(4), intent(out):: lam(:,:), phi(:,:)
    !     local parameters
    integer :: i, j
    do j = lbound(lam1,2),ubound(lam1,2)
      do i = lbound(lam1,1),ubound(lam1,1)
        call w3trll( lam0, phi0, lam1(i,j), phi1(i,j), lam(i,j), phi(i,j) )
      enddo
    enddo
  end subroutine w3trll_2d_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3trll_2d_r8( lam0, phi0, lam1, phi1, lam, phi )
    !     double precision 2d array interface
    real(8), intent(in) :: lam0, phi0
    real(8), intent(in) :: lam1(:,:), phi1(:,:)
    real(8), intent(out):: lam(:,:), phi(:,:)
    !     local parameters
    integer :: i, j
    do j = lbound(lam1,2),ubound(lam1,2)
      do i = lbound(lam1,1),ubound(lam1,1)
        call w3trll( lam0, phi0, lam1(i,j), phi1(i,j), lam(i,j), phi(i,j) )
      enddo
    enddo
  end subroutine w3trll_2d_r8
  !/
  !/ end of w3trll ===================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    function w3llaz( lam1, phi1, lam2, phi2 ) result(az)
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     compute azimuth (az) east of north which point (lam2,phi2) bears
  !     to point (lam1,phi1).
  !
  !  2. method :
  !
  !     map projections -- a working manual, john p. snyder
  !     u.s. geological survey professional paper; 1395
  !     chapter 5. transformation of map graticules
  !
  !  3. parameters :
  !
  !     return parameter
  !     ----------------------------------------------------------------
  !       az      real   o   azimuth in degrees east of north
  !     ----------------------------------------------------------------
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !       lam1    real   i   longitude for point 1
  !       phi1    real   i   latitude for point 1
  !       lam2    real   i   longitude for point 2
  !       phi2    real   i   latitude for point 2
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
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
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function w3llaz_r4( lam1, phi1, lam2, phi2 ) result(az)
    !     single precision interface
    real(4)            :: az
    real(4), intent(in):: lam1, phi1
    real(4), intent(in):: lam2, phi2
    !     local parameters
    real(8) :: clam, slam, cph1, sph1, cph2, sph2
    clam = cos((lam2-lam1)*d2r)
    slam = sin((lam2-lam1)*d2r)
    cph1 = cos(phi1*d2r)
    sph1 = sin(phi1*d2r)
    cph2 = cos(phi2*d2r)
    sph2 = sin(phi2*d2r)
    az   = atan2(cph2*slam,cph1*sph2-sph1*cph2*clam)*r2d
  end function w3llaz_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function w3llaz_r8( lam1, phi1, lam2, phi2 ) result(az)
    !     double precision interface
    real(8)            :: az
    real(8), intent(in):: lam1, phi1
    real(8), intent(in):: lam2, phi2
    !     local parameters
    real(8) :: clam, slam, cph1, sph1, cph2, sph2
    clam = cos((lam2-lam1)*d2r)
    slam = sin((lam2-lam1)*d2r)
    cph1 = cos(phi1*d2r)
    sph1 = sin(phi1*d2r)
    cph2 = cos(phi2*d2r)
    sph2 = sin(phi2*d2r)
    az   = atan2(cph2*slam,cph1*sph2-sph1*cph2*clam)*r2d
  end function w3llaz_r8
  !/
  !/ end of w3llaz ===================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    subroutine w3fdwt( n, nd, m, z, x, c )
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     compute finite-difference weights on arbitrarily spaced
  !     1-d node sets.
  !
  !  2. method :
  !
  !     fornberg, b., calculation of weights in finite difference formulas,
  !       siam rev. 40:685-691, 1998.
  !
  !  3. parameters :
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !       n       int.   i   one less than total number of grid points;
  !                          n must not exceed the parameter nd below.
  !       nd      int.   i   dimension of x- and c-arrays in calling program
  !                          x(0:nd) and c(0:nd,0:m), respectively.
  !       m       int.   i   highest derivative for which weights are sought.
  !       z       real   i   location where approximations are to be accurate.
  !       x       r.a.   i   grid point locations, found in x(0:n)
  !       c       r.a.   o   weights at grid locations x(0:n) for derivatives
  !                          of order 0:m, found in c(0:n,0:m)
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
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
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3fdwt_r4 ( n, nd, m, z, x, c )
    !     single precision interface
    integer, intent(in)  :: n, nd, m
    real(4), intent(in)  :: z
    real(4), intent(in)  :: x(0:nd)
    real(4), intent(out) :: c(0:nd,0:m)
    !     local parameters
    integer :: i, j, k, mn
    real(8) :: c1, c2, c3, c4, c5
    c1 = one
    c4 = x(0)-z
    c(:,:) = zero
    c(0,0) = one
    iloop: do i = 1,n
      mn = min(i,m)
      c2 = one
      c5 = c4
      c4 = x(i)-z
      jloop: do j = 0,i-1
        c3 = x(i)-x(j)
        c2 = c2*c3
        if ( j.eq.i-1 ) then
          do k = mn,1,-1
            c(i,k) = c1*(k*c(i-1,k-1)-c5*c(i-1,k))/c2
          end do
          c(i,0) = -c1*c5*c(i-1,0)/c2
        end if
        do k = mn,1,-1
          c(j,k) = (c4*c(j,k)-k*c(j,k-1))/c3
        end do
        c(j,0) = c4*c(j,0)/c3
      end do jloop
      c1 = c2
    end do iloop
  end subroutine w3fdwt_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3fdwt_r8 ( n, nd, m, z, x, c )
    !     double precision interface
    integer, intent(in)  :: n, nd, m
    real(8), intent(in)  :: z
    real(8), intent(in)  :: x(0:nd)
    real(8), intent(out) :: c(0:nd,0:m)
    !     local parameters
    integer :: i, j, k, mn
    real(8) :: c1, c2, c3, c4, c5
    c1 = one
    c4 = x(0)-z
    c(:,:) = zero
    c(0,0) = one
    iloop: do i = 1,n
      mn = min(i,m)
      c2 = one
      c5 = c4
      c4 = x(i)-z
      jloop: do j = 0,i-1
        c3 = x(i)-x(j)
        c2 = c2*c3
        if ( j.eq.i-1 ) then
          do k = mn,1,-1
            c(i,k) = c1*(k*c(i-1,k-1)-c5*c(i-1,k))/c2
          end do
          c(i,0) = -c1*c5*c(i-1,0)/c2
        end if
        do k = mn,1,-1
          c(j,k) = (c4*c(j,k)-k*c(j,k-1))/c3
        end do
        c(j,0) = c4*c(j,0)/c3
      end do jloop
      c1 = c2
    end do iloop
  end subroutine w3fdwt_r8
  !/
  !/ end of w3fdwt ===================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    function w3nnsc( nlvl ) result(nns)
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     create nearest-neighbor (nnbr) search object.
  !
  !  2. method :
  !
  !     notation
  !     ( l,  n):  l = nnbr level;  n = nnbr sequential index
  !     {di, dj}:  di = i-index delta;  dj = j-index delta
  !
  !     ---------------------------------------------------
  !     | ( 2,21) | ( 2,20) | ( 2,19) | ( 2,18) | ( 2,17) |
  !     | {-2,+2} | {-1,+2} | { 0,+2} | {+1,+2} | {+2,+2} |
  !     ---------------------------------------------------
  !     | ( 2,22) | ( 1, 7) | ( 1, 6) | ( 1, 5) | ( 2,16) |
  !     | {-2,+1} | {-1,+1} | { 0,+1} | {+1,+1} | {+2,+1} |
  !     ---------------------------------------------------
  !     | ( 2,23) | ( 1, 8) | ( 0, 0) | ( 1, 4) | ( 2,15) |
  !     | {-2, 0} | {-1, 0} | { 0, 0} | {+1, 0} | {+2, 0} |
  !     ---------------------------------------------------
  !     | ( 2,24) | ( 1, 1) | ( 1, 2) | ( 1, 3) | ( 2,14) |
  !     | {-2,-1} | {-1,-1} | { 0,-1} | {+1,-1} | {+2,-1} |
  !     ---------------------------------------------------
  !     | ( 2, 9) | ( 2,10) | ( 2,11) | ( 2,12) | ( 2,13) |
  !     | {-2,-2} | {-1,-2} | { 0,-2} | {+1,-2} | {+2,-2} |
  !     ---------------------------------------------------
  !
  !  3. parameters :
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
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
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function w3nnsc( nlvl ) result(nns)
    type(t_nns), pointer :: nns
    integer, intent(in)  :: nlvl
    !     local parameters
    integer :: i, j, l, n
    !
    !-----allocate object
    allocate(nns)
    !-----initialize sizes
    nns%nlvl = nlvl
    nns%nnbr = (2*nlvl+1)**2
    !-----allocate arrays
    allocate(nns%n1(0:nns%nlvl))
    allocate(nns%n2(0:nns%nlvl))
    allocate(nns%di(0:nns%nnbr-1))
    allocate(nns%dj(0:nns%nnbr-1))
    !-----compute index deltas for nearest-neighbor searches
    n = 0
    !-----central point
    l = 0
    nns%n1(l) = 0;  nns%n2(l) = (2*l+1)**2-1;
    nns%di(n) = 0;  nns%dj(n) = 0;
    !-----loop over levels
    do l=1,nns%nlvl
      !---------nnbr loop bounds
      nns%n1(l) = (2*l-1)**2;  nns%n2(l) = (2*l+1)**2-1;
      !---------bottom-layer
      j = -l
      do i=-l,l-1
        n = n + 1
        nns%di(n) = i;  nns%dj(n) = j;
      end do
      !---------right-layer
      i =  l
      do j=-l,l-1
        n = n + 1
        nns%di(n) = i;  nns%dj(n) = j;
      end do
      !---------top-layer
      j =  l
      do i=l,-l+1,-1
        n = n + 1
        nns%di(n) = i;  nns%dj(n) = j;
      end do
      !---------left-layer
      i = -l
      do j=l,-l+1,-1
        n = n + 1
        nns%di(n) = i;  nns%dj(n) = j;
      end do
    end do !loop over levels
  end function w3nnsc
  !/
  !/ end of w3nnsc ===================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    subroutine w3nnsd( nns )
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     destroy nearest-neighbor (nnbr) search object.
  !
  !  2. method :
  !
  !  3. parameters :
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
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
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3nnsd( nns )
    type(t_nns), pointer :: nns
    !     local parameters
    !
    if ( associated(nns) ) then
      nns%nlvl = 0
      nns%nnbr = 0
      if ( associated(nns%n1) ) then
        deallocate(nns%n1);  nullify(nns%n1);
      end if
      if ( associated(nns%n2) ) then
        deallocate(nns%n2);  nullify(nns%n2);
      end if
      if ( associated(nns%di) ) then
        deallocate(nns%di);  nullify(nns%di);
      end if
      if ( associated(nns%dj) ) then
        deallocate(nns%dj);  nullify(nns%dj);
      end if
      deallocate(nns)
      nullify(nns)
    end if
  end subroutine w3nnsd
  !/
  !/ end of w3nnsd ===================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    subroutine w3nnsp( nns, iunit )
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     print nearest-neighbor (nnbr) search object to iunit.
  !
  !  2. method :
  !
  !  3. parameters :
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !       nnbr    type   i   nearest-neighbor search object.
  !       iunit   int.   i   optional unit for output. default is stdout.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
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
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3nnsp(nns, iunit)
    type(t_nns), intent(in) :: nns
    integer, optional, intent(in) :: iunit
    !     local parameters
    integer :: ndst, l, n
    !
    if ( present(iunit) ) then
      ndst = iunit
    else
      ndst = 6
    end if
    !
    write(ndst,'(a,2i6)') 'nlvl,nnbr:',nns%nlvl,nns%nnbr
    do l=0,nns%nlvl
      do n=nns%n1(l),nns%n2(l)
        write(ndst,'(a,4i6)') 'l,n,di,dj:',l,n,nns%di(n),nns%dj(n)
      end do
    end do
  end subroutine w3nnsp
  !/
  !/ end of w3nnsp ===================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    subroutine w3sort( n, i, j, d )
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     sort input arrays in increasing order according to input array d.
  !
  !  2. method :
  !
  !  3. parameters :
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
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
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3sort_r4( n, i, j, d )
    !     single precision interface.
    integer, intent(in)    :: n
    integer, intent(inout) :: i(n)
    integer, intent(inout) :: j(n)
    real(4), intent(inout) :: d(n)
    !     local parameters
    integer :: k, l, im, jm
    real(4) :: dm
    do k=1, n-1
      do l=k+1, n
        if ( d(l) .lt. d(k) ) then
          im = i(k);  jm = j(k);  dm = d(k);
          i(k) = i(l);  j(k) = j(l);  d(k) = d(l);
          i(l) = im;  j(l) = jm;  d(l) = dm;
        end if
      end do !l
    end do !k
  end subroutine w3sort_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3sort_r8( n, i, j, d )
    !     double precision interface.
    integer, intent(in)    :: n
    integer, intent(inout) :: i(n)
    integer, intent(inout) :: j(n)
    real(8), intent(inout) :: d(n)
    !     local parameters
    integer :: k, l, im, jm
    real(8) :: dm
    do k=1, n-1
      do l=k+1, n
        if ( d(l) .lt. d(k) ) then
          im = i(k);  jm = j(k);  dm = d(k);
          i(k) = i(l);  j(k) = j(l);  d(k) = d(l);
          i(l) = im;  j(l) = jm;  d(l) = dm;
        end if
      end do !l
    end do !k
  end subroutine w3sort_r8
  !/
  !/ end of w3sort ===================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    subroutine w3isrt( ii, jj, dd, n, i, j, d )
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     insert dd data into d at location where dd < d(k).
  !
  !  2. method :
  !
  !  3. parameters :
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
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
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3isrt_r4( ii, jj, dd, n, i, j, d )
    !     single precision interface
    integer, intent(in)    :: ii
    integer, intent(in)    :: jj
    real(4), intent(in)    :: dd
    integer, intent(in)    :: n
    integer, intent(inout) :: i(n)
    integer, intent(inout) :: j(n)
    real(4), intent(inout) :: d(n)
    !     local parameters
    integer :: k, l
    k_loop: do k=1,n
      if ( dd .lt. d(k) ) then
        !-------------right-shift list (>= k)
        do l=n,k+1,-1
          i(l) = i(l-1);  j(l) = j(l-1);  d(l) = d(l-1);
        end do !l
        !-------------insert point into list at k
        i(k) = ii;  j(k) = jj;  d(k) = dd;
        exit k_loop
      end if !dd.lt.d(k)
    end do k_loop
  end subroutine w3isrt_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine w3isrt_r8( ii, jj, dd, n, i, j, d )
    !     double precision interface
    integer, intent(in)    :: ii
    integer, intent(in)    :: jj
    real(8), intent(in)    :: dd
    integer, intent(in)    :: n
    integer, intent(inout) :: i(n)
    integer, intent(inout) :: j(n)
    real(8), intent(inout) :: d(n)
    !     local parameters
    integer :: k, l
    k_loop: do k=1,n
      if ( dd .lt. d(k) ) then
        !-------------right-shift list (>= k)
        do l=n,k+1,-1
          i(l) = i(l-1);  j(l) = j(l-1);  d(l) = d(l-1);
        end do !l
        !-------------insert point into list at k
        i(k) = ii;  j(k) = jj;  d(k) = dd;
        exit k_loop
      end if !dd.lt.d(k)
    end do k_loop
  end subroutine w3isrt_r8
  !/
  !/ end of w3isrt ===================================================== /
  !/
  !/
  !/ =================================================================== /
  !/
  !/    function w3inan( x ) result(inan)
  !/
  !/ =================================================================== /
  !/
  !  1. purpose :
  !
  !     return true if input is infinite or nan (not a number).
  !
  !  2. method :
  !
  !  3. parameters :
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !     see module documentation.
  !
  !  5. called by :
  !
  !  6. error messages :
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
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function w3inan_r4( x ) result(inan)
    !     single precision interface
    logical             :: inan
    real(4), intent(in) :: x
    !     local parameters
    !-----return true if x is nan or +inf or -inf
    inan = .not. ( x .ge. -huge(x) .and. x .le. huge(x) )
  end function w3inan_r4
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function w3inan_r8( x ) result(inan)
    !     double precision interface
    logical             :: inan
    real(8), intent(in) :: x
    !     local parameters
    !-----return true if x is nan or +inf or -inf
    inan = .not. ( x .ge. -huge(x) .and. x .le. huge(x) )
  end function w3inan_r8
  !/
  !/ end of w3inan ===================================================== /
  !/
  !/
  !/ internal support routines ========================================= /
  !/
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function gsu_create( ijg, llg, iclo, lb, ub, xg4, yg4, xg8, yg8, &
       bbox_only, ncb, nnp, debug ) result(gsu)
    !     *** internal subroutine ***
    type(t_gsu)         :: gsu
    logical, intent(in) :: ijg
    logical, intent(in) :: llg
    integer, intent(in) :: iclo
    integer, intent(in) :: lb(2)
    integer, intent(in) :: ub(2)
    real(4), target, optional     :: xg4(lb(1):ub(1),lb(2):ub(2))
    real(4), target, optional     :: yg4(lb(1):ub(1),lb(2):ub(2))
    real(8), target, optional     :: xg8(lb(1):ub(1),lb(2):ub(2))
    real(8), target, optional     :: yg8(lb(1):ub(1),lb(2):ub(2))
    logical, intent(in), optional :: bbox_only
    integer, intent(in), optional :: ncb
    integer, intent(in), optional :: nnp
    logical, intent(in), optional :: debug
    !     local parameters
    type(class_gsu), pointer :: ptr
    logical :: type_r4, type_r8
    logical :: ldbg, lbbox, lbc, lpl, lnpl, lspl
    integer :: lbx, lby, ubx, uby, nx, ny
    integer :: lxc, lyc, uxc, uyc
    integer :: i, j, k, l, n, ic(4), jc(4), ib, jb
    integer :: ns, ib1(2), ib2(2), jb1(2), jb2(2), ibc(4), jbc(4)
    integer :: istep, istat
    real(8) :: xc(4), yc(4)
    ! -------------------------------------------------------------------- /
    ! 1.  test input
    !
    type_r4 = present(xg4).and.present(yg4)
    type_r8 = present(xg8).and.present(yg8)
    if ( .not.type_r4.and..not.type_r8 ) then
      write(0,'(/1a,1a,1i2/)') 'w3gsuc error -- ', &
           'no input grid coordinates specified'
      call extcde (1)
    end if
    if (ijg) then
      lbx = lb(1)
      lby = lb(2)
      ubx = ub(1)
      uby = ub(2)
    else
      lbx = lb(2)
      lby = lb(1)
      ubx = ub(2)
      uby = ub(1)
    end if
    nx = ubx - lbx + 1
    ny = uby - lby + 1
    select case ( iclo )
    case ( iclo_none, iclo_grdi, iclo_grdj, iclo_trdl, iclo_trpl )
      continue
    case default
      write(0,'(/1a,1a,1i2/)') 'w3gsuc error -- ', &
           'unsupported iclo: ',iclo
      call extcde (1)
    end select
    if ( iclo.eq.iclo_trpl .and. mod(nx,2).ne.0 ) then
      write(0,'(/1a,1a/)') 'w3gsuc error -- ', &
           'tripole grid closure requires nx=ubx-lbx+1 be even'
      call extcde (1)
    end if
    if ( present(bbox_only) ) then
      lbbox = bbox_only
    else
      lbbox = .false.
    end if
    if ( present(ncb) ) then
      if ( ncb .le. 0 ) then
        write(0,'(/1a,1a/)') 'w3gsuc error -- ', &
             'ncb must be greater than zero'
        call extcde (1)
      end if
    end if
    !
    if ( present(debug) ) then
      ldbg = debug
    else
      ldbg = .false.
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  allocate object and set grid related data and pointers
    !
    allocate(ptr, stat=istat)
    if ( istat .ne. 0 ) then
      write(0,'(/1a,1a/)') 'w3gsuc error -- ', &
           'gsu object allocation failed'
      call extcde (istat)
    end if
    ptr%ijg = ijg
    ptr%llg = llg
    ptr%iclo = iclo
    ptr%lbx = lbx
    ptr%lby = lby
    ptr%ubx = ubx
    ptr%uby = uby
    ptr%nx = nx
    ptr%ny = ny
    if (type_r4) then
      ptr%xg4 => xg4
      ptr%yg4 => yg4
      ptr%gkind = 4
    else
      ptr%xg8 => xg8
      ptr%yg8 => yg8
      ptr%gkind = 8
    end if
    nullify( ptr%nnp )
    nullify( ptr%b )
    nullify( ptr%nnb )
    !
    ! -------------------------------------------------------------------- /
    ! 3.  create nearest-neighbor point search object
    !
    if ( .not.lbbox ) then
      if ( present(nnp) ) then
        ptr%nnp => w3nnsc(nnp)
      else
        ptr%nnp => w3nnsc(nnp_default)
      end if
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 4.  construct bucket search "object"
    !
    !-----number of cells
    lxc = lbx;  lyc = lby;
    select case ( iclo )
    case ( iclo_none )
      uxc = ubx-1;  uyc = uby-1;
    case ( iclo_grdi )
      uxc = ubx;    uyc = uby-1;
    case ( iclo_grdj )
      uxc = ubx-1;  uyc = uby;
    case ( iclo_trdl )
      uxc = ubx;    uyc = uby;
    case ( iclo_trpl )
      uxc = ubx;    uyc = uby;
    end select
    !
    !-----initialize longitudinal periodicity flag (lclo)
    if ( llg .and. iclo.ne.iclo_none ) then
      ptr%lclo = .true.
    else
      ptr%lclo = .false.
    end if
    !
    !-----check existence of longitudinal branch cut
    !-----check if source grid includes poles
    if ( ldbg ) then
      write(*,'(/a)') 'w3gsuc - check source grid'
    end if
    lnpl = .false.
    lspl = .false.
    do i=lxc,uxc
      do j=lyc,uyc
        !-------------create list of cell vertices
        ic(1) = i  ;  jc(1) = j  ;
        ic(2) = i+1;  jc(2) = j  ;
        ic(3) = i+1;  jc(3) = j+1;
        ic(4) = i  ;  jc(4) = j+1;
        do l=1,4
          !-----------------apply index closure
          if ( mod(iclo,2).eq.0 ) &
               ic(l) = lbx + mod(nx - 1 + mod(ic(l) - lbx + 1, nx), nx)
          if ( mod(iclo,3).eq.0 ) &
               jc(l) = lby + mod(ny - 1 + mod(jc(l) - lby + 1, ny), ny)
          if ( iclo.eq.iclo_trpl .and. jc(l).gt.uby ) then
            ic(l) = ubx + lbx - ic(l)
            jc(l) = 2*uby - jc(l) + 1
          end if
          !-----------------copy cell vertex coordinates into local variables
          if ( ijg ) then
            if (type_r4) then
              xc(l) = xg4(ic(l),jc(l))
              yc(l) = yg4(ic(l),jc(l))
            else
              xc(l) = xg8(ic(l),jc(l))
              yc(l) = yg8(ic(l),jc(l))
            end if
          else
            if (type_r4) then
              xc(l) = xg4(jc(l),ic(l))
              yc(l) = yg4(jc(l),ic(l))
            else
              xc(l) = xg8(jc(l),ic(l))
              yc(l) = yg8(jc(l),ic(l))
            end if
          end if
        end do !l
        !-------------check if cell includes a pole or branch cut
        lpl = .false.
        lbc = .false.
        if ( llg ) then
          !-----------------count longitudinal branch cut crossings
          n = 0
          do l=1,4
            k = mod(l,4)+1
            if ( abs(xc(k)-xc(l)) .gt. d180 ) n = n + 1
          end do
          !-----------------multiple longitudinal branch cut crossing => cell includes branch cut
          lbc = n.gt.1
          if ( lbc .and. ldbg ) &
               write(*,'(a,8i6)') &
               'w3gsuc -- cell includes branch cut:',ic(:),jc(:)
          !-----------------single longitudinal branch cut crossing
          !                 or single vertex at 90 degrees => cell includes pole
          lpl = n.eq.1 .or. count(abs(yc).eq.d90).eq.1
          if ( lpl.and.minval(yc).gt.zero ) then
            if ( ldbg ) &
                 write(*,'(a,8i6)') &
                 'w3gsuc -- cell includes n-pole:',ic(:),jc(:)
            lnpl = .true.
          end if
          if ( lpl.and.maxval(yc).lt.zero ) then
            if ( ldbg ) &
                 write(*,'(a,8i6)') &
                 'w3gsuc -- cell includes s-pole:',ic(:),jc(:)
            lspl = .true.
          end if
          !-----------------longitudinal branch cut crossing => longitudinal closure
          if ( n.gt.0 ) ptr%lclo = .true.
        end if !llg
      end do !j
    end do !i
    !
    !-----compute domain for search buckets
    !     if longitudinal periodicity, then force domain in x to [0:360]
    !     if grid includes north pole, then set ymax =  90 degrees
    !     if grid includes south pole, then set ymin = -90 degrees
    if (type_r4) then
      ptr%xmin = minval(xg4);  ptr%xmax = maxval(xg4);
      ptr%ymin = minval(yg4);  ptr%ymax = maxval(yg4);
    else
      ptr%xmin = minval(xg8);  ptr%xmax = maxval(xg8);
      ptr%ymin = minval(yg8);  ptr%ymax = maxval(yg8);
    end if
    if ( ptr%lclo ) then
      ptr%xmin =  zero;  ptr%xmax = d360;
    end if
    if ( lspl ) ptr%ymin = -d90
    if ( lnpl ) ptr%ymax =  d90
    ptr%l360 = ptr%xmin.ge.zero
    !
    !-----if bbox only, then set pointer and return
    if ( lbbox ) then
      gsu%ptr => ptr
      return
    end if
    !
    !-----compute number of search buckets and bucket size
    if ( present(ncb) ) then
      ptr%nbx = max(1,nx/ncb)
      ptr%nby = max(1,ny/ncb)
    else
      ptr%nbx = max(1,nx/ncb_default)
      ptr%nby = max(1,ny/ncb_default)
    end if
    ptr%dxb = (ptr%xmax-ptr%xmin)/real(ptr%nbx)
    ptr%dyb = (ptr%ymax-ptr%ymin)/real(ptr%nby)
    !
    !-----print debug info
    if ( ldbg ) then
      write(*,'(/a,1i2,1l2,1i2)') 'w3gsuc - iclo,lclo,gkind: ', &
           ptr%iclo,ptr%lclo,ptr%gkind
      write(*,'(a,4e24.16)') 'w3gsuc - grid search domain:', &
           ptr%xmin,ptr%ymin,ptr%xmax,ptr%ymax
      write(*,'(a,2i6)') 'w3gsuc - number of search buckets:', &
           ptr%nbx,ptr%nby
      write(*,'(a,2e24.16)') 'w3gsuc - search bucket size:', &
           ptr%dxb,ptr%dyb
    end if
    !
    !-----allocate array of search buckets
    allocate(ptr%b(ptr%nby,ptr%nbx),stat=istat)
    if ( istat .ne. 0 ) then
      write(0,'(/1a,1a/)') 'w3gsuc error -- ', &
           'search bucket array allocation failed'
      call extcde (istat)
    end if
    !
    !-----begin istep_loop
    !     first step: compute number of cells in each bucket
    !     second step: allocate buckets and assign cells to buckets
    istep_loop: do istep=1,2
      !
      !-----allocate search bucket cell lists
      if ( istep .eq. 2 ) then
        do ib=1,ptr%nbx
          do jb=1,ptr%nby
            nullify(ptr%b(jb,ib)%i)
            nullify(ptr%b(jb,ib)%j)
            if ( ptr%b(jb,ib)%n .gt. 0 ) then
              allocate(ptr%b(jb,ib)%i(ptr%b(jb,ib)%n),stat=istat)
              if ( istat .ne. 0 ) then
                write(0,'(/1a,2a,3i6/)') 'w3gsuc error -- ', &
                     'search bucket cell-i list allocation failed -- ', &
                     'bucket: ',ib,jb,n
                call extcde (istat)
              end if
              allocate(ptr%b(jb,ib)%j(ptr%b(jb,ib)%n),stat=istat)
              if ( istat .ne. 0 ) then
                write(0,'(/1a,2a,3i6/)') 'w3gsuc error -- ', &
                     'search bucket cell-j list allocation failed -- ', &
                     'bucket: ',ib,jb,n
                call extcde (istat)
              end if
            end if
          end do
        end do
      end if !istep.eq.2
      !
      !-----build search bucket cell lists
      ptr%b(:,:)%n = 0
      do i=lxc,uxc
        do j=lyc,uyc
          if ( iclo.eq.iclo_trpl ) then
            if ( j.eq.uyc .and. i.gt.lbx+nx/2 ) cycle
          endif
          !-------------create list of cell vertices
          ic(1) = i  ;  jc(1) = j  ;
          ic(2) = i+1;  jc(2) = j  ;
          ic(3) = i+1;  jc(3) = j+1;
          ic(4) = i  ;  jc(4) = j+1;
          do l=1,4
            !-----------------apply index closure
            if ( mod(iclo,2).eq.0 ) &
                 ic(l) = lbx + mod(nx - 1 + mod(ic(l) - lbx + 1, nx), nx)
            if ( mod(iclo,3).eq.0 ) &
                 jc(l) = lby + mod(ny - 1 + mod(jc(l) - lby + 1, ny), ny)
            if ( iclo.eq.iclo_trpl .and. jc(l).gt.uby ) then
              ic(l) = ubx + lbx - ic(l)
              jc(l) = 2*uby - jc(l) + 1
            end if
            !-----------------copy cell vertex coordinates into local variables
            if ( ijg ) then
              if (type_r4) then
                xc(l) = xg4(ic(l),jc(l))
                yc(l) = yg4(ic(l),jc(l))
              else
                xc(l) = xg8(ic(l),jc(l))
                yc(l) = yg8(ic(l),jc(l))
              end if
            else
              if (type_r4) then
                xc(l) = xg4(jc(l),ic(l))
                yc(l) = yg4(jc(l),ic(l))
              else
                xc(l) = xg8(jc(l),ic(l))
                yc(l) = yg8(jc(l),ic(l))
              end if
            end if
          end do !l
          !-------------check if cell includes a pole or branch cut
          lpl = .false.
          lbc = .false.
          if ( llg ) then
            !-----------------shift longitudes to appropriate range
            xc = mod(xc,d360)
            if ( ptr%lclo .or. ptr%l360 ) then
              where ( xc.lt.zero ) xc = xc + d360
            else
              where ( xc.gt.d180 ) xc = xc - d360
            end if
            !-----------------count longitudinal branch cut crossings
            n = 0
            do l=1,4
              k = mod(l,4)+1
              if ( abs(xc(k)-xc(l)) .gt. d180 ) n = n + 1
            end do
            !-----------------multiple longitudinal branch cut crossing => cell includes branch cut
            lbc = n.gt.1
            !-----------------single longitudinal branch cut crossing
            !                 or single vertex at 90 degrees => cell includes pole
            lpl = n.eq.1 .or. count(abs(yc).eq.d90).eq.1
          end if !llg
          !-------------set bucket id for each cell vertex
          do l=1,4
            ibc(l) = int((xc(l)-ptr%xmin)/ptr%dxb)+1
            if ( .not.ptr%lclo ) ibc(l) = min(ibc(l),ptr%nbx)
            jbc(l) = min(int((yc(l)-ptr%ymin)/ptr%dyb)+1,ptr%nby)
          end do !l
          !-------------set bucket overlap bounds
          if ( lpl ) then
            !---------------cell includes pole: overlap includes full longitudinal range
            ns = 1
            ib1(1) = 1
            ib2(1) = ptr%nbx
            if ( minval(yc).gt.zero ) then
              jb1(1) = max(1,minval(jbc))
              jb2(1) = ptr%nby
            end if
            if ( maxval(yc).lt.zero ) then
              jb1(1) = 1
              jb2(1) = min(ptr%nby,maxval(jbc))
            end if
            ib1(2) = 0
            ib2(2) = 0
            jb1(2) = 0
            jb2(2) = 0
          else if ( lbc ) then
            !---------------cell includes branch cut: split overlap into two sets
            ns = 2
            ib1(1) = ptr%nbx
            ib2(1) = ptr%nbx
            ib1(2) = 1
            ib2(2) = 1
            do l=1,4
              if ( ibc(l) .gt. ptr%nbx/2 ) then
                ib1(1) = min(ib1(1),ibc(l))
              else
                ib2(2) = max(ib2(2),ibc(l))
              end if
            end do !l
            jb1(:) = max(1,minval(jbc))
            jb2(:) = min(ptr%nby,maxval(jbc))
          else
            !---------------default: overlap computed from min/max
            ns = 1
            ib1(1) = max(1,minval(ibc))
            ib2(1) = min(ptr%nbx,maxval(ibc))
            jb1(1) = max(1,minval(jbc))
            jb2(1) = min(ptr%nby,maxval(jbc))
            ib1(2) = 0
            ib2(2) = 0
            jb1(2) = 0
            jb2(2) = 0
          end if
          !-------------debug output
          if ( ldbg .and. istep.eq.1 ) then
            write(*,'(/a,2i6)')    'w3gsuc -- bucket sort:',i,j
            write(*,'(a,2l6,1i6)') 'w3gsuc -- lbc,lpl:',lbc,lpl
            write(*,'(a,4i6)')     'w3gsuc -- ic:',ic(:)
            write(*,'(a,4i6)')     'w3gsuc -- jc:',jc(:)
            write(*,'(a,4e24.16)') 'w3gsuc -- xc:',xc(:)
            write(*,'(a,4e24.16)') 'w3gsuc -- yc:',yc(:)
            write(*,'(a,4i6)')     'w3gsuc -- ibc:',ibc(:)
            write(*,'(a,4i6)')     'w3gsuc -- jbc:',jbc(:)
            write(*,'(a,1i6)')     'w3gsuc -- ns:',ns
            write(*,'(a,4i6)')     'w3gsuc -- ib1:',ib1(:)
            write(*,'(a,4i6)')     'w3gsuc -- jb1:',jb1(:)
            write(*,'(a,4i6)')     'w3gsuc -- ib2:',ib2(:)
            write(*,'(a,4i6)')     'w3gsuc -- jb2:',jb2(:)
          end if
          !-------------assign cell to buckets based on overlap
          do k=1,ns
            do ib=ib1(k),ib2(k)
              do jb=jb1(k),jb2(k)
                ptr%b(jb,ib)%n = ptr%b(jb,ib)%n + 1
                if ( istep .eq. 2 ) then
                  ptr%b(jb,ib)%i(ptr%b(jb,ib)%n) = ic(1)
                  ptr%b(jb,ib)%j(ptr%b(jb,ib)%n) = jc(1)
                end if
              end do !jb
            end do !ib
          end do !k
        end do !j
      end do !i
      !
      !-----end istep_loop
    end do istep_loop
    !
    !-----create nearest-neighbor bucket search object
    ptr%nnb => w3nnsc(nint(half*max(ptr%nbx,ptr%nby)))
    !
    !-----print debug info
    if ( ldbg ) then
      write(*,'(/a,3i6,4e24.16)') 'w3gsuc - search bucket list:'
      write(*,'(3a6,4a14)') 'i','j','n','x1','y1','x2','y2'
      do ib=1,ptr%nbx
        do jb=1,ptr%nby
          write(*,'(3i6,4e24.16)') ib,jb,ptr%b(jb,ib)%n, &
               ptr%xmin+(ib-1)*ptr%dxb,ptr%ymin+(jb-1)*ptr%dyb, &
               ptr%xmin+(ib-0)*ptr%dxb,ptr%ymin+(jb-0)*ptr%dyb
        end do
      end do
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 5.  set return parameter
    !
    gsu%ptr => ptr
  end function gsu_create
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine getpqr( xt, yt, xs, ys, pr, qr, eps, debug )
    !     *** internal subroutine ***
    !     compute source grid cell-relative coordinates (pr,qr) for target point (xt,yt)
    real(8), intent(in)   :: xt
    real(8), intent(in)   :: yt
    real(8), intent(in)   :: xs(4)
    real(8), intent(in)   :: ys(4)
    real(8), intent(out)  :: pr
    real(8), intent(out)  :: qr
    real(8), intent(in),  optional :: eps
    logical, intent(in) , optional :: debug
    !     local parameters
    integer, parameter :: max_iter = 10
    real(8), parameter :: converge = 1d-6
    real(8) :: leps
    logical :: ldbg
    integer :: k, iter
    real(8) :: dxt, dx1, dx2, dx3, dxp, dyt, dy1, dy2, dy3, dyp
    real(8) :: mat1, mat2, mat3, mat4, delp, delq, det
    if ( present(eps) ) then
      if ( eps .lt. zero ) then
        write(0,'(/2a/)') 'getpqr error -- ', &
             'eps parameter must be >= 0'
        call extcde (1)
      end if
      leps = eps
    else
      leps = eps_default
    end if
    if ( present(debug) ) then
      ldbg = debug
    else
      ldbg = .false.
    end if
    !
    !-----handle point coincident with a cell vertex
    do k=1,4
      if ( abs(xt-xs(k)).le.leps .and. abs(yt-ys(k)).le.leps ) then
        select case ( k )
        case ( 1 )
          pr = zero;  qr = zero;
        case ( 2 )
          pr =  one;  qr = zero;
        case ( 3 )
          pr =  one;  qr =  one;
        case ( 4 )
          pr = zero;  qr =  one;
        end select
        if ( ldbg ) &
             write(*,'(a,i3,4e24.16)') 'getpqr - coincident:', &
             k,abs(xt-xs(k)),abs(yt-ys(k)),pr,qr
        return
      end if
    end do
    !
    !-----set iteration parameters and initial guess
    pr = half
    qr = half
    dyt = yt    - ys(1)
    dy1 = ys(2) - ys(1)
    dy2 = ys(4) - ys(1)
    dy3 = ys(3) - ys(2) - dy2
    dxt = xt    - xs(1)
    dx1 = xs(2) - xs(1)
    dx2 = xs(4) - xs(1)
    dx3 = xs(3) - xs(2) - dx2
    !-----iterate to find (pr,qr)
    iter_loop: do iter=1,max_iter
      dyp  = dyt - dy1*pr - dy2*qr - dy3*pr*qr
      dxp  = dxt - dx1*pr - dx2*qr - dx3*pr*qr
      mat1 = dy1 + dy3*qr
      mat2 = dy2 + dy3*pr
      mat3 = dx1 + dx3*qr
      mat4 = dx2 + dx3*pr
      det  = mat1*mat4 - mat2*mat3
      delp = (dyp*mat4 - mat2*dxp)/det
      delq = (mat1*dxp - dyp*mat3)/det
      if ( ldbg ) &
           write(*,'(a,i3,4e24.16)') 'getpqr - iter:', &
           iter,pr,qr,delp,delq
      pr = pr + delp
      qr = qr + delq
      if ( abs(delp) < converge .and. &
           abs(delq) < converge ) exit iter_loop
    end do iter_loop
    !-----if max iteration count exceeded, then exit with error
    if ( iter .gt. max_iter ) then
      write(0,'(/a)') &
           'getpqr -- error: exceeded max iteration count'
      write(0,'(a,2e24.16)') 'getpqr - dest point coords: ',xt,yt
      do k=1,4
        write(0,'(a,i1,a,2e24.16)') &
             'getpqr - src point ',k,': ',xs(k),ys(k)
      end do
      write(0,'(a,4e24.16)') &
           'getpqr - current pr,qr,delp,delq: ',pr,qr,delp,delq
      call extcde (1)
    end if !(iter.le.max_iter)
  end subroutine getpqr
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine getblc( gsu, i, j, pr, qr, lcmp, ns, ls, is, js, cs )
    !     *** internal subroutine ***
    !     compute bilinear remap factors for a given point (p,q)
    !     (i,j) = lower-left corner point of grid cell containing target point
    !     (pr,qr) = cell-relative coordinate of target point
    !     double precision interface
    type(t_gsu), intent(in) :: gsu
    integer, intent(in)  :: i, j
    real(8), intent(in)  :: pr, qr
    logical, intent(in)  :: lcmp
    integer, intent(out) :: ns
    logical, pointer, intent(inout) :: ls(:)
    integer, pointer, intent(inout) :: is(:), js(:)
    real(8), pointer, intent(inout) :: cs(:)
    !     local parameters
    logical :: ijg, llg, lclo
    integer :: iclo, gkind
    integer :: lbx, lby, ubx, uby, nx, ny
    integer :: istat, k
    !
    !---- initialize
    if ( .not.associated(gsu%ptr) ) then
      write(0,'(/2a/)') 'getblc error -- ', &
           'grid search utility object not created'
      call extcde (1)
    end if
    ijg = gsu%ptr%ijg
    llg = gsu%ptr%llg
    iclo = gsu%ptr%iclo
    lclo = gsu%ptr%lclo
    gkind = gsu%ptr%gkind
    lbx = gsu%ptr%lbx;  lby = gsu%ptr%lby;
    ubx = gsu%ptr%ubx;  uby = gsu%ptr%uby;
    nx = gsu%ptr%nx;  ny = gsu%ptr%ny;
    !
    !---- check & deallocate
    if ( associated(ls) ) then
      deallocate(ls);  nullify(ls);
    end if
    if ( associated(is) ) then
      deallocate(is);  nullify(is);
    end if
    if ( associated(js) ) then
      deallocate(js);  nullify(js);
    end if
    if ( associated(cs) ) then
      deallocate(cs);  nullify(cs);
    end if
    !
    !---- set number of interpolation points and allocate arrays
    ns = 4
    allocate( ls(ns), is(ns), js(ns), cs(ns), stat=istat )
    if ( istat .ne. 0 ) then
      write(0,'(/1a,1a/)') 'getblc error -- ', &
           'array allocation failed'
      call extcde (istat)
    end if
    ls(:) = .true.
    cs(:) = zero
    !
    !---- 4 source points for the bilinear interpolation
    !     (4)------------------(3)
    !      |                    |
    !      |  pr                |
    !      |-----.              |
    !      |     |              |
    !      |     |qr            |
    !      |     |              |
    !     (1)------------------(2)
    is(1) = i  ;  js(1) = j  ;
    is(2) = i+1;  js(2) = j  ;
    is(3) = i+1;  js(3) = j+1;
    is(4) = i  ;  js(4) = j+1;
    !
    !---- apply index closure
    do k=1,ns
      if ( mod(iclo,2).eq.0 ) &
           is(k) = lbx + mod(nx - 1 + mod(is(k) - lbx + 1, nx), nx)
      if ( mod(iclo,3).eq.0 ) &
           js(k) = lby + mod(ny - 1 + mod(js(k) - lby + 1, ny), ny)
      if ( iclo.eq.iclo_trpl .and. js(k).gt.uby ) then
        is(k) = ubx + lbx - is(k)
        js(k) = 2*uby - js(k) + 1
      end if
    end do
    !
    !---- calculate bilinear interpolation coefficients
    if ( lcmp ) then
      cs(1) = (one-pr)*(one-qr)
      cs(2) = pr*(one-qr)
      cs(3) = pr*qr
      cs(4) = (one-pr)*qr
    end if
  end subroutine getblc
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine getbcc( gsu, i, j, pr, qr, lcmp, ns, ls, is, js, cs )
    !     *** internal subroutine ***
    !     compute bicubic remap factors for a given point (p,q)
    !     (i,j) = lower-left corner point of grid cell containing target point
    !     (pr,qr) = cell-relative coordinate of target point
    type(t_gsu), intent(in) :: gsu
    integer, intent(in)  :: i, j
    real(8), intent(in)  :: pr, qr
    logical, intent(in)  :: lcmp
    integer, intent(out) :: ns
    logical, pointer, intent(inout) :: ls(:)
    integer, pointer, intent(inout) :: is(:), js(:)
    real(8), pointer, intent(inout) :: cs(:)
    !     local parameters
    real(8), parameter :: small = 1d-6
    logical :: ijg, llg, lclo
    integer :: iclo, gkind
    integer :: lbx, lby, ubx, uby, nx, ny
    integer :: istat, p, q, ii, jj, k, l, n, m
    real(8) :: pv(0:3), qv(0:3), pw(0:3), qw(0:3)
    real(8) :: a(0:1,0:1,0:3)
    real(8) :: w(0:3,0:3) = reshape((/  1,  0, -3,  2, &
         0,  0,  3, -2, &
         0,  1, -2,  1, &
         0,  0, -1,  1 /), &
         (/4,4/))
    integer, parameter :: nfd = 2 ! finite-difference order (even)
    integer :: kfd(0:nfd,0:nfd) = reshape((/  0,  1,  2, &
         -1,  0,  1, &
         -2, -1,  0 /), &
         (/nfd+1,nfd+1/))
    real(8) :: cfd(0:nfd,0:nfd) = half* reshape((/ -3,  4, -1, &
         -1,  0,  1, &
         1, -4,  3 /), &
         (/nfd+1,nfd+1/))
    real(8) :: cs2d(-nfd/2:nfd,-nfd/2:nfd)
    !
    !---- initialize
    if ( .not.associated(gsu%ptr) ) then
      write(0,'(/2a/)') 'getbcc error -- ', &
           'grid search utility object not created'
      call extcde (1)
    end if
    ijg = gsu%ptr%ijg
    llg = gsu%ptr%llg
    iclo = gsu%ptr%iclo
    lclo = gsu%ptr%lclo
    gkind = gsu%ptr%gkind
    lbx = gsu%ptr%lbx;  lby = gsu%ptr%lby;
    ubx = gsu%ptr%ubx;  uby = gsu%ptr%uby;
    nx = gsu%ptr%nx;  ny = gsu%ptr%ny;
    !
    !---- check & deallocate
    if ( associated(ls) ) then
      deallocate(ls);  nullify(ls);
    end if
    if ( associated(is) ) then
      deallocate(is);  nullify(is);
    end if
    if ( associated(js) ) then
      deallocate(js);  nullify(js);
    end if
    if ( associated(cs) ) then
      deallocate(cs);  nullify(cs);
    end if
    !
    !---- setup table of bicubic coefficients
    !
    !    (0,1)----------------(1,1)
    !      |                    |
    !      |                    |
    !      |-----x(pr,qr)       |
    !      |     |              |
    !      |     |              |
    !      |     |              |
    !    (0,0)----------------(1,0)
    !
    !     pv = [ pr**0, pr**1, pr**2, pr**3 ]^t
    !     qv = [ qr**0, qr**1, qr**2, qr**3 ]^t
    !
    !     pw = w*pv
    !     qw = w*qv
    !
    !     a(i,j,0) = pw(i  )*qw(j  )
    !     a(i,j,1) = pw(i+2)*qw(j  )
    !     a(i,j,2) = pw(i  )*qw(j+2)
    !     a(i,j,3) = pw(i+2)*qw(j+2)
    !
    !     f(pr,qr) = sum[i=0:1]{ sum[j=0:1]{
    !                    a(i,j,0) *   f(i,j) +
    !                    a(i,j,1) *  fp(i,j) +
    !                    a(i,j,2) *  fq(i,j) +
    !                    a(i,j,3) * fpq(i,j) } }
    !
    do k=0,3
      pv(k) = pr**k
      qv(k) = qr**k
    end do
    pw = matmul(pv,w)
    qw = matmul(qv,w)
    do jj=0,1
      do ii=0,1
        a(ii,jj,0) = pw(ii)  *qw(jj)
        a(ii,jj,1) = pw(ii+2)*qw(jj)
        a(ii,jj,2) = pw(ii)  *qw(jj+2)
        a(ii,jj,3) = pw(ii+2)*qw(jj+2)
      end do
    end do
    !
    !---- source points for the bicubic interpolation
    !     the additional points are needed to construct derivatives (centered in space).
    !     if boundary points are not available one sided finite differences are used.
    !
    !     (-1, 2).... (0, 2).....(1, 2).....(2, 2)
    !        .          .          .          .
    !        .          .          .          .
    !        .          .          .          .
    !        .          .          .          .
    !        .          .          .          .
    !     (-1, 1).....(0, 1)-----(1, 1).....(2, 1)
    !        .          |          |          .
    !        .          |  pr      |          .
    !        .          |----x     |          .
    !        .          |    |qr   |          .
    !        .          |    |     |          .
    !     (-1, 0).....(0, 0)-----(1, 0).....(2, 0)
    !        .          .          .          .
    !        .          .          .          .
    !        .          .          .          .
    !        .          .          .          .
    !        .          .          .          .
    !     (-1,-1).....(0,-1).....(1,-1).....(2,-1)
    !
    !      fp(i,j) = sum[n=0:nfd]{ cfd(n,l)*f(i+kfd(n,l),j) }
    !      fq(i,j) = sum[n=0:nfd]{ cfd(n,k)*f(i,j+kfd(n,k)) }
    !     fpq(i,j) = sum[n=0:nfd]{ sum[m=0:nfd]{
    !                    cfd(n,l)*cfd(m,k)*f(i+kfd(n,l),j+kfd(m,k)) } }
    !
    !     (i,j) = (0,0),(1,0),(1,1),(0,1)
    !     l or k = 0 : one-sided finite-difference (left)
    !     l or k = 1 : centered finite-difference
    !     l or k = 2 : one-sided finite-difference (right)
    !
    cs2d = zero
    do jj=0,1
      do ii=0,1
        p = i + ii
        q = j + jj
        if ( mod(iclo,2).eq.0 ) then
          k = nfd/2
        else
          if (p-lbx.lt.nfd/2) then
            k = p - lbx
          else if (ubx-p.lt.nfd/2) then
            k = nfd + p - ubx
          else
            k = nfd/2
          end if
        end if
        if ( mod(iclo,3).eq.0 ) then
          l = nfd/2
        else if ( iclo.eq.iclo_trpl ) then
          if (q-lby.lt.nfd/2) then
            l = q - lby
          else
            l = nfd/2
          end if
        else
          if (q-lby.lt.nfd/2) then
            l = q - lby
          else if (uby-q.lt.nfd/2) then
            l = nfd + q - uby
          else
            l = nfd/2
          end if
        end if
        cs2d(ii,jj) = cs2d(ii,jj) + a(ii,jj,0)
        do n=0,nfd
          cs2d(ii+kfd(n,k),jj) = cs2d(ii+kfd(n,k),jj) &
               + a(ii,jj,1)*cfd(n,k)
          cs2d(ii,jj+kfd(n,l)) = cs2d(ii,jj+kfd(n,l)) &
               + a(ii,jj,2)*cfd(n,l)
          do m=0,nfd
            cs2d(ii+kfd(n,k),jj+kfd(m,l)) = &
                 cs2d(ii+kfd(n,k),jj+kfd(m,l)) &
                 + a(ii,jj,3)*cfd(n,k)*cfd(m,l)
          end do
        end do
      end do
    end do
    !
    !---- set number of interpolation points and allocate arrays
    ns = count( abs(cs2d) .gt. small )
    allocate( ls(ns), is(ns), js(ns), cs(ns), stat=istat )
    if ( istat .ne. 0 ) then
      write(0,'(/1a,1a/)') 'getbcc error -- ', &
           'array allocation failed'
      call extcde (istat)
    end if
    ls(:) = .true.
    cs(:) = zero
    !
    !---- load arrays and apply index closure
    ns = 0
    do jj=-nfd/2,nfd
      do ii=-nfd/2,nfd
        if ( abs(cs2d(ii,jj)) .gt. small ) then
          ns = ns + 1
          is(ns) = i + ii
          js(ns) = j + jj
          cs(ns) = cs2d(ii,jj)
          if ( mod(iclo,2).eq.0 ) &
               is(ns) = lbx + mod(nx - 1 + mod(is(ns) - lbx + 1, nx), nx)
          if ( mod(iclo,3).eq.0 ) &
               js(ns) = lby + mod(ny - 1 + mod(js(ns) - lby + 1, ny), ny)
          if ( iclo.eq.iclo_trpl .and. js(ns).gt.uby ) then
            is(ns) = ubx + lbx - is(ns)
            js(ns) = 2*uby - js(ns) + 1
          end if
        end if
      end do
    end do
  end subroutine getbcc
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine getgfc( gsu, i, j, pr, qr, width, lcmp, ns, ls, is, js, cs )
    !     *** internal subroutine ***
    !     compute gaussian filter remap factors for a given point (p,q)
    !     (i,j) = lower-left corner point of grid cell containing target point
    !     (pr,qr) = cell-relative coordinate of target point
    !     double precision interface
    type(t_gsu), intent(in) :: gsu
    integer, intent(in)  :: i, j
    real(8), intent(in)  :: pr, qr
    real(8), intent(in)  :: width
    logical, intent(in)  :: lcmp
    integer, intent(out) :: ns
    logical, pointer, intent(inout) :: ls(:)
    integer, pointer, intent(inout) :: is(:), js(:)
    real(8), pointer, intent(inout) :: cs(:)
    !     local parameters
    !     note, width (=nsig*sigma) is set to max(width,width_min)
    !     so that the filter includes at least one source point.
    real(8), parameter :: nsig = 6.0d0
    real(8), parameter :: width_min = 1.5d0
    logical :: ijg, llg, lclo
    integer :: iclo, gkind
    integer :: lbx, lby, ubx, uby, nx, ny
    integer :: istat, k
    integer :: ii, jj, imin, jmin, imax, jmax
    real(8) :: wdth, sig2, rmax, r2mx, sfac, r2, gij, gsum
    !
    !---- initialize
    if ( .not.associated(gsu%ptr) ) then
      write(0,'(/2a/)') 'getblc error -- ', &
           'grid search utility object not created'
      call extcde (1)
    end if
    ijg = gsu%ptr%ijg
    llg = gsu%ptr%llg
    iclo = gsu%ptr%iclo
    lclo = gsu%ptr%lclo
    gkind = gsu%ptr%gkind
    lbx = gsu%ptr%lbx;  lby = gsu%ptr%lby;
    ubx = gsu%ptr%ubx;  uby = gsu%ptr%uby;
    nx = gsu%ptr%nx;  ny = gsu%ptr%ny;
    wdth = max(width,width_min)
    sig2 = (wdth/nsig)**2
    sfac = -0.5d0/sig2
    rmax = 0.5d0*wdth
    r2mx = rmax**2
    imin = int(min(zero,pr)-rmax)
    jmin = int(min(zero,qr)-rmax)
    imax = ceiling(max(zero,pr)+rmax)
    jmax = ceiling(max(zero,qr)+rmax)
    !
    !---- check & deallocate
    if ( associated(ls) ) then
      deallocate(ls);  nullify(ls);
    end if
    if ( associated(is) ) then
      deallocate(is);  nullify(is);
    end if
    if ( associated(js) ) then
      deallocate(js);  nullify(js);
    end if
    if ( associated(cs) ) then
      deallocate(cs);  nullify(cs);
    end if
    !
    !---- set number of interpolation points and allocate arrays
    ns = (imax-imin+1)*(jmax-jmin+1)
    allocate( ls(ns), is(ns), js(ns), cs(ns), stat=istat )
    if ( istat .ne. 0 ) then
      write(0,'(/1a,1a/)') 'getgfc error -- ', &
           'array allocation failed'
      call extcde (istat)
    end if
    ls(:) = .false.
    cs(:) = zero
    !
    !---- calculate filter coefficients
    gsum = zero
    do jj=jmin,jmax
      do ii=imin,imax
        k = (imax-imin+1)*(jj-jmin) + ii - imin + 1
        !-------- source points for the filter
        is(k) = i + ii
        js(k) = j + jj
        !-------- apply index closure
        if ( mod(iclo,2).eq.0 ) &
             is(k) = lbx + mod(nx - 1 + mod(is(k) - lbx + 1, nx), nx)
        if ( mod(iclo,3).eq.0 ) &
             js(k) = lby + mod(ny - 1 + mod(js(k) - lby + 1, ny), ny)
        if ( iclo.eq.iclo_trpl .and. js(k).gt.uby ) then
          is(k) = ubx + lbx - is(k)
          js(k) = 2*uby - js(k) + 1
        end if
        !-------- skip if source point is outside domain
        if ( is(k).lt.lbx .or. is(k).gt.ubx ) cycle
        if ( js(k).lt.lby .or. js(k).gt.uby ) cycle
        !-------- compute distance
        r2 = (pr - ii)**2 + (qr - jj)**2
        !         if ( r2.gt.r2mx ) cycle
        !-------- compute coefficient
        ls(k) = .true.
        if ( lcmp ) then
          gij = exp( sfac*r2 )
          gsum = gsum + gij
          cs(k) = gij
        end if
      end do
    end do
    if ( lcmp ) then
      where ( ls ) cs = cs/gsum
    end if
  end subroutine getgfc
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine dxydp( n, k, c, ijg, llg, iclo, &
       ptiled, qtiled, prange, qrange, &
       lb, ub, p, q, dxdp, dydp, mask, &
       x4, y4, x8, y8, rc )
    !     *** internal subroutine ***
    integer, intent(in)   :: n
    integer, intent(in)   :: k(0:n,0:n,1:n)
    real(8), intent(in)   :: c(0:n,0:n,1:n)
    logical, intent(in)   :: ijg
    logical, intent(in)   :: llg
    integer, intent(in)   :: iclo
    logical, intent(in)   :: ptiled, qtiled
    integer, intent(in)   :: prange(2), qrange(2)
    integer, intent(in)   :: lb(2), ub(2)
    integer, intent(in)   :: p, q
    real(8), intent(out)  :: dxdp, dydp
    logical, intent(in),  optional :: mask(lb(1):ub(1),lb(2):ub(2))
    real(4), intent(in),  optional :: x4(lb(1):ub(1),lb(2):ub(2))
    real(4), intent(in),  optional :: y4(lb(1):ub(1),lb(2):ub(2))
    real(8), intent(in),  optional :: x8(lb(1):ub(1),lb(2):ub(2))
    real(8), intent(in),  optional :: y8(lb(1):ub(1),lb(2):ub(2))
    integer, intent(out), optional :: rc
    !     local parameters
    integer, parameter :: m = 1 ! order of derivative
    logical, parameter :: debug = .false.
    character(64) :: fstr
    logical :: comp_m, type_r4, type_r8
    integer :: ihem
    integer :: np, nq, lbp, lbq, ubp, ubq, p0, q0
    integer :: istat=0, i, l, ii, ni, ii0, iin
    integer :: kp(0:n), kq(0:n)
    logical :: mp(0:n)
    real(8) :: xp(0:n)
    real(8) :: yp(0:n)
    real(8) :: up(0:n)
    real(8) :: vp(0:n)
    real(8) :: x0, y0, lon0, lat0, c0
    real(8) :: d1dp, d2dp
    !
    ! -------------------------------------------------------------------- /
    ! 1.  check and setup inputs
    !
    if ( present(rc) ) rc = 0
    type_r4 = present(x4).and.present(y4)
    type_r8 = present(x8).and.present(y8)
    if ( .not.type_r4.and..not.type_r8 ) then
      write(0,'(/1a,1a/)') 'dxydp error -- ', &
           'no input grid coordinates specified'
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    np = prange(2) - prange(1) + 1
    nq = qrange(2) - qrange(1) + 1
    if ( ijg ) then
      lbp = lb(1); lbq = lb(2)
      ubp = ub(1); ubq = ub(2)
    else
      lbp = lb(2); lbq = lb(1)
      ubp = ub(2); ubq = ub(1)
    end if
    if ( p.lt.lbp .or. p.gt.ubp .or. q.lt.lbq .or. q.gt.ubq ) then
      write(0,'(/1a,/1a,1l2,5i6,/1a,1l2,5i6/)') 'dxydp error -- '// &
           'input index coordinates outside input array bounds', &
           'dxydp error -- ptiled,prange,p,lbp,ubp:',ptiled,prange,p,lbp,ubp, &
           'dxydp error -- qtiled,qrange,q,lbq,ubq:',qtiled,qrange,q,lbq,ubq
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    p0 = p
    q0 = q
    if ( mod(iclo,2).eq.0 ) &
         p0 = prange(1) + mod(np - 1 + mod(p0 - prange(1) + 1, np), np)
    if ( mod(iclo,3).eq.0 ) &
         q0 = qrange(1) + mod(nq - 1 + mod(q0 - qrange(1) + 1, nq), nq)
    if ( iclo.eq.iclo_trpl .and. q0.gt.qrange(2) ) then
      p0 = prange(2) + prange(1) - p0
      q0 = 2*qrange(2) - q0 + 1
    end if
    if ( p0.lt.prange(1) .or. p0.gt.prange(2) .or. &
         q0.lt.qrange(1) .or. q0.gt.qrange(2) ) then
      write(0,'(/1a,/1a,4i6,/1a,4i6/)') 'dxydp error -- '// &
           'shifted input index coordinates outside allowed range', &
           'dxydp error -- prange,p,p0:',prange,p,p0, &
           'dxydp error -- qrange,q,q0:',qrange,q,q0
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    dxdp = zero
    dydp = zero
    comp_m = present(mask)
    if ( comp_m ) then
      if ( ijg ) then
        if ( mask(p0,q0) ) return
      else
        if ( mask(q0,p0) ) return
      end if
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  compute dx/dp & dy/dp
    !
    if ( mod(iclo,2).eq.0 ) then
      i = n/2
    else
      if (p0-prange(1).lt.n/2) then
        i = p0 - prange(1)
      else if (prange(2)-p0.lt.n/2) then
        i = n + p0 - prange(2)
      else
        i = n/2
      end if
    end if
    kp(:) = p + k(:,i,n)
    kq(:) = q
    if ( .not.ptiled ) then
      if ( mod(iclo,2).eq.0 ) then
        kp = prange(1) + mod(np - 1 + mod(kp - prange(1) + 1, np), np)
      end if
    end if
    if ( minval(kp).lt.lbp .or. maxval(kp).gt.ubp .or. &
         minval(kq).lt.lbq .or. maxval(kq).gt.ubq ) then
      write(0,'(/1a,/1a,1l2,8i6,/1a,1l2,8i6/)') 'dxydp error -- '// &
           'stencil index coordinates outside array bounds', &
           'dxydp error -- ptiled,prange,p,p0,lbp,ubp,pmin,pmax:', &
           ptiled,prange,p,p0,lbp,ubp,minval(kp),maxval(kp), &
           'dxydp error -- qtiled,qrange,q,q0,lbq,ubq,qmin,qmax:', &
           qtiled,qrange,q,q0,lbq,ubq,minval(kq),maxval(kq)
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    do l = 0, n
      if ( ijg ) then
        if ( comp_m ) mp(l) = mask(kp(l),kq(l))
        if ( type_r4 ) then
          xp(l) = x4(kp(l),kq(l))
          yp(l) = y4(kp(l),kq(l))
        else
          xp(l) = x8(kp(l),kq(l))
          yp(l) = y8(kp(l),kq(l))
        end if
      else
        if ( comp_m ) mp(l) = mask(kq(l),kp(l))
        if ( type_r4 ) then
          xp(l) = x4(kq(l),kp(l))
          yp(l) = y4(kq(l),kp(l))
        else
          xp(l) = x8(kq(l),kp(l))
          yp(l) = y8(kq(l),kp(l))
        end if
      end if
    end do
    ii = i
    ni = n
    ii0 = 0
    iin = n
    if ( comp_m ) then
      do l = i-1, 0, -1
        if ( mp(l) ) then
          mp(0:l) = .true.
          exit
        end if
      end do
      do l = i+1, n
        if ( mp(l) ) then
          mp(l:n) = .true.
          exit
        end if
      end do
      ii = count(.not.mp(0:i)) - 1
      ni = count(.not.mp(0:n)) - 1
      ii0 = i - ii
      iin = ii0 + ni
    end if
    if ( ni.le.0 ) then
      write(0,'(/1a,1a,4i6/)') 'dxydp error -- ', &
           'single point wide channel not allowed',p,q,p0,q0
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    if ( ni.gt.0 ) then
      if ( llg ) then
        if ( ijg ) then
          if ( type_r4 ) then
            x0 = x4(p,q); y0 = y4(p,q);
          else
            x0 = x8(p,q); y0 = y8(p,q);
          end if
        else
          if ( type_r4 ) then
            x0 = x4(q,p); y0 = y4(q,p);
          else
            x0 = x8(q,p); y0 = y8(q,p);
          end if
        end if
        ihem = 1; if (maxval(yp(ii0:iin)).lt.zero) ihem = -1;
        lon0 = zero; lat0 = sign(d90,real(ihem,8));
        c0 = d90 - abs(y0)
        call w3splx(lon0,lat0,c0,xp(ii0:iin),yp(ii0:iin), &
             up(ii0:iin),vp(ii0:iin))
        d1dp = dot_product(c(0:ni,ii,ni),up(ii0:iin))
        d2dp = dot_product(c(0:ni,ii,ni),vp(ii0:iin))
        call spddp(lon0,c0,ihem,x0,y0,d1dp,d2dp,dxdp,dydp)
      else !.not.llg
        dxdp = dot_product(c(0:ni,ii,ni),xp(ii0:iin))
        dydp = dot_product(c(0:ni,ii,ni),yp(ii0:iin))
      end if !.not.llg
      if ( debug ) then
        write(fstr,'(a,i0,a,i0,a)') &
             '(/1a,12i8,5(/1a,2e16.8),/1a,', &
             ni+1,'i16,3(/1a,',ni+1,'e16.8))'
        write(*,trim(fstr)) &
             'dxydp -- prange,qrange,p,q,p0,q0,ni,ii,ii0,iin:',&
             prange,qrange,p,q,p0,q0,ni,ii,ii0,iin,  &
             'dxydp --   x0,  y0:',x0,y0, &
             'dxydp -- lon0,lat0:',lon0,lat0, &
             'dxydp --   c0,ihem:',c0,real(ihem), &
             'dxydp -- d1dp,d2dp:',d1dp,d2dp, &
             'dxydp -- dxdp,dydp:',dxdp,dydp, &
             'dxydp --  k:', k(0:ni,ii,ni), &
             'dxydp --  c:', c(0:ni,ii,ni), &
             'dxydp -- xp:',xp(ii0:iin), &
             'dxydp -- yp:',yp(ii0:iin)
      end if
    else
      dxdp = zero
      dydp = zero
    end if
  end subroutine dxydp
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine dxydq( n, k, c, ijg, llg, iclo, &
       ptiled, qtiled, prange, qrange, &
       lb, ub, p, q, dxdq, dydq, mask, &
       x4, y4, x8, y8, rc )
    !     *** internal subroutine ***
    integer, intent(in)   :: n
    integer, intent(in)   :: k(0:n,0:n,1:n)
    real(8), intent(in)   :: c(0:n,0:n,1:n)
    logical, intent(in)   :: ijg
    logical, intent(in)   :: llg
    integer, intent(in)   :: iclo
    logical, intent(in)   :: ptiled, qtiled
    integer, intent(in)   :: prange(2), qrange(2)
    integer, intent(in)   :: lb(2), ub(2)
    integer, intent(in)   :: p, q
    real(8), intent(out)  :: dxdq, dydq
    logical, intent(in),  optional :: mask(lb(1):ub(1),lb(2):ub(2))
    real(4), intent(in),  optional :: x4(lb(1):ub(1),lb(2):ub(2))
    real(4), intent(in),  optional :: y4(lb(1):ub(1),lb(2):ub(2))
    real(8), intent(in),  optional :: x8(lb(1):ub(1),lb(2):ub(2))
    real(8), intent(in),  optional :: y8(lb(1):ub(1),lb(2):ub(2))
    integer, intent(out), optional :: rc
    !     local parameters
    integer, parameter :: m = 1 ! order of derivative
    logical, parameter :: debug = .false.
    character(64) :: fstr
    logical :: comp_m, type_r4, type_r8
    integer :: ihem
    integer :: np, nq, lbp, lbq, ubp, ubq, p0, q0
    integer :: istat=0, j, l, jj, nj, jj0, jjn
    integer :: kp(0:n), kq(0:n)
    logical :: mq(0:n)
    real(8) :: xq(0:n)
    real(8) :: yq(0:n)
    real(8) :: uq(0:n)
    real(8) :: vq(0:n)
    real(8) :: x0, y0, lon0, lat0, c0
    real(8) :: d1dq, d2dq
    !
    ! -------------------------------------------------------------------- /
    ! 1.  check and setup inputs
    !
    if ( present(rc) ) rc = 0
    type_r4 = present(x4).and.present(y4)
    type_r8 = present(x8).and.present(y8)
    if ( .not.type_r4.and..not.type_r8 ) then
      write(0,'(/1a,1a/)') 'dxydq error -- ', &
           'no input grid coordinates specified'
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    np = prange(2) - prange(1) + 1
    nq = qrange(2) - qrange(1) + 1
    if ( ijg ) then
      lbp = lb(1); lbq = lb(2)
      ubp = ub(1); ubq = ub(2)
    else
      lbp = lb(2); lbq = lb(1)
      ubp = ub(2); ubq = ub(1)
    end if
    if ( p.lt.lbp .or. p.gt.ubp .or. q.lt.lbq .or. q.gt.ubq ) then
      write(0,'(/1a,/1a,1l2,5i6,/1a,1l2,5i6/)') 'dxydq error -- '// &
           'input index coordinates outside input array bounds', &
           'dxydq error -- ptiled,prange,p,lbp,ubp:',ptiled,prange,p,lbp,ubp, &
           'dxydq error -- qtiled,qrange,q,lbq,ubq:',qtiled,qrange,q,lbq,ubq
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    p0 = p
    q0 = q
    if ( mod(iclo,2).eq.0 ) &
         p0 = prange(1) + mod(np - 1 + mod(p0 - prange(1) + 1, np), np)
    if ( mod(iclo,3).eq.0 ) &
         q0 = qrange(1) + mod(nq - 1 + mod(q0 - qrange(1) + 1, nq), nq)
    if ( iclo.eq.iclo_trpl .and. q0.gt.qrange(2) ) then
      p0 = prange(2) + prange(1) - p0
      q0 = 2*qrange(2) - q0 + 1
    end if
    if ( p0.lt.prange(1) .or. p0.gt.prange(2) .or. &
         q0.lt.qrange(1) .or. q0.gt.qrange(2) ) then
      write(0,'(/1a,/1a,4i6,/1a,4i6/)') 'dxydq error -- '// &
           'shifted input index coordinates outside allowed range', &
           'dxydq error -- prange,p,p0:',prange,p,p0, &
           'dxydq error -- qrange,q,q0:',qrange,q,q0
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    dxdq = zero
    dydq = zero
    comp_m = present(mask)
    if ( comp_m ) then
      if ( ijg ) then
        if ( mask(p0,q0) ) return
      else
        if ( mask(q0,p0) ) return
      end if
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  compute dx/dq & dy/dq
    !
    if ( mod(iclo,3).eq.0 ) then
      j = n/2
    else if ( iclo.eq.iclo_trpl ) then
      if (q0-qrange(1).lt.n/2) then
        j = q0 - qrange(1)
      else
        j = n/2
      end if
    else
      if (q0-qrange(1).lt.n/2) then
        j = q0 - qrange(1)
      else if (qrange(2)-q0.lt.n/2) then
        j = n + q0 - qrange(2)
      else
        j = n/2
      end if
    end if
    kp(:) = p
    kq(:) = q + k(:,j,n)
    if ( .not.qtiled ) then
      if ( mod(iclo,3).eq.0 ) then
        kq = qrange(1) + mod(nq - 1 + mod(kq - qrange(1) + 1, nq), nq)
      end if
      if ( iclo.eq.iclo_trpl .and. .not.ptiled ) then
        where ( kq.gt.qrange(2) )
          kp = prange(2) + prange(1) - kp
          kq = 2*qrange(2) - kq + 1
        end where
      end if
    end if
    if ( minval(kp).lt.lbp .or. maxval(kp).gt.ubp .or. &
         minval(kq).lt.lbq .or. maxval(kq).gt.ubq ) then
      write(0,'(/1a,/1a,1l2,8i6,/1a,1l2,8i6/)') 'dxydq error -- '// &
           'stencil index coordinates outside array bounds', &
           'dxydq error -- ptiled,prange,p,p0,lbp,ubp,pmin,pmax:', &
           ptiled,prange,p,p0,lbp,ubp,minval(kp),maxval(kp), &
           'dxydq error -- qtiled,qrange,q,q0,lbq,ubq,qmin,qmax:', &
           qtiled,qrange,q,q0,lbq,ubq,minval(kq),maxval(kq)
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    do l = 0, n
      if ( ijg ) then
        if ( comp_m ) mq(l) = mask(kp(l),kq(l))
        if ( type_r4 ) then
          xq(l) = x4(kp(l),kq(l))
          yq(l) = y4(kp(l),kq(l))
        else
          xq(l) = x8(kp(l),kq(l))
          yq(l) = y8(kp(l),kq(l))
        end if
      else
        if ( comp_m ) mq(l) = mask(kq(l),kp(l))
        if ( type_r4 ) then
          xq(l) = x4(kq(l),kp(l))
          yq(l) = y4(kq(l),kp(l))
        else
          xq(l) = x8(kq(l),kp(l))
          yq(l) = y8(kq(l),kp(l))
        end if
      end if
    end do
    jj = j
    nj = n
    jj0 = 0
    jjn = n
    if ( comp_m ) then
      do l = j-1, 0, -1
        if ( mq(l) ) then
          mq(0:l) = .true.
          exit
        end if
      end do
      do l = j+1, n
        if ( mq(l) ) then
          mq(l:n) = .true.
          exit
        end if
      end do
      jj = count(.not.mq(0:j)) - 1
      nj = count(.not.mq(0:n)) - 1
      jj0 = j - jj
      jjn = jj0 + nj
    end if
    if ( nj.le.0 ) then
      write(0,'(/1a,1a,4i6/)') 'dxydq error -- ', &
           'single point wide channel not allowed',p,q,p0,q0
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    if ( nj.gt.0 ) then
      if ( llg ) then
        if ( ijg ) then
          if ( type_r4 ) then
            x0 = x4(p,q); y0 = y4(p,q);
          else
            x0 = x8(p,q); y0 = y8(p,q);
          end if
        else
          if ( type_r4 ) then
            x0 = x4(q,p); y0 = y4(q,p);
          else
            x0 = x8(q,p); y0 = y8(q,p);
          end if
        end if
        ihem = 1; if (maxval(yq(jj0:jjn)).lt.zero) ihem = -1;
        lon0 = zero; lat0 = sign(d90,real(ihem,8));
        c0 = d90 - abs(y0)
        call w3splx(lon0,lat0,c0,xq(jj0:jjn),yq(jj0:jjn), &
             uq(jj0:jjn),vq(jj0:jjn))
        d1dq = dot_product(c(0:nj,jj,nj),uq(jj0:jjn))
        d2dq = dot_product(c(0:nj,jj,nj),vq(jj0:jjn))
        call spddq(lon0,c0,ihem,x0,y0,d1dq,d2dq,dxdq,dydq)
      else !.not.llg
        dxdq = dot_product(c(0:nj,jj,nj),xq(jj0:jjn))
        dydq = dot_product(c(0:nj,jj,nj),yq(jj0:jjn))
      end if !.not.llg
      if ( debug ) then
        write(fstr,'(a,i0,a,i0,a)') &
             '(/1a,12i8,5(/1a,2e16.8),/1a,', &
             nj+1,'i16,3(/1a,',nj+1,'e16.8))'
        write(*,trim(fstr)) &
             'dxydq -- prange,qrange,p,q,p0,q0,nj,jj,jj0,jjn:',&
             prange,qrange,p,q,p0,q0,nj,jj,jj0,jjn,  &
             'dxydq --   x0,  y0:',x0,y0, &
             'dxydq -- lon0,lat0:',lon0,lat0, &
             'dxydq --   c0,ihem:',c0,real(ihem), &
             'dxydq -- d1dq,d1dq:',d1dq,d1dq, &
             'dxydq -- dxdq,dydq:',dxdq,dydq, &
             'dxydq --  k:', k(0:nj,jj,nj), &
             'dxydq --  c:', c(0:nj,jj,nj), &
             'dxydq -- xq:',xq(jj0:jjn), &
             'dxydq -- yq:',yq(jj0:jjn)
      end if
    else
      dxdq = zero
      dydq = zero
    end if
  end subroutine dxydq
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine spddp( lam0, c0, ihem, lam, phi, dxdp, dydp, &
       dlamdp, dphidp )
    !     *** internal subroutine ***
    !     routine to compute polar stereographic transformation of
    !     grid derivatives dx/dp & dy/dp to dlam/dp & dphi/dp.
    !
    !     mu = lam - lam0
    !     nu = pi/4 - alpha*phi/2
    !     k0 = cos(c0/2)**2
    !
    !     dlam/dx = (     1/(2*r*k0)) * cot(nu)   * cos(mu)
    !     dlam/dy = ( alpha/(2*r*k0)) * cot(nu)   * sin(mu)
    !     dphi/dx = (-alpha/(  r*k0)) * cos(nu)^2 * sin(mu)
    !     dphi/dy = (     1/(  r*k0)) * cos(nu)^2 * cos(mu)
    !
    !     dlam/dp = dx/dp*dlam/dx + dy/dp*dlam/dy
    !     dphi/dp = dx/dp*dphi/dx + dy/dp*dphi/dy
    !     dlam/dq = dx/dq*dlam/dx + dy/dq*dlam/dy
    !     dphi/dq = dx/dq*dphi/dx + dy/dq*dphi/dy
    !
    real(8),intent(in) :: lam0, c0
    integer,intent(in) :: ihem
    real(8),intent(in) :: lam, phi
    real(8),intent(in) :: dxdp, dydp
    real(8),intent(out):: dlamdp, dphidp
    !     local parameters
    real(8), parameter :: small = 1d-6
    real(8) :: k0, a, mu, nu, fac
    real(8) :: cosmu, sinmu, cosnu2, cotnu
    real(8) :: dlamdx, dlamdy, dphidx, dphidy
    k0  = cos(half*c0*d2r)**2
    mu  = (lam-lam0)*d2r
    a   = sign(one,real(ihem,8))
    nu  = pio4 - a*half*phi*d2r
    nu  = sign(max(small,abs(nu)),nu)
    fac = r2d*half/rearth/k0
    cosmu  = cos(mu)
    sinmu  = sin(mu)
    cosnu2 = cos(nu)**2
    cotnu  = one/tan(nu)
    dlamdx =        fac*cotnu*cosmu
    dlamdy =      a*fac*cotnu*sinmu
    dphidx = -a*two*fac*cosnu2*sinmu
    dphidy =    two*fac*cosnu2*cosmu
    dlamdp = dxdp*dlamdx + dydp*dlamdy
    dphidp = dxdp*dphidx + dydp*dphidy
  end subroutine spddp
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine spddq( lam0, c0, ihem, lam, phi, dxdq, dydq, &
       dlamdq, dphidq )
    !     *** internal subroutine ***
    !     routine to compute polar stereographic transformation of
    !     grid derivatives dx/dq & dy/dq to dlam/dq & dphi/dq.
    !
    !     mu = lam - lam0
    !     nu = pi/4 - alpha*phi/2
    !     k0 = cos(c0/2)**2
    !
    !     dlam/dx = (     1/(2*r*k0)) * cot(nu)   * cos(mu)
    !     dlam/dy = ( alpha/(2*r*k0)) * cot(nu)   * sin(mu)
    !     dphi/dx = (-alpha/(  r*k0)) * cos(nu)^2 * sin(mu)
    !     dphi/dy = (     1/(  r*k0)) * cos(nu)^2 * cos(mu)
    !
    !     dlam/dp = dx/dp*dlam/dx + dy/dp*dlam/dy
    !     dphi/dp = dx/dp*dphi/dx + dy/dp*dphi/dy
    !     dlam/dq = dx/dq*dlam/dx + dy/dq*dlam/dy
    !     dphi/dq = dx/dq*dphi/dx + dy/dq*dphi/dy
    !
    real(8),intent(in) :: lam0, c0
    integer,intent(in) :: ihem
    real(8),intent(in) :: lam, phi
    real(8),intent(in) :: dxdq, dydq
    real(8),intent(out):: dlamdq, dphidq
    !     local parameters
    real(8), parameter :: small = 1d-6
    real(8) :: k0, a, mu, nu, fac
    real(8) :: cosmu, sinmu, cosnu2, cotnu
    real(8) :: dlamdx, dlamdy, dphidx, dphidy
    k0  = cos(half*c0*d2r)**2
    mu  = (lam-lam0)*d2r
    a   = sign(one,real(ihem,8))
    nu  = pio4 - a*half*phi*d2r
    nu  = sign(max(small,abs(nu)),nu)
    fac = r2d*half/rearth/k0
    cosmu  = cos(mu)
    sinmu  = sin(mu)
    cosnu2 = cos(nu)**2
    cotnu  = one/tan(nu)
    dlamdx =        fac*cotnu*cosmu
    dlamdy =      a*fac*cotnu*sinmu
    dphidx = -a*two*fac*cosnu2*sinmu
    dphidy =    two*fac*cosnu2*cosmu
    dlamdq = dxdq*dlamdx + dydq*dlamdy
    dphidq = dxdq*dphidx + dydq*dphidy
  end subroutine spddq
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine dfdpq( n, k, c, ijg, iclo, &
       ptiled, qtiled, &
       prange, qrange, &
       lb, ub, p, q, &
       f4, f8, dfdp, dfdq, &
       g4, g8, dgdp, dgdq, &
       h4, h8, dhdp, dhdq, &
       nsdp, isdp, jsdp, csdp, &
       nsdq, isdq, jsdq, csdq, &
       mask, rc )
    !     *** internal subroutine ***
    integer, intent(in)   :: n
    integer, intent(in)   :: k(0:n,0:n,1:n)
    real(8), intent(in)   :: c(0:n,0:n,1:n)
    logical, intent(in)   :: ijg
    integer, intent(in)   :: iclo
    logical, intent(in)   :: ptiled, qtiled
    integer, intent(in)   :: prange(2), qrange(2)
    integer, intent(in)   :: lb(2), ub(2)
    integer, intent(in)   :: p, q
    real(4), intent(in),  optional :: f4(lb(1):ub(1),lb(2):ub(2))
    real(8), intent(in),  optional :: f8(lb(1):ub(1),lb(2):ub(2))
    real(8), intent(out), optional :: dfdp, dfdq
    real(4), intent(in),  optional :: g4(lb(1):ub(1),lb(2):ub(2))
    real(8), intent(in),  optional :: g8(lb(1):ub(1),lb(2):ub(2))
    real(8), intent(out), optional :: dgdp, dgdq
    real(4), intent(in),  optional :: h4(lb(1):ub(1),lb(2):ub(2))
    real(8), intent(in),  optional :: h8(lb(1):ub(1),lb(2):ub(2))
    real(8), intent(out), optional :: dhdp, dhdq
    integer, intent(out), optional :: nsdp
    integer, pointer,     optional :: isdp(:), jsdp(:)
    real(8), pointer,     optional :: csdp(:)
    integer, intent(out), optional :: nsdq
    integer, pointer,     optional :: isdq(:), jsdq(:)
    real(8), pointer,     optional :: csdq(:)
    logical, intent(in),  optional :: mask(lb(1):ub(1),lb(2):ub(2))
    integer, intent(out), optional :: rc
    !     local parameters
    integer, parameter :: m = 1 ! order of derivative
    logical, parameter :: debug = .false.
    character(64) :: fstr
    logical :: comp_m, comp_f, comp_g, comp_h, type_r4
    logical :: comp_cp, comp_cq
    integer :: np, nq, lbp, lbq, ubp, ubq, p0, q0
    integer :: istat=0, i, j, l, ii, jj, ni, nj, ii0, iin, jj0, jjn
    integer :: kp(0:n), kq(0:n)
    logical :: mp(0:n), mq(0:n)
    real(8) :: fp(0:n), fq(0:n)
    real(8) :: gp(0:n), gq(0:n)
    real(8) :: hp(0:n), hq(0:n)
    integer :: ip(0:n), iq(0:n)
    integer :: jp(0:n), jq(0:n)
    !
    ! -------------------------------------------------------------------- /
    ! 1.  check and setup inputs
    !
    if ( present(rc) ) rc = 0
    comp_f = ( present(f4) .or. present(f8) ) .and. &
         present(dfdp) .and. present(dfdq)
    comp_g = ( present(g4) .or. present(g8) ) .and. &
         present(dgdp) .and. present(dgdq)
    comp_h = ( present(h4) .or. present(h8) ) .and. &
         present(dhdp) .and. present(dhdq)
    comp_cp = present(nsdp) .and. present(isdp) .and. &
         present(jsdp) .and. present(csdp)
    comp_cq = present(nsdq) .and. present(isdq) .and. &
         present(jsdq) .and. present(csdq)
    if ( .not.comp_f.and..not.comp_g.and..not.comp_h.and. &
         .not.comp_cp.and..not.comp_cq ) return
    if ( comp_f ) then
      type_r4 = present(f4)
    else if ( comp_g ) then
      type_r4 = present(g4)
    else if ( comp_h ) then
      type_r4 = present(h4)
    end if
    np = prange(2) - prange(1) + 1
    nq = qrange(2) - qrange(1) + 1
    if ( ijg ) then
      lbp = lb(1); lbq = lb(2)
      ubp = ub(1); ubq = ub(2)
    else
      lbp = lb(2); lbq = lb(1)
      ubp = ub(2); ubq = ub(1)
    end if
    if ( p.lt.lbp .or. p.gt.ubp .or. q.lt.lbq .or. q.gt.ubq ) then
      write(0,'(/1a,/1a,1l2,5i6,/1a,1l2,5i6/)') 'dfdpq error -- '// &
           'input index coordinates outside input array bounds', &
           'dfdpq error -- ptiled,prange,p,lbp,ubp:',ptiled,prange,p,lbp,ubp, &
           'dfdpq error -- qtiled,qrange,q,lbq,ubq:',qtiled,qrange,q,lbq,ubq
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    p0 = p
    q0 = q
    if ( mod(iclo,2).eq.0 ) &
         p0 = prange(1) + mod(np - 1 + mod(p0 - prange(1) + 1, np), np)
    if ( mod(iclo,3).eq.0 ) &
         q0 = qrange(1) + mod(nq - 1 + mod(q0 - qrange(1) + 1, nq), nq)
    if ( iclo.eq.iclo_trpl .and. q0.gt.qrange(2) ) then
      p0 = prange(2) + prange(1) - p0
      q0 = 2*qrange(2) - q0 + 1
    end if
    if ( p0.lt.prange(1) .or. p0.gt.prange(2) .or. &
         q0.lt.qrange(1) .or. q0.gt.qrange(2) ) then
      write(0,'(/1a,/1a,4i6,/1a,4i6/)') 'dfdpq error -- '// &
           'shifted input index coordinates outside allowed range', &
           'dfdpq error -- prange,p,p0:',prange,p,p0, &
           'dfdpq error -- qrange,q,q0:',qrange,q,q0
      istat = 1
      if ( present(rc) ) then
        rc = istat
        return
      else
        call extcde (istat)
      end if
    end if
    comp_m = present(mask)
    if ( comp_m ) then
      if ( ijg ) then
        if ( mask(p0,q0) ) return
      else
        if ( mask(q0,p0) ) return
      end if
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  compute df/dp
    !
    if ( comp_f.or.comp_g.or.comp_h.or.comp_cp ) then
      if ( mod(iclo,2).eq.0 ) then
        i = n/2
      else
        if (p0-prange(1).lt.n/2) then
          i = p0 - prange(1)
        else if (prange(2)-p0.lt.n/2) then
          i = n + p0 - prange(2)
        else
          i = n/2
        end if
      end if
      kp(:) = p + k(:,i,n)
      kq(:) = q
      if ( .not.ptiled ) then
        if ( mod(iclo,2).eq.0 ) then
          kp = prange(1) + mod(np - 1 + mod(kp - prange(1) + 1, np), np)
        end if
      end if
      if ( minval(kp).lt.lbp .or. maxval(kp).gt.ubp .or. &
           minval(kq).lt.lbq .or. maxval(kq).gt.ubq ) then
        write(0,'(/1a,/1a,1l2,8i6,/1a,1l2,8i6/)') 'dfdpq error -- '// &
             'stencil index coordinates outside array bounds', &
             'dfdpq error -- ptiled,prange,p,p0,lbp,ubp,pmin,pmax:', &
             ptiled,prange,p,p0,lbp,ubp,minval(kp),maxval(kp), &
             'dfdpq error -- qtiled,qrange,q,q0,lbq,ubq,qmin,qmax:', &
             qtiled,qrange,q,q0,lbq,ubq,minval(kq),maxval(kq)
        istat = 1
        if ( present(rc) ) then
          rc = istat
          return
        else
          call extcde (istat)
        end if
      end if
      if ( comp_cp ) then
        ip(:) = p0 + k(:,i,n)
        jp(:) = q0
        if ( mod(iclo,2).eq.0 ) then
          ip = prange(1) + mod(np - 1 + mod(ip - prange(1) + 1, np), np)
        end if
      end if
      do l = 0, n
        if ( ijg ) then
          if ( comp_m ) mp(l) = mask(kp(l),kq(l))
          if ( type_r4 ) then
            if ( comp_f ) fp(l) = f4(kp(l),kq(l))
            if ( comp_g ) gp(l) = g4(kp(l),kq(l))
            if ( comp_h ) hp(l) = h4(kp(l),kq(l))
          else
            if ( comp_f ) fp(l) = f8(kp(l),kq(l))
            if ( comp_g ) gp(l) = g8(kp(l),kq(l))
            if ( comp_h ) hp(l) = h8(kp(l),kq(l))
          end if
        else
          if ( comp_m ) mp(l) = mask(kq(l),kp(l))
          if ( type_r4 ) then
            if ( comp_f ) fp(l) = f4(kq(l),kp(l))
            if ( comp_g ) gp(l) = g4(kq(l),kp(l))
            if ( comp_h ) hp(l) = h4(kq(l),kp(l))
          else
            if ( comp_f ) fp(l) = f8(kq(l),kp(l))
            if ( comp_g ) gp(l) = g8(kq(l),kp(l))
            if ( comp_h ) hp(l) = h8(kq(l),kp(l))
          end if
        end if
      end do
      ii = i
      ni = n
      ii0 = 0
      iin = n
      if ( comp_m ) then
        do l = i-1, 0, -1
          if ( mp(l) ) then
            mp(0:l) = .true.
            exit
          end if
        end do
        do l = i+1, n
          if ( mp(l) ) then
            mp(l:n) = .true.
            exit
          end if
        end do
        ii = count(.not.mp(0:i)) - 1
        ni = count(.not.mp(0:n)) - 1
        ii0 = i - ii
        iin = ii0 + ni
      end if
      if ( ni.gt.0 ) then
        if ( comp_f ) dfdp = dot_product(c(0:ni,ii,ni),fp(ii0:iin))
        if ( comp_g ) dgdp = dot_product(c(0:ni,ii,ni),gp(ii0:iin))
        if ( comp_h ) dhdp = dot_product(c(0:ni,ii,ni),hp(ii0:iin))
        if ( comp_cp ) then
          if ( associated(isdp) ) deallocate(isdp)
          if ( associated(jsdp) ) deallocate(jsdp)
          if ( associated(csdp) ) deallocate(csdp)
          nsdp = ni+1
          allocate(isdp(nsdp),jsdp(nsdp),csdp(nsdp))
          isdp(1:nsdp) = ip(ii0:iin)
          jsdp(1:nsdp) = jp(ii0:iin)
          csdp(1:nsdp) = c(0:ni,ii,ni)
        end if
        if ( debug .and. comp_f ) then
          write(fstr,'(a,i0,a,i0,a,i0,a)') '(/1a,8i6,e16.8,/1a,',&
               ni+1,'i16,/1a,',ni+1,'e16.8,/1a,',ni+1,'e16.8)'
          write(*,trim(fstr)) &
               'dfdpq -- dfdp -- p,q,p0,q0,ni,ii,ii0,iin,dfdp:',&
               p,q,p0,q0,ni,ii,ii0,iin,dfdp, &
               'dfdpq -- dfdp --  k:', k(0:ni,ii,ni), &
               'dfdpq -- dfdp --  c:', c(0:ni,ii,ni), &
               'dfdpq -- dfdp -- fp:', fp(ii0:iin)
        end if
      else
        if ( comp_f ) dfdp = zero
        if ( comp_g ) dgdp = zero
        if ( comp_h ) dhdp = zero
        if ( comp_cp ) nsdp = 0
      end if
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 3.  compute df/dq
    !
    if ( comp_f.or.comp_g.or.comp_h.or.comp_cq ) then
      if ( mod(iclo,3).eq.0 ) then
        j = n/2
      else if ( iclo.eq.iclo_trpl ) then
        if (q0-qrange(1).lt.n/2) then
          j = q0 - qrange(1)
        else
          j = n/2
        end if
      else
        if (q0-qrange(1).lt.n/2) then
          j = q0 - qrange(1)
        else if (qrange(2)-q0.lt.n/2) then
          j = n + q0 - qrange(2)
        else
          j = n/2
        end if
      end if
      kp(:) = p
      kq(:) = q + k(:,j,n)
      if ( .not.qtiled ) then
        if ( mod(iclo,3).eq.0 ) then
          kq = qrange(1) + mod(nq - 1 + mod(kq - qrange(1) + 1, nq), nq)
        end if
        if ( iclo.eq.iclo_trpl .and. .not.ptiled ) then
          where ( kq.gt.qrange(2) )
            kp = prange(2) + prange(1) - kp
            kq = 2*qrange(2) - kq + 1
          end where
        end if
      end if
      if ( minval(kp).lt.lbp .or. maxval(kp).gt.ubp .or. &
           minval(kq).lt.lbq .or. maxval(kq).gt.ubq ) then
        write(0,'(/1a,/1a,1l2,8i6,/1a,1l2,8i6/)') 'dfdpq error -- '// &
             'stencil index coordinates outside array bounds', &
             'dfdpq error -- ptiled,prange,p,p0,lbp,ubp,pmin,pmax:', &
             ptiled,prange,p,p0,lbp,ubp,minval(kp),maxval(kp), &
             'dfdpq error -- qtiled,qrange,q,q0,lbq,ubq,qmin,qmax:', &
             qtiled,qrange,q,q0,lbq,ubq,minval(kq),maxval(kq)
        istat = 1
        if ( present(rc) ) then
          rc = istat
          return
        else
          call extcde (istat)
        end if
      end if
      if ( comp_cq ) then
        iq(:) = p0
        jq(:) = q0 + k(:,j,n)
        if ( mod(iclo,3).eq.0 ) then
          jq = qrange(1) + mod(nq - 1 + mod(jq - qrange(1) + 1, nq), nq)
        end if
        if ( iclo.eq.iclo_trpl ) then
          where ( jq.gt.qrange(2) )
            iq = prange(2) + prange(1) - iq
            jq = 2*qrange(2) - jq + 1
          end where
        end if
      end if
      do l = 0, n
        if ( ijg ) then
          if ( comp_m ) mq(l) = mask(kp(l),kq(l))
          if ( type_r4 ) then
            if ( comp_f ) fq(l) = f4(kp(l),kq(l))
            if ( comp_g ) gq(l) = g4(kp(l),kq(l))
            if ( comp_h ) hq(l) = h4(kp(l),kq(l))
          else
            if ( comp_f ) fq(l) = f8(kp(l),kq(l))
            if ( comp_g ) gq(l) = g8(kp(l),kq(l))
            if ( comp_h ) hq(l) = h8(kp(l),kq(l))
          end if
        else
          if ( comp_m ) mq(l) = mask(kq(l),kp(l))
          if ( type_r4 ) then
            if ( comp_f ) fq(l) = f4(kq(l),kp(l))
            if ( comp_g ) gq(l) = g4(kq(l),kp(l))
            if ( comp_h ) hq(l) = h4(kq(l),kp(l))
          else
            if ( comp_f ) fq(l) = f8(kq(l),kp(l))
            if ( comp_g ) gq(l) = g8(kq(l),kp(l))
            if ( comp_h ) hq(l) = h8(kq(l),kp(l))
          end if
        end if
      end do
      jj = j
      nj = n
      jj0 = 0
      jjn = n
      if ( comp_m ) then
        do l = j-1, 0, -1
          if ( mq(l) ) then
            mq(0:l) = .true.
            exit
          end if
        end do
        do l = j+1, n
          if ( mq(l) ) then
            mq(l:n) = .true.
            exit
          end if
        end do
        jj = count(.not.mq(0:j)) - 1
        nj = count(.not.mq(0:n)) - 1
        jj0 = j - jj
        jjn = jj0 + nj
      end if
      if ( nj.gt.0 ) then
        if ( comp_f ) dfdq = dot_product(c(0:nj,jj,nj),fq(jj0:jjn))
        if ( comp_g ) dgdq = dot_product(c(0:nj,jj,nj),gq(jj0:jjn))
        if ( comp_h ) dhdq = dot_product(c(0:nj,jj,nj),hq(jj0:jjn))
        if ( comp_cq ) then
          if ( associated(isdq) ) deallocate(isdq)
          if ( associated(jsdq) ) deallocate(jsdq)
          if ( associated(csdq) ) deallocate(csdq)
          nsdq = nj+1
          allocate(isdq(nsdq),jsdq(nsdq),csdq(nsdq))
          isdq(1:nsdq) = iq(jj0:jjn)
          jsdq(1:nsdq) = jq(jj0:jjn)
          csdq(1:nsdq) = c(0:nj,jj,nj)
        end if
        if ( debug .and. comp_f ) then
          write(fstr,'(a,i0,a,i0,a,i0,a)') '(/1a,8i6,e16.8,/1a,',&
               nj+1,'i16,/1a,',nj+1,'e16.8,/1a,',nj+1,'e16.8)'
          write(*,trim(fstr)) &
               'dfdpq -- dfdq -- p,q,p0,q0,nj,jj,jj0,jjn,dfdq:',&
               p,q,p0,q0,nj,jj,jj0,jjn,dfdq, &
               'dfdpq -- dfdq --  k:', k(0:nj,jj,nj), &
               'dfdpq -- dfdq --  c:', c(0:nj,jj,nj), &
               'dfdpq -- dfdq -- fq:', fq(jj0:jjn)
        end if
      else
        if ( comp_f ) dfdq = zero
        if ( comp_g ) dgdq = zero
        if ( comp_h ) dhdq = zero
        if ( comp_cq ) nsdq = 0
      end if
    end if
  end subroutine dfdpq
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine get_fdw2( n, m, k, c )
    !     *** internal subroutine ***
    integer,intent(in) :: n, m
    integer,intent(out):: k(0:n,0:n)
    real(8),intent(out):: c(0:n,0:n)
    integer :: i, j
    real(8) :: a(0:n), b(0:n,0:m)
    do i = 0, n
      do j = 0, n
        k(j,i) = j-i
        a(j) = k(j,i)
      end do
      call w3fdwt( n, n, m, zero, a, b )
      c(0:n,i) = b(0:n,m)
      !write(0,'(a,i1,2x,11i16)') 'i=',i,k(0:n,i)
      !write(0,'(5x,11e16.8)') c(0:n,i)
    end do
  end subroutine get_fdw2
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine get_fdw3( n, m, k, c )
    !     *** internal subroutine ***
    integer,intent(in) :: n, m
    integer,intent(out):: k(0:n,0:n,1:n)
    real(8),intent(out):: c(0:n,0:n,1:n)
    integer :: l, i, j
    real(8) :: a(0:n), b(0:n,0:m)
    do l = 1, n
      !write(0,'(a,i1,2x,11a)') 'l=',l,('----------------',i=0,l)
      do i = 0, l
        do j = 0, l
          k(j,i,l) = j-i
          a(j) = k(j,i,l)
        end do
        call w3fdwt( l, n, m, zero, a, b )
        c(0:l,i,l) = b(0:l,m)
        !write(0,'(a,i1,2x,11i16)') 'i=',i,k(0:l,i,l)
        !write(0,'(5x,11e16.8)') c(0:l,i,l)
      end do
    end do
  end subroutine get_fdw3
  !/
  !/ ------------------------------------------------------------------- /
  !/
  !/
  !/ end internal support routines ===================================== /
  !/
  !/
  !/
  !/ end of module w3gsrumd ============================================ /
  !/
end module w3gsrumd
