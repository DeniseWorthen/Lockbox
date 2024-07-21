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
module w3gkemd
  !/ ------------------------------------------------------------------- /
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |          odin gramstad            |
  !/                  |          qingxiang liu            |
  !/                  |                                   |
  !/                  |                        fortran 90 |
  !/                  | last update :         03-jun-2021 |
  !/                  +-----------------------------------+
  !/
  !/    26-may-2014 : origination.                        ( version 3.14 )
  !/                  intially dr. o. gramstad implemented his gke method
  !/                  in ww3 v3.14 which worked well for the single-grid
  !/                  point duration-limited test.
  !/
  !/    09-nov-2018 : fully implemented in ww3            ( version 7.13 )
  !/                                                      ( q. liu       )
  !/    16-apr-2019 : add save attribute explicitly       ( version 7.13 )
  !/                                                      ( q. liu       )
  !/    18-apr-2019 : add the bilinear interp. option     ( version 7.13 )
  !/                                                      ( q. liu       )
  !/    08-jul-2019 : use kind=8 for qi_nnz               ( q. liu       )
  !/    01-apr-2020 : boundary conditions                 ( q. liu       )
  !/    03-jun-2021 : merge into the ww3 github           ( version 7.12 )
  !/                                                      ( q. liu       )
  !/
  !  1. purpose:
  !     calculate the (resonant & quasi/near-resonant) four-wave nonlinear
  !     interaction term s_{nl} according to the generalized kinetic
  !     equation (gke) developed in gramstad and stiassnie (2013).
  !
  !     references:
  !     gramstad and stiassnie (2013), jfm, 818, 280-303 (hereafter gs13)
  !     gramstad and babanin (2016),    od,  66, 509-526 (hereafter gb16)
  !     liu et al. (2021),             jfm, 910, a50     (hereafter lgb21)
  !     &
  !     annenkov and shrira (2006),    jfm, 561, 181-207 (*)
  !     annenkov and shrira (2015),    jpo,  45, 807-812
  !     annenkov and shrira (2018),    jfm, 844, 766-795 (*)
  !     &
  !     shrira and annenkov (2013),    book ch., 239-281
  !     annenkov and shrira (2016),    book ch., 159-178
  !
  !     (*) note that equations therein contain typos.
  !
  !  2. subroutines and functions :
  !
  !     [part 1]: kernel function
  !
  !     calculate the kernel function t_{0, 1, 2, 3} for the zakharov
  !     equation.
  !
  !     references:
  !     krasitskii (1994), jfm, 272, 1 - 20 (hereafter k94)
  !     janssen    (2009), jfm, 637, 1 - 44 (hereafter j09)
  !     mei et al. (2005), ch. 14, 882 - 884
  !
  !     based on my own observation, odin has closely followed the
  !     equations presented in the appendix (a.1/2) of j09.
  !
  !     ----------------------------------------------------------------
  !      name                type  scope    description
  !     ----------------------------------------------------------------
  !      qfunc               func. private  q = ω^2 / g
  !      vpfunc              func. private  v^{(+)}_{1, 2, 3}
  !      vmfunc              func. private  v^{(-)}_{1, 2, 3}
  !      ufunc               func. private  u_{1, 2, 3, 4}
  !      tfunc               func. public   t_{1, 2, 3, 4}
  !     ----------------------------------------------------------------
  !
  !     [part 2]: find quartets (total number & configurations)
  !
  !     references:
  !     annenkov and shrira (2015),       jpo, 45, 807-812
  !     hasselmann and hasselmann (1981), exact-nl/dia report
  !     hasselmann and hasselmann (1985), jpo, 15, 1369-1377
  !
  !     ----------------------------------------------------------------
  !      name                type  scope    description
  !     ----------------------------------------------------------------
  !      findquartetnumber   subr. private  total no. of quartets
  !      findquartetconfig   subr. private  config.   of quartets
  !
  !      [part 3]: sparse matrix (storage, operation)
  !
  !      references:
  !      saad (1994) sparskit: a basic tool kit for sparse matrix
  !                            compuation (version 2)
  !
  !     ----------------------------------------------------------------
  !      name                type  scope    description
  !     ----------------------------------------------------------------
  !      coocsrind           subr. private  coo to csr format
  !      asymsmattimvec      subr. private  (a±a^t)∙x, where (a±a^t) is an
  !                                         (anti)symmetric sparse matrix
  !
  !      [part 4]: gke integral (main subrs.)
  !
  !      references:
  !      gramstad and stiassnie (2013),    jfm, 818, 280-303 (hereafter gs13)
  !      gramstad and babanin (2016),       od,  66, 509-526 (hereafter gb16)
  !      liu et al. (2021),                jfm, 910, a50     (hereafter lgb21)
  !      janssen (2003),                   jpo,  33, 863-884 (hereafter j03)
  !      janssen (2009),                   jfm, 637,   1- 44 (hereafter j09)
  !      annenkov and shrira (2013),       jfm, 726, 517-546
  !
  !      hasselmann and hasselmann (1981), exact-nl/dia report
  !      hasselmann and hasselmann (1985), jpo, 15, 1369-1377
  !      van vledder (2006),               ce,  53, 223-242
  !      tolman (2013),                    om,  70,  11- 24
  !
  !     ----------------------------------------------------------------
  !      name                type  scope    description
  !     ----------------------------------------------------------------
  !      prepkgrid           subr. private  (σ, θ) to (kx, ky)
  !      prepkernelio        subr. private  read/write quartet cfg file
  !      biinterpwt          subr. private  calc. interp. weights
  !      calcqrsnl           subr. public   gke transfer integral
  !
  !  3. future work (todo)
  !   * the current version only works for a constant-depth application (
  !     either deep or finite deep). extension of this module to be
  !     applicable to varying-depth cases may be pursued in the future.
  !
  !   * dnl   -- diagonal term
  !   * βnpqr -- nonlinear stokes correction
  !/
  !/ ------------------------------------------------------------------- /
  !
  ! public parameters
  !
  ! * `qi_` denotes the variable is integer number
  ! * `qr_` ...                     real    ...
  ! * `qs_` ...                     string
  ! * `ql_` ...                     logical
  ! * `qc_` ...                     complex
  !/
  implicit none
  !/ ------------------------------------------------------------------- /
  public  :: prepkernelio, calcqrsnl
  !
  public  :: qr_depth, qr_oml, qi_disc, qi_kev, qi_nnz, qi_interp
  private :: qfunc, vpfunc, vmfunc, ufunc, tfunc,           &
       findquartetnumber, findquartetconfig,          &
       coocsrind, asymsmattimvec,                     &
       prepkgrid, biinterpwt
  !
  private :: qs_ver, qi_lrb, qr_eps, qr_grav,               &
       qr_pi, qr_tpi, qr_dmax, qc_iu, qs_cfg,         &
       qr_kx, qr_ky, qr_dk, qr_om, qi_nrsm,           &
       qi_nn, qi_pp, qi_qq, qi_rr,                    &
       qr_k4x, qr_k4y, qr_om4, qr_dom,                &
       qr_tkern, qr_tkurt,                            &
       qi_iccos, qi_ircsr, qr_sumqr, qr_sumnp,        &
       qi_bind, qr_bwgh, qr_wn1
  !
  private :: qi_bound, qr_fpow, qr_bdry
  !
  !/ ------------------------------------------------------------------- /
  real                       :: qr_depth                    ! real water depth d (m)
  real                       :: qr_oml                      ! λ cut off factor
  ! λ ≤ 0 →  quartets far
  ! from resonance will not
  ! be excluded.
  integer                    :: qi_disc                     ! discretization of gke
  ! 0: continuous (like exact-nl, wrt)
  ! 1: discrete   (see gs13)
  integer                    :: qi_kev                      ! version of ke
  ! 0: gke
  ! 1: ke from j03
  integer                    :: qi_interp                   ! interp. option
  ! 0: nearest bin
  ! 1: bilinear interp
  integer, parameter         :: qi_bound= 1                 ! boundary conditions
  ! 0: no bound
  ! 1: tail extension
  real, parameter            :: qr_fpow= -5.                ! e(f) tail power law
  !
  character(len=50), parameter                              &
       :: qs_ver  = 'gkev0'           ! version number/str
  integer, parameter         :: qi_lrb  = 4                 ! 4 bytes
  real, parameter            :: qr_eps  = epsilon(100.0)    ! smallest positive
  ! value supported by the
  ! compiler (e.g., gfortran
  ! → 1.19e-7)
  real, parameter            :: qr_grav = 9.806             ! gravational acc (m/s^2)
  real, parameter            :: qr_pi   = 3.141592653589793 ! π
  real, parameter            :: qr_tpi  = 2 * qr_pi         ! π * 2
  real, parameter            :: qr_dmax = 3000.0            ! maximum allowed water
  complex, parameter         :: qc_iu   = (0.0, 1.0)        ! complex unit `i`
  !
  character(len=100)         :: qs_cfg                      ! file name for quartet/kernel
  !
  real, allocatable, save    :: qr_kx(:), qr_ky(:),   &     ! kx, ky (2d grid → 1d vector)
       qr_dk(:), qr_om(:)          ! δ\vec{k}, ω,
  !
  integer(kind=8)            :: qi_nnz                      ! # of quartets
  integer                    :: qi_nrsm                     ! # of rows of smat
  integer, allocatable, save :: qi_nn(:), qi_pp(:),   &     ! index for quartets
       qi_qq(:), qi_rr(:)
  real, allocatable, save    :: qr_k4x(:), qr_k4y(:), &     ! kx, ky, ω for 4th wave
       qr_om4(:)
  real, allocatable, save    :: qr_dom(:),            &     ! δω
       qr_tkern(:),          &     ! kernel `t`
       qr_tkurt(:)                 ! kurtosis `t`
  integer, allocatable, save :: qi_iccos(:),          &     ! col index of coocsr
       qi_ircsr(:)                 ! row begining index of
  ! csr sparse matrix
  real, allocatable, save    :: qr_sumqr(:),          &     ! σ over q, r
       qr_sumnp(:, :)              ! σ over p
  !
  integer, allocatable, save :: qi_bind(:, :)               ! bilinear interp. (index and
  real, allocatable, save    :: qr_bwgh(:, :)               ! weight)
  real, allocatable, save    :: qr_bdry(:)                  ! boundary weight
  real, allocatable, save    :: qr_wn1(:)                   ! wavenumber k(nk)
  !/
  !/ ------------------------------------------------------------------- /
contains
  !/ ------------------------------------------------------------------- /
  !/ [part 1]
  !/
  function qfunc(kx, ky)
    !/
    !/    19-dec-2011 : origination.                        ( o. gramstad )
    !/
    !/    09-nov-2018 : prepare ww3 distribution            ( q. liu      )
    !/
    !  1. purpose: define q = ω^2 / g (i.e., q in k94 & j09)
    !
    !  2. method:
    !     for wind-generated ocean surface waves, the dispersion relation
    !     reads
    !         ω^2 = g k tanh(kd),
    !     where g is the gravtional acceleration, ω is the radian frequency,
    !     d is the water depth. hence,
    !
    !             / k = √(k_x**2. + k_y**2.) for deep-water
    !         q = |
    !             \ k tanh(kd)               for finite-deep water (e.g.,
    !                                            d < 2500.)
    !
    !/
    implicit none
    !
    real, intent(in) :: kx, ky   ! x, y components of wavenumber
    ! vector (kx, ky)
    real             :: qfunc    ! returned function
    !/
    qfunc = sqrt(kx*kx + ky*ky)  ! deep-water case (q = k)
    !
    ! odin used qr_dmax = 2500.
    !
    if (qr_depth > qr_eps .and. qr_depth < qr_dmax) then
      qfunc = qfunc * tanh(qfunc * qr_depth)  ! finite-deep
    end if
    !
    return
    !/
  end function qfunc
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function vpfunc(k0x, k0y, k1x, k1y, k2x, k2y)
    !/
    !/    19-dec-2011: origination.                        ( o. gramstad )
    !/
    !/    09-nov-2018 : prepare ww3 distribution            ( q. liu      )
    !/
    !/
    !  1. purpose:
    !     calculate the second-order coefficient v^{(+)}_{1, 2, 3} of j09,
    !     which corresponds to u^{(3)}_{0, 1, 2} of k94.
    !
    !     ◆ v^{(+)}_{1, 2, 3} differs from u^{(3)}_{0, 1, 2} by a factor
    !       of 1/2π --- this is because the wave spectrum f(k) used in k94
    !       and j09 differ by a fator of (1/2π)^2.
    !
    !/
    implicit none
    !
    real, intent(in) :: k0x, k0y, k1x, k1y, k2x, k2y ! 3 waves
    real             :: vpfunc                       ! v^{(+)}_{1, 2, 3}
    !
    real             :: q0, q1, q2                   ! q for 3 waves
    !/
    ! call q function here
    q0 = qfunc(k0x, k0y)
    q1 = qfunc(k1x, k1y)
    q2 = qfunc(k2x, k2y)
    !
    ! odin has ignored √g here because it will be absorbed/vanish when we
    ! calculate the kernel function t. i, however, included √g here for
    ! clarity.
    ! v^{(+)}_{1, 2, 3}
    !
    vpfunc = sqrt(1.0/32.0) * (                                       &
         (k0x*k1x + k0y*k1y + q0*q1) * sqrt(sqrt(qr_grav*q2 / (q0*q1)))&
         + (k0x*k2x + k0y*k2y + q0*q2) * sqrt(sqrt(qr_grav*q1 / (q0*q2)))&
         + (k1x*k2x + k1y*k2y + q1*q2) * sqrt(sqrt(qr_grav*q0 / (q1*q2))))
    !
    return
    !/
  end function vpfunc
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function vmfunc(k0x, k0y, k1x, k1y, k2x, k2y)
    !/
    !/    19-dec-2011 : origination.                        ( o. gramstad )
    !/
    !/    09-nov-2018 : prepare ww3 distribution            ( q. liu      )
    !/
    !  1. purpose:
    !     calculate the second-order coefficient v^{(-)}_{1, 2, 3} of j09,
    !     which corresponds to u^{(1)}_{0, 1, 2} of k94.
    !
    !     ◆ v^{(-)}_{1, 2, 3} differs from u^{(1)}_{0, 1, 2} by a factor
    !       of 1/2π
    !
    !/
    implicit none
    !
    real, intent(in) :: k0x, k0y, k1x, k1y, k2x, k2y ! 3 waves
    real             :: vmfunc                       ! v^{(-)}_{1, 2, 3}
    !
    real             :: q0, q1, q2                   ! q for 3 waves
    !/
    ! call q function here
    q0 = qfunc(k0x, k0y)
    q1 = qfunc(k1x, k1y)
    q2 = qfunc(k2x, k2y)
    !
    ! v^{(-)}_{1, 2, 3}
    !
    vmfunc = sqrt(1.0/32.0) * (                                       &
         (k0x*k1x + k0y*k1y - q0*q1) * sqrt(sqrt(qr_grav*q2 / (q0*q1)))&
         + (k0x*k2x + k0y*k2y - q0*q2) * sqrt(sqrt(qr_grav*q1 / (q0*q2)))&
         + (k1x*k2x + k1y*k2y + q1*q2) * sqrt(sqrt(qr_grav*q0 / (q1*q2))))
    !
    return
    !/
  end function vmfunc
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function ufunc(k0x, k0y, k1x, k1y, k2x, k2y, k3x, k3y)
    !/
    !/    19-dec-2011 : origination.                        ( o. gramstad )
    !/
    !/    09-nov-2018 : prepare ww3 distribution            ( q. liu      )
    !/
    !  1. purpose:
    !     calculate the intermediate quantity (i.e., u_{1, 2, 3, 4} in j09,
    !     v_{0, 1, 2, 3} in k94) for the third-order coefficient (i.e.,
    !     w^{(2)}_{1, 2, 3, 4} in j09, v^{(2)}_{0, 1, 2, 3} in k94).
    !
    !     ◆ u_{1, 2, 3, 4} differs from v_{0, 1, 2, 3} by a factor of
    !       (1/2π)^2.
    !
    !/
    implicit none
    !
    real, intent(in) :: k0x, k0y, k1x, k1y,       &
         k2x, k2y, k3x, k3y        ! 4 waves
    real             :: ufunc                     ! u_{1, 2, 3, 4}
    !
    real             :: q0, q1, q2, q3            ! q for 4 waves
    !/
    ! call q function here
    q0 = qfunc(k0x, k0y)
    q1 = qfunc(k1x, k1y)
    q2 = qfunc(k2x, k2y)
    q3 = qfunc(k3x, k3y)
    !
    ! u_{1, 2, 3, 4}
    !
    ufunc = (1.0/16.0) * sqrt(sqrt(q2*q3 / (q0*q1))) *              &
         (2.0*((k0x*k0x + k0y*k0y) * q1 + (k1x*k1x + k1y*k1y) * q0)-&
         q0*q1*( qfunc(k0x+k2x, k0y+k2y) + qfunc(k1x+k2x, k1y+k2y)+&
         qfunc(k0x+k3x, k0y+k3y) + qfunc(k1x+k3x, k1y+k3y) ))
    !
    return
    !/
  end function ufunc
  !/
  !/ ------------------------------------------------------------------- /
  !/
  function tfunc(k0x, k0y, k1x, k1y, k2x, k2y, k3x, k3y)
    !/
    !/    19-dec-2011 : origination.                        ( o. gramstad )
    !/
    !/    09-nov-2018 : prepare ww3 distribution            ( q. liu      )
    !/
    !  1. purpose:
    !     calculate the kernel function for the four-wave interaction, i.e.,
    !     (t_{1, 2, 3, 4}, \widetilde{v}^{(2)}_{0, 1, 2, 3} in k94).
    !     ◆ t from j09 and k94 differ by a factor of (1/2π)^2.
    !
    !     odin's comment:
    !     kernel function for all combination that are not stokes correction.
    !     i.e. n0 != n2 and n0 != n3
    !/
    implicit none
    !
    real, intent(in) :: k0x, k0y, k1x, k1y, &
         k2x, k2y, k3x, k3y        ! 4 waves
    real             :: tfunc                     ! t_{1, 2, 3, 4}
    !
    ! virtual-state interaction: two free waves generate a virtual state
    ! consisting of bound waves, which then decays into a different set of
    ! free waves (see j09)
    !
    real             :: om0, om1, om2, om3, &     ! ω for 4 waves
         om02,               &     ! ω_{0-2}
         om13,               &     ! ω_{1-3}
         om12,               &     ! ω_{1-2}
         om03,               &     ! ω_{0-3}
         om0p1,              &     ! ω_{0+1}
         om2p3                     ! ω_{2+3}
    !
    real             :: l14, l23, l56,      &
         w                         ! w^{(2)}_{1, 2, 3, 4} in j09
    ! or
    ! v^{(2)}_{0, 1, 2, 3} in k94
    !/
    ! initilization
    om0p1 = 0.
    om2p3 = 0.
    w     = 0.
    l14   = 0.
    l23   = 0.
    l56   = 0.
    !
    ! get ω from q: q = ω^2 / g →  ω = √(qg)
    ! odin has ignored √g here because it will be absorbed/vanish when we
    ! calculate the kernel function t (v / ω). i, however, included √g here for
    ! clarity.
    !
    ! ω for four free waves
    om0 = sqrt(qr_grav * qfunc(k0x, k0y))
    om1 = sqrt(qr_grav * qfunc(k1x, k1y))
    om2 = sqrt(qr_grav * qfunc(k2x, k2y))
    om3 = sqrt(qr_grav * qfunc(k3x, k3y))
    !
    ! ω for other combined waves
    !
    om02 = sqrt(qr_grav * qfunc(k0x-k2x, k0y-k2y))
    om13 = sqrt(qr_grav * qfunc(k1x-k3x, k1y-k3y))
    om12 = sqrt(qr_grav * qfunc(k1x-k2x, k1y-k2y))
    om03 = sqrt(qr_grav * qfunc(k0x-k3x, k0y-k3y))
    !
    if (abs(k0x+k1x) > qr_eps .or. abs(k0y+k1y) > qr_eps) then
      !           k₀ + k₁ = k₂ + k₃ = 0., ω_{0+1} = 0.,
      !           v^{(-)}_{0+1, 0, 1} ~ 1/ω_{0+1} = nan, l56 = nan
      om0p1 = sqrt(qr_grav * qfunc(k0x+k1x, k0y+k1y))
      om2p3 = sqrt(qr_grav * qfunc(k2x+k3x, k2y+k3y))
    end if
    !
    ! w^{(2)}_{1, 2, 3, 4} [call u function here] for direct interaction
    !
    w = ufunc(-k0x, -k0y, -k1x, -k1y,  k2x,  k2y,  k3x,  k3y) +     &
         ufunc( k2x,  k2y,  k3x,  k3y, -k0x, -k0y, -k1x, -k1y) -     &
         ufunc( k2x,  k2y, -k1x, -k1y, -k0x, -k0y,  k3x,  k3y) -     &
         ufunc(-k0x, -k0y,  k2x,  k2y, -k1x, -k1y,  k3x,  k3y) -     &
         ufunc(-k0x, -k0y,  k3x,  k3y,  k2x,  k2y, -k1x, -k1y) -     &
         ufunc( k3x,  k3y, -k1x, -k1y,  k2x,  k2y, -k0x, -k0y)
    !
    ! first & fourth lines for virtual-state interaction in j09
    !
    l14 = vmfunc(k0x, k0y, k2x, k2y, k0x-k2x, k0y-k2y) *            &
         vmfunc(k3x, k3y, k1x, k1y, k3x-k1x, k3y-k1y) *            &
         (1.0/(om2 + om02 - om0) + 1.0/(om1 + om13 - om3)) +       &
         vmfunc(k1x, k1y, k3x, k3y, k1x-k3x, k1y-k3y) *            &
         vmfunc(k2x, k2y, k0x, k0y, k2x-k0x, k2y-k0y) *            &
         (1.0/(om3 + om13 - om1) + 1.0/(om0 + om02 - om2))
    !
    ! second & third lines for virtual-state interaction in j09
    !
    l23 = vmfunc(k1x, k1y, k2x, k2y, k1x-k2x, k1y-k2y) *            &
         vmfunc(k3x, k3y, k0x, k0y, k3x-k0x, k3y-k0y) *            &
         (1.0/(om2 + om12 - om1) + 1.0/(om0 + om03 - om3)) +       &
         vmfunc(k0x, k0y, k3x, k3y, k0x-k3x, k0y-k3y) *            &
         vmfunc(k2x, k2y, k1x, k1y, k2x-k1x, k2y-k1y) *            &
         (1.0/(om3 + om03 - om0) + 1.0/(om1 + om12 - om2))
    !
    ! fifth & sixth lines for virtual-state interaction in j09
    !
    if (abs(k0x+k1x) > qr_eps .or. abs(k0y+k1y) > qr_eps) then
      !           k₁ + k₂ = k₃ + k₄ = 0., ω_{1+2} = 0.,
      !           v^{(-)}_{1+2, 1, 2} ~ 1/ω_{1+2} = nan, l56 = nan
      l56 = vmfunc(k0x+k1x, k0y+k1y, k0x, k0y, k1x, k1y) *        &
           vmfunc(k2x+k3x, k2y+k3y, k2x, k2y, k3x, k3y) *        &
           (1.0/(om0p1 - om0 - om1) + 1.0/(om2p3 - om2 - om3)) + &
           vpfunc(-k0x-k1x, -k0y-k1y, k0x, k0y, k1x, k1y) *      &
           vpfunc(-k2x-k3x, -k2y-k3y, k2x, k2y, k3x, k3y) *      &
           (1.0/(om0p1 + om0 + om1) + 1.0/(om2p3 + om2 + om3))
    end if
    !
    ! t_{1, 2, 3, 4}
    !
    tfunc =  w - l14 - l23 - l56
    !
    return
    !/
  end function tfunc
  !/
  !/ ------------------------------------------------------------------- /
  !/ [part 2]
  !/
  subroutine findquartetnumber(ns, kx, ky, om, oml, nnz)
    !/
    !/    19-dec-2011 : origination.                        ( o. gramstad )
    !/
    !/    09-nov-2018 : prepare ww3 distribution            ( q. liu      )
    !/    02-apr-2020 : boundary conditions (< kmin, > kmax)( q. liu      )
    !/
    !  1. purpose:
    !     find the total number of quartets (resonant and quasi/near-resonant
    !     four waves) satisfying the criteria below:
    !
    !     1) \vec{k₁} + \vec{k₂} = \vec{k₃} + \vec{k₄}
    !
    !     2) δω = |ω₁ + ω₂ - ω₃ - ω₄| <= λc \min(ω₁, ω₂, ω₃, ω₄)
    !        - that is, quartets far from the resonance is excluded for
    !          saving the computational cost.
    !
    !     3) for a given 2d frequency-direction grid (k_i, θ_j, i = 1, ...,
    !        nk, j = 1, ..., nth) consisting of ns points (ns = nk * nth),
    !        we will first reshape the 2d spectral grid (k_i, θ_j)
    !        into a 1d wavenumber vector (k_{xl}, k_{yl}, l = 1, ..., ns).
    !        afterwards, we should have
    !        3.a) l₂ >= l₁
    !        3.b) l₃ ≠  l₁, l₃ ≠ l₂ (otherwise the third and fourth wave
    !                                components will be the same as the
    !                                first and second)
    !        3.c) l₄ >= l₃
    !        3.d) l₄ ≠  l₁, l₄ ≠ l₂
    !        3.e) k₄ >= k_{min}, k₄ <= k_{max}
    !
    !        note that `l` here only denotes the index of a specific wave
    !        component inside the 1d wavenumber vector array. for k₄, its
    !        index l₄ is not exact, and is just approximated by the index
    !        of its closest wave component.
    !
    !     4) if we store the located quartets in a 2d large sparse matrix,
    !        which can be organized as
    !               |k k k |
    !               |∩ ∩ ∩ |
    !               |3 q l₃| 1 2 . . . n 1 2 . . . n . . . . . . 1 2 . . . n
    !               |4 r l₄| 1 1 1 1 1 1 2 2 2 2 2 2 . . . . . . n n n n n n
    !               |∪ ∪ ∪ |
    !        -------
    !        k {1 2}
    !        k {n p}
    !        k {l₁l₂}
    !        -------
    !           1 1
    !           2 1                                  (2,1,n,3) ✗⁴    ✗²(2,1,3,n)
    !           . 1                           col > row  → ▲
    !           . 1
    !           . 1
    !           n 1
    !           1 2                                  (1,2,n,3) ✗³   [★ (1,2,3,n)]
    !           2 2
    !           . 2
    !           . 2                        ⊚ ← row = l₁ + (l₂ - 1) * ns
    !           . 2                        ↑
    !           n 2                        col = l₃ + (l₄ - 1) * ns
    !           . .
    !           . .
    !           . .
    !           . .
    !           . .
    !           . .  (n,3,2,1) ✓⁴        ✓³(n,3,1,2)
    !           1 n              ▼ ← col < row
    !           2 n
    !           . n  (3,n,2,1) ✓²        ☆ (3,n,1,2)
    !           . n
    !           . n
    !           n n
    !
    !        where `n` shown above denotes `ns`, not `nk`, therefore the shape
    !        of this large sparse matrix is (ns*ns, ns*ns).
    !
    !        only quartets with col > row (highlighted by ▲ , i.e.,
    !                           ---------
    !        elements in the upper trianglar matrix) are selected because,
    !        for example, ★ (1, 2, 3, ns) & ☆ (3, ns, 1, 2) essentially
    !        refer to the same quartet.
    !
    !     to sum up, criteria 1) and 2) are kinetic, whereas 3) and 4) are
    !     enforced to avoid duplicating quartets since
    !     ★  (k₁, k₂, k₃, k₄)
    !
    !     & [symmetric]
    !     ✗² (k₂, k₁, k₃, k₄) ← filterd by 3.a)
    !     ✗³ (k₁, k₂, k₄, k₃) ← filterd by 3.c)
    !     ✗⁴ (k₂, k₁, k₄, k₃) ← filterd by 3.a) and 3.c)
    !
    !     & [antisymmetric]
    !     ☆  (k₃, k₄, k₁, k₂) ← filterd by 4)
    !     ✓² (k₃, k₄, k₂, k₁)
    !     ✓³ (k₄, k₃, k₁, k₂)
    !     ✓⁴ (k₄, k₃, k₂, k₁)
    !
    !     are essentially the same.
    !
    !     ◆ criteria 3.b) and 3.d) exclude two quartets:
    !        / k₁ = k₃, k₂ = k₄ (k₁, k₂, k₁, k₂)
    !        \ k₁ = k₄, k₂ = k₃ (k₁, k₂, k₂, k₁)
    !        →  singular points for the nonlinear transfer integral as
    !           t_{1, 2, 1, 2} or t_{1, 2, 2, 1} ~ 1 / 0 = nan
    !
    !       van vledder (2006, p. 231) argued that the first quadruplet had
    !       negligible contribution to the total transfer rate. similarly,
    !       for the symmetric reason, the contribution from the second
    !       quadruplet is also very limited.
    !
    !     ◆ we should keep in mind that the snl term for wave component
    !       3 in ★ (i.e., k₃) and in ☆ (i.e., k₁) are the same.
    !
    !     ◆ although the other 7 quartets are not counted here, their
    !       contributions to the nonlinear transfer rates should not be
    !       ignored as the interval of the 6d integration starts from
    !       -∞ and ends at +∞ !
    !
    !     more details can be found in appendix of lgb21.
    !
    !     see also references:
    !     hasselmann and hasselmann (1981)  exact-nl/dia report
    !     hasselmann and hasselmann (1985), jpo, 15, 1369 - 1377.
    !     annenkov and shrira (2015),       jpo, 45, 807-812
    !
    !/
    implicit none
    !
    integer, intent(in)  :: ns           ! length of 1d wavenumber
    ! vector, ns = nk * nth
    real, intent(in)     :: kx(ns),   &
         ky(ns),   &  ! (kx, ky) components
         om(ns)       ! ω or σ
    real, intent(in)     :: oml          ! cut-off value λc for the
    ! quasi-resonant criterion 2)
    !
    integer(kind=8), intent(out)      &
         :: nnz          ! total number of quartets
    ! i.e., nonzero values
    ! in the large-sparse matrix
    ! illustrated above
    !
    ! local parameters
    real                 :: k(ns)        ! scalar/mag k
    integer              :: i1, i2, i3, i4, row, col
    real                 :: k4x, k4y, k4, om4, kmin, kmax, dom
    !/
    ! scalar wavenumber (i.e., magnitude)
    k    = sqrt(kx*kx + ky*ky)
    kmin = minval(k)
    kmax = maxval(k)
    !
    ! boundary conditions: include k4 beyond kmin & kmax
    if (qi_interp .eq. 1 .and. qi_bound .eq. 1) then
      kmin = kmin / 9.  ! 1/3 fmax
      kmax = kmax * 9.  ! 3   fmax
    end if
    !
    ! start to find the quartets: \vec{k_j}, j = 1, 2, 3 are chosen at the
    ! grid points, and \vec_{k_4} is found by
    !     \vec{k_4} = \vec{k_1} + \vec{k_2} - \vec{k_3}
    !
    nnz = 0
    !
    do i1 = 1, ns
      !           criterion 3.a) ← starting from i1
      do i2 = i1, ns
        do i3 = 1, ns
          !                   criterion 3.b)
          if (i3 .ne. i1 .and. i3 .ne. i2) then
            !                       criterion 1)
            k4x = kx(i1) + kx(i2) - kx(i3)
            k4y = ky(i1) + ky(i2) - ky(i3)
            k4  = sqrt(k4x*k4x + k4y*k4y)
            !
            ! wavenumber k4 falls outside the grid (criterion 3.e)
            if (k4 >= kmin .and. k4 <= kmax) then
              !                           ω = √(qg) & δω
              om4 = sqrt(qr_grav * qfunc(k4x, k4y))
              dom = abs(om(i1) + om(i2) - om(i3) - om4) / &
                   min(om(i1), om(i2), om(i3), om4)
              !                           criterion 2)
              if (oml <= qr_eps .or. dom <= oml) then
                i4 = minloc((kx - k4x)*(kx - k4x) +     &
                     (ky - k4y)*(ky - k4y), 1)
                !                               criterion 3.d)
                if (i4 .ne. i1 .and. i4 .ne. i2) then
                  !                                   criterion 3.c)
                  if (i4 >= i3) then
                    row = i1 + ns * (i2-1)
                    col = i3 + ns * (i4-1)
                    !                                       criterion 4)
                    if (col > row) then
                      nnz = nnz + 1
                    end if
                  end if
                end if
              end if
            end if
          end if
        end do
      end do
    end do
    !/
  end subroutine findquartetnumber
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine findquartetconfig(ns, kx, ky, om, oml, nnz,     &
       nn, pp, qq, rr,               &
       k4x, k4y, om4)
    !/
    !/    19-dec-2011 : origination.                        ( o. gramstad )
    !/
    !/    09-nov-2018 : prepare ww3 distribution            ( q. liu      )
    !/    02-apr-2020 : boundary conditions (< kmin, > kmax)( q. liu      )
    !/
    !  1. purpose:
    !     find all the quartets that we are interested in. initially i thought
    !     we may merge this subroutine and the subroutine above (i.e.,
    !     findquartetnumber) in such a way that we first initialize a large
    !     array like quartet(hnum), where hnum is a huge integer (something
    !     like 0.5*ns**4). but it quickly turned out this was a very naive
    !     idea because for the wavenumber grid (k, θ) used by 3g spectral
    !     wave models, in general ns~o(10^2-3), then ns^4~o(10^8-12). thus,
    !     hnum becomes really very very very huge, and then we may have
    !     the integer/memory overflow problem.
    !
    !     based on the above-mentioned, we must split the whole process:
    !     1) find the total number of quartets with findquartetnumber, `nnz`
    !     2) allocate arrays with the known `nnz`, and store the wavenumber
    !        and ω for k₄
    !
    !     for more details, see the header of the subr. findquartetnumber.
    !
    !/
    implicit none
    !
    integer, intent(in)  :: ns           ! length of 1d wavenumber
    ! vector, ns = nk * nth
    real, intent(in)     :: kx(ns),   &
         ky(ns),   &  ! (kx, ky) components
         om(ns)       ! ω or σ
    real, intent(in)     :: oml          ! cut-off value λc for the
    ! quasi-resonant criterion 2)
    integer(kind=8), intent(in)       &
         :: nnz          ! total number of quartets
    ! returned from the subr.
    ! findquartetnumber
    !
    integer, intent(out) :: nn(nnz),  &  ! index of k₁
         pp(nnz),  &  !          k₂
         qq(nnz),  &  !          k₃
         rr(nnz)      !          k₄ in the 1d
    ! wavenumber vector [1 - ns]
    real, intent(out)    :: k4x(nnz), &
         k4y(nnz), &  ! x, y comp. of k₄
         om4(nnz)     ! ω₄
    !
    ! local parameters
    real                 :: k(ns)        ! scalar/mag k
    integer              :: i1, i2, i3, i4, row, col, s
    real                 :: k4xt, k4yt, k4t, om4t, kmin, kmax, dom
    !/
    ! scalar wavenumber (i.e., magnitude)
    k    = sqrt(kx*kx + ky*ky)
    kmin = minval(k)
    kmax = maxval(k)
    !
    ! boundary conditions: include k4 beyond kmin & kmax
    if (qi_interp .eq. 1 .and. qi_bound .eq. 1) then
      kmin = kmin / 9.  ! 1/3 fmax
      kmax = kmax * 9.  ! 3   fmax
    end if
    !
    ! start to find the quartets: \vec{k_j}, j = 1, 2, 3 are chosen at the
    ! grid points, and \vec_{k_4} is found by
    !     \vec{k_4} = \vec{k_1} + \vec{k_2} - \vec{k_3}
    !
    ! s: count of quartets. this time the total number of quartets `nnz` is
    ! already known from `findquartetnumber`.
    !       nnz = 0
    s = 0
    !
    do i1 = 1, ns
      !           criterion 3.a) ← starting from i1
      do i2 = i1, ns
        do i3 = 1, ns
          !                   criterion 3.b)
          if (i3 .ne. i1 .and. i3 .ne. i2) then
            !                       criterion 1)
            k4xt = kx(i1) + kx(i2) - kx(i3)
            k4yt = ky(i1) + ky(i2) - ky(i3)
            k4t  = sqrt(k4xt*k4xt + k4yt*k4yt)
            !
            ! wavenumber k4 falls outside the grid (criterion 3.e)
            if (k4t >= kmin .and. k4t <= kmax) then
              !                           ω = √qg & δω
              om4t = sqrt(qr_grav * qfunc(k4xt, k4yt))
              dom  = abs(om(i1) + om(i2) - om(i3) - om4t)/&
                   min(om(i1), om(i2), om(i3), om4t)
              !                           criterion 2)
              if (oml <= qr_eps .or. dom <= oml) then
                i4 = minloc((kx - k4xt)*(kx - k4xt) +   &
                     (ky - k4yt)*(ky - k4yt), 1)
                !                               criterion 3.d)
                if (i4 .ne. i1 .and. i4 .ne. i2) then
                  !                                   criterion 3.c)
                  if (i4 >= i3) then
                    row = i1 + ns * (i2-1)
                    col = i3 + ns * (i4-1)
                    !                                       criterion 4)
                    if (col > row) then
                      !                                           nnz    = nnz + 1
                      s      = s + 1 ! find 1 quartet
                      !
                      nn(s)  = i1    ! store index
                      pp(s)  = i2
                      qq(s)  = i3
                      rr(s)  = i4
                      !
                      k4x(s) = k4xt  ! k₄, ω₄
                      k4y(s) = k4yt
                      om4(s) = om4t
                      !
                    end if
                  end if
                end if
              end if
            end if
          end if
        end do
      end do
    end do
    !
    ! check consistency of s and nnz
    if (s .ne.  nnz) then
      write(*, 1001) 'findquartetconfig'
      call exit(1)
    end if
    !
    ! formats
1001 format(/' *** gke error in gkemodule : '/  &
         '     subr. ', a, ': the number of quartet configs. does not match nnz!'/)
    !/
  end subroutine findquartetconfig
  !/
  !/ ------------------------------------------------------------------- /
  !/ [part 3]
  !/
  subroutine coocsrind (nrow, nnz, ir, jc, ind_translate, iao)
    !/
    !/    12-sep-2012 : origination.                        ( version 3.14 )
    !/                  based on coocsr of sparkit          ( o. gramstad  )
    !/
    !/    16-nov-2018 : prepare ww3 distribution            ( q. liu      )
    !/
    !  1. purpose:
    !     it becomes clear from subr. findquartetnumber & findquartetconfig
    !     that we are faced with a problem of large sparse matrice when we
    !     manipulate the huge set of quartets. by sparse matrix we mean
    !     only a `relatively small number` of its matrix elements are nonzero.
    !
    !     for saving time or memory space, a sparse matrix is usually stored
    !     in some compressed formats in the computer memory. two among those
    !     formats, coo & csr are relevant here in our application:
    !     1) the coordinate format (coo) --- the simplest storage scheme
    !        for a given sparse matrix `a` (n, n) with nnz nonzero elements,
    !        the coo format consists of 3 arrays:
    !        * a (nnz): real nonzero values of a in `any order`
    !        * ir(nnz): row indices of these nonzero values
    !        * jc(nnz): column indices
    !
    !     2) the compressed sparse row format (csr)
    !        the csr format is the basic format used in sparskit, consisting
    !        of three arrays as well
    !        * a (nnz): real nonzero values of a stored row by row from row
    !                   1 to row n
    !        * jc(nnz): column indices in `any order`
    !        * ia(n+1): the index of the first nonzero element at this
    !                   corresponding row in the array a and jc, that is
    !                   ia(i) provides the position in a & jc where the i-th
    !                   row starts.
    !
    !     this subroutine converts the sparse matrix (nrow, nrow) in the coo
    !     format, as represented by (ir, jc) to the csr format, as represented
    !     by (ind_translate, iao).
    !
    !     n.b.:
    !     this subr. neither needs the real value array in the coo format,
    !     nor returns the real value array in the csr format. alternatively,
    !     it returns the tranformed index (ind_translate) from coo to csr.
    !     with such indices, we have
    !     *) a_csr  = a_coo(ind_translate)
    !     *) jc_csr = jc_coo(ind_translate)
    !
    !     references:
    !     youcef saad, 1994, sparskit: a basic tool kit for sparse matrix
    !         compuation (version 2, `coocsr` therein)
    !     see also numerical recipe in fortran (ch. 2.7, p. 71)
    !/
    implicit none
    !
    integer, intent(in)  :: nrow               ! # of rows of sparse matrix
    integer(kind=8), intent(in)     &
         :: nnz                ! # of nonzero elements
    integer, intent(in)  :: ir(nnz)            ! coo row
    integer, intent(in)  :: jc(nnz)            ! coo col
    integer, intent(out) :: ind_translate(nnz) ! indices from coo to csr
    integer, intent(out) :: iao(nrow+1)        ! csr iao
    !
    ! local parameters
    integer              :: i, j, k, k0, iad
    !/
    ! determine the number of non-zeros in each row (iao(i), i = 1, ..., nrow,
    ! will be the # of nonzero elements at the i-th row), whereas
    ! iao(nrow+1) = 0
    iao(1:nrow+1) = 0
    do k = 1, nnz
      iao(ir(k)) = iao(ir(k)) + 1 ! row by row
    end do
    !
    ! find the positions that correspond to the first value in each row.
    ! now iao(i) is the position where the i-th row starts, and
    ! iao(nrow+1) = 1 + nnz
    k = 1
    do j = 1, nrow+1
      k0     = iao(j)  ! num_i, # of nonzero in this row
      iao(j) = k       ! starting pos
      k      = k + k0  ! k = σnum_i, where i <= j
    end do
    !
    ! go through the structure once more. fill in ind_translate
    do k = 1, nnz
      i = ir(k)        ! coo row
      j = jc(k)        ! coo col
      !
      ! when i-th row is encountered by the first time, iad = iao(i) denotes
      ! the starting position for this row. afterwards, iao(i) is added by 1
      ! when i-th row arrives every time. in the end, iao(i) records the
      ! starting position for the (i+1)-th row. however, the last element of
      ! iao remains unchanged, i.e., iao(nrow+1) = iao(nrow) = 1 + nnz
      iad                = iao(i)
      ind_translate(iad) = k
      iao(i)             = iad + 1
    end do
    !
    ! shift back iao.
    do j = nrow, 1, -1
      iao(j+1) = iao(j)
    end do
    iao(1) = 1
    !
    return
    !/
  end subroutine coocsrind
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine asymsmattimvec (n, a, ja, ia, x, y, symb)
    !/
    !/    07-sep-2012 : origination.                        ( version 3.14 )
    !/                  based on amux & atmux of sparkit    ( o. gramstad  )
    !/
    !/    16-nov-2018 : prepare ww3 distribution            ( q. liu      )
    !/    19-feb-2018 : add `symb` keyword                  ( q. liu      )
    !/
    ! 1. purpose:
    !    --------> symb = -1 (antisymmetric)
    !    calculate the dot product of an antisymmetric csr sparse matrix
    !    and a vector x.
    !
    !    an antisymmetric (skew-symmetric) matrix is a square matrix `b`
    !    whose transpose equals to its negative, i.e.,
    !        b^t = -b
    !
    !    ◆ do not be confused by the name of this subr. the coming-in csr
    !      sparse matrix `a` is not symmetric or antisymmetric. in our case,
    !      `a` is a upper triangular sparse matrix, and we are acturally
    !      calculating the dot product of `a - a^t` and `x`, where
    !      'a - a^t' is an antisymmetric matrix due to the symmetry of
    !      four-wave nonlinear interactions (dn₁/dt = -dn₃/dt).
    !
    !    this operation is in essence the dot product of two common dense
    !    matrix/vector, such as
    !        m(n, 1)  = a(n, n) * x(n, 1)
    !        or
    !        m_{i, 1} = σa(i, j)  * x(j, 1)
    !
    !    for the transposed array a^t,
    !        n_{i, 1} = σat(i, j) * x(j, 1)
    !                 = σ a(j, i) * x(j, 1)
    !    alternatively, we can exchange the index of i, j for easy
    !    understanding:
    !        n_{j, 1} = σat(j, i) * x(i, 1)
    !                 = σ a(i, j) * x(i, 1)
    !
    !    finally, y = m - n = a * x - a^t * x
    !
    !    --------> symb = 1 (symmetric)
    !    same as above but for y = m + n = a * x + a^t * x
    !/
    implicit none
    !
    integer, intent(in)   :: n           ! # of rows/cols
    !
    real, intent(in)      :: a(:)        ! csr a (nnz)
    integer, intent(in)   :: ja(:)       ! csr ja(nnz)
    integer, intent(in)   :: ia(n+1)     ! csr ia(n+1)
    !
    real, intent(in)      :: x(n)        ! vector of the same length
    real, intent(out)     :: y(n)        ! return product y = b * x
    real, intent(in)      :: symb        ! -1 for minus, 1 for plus
    !
    ! local parameters
    integer               :: i, k
    real                  :: t
    !/
    ! initilization
    y(1:n) = 0.0
    !
    do i = 1, n
      t = 0.0
      do k = ia(i), ia(i+1)-1
        !
        ! m_{i, 1} =  σa(i, j) * x(j, 1)
        t        = t + a(k) * x(ja(k))
        !
        !±n_{j, 1} = ±σa(i, j)  * x(i, 1)
        y(ja(k)) = y(ja(k)) + symb * a(k)*x(i)
      end do
      y(i) = y(i) + t
    end do
    ! the final y = m ± n = a * x ± a^t * x
    !
    return
    !/
  end subroutine asymsmattimvec
  !/
  !/ ------------------------------------------------------------------- /
  !/ part 4
  !/
  subroutine prepkgrid(nk, nth, dpt, sig, th)
    !/
    !/    04-dec-2018 : origination.                        ( q. liu      )
    !/    04-dec-2018 : based on `z_cmpcg` & `z_wnumb` of serv_xnl4v5.f90
    !/                                                      ( q. liu      )
    !/    01-apr-2019 : add the option using wavnu1         ( q. liu      )
    !/
    ! 1. purpose:
    !    compute wave number k for a given discrete frequency grid and water
    !    depth based on the dispersion relation for the linear wave theory
    !        ω^2 = gk tanh(kd)
    !
    !    ◆ in ww3, the radian frequency grid ω is invariant.
    !
    !    ◆ it is desired that the gke module should be independent from ww3
    !      as much as possible. so i decided not to directly obtain
    !      `nk, nth, sig, wn, cg, dsii` from ww3
    !
    ! 2. method
    !    ✓ dispopt = 0
    !    finite depth linear dispersion relation, using a pade approximation
    !    (hunt, 1988) [see wrt serv_xnl4v5.f90]
    !
    !    ✓ dispopt = 1 for wavnu1
    !/
    use w3dispmd, only: wavnu1
    !
    implicit none
    !
    integer, intent(in)    :: nk          ! # of frequencies
    integer, intent(in)    :: nth         ! # of directions
    real, intent(in)       :: dpt         ! water depth (m)
    real, intent(in)       :: sig(nk)     ! radian frequency σ
    real, intent(in)       :: th(nth)     ! θ (rad) [equally spaced,
    ! but may start from non-zero
    ! value]
    !
    integer, parameter     :: dispopt = 1 ! dispersion relation
    !
    integer                :: ik, ith, jkth, ns
    real                   :: x, xx, y, omega
    real                   :: k, cg, dsii, angr, dth
    real                   :: esin(nth), ecos(nth)
    !/
    ! initialization
    ns     = nk * nth
    ! allocation of qr_kx/ky/dk/om/wn1 was done in prepkernelio)
    qr_kx  = 0. ! ns
    qr_ky  = 0.
    qr_dk  = 0.
    qr_om  = 0.
    qr_wn1 = 0. ! nk
    !
    ! calc δθ, cosθ, sinθ [θ is equally spaced]
    dth = qr_tpi / real(nth)
    !
    do ith = 1, nth
      angr      = th(ith)
      esin(ith) = sin(angr)
      ecos(ith) = cos(angr)
      !
      if (abs(esin(ith)) .lt. 1.e-5) then
        esin(ith) = 0.
        if (ecos(ith) .gt. 0.5) then
          ecos(ith) = 1.      ! θ = 0.
        else
          ecos(ith) = -1.     ! θ = π
        end if
      end if
      !
      if (abs(ecos(ith)) .lt. 1.e-5) then
        ecos(ith) = 0.
        if (esin(ith) .gt. 0.5) then
          esin(ith) = 1.     ! θ = π/2
        else
          esin(ith) = -1.    ! θ = π * 3/2
        end if
      end if
    end do
    !
    do ik = 1, nk
      if (dispopt .eq. 0) then
        ! calc k & cg (`z_cmpcg` & `z_wnumb` of serv_xnl4v5.f90)
        omega   = sig(ik)**2.0/qr_grav
        y       = omega*dpt
        xx      = y*(y+1.0/(1.0+y*(0.66667+y*(0.35550+y*(0.16084+y*(0.06320+y* &
             (0.02174+y*(0.00654+y*(0.00171+y*(0.00039+y*0.00011))))))))))
        x       = sqrt(xx)
        k       = x/dpt
        !
        if(dpt*k > 30.0) then
          cg  = qr_grav/(2.0*sig(ik))
        else
          cg  = sig(ik)/k*(0.5+dpt*k/sinh(2.0*dpt*k))
        end if
        !
      else if (dispopt .eq. 1) then
        ! calc k & cg (wavnu1 from ww3)
        call wavnu1(sig(ik), dpt, k, cg)
      end if
      qr_wn1(ik) = k ! store k in qr_wn1 ('ll used for interp.)
      ! calc δσ
      if (ik .eq. 1) then
        dsii = 0.5 * (sig(2) - sig(1))       ! first bin
      else if (ik .eq. nk) then
        dsii = 0.5 * (sig(nk) - sig(nk-1))   ! last bin
      else
        dsii = 0.5 * (sig(ik+1) - sig(ik-1)) ! interm. bin
      end if
      ! calc kx, ky
      do ith = 1, nth
        jkth        = ith + (ik - 1) * nth
        qr_kx(jkth) = k * ecos(ith)
        qr_ky(jkth) = k * esin(ith)
        ! calc δ\vec{k} = k δk δθ = k δσ/cg δθ
        qr_dk(jkth) = k * dsii / cg * dth
        qr_om(jkth) = sig(ik)
      end do
    end do
    !
    return
    !/
  end subroutine prepkgrid
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine prepkernelio(nk, nth, sig, th, act)
    !/
    !/    04-dec-2018 : origination                         ( q. liu      )
    !/    04-dec-2018 : extracted from odin's subr. `calcqrsnl`
    !/                                                      ( q. liu      )
    !/
    ! 1. purpose:
    !    read & write the pre-computed kernel coefficients `t` for a given
    !    discrete wavenumber grid and water depth.
    !
    !    for a typical 2d finite-depth wave model application, the wavenumber
    !    grid varies according to water depth. consequently, the quartet
    !    configuration and interactive kernel coefficients will change as
    !    well.
    !
    !    therefore, it seems extremely difficult to handle a 2d varied-depth
    !    application as the total number of quartets (qi_nnz) and thus the
    !    array size of `inpqr0` vary [see calcqrsnl]. initializing a 2d
    !    array `inpqr0` with a fixed size of (qi_nnz, nsea) becomes impossible.
    !
    !    so currently we are limiting ourself to deep-water or constant
    !    finite-deep cases.
    !
    !/
    use constants, only: file_endian
    implicit none
    !
    integer, intent(in)          :: nk             ! # of frequencies
    integer, intent(in)          :: nth            ! # of directions
    real, intent(in)             :: sig(nk)        ! radian frequency (rad)
    real, intent(in)             :: th(nth)        ! θ (rad) [equally spaced,
    ! but may start from non-zero
    ! value]
    character(len=*), intent(in) :: act            ! 'read' or 'write'
    !
    ! local parameters
    integer                      :: ns, iq, i1, i3, icol
    integer(kind=8)              :: rpos           ! reading position
    integer, allocatable         :: irow_coo(:), & ! row of coo mat
         icootcsr(:)    ! index for coo → csr
    !/
    ! initilization
    ns      = nk * nth
    qi_nrsm = ns * ns
    ! → be very careful that the size of `qi_ircsr` is not qi_nnz !
    if (allocated(qi_ircsr)) deallocate(qi_ircsr); allocate(qi_ircsr(qi_nrsm+1))
    if (allocated(qr_sumqr)) deallocate(qr_sumqr); allocate(qr_sumqr(qi_nrsm))
    if (allocated(qr_sumnp)) deallocate(qr_sumnp); allocate(qr_sumnp(ns, ns))
    ! qr_dk/om
    if (allocated(qr_dk))    deallocate(qr_dk);    allocate(qr_dk(ns))
    if (allocated(qr_om))    deallocate(qr_om);    allocate(qr_om(ns))
    !
    ! determine water depth for the whole module, which will be used by
    ! `t` & `q` func.
    qr_depth  = max(0., min(qr_depth, qr_dmax))
    qi_disc   = max(0,  min(qi_disc, 1))
    qi_kev    = max(0,  min(qi_kev, 1))
    qi_interp = max(0,  min(qi_interp, 1))
    if (qi_disc .eq. 1) qi_interp = 0
    !
    ! determine the name for the binary file which stores the quartet
    ! configuration and the corresponding kernel coefficient ['gkev?_d????.cfg]
    ! constant-depth or deep water
    write(qs_cfg, "(a, '_d', i4.4, '.cfg')") trim(qs_ver), int(qr_depth)
    !
    if (trim(act) == 'write') then
      ! calc kgrid → [qr_kx/ky/dk/om/wn]
      if (allocated(qr_kx))     deallocate(qr_kx);    allocate(qr_kx(ns))
      if (allocated(qr_ky))     deallocate(qr_ky);    allocate(qr_ky(ns))
      if (allocated(qr_wn1))    deallocate(qr_wn1);   allocate(qr_wn1(nk))
      call prepkgrid(nk, nth, qr_depth, sig, th)
      ! find total # of quartets → [qi_nnz]
      call findquartetnumber(ns, qr_kx, qr_ky, qr_om, qr_oml, qi_nnz)
      ! find quartet config. → [qi_nn/pp/qq/rr & qr_k4x/k4y/om4]
      if (allocated(qi_nn))     deallocate(qi_nn);    allocate(qi_nn(qi_nnz))
      if (allocated(qi_pp))     deallocate(qi_pp);    allocate(qi_pp(qi_nnz))
      if (allocated(qi_qq))     deallocate(qi_qq);    allocate(qi_qq(qi_nnz))
      if (allocated(qi_rr))     deallocate(qi_rr);    allocate(qi_rr(qi_nnz))
      !
      if (allocated(qr_k4x))    deallocate(qr_k4x);   allocate(qr_k4x(qi_nnz))
      if (allocated(qr_k4y))    deallocate(qr_k4y);   allocate(qr_k4y(qi_nnz))
      if (allocated(qr_om4))    deallocate(qr_om4);   allocate(qr_om4(qi_nnz))
      !
      call findquartetconfig(ns, qr_kx, qr_ky, qr_om, qr_oml, qi_nnz, &
           qi_nn, qi_pp, qi_qq, qi_rr,              &
           qr_k4x, qr_k4y, qr_om4)
      !
      ! calc kernel `t`
      if (allocated(qr_tkern))  deallocate(qr_tkern); allocate(qr_tkern(qi_nnz))
      if (allocated(qr_tkurt))  deallocate(qr_tkurt); allocate(qr_tkurt(qi_nnz))
      if (allocated(qr_dom))    deallocate(qr_dom);   allocate(qr_dom(qi_nnz))
      !
      do iq = 1, qi_nnz
        qr_tkern(iq) = tfunc(qr_kx(qi_nn(iq)), qr_ky(qi_nn(iq)),&
             qr_kx(qi_pp(iq)), qr_ky(qi_pp(iq)),&
             qr_kx(qi_qq(iq)), qr_ky(qi_qq(iq)),&
             qr_k4x(iq)      , qr_k4y(iq)      )
      end do
      ! calc kernel coeff. for kurtosis
      qr_tkurt = qr_tkern * sqrt(qr_om(qi_nn) * qr_om(qi_pp) * qr_om(qi_qq) * qr_om4)
      ! calc δω (remove very small δω; δω=0 →  resonant quartets)
      qr_dom = qr_om(qi_nn) + qr_om(qi_pp) - qr_om(qi_qq) - qr_om4
      ! todo: should we use double precision for qr_dom
      ! note for gnu compiler, qr_eps~1.2e-7 (single prec.) & ~2.2e-16 (double).
      ! the values above are also true for the intel compiler.
      ! sin(δωt) / δω is very different for δω = 0 and δw~1e-7 when t is large.
      where(abs(qr_dom) < qr_eps) qr_dom = 0.0
      !
      ! calc interp. weight if necessary
      if (qi_interp .eq. 1) then
        if (allocated(qi_bind)) deallocate(qi_bind); allocate(qi_bind(4, qi_nnz))
        if (allocated(qr_bwgh)) deallocate(qr_bwgh); allocate(qr_bwgh(4, qi_nnz))
        if (qi_bound .eq. 1 ) then
          if (allocated(qr_bdry)) deallocate(qr_bdry); allocate(qr_bdry(qi_nnz))
        end if
        call biinterpwt(nk, nth, qr_wn1, th)
      end if
      !
      deallocate(qr_kx, qr_ky)
      deallocate(qr_k4x, qr_k4y, qr_om4)
      if (qi_interp .eq. 1) deallocate(qr_wn1)
      !
      ! sparse matrix index conversion [iccos shared by two formats: coo & csr]
      if (allocated(qi_iccos))  deallocate(qi_iccos); allocate(qi_iccos(qi_nnz))
      if (allocated(irow_coo))  deallocate(irow_coo); allocate(irow_coo(qi_nnz))
      if (allocated(icootcsr))  deallocate(icootcsr); allocate(icootcsr(qi_nnz))
      !
      irow_coo = qi_nn + (qi_pp - 1) * ns
      qi_iccos = qi_qq + (qi_rr - 1) * ns
      !
      ! findquartetconfig stores the quartet row by row in a discontinuous order,
      ! so we need keep icootcsr & qi_ircsr
      call coocsrind(qi_nrsm, qi_nnz, irow_coo, qi_iccos, icootcsr, qi_ircsr)
      !
      ! reorder index & arrays [coo → crs]
      qi_nn    = qi_nn(icootcsr)     ! used for calc. action prod.
      qi_pp    = qi_pp(icootcsr)
      qi_qq    = qi_qq(icootcsr)
      qi_rr    = qi_rr(icootcsr)
      qr_tkern = qr_tkern(icootcsr)
      qr_tkurt = qr_tkurt(icootcsr)
      qr_dom   = qr_dom(icootcsr)    ! δω
      !
      if (qi_interp .eq. 1) then     ! bilinear interp. weight
        qi_bind = qi_bind(:, icootcsr)
        qr_bwgh = qr_bwgh(:, icootcsr)
        if (qi_bound .eq. 1) qr_bdry = qr_bdry(icootcsr)
      end if
      !
      qi_iccos = qi_iccos(icootcsr)
      deallocate(irow_coo, icootcsr)
      !
      ! construct the sum vectors [used for 6d integration]
      ! σ over q, r [qr_sumqr]
      qr_sumqr = 2.0
      do i3 = 1, ns
        !              i3 == i4
        icol = i3 + (i3 - 1) * ns
        qr_sumqr(icol) = 1.0
      end do
      ! σ over p [qr_sumnp]
      qr_sumnp = 1.0
      do i1 = 1, ns
        !               i1 == i2
        qr_sumnp(i1, i1) = 0.5
      end do
      !
      ! write kgrid & kernel into qs_cfg
      write(*, *) '[w] writing |', trim(qs_cfg), '| ...'
      open(51, file=trim(qs_cfg), form='unformatted', convert=file_endian, &
           access='stream', status='replace')
      !
      ! it is not necessary to store `ns` since `ns = nk * nth`
      write(51) nk, nth, sig, th                  ! (f, θ) grid
      write(51) qr_depth, qr_oml, qi_disc,        &
           qi_kev, qi_interp                 ! parameters
      write(51) qr_om, qr_dk
      write(51) qi_nnz
      write(51) qi_nn, qi_pp, qi_qq, qi_rr
      write(51) qr_tkern, qr_tkurt, qr_dom
      write(51) qi_iccos, qi_ircsr
      write(51) qr_sumqr, qr_sumnp
      !
      if (qi_interp .eq. 1) write(51) qi_bind, qr_bwgh
      if ( (qi_interp .eq. 1) .and. (qi_bound .eq. 1) ) &
           write(51) qr_bdry
      close(51)
      ! screen test
      !
    else if (trim(act) == 'read') then
      write(*, *) '⊚ → [r] reading |', trim(qs_cfg), '| ...'
      open(51, file=trim(qs_cfg), form='unformatted', convert=file_endian, &
           access='stream', status='old')
      ! nk, nth, sig, th can be skipped by using pos
      rpos = 1_8 + qi_lrb * (2_8 + nk + nth)
      read(51, pos=rpos) qr_depth, qr_oml, qi_disc,   &
           qi_kev, qi_interp
      !
      ! read ω & δ\vec{k}
      read(51) qr_om, qr_dk
      ! read total # of quartets
      read(51) qi_nnz
      write(*, *) "⊚ → [r] the total number of quartets is ", qi_nnz
      write(*, *)
      ! allocate arrays
      if (allocated(qi_nn))     deallocate(qi_nn);    allocate(qi_nn(qi_nnz))
      if (allocated(qi_pp))     deallocate(qi_pp);    allocate(qi_pp(qi_nnz))
      if (allocated(qi_qq))     deallocate(qi_qq);    allocate(qi_qq(qi_nnz))
      if (allocated(qi_rr))     deallocate(qi_rr);    allocate(qi_rr(qi_nnz))
      !
      if (allocated(qr_tkern))  deallocate(qr_tkern); allocate(qr_tkern(qi_nnz))
      if (allocated(qr_tkurt))  deallocate(qr_tkurt); allocate(qr_tkurt(qi_nnz))
      if (allocated(qr_dom))    deallocate(qr_dom);   allocate(qr_dom(qi_nnz))
      !
      if (allocated(qi_iccos))  deallocate(qi_iccos); allocate(qi_iccos(qi_nnz))
      !
      read(51) qi_nn, qi_pp, qi_qq, qi_rr
      read(51) qr_tkern, qr_tkurt, qr_dom
      read(51) qi_iccos, qi_ircsr
      read(51) qr_sumqr, qr_sumnp
      !
      if (qi_interp .eq. 1) then
        if (allocated(qi_bind)) deallocate(qi_bind); allocate(qi_bind(4, qi_nnz))
        if (allocated(qr_bwgh)) deallocate(qr_bwgh); allocate(qr_bwgh(4, qi_nnz))
        read(51) qi_bind, qr_bwgh
        !
        if (qi_bound .eq. 1) then
          if (allocated(qr_bdry)) deallocate(qr_bdry); allocate(qr_bdry(qi_nnz))
          read(51) qr_bdry
        end if
        !
      end if
      !
      close(51)
      ! screen test
    end if
    !/
  end subroutine prepkernelio
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine biinterpwt(nk, nth, wn, th)
    !/
    !/    19-apr-2019 : origination                         ( q. liu      )
    !/    19-apr-2019 : extracted from a few subrs. of mod_xnl4v5.f90
    !/                                                      ( q. liu      )
    !/    01-apr-2020 : boundary conditions                 ( q. liu      )
    !/
    ! 1. purpose:
    !    calculate weights for the bilinear interpolation.
    !
    ! 2. method:
    !    see also fig. 9 of van vledder (2006, ce) and mod_xnl4v5.f90 (wrt).
    !    [q_t13v4,  q_weight, q_makegrid
    !/
    implicit none
    !
    integer, intent(in) :: nk
    integer, intent(in) :: nth
    real, intent(in)    :: wn(nk)  ! k
    real, intent(in)    :: th(nth) ! θ
    !
    integer             :: iq, jku, jk4, jk4p, jth4t, jth4, jth4p
    real                :: dth, aref, k4t, angr, &
         r_jk, r_jth, delk, w_k4, w_th4
    real                :: kmin, kmax, k4r
    real                :: qr_kpow
    !
    ! initialization
    qi_bind = 0
    qr_bwgh = 0.
    if (qi_bound .eq. 1) qr_bdry = 1.
    !
    ! get power law for f(k) from qi_fpow for e(f)
    !     e(f) df = f(k) dk →  f(k) ~ f^n * cg = k^{(n-1)/2}
    !     n(k) = f(k) / ω ~ k^{n/2-1}
    !     c(k) = n(k) / k ~ k^{n/2-1 -1}
    qr_kpow = qr_fpow / 2. - 2.
    ! kmin & kmax
    kmin = minval(wn)
    kmax = maxval(wn)
    !
    ! in general, th(nth) in [0, 2π). note however, it is not the case when
    ! the first directional bin defined in ww3_grid.inp (rth0) is not zero.
    dth     = qr_tpi / real(nth)
    aref    = th(1)
    !
    ! qr_k4x(nnz), qr_k4y(nnz), wn(nk) are already available for use
    do iq = 1, qi_nnz
      k4r = sqrt(qr_k4x(iq)**2. + qr_k4y(iq)**2.)  ! k₄
      angr= atan2(qr_k4y(iq), qr_k4x(iq))          ! θ₄ [-π, π]
      ! boundary
      if (qi_bound .eq. 1) then
        k4t = max(kmin, min(k4r, kmax))
      else
        k4t = k4r ! already bounded in [kmin, kmax]
      end if
      !
      ! layout of surrouding four (f, θ) grid points
      !
      !          (θ)↑
      !             ↑ ₄             ₃
      !       jth4+1 ▪ ----------- ▪
      !              |             |
      !              |      r_jth) |
      !     w-       |---✗ (r_jk,  |
      !     t|       |   |         |
      !     h|       |₁  |         |₂
      !     4-  jth4 ▪ ----------- ▪  → → (k)
      !             jk4            jk4+1
      !              |---|
      !               wk4
      !
      ! i) θ index (counted counterclockwisely)
      r_jth = (angr - aref) / dth + 1.
      jth4t = floor(r_jth)              ! 'll be revised later
      w_th4 = r_jth - real(jth4t)       ! dirc. weight
      !
      jth4  = mod(jth4t-1+nth, nth) + 1 ! wrap around 2π
      jth4p = mod(jth4t+nth, nth) + 1
      !
      ! ii) k index (counted in an ascending order). note, as required in
      ! findquartetconfig, k4t >= kmin & k4t <= kmax are already satisfied.
      ! thus, the resulted jku will be in [1, nk].
      ! two special cases:
      !           /  1,  k4t = kmin
      !     jku = |
      !           \ nk,  k4t = kmax or k4t in (wn(nk-1), kmax)
      !
      jku = 1
      do while (k4t > wn(jku))
        jku  = jku + 1
        if (jku > nk) exit ! impossible in our case
      end do
      !
      if (jku .eq. 1) then           ! k4t = kmin
        r_jk = 1.
      else                           ! k4t in (kmin, kmax]
        delk = wn(jku) - wn(jku-1) ! δk
        r_jk = real(jku - 1.) + (k4t - wn(jku-1)) / delk
      end if
      ! parse r_jk
      jk4   = floor(r_jk)            ! in [1, nk]
      w_k4  = r_jk  - real(jk4)
      jk4p  = min(jk4+1, nk)         ! k4t = kmax ← min func.
      !
      ! store indices (in 1d vector; jkth = ith + (ik-1) * nth)
      qi_bind(1, iq) = jth4  + (jk4  - 1) * nth
      qi_bind(2, iq) = jth4  + (jk4p - 1) * nth
      qi_bind(3, iq) = jth4p + (jk4p - 1) * nth
      qi_bind(4, iq) = jth4p + (jk4  - 1) * nth
      !
      ! store weights
      qr_bwgh(1, iq) = (1. - w_k4) * (1. - w_th4)
      qr_bwgh(2, iq) = w_k4 * (1. - w_th4)
      qr_bwgh(3, iq) = w_k4 * w_th4
      qr_bwgh(4, iq) = (1. - w_k4) * w_th4
      !
      ! note that the qi_bind & qr_bwgh do not make full sense when
      ! k4 < kmin (k indices are not correct at all) or k4 > kmax (k index = nk)
      ! because we have capped k4t in between kmin and kmax.
      ! but no need to worry about this because
      !     1) c(k) = 0. when k < kmin
      !     2) c(k) = c(nk) * power decay when k > kmax
      !
      if (qi_bound .eq. 1) then
        if (k4r < kmin) then
          qr_bdry(iq) = 0.
        else if (k4r > kmax) then
          qr_bdry(iq) = (k4r/kmax)**qr_kpow
        end if
      end if
      !
    end do
    !/
  end subroutine biinterpwt
  !/
  !/ ------------------------------------------------------------------- /
  !/
  subroutine calcqrsnl(nk, nth, sig, th,         &
       t0, t1, cvk0, cvk1,       &
       inpqr0, snl, dnl, kurt)
    !/
    !/    09-dec-2018 : origination                         ( q. liu      )
    !/    09-dec-2018 : extracted from odin's subr. `calcqrsnl`
    !/                                                      ( q. liu      )
    !/    10-jun-2019 : include janssen's ke properly       ( q. liu      )
    !/    07-jun-2021 : switch off the cal. of kurtosis (!|kt|)
    !/                                                      ( q. liu      )
    !/
    ! 1. purpose:
    !    calculate the nonlinear transfer rates for a given frequency
    !    grid and given action density spectrum c(\vec{k}).
    !
    !    according to j09 and gs13, c(\vec{k}) is given by
    !             /
    !        m0 = | f(\vec{k}) d \vec{k}
    !             /
    !
    !        f(\vec{k}) = ω c(\vec{k}) / g,
    !
    !    whereas the wave action density spectrum used in ww3 is given by
    !        f(\vec{k}) d \vec{k} = f(k, θ)   dk dθ
    !                             = n(k, θ) ω dk dθ
    !
    !    thus, we have
    !        c(\vec{k}) = n * g / k.
    !
    ! 2. method
    !    see gs13 & gb16 for all the details.
    !
    !    ◆ t0, t1 here are time `relative` to the begining time of the
    !      simulaiton, rather than the `absolute` time
    !
    !    ◆ cvk0, cvk1, snl, dnl shoud be organized/stored in the same way as
    !      qr_kx, qr_ky, qr_dk, qr_om
    !/
    implicit none
    !
    integer, intent(in)        :: nk             ! # of frequencies
    integer, intent(in)        :: nth            ! # of directions
    real, intent(in)           :: sig(nk)        ! radian frequency (rad)
    real, intent(in)           :: th(nth)        ! θ (rad) [equally spaced,
    ! but may start from non-zero
    !
    real, intent(inout)        :: t0             ! previous time step
    real, intent(inout)        :: cvk0(nk*nth)   ! action density @ t0
    complex, intent(inout)     :: inpqr0(:)      ! i(t) @ t0
    !
    real, intent(in)           :: t1             ! current  time step
    real, intent(in)           :: cvk1(nk*nth)   ! action density @ t1
    ! ... c(\vec{k})
    !
    real, intent(out)          :: snl(nk*nth)    ! snl = dc/dt
    real, intent(out)          :: dnl(nk*nth)    ! dnl
    real, intent(out)          :: kurt           ! kurtosis
    !
    ! local parameters
    !
    real                       :: delt           ! δt
    logical, save              :: flread = .true.
    integer                    :: num_i, ns
    real                       :: dvk0(nk*nth),& ! odin's discrete cvk @ t0
         dvk1(nk*nth)   ! ...     @ t1
    real, allocatable, save    :: cvk0_r(:),   & ! c₄      @ t0
         cvk1_r(:)      !         @ t1
    real, allocatable, save    :: fnpqr0(:),   & ! c prod. @ t0
         fnpqr1(:),   & ! c prod. @ t1
         mnpqr (:)      ! δc_1/δt * δk_1
    complex, allocatable, save :: inpqr1(:),   & ! i(t) @ t1
         etau(:),     & ! exp(iδωt)
         edelt(:)       ! exp(iδωδt)
    !
    real, allocatable, save    :: mnp1d(:), mnp2d(:, :)
    real                       :: secm2          ! second-order moment²
    !/
    !
    ! initilization
    ns      = nk * nth
    qi_nrsm = ns * ns
    !
    ! only constant depth is allowed now. accordingly, we only need a single
    ! binary config file which provides the wavenumber grid and kernel
    ! coefficients.
    !
    ! read quartets & kernel coefficients in
    if (flread) then
      ! only read data once
      call prepkernelio(nk, nth, sig, th, 'read')
      flread = .false.
      !           write(*, *) "⊚ → [r] flag for reading kernels becomes |", flread, "|"
      ! allocate arrays
      ! ✓ a variable with the save attribute retains its value and definition,
      ! association, and `allocation` status on exit from a procedure
      !
      if (allocated(fnpqr0)) deallocate(fnpqr0); allocate(fnpqr0(qi_nnz))
      if (allocated(fnpqr1)) deallocate(fnpqr1); allocate(fnpqr1(qi_nnz))
      if (allocated(etau  )) deallocate(etau  ); allocate(etau  (qi_nnz))
      if (allocated(edelt )) deallocate(edelt ); allocate(edelt (qi_nnz))
      if (allocated(mnpqr )) deallocate(mnpqr ); allocate(mnpqr (qi_nnz))
      if (allocated(inpqr1)) deallocate(inpqr1); allocate(inpqr1(qi_nnz))
      !
      if (allocated(mnp1d))  deallocate(mnp1d);  allocate(mnp1d(qi_nrsm))
      if (allocated(mnp2d))  deallocate(mnp2d);  allocate(mnp2d(ns, ns))
      !
      if (qi_disc .eq. 0) then
        if (allocated(cvk0_r)) deallocate(cvk0_r); allocate(cvk0_r(qi_nnz))
        if (allocated(cvk1_r)) deallocate(cvk1_r); allocate(cvk1_r(qi_nnz))
      end if
      !
    end if
    !
    ! screen output (check whether the kernel data are stored in memory)
    !
    num_i = size(inpqr0)
    if (num_i .ne. qi_nnz) then
      write(*, 1001) 'calcqrsnl'
      call exit(1)
    end if
    !
    ! start to calc. snl term
    if (qi_disc == 0) then
      ! define δc = dc/dt * δk δt, we have δc₁ = δc₂ = -δc₃ = -δc₄ (δt can be
      ! removed by taking the unit time)
      !
      ! cvk0/1_r (bilinear interp. or nearest bin)
      if (qi_interp .eq. 0) then
        cvk0_r = cvk0(qi_rr)
        cvk1_r = cvk1(qi_rr)
        !
      else if (qi_interp .eq. 1) then
        cvk0_r = qr_bwgh(1, :) * cvk0(qi_bind(1, :)) + &
             qr_bwgh(2, :) * cvk0(qi_bind(2, :)) + &
             qr_bwgh(3, :) * cvk0(qi_bind(3, :)) + &
             qr_bwgh(4, :) * cvk0(qi_bind(4, :))
        !
        cvk1_r = qr_bwgh(1, :) * cvk1(qi_bind(1, :)) + &
             qr_bwgh(2, :) * cvk1(qi_bind(2, :)) + &
             qr_bwgh(3, :) * cvk1(qi_bind(3, :)) + &
             qr_bwgh(4, :) * cvk1(qi_bind(4, :))
        !
        if (qi_bound .eq. 1) then
          cvk0_r = cvk0_r * qr_bdry
          cvk1_r = cvk1_r * qr_bdry
        end if
        !
      end if
      !
      ! f = [c₃ c₄ (c₁ + c₂) - c₁ c₂ (c₃ + c₄)] dk₂ dk₃ dk₄ ∙ dk₁
      ! dk₄ vanishes with the δ function
      fnpqr0 = (cvk0(qi_qq) * cvk0_r      * (  &
           cvk0(qi_nn) + cvk0(qi_pp) ) -  &
           cvk0(qi_nn) * cvk0(qi_pp) * (  &
           cvk0(qi_qq) + cvk0_r      )) * &
           qr_dk(qi_nn) * qr_dk(qi_pp) * qr_dk(qi_qq)
      !
      fnpqr1 = (cvk1(qi_qq) * cvk1_r      * (  &
           cvk1(qi_nn) + cvk1(qi_pp) ) -  &
           cvk1(qi_nn) * cvk1(qi_pp) * (  &
           cvk1(qi_qq) + cvk1_r      )) * &
           qr_dk(qi_nn) * qr_dk(qi_pp) * qr_dk(qi_qq)
      !
    else if (qi_disc == 1) then
      ! used in gs13 & gb16
      ! f = [c₃dk₃ c₄dk₄ (c₁dk₁ + c₂dk₂) - c₁dk₁ c₂dk₂ (c₃dk₃ + c₄dk₄)]
      ! it seems the bilinear interpolation for this discretization approach
      ! is not very meaningful.
      dvk0   = cvk0 * qr_dk
      fnpqr0 = dvk0(qi_qq) * dvk0(qi_rr) * ( &
           dvk0(qi_nn) + dvk0(qi_pp) ) - &
           dvk0(qi_nn) * dvk0(qi_pp) * ( &
           dvk0(qi_qq) + dvk0(qi_rr) )
      !
      dvk1   = cvk1 * qr_dk
      fnpqr1 = dvk1(qi_qq) * dvk1(qi_rr) * ( &
           dvk1(qi_nn) + dvk1(qi_pp) ) - &
           dvk1(qi_nn) * dvk1(qi_pp) * ( &
           dvk1(qi_qq) + dvk1(qi_rr) )
      !
    end if
    !|kt|! calc m2 for kurtosis estimation ((2.6) of annekov & shrira (2013))
    !|kt|        secm2 = sum(cvk1 * qr_om * qr_dk) ** 2.
    !
    !       write(*, *) '.... input args: t0, t1 :', t0, t1
    if (abs(t1) < qr_eps) then
      ! t1 = 0.0 [essentially i₁ = 0 → i₀ = 0]
      t0     = 0.0
      cvk0   = cvk1
      inpqr0 = (0.0, 0.0)  ! \int_{0}^{0} dt  = 0
      snl    = 0.0
      dnl    = 0.0
      kurt   = 0.0
    else
      ! t1 ≠ 0.0
      delt   = t1 - t0
      if (delt < 0.0) then
        write(*, 1002) 'calcqrsnl'
        call exit(2)
      end if
      etau   = exp(qc_iu * cmplx(qr_dom * t1))       ! exp(iδωt)
      edelt  = exp(qc_iu * cmplx(qr_dom * delt))     ! exp(iδωδt)
      !
      ! ◆ calc. i₁: note here i₁ = i(t₁) dk₁ dk₂ dk₃ for both qi_disc = 0/1
      if (qi_kev .eq. 0) then
        ! gke from gs13, gb16
        inpqr1 = inpqr0 + cmplx(0.5 * delt) *  &
             conjg(etau)                *  &       ! exp(-iδωt)
             (cmplx(fnpqr0) * edelt + cmplx(fnpqr1))
      else if (qi_kev .eq. 1) then
        ! ke from j03 (fnpqr1 is taken outside the time integral; fnpqr0 is not
        ! used in this case; and the real part of inpqr1 is sin(δωt)/δω, and
        ! the imaginary part is [1 - cos(δωt)] / δω
        ! approximation used before
        !               inpqr1 = inpqr0 + cmplx(0.5 * delt) *  &
        !                        conjg(etau) * (edelt + 1)
        !
        where (abs(qr_dom) < qr_eps)
          ! δω = 0., sin(δωt)/δω ~ t, [1 - cos(δωt)] / δω ~ 0
          inpqr1 = cmplx(t1, 0.)
        elsewhere
          ! δω ≠ 0., cacl. sin(δωt)/δω & [1 - cos(δωt)] / δω directly
          ! todo: the sign of cos is not clear yet.
          inpqr1 = cmplx(sin(qr_dom * t1) / qr_dom,     &
               (1 - cos(qr_dom * t1)) / qr_dom)
        end where
      end if
      ! ◆ snl [tranfer integal]
      if (qi_kev .eq. 0) then
        ! gke from gs13, gb16
        mnpqr  = 4.0 * (qr_tkern ** 2.) * real(etau * inpqr1)
      else if (qi_kev .eq. 1) then
        ! ke from j03
        !               mnpqr  = 4.0 * (qr_tkern ** 2.) * fnpqr1 * real(etau * inpqr1)
        mnpqr  = 4.0 * (qr_tkern ** 2.) * fnpqr1 * real(inpqr1)
      end if
      ! calc. σ over q, r [mnpqr is a upper triangular sparse matrix]
      ! dn₁/dt = - dn₃/dt →  anti-symmetric array operation
      ! mnp1d  = (mnpqr - mnpqr^{t}) × s_{qr}
      call asymsmattimvec(qi_nrsm, mnpqr, qi_iccos, qi_ircsr, qr_sumqr, mnp1d, -1.0)
      ! calc. σ over p [mnp2d is a upper triangular matrix]
      ! dn₁/dt = dn₂/dt →  symmetric array operation
      ! snl    = {σ (mnp + mnp^{t}) ⊙ s_{p}} / d\vec{k₁}
      mnp2d  = reshape(mnp1d, (/ns, ns/))
      snl    = sum((mnp2d + transpose(mnp2d)) * qr_sumnp, 2) / qr_dk
      ! ◆ conservation check
      !
      ! ◆ dnl [diagonal term]  <todo>
      !   i) it is easy to calculate dnl for janssen's ke (but we may
      !      have to abandon the sparse array approach)
      !  ii) it is challenging to get dnl for gke.
      dnl  = 0.0
      kurt = 0.0
      !
      !|kt|! ◆ kurtosis
      !|kt|            if (qi_kev .eq. 0) then
      !|kt|! gke from gs13, gb16
      !|kt|                mnpqr  = -3.0 / secm2 * qr_tkurt * aimag(etau * inpqr1)
      !|kt|            else if (qi_kev .eq. 1) then
      !|kt|! ke from j03 (here the imaginary part becomes [1 - cos(δωt)] / δω
      !|kt|!               mnpqr  = -3.0 / secm2 * qr_tkurt * fnpqr1 * aimag(etau * inpqr1)
      !|kt|                mnpqr  = -3.0 / secm2 * qr_tkurt * fnpqr1 * aimag(inpqr1)
      !|kt|            end if
      !|kt|! calc. σ over q, r [mnpqr is a upper triangular sparse matrix]
      !|kt|! symmetric array operation mnp1d  = (mnpqr - mnpqr^{t}) × s_{qr}
      !|kt|            call asymsmattimvec(qi_nrsm, mnpqr, qi_iccos, qi_ircsr, qr_sumqr, mnp1d, 1.0)
      !|kt|            mnp2d  = reshape(mnp1d, (/ns, ns/))
      !|kt|            kurt   = sum((mnp2d + transpose(mnp2d)) * qr_sumnp)
      !
      ! i₁ → i₀ for next computation (time step)
      t0     = t1
      cvk0   = cvk1
      inpqr0 = inpqr1
    end if
    !
    !       write(*, *) '.... output args: t0, t1 :', t0, t1
    !
    ! formats
1001 format(/' *** gke error in gkemodule : '/  &
         '     subr. ', a, ': the stored total number of quartets &
         & and the size of inpqr0 do not match !'/)
1002 format(/' *** gke error in gkemodule : '/  &
         '     subr. ', a, ': t0 ≤ t1 is not satisfied !'/)
    !/
  end subroutine calcqrsnl
  !/
  !/ ------------------------------------------------------------------- /
end module w3gkemd
!/ ------------------------------------------------------------------- /
