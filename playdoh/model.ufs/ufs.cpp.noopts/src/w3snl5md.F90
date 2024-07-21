!> @file
!> @brief interface module for gke (resonant & quasi-resonant four-wave
!>        interactions).
!>
!> @author o. gramstad
!> @author q. liu
!> @date   07-jun-2021
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
!> @brief interface module for gke (resonant & quasi-resonant four-wave
!>        interactions).
!>
!> @author o. gramstad
!> @author q. liu
!> @date   07-jun-2021
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3snl5md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           o. gramstad             |
  !/                  |           q. liu                  |
  !/                  |                        fortran 90 |
  !/                  | last update :         07-jun-2021 |
  !/                  +-----------------------------------+
  !/
  !/    24-sep-2013 : origination.                        ( version 3.14 )
  !/    24-sep-2013 : gke for the regular wavenumbergrid  ( o. gramstad  )
  !/                  (interpolation required)
  !/    02-dec-2013 : gke for ww3 logarithmic freq. grid  ( o. gramstad  )
  !/                  (single grid point)
  !/    27-feb-2019 : gke for 2d applications.            ( version 7.13 )
  !/                                                      ( q. liu )
  !/    07-06-2021  : merge into ww3 github               ( version 7.13 )
  !/                                                      ( q. liu       )
  !/
  !  1. purpose :
  !     interface module for gke (resonant & quasi-resonant four-wave
  !     interactions)
  !
  !  2. variables and types :
  !
  !  3. subroutines and functions :
  !
  !      name         type  scope    description
  !     -------------------------------------------------------------------
  !      w3snl5       subr. public   interface to gkemodule
  !      insnl5       subr. public   initialization routine
  !      calc_wbtv2   subr. private  calc. dominant wave breaking prob.
  !      inpout       subr. private  point output
  !     -------------------------------------------------------------------
  !
  !  4. future work: dnl
  !/
  !/ ------------------------------------------------------------------- /
  implicit none
  !/
  ! subrs.
  public      :: w3snl5, insnl5
  private     :: calc_wbtv2, inpout
  ! vars.
  private     :: nsel, psea, pnms
  !/ ------------------------------------------------------------------- /
  ! parameter list
  integer                              :: nsel
  integer, allocatable, save           :: psea(:)
  character(len=10), allocatable, save :: pnms(:)
  !
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief interface to calcqrsnl subroutine of the gke module.
  !>
  !> @param[in]  a
  !> @param[in]  cg
  !> @param[in]  wn
  !> @param[in]  fmean
  !> @param[in]  t1abs
  !> @param[in]  u10
  !> @param[in]  udir
  !> @param[in]  jsea
  !> @param[out] s
  !> @param[out] d
  !> @param[out] kurt
  !>
  !> @author o. gramstad
  !> @author q. liu
  !> @date   24-apr-2019
  !>
  subroutine w3snl5(a, cg, wn, fmean, t1abs, u10, udir, jsea, &
       s, d, kurt)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           o. gramstad             |
    !/                  |           q. liu                  |
    !/                  |                        fortran 90 |
    !/                  | last update :         24-apr-2019 |
    !/                  +-----------------------------------+
    !/
    !/    24-sep-2013 : origination.                        ( version 3.14 )
    !/    24-sep-2013 : gke for resonant & quasi-resonant four-wave
    !/                  interactions                        ( o. gramstad  )
    !/    27-feb-2019 : full implementation of gke          ( version 7.13 )
    !/                                                      ( q. liu       )
    !/    21-apr-2019 : phase mixing option                 ( version 7.13 )
    !/                                                      ( q. liu       )
    !/    24-apr-2019 : phase mixing option (b_t)           ( version 7.13 )
    !/                                                      ( q. liu       )
    !/    02-may-2019 : organize screen output & disable binary output
    !/                                                      ( version 7.13 )
    !/                                                      ( q. liu       )
    !/
    !/
    !  1. purpose :
    !
    !     interface to calcqrsnl subr. of the gke module. please refer to
    !     -------------
    !     gkemodule.f90 for further details.
    !     -------------
    !
    !     ◆ different times used in this module
    !
    !     |----o---------o----o--|-|--o-----o------o-----o------o----> (t)
    !     ^    ^         ^       ^ ^ t1abs (absol. current time step)¹
    !     |    |         |       |
    !     |    |         |       v t0 (relat. time, previous time step)
    !     |    |<------->|
    !     |    | pm_ival (phase mixing interval, relat. time)
    !     |    |
    !     |    v pm_prev (phase mixing, appear quasi-periodically)
    !     |              (relat. time)
    !     |
    !     v tbeg (absol. begining time, defined by ww3_shel.inp)
    !
    !     ¹ because of using the dynamic integration scheme, t1abs
    !       is related to, but not the same as, time in w3wdatmd.ftn
    !  2. method :
    !
    !  3. parameters :
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
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants, only: grav, tpi
    use w3gkemd,   only: calcqrsnl, qr_depth
    use w3gdatmd,  only: nk, nth, nspec, sig, th,            &
         gtype, rlgtype, clgtype,            &
         qr5dpt, qi5nnz, qi5pmx
    use w3wdatmd,  only: qi5tbeg, qr5tim0, qr5cvk0, qc5int0, &
         qr5tmix
    use w3odatmd,  only: flout, nopts, tosnl5, tolast, &
         iaproc, napout, screen
    use w3parall,  only: init_get_isea
    use w3timemd,  only: dsec21
    use w3servmd,  only: extcde
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: a(nth, nk)         ! n(θ, k)
    real, intent(in)        :: cg(nk)             ! cg(k)
    real, intent(in)        :: wn(nk)             ! wn(k)
    real, intent(in)        :: fmean              ! 1/t_{0, -1}
    integer, intent(in)     :: t1abs(2)           ! absol. t1
    real, intent(in)        :: u10                ! wind velocity
    real, intent(in)        :: udir               ! φ (in rad)
    integer, intent(in)     :: jsea               ! local sea point count
    real, intent(out)       :: s(nth,nk),      &  ! snl
         d(nth,nk),      &  ! dnl
         kurt               ! kurtosis
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    real, parameter         :: btlow = 10., bthgh = 500.
    real                    :: t0rel, t1rel, tdel1, tdel2
    real                    :: cvk1(nspec), snl(nspec), dnl(nspec)
    real                    :: cvk0(nspec)
    complex                 :: inpqr0(qi5nnz)
    integer                 :: ik, ith, ispec, isea, jloc
    integer, allocatable    :: pdiff(:)
    logical, save           :: fstout = .true.
    real                    :: factor(nk), a2(nk, nth), s2(nk, nth)
    real                    :: pm_prev, pm_ival, pm_delt
    real                    :: wbt, btinv
    integer                 :: iunt
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !/ ------------------------------------------------------------------- /
    ! read in wave info. @ the previous time step t0
    ! array initialization is done in w3wdat/w3setw (called by w3initmd)
    t0rel  = qr5tim0(jsea)              ! t0 (nsea)
    cvk0   = qr5cvk0(:, jsea)           ! cvk (ns, nsea) @ t0
    inpqr0 = qc5int0(:, jsea)           ! inpqr (nnz, nsea) @ t0
    !
    ! calc. relative time for t1abs (qi5tbeg as the reference)
    t1rel  = dsec21(qi5tbeg, t1abs)     ! in second
    !
    ! w3wavemd: if ( it.eq.0 ) dtg = 1 → t1rel = -1 (the first step of
    ! integration; time = tcalc/tofrst, dtg = 0 →  1, qi5tbeg = time - 1)
    if(t1rel < 0.) t1rel = 0.
    !
    ! three options for phase mixing
    if (qi5pmx .eq. 0) then
      ! 1) 0: no phase mixing
    else
      !
      ! calc. phase mixing interval
      if (qi5pmx .gt. 0) then
        ! 2) n: mix phase by every n characteristic wave periods
        if (abs(fmean) < 1e-7) then      ! fmean may be 0.
          pm_ival = real(qi5pmx) * 1.  ! then, assume fmean = 1.
        else
          pm_ival = real(qi5pmx) * (1. / fmean)
        end if
        !
      else if (qi5pmx .lt. 0) then
        ! 3) < 0: mix phase based on dominant wave breaking probability bt
        ! calc bt
        wbt   = calc_wbtv2(a, cg, wn, qr5dpt, u10, udir) ! [0, 1.]
        ! mix phase by every 1/bt periods
        ! odin used bt < 1/15. (0.066) → btlow = 15 and pm_ival > 150 s
        btinv = max(btlow, min(1./max(1e-6, wbt), bthgh))
        if (abs(fmean) < 1e-7) then      ! fmean may be 0.
          pm_ival = btinv * 1.  ! then, assume fmean = 1.
        else
          pm_ival = btinv * (1. / fmean)
        end if
      end if
      !
      ! previous phase mixing time (relat. to tbeg)
      ! qr5tmix has already been initialized in w3wdatmd as zero.
      pm_prev = qr5tmix(jsea)
      ! update t1 if necessary
      pm_delt = t1rel - pm_prev
      if (pm_delt .ge. pm_ival) then
        qr5tmix(jsea) = t1rel        ! relat. to tbeg → pm_prev
        t1rel         = 0.
      else
        t1rel         = pm_delt
      end if
    end if
    !
    ! calc. cvk1 from a (c(\bm{k}) = g n(k, θ) / k)
    do ik = 1, nk
      do ith = 1, nth
        ispec = ith + (ik-1) * nth
        cvk1(ispec) = a(ith, ik) / wn(ik) * grav
      end do
    end do
    !
    ! calcqrsnl(nk, nth, sig, th, t0, t1, cvk0, cvk1, inpqr0, snl, dnl, kurt)
    ! depth is needed for reading in kernels at the first run
    qr_depth = qr5dpt
    call calcqrsnl(nk, nth, sig(1:nk), th,     &
         t0rel, t1rel, cvk0, cvk1,   &
         inpqr0, snl, dnl, kurt)
    !
    ! tranform back from c(k) to n(k)
    ! todo d(ith, ik) (see nl2 for reference)
    d = 0.0
    do ik = 1, nk
      do ith = 1, nth
        ispec = ith + (ik-1) * nth
        s(ith, ik) = snl(ispec) * wn(ik) / grav
      end do
    end do
    !
    ! store wave info. @ t1 → t0
    qr5tim0(jsea)    = t0rel
    qr5cvk0(:, jsea) = cvk0
    qc5int0(:, jsea) = inpqr0
    !
    ! point output (snl term)
    ! first ouput action (find nearest grid points & generate binary files)
    if (fstout) then
      call inpout
      fstout = .false.
      if (iaproc .eq. napout) then
        write(screen, *)
        write(screen, '(a)')                          &
             ' ⊚ → [ww3 snl₅] point ouptut initialization'
        write(screen, '(a, i4)')                      &
             ' ⊚ → [ww3 snl₅] # of valid points: ', nsel
        write(screen, *)
      end if
    end if
    !
    ! calc factor used for jacobian tranformation from n(k, θ) to e(f, θ)
    factor = tpi / cg * sig(1:nk)
    !
    ! regular grid & curvilinear grid
    if ( ((gtype .eq. rlgtype) .or. (gtype .eq. clgtype)) &
         .and. flout(2) .and. nsel .gt. 0) then
      tdel1 = dsec21(t1abs, tosnl5)
      tdel2 = dsec21(t1abs, tolast(:, 2)) ! not really useful since
      ! tosnl5 can never catch
      ! tolast
      ! output time
      if (abs(tdel1) < 1e-6 .or. abs(tdel2) < 1e-6) then
        ! jsea→ isea
        call init_get_isea(isea, jsea)
        ! find the loc of isea at psea (nearest sea grid point)
        if (allocated(pdiff)) deallocate(pdiff); allocate(pdiff(nsel))
        pdiff = abs(psea(1:nsel) - isea)
        if (any(pdiff .eq. 0)) then
          jloc = minloc(pdiff, 1)
          ! n(θ, k) →  f(f, θ) & s(θ, k) →  s(f, θ)
          do ith = 1, nth
            a2(:, ith) = a(ith, :) * factor
            s2(:, ith) = s(ith, :) * factor
          end do
          ! nan check
          if (hasnan(nk, nth, a2) .or. hasnan(nk, nth, s2)) then
            if (iaproc .eq. napout)                            &
                 write(screen, *) '★★★ warning: find nan in e(f, θ) &
                 & or snl(f, θ) !'
          end if
          ! unit no.
          iunt = 500 + jloc
          ! store data (binary)
          !                 open(iunt, file='nl5_'//trim(pnms(jloc))//'_src.bin',  &
          !                      form='unformatted', convert=file_endian, access='stream',             &
          !                      status='old', position='append', action='write')
          !                 write(iunt) t1abs
          !                 write(iunt) kurt
          !                 write(iunt) a2
          !                 write(iunt) s2
          !                 close(iunt)
          ! store data (ascii)
          open(iunt, file='nl5_'//trim(pnms(jloc))//'_src.dat', &
               form='formatted', status='old',                  &
               position='append', action='write')
          write(iunt, '(i10.8, i7.6)') t1abs
          write(iunt, '(es11.3)')      kurt
          write(iunt, 113) a2
          write(iunt, 113) s2
          close(iunt)
          !
        end if
      end if
    end if
    ! format
113 format ((10es11.3))
    !/
    !/ end of w3snl5 ----------------------------------------------------- /
    !/
  end subroutine w3snl5
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief initialization for the gke module (prepare wavenumber grid & kernel
  !>        coefficients).
  !>
  !> @author q. liu
  !> @date   27-feb-2019
  !>
  subroutine insnl5
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           q. liu                  |
    !/                  |                        fortran 90 |
    !/                  | last update :         27-feb-2019 |
    !/                  +-----------------------------------+
    !/
    !/    27-feb-2019 : origination.                        ( version 7.13 )
    !/                                                      ( q. liu )
    !/
    !  1. purpose :
    !
    !     initialization for the gke module (prepare wavenumber grid & kernel
    !     coefficients)
    !
    !  2. method :
    !     see subrs. prepkgrid & prepkernelio of gkemodule.f90
    !
    !  3. parameters :
    !
    !  4. subroutines used :
    !     ----------------------------------------------------------------
    !      name          type  module      description
    !     ----------------------------------------------------------------
    !      strace        subr. w3servmd    subroutine tracing.
    !      prepkernelio  subr. gkemodule   kgrid & kernel coeff.
    !
    !  5. called by :
    !     ----------------------------------------------------------------
    !      name          type  module   description
    !     ----------------------------------------------------------------
    !      w3iogr        subr. w3iogrmd model definition file management.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gkemd,  only: qr_depth, qr_oml, qi_disc, qi_kev, qi_nnz,  &
         qi_interp, prepkernelio
    use w3gdatmd, only: nk, nth, sig, th,                           &
         qr5dpt, qr5oml, qi5dis, qi5kev, qi5nnz,     &
         qi5ipl, qi5pmx
    use w3odatmd, only: iaproc, napout, screen
    use w3servmd, only: extcde
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! set important parameters for gke module (qr[i]5dpt/oml/dis/kev are
    ! defined in ww3_grid.inp, and qi5nnz is not known yet)
    qr_depth = qr5dpt
    qr_oml   = qr5oml
    qi_disc  = qi5dis
    qi_kev   = qi5kev
    qi_interp= qi5ipl
    !
    ! prepare (kx, ky) grid & kernel coefficients
    call prepkernelio(nk, nth, sig(1:nk), th, 'write')
    !
    ! store qi_nnz to qi5nnz (which will be used to initialize the
    ! qc5int0 array)
    qi5nnz   = qi_nnz
    !
    ! q. liu (todo)
    if (iaproc .eq. napout) then
      write(screen, '(a, f6.1)') " ⊚ → [ww3 snl₅]: water depth   : ", qr_depth
      write(screen, '(a, f7.2)') " ⊚ → [ww3 snl₅]: ω λc cut off  : ", qr_oml
      write(screen, '(a, i4)'  ) " ⊚ → [ww3 snl₅]: discretiza.   : ", qi_disc
      write(screen, '(a, i4)'  ) " ⊚ → [ww3 snl₅]: gke version   : ", qi_kev
      write(screen, '(a, i12)' ) " ⊚ → [ww3 snl₅]: # of quartets : ", qi_nnz
      write(screen, '(a, i4)'  ) " ⊚ → [ww3 snl₅]: interpol.     : ", qi_interp
      write(screen, '(a, i4)'  ) " ⊚ → [ww3 snl₅]: phase mixing  : ", qi5pmx
    end if
    !/
    !/ end of insnl5 ----------------------------------------------------- /
    !/
  end subroutine insnl5
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief estimate the dominant wave breaking probability.
  !>
  !> @details based on the empirical parameterization proposed by
  !>  babanin et al. (2001).
  !>
  !> @param   a
  !> @param   cg
  !> @param   wn
  !> @param   dpt
  !> @param   u10
  !> @param   udir
  !> @returns calc_wbtv2
  !>
  !> @author q. liu
  !> @date   24-apr-2019
  !>
  function calc_wbtv2 (a, cg, wn, dpt, u10, udir)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           q. liu                  |
    !/                  |                        fortran 90 |
    !/                  | last update :         24-apr-2019 |
    !/                  +-----------------------------------+
    !/
    !/    24-aug-2018 : origination. (w3iogomd.ftn)         ( version 6.06 )
    !/                  used for output parameter b_t       ( q. liu       )
    !/
    !/    24-apr-2019 : simplified for nl5                  ( version 7.13 )
    !/                                                      ( q. liu       )
    !/
    !  1. purpose :
    !
    !     estimate the dominant wave breaking probability b_t based on
    !     the empirical parameterization proposed by babanin et al. (2001).
    !     from their fig. 12, we have
    !
    !         b_t = 85.1 * [(εp - 0.055) * (1 + h_s/d)]^2.33,
    !
    !     where ε is the significant steepness of the spectral peak, h_s is
    !     the significant wave height, d is the water depth.
    !
    !     for more details, please see
    !         banner et al.  2000: jpo,      30,  3145 -  3160.
    !         babanin et al. 2001: jgr, 106(c6), 11569 - 11676.
    !
    !     see subr. calc_wbt in w3iogomd.ftn for more details.
    !
    !/ ------------------------------------------------------------------- /
    use w3dispmd, only: wavnu1
    use w3gdatmd, only: nk, nth, sig, esin, ecos, dth, dsii
    !
    implicit none
    !
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)     :: a(nth, nk)       ! n(θ, k)
    real, intent(in)     :: cg(nk)           ! cg(k)
    real, intent(in)     :: wn(nk)           ! wn(k)
    real, intent(in)     :: dpt              ! water depth
    real, intent(in)     :: u10              ! wind velocity
    real, intent(in)     :: udir             ! wind dirc. (φ in rad)
    real                 :: calc_wbtv2
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !
    real, parameter      :: beta  = 1.2
    !
    integer              :: ik, ith
    real                 :: sinu, cosu, tc, tforce
    real                 :: esig(nk) ! e(σ)
    real                 :: factor, et, hs, etp, hsp, sigp, kp, &
         cgp, wstp, twbt
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! wind info. is required to select wind sea partition from the wave
    ! spectrum.
    !
    ! following janssen et al. (1989) and bidlot (2001), spectral components
    ! are considered to be subject to local wind forcing when
    !
    !          c / [u cos(θ - φ)] < β,
    !
    ! where c is the phase velocity c = σ/k, φ is the wind direction, u is
    ! the wind speed u10, (sometimes approximated by u10≅ 28 * ust), β is
    ! the constant forcing parameter with β∈ [1.0, 2.0]. by default, we use
    ! β = 1.2 (bidlot 2001).
    !
    sinu  = sin(udir)                    ! sinφ
    cosu  = cos(udir)                    ! cosφ
    !
    esig  = 0.                           ! e(σ)
    et    = 0.                           ! σe(σ)δσ
    etp   = 0.                           ! σe(σ)δσ at peak only
    !
    do ik = 1, nk
      tc     = sig(ik) / wn(ik)        ! phase velocity c=σ/k
      factor = sig(ik) / cg(ik)        ! σ / cg
      factor = factor * dth            ! σ / cg * δθ
      !
      do ith = 1, nth
        tforce = tc - u10 * (cosu*ecos(ith)+sinu*esin(ith)) &
             * beta
        if (tforce .lt. 0.) then ! wind sea component
          esig(ik) = esig(ik) + a(ith, ik) * factor
        endif
      enddo ! ith
      !
    enddo ! ik
    !
    ! esig is e(σ) of the wind sea after filtration of any background swell.
    ! now we need to get hs & σp for the wind sea spectrum.
    ! unlike w3iogomd.ftn, the tail energy is not added here.
    et = sum(esig * dsii)
    hs = 4. * sqrt(max(0., et))
    !
    ! get σp from e(σ)
    ! fpopt = 0 in w3iogomd.ftn: fp defined by young (1999, p. 239)
    sigp = sum(esig**4. * sig(1:nk) * dsii) /  &
         max(1e-10, sum(esig**4. * dsii))
    if (abs(sigp) < 1e-7) sigp = sig(nk) ! σp = 0
    !
    ! kp from σp (linear dispersion)
    call wavnu1 (sigp, dpt, kp, cgp)
    !
    !                         { /1.3σp         }1/2
    ! peak wave height hp = 4 { |      e(σ) dσ }
    !                         { /0.7σp         }
    !
    do ik = 1, nk
      if ( (sig(ik) >= 0.7 * sigp) .and. &
           (sig(ik) <= 1.3 * sigp) ) then
        etp = etp + esig(ik) * dsii(ik)
      endif
    enddo ! ik
    hsp  = 4. * sqrt(max(0., etp))
    !
    ! significant steepness of the peak region εp
    !
    wstp = 0.5 * kp * hsp
    !
    ! dominant wave breaking b_t
    !
    twbt = 85.1 * (max(0.0, wstp - 0.055) * (1 + hs/dpt))**2.33
    twbt = min(1.0, twbt)
    !
    calc_wbtv2 = twbt
    return
    !/
    !/ end of  calc_wbtv2 ------------------------------------------------ /
    !/
  end function calc_wbtv2
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief initialization for point output (snl).
  !>
  !> @author q. liu
  !> @date   25-mar-2019
  !>
  subroutine inpout
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           q. liu                  |
    !/                  |                        fortran 90 |
    !/                  | last update :         25-mar-2019 |
    !/                  +-----------------------------------+
    !/
    !/    24-mar-2019 : origination.                        ( version 7.13 )
    !/                                                      ( q. liu       )
    !/    27-apr-2019 : add the ascii option                ( q. liu       )
    !/
    !  1. purpose :
    !
    !     initialization for point output (snl) [see also w3iopp of w3iopomd]
    !
    !  2. method :
    !
    !  3. parameters :
    !
    !  4. subroutines used :
    !
    !  5. called by :
    !     ----------------------------------------------------------------
    !      name          type  module   description
    !     ----------------------------------------------------------------
    !      w3snl5        subr. w3snl5md s_{nl} gke
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/
    !/ ------------------------------------------------------------------- /
    use constants, only: tpi
    use w3gdatmd,  only: nk, nth, sig, th, qr5dpt,         &
         flagll, xgrd, ygrd, mapsta, mapfs
    use w3odatmd,  only: nopts, ptnme, ptloc, iptint,      &
         iaproc, napout, screen
    use w3servmd,  only: dist_sphere
    !/
    implicit none
    !/ ------------------------------------------------------------------- /
    !/
    integer           :: ixs(4), iys(4), ix, iy, ipt, is, &
         jloc, jx, jy, isea, smap(4), iunt
    real              :: plon, plat, xlon, ylat, dist(4)
    !/ ------------------------------------------------------------------- /
    !/
    ! initialize arrays
    if (allocated(psea)) deallocate(psea); allocate(psea(nopts))
    if (allocated(pnms)) deallocate(pnms); allocate(pnms(nopts))
    !
    nsel    = 0
    psea(:) = 0
    pnms(:) = 'null'
    dist(:) = -999.
    !
    do ipt = 1, nopts
      ! get lon & lat of this output point
      plon   = ptloc(1, ipt)
      plat   = ptloc(2, ipt)
      ! get four indices surrounding the output point
      ixs(:) = iptint(1, :, ipt)
      iys(:) = iptint(2, :, ipt)
      do is = 1, 4
        ! get lon & lat of four corner points
        ix   = ixs(is)
        iy   = iys(is)
        xlon = xgrd(iy, ix)
        ylat = ygrd(iy, ix)
        ! grid point status
        if (mapsta(iy, ix) .eq. 0) cycle
        ! calc dist.
        if (flagll) then
          dist(is) = dist_sphere(plon, plat, xlon, ylat)
        else
          dist(is) = sqrt((plon - xlon)**2. + (plat - ylat)**2.)
        end if
      end do
      ! a sea point filter: there must be at least one sea grid point around
      ! the selected output location. [maybe not necessary since iopp already
      ! checked this criterion]
      !
      if (all(dist < 0.)) cycle
      ! find the nearest sea grid point
      jloc = minloc(dist, 1, dist >= 0.)
      jx   = ixs(jloc)
      jy   = iys(jloc)
      isea = mapfs(jy, jx)
      ! basic check
      ! store isea
      nsel = nsel + 1
      psea(nsel) = isea
      pnms(nsel) = ptnme(ipt)
      ! store unit (open & write binary files)
      iunt = 500 + nsel
      ! binary
      !         open(iunt, file='nl5_'//trim(pnms(nsel))//'_src.bin',       &
      !              form='unformatted', convert=file_endian, access='stream', status='replace', &
      !              action='write')
      !         write(iunt) plon, plat
      !         write(iunt) xgrd(jy, jx), ygrd(jy, jx)
      !         write(iunt) qr5dpt
      !         write(iunt) nk, nth
      !         write(iunt) sig(1:nk)/tpi  ! f, θ
      !         write(iunt) th
      !         close(iunt)
      ! ascii
      open(iunt, file='nl5_'//trim(pnms(nsel))//'_src.dat',       &
           form='formatted', status='replace', action='write')
      write(iunt, '(2es11.3)') plon, plat
      write(iunt, '(es11.3)' ) qr5dpt
      write(iunt, '(2i5)')     nk, nth
      write(iunt, 113) sig(1:nk)/tpi  ! f, θ
      write(iunt, 113) th
      close(iunt)
      !
    end do
    ! format
113 format ((10es11.3))
    !
  end subroutine inpout
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief check if the 2d array `arr2d` contains nan.
  !>
  !> @param   nk
  !> @param   nth
  !> @param   arr2d
  !> @returns hasnan
  !>
  !> @author q. liu
  !> @date   25-apr-2019
  !>
  function hasnan(nk, nth, arr2d)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           q. liu                  |
    !/                  |                        fortran 90 |
    !/                  | last update :         25-apr-2019 |
    !/                  +-----------------------------------+
    !/
    !/    24-apr-2019 : origination.                        ( version 7.13 )
    !/                                                      ( q. liu       )
    !/
    !  1. purpose :
    !     check if the 2d array `arr2d` contains nan (see also w3gsrumd.ftn)
    !/
    implicit none
    !
    integer, intent(in)  :: nk, nth        ! # of freq. & dirc.
    real, intent(in)     :: arr2d(nk, nth)
    logical              :: hasnan
    !/
    hasnan = .true.
    !
    if ( all(arr2d .ge. -huge(arr2d(1, 1))) .and.  &
         all(arr2d .le.  huge(arr2d(1, 1))) ) then
      hasnan = .false.
    end if
    !
    return
    !/
  end function hasnan
  !/ ------------------------------------------------------------------- /
  !/
  !/ end of module w3snl5md -------------------------------------------- /
  !/
end module w3snl5md
!/ ------------------------------------------------------------------- /
