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
module w3tidemd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii                     |
  !/                  !            m. foreman, ios        !
  !/                  |                        fortran 90 |
  !/                  | last update :         21-apr-2020 |
  !/                  +-----------------------------------+
  !/
  !/    01-sep-2012 : origination.                        ( version 4.07 )
  !/    04-mar-2013 : correction of fast and new vfast    ( version 4.08 )
  !/    21-apr-2020 : correction of time and implicit none( version 7.13 )
  !/
  !  1. purpose :
  !
  !     tidal analysis of time series for storage of tidal constituents
  !     only. this module is built around the versatile tidal analysis
  !     package : http://www.pac.dfo-mpo.gc.ca/science/oceans/tidal-marees/index-eng.htm
  !     by mike foreman et al.  (see publication in j. ocean atmos. tech.: vol.
  !                http://journals.ametsoc.org/doi/pdf/10.1175/2008jtecho615.1 )
  !     adaptation to wavewatch iii was performed by f. ardhuin
  !
  !     still to be done:
  !     - adding a namelist in ww3_grid to allow adjustment of tide_dt
  !     - check on constituents (m2, s2, n2 ...) when running ww3_shel,
  !        in order to allow use of different sets of constituents
  !     - add residual currents (or geostrophic ...) ...
  !     - make this work with multigrids
  !
  !  2. variables and types :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !     ----------------------------------------------------------------
  !
  !  3. subroutines and functions :
  !
  !      name                  type  scope    description
  !     ----------------------------------------------------------------
  !      tide_write_results    subr. public   writes tidal results
  !                                          (with m. forman's format)
  !      tide_predict          subr. public  predicts tide from amp. & phases
  !      tide_settings_full    subr. public  choice of constituents
  !      tide_settings_fast    subr. public  choice of constituents
  !      tide_settings_vfast   subr. public  choice of constituents
  !      tide_set_indices      subr. public
  !      setvuf_fast           subr. public calculates the v,u,f values
  !      tide_read_settings    subr. public reads data from file (ios format)
  !      tide_read_anapar      subr. public reads data from file (ios format)
  !      tide_read_timeseries  subr. public reads data from file (ios format)
  !      astr                  subr. public calculates the ephermides
  !      juldayt               func. public julian day
  !      caldatt               subr. public
  !      dsvbksb               subr. public
  !      dsvdcmp               subr. public
  !      svd                   subr. public matrix singular value decomposition
  !      vuf_set_parameters    subr. public
  !      vuf                   subr. public
  !      opnvuf                subr. public
  !      setvuf                subr. public
  !      flex_tidana_webpage   subr. public
  !      tide_predict          subr. public tide prediction and error estimate
  !      tide_predict_only     subr. public tide prediction only
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !
  !  6. switches :
  !
  !  7. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  !      public
  !/
  !/ private variables
  !/
  double precision, parameter  :: twpi=3.1415926535898*2.
  double precision, parameter  :: fac=twpi/360.
  !
  !  array sizes
  !
  integer, parameter          :: mc=70,nr=106000,nmaxp1=mc*2,nmaxpm=nr*2+nmaxp1
  integer, parameter          :: mc2=mc*2
  character*5, parameter       :: kblank='     '
  !
  integer                     :: ntidal_con, ntotal_con, nkonco
  character*5,    allocatable :: tidecon_allnames(:)       ! array of names of tidal constituents
  character*5,    allocatable   :: konco_con(:)
  integer, allocatable, private :: ii(:),jj(:),kk(:),ll(:),mm(:),nn(:),nj(:)
  real, allocatable             :: semi(:),coef_con(:)
  real , allocatable            :: v_arg(:,:),f_arg(:,:),u_arg(:,:)
  real                          :: ee(180),ph(180)
  integer                       :: ldel(180),mdel(180),ndel(180),ir(180)
  ! these two index table are used in vuf ...
  integer , allocatable         :: tide_indexj(:),tide_indexjk(:)
  !
  ! parameters for tidal analysis
  !
  integer                      :: tide_mf, tide_nx, tide_ny
  real, allocatable            :: tide_freqc(:)                 ! array of freq. of tidal constituents
  character(len=5), allocatable:: tidecon_namei(:)              ! array of names of tidal constituents
  character(len=5), allocatable:: tidecon_name(:)               ! array of names of tidal constituents
  character(len=5)             :: tide_konan(10), tide_konin(10,10)
  real                         :: tide_r(10,10), tide_zeta(10,10)
  real                         :: tide_sigan(10),tide_sigin(10,10)   ! these two are only read from files and written out
  integer                      :: tide_nin,tide_ninf(10)
  real, allocatable            :: tidal_const(:,:,:,:,:)                 ! array of freq. of tidal constituents
  !
  ! data to be analyzed
  !
  integer(kind=4)              :: tide_nti
  real, allocatable            :: tide_data(:,:)
  integer(kind=4), allocatable :: tide_days(:), tide_secs(:)
  real(kind=8),    allocatable :: tide_hours(:)
  real,            parameter   :: tide_dt = 1800.   ! time step used for forcing
  !
  ! analysis result
  !
  real                         :: tide_ampc(mc,2), tide_phg(mc,2),   &
       tide_sig1(mc,2), tide_sig2(mc,2),  &
       tide_sig3(mc,2), tide_ttest(mc,2)
  real                         :: tide_ampci(10,10,2), tide_phgi(10,10,2)
  integer                      :: tide_index(mc),tide_index2(mc)
  integer                      :: ndset, tide_verbose = 0
  !public  :: tide_mf, tidecon_name
  !/
contains
  !/ ------------------------------------------------------------------- /
  subroutine tide_write_results(lp,filename,ndef,kd1, kd2, itz, xlat,xlon,        &
       res, ssq, rmsr0, sdev0,sdev,rmsr, resmax, imax, rmsrp)
    implicit none
    !
    character*256, intent(in)     :: filename
    integer,       intent(in)     :: lp, ndef, imax(ndef)
    integer(kind=4),intent(in)    :: kd1,kd2
    character*4  , intent(in)     :: itz
    real(kind=8),  intent(in)     :: rmsr0(ndef), &
         sdev0(ndef), sdev(ndef), rmsr(ndef), resmax(ndef), rmsrp(ndef)
    real         , intent(in)     :: res(ndef), ssq(ndef), xlat,xlon
    !
    integer                       :: idef, i, k, k2, l, i1, inftot
    integer                       :: id1,im1,iy1,id2,im2,iy2
    open(unit=lp,file=filename,status='unknown',form='formatted')
    call caldatt(kd1,id1,im1,iy1)
    call caldatt(kd2,id2,im2,iy2)
    write(lp,15) id1,im1,iy1,id2,im2,iy2,itz
15  format(/'the analysis period is from',i3,'/',i2,'/',i4,     &
         ' to ',i2,'/',i2,'/',i4,'  in the time zone ',a4)
    write(lp,*)'using svd to solve the overdetermined system'
    !      write(lp,150)id1,im1,ic1,iy1,id2,im2,ic2,iy2
150 format(2i3,2i2,5x,2i3,2i2)
    write(lp,255)tide_nti
255 format('number of points in the analysis =',i6)
    write(lp,*) ' nin=',tide_nin
    do idef=1,ndef
      write(lp,52) res(idef),ssq(idef)
52    format('largest residual magnitude & residual sum of squares:'     &
           ,2e12.5)
      write(lp,66) sdev0(idef),rmsr0(idef)
66    format(     &
           'st. dev. of right hand sides of original overdetermined system:' &
           ,e12.5/ &
           '                       and the root mean square residual error:' &
           ,e12.5)
      write(lp,*) ' rms residual: brute force =',rmsr(idef)
      write(lp,*) ' max residual: ',resmax(idef),imax(idef)
      write(lp,41)
41    format('harmonic analysis results: amplitudes, phase lags, c, s,      &
           & amp sd estimates, t-test value')
      !      write out results for constant term & linear trend
      do i=1,tide_mf
        write(lp,43) tidecon_name(i),tide_freqc(i),tide_ampc(i,idef),tide_phg(i,idef),     &
             tide_sig1(i,idef),tide_sig2(i,idef),tide_sig3(i,idef),tide_ttest(i,idef)
      end do
43    format(5x,a5,4x,f12.9,2x,f10.5,2x,f10.3,5x,4f8.3)
      !
      !*  inference results are given now
      !
      if (tide_nin.ge.0) then
        write(lp,*) ' inference results'
        l=0
        do k=1,tide_nin
          do i=2,tide_mf
            if (tidecon_name(i).eq.tide_konan(k)) exit
          end do
          i1=i
          do k2=1,tide_ninf(k)
            l=l+1
            write(lp,79) tide_konin(k,k2),tide_sigin(k,k2),tide_ampci(k,k2,idef), &
                 tide_phgi(k,k2,idef)
79          format(5x,a5,4x,f12.9,15x,f10.4,5x,f10.4)
          end do
        end do
        inftot=l
      end if
      write(lp,70)tide_nti,tide_mf*2,'    ',xlat,xlon,sngl(sdev0),sngl(sdev)
70    format('n,m,lat,lon,sdev0,sdev:  ',2i10,a4,f9.4,f10.4,2f10.2)
      !
      if (tide_nin.gt.0) then
        write(lp,71) rmsrp(idef)
71      format('root mean square residual error after inference is',     &
             e15.6, //)
      else
        write(lp,72) rmsrp(idef)
72      format('recalculated root mean square residual error is   ',     &
             e15.6, //)
      endif
    end do
  end subroutine tide_write_results
  !/ ------------------------------------------------------------------- /
  subroutine tide_find_indices_prediction(list,inds,tide_prmf)
    !/                  +-----------------------------------+
    !/                  |           f. ardhuin              |
    !/                  |                        fortran 90 |
    !/                  | last update :         28-feb-2013 |
    !/                  +-----------------------------------+
    !/
    !/    29-jun-2013 : creation                            ( version 4.11 )
    !/
    !  1. purpose :
    !
    !     finds indices of tidal constituents to be used for prediction
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       list         char  i  array of tidal constituents names to be used
    !       inds         i.a.  o  array of indices
    !       tide_prmf    i.a.  o  number of constituents to be used
    !     ----------------------------------------------------------------
    !
    !
    !  4. subroutines used :
    !
    !     none
    !
    !  5. called by :
    !
    !     ww3_prtide
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
    !       !/s  enable subroutine tracing.
    !       !/t  enable test output.
    !
    ! 10. source code :
    !
    use w3odatmd, only: iaproc, naproc, naperr, napout
    use w3odatmd, only: ndse, ndso
    !/ ------------------------------------------------------------------- /
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    character(len=100), intent(in) :: list(70)
    integer, intent(out)    :: inds(70), tide_prmf
    integer j, found
    !
    !
    tide_prmf=0
    if (trim(list(1)).eq.'vfast' .or. trim(list(1)).eq.'fast') then
      do j=1,tide_mf
        inds(j)=j
      end do
      tide_prmf = tide_mf
      return
    end if
    !
    do while (len_trim(list(tide_prmf+1)).ne.0)
      tide_prmf=tide_prmf+1
      found = 0
      do j=1,tide_mf
        if (trim(tidecon_name(j)).eq.trim(list(tide_prmf))) then
          inds(tide_prmf)=j
          found=1
          if (iaproc.eq.napout) write(ndso,'(a,a,e12.2)') 'tidal constituent to be used in pre:', &
               trim(list(tide_prmf)),tide_freqc(j)
        end if
      end do
      if (found.eq.0 .and. iaproc.eq.napout) write(ndso,'(3a)') 'tidal constituent ',trim(list(tide_prmf)), &
           ' not available.'
    end do
    !
  end subroutine tide_find_indices_prediction
  !/ ------------------------------------------------------------------- /
  subroutine tide_find_indices_analysis(list)
    !/                  +-----------------------------------+
    !/                  |           f. ardhuin              |
    !/                  |                        fortran 90 |
    !/                  | last update :         21-apr-2020 |
    !/                  +-----------------------------------+
    !/
    !/    29-jun-2013 : creation                            ( version 4.11 )
    !/    21-apr-2020 : add 5 additional tidal const.       ( version 7.13 )
    !/
    !  1. purpose :
    !
    !     finds indices of tidal constituents to be used for analysis
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       list         char  i  array of tidal constituents names to be used
    !     ----------------------------------------------------------------
    !
    !
    !  4. subroutines used :
    !
    !     none
    !
    !  5. called by :
    !
    !     ww3_prtide
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
    !       !/s  enable subroutine tracing.
    !       !/t  enable test output.
    !
    ! 10. source code :
    !
    use w3odatmd, only: iaproc, naproc, naperr, napout
    use w3odatmd, only: ndse, ndso
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    character(len=100), intent(in) :: list(70)
    !
    integer tide_mf_all
    character(len=5)             :: tidecon_name_all(65)      ! array of names of tidal constituents
    real                         :: tide_freqc_all(65)        ! array of freq. of tidal constituents
    integer                      :: inds(65), j, found, ntides
    !
    !
    tidecon_name_all(:)=(/ &
         'z0   ', 'ssa  ', 'msm  ', 'mm   ', 'msf  ', 'mf   ', 'alp1 ', '2q1  ', 'sig1 ', 'q1   ',  &
         'rho1 ', 'o1   ', 'tau1 ', 'bet1 ', 'no1  ', 'chi1 ', 'p1   ', 'k1   ', 'phi1 ', 'the1 ',  &
         'j1   ', 'so1  ', 'oo1  ', 'ups1 ', 'oq2  ', 'eps2 ', '2n2  ', 'mu2  ', 'n2   ', 'nu2  ',  &
         'm2   ', 'mks2 ', 'lda2 ', 'l2   ', 's2   ', 'k2   ', 'msn2 ', 'eta2 ', 'mo3  ', 'm3   ',  &
         'so3  ', 'mk3  ', 'sk3  ', 'mn4  ', 'm4   ', 'sn4  ', 'ms4  ', 'mk4  ', 's4   ', 'sk4  ',  &
         '2mk5 ', '2sk5 ', '2mn6 ', 'm6   ', '2ms6 ', '2mk6 ', '2sm6 ', 'msk6 ', '3mk7 ', 'm8   ',  &
         'n4   ', 'r2   ', 's1   ', 'sa   ', 't2   ' /)
    !
    tide_freqc_all(:)=(/0.0000000000, 0.0002281591, 0.0013097807, 0.0015121520, 0.0028219327, 0.0030500918, &
         0.0343965698, 0.0357063505, 0.0359087218, 0.0372185025, 0.0374208738, &
         0.0387306544, 0.0389588136, 0.0400404351, 0.0402685943, 0.0404709655, &
         0.0415525871, 0.0417807462, 0.0420089053, 0.0430905269, 0.0432928982, &
         0.0446026789, 0.0448308380, 0.0463429900, 0.0759749448, 0.0761773160, &
         0.0774870967, 0.0776894680, 0.0789992487, 0.0792016200, 0.0805114007, &
         0.0807395598, 0.0818211814, 0.0820235526, 0.0833333333, 0.0835614924, &
         0.0848454853, 0.0850736444, 0.1192420551, 0.1207671010, 0.1220639878, &
         0.1222921469, 0.1251140796, 0.1595106494, 0.1610228013, 0.1623325820, &
         0.1638447340, 0.1640728931, 0.1666666667, 0.1668948258, 0.2028035476, &
         0.2084474129, 0.2400220500, 0.2415342020, 0.2443561347, 0.2445842938, &
         0.2471780673, 0.2474062264, 0.2833149482, 0.3220456027, &
         0.157998497 , 0.083447407 , 0.041666667 , 0.000114080 , 0.083219259 /)
    inds(:) = 0
    !
    if (trim(list(1)).eq.'fast') then
      tide_mf = 44
      inds(1:44)= (/ 1, 2, 3, 4, 5, 6, 12, 17, 18, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36,  &
           37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,&
           55, 56, 57, 58, 59, 60 /)
      !
    else if (trim(list(1)).eq.'vfast') then
      tide_mf = 20
      inds(1:20)= (/ 1, 2, 3, 5, 6, 27, 28, 29, 30, 31, 35, 36, 37, 44, 45, 47, 49, 54, 55, 60 /)
      !
    else
      tide_mf=0
      ntides=0
      !
      do while (len_trim(list(tide_mf+1)).ne.0)
        !
        tide_mf=tide_mf+1
        found = 0
        do j=1,65
          if (trim(tidecon_name_all(j)).eq.trim(list(tide_mf))) then
            ntides=ntides+1
            inds(ntides)=j
            found = 1
            if (iaproc.eq.napout) write(ndso,'(a,i4,2a,e12.2)')  &
                 'tidal constituent in analysis:', j, ' ', &
                 trim(tidecon_name_all(j)),tide_freqc_all(j)
          end if
        end do
        if (found.eq.0 .and. iaproc.eq.napout) write(ndso,'(a,i4,a,a)') &
             'tidal constituent ',tide_mf,trim(list(tide_mf)),' not available.'
        !
      end do
      !
      tide_mf=ntides
    end if
    !
    ! defines names and frequencies
    !
    if (allocated(tide_freqc)) deallocate(tide_freqc)
    allocate(tide_freqc(tide_mf),tidecon_name(tide_mf))
    do j=1,tide_mf
      tidecon_name(j) = tidecon_name_all(inds(j))
      tide_freqc(j) = tide_freqc_all(inds(j))
    end do
    call tide_set_indices
    !
  end subroutine tide_find_indices_analysis
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine tide_set_indices
    !
    implicit none
    !
    integer j, k, k1, l, j1, jl, l2, km1, jbase
    !
    do l=1,tide_mf
      do k=1,ntotal_con
        if (tidecon_allnames(k).eq.tidecon_name(l)) tide_index2(l)=k
      end do
    end do
    !
    tide_indexj(:)=0
    tide_indexjk(:)=0
    jbase=0
    k1=ntidal_con+1
    !
    do k=k1,ntotal_con
      j1=jbase+1
      tide_indexj(k)=j1
      jl=jbase+nj(k)
      do j=j1,jl
        km1=k-1
        l2=0
        do l2=1,km1
          if (tidecon_allnames(l2).eq.konco_con(j)) then
            tide_indexjk(j)=l2
          end if
        end do ! l2
      end do   ! j
      jbase=jl
    end do     ! k
    !
  end subroutine tide_set_indices
  !/ ------------------------------------------------------------------- /
  subroutine setvuf_fast(h,pp,s,p,enp,dh,dpp,ds,dp,dnp,tau,xlat,f,u,v)
    !      setvuf calculates the v,u,f values at time hr for all constituents
    implicit none
    real,         intent(in)  :: xlat
    real(kind=8), intent(in)  :: h,pp,s,p,enp,dh,dpp,ds,dp,dnp,tau
    integer, parameter        :: ntil=44
    real        , intent(out) :: f(ntil),u(ntil),v(ntil)
    real                      :: fa(170),ua(170),va(170)
    !
    !   local variables
    !
    integer                   :: k, l, l2, jbase, j1, j, jl, k1
    real(kind=4), parameter   :: pi=3.1415926536
    real(kind=4), parameter   :: twopi=2.*3.1415926536
    real                      :: slat, vdbl, rr, sumc, sums, uudbl, uu, cxlat
    integer                   :: iuu, iv
    ! this comment was taken from t_tide, a matlab tidal prediction suite
    !
    ! apparently the second-order terms in the tidal potential go to zero
    ! at the equator, but the third-order terms do not. hence when trying
    ! to infer the third-order terms from the second-order terms, the
    ! nodal correction factors blow up. in order to prevent this, it is
    ! assumed that the equatorial forcing is due to second-order forcing
    ! off the equator, from about the 5 degree location. latitudes are
    ! hence (somewhat arbitrarily) forced to be no closer than 5 deg to
    ! the equator, as per note in foreman.
    cxlat = max(abs(xlat), 5.)
    slat=sin(pi*cxlat/180.)
    jbase=0
    ! all
    ! 1'z0  *','sa   ','ssa *','msm *','mm   *,'msf *','mf  *','alp1*','2q1  ','sig1 ', &
    !11'q1   ','rho1 ','o1  *','tau1 ','bet1 ','no1  ','chi1 ','pi1  ','p1  *','s1   ', &
    !21'k1  *','psi1 ','phi1 ','the1 ','j1   ','oo1  ','ups1 ','oq2  ','eps2*','2n2 *', &
    !31'mu2 *','n2  *','nu2 *','gam2 ','h1   ','m2  *','h2   ','lda2*','l2  *','t2   ', &
    !41's2  *','r2   ','k2  *','eta2 ','m3   ','2po1 ','so1  ','st36 ','2ns2 ','st37 ', &
    !51'st1  ','st2  ','st3  ','o2   ','st4  ','snk2 ','op2  ','mks2*','st5  ','st6  ', &
    !61'2sk2 ','msn2 ','st7  ','2sm2 ','st38 ','skm2 ','2sn2 ','no3  ','mo3  ','nk3  ', &
    !71'so3  ','mk3  ','sp3  ','sk3  ','st8  ','n4   ','3ms4 ','st39 ','mn4  ','st40 ', &
    !81'st9  ','m4   ','st10 ','sn4  ','kn4  ','ms4  ','mk4  ','sl4  ','s4   ','sk4  ', &
    !91'mno5 ','2mo5 ','3mp5 ','mnk5 ','2mp5 ','2mk5 ','msk5 ','3km5 ','2sk5 ','st11 ', &
    !01'2nm6 ','st12 ','st41 ','2mn6 ','st13 ','m6   ','msn6 ','mkn6 ','2ms6 ','2mk6 ', &
    !11'nsk6 ','2sm6 ','msk6 ','st42 ','s6   ','st14 ','st15 ','m7   ','st16 ','3mk7 ', &
    !21'st17 ','st18 ','3mn8 ','st19 ','m8   ','st20 ','st21 ','3ms8 ','3mk8 ','st22 ', &
    !31'st23 ','st24 ','st25 ','st26 ','4mk9 ','st27 ','st28 ','m10  ','st29 ','st30 ', &
    !41'st31 ','st32 ','st33 ','m12  ','st34 ','st35 '/)
    !   possible
    !       'z0   ', 'ssa  ', 'msm  ', 'mm   ', 'msf  ', 'mf   ', 'alp1 ', '2q1  ', 'sig1 ', 'q1   ',  &
    !       'rho1 ', 'o1   ', 'tau1 ', 'bet1 ', 'no1  ', 'chi1 ', 'p1   ', 'k1   ', 'phi1 ', 'the1 ',  &
    !       'j1   ', 'so1  ', 'oo1  ', 'ups1 ', 'oq2  ', 'eps2 ', '2n2  ', 'mu2  ', 'n2   ', 'nu2  ',  &
    !       'm2   ', 'mks2 ', 'lda2 ', 'l2   ', 's2   ', 'k2   ', 'msn2 ', 'eta2 ', 'mo3  ', 'm3   ',  &
    !       'so3  ', 'mk3  ', 'sk3  ', 'mn4  ', 'm4   ', 'sn4  ', 'ms4  ', 'mk4  ', 's4   ', 'sk4  ',  &
    !       '2mk5 ', '2sk5 ', '2mn6 ', 'm6   ', '2ms6 ', '2mk6 ', '2sm6 ', 'msk6 ', '3mk7 ', 'm8   ' /)
    ! subset
    !       'z0   ', 'ssa  ', 'msm  ', 'mm   ', 'msf  ', 'mf   ',  &
    !                'o1   ',                                     'p1   ', 'k1   '                  ,  &
    !                                                    'eps2 ', '2n2  ', 'mu2  ', 'n2   ', 'nu2  ',  &
    !       'm2   ', 'mks2 ', 'lda2 ', 'l2   ', 's2   ', 'k2   ', 'msn2 ', 'eta2 ', 'mo3  ', 'm3   ',  &
    !       'so3  ', 'mk3  ', 'sk3  ', 'mn4  ', 'm4   ', 'sn4  ', 'ms4  ', 'mk4  ', 's4   ', 'sk4  ',  &
    !       '2mk5 ', '2sk5 ', '2mn6 ', 'm6   ', '2ms6 ', '2mk6 ', '2sm6 ', 'msk6 ', '3mk7 ', 'm8   ' /)
    !
    jbase=0
    ! initialize arrays to avoid nan values
    fa(:)=0
    ua(:)=0
    va(:)=0
    do k=1,ntidal_con
      j1=jbase+1
      jl=jbase+nj(k)
      do l=1,tide_mf
        if (tide_index2(l).eq.k) then
          vdbl=ii(k)*tau+jj(k)*s+kk(k)*h+ll(k)*p+mm(k)*enp+nn(k)*pp+semi(k)
          iv=vdbl
          iv=(iv/2)*2
          sumc=1.
          sums=0.
          do j=j1,jl
            ! itime ???
            !***********************************************************************
            !*  here the satellite amplitude ratio adjustment for latitude is made
            !
            rr=ee(j)
            l2=ir(j)+1
            if (l2.eq.2) then
              rr=ee(j)*0.36309*(1.-5.*slat*slat)/slat
            else if (l2.eq.3) then
              rr=ee(j)*2.59808*slat
            end if
            uudbl=ldel(j)*p+mdel(j)*enp+ndel(j)*pp+ph(j)
            iuu=uudbl
            uu=uudbl-iuu
            sumc=sumc+rr*cos(uu*twopi)
            sums=sums+rr*sin(uu*twopi)
          end do
          !
          fa(k)=sqrt(sumc*sumc+sums*sums)
          va(k)=vdbl-iv
          ua(k)=atan2(sums,sumc)/twopi
        end if
        jbase=jl ! (indx(l).eq.k)
      end do   ! l
    end do     ! k
    !
    !  here f and v+u of the shallow water constituents are computed from
    !  the values of the main constituent from which they are derived.
    !
    k1=ntidal_con+1
    if (k1.gt.ntotal_con) return
    !
    do k=k1,ntotal_con
      fa(k)=1.0
      va(k)=0.0
      ua(k)=0.
      do j=tide_indexj(k),tide_indexj(k)+nj(k)-1
        l2=tide_indexjk(j)
        fa(k)=fa(k)*fa(l2)**abs(coef_con(j))
        va(k)=va(k)+coef_con(j)*va(l2)
        ua(k)=ua(k)+coef_con(j)*ua(l2)
      end do   ! j
    end do     ! k
    !
    do l=1,tide_mf
      f(l)=fa(tide_index2(l))
      u(l)=ua(tide_index2(l))
      v(l)=va(tide_index2(l))
    end do   ! l
    return
    !
  end subroutine setvuf_fast
  !/ ------------------------------------------------------------------- /
  subroutine tide_read_settings(filename,fnam6,fnam7,fnam8,fnam9,fnam11)
    implicit none
    character*256, intent(in)     :: filename
    character*256, intent(out)    :: fnam6,fnam7,fnam8,fnam9,fnam11
    integer kin
    ! parameters for reading kr1
    !      file i/o
    !      kin is the master input file.
    !      fnam6 is the file to which the output is sent. it is assigned the number
    !            lp.
    !      fnam7 is file containing the constituents to be included in the analysis,
    !            the analysis period, inference parameters, the flag controlling
    !            height or current analyses, and site information. it is assigned the
    !            number kr1.
    !      fnam8 is the file containing all the astronomical argument information
    !            (it should not have to be changed)
    !      fnam11 is a file to which information on the svd matrix fit is output when
    !
    !      original, fitted, and residual time series are output to file 25 while
    !      the same are also output to file 26 in a format that could be input to
    !      excel for plotting.
    !
    kin=40   ! input file assigned to unit 4
    !      open(unit=kin,file='tuk75_tidana.inp',status='old')
    !      open(unit=kin,file='victoria_2008_test.inp',status='old')
    open(unit=kin,file=filename,status='old')
    !      open(unit=kin,file='kiw05_mar2008.inp',status='old')
    !      open(unit=kin,file='tcs05_sep07-mar08.inp',status='old')
    read(kin,'(a)') fnam6
    read(kin,'(a)') fnam7
    read(kin,'(a)') fnam8
    read(kin,'(a)') fnam9
    read(kin,'(a)') fnam11
    !      open(unit=11,file=fnam11,status='unknown',form='formatted')
    !      unit 25 stores the residual time series
    !read(kin,'(a)') fname
    !open(unit=25,file=fname,status='unknown',form='formatted')
    !read(kin,'(a)') fname
    !open(unit=26,file=fname,status='unknown',form='formatted')
  end subroutine tide_read_settings
  !/ ------------------------------------------------------------------- /
  subroutine tide_read_anapar(kr1,lp,filename,kd1,kd2,xlon,xlat,ndef,itrend,itz)
    ! parameters for reading kr1
    implicit none
    integer,       intent(in)     :: kr1, lp
    character*256, intent(in)     :: filename
    integer(kind=4), intent(out)  :: kd1, kd2
    integer        , intent(out)  :: ndef,itrend
    real,            intent(out)  :: xlon, xlat
    character*4 ,    intent(out)  :: itz
    !
    integer                   :: i, iy
    integer       id1,im1,iy1,id2,im2,iy2,ic1,ic2
    integer       jstn, latd,latm,lond,lonm, k, k2
    character*4   nstn(5)
    open(unit=kr1,file=filename,status='old',form='formatted')
    !
    !*
    !***********************************************************************
    !*  read from device kr1 the analysis type and tidal station details.
    !*
    !*  1)one record for the variables tide_mf
    !*  tide_mf   = the number of constituents, including the constant term z0,
    !*         to be in the least squares fit.
    !*
    !*  2)one record for each of the tide_mf constituents to be included in the
    !*  fit.  each record contains the variables name and tide_freqc in the
    !*  format (a5,2x,f13.10).  name is the constituent name, which should
    !*  be left justified in the alphanumeric field, while tide_freqc is its
    !*  frequency measured in cycles per hour.
    !*
    !*  3) one record in the format (8i5) containing the following
    !*  information on the time period of the analysis.
    !*  id1,im1,iy1 - day,month,year of the beginning of the analysis
    !*                period,
    !*  id2,im2,iy2 - day,month,year of the end of the analysis period.
    !*  ic1,ic2     - century ofr the beginning and end of the analysis
    !*                period (zero values are reset to 19)
    !*
    !*
    !*  4)one record in the format (i5,5a4,1x,a4,4i5) containing the
    !*  following tidal station  information.
    !*  jstn      = tidal station number,
    !*  (nstn(i),i=1,5) = tidal station name,
    !              *  itz       = time zone in which the observations were recorded,
    !              *  latd,latm = station latitude in degrees and minutes,
    !              *  lond,lonm = station longitude in degrees and minutes.
    !              *
    !              *  5)one set records for each possible inference. the first record has the
    !              *      consituent name, its frequency, and the number of constituents to be
    !              *      inferred (4x,a5,e16.10,i5), while there is one record for each of the
    !              *      constituents to be inferred with the name, frequency, amplitude ratio
    !              *      (inferred to reference) and phase difference (greenwich phase lag of
    !              *      the inferred constituent subtracted from the greenwich phase lag of the
    !              *    (analysed constituent in the format(4x,a5,e16.10,2f10.3)
    !              *
    !              *  for kr1 input, all constituent names should be left justified in
    !              *  the alphanumeric field, frequencies are measured in cycles/hour, and
    !              *  all constituents must be included in the list in read from fnam8.
    !
    !                    write(6,*) ' reading from unit kr1'
    read(kr1,*) tide_mf,ndef,itrend
    allocate(tide_freqc(tide_mf),tidecon_name(tide_mf))
    !      ndef=1 if only 1d field to be analysed (eg., elevations)
    !      ndef=2 if 2d field: velocity components, ew followed by ns   : this is now de-activated
    if (itrend.eq.1) then
    else
    end if
    !      tide_mf= number of consituents, excluding linear trend. the constant
    !      term, z0 should be first in the list.
    !      itrend= 1 if include linear trend
    !      itrend= otherwise, no trend
    !      number of unknowns, m, depends on whether we have a linear trend
10  format(2i5,f5.2)
    read(kr1,11) (tidecon_name(i),tide_freqc(i),i=1,tide_mf)
11  format(4x,a5,f16.10)
    read(kr1,7) id1,im1,iy1,id2,im2,iy2,ic1,ic2
    if (ic1.eq.0) ic1=19
    if (ic2.eq.0) ic2=19
7   format(16i5)
    read(kr1,9) jstn,nstn(1:5),itz,latd,latm,lond,lonm
9   format(i5,5a4,1x,a4,4i5)
    iy=ic1*100+iy1
    kd1=juldayt(id1,im1,iy)
    iy=ic2*100+iy2
    kd2=juldayt(id2,im2,iy)
    !
    !      read in inference information now as it will be used in the lsq matrix
    !
    do k=1,10
      read(kr1,'(4x,a5,e17.10,i5)')tide_konan(k),tide_sigan(k),tide_ninf(k)
      !      write(6,1010)tide_konan(k),tide_sigan(k),tide_ninf(k)
      if (tide_konan(k).eq.kblank) exit
      do k2=1,tide_ninf(k)
        read(kr1,'(4x,a5,e17.10,2f10.3)') tide_konin(k,k2),tide_sigin(k,k2),tide_r(k,k2),tide_zeta(k,k2)
      end do
    end do
    tide_nin=k-1
    close(kr1)
    xlat=latd+latm/60.
    xlon=lond+lonm/60.
    return
  end subroutine tide_read_anapar
  !/ ------------------------------------------------------------------- /
  subroutine tide_read_timeseries(kr2,filename,kd1,kd2,tide_nti,ndef)
    !
    implicit none
    !
    integer,       intent(in)     :: kr2, ndef
    character*256, intent(in)     :: filename
    integer(kind=4), intent(in)   :: kd1,kd2
    integer(kind=4), intent(out)  :: tide_nti
    !
    integer                       :: i, idd,imm,icc,iyy,ihh,imin,isec,iy
    real, allocatable             :: tide_datatmp(:,:)
    integer(kind=4), allocatable  :: tide_daystmp(:), tide_secstmp(:)
    integer(kind=4)               :: kdd
    integer                       :: icode
    real                          :: htt(ndef)
    !
    ! initialize variables
    !
    allocate( tide_datatmp(nr,ndef), tide_daystmp(nr), tide_secstmp(nr) )
    ! reads in data between dates kd1 and kd2 in file kr2
    !
    open(unit=kr2,file=filename,status='old',form='formatted')
    icode = 0
    kdd = kd1
    i=0
    do while(icode.eq.0.and.kdd.le.kd2)
      !
      ! reads with the original foreman's format
      !
      read(kr2,145,iostat=icode) idd,imm,icc,iyy,ihh,imin,htt(1:ndef)
145   format(6i2,4f10.4)
      isec=0
      iy=icc*100+iyy
      kdd=juldayt(idd,imm,iy)
      if (kdd.lt.kd1) then
        write(*,*) icc,iyy,imm,idd,ihh,imin
        write(*,*)'kd, kd1, kd2 =',kdd,kd1,kd2
        write(*,*) ' observation before analysis period'
      else
        !
        !  fills in data array
        !
        if (icode.eq.0.and.kdd.le.kd2) then
          i=i+1
          tide_datatmp(i,:)=htt(:)
          tide_daystmp(i)=kdd
          tide_secstmp(i)=ihh*3600+imin*60+isec
        end if
      end if
    end do
    tide_nti=i
    allocate( tide_data(tide_nti,ndef) )
    allocate( tide_days(tide_nti), tide_secs(tide_nti), tide_hours(tide_nti) )
    tide_data(1:tide_nti,1:ndef)=tide_datatmp(1:tide_nti,1:ndef)
    tide_days(1:tide_nti)=tide_daystmp(1:tide_nti)
    tide_secs(1:tide_nti)=tide_secstmp(1:tide_nti)
    tide_hours(:)=24.d0*dfloat(tide_days(:))+dfloat(tide_secs(:))/3600.d0
    close(kr2)
    return
  end subroutine tide_read_timeseries
  !/ ------------------------------------------------------------------- /
  subroutine astr(d1,h,pp,s,p,np,dh,dpp,ds,dp,dnp)
    !      this subroutine calculates the following five ephermides
    !      of the sun and moon
    !      h = mean longitude of the sum
    !      pp = mean longitude of the solar perigee
    !      s = mean longitude of the moon
    !      p = mean longitude of the lunar perigee
    !      np = negative of the longitude of the mean ascending node
    !      and their rates of change.
    !      units for the ephermides are cycles and for their derivatives
    !      are cycles/365 days
    !      the formulae for calculating this ephermides were taken from
    !      pages 98 and 107 of the explanatory supplement to the
    !      astronomical ephermeris and the american ephermis and
    !      nautical almanac (1961)
    !
    implicit none
    real(kind=8), intent(in ):: d1
    real(kind=8), intent(out):: h,pp,s,p,np,dh,dpp,ds,dp,dnp
    !
    !   local variables
    !
    real(kind=8)              :: d2,f,f2
    d2=d1*1.d-4
    f=360.d0
    f2=f/365.d0
    h=279.696678d0+.9856473354d0*d1+.00002267d0*d2*d2
    pp=281.220833d0+.0000470684d0*d1+.0000339d0*d2*d2+&
         .00000007d0*d2**3
    s=270.434164d0+13.1763965268d0*d1-.000085d0*d2*d2+&
         .000000039d0*d2**3
    p=334.329556d0+.1114040803d0*d1-.0007739d0*d2*d2-&
         .00000026d0*d2**3
    np=-259.183275d0+.0529539222d0*d1-.0001557d0*d2*d2-&
         .00000005d0*d2**3
    h=h/f
    pp=pp/f
    s=s/f
    p=p/f
    np=np/f
    h=h-dint(h)
    pp=pp-dint(pp)
    s=s-dint(s)
    p=p-dint(p)
    np=np-dint(np)
    dh=.9856473354d0+2.d-8*.00002267d0*d1
    dpp=.0000470684d0+2.d-8*.0000339d0*d1&
         +3.d-12*.00000007d0*d1**2
    ds=13.1763965268d0-2.d-8*.000085d0*d1+&
         3.d-12*.000000039d0*d1**2
    dp=.1114040803d0-2.d-8*.0007739d0*d1-&
         3.d-12*.00000026d0*d1**2
    dnp=+.0529539222d0-2.d-8*.0001557d0*d1-&
         3.d-12*.00000005d0*d1**2
    dh=dh/f2
    dpp=dpp/f2
    ds=ds/f2
    dp=dp/f2
    dnp=dnp/f2
    return
  end subroutine astr
  !/ ------------------------------------------------------------------- /
  ! note by fa: should try to replace with standard distance  d= sqrt(a^2+b^2)
  function dpythag(a,b)
    double precision a,b,dpythag
    double precision absa,absb
    absa=abs(a)
    absb=abs(b)
    if (absa.gt.absb)then
      dpythag=absa*sqrt(1.0d0+(absb/absa)**2)
    else
      if (absb.eq.0.0d0)then
        dpythag=0.0d0
      else
        dpythag=absb*sqrt(1.0d0+(absa/absb)**2)
      endif
    endif
    return
  end function dpythag
  !/ ------------------------------------------------------------------- /
  !  (c) copr. 1986-92 numerical recipes software '%1&&yw^2.
  subroutine dsvbksb(u,w,v,m,n,mp,np,b,x)
    integer m,mp,n,np,nmax
    double precision b(mp),u(mp,np),v(np,np),w(np),x(np)
    parameter (nmax=500)
    integer i,j,jj
    double precision s,tmp(nmax)
    do j=1,n
      s=0.0d0
      if (w(j).ne.0.0d0)then
        do i=1,m
          s=s+u(i,j)*b(i)
        end do
        s=s/w(j)
      endif
      tmp(j)=s
    end do
    do  j=1,n
      s=0.0d0
      do  jj=1,n
        s=s+v(j,jj)*tmp(jj)
      end do
      x(j)=s
    end do
    return
  end subroutine dsvbksb
  !/ ------------------------------------------------------------------- /
  subroutine dsvdcmp(a,m,n,mp,np,w,v)
    integer m,mp,n,np,nmax
    double precision a(mp,np),v(np,np),w(np)
    parameter (nmax=500)
    integer i,its,j,jj,k,l,nm
    double precision anorm,c,f,g,h,s,scale,x,y,z,rv1(nmax)
    g=0.0d0
    scale=0.0d0
    anorm=0.0d0
    do i=1,n
      l=i+1
      rv1(i)=scale*g
      g=0.0d0
      s=0.0d0
      scale=0.0d0
      if (i.le.m)then
        do k=i,m
          scale=scale+abs(a(k,i))
        end do
        if (scale.ne.0.0d0) then
          do  k=i,m
            a(k,i)=a(k,i)/scale
            s=s+a(k,i)*a(k,i)
          end do
          f=a(i,i)
          g=-sign(sqrt(s),f)
          h=f*g-s
          a(i,i)=f-g
          do j=l,n
            s=0.0d0
            do k=i,m
              s=s+a(k,i)*a(k,j)
            end do
            f=s/h
            do k=i,m
              a(k,j)=a(k,j)+f*a(k,i)
            end do
          end do
          do k=i,m
            a(k,i)=scale*a(k,i)
          end do
          !
        end if
        !
      end if
      !
      w(i)=scale *g
      g=0.0d0
      s=0.0d0
      scale=0.0d0
      if ((i.le.m).and.(i.ne.n))then
        do k=l,n
          scale=scale+abs(a(i,k))
        end do
        if (scale.ne.0.0d0)then
          do k=l,n
            a(i,k)=a(i,k)/scale
            s=s+a(i,k)*a(i,k)
          end do
          f=a(i,l)
          g=-sign(sqrt(s),f)
          h=f*g-s
          a(i,l)=f-g
          do k=l,n
            rv1(k)=a(i,k)/h
          end do
          do j=l,m
            s=0.0d0
            do k=l,n
              s=s+a(j,k)*a(i,k)
            end do
            do k=l,n
              a(j,k)=a(j,k)+s*rv1(k)
            end do
          end do
          do k=l,n
            a(i,k)=scale*a(i,k)
          end do
        endif
      endif
      anorm=max(anorm,(abs(w(i))+abs(rv1(i))))
    end do
    do i=n,1,-1
      if (i.lt.n)then
        if (g.ne.0.0d0)then
          do j=l,n
            v(j,i)=(a(i,j)/a(i,l))/g
          end do
          do j=l,n
            s=0.0d0
            do k=l,n
              s=s+a(i,k)*v(k,j)
            end do
            do k=l,n
              v(k,j)=v(k,j)+s*v(k,i)
            end do
          end do
        endif
        do j=l,n
          v(i,j)=0.0d0
          v(j,i)=0.0d0
        end do
      endif
      v(i,i)=1.0d0
      g=rv1(i)
      l=i
    end do
    do i=min(m,n),1,-1
      l=i+1
      g=w(i)
      do  j=l,n
        a(i,j)=0.0d0
      end do
      if (g.ne.0.0d0)then
        g=1.0d0/g
        do j=l,n
          s=0.0d0
          do k=l,m
            s=s+a(k,i)*a(k,j)
          end do
          f=(s/a(i,i))*g
          do k=i,m
            a(k,j)=a(k,j)+f*a(k,i)
          end do
        end do
        do j=i,m
          a(j,i)=a(j,i)*g
        end do
      else
        do  j= i,m
          a(j,i)=0.0d0
        end do
      endif
      a(i,i)=a(i,i)+1.0d0
    end do
    do k=n,1,-1
      do  its=1,30
        do  l=k,1,-1
          nm=l-1
          if ((abs(rv1(l))+anorm).eq.anorm)  goto 2
          if ((abs(w(nm))+anorm).eq.anorm)  goto 1
        end do
1       c=0.0d0
        s=1.0d0
        do  i=l,k
          f=s*rv1(i)
          rv1(i)=c*rv1(i)
          if ((abs(f)+anorm).eq.anorm) goto 2
          g=w(i)
          h=dpythag(f,g)
          w(i)=h
          h=1.0d0/h
          c= (g*h)
          s=-(f*h)
          do j=1,m
            y=a(j,nm)
            z=a(j,i)
            a(j,nm)=(y*c)+(z*s)
            a(j,i)=-(y*s)+(z*c)
          end do
        end do
2       z=w(k)
        if (l.eq.k)then
          if (z.lt.0.0d0)then
            w(k)=-z
            do j=1,n
              v(j,k)=-v(j,k)
            end do
          endif
          goto 3
        endif
        if (its.eq.30) then
          write(6,*) 'no convergence in svdcmp'
          stop
        end if
        x=w(l)
        nm=k-1
        y=w(nm)
        g=rv1(nm)
        h=rv1(k)
        f=((y-z)*(y+z)+(g-h)*(g+h))/(2.0d0*h*y)
        g=dpythag(f,1.0d0)
        f=((x-z)*(x+z)+h*((y/(f+sign(g,f)))-h))/x
        c=1.0d0
        s=1.0d0
        do  j=l,nm
          i=j+1
          g=rv1(i)
          y=w(i)
          h=s*g
          g=c*g
          z=dpythag(f,h)
          rv1(j)=z
          c=f/z
          s=h/z
          f= (x*c)+(g*s)
          g=-(x*s)+(g*c)
          h=y*s
          y=y*c
          do  jj=1,n
            x=v(jj,j)
            z=v(jj,i)
            v(jj,j)= (x*c)+(z*s)
            v(jj,i)=-(x*s)+(z*c)
          end do
          z=dpythag(f,h)
          w(j)=z
          if (z.ne.0.0d0)then
            z=1.0d0/z
            c=f*z
            s=h*z
          endif
          f= (c*g)+(s*y)
          x=-(s*g)+(c*y)
          do jj=1,m
            y=a(jj,j)
            z=a(jj,i)
            a(jj,j)= (y*c)+(z*s)
            a(jj,i)=-(y*s)+(z*c)
          end do
        end do
        rv1(l)=0.0d0
        rv1(k)=f
        w(k)=x
      end do
3     continue
    end do
    return
  end subroutine dsvdcmp
  !/ ------------------------------------------------------------------- /
  function juldayt(id,mm,iyyy)
    ! see numerical recipes 2nd ed. the order of month and day have been swapped!
    !*********************************************************************
    implicit none
    integer id,mm,iyyy
    integer igreg
    integer*4 ja,jm,jy
    integer*4 juldayt
    igreg=15+31*(10+12*1582)
    jy=iyyy
    if (jy.eq.0) write(6,*) 'there is no zero year !!'
    if (jy.lt.0) jy=jy+1
    if (mm.gt.2) then
      jm=mm+1
    else
      jy=jy-1
      jm=mm+13
    endif
    juldayt=int(365.25*jy)+int(30.6001*jm)+id+1720995
    if (id+31*(mm+12*iyyy).ge.igreg) then
      ja=int(0.01*jy)
      juldayt=juldayt+2-ja+int(0.25*ja)
    endif
    return
  end function juldayt
  !/ ------------------------------------------------------------------- /
  subroutine caldatt(julian,id,mm,iyyy)
    ! see numerical recipes 2nd ed. the order of month and day have been swapped!
    ! should be removed : same is now in w3timemd
    !*********************************************************************
    implicit none
    integer(kind=4),    intent(in)  :: julian
    integer(kind=4),    intent(out) :: id,mm,iyyy
    integer(kind=4), parameter :: igreg=2299161
    integer(kind=4) ja,jalpha,jb,jc,jd,je
    if (julian.ge.igreg) then
      jalpha=int(((julian-1867216)-0.25)/36524.25)
      ja=julian+1+jalpha-int(0.25*jalpha)
    else
      ja=julian
    endif
    jb=ja+1524
    jc=int(6680.+((jb-2439870)-122.1)/365.25)
    jd=365*jc+int(0.25*jc)
    je=int((jb-jd)/30.6001)
    id=jb-jd-int(30.6001*je)
    mm=je-1
    if (mm.gt.12) mm=mm-12
    iyyy=jc-4715
    if (mm.gt.2) iyyy=iyyy-1
    if (iyyy.le.0) iyyy=iyyy-1
    return
  end subroutine caldatt
  !/ ------------------------------------------------------------------- /
  subroutine svd(q,u,v,cov,w,p,b,sig,ic,m,n,mm,n2,toler,jc     &
       ,ssq,res)
    !-----------------------------------------------------------------------
    ! svd uses singular-value-decomposition to calculate the least-squares
    ! solution p to an overdetermined system of linear equations with
    ! coefficient matrix q, which includes right hand side vector b.
    !
    ! there are two ways to use svd:
    ! 1 given an overdetermined system, svd will orthogonalize
    !   a and b and produce the least-squares solution.
    ! 2 given an orthogonalized a (i.e. output from 1),
    !   svd will orthogonalize b with respect to a and produce
    !   the least-squares solution. this allows the use of
    !   multiple r.h.s. without reorthogonalizing a.
    !-----------------------------------------------------------------------
    ! description of parameters:
    ! ic     an input code which must be set to 1 or 2
    ! m      the number of equations (rows of q) to solve.
    ! n      the total number of columns of q to be used (<n2)
    ! n2   the number of columns of q, at least n+1.
    ! mm   the number of rows of q, at least n2+m.
    ! q      an mm-by-n2 array which on entry must contain the
    !        matrix a in its first m rows and n columns, and the vector
    !        b in its first m rows of column mm. on exit the residual
    !        of equation i is stored in q(i,n2), i=1 to m.
    ! sig    measurement error (standard deviation) for the rhs
    !        from the calling program (can be set to 1.)
    ! u      an mm-by-n2 matrix used in svd, replaced by "left" matrix u
    ! v      an n2-by-n2 matrix of n "right" eigenvectors of q (=u)
    ! w      an n2-diagonal vector(matrix) of n eigenvalues of q (u)
    ! cov    an output covariance matrix between eigenvectors of q (u)
    ! toler  an input tolerance, for acceptable matrix condition number
    ! p      an output array of dimension at least n2 containing
    !        the least-squares solution in positions 1 to n.
    ! jc     an output code which is set to the index of the
    !        1st dependent column, if such a column is detected.
    ! ssq    sum of squares of the residuals
    ! res    the largest residual in magnitude
    !-----------------------------------------------------------------------
    ! history:
    !       written by j. cherniawsky, august 1997
    !        last modified              august 1998
    !-----------------------------------------------------------------------
    implicit none
    ! parameters
    integer, parameter        :: nwt=302      ! make nwt > expected value of n2
    ! i/o variables
    integer, intent(in)               :: ic, m, n, n2, mm
    real, intent(in)                  :: toler
    real, intent(inout)               :: q(mm,n2), ssq, res
    integer, intent(inout)            :: jc
    double precision, intent(inout)   :: sig(mm)
    double precision, intent(out)     :: u(mm,n2),v(n2,n2),cov(n2,n2),      &
         w(n2),b(mm),p(n2)
    ! local variables
    double precision          :: wti(nwt)
    integer                   :: i, j, k
    real                      :: wmax, thresh
    double precision          :: eps, sum, resi
    jc=0
    do i=1,mm
      b(i)=q(i,n2)
    enddo
    ! no need to solve if only rhs has changed
    if (ic.eq.2) go to 10
    ! define a "design matrix" u(=a) and set-up working arrays
    do j=1,n2
      do i=1,mm
        u(i,j)=q(i,j)
      enddo
    enddo
    ! compute svd decomposition of u(=a), with a being replaced by its upper
    ! matrix u, viz a=u*w*transpose(v), and vector w is output of a diagonal
    ! matrix of singular values w(i), i=1,n.
    call dsvdcmp(u,m,n,mm,n2,w,v)
    ! check for small singular values
    wmax=0.
    do j=1,n
      if (w(j).gt.wmax) wmax=w(j)
    enddo
    thresh=toler*wmax
    do j=1,n
      if (w(j).lt.thresh) then
        w(j)=0.d0
        if (jc.lt.1) jc=j
      endif
    enddo
10  eps=1.d-10
    ! compute summation weights (wti, used below)
    do j=1,n
      wti(j)=0.d0
      if (w(j).gt.eps) then
        !         wti(j)=sig(j)*sig(j)/(w(j)*w(j))
        wti(j)=1.d0/(w(j)*w(j))
      endif
    enddo
    ! use back-substitution to compute the solution p(i), i=1,n
    call dsvbksb(u,w,v,m,n,mm,n2,b,p)
    ! compute chisq (=ssq) and the largest residual (res)
    ssq=0.
    res=0.
    do i=1,m
      sum=0.d0
      do j=1,n
        sum=sum+p(j)*q(i,j)
      enddo
      resi=abs(b(i)-sum)
      !      tide_mf addition
      q(i,n2)=b(i)-sum
      res=max(res,resi)
      ssq=ssq+resi**2
    enddo
    ! compute variances, covariances, these may need to be given dimension
    ! of b(i), e.g., using sig(i), but this is better done after return to main
    do i=1,n
      do j=1,i
        sum=0.d0
        do k=1,n
          sum=sum+v(i,k)*v(j,k)*wti(k)
        enddo
        cov(i,j)=sum
        cov(j,i)=sum
      enddo
    enddo
    return
  end subroutine svd
  !/ ------------------------------------------------------------------- /
  subroutine vuf_set_parameters
    !
    !***********************************************************************
    !*  here the main constituents and their doodson numbers are set
    !*  format (6x,a5,1x,6i3,f5.2,i4). the values are respectively
    !*     tidecon_allnames   = constituent name
    !*  ii,jj,kk,ll,mm,nn = the six doodson numbers
    !*     semi   = phase correction
    !*     nj     = the number of satellites for this constituent.
    !*  the end of all main constituents is denoted by a blank card.
    !
    implicit none
    integer          :: jlm
    ntidal_con = 45
    ntotal_con = 45+101
    nkonco = 251
    jlm = 170
    allocate(tide_indexj(ntotal_con),tide_indexjk(nkonco))
    !
    allocate(tidecon_allnames(ntotal_con))
    allocate(ii(ntidal_con),jj(ntidal_con),kk(ntidal_con),ll(ntidal_con),mm(ntidal_con), &
         nn(ntidal_con),semi(ntidal_con), nj(ntotal_con))
    allocate(konco_con(nkonco),coef_con(nkonco))
    tidecon_allnames(:)=(/ &
         'z0   ','sa   ','ssa  ','msm  ','mm   ','msf  ','mf   ','alp1 ','2q1  ','sig1 ', &
         'q1   ','rho1 ','o1   ','tau1 ','bet1 ','no1  ','chi1 ','pi1  ','p1   ','s1   ', &
         'k1   ','psi1 ','phi1 ','the1 ','j1   ','oo1  ','ups1 ','oq2  ','eps2 ','2n2  ', &
         'mu2  ','n2   ','nu2  ','gam2 ','h1   ','m2   ','h2   ','lda2 ','l2   ','t2   ', &
         's2   ','r2   ','k2   ','eta2 ','m3   ','2po1 ','so1  ','st36 ','2ns2 ','st37 ', &
         'st1  ','st2  ','st3  ','o2   ','st4  ','snk2 ','op2  ','mks2 ','st5  ','st6  ', &
         '2sk2 ','msn2 ','st7  ','2sm2 ','st38 ','skm2 ','2sn2 ','no3  ','mo3  ','nk3  ', &
         'so3  ','mk3  ','sp3  ','sk3  ','st8  ','n4   ','3ms4 ','st39 ','mn4  ','st40 ', &
         'st9  ','m4   ','st10 ','sn4  ','kn4  ','ms4  ','mk4  ','sl4  ','s4   ','sk4  ', &
         'mno5 ','2mo5 ','3mp5 ','mnk5 ','2mp5 ','2mk5 ','msk5 ','3km5 ','2sk5 ','st11 ', &
         '2nm6 ','st12 ','st41 ','2mn6 ','st13 ','m6   ','msn6 ','mkn6 ','2ms6 ','2mk6 ', &
         'nsk6 ','2sm6 ','msk6 ','st42 ','s6   ','st14 ','st15 ','m7   ','st16 ','3mk7 ', &
         'st17 ','st18 ','3mn8 ','st19 ','m8   ','st20 ','st21 ','3ms8 ','3mk8 ','st22 ', &
         'st23 ','st24 ','st25 ','st26 ','4mk9 ','st27 ','st28 ','m10  ','st29 ','st30 ', &
         'st31 ','st32 ','st33 ','m12  ','st34 ','st35 '/)
    ii(:)=(/ 0,  0,  0,  0,  0,  0,  0,  1,  1,  1, &
         1,  1,  1,  1,  1,  1,  1,  1,  1,  1, &
         1,  1,  1,  1,  1,  1,  1,  2,  2,  2, &
         2,  2,  2,  2,  2,  2,  2,  2,  2,  2, &
         2,  2,  2,  2,  3 /)
    jj(:)=(/ 0,  0,  0,  1,  1,  2,  2, -4, -3, -3, &
         -2, -2, -1, -1,  0,  0,  0,  1,  1,  1, &
         1,  1,  1,  2,  2,  3,  4, -3, -3, -2, &
         -2, -1, -1,  0,  0,  0,  0,  1,  1,  2, &
         2,  2,  2,  3,  0 /)
    kk(:)=(/ 0,  1,  2, -2,  0, -2,  0,  2,  0,  2, &
         0,  2,  0,  2, -2,  0,  2, -3, -2, -1, &
         0,  1,  2, -2,  0,  0,  0,  0,  2,  0, &
         2,  0,  2, -2, -1,  0,  1, -2,  0, -3, &
         -2, -1,  0,  0,  0 /)
    ll(:)=(/ 0,  0,  0,  1, -1,  0,  0,  1,  2,  0, &
         1, -1,  0,  0,  1,  1, -1,  0,  0,  0, &
         0,  0,  0,  1, -1,  0, -1,  3,  1,  2, &
         0,  1, -1,  2,  0,  0,  0,  1, -1,  0, &
         0,  0,  0, -1,  0 /)
    mm(:)=(/ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
         0,  0,  0,  0,  0 /)
    nn(:)=(/ 0, -1,  0,  0,  0,  0,  0,  0,  0,  0, &
         0,  0,  0,  0,  0,  0,  0,  1,  0,  1, &
         0, -1,  0,  0,  0,  0,  0,  0,  0,  0, &
         0,  0,  0,  0,  1,  0, -1,  0,  0,  1, &
         0, -1,  0,  0,  0 /)
    semi(:)=(/ 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,-0.25,-0.25,-0.25, &
         -0.25,-0.25,-0.25,-0.75,-0.75,-0.75,-0.75,-0.25,-0.25,-0.75, &
         -0.75,-0.75,-0.75,-0.75,-0.75,-0.75,-0.75, 0.00, 0.00, 0.00, &
         0.00, 0.00, 0.00,-0.50,-0.50, 0.00, 0.00,-0.50,-0.50, 0.00, &
         0.00,-0.50, 0.00, 0.00,-0.50 /)
    nj(:)=(/    1,   1,   1,   1,   1,   1,   1,   2,   5,   4, &
         10,   5,   8,   5,   1,   9,   2,   1,   6,   2, &
         10,   1,   5,   4,  10,   8,   5,   2,   3,   4, &
         3,   4,   4,   3,   2,   9,   1,   1,   5,   1, &
         3,   2,   5,   7,   1,   2,   2,   3,   2,   2, &
         3,   4,   3,   1,   3,   3,   2,   3,   3,   4, &
         2,   3,   4,   2,   3,   3,   2,   2,   2,   2, &
         2,   2,   2,   2,   3,   1,   2,   4,   2,   3, &
         4,   1,   3,   2,   2,   2,   2,   2,   1,   2, &
         3,   2,   2,   3,   2,   2,   3,   3,   2,   3, &
         2,   4,   3,   2,   4,   1,   3,   3,   2,   2, &
         3,   2,   3,   3,   1,   3,   3,   1,   3,   2, &
         4,   2,   2,   4,   1,   3,   3,   2,   2,   4, &
         2,   3,   3,   3,   2,   3,   2,   1,   3,   2, &
         4,   2,   3,   1,   2,   4/)
    ldel(1:jlm)=(/  0,  0,  0,  0,  0,  0,  0, -1,  0, -2, &
         -1, -1,  0,  0, -1,  0,  0,  2, -2, -2, &
         -1, -1, -1,  0, -1,  0,  1,  2,  0,  0, &
         1,  2,  2, -1,  0,  0,  1,  1,  1,  2, &
         2, -2, -1,  0,  0,  0,  0, -2, -2, -2, &
         -1, -1, -1,  0,  0,  0,  0,  0,  0,  0, &
         0,  0,  1,  2,  2,  0,  0, -2, -1, -1, &
         -1,  0,  0,  0,  0,  1,  1,  0, -2, -2, &
         0,  0,  0, -2, -1,  0,  0,  0,  0,  0, &
         1,  1,  1,  1,  2,  2,  2, -2, -2, -2, &
         -1, -1,  0,  0,  0, -2,  0,  0,  1,  1, &
         -1,  0, -1, -1,  0, -2, -1, -1,  0, -1, &
         -1,  0, -2, -1,  0,  0,  0,  1,  2,  2, &
         -2, -1,  0,  0,  1, -1, -1,  0,  0,  1, &
         1,  1,  2,  2,  0,  0,  0,  2,  2,  2, &
         2,  0,  0,  1,  2,  0,  0, -1, -1,  0, &
         0,  0,  0,  0,  0,  1,  1,  1,  2,  0/)
    mdel(1:jlm)=(/  0,  0,  0,  0,  0,  0,  0,  0, -1, -2, &
         -1,  0, -2, -1,  0, -2, -1,  0, -3, -2, &
         -2, -1,  0, -2,  0, -1,  0,  0, -2, -1, &
         0,  0,  1,  0, -2, -1, -1,  0,  1,  0, &
         1,  0,  0, -1,  1,  2, -1, -2, -1,  0, &
         -1,  0,  1, -1,  1,  2, -1,  1, -1, -2, &
         -1,  0,  0,  0,  1,  0,  1, -1, -1,  0, &
         1, -2, -1,  1,  2,  0,  1,  1,  0,  1, &
         0,  1,  2, -1,  0, -1,  1, -1,  1,  2, &
         -1,  0,  1,  2,  0,  1,  2, -1,  0,  1, &
         0,  1,  1,  2,  3,  0,  1,  2,  0,  1, &
         0, -1, -1,  0, -1, -2, -1,  0, -1, -1, &
         0, -1, -2,  0, -2, -1, -1,  0,  0,  1, &
         -2,  0, -1, -1,  0, -1,  0, -2, -1, -1, &
         0,  1,  0,  1, -1, -1, -1, -1,  0,  1, &
         2,  0, -1,  0,  0,  0,  1,  0,  1, -1, &
         1,  2, -1,  1,  2,  0,  1,  2,  0, -1 /)
    ndel(1:jlm)=(/ 0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
         0,  0,  0,  0,  1,  0,  0,  0,  0,  0, &
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
         0,  2,  0,  0,  0, -2,  0,  0,  0,  0, &
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
         -2,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
         0,  0,  0,  1,  0,  0,  0,  0,  0,  0, &
         0,  0,  0,  0, -1,  0,  0,  0,  0,  0, &
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
         0,  0,  0,  0,  0,  2,  2,  0,  0,  0, &
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0 /)
    ph(1:jlm)=(/ 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.75, 0.00, 0.50, &
         0.75, 0.75, 0.50, 0.00, 0.75, 0.50, 0.00, 0.50, 0.50, 0.50, &
         0.75, 0.75, 0.75, 0.50, 0.00, 0.00, 0.75, 0.50, 0.50, 0.00, &
         0.75, 0.50, 0.00, 0.25, 0.50, 0.00, 0.25, 0.75, 0.25, 0.50, &
         0.50, 0.00, 0.25, 0.50, 0.50, 0.50, 0.00, 0.50, 0.00, 0.00, &
         0.75, 0.25, 0.75, 0.50, 0.00, 0.50, 0.50, 0.00, 0.50, 0.00, &
         0.50, 0.50, 0.75, 0.50, 0.50, 0.00, 0.50, 0.00, 0.75, 0.25, &
         0.75, 0.00, 0.50, 0.00, 0.50, 0.25, 0.25, 0.00, 0.00, 0.00, &
         0.00, 0.50, 0.50, 0.00, 0.25, 0.50, 0.00, 0.50, 0.00, 0.50, &
         0.75, 0.25, 0.25, 0.25, 0.50, 0.50, 0.50, 0.50, 0.00, 0.00, &
         0.25, 0.25, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.25, 0.25, &
         0.25, 0.50, 0.25, 0.25, 0.50, 0.50, 0.25, 0.25, 0.50, 0.25, &
         0.25, 0.50, 0.50, 0.00, 0.00, 0.50, 0.50, 0.75, 0.00, 0.50, &
         0.00, 0.25, 0.50, 0.50, 0.50, 0.75, 0.75, 0.00, 0.50, 0.25, &
         0.75, 0.75, 0.00, 0.00, 0.50, 0.50, 0.50, 0.00, 0.50, 0.50, &
         0.50, 0.00, 0.00, 0.75, 0.00, 0.50, 0.00, 0.75, 0.75, 0.50, &
         0.00, 0.00, 0.50, 0.00, 0.00, 0.75, 0.75, 0.75, 0.50, 0.50 /)
    ee(1:jlm)=(/ 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0360, 0.1906, 0.0063, &
         0.0241, 0.0607, 0.0063, 0.1885, 0.0095, 0.0061, 0.1884, 0.0087, 0.0007, 0.0039, &
         0.0010, 0.0115, 0.0292, 0.0057, 0.0008, 0.1884, 0.0018, 0.0028, 0.0058, 0.1882, &
         0.0131, 0.0576, 0.0175, 0.0003, 0.0058, 0.1885, 0.0004, 0.0029, 0.0004, 0.0064, &
         0.0010, 0.0446, 0.0426, 0.0284, 0.2170, 0.0142, 0.2266, 0.0057, 0.0665, 0.3596, &
         0.0331, 0.2227, 0.0290, 0.0290, 0.2004, 0.0054, 0.0282, 0.2187, 0.0078, 0.0008, &
         0.0112, 0.0004, 0.0004, 0.0015, 0.0003, 0.3534, 0.0264, 0.0002, 0.0001, 0.0007, &
         0.0001, 0.0001, 0.0198, 0.1356, 0.0029, 0.0002, 0.0001, 0.0190, 0.0344, 0.0106, &
         0.0132, 0.0384, 0.0185, 0.0300, 0.0141, 0.0317, 0.1993, 0.0294, 0.1980, 0.0047, &
         0.0027, 0.0816, 0.0331, 0.0027, 0.0152, 0.0098, 0.0057, 0.0037, 0.1496, 0.0296, &
         0.0240, 0.0099, 0.6398, 0.1342, 0.0086, 0.0611, 0.6399, 0.1318, 0.0289, 0.0257, &
         0.1042, 0.0386, 0.0075, 0.0402, 0.0373, 0.0061, 0.0117, 0.0678, 0.0374, 0.0018, &
         0.0104, 0.0375, 0.0039, 0.0008, 0.0005, 0.0373, 0.0373, 0.0042, 0.0042, 0.0036, &
         0.1429, 0.0293, 0.0330, 0.0224, 0.0447, 0.0001, 0.0004, 0.0005, 0.0373, 0.0001, &
         0.0009, 0.0002, 0.0006, 0.0002, 0.0217, 0.0448, 0.0366, 0.0047, 0.2505, 0.1102, &
         0.0156, 0.0000, 0.0022, 0.0001, 0.0001, 0.2535, 0.0141, 0.0024, 0.0004, 0.0128, &
         0.2980, 0.0324, 0.0187, 0.4355, 0.0467, 0.0747, 0.0482, 0.0093, 0.0078, 0.0564 /)
    ir(1:jlm)=(/ 0,  0,  0,  0,  0,  0,  0,  1,  0,  0, &
         1,  1,  0,  0,  1,  0,  0,  0,  0,  0, &
         1,  1,  1,  0,  0,  0,  1,  0,  0,  0, &
         1,  0,  0,  1,  0,  0,  1,  1,  1,  0, &
         0,  0,  1,  0,  0,  0,  0,  0,  0,  0, &
         1,  1,  1,  0,  0,  0,  0,  0,  0,  0, &
         0,  0,  1,  0,  0,  0,  0,  0,  1,  1, &
         1,  0,  0,  0,  0,  1,  1,  0,  0,  0, &
         0,  0,  0,  0,  1,  0,  0,  0,  0,  0, &
         1,  1,  1,  1,  0,  0,  0,  0,  0,  0, &
         1,  1,  0,  0,  0,  0,  0,  0,  1,  1, &
         2,  0,  2,  2,  0,  0,  2,  2,  0,  2, &
         2,  0,  0,  0,  0,  0,  0,  2,  0,  0, &
         0,  2,  0,  0,  0,  2,  2,  0,  0,  2, &
         2,  2,  0,  0,  0,  0,  0,  0,  0,  0, &
         0,  0,  0,  2,  0,  0,  0,  2,  2,  0, &
         0,  0,  0,  0,  0,  2,  2,  2,  0,  0 /)
    coef_con(:)=(/ 2.00,-1.00, 1.00,-1.00, 2.00, 1.00,-2.00, 2.00,-1.00, 3.00, &
         -2.00, 2.00, 1.00,-2.00, 1.00, 1.00, 1.00,-2.00, 2.00, 1.00, &
         -2.00, 2.00, 2.00, 1.00,-2.00, 1.00, 1.00,-1.00, 1.00, 1.00, &
         1.00, 1.00,-1.00, 1.00, 2.00,-2.00, 2.00, 1.00,-1.00,-1.00, &
         2.00,-1.00, 1.00, 1.00,-1.00, 2.00, 1.00,-1.00,-1.00, 2.00, &
         -1.00, 2.00, 1.00,-2.00, 1.00, 1.00,-1.00, 2.00,-1.00, 1.00, &
         1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, &
         1.00, 1.00, 1.00, 2.00, 1.00,-1.00, 2.00, 3.00,-1.00, 1.00, &
         1.00, 1.00,-1.00, 1.00, 1.00, 2.00, 1.00,-1.00, 1.00, 1.00, &
         1.00,-1.00, 2.00, 2.00, 1.00,-1.00, 1.00, 1.00, 1.00, 1.00, &
         1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 2.00, 1.00, 1.00, 1.00, &
         1.00, 1.00, 2.00, 1.00, 3.00,-1.00, 1.00, 1.00, 1.00, 2.00, &
         1.00, 2.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 2.00, &
         1.00, 3.00, 1.00,-1.00, 2.00, 1.00, 2.00, 1.00, 1.00,-1.00, &
         3.00, 1.00,-1.00, 2.00, 1.00, 2.00, 1.00, 1.00,-1.00, 3.00, &
         1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 2.00, 1.00, 2.00, 1.00, &
         1.00, 1.00, 1.00, 2.00, 1.00, 1.00, 1.00, 1.00, 2.00, 2.00, &
         -1.00, 3.00, 2.00, 1.00, 1.00, 2.00, 1.00, 1.00, 3.50, 2.00, &
         1.00, 1.00, 3.00, 1.00, 1.00, 1.00, 1.00, 1.00, 2.00, 2.00, &
         3.00, 1.00, 3.00, 1.00, 1.00,-1.00, 4.00, 2.00, 1.00, 1.00, &
         2.00, 1.00, 1.00, 3.00, 1.00, 3.00, 1.00, 1.00, 1.00, 1.00, &
         1.00, 2.00, 2.00, 2.00, 1.00, 1.00, 2.00, 2.00, 1.00, 3.00, &
         1.00, 1.00, 4.00, 1.00, 3.00, 1.00, 1.00, 4.00, 1.00, 5.00, &
         3.00, 1.00, 1.00, 4.00, 1.00, 2.00, 1.00, 1.00, 1.00, 3.00, &
         2.00, 4.00, 1.00, 1.00, 6.00, 5.00, 1.00, 3.00, 1.00, 1.00, &
         1.00 /)
    konco_con(:)=(/  &
         'p1   ','o1   ','s2   ','o1   ','m2   ','n2   ','s2   ','n2   ','s2   ','m2   ', &
         's2   ','n2   ','k2   ','s2   ','m2   ','n2   ','k2   ','s2   ','m2   ','s2   ', &
         'k2   ','o1   ','k2   ','n2   ','s2   ','s2   ','n2   ','k2   ','o1   ','p1   ', &
         'm2   ','k2   ','s2   ','m2   ','k2   ','s2   ','s2   ','n2   ','m2   ','k2   ', &
         's2   ','k2   ','m2   ','s2   ','n2   ','k2   ','m2   ','s2   ','n2   ','s2   ', &
         'm2   ','m2   ','s2   ','n2   ','s2   ','k2   ','m2   ','s2   ','n2   ','n2   ', &
         'o1   ','m2   ','o1   ','n2   ','k1   ','s2   ','o1   ','m2   ','k1   ','s2   ', &
         'p1   ','s2   ','k1   ','m2   ','n2   ','s2   ','n2   ','m2   ','s2   ','m2   ', &
         's2   ','n2   ','k2   ','m2   ','n2   ','m2   ','s2   ','k2   ','m2   ','n2   ', &
         'k2   ','s2   ','m2   ','m2   ','k2   ','s2   ','s2   ','n2   ','k2   ','n2   ', &
         'm2   ','s2   ','m2   ','k2   ','s2   ','l2   ','s2   ','s2   ','k2   ','m2   ', &
         'n2   ','o1   ','m2   ','o1   ','m2   ','p1   ','m2   ','n2   ','k1   ','m2   ', &
         'p1   ','m2   ','k1   ','m2   ','s2   ','k1   ','k2   ','k1   ','m2   ','s2   ', &
         'k1   ','n2   ','k2   ','s2   ','n2   ','m2   ','n2   ','m2   ','k2   ','s2   ', &
         'm2   ','s2   ','k2   ','m2   ','n2   ','m2   ','n2   ','k2   ','s2   ','m2   ', &
         'm2   ','s2   ','n2   ','m2   ','k2   ','n2   ','m2   ','s2   ','m2   ','k2   ', &
         'n2   ','s2   ','k2   ','s2   ','m2   ','m2   ','s2   ','k2   ','m2   ','s2   ', &
         'k2   ','s2   ','m2   ','n2   ','o1   ','n2   ','m2   ','k1   ','m2   ','m2   ', &
         's2   ','o1   ','m2   ','k1   ','m2   ','s2   ','k2   ','o1   ','m2   ','n2   ', &
         'm2   ','n2   ','m2   ','n2   ','k2   ','s2   ','m2   ','m2   ','s2   ','n2   ', &
         'm2   ','n2   ','k2   ','m2   ','s2   ','m2   ','k2   ','m2   ','s2   ','n2   ', &
         'k2   ','m2   ','s2   ','m2   ','s2   ','k2   ','m2   ','n2   ','k1   ','m2   ', &
         'n2   ','k1   ','m2   ','k1   ','m2   ','s2   ','k1   ','m2   ','n2   ','m2   ', &
         'm2   ','n2   ','s2   ','m2   ','s2   ','m2   ','n2   ','s2   ','k2   ','m2   ', &
         's2   ','m2   ','s2   ','k1   ','m2   ','m2   ','s2   ','m2   ','n2   ','k2   ', &
         's2   '/)
  end subroutine vuf_set_parameters
  !/ ------------------------------------------------------------------- /
  subroutine vuf (konx,vx,ux,fx,itime)
    !
    !
    !* given constituent konx , the nodal corrections v+u and f are returned
    ! that subroutine can now be replaced by look-up table to get k from j
    !
    !
    implicit none
    !
    character*5, intent(in)      :: konx
    real(kind=8), intent(out)    :: vx,ux,fx
    integer,      intent(in)     :: itime
    integer          :: k
    do  k=1,ntotal_con
      if (tidecon_allnames(k).eq.konx) go to 40
    end do
    write(ndset,30) konx
30  format('error in vuf: stop.',a5)
    stop
40  vx=v_arg(k,itime)
    ux=u_arg(k,itime)
    fx=f_arg(k,itime)
    return
    !
    !***********************************************************************
    !*  the astronomical arguments and their rates of change,
    !*  s0,h0,p0,enp0,pp0,ds,dh,dp,dnp,dpp,  are read from two records in
    !*  the format(5f13.10):
    !*     s0  = mean longitude of the moon (cycles) at 000 et 1/1/1976.
    !*     h0  = mean longitude of the sun.
    !*     p0  = mean longitude of the lunar perigee.
    !*     enp0= negative of the mean longitude of the ascending node.
    !*     pp0 = mean longitude of the solar perigee (perihelion).
    !*     ds,dh,dp,dnp,dpp are their respective rates of change over a 365
    !*     day period as of 000 et 1/1/1976.
    !
  end subroutine vuf
  !/ ------------------------------------------------------------------- /
  subroutine opnvuf(filename)
    implicit none
    character*256, intent(in)     :: filename
    integer                  :: j, j1, jbase, j4, k, k1, jl, jlm, kr
    double precision, parameter   :: twopi=3.1415926535898*2.
    double precision, parameter   :: fac=twpi/360.
    kr=8
    !
    !***********************************************************************
    !*  here the main constituents and their doodson numbers are read in
    !*  format (6x,a5,1x,6i3,f5.2,i4). the values are respectively
    !*     tidecon_allnames    = constituent name
    !*  ii,jj,kk,ll,mm,nn = the six doodson numbers
    !*     semi   = phase correction
    !*     nj     = the number of satellites for this constituent.
    !*  the end of all main constituents is denoted by a blank card.
    !
    allocate(tidecon_allnames(170))
    allocate(ii(mc2),jj(mc2),kk(mc2),ll(mc2),mm(mc2),nn(mc2), &
         semi(mc2), nj(170))
    allocate(konco_con(320),coef_con(320))
    kr=8
    open(unit=kr,file=filename,status='old',form='formatted')
    jbase=0
    do k=1,1000
      read(kr,60)tidecon_allnames(k),ii(k),jj(k),kk(k),ll(k),mm(k),nn(k),semi(k), &
           nj(k)
60    format(6x,a5,1x,6i3,f5.2,i4)
      !write(995,'(i4,a5,1x,6i3,f5.2,i4)') k,tidecon_allnames(k),ii(k),jj(k),kk(k),ll(k),mm(k),nn(k),semi(k), &
      !           nj(k)
      if (tidecon_allnames(k).eq.kblank) go to 100
70    j1=jbase+1
      if (nj(k).lt.1) then
        nj(k)=1
        jl=j1
        ph(j1)=0.
        ee(j1)=0.
        ldel(j1)=0
        mdel(j1)=0
        ndel(j1)=0
        ir(j1)=0
      else
        jl=jbase+nj(k)
        !
        !***********************************************************************
        !*  if nj>0, information on the satellite constituents is read , three
        !*  satellites per card, in the format (11x,3(3i3,f4.2,f7.4,1x,i1,1x)).
        !*  for each satellite the values read are
        !*     ldel,mdel,ndel = the changes in the last three doodson numbers
        !*                  from those of the main constituent.
        !*     ph     = the phase correction
        !*     ee     = the amplitude ratio of the satellite tidal potential to
        !*            that of the main constituent.
        !*     ir     = 1 if the amplitude ratio has to be multiplied by the
        !*            latitude correction factor for diurnal constituents
        !*            2 if the amplitude ratio has to be multiplied by the
        !*            latitude correction factor for semi-diurnal consti-
        !*            tuents.
        !*            otherwise if no correction is required to the amplitude
        !*            ratio.
        !
        read(kr,80)(ldel(j),mdel(j),ndel(j),ph(j),ee(j),ir(j),j=j1,jl)
80      format((11x,3(3i3,f4.2,f7.4,1x,i1,1x)))
      end if
      jbase=jl
    end do
100 ntidal_con=k-1
    jlm=jl
    !
    !***********************************************************************
    !*  the shallow water constituents and the main constituents from which
    !*  they are derived are read in here with the format
    !*  (6x,a5,i1,2x,4(f5.2,a5,5x)). the values are respectively
    !*     tidecon_allnames    = name  of the shallow water constituent
    !*     nj     = number of main constituents from which it is derived.
    !*     coef_con,konco_con = combination number and name of these main
    !*              constituents.
    !*  the end of these constituents is denoted by a blank card.
    !
    jbase=0
    k1=ntidal_con+1
    do  k=k1,1000
      j1=jbase+1
      j4=j1+3
      read(kr,130)tidecon_allnames(k),nj(k),(coef_con(j),konco_con(j),j=j1,j4)
130   format(6x,a5,i1,2x,4(f5.2,a5,5x))
      !write(995,130)tidecon_allnames(k),nj(k),(coef_con(j),konco_con(j),j=j1,j4)
      if (tidecon_allnames(k).eq.kblank) go to 170
      jbase=jbase+nj(k)
    end do
170 ntotal_con=k-1
    !  write out  for cut and paste ...
    !    write(6,*) 'numbers:',ntidal_con, ntotal_con, jlm, j1, j4
    !    write(996,*) ntidal, ntotal_con, jlm, j1, j4
    !999      format(10("'",a5,"',"),' &')
    !        write(996,999) tidecon_allnames(1:ntotal_con)
    !998      format(10(i3,","),' &')
    !997      format(10(i4,","),' &')
    !991      format(10(f5.2,","),' &')
    !990      format(10(f7.4,","),' &')
    !        write(996,998) ii(1:ntidal_con)
    !        write(996,998) jj(1:ntidal_con)
    !        write(996,998) kk(1:ntidal_con)
    !        write(996,998) ll(1:ntidal_con)
    !        write(996,998) mm(1:ntidal_con)
    !        write(996,998) nn(1:ntidal_con)
    !        write(996,991) semi(1:ntidal_con)
    !        write(996,997) nj(1:ntotal_con)
    !        write(996,998) ldel(1:jlm)
    !        write(996,998) mdel(1:jlm)
    !        write(996,998) ndel(1:jlm)
    !        write(996,991) ph(1:jlm)
    !        write(996,990) ee(1:jlm)
    !        write(996,998) ir(1:jlm)
    !        write(996,991) coef_con(1:j4)
    !        write(996,999) konco_con(1:j4)
    return
    !
    !***********************************************************************
    !*  ntidal_con is the number of main constituents
    !*  ntotal_con is the number of constituents (main + shallow water)
    !*  for  the given time hr, the table of f and v+u values is
    !*  calculated for all the constituents.
    !*     f is the nodal modulation adjustment factor for amplitude
    !*     u is the nodal modulation adjustment factor for phase
    !*     v is the astronomical argument adjustment for phase.
    !
    !      setvuf calculates the v,u,f values at time hr for all constituents
    !
  end subroutine opnvuf
  !/ ------------------------------------------------------------------- /
  subroutine setvuf(hr,xlat,itime)
    !      setvuf calculates the v,u,f values at time hr for all constituents
    implicit none
    real(kind=8), intent(in)  :: hr
    real,         intent(in)  :: xlat
    integer,      intent(in)  :: itime
    !
    !   local variables
    !
    integer                   :: kd0, int24, intdys, &
         jbase, j, k, l, j1, k1, jl, lk, iflag
    integer                   :: iv, iuu
    !
    real(kind=4), parameter   :: pi=3.1415926536
    real(kind=4), parameter   :: twopi=2.*3.1415926536
    !
    real                      :: slat, vdbl, vv, sumc, sums, rr, &
         uudbl, uu, cxlat
    real(kind=8)              :: d1,h,pp,s,p,enp,dh,dpp,ds,dp,dnp,hh,tau
    integer                   :: indx(170)
    cxlat = max(abs(xlat), 5.)
    slat=sin(pi*cxlat/180.)
    !
    !***********************************************************************
    !*  the astronomical arguments are calculated by linear approximation
    !*  at the mid point of the analysis period.
    !
    !      day number measured from january 0.5 1900 (i.e.,
    !      1200 ut december 31, 1899
    d1=hr/24.d0
    ! this was with "gregorian days from kday"
    !call gday(31,12,99,18,kd0)   !  call gday(idd,imm,iyy,icc,kdd)
    ! now uses "julian days from juldayt"
    ! kd0= 693961
    !kd0=juldayt(31,12,1899)   ! juldayt(id,mm,iyyy)
    kd0= 2415020
    ! substracting 0.5day is not necessary anymore with new time functions
    d1=d1-dfloat(kd0)
    call astr(d1,h,pp,s,p,enp,dh,dpp,ds,dp,dnp)
    int24=24
    intdys=int((hr+0.00001)/int24)
    hh=hr-dfloat(intdys*int24)
    tau=hh/24.d0+h-s
    !
    !***********************************************************************
    !*  only the fractional part of a solar day need be retained for compu-
    !*  ting the lunar time tau.
    !
    jbase=0
    do  k=1,ntidal_con
      do l=1,tide_mf
        if (tidecon_allnames(k).eq.tidecon_name(l)) then
          indx(k)=l
        end if
      end do
      vdbl=ii(k)*tau+jj(k)*s+kk(k)*h+ll(k)*p+mm(k)*enp+nn(k)*pp+semi(k)
      iv=vdbl
      iv=(iv/2)*2
      vv=vdbl-iv
      j1=jbase+1
      jl=jbase+nj(k)
      sumc=1.
      sums=0.
      do j=j1,jl
        !
        !***********************************************************************
        !*  here the satellite amplitude ratio adjustment for latitude is made
        !
        rr=ee(j)
        l=ir(j)+1
        if (l.eq.2) then
          rr=ee(j)*0.36309*(1.-5.*slat*slat)/slat
        else if (l.eq.3) then
          rr=ee(j)*2.59808*slat
        end if
        uudbl=ldel(j)*p+mdel(j)*enp+ndel(j)*pp+ph(j)
        iuu=uudbl
        uu=uudbl-iuu
        sumc=sumc+rr*cos(uu*twopi)
        sums=sums+rr*sin(uu*twopi)
      end do
      f_arg(k,itime)=sqrt(sumc*sumc+sums*sums)
      v_arg(k,itime)=vv
      u_arg(k,itime)=atan2(sums,sumc)/twopi
      jbase=jl
    end do
    !
    !***********************************************************************
    !*  here f and v+u of the shallow water constituents are computed from
    !*  the values of the main constituent from which they are derived.
    !
    jbase=0
    k1=ntidal_con+1
    if (k1.gt.ntotal_con) return
    !
    do k=k1,ntotal_con
      f_arg(k,itime)=1.0
      v_arg(k,itime)=0.0
      u_arg(k,itime)=0.
      iflag=0
      do lk=1,tide_mf
        if (tidecon_allnames(k).eq.tidecon_name(lk)) then
          iflag=1
          exit
        end if
      end do   ! lk
      do j=tide_indexj(k),tide_indexj(k)+nj(k)-1
        l=tide_indexjk(j)
        f_arg(k,itime)=f_arg(k,itime)*f_arg(l,itime)**abs(coef_con(j))
        v_arg(k,itime)=v_arg(k,itime)+coef_con(j)*v_arg(l,itime)
        u_arg(k,itime)=u_arg(k,itime)+coef_con(j)*u_arg(l,itime)
      end do   ! j
    end do     ! k
    !  test output for verification purposes
    if (itime.eq.-1) then
      write(992,'(a,f20.2,13f8.3)') 'test isea 0:',    &
           d1,h,s,tau,pp,s,p,enp,dh,dpp,ds,dp,dnp,xlat
      do l=1,tide_mf
        do k=1,ntotal_con
          if (tidecon_allnames(k).eq.tidecon_name(l)) then
            tide_index(l)=k
            write(992,'(a,4i9,f12.0,3f8.3,i4,x,a)') 'test isea 1:',1,l,20071201,0,hr,    &
                 f_arg(k,itime),u_arg(k,itime),v_arg(k,itime),tide_index(l),tidecon_name(l)
          end if
        end do
      enddo
    endif
    return
    !
  end subroutine setvuf
  !/ ------------------------------------------------------------------- /
  subroutine flex_tidana_webpage(ix,iy,xlon,xlat,kd1,kd2,ndef, itrend, res, ssq, rmsr0, sdev0, &
       rmsr, resmax, imax, itest)
    !
    !***********************************************************************
    !*
    !*  this program does a tidal heights 'harmonic' analysis of irregularly
    !*  sampled observations.  the analysis method is a least squares fit
    !*  using svd coupled with nodal modulation and inference(if so requested).
    !*
    !*      the code is based on topex analysis code originally developed by josef
    !*      cherniawsky (jaot, 2001, 18(4): 649-664) and modified by rob bell and
    !*      mike foreman. enhancements to that version include
    !*
    !*      1. provision for multi-constituent inferences computed directly within
    !*        the least squares matrix rather than as post fit corrections. this
    !*        means that the inferred constituents will affect all constituents, not
    !*        just the reference constituent.
    !*
    !*      2. an extension to permit the analysis of current observations.
    !*
    !*      3. removal of a central time as the basis for the calculation of the
    !*        astronomical arguments v. now the v value for each observation, as well
    !*        as those for the nodal corrections f and u (done for the jaot analysis),
    !*        are incorporated directly into the overdetermined matrix. these changes
    !*        mean that analyses no longer need be restricted to periods of a year or
    !*        less. (though as the period approaches 18.6 years, using another "long
    !*        period" analysis program that solves for the "nodal satellites" directly
    !*        is advisable.)
    !*
    !***********************************************************************
    !*
    !*  file reference numbers of devices required by this program.
    !*      kr  - input file  - contains the tidal constituent information.
    !*      kr1 - input file  - gives analysis type and tidal station
    !*                          details.
    !*      kr2 - input file  - contains the observed times and heights.
    !*  presently kr,kr1,kr2, and lp are assigned the respective values
    !*  8,5,9, and 6.  see the manual or comment statements within this
    !*  program for further details on their use.
    !*
    !***********************************************************************
    !*
    !*  array definitions and dimension guidelines.
    !*
    !*  let    mc      be the total number of constituents, including z0
    !*                 and any inferred constituents, to be included in the
    !*                 analysis; (for t/p, mc=30 > number of constituents)
    !*         tide_nti    be the number of tidal height observations;
    !*         nr      be the number of input records of observed tidal
    !*                 heights (for t/p data, same as tide_nti, set to 200)
    !*         mpar    be 2*mc-1;
    !*         neq     be tide_nti*2 if all the observations are extremes and
    !*                 the derivative condition is to be included for each,
    !*                 and tide_nti otherwise (= nr for t/p data).
    !*  then parameters nmaxp1, and nmaxpm should be at least mpar+1, and
    !*  neq+mpar respectively.  they are currently set to 40 and 240.
    !*
    !*  tidecon_name(i) is the array containing all the constituent names,
    !*                 including z0 and any inferred constituents, to be in
    !*                 the analysis.  it should be dimensioned at least mc.
    !*  tide_freqc(i),      are the arrays of frequencies in cycles/hr and
    !*  freq(i)        radians/hr respectively corresponding to the
    !*                 constituent name(i).  they should be dimensioned at
    !*                 least mc.
    !*  amp(i),ph(i)   are arrays containing the raw amplitude and phase for
    !*                 constituent name(i) as found via the least squares
    !*                 analysis.  they should be dimensioned at least mc.
    !*  ampc(i),phg(i) are arrays containing the amplitude and phase for
    !*                 constituent name(i) after corrections for nodal
    !*                 modulation, astronomical argument and inferred
    !*                 constituents.  their minimum dimension should be mc.
    !*  tide_data(i)   and heights, of the observed data as it is input by
    !*                 record.  they should be dimensioned accordingly( at
    !*                 present only 6 observations are expected per record).
    !*  x(i)           array  containing all the times(in hours as
    !*                 measured from the centre of the analysis period) its minimum
    !*                 dimension should be tide_nti.
    !*  nstn(i)        is the array containing the tidal station tidecon_name.  it
    !*                 should have minimum dimension 5.
    !*  q(i)           is the overdetermined array of equations that is
    !*                 solved in the least squares sense by the modified
    !*                 gram-schmidt algorithm.  it should have the exact
    !*                 dimension of nmaxpm by nmaxp1.
    !*  p(i)           is the array containing the tidal constituent sine
    !*                 and cosine coeficients as found with the least
    !*                 squares fit.  it should have minimum dimension mpar.
    !
    !***********************************************************************
    !
    implicit none
    integer, intent(in)         :: ndef, itrend, itest
    integer(kind=4), intent(in) :: kd1, kd2, ix, iy
    real           , intent(in) :: xlon, xlat
    real(kind=8)   , intent(out):: sdev0(ndef), rmsr0(ndef), rmsr(ndef), resmax(ndef)
    real           , intent(out):: ssq(ndef), res(ndef)
    integer        , intent(out):: imax(ndef)
    !
    integer                     :: i, i1, i2, i21, ii1, idef, icode,  inflag,  &
         j, inftot, irep, j2, jcode, jj1, k, k2, kh1,     &
         kh2, khm, kinf, l, m, meq, n, ncol, new, nmax
    real(kind=8)                :: aamp, arg, arg1, arg2, arg3, c, c2, c3,     &
         fx, fxi,  s, s2, s3, ux, vx, uxi, vxi,  &
         wmin, wmax, xmid
    real                        :: toler
    real(kind=8)                :: av, sdev, sum2, hrm
    double precision            :: x(nr),y(nr), time(nr)
    real                        :: q(nmaxpm,nmaxp1),freq(mc),amp(mc),ph(mc)
    double precision            :: p(nmaxp1),cenhr,cumhr
    double precision            :: yy
    !
    !     additional arrays, for use in the svd routine (j.ch., aug. 1997)
    double precision     :: u(nmaxpm,nmaxp1),v(nmaxp1,nmaxp1),      &
         cov(nmaxp1,nmaxp1),b(nmaxpm),w(nmaxp1),sig(nmaxpm)
    !
    !***********************************************************************
    !
    kh1=24*kd1
    kh2=24*(kd2+1)
    khm=(kh1+kh2)/2
    hrm=khm
    cenhr=dfloat((kh2-kh1)/2)
    if (itrend.eq.1) then
      m=2*tide_mf
    else
      m=2*tide_mf-1
    end if
    i=0
    do i=1,tide_mf
      freq(i)=tide_freqc(i)*twpi
    end do
    !
    !***********************************************************************
    !*  determine the central hour of the analysis period and set up the
    !*  dependent and independent variables, y and x.
    ! actually, cumhr=24.d0*(kd-khm) (check), but keep same notation as before
    !
    k=1
    do i=1,tide_nti
      cumhr=-cenhr+24.d0*(tide_days(i)-kd1)
      time(i)=cumhr+dfloat(tide_secs(i))/3600.d0
      x(k)=time(i)-time(1)
      n=k
      k=k+1
    end do
    !
    !***********************************************************************
    !*  setting up the overdetermined matrix and solving with modified svd
    !
    irep=0
    do idef=1,ndef   ! loop thru once or twice
      ! modifies the time reference xmid and puts it at 0 : modification by fa, 2012/09/26
      ! the impact of that change has not been verified ...
      xmid=0. !0.5*(tide_hours(1)+tide_hours(tide_nti))
      q(1:nmaxpm,1:nmaxp1)=0.0
      do i=1,n
        !      if itrend=1, then
        !      first 2 parameters are constant and linear trend (per 365 days)
        !      fitted as const+trend(t-tmid) where tmid (=xmid) is the middle time
        !      of the analysis period (this makes the constant consistent with z0
        !      in the old analysis program)
        !      if itrend=0 then the second parameter is is associated with the next
        !      constituent
        q(i,1)=1.
        if (itrend.eq.1) then
          !            q(i,2)=(x(i)-xmid)/(24.*365.)
          q(i,2)=x(i)/(24.*365.)
        end if
        q(i,nmaxp1)=tide_data(i,idef)
        icode=1
        !      should only have to assemble lhs of matrix when idef=1
        !      but something is not right if don't do it 2nd time too
        ! call setvuf(tide_hours(i),xlat,i)
        do j=2,tide_mf
          call vuf(tidecon_name(j),vx,ux,fx,i)
          !      check to see if this constituent is to be used for inference
          inflag=0
          kinf=0
          if (tide_nin.ge.0) then
            do k=1,tide_nin
              if (tidecon_name(j).eq.tide_konan(k)) then
                inflag=1
                kinf=k
                exit
              end if
            end do
          end if
          !
          if (inflag.eq.0) then
            arg=(vx+ux)*twpi
            if (itrend.eq.1) then
              j2=2*(j-1)+1
            else
              j2=2*(j-1)
            end if
            jj1=j2+1
            q(i,j2)=cos(arg)*fx
            q(i,jj1)=sin(arg)*fx
          else
            if (itrend.eq.1) then
              j2=2*(j-1)+1
            else
              j2=2*(j-1)
            end if
            jj1=j2+1
            arg1=(vx+ux)*twpi
            q(i,j2)=cos(arg1)*fx
            q(i,jj1)=sin(arg1)*fx
            do k2=1,tide_ninf(kinf)
              call vuf(tide_konin(kinf,k2),vxi,uxi,fxi,i)
              !      freq is radians/hr but sigin is cycles/hr
              arg2=(vxi+uxi)*twpi
              c2=cos(arg2)
              s2=sin(arg2)
              arg3=tide_zeta(kinf,k2)*fac
              c3=cos(arg3)
              s3=sin(arg3)
              q(i,j2)=q(i,j2)+fxi*tide_r(kinf,k2)*(c2*c3-s2*s3)
              q(i,jj1)=q(i,jj1)+fxi*tide_r(kinf,k2)*(c2*s3+s2*c3)
            end do
          end if
        end do ! j
      end do  !i
      nmax=m
      meq=n
      ssq(idef)=1.0
      res(idef)=1.0
      ncol=nmax
      new=nmax
      !
      !***********************************************************************
      !*  calculation of the standard deviation of the right hand sides of
      !*  the overdetermined system
      !
      av=0.d0
      do i=1,meq
        av=av+q(i,nmaxp1)
      end do
      av=av/meq
      sdev=0.d0
      do i=1,meq
        sdev=sdev+(q(i,nmaxp1)-av)**2
      end do
      sdev=sdev/(meq-1)
      sdev=sqrt(sdev)
      sdev0(idef)=sdev
109   continue
      !
      !   use singular-value-decomposition to solve the overdetermined system
      !
      toler=1.e-5
      do i=1,nmaxpm
        sig(i)=1.d0
      end do
      !
      !      no solution if meq lt m. ie underdetermined system
      !      go to next time series
      if (meq.le.m) then
        write(ndset,*) ' underdetermined system: no svd solution',ix,iy,meq,m
        stop
      end if
      call svd(q,u,v,cov,w,p,b,sig,icode,meq,nmax,nmaxpm,nmaxp1,toler     &
           ,jcode,ssq(idef),res(idef))
      !        if (jcode.gt.0) write(lp,55)jcode
      !   55 format('column',i5,' is the 1st dependent columns in svd')
      !      write out eigenvalues
      wmax=-1000.
      wmin=1000.
      do i=1,nmax
        if (w(i).gt.wmax) wmax=w(i)
        if (w(i).lt.wmin) wmin=w(i)
      end do
      !        write(6,*) ' max, min eigenvalues =',wmax,wmin
      !      write(6,*) ' all eigenvalues'
      !      write(6,56) (w(i),i=1,nmax)
56    format(10e12.5)
      !***********************************************************************
      if (ssq(idef).gt.1.e-10) then
        rmsr0(idef)=sqrt(ssq(idef)/(meq-m))
      else
        rmsr0(idef)=0.
      end if
      rmsr(idef)=0.d0
      resmax(idef)=0.
      do  i=1,n
        yy=q(i,nmaxp1)
        rmsr(idef)=rmsr(idef)+yy*yy
        if (abs(yy).gt.resmax(idef)) then
          resmax(idef)=abs(yy)
          imax(idef)=i
        end if
      end do
160   format(' ',7i2,f15.5,f10.5,i6)
      if (rmsr(idef).gt.1.e-10) then
        rmsr(idef)=dsqrt(rmsr(idef)/(n-m))
      else
        rmsr(idef)=0.
      end if
      !      close(unit=25)
      !
      !***********************************************************************
      !*  calculate amplitudes and phases
      !
      !      if itrend=1 then the linear trend is shown as the phase of the constant
      !      z0 term (& the true phase of z0 is zero)
      !      otherwise, the phase of z0 is shown as zero
      amp(1)=p(1)
      if (itrend.eq.1) then
        ph(1)=p(2)
      else
        ph(1)=0.
      end if
      do i=2,tide_mf
        !
        if (itrend.eq.1) then
          i2=2*(i-1)+1
        else
          i2=2*(i-1)
        end if
        i21=i2+1
        c=p(i2)
        s=p(i21)
        aamp=sqrt(c*c+s*s)
        if (aamp.lt.1.e-5) then
          ph(i)=0.
        else
          ph(i)=atan2(s,c)/fac
          if (ph(i).lt.0.) ph(i)=ph(i)+360.
        end if
        amp(i)=aamp
      end do  ! end of loop on tide_mf
      !***********************************************************************
      !      note that with f & u included in the lsq fit, we only need v from routine vuf
      !      but we don't want to correct with v for a central hour. better to include
      !      the right v in the lsq fit. this has been done.
      tide_ampc(1,idef)=amp(1)
      tide_phg(1,idef)=ph(1)
      do i=2,tide_mf
        tide_ampc(i,idef)=amp(i)
        tide_phg(i,idef)=ph(i)
      end do
      !
      tide_sig3(:,idef)=0.
      tide_ttest(:,idef)=0.
      !
      if (itest.ge.1) then
        !---------------------------------------------------
        !
        i=1
        if (cov(1,1).gt.1.e-8) then
          tide_sig1(i,idef)=sqrt(cov(1,1))*rmsr0(idef)
        else
          tide_sig1(i,idef)=0.
        end if
        if (itrend.eq.1.and.cov(2,2).gt.1.e-8) then
          tide_sig2(i,idef)=sqrt(cov(2,2))*rmsr0(idef)
        else
          tide_sig2(i,idef)=0.
        end if
        tide_sig3(i,idef)=0.
        tide_ttest(i,idef)=0.
        !
        !    results for the other constituents
        !
        do i=2,tide_mf
          if (itrend.eq.1) then
            i2=2*(i-1)+1
          else
            i2=2*(i-1)
          end if
          ii1=i2+1
          !
          !      multiply cov values with residual standard deviation, as described in equation
          !      (6) of cherniasky et al. (2001)
          !
          if (cov(i2,i2).gt.1.e-8) then
            tide_sig1(i,idef)=sqrt(cov(i2,i2))*rmsr0(idef)
          else
            tide_sig1(i,idef)=0.
          end if
          if (cov(ii1,ii1).gt.1.e-8) then
            tide_sig2(i,idef)=sqrt(cov(ii1,ii1))*rmsr0(idef)
          else
            tide_sig2(i,idef)=0.
          end if
          !      from equation 11 in pawlowicz et al (2002)
          c=tide_ampc(i,idef)*cos(tide_phg(i,idef)*fac)
          s=tide_ampc(i,idef)*sin(tide_phg(i,idef)*fac)
          tide_sig3(i,idef)=sqrt(((c*tide_sig1(i,idef))**2+(s*tide_sig2(i,idef))**2)/(c**2+s**2))
          tide_ttest(i,idef)=tide_ampc(i,idef)/tide_sig3(i,idef)
        end do
        !---------------------------------------------------
      end if ! (itest.ge.1)
      !
      !  now inferred constituents
      !
      if (tide_nin.ge.0) then
        l=0
        do k=1,tide_nin
          do i=2,tide_mf
            if (tidecon_name(i).eq.tide_konan(k)) exit
          end do
          i1=i
          do k2=1,tide_ninf(k)
            l=l+1
            tide_ampci(k,k2,idef)=tide_ampc(i1,idef)*tide_r(k,k2)
            tide_phgi(k,k2,idef)=tide_phg(i1,idef)-tide_zeta(k,k2)
          end do
        end do
        inftot=l
      end if
      !
      !***********************************************************************
      !      compute (cherniawsky et al (2001), page 653) and rank correlation coefficients
      !      largest niter value are computed and shown
      !      if itrend=1, then the second part of z0 is the linear trend coefficient
      !
      !      do i=1,m
      !        do j=1,i
      !          cor(i,j)=cov(i,j)/sqrt(cov(i,i)*cov(j,j))
      !          end do
      !        end do
      !
      !      niter=20
      !      do 81 iter=1,niter
      !        cormax=0.
      !        do i=2,m
      !          im1=i-1
      !          do j=1,im1
      !            ac=abs(cor(i,j))
      !            if (ac.gt.cormax) then
      !              cormax=ac
      !              imax=i
      !              jmax=j
      !             end if
      !            end do
      !          end do
      !        if (itrend.eq.1) then
      !          iconst=(imax+1)/2
      !          jconst=(jmax+1)/2
      !        else
      !          iconst=(imax+2)/2
      !          jconst=(jmax+2)/2
      !          end if
      !        write(lp,83) iter,cormax,imax,jmax,tidecon_name(iconst),tidecon_name(jconst)
      !83      format(i5,' largest correlation coefficient is ',f8.3,' at (i,j)='     &
      !           ,2i5,' for constituents ',a5,' and ',a5)
      !        cor(imax,jmax)=0.
      !        end do
      !
    end do  ! end of loop on idef
    return
  end subroutine flex_tidana_webpage
  !/ ------------------------------------------------------------------- /
  subroutine tide_predict(itrend,ndef,n,hours, datain, predicted, resid, xlat,sdev,rmsr)
    !
    implicit none
    !
    integer,       intent(in)      :: itrend, ndef, n
    real(kind=8)  , intent(in)     :: hours(n)
    real,          intent(in)      :: xlat, datain(n,ndef)
    real(kind=8),  intent(out)     :: sdev(ndef),rmsr(ndef)
    real,          intent(out)     :: predicted(n,ndef), resid(n,ndef)
    !
    integer                        :: idef, i, k, k2
    integer                        :: j, m
    real                           :: arg, add, sum1, ssq
    real(kind=8)                   :: vx, ux, fx
    m = 2*tide_mf
    do idef=1,ndef
      sdev=0.d0
      do i=1,n
        if (itrend.eq.1) then
          sum1=tide_ampc(1,idef)+tide_phg(1,idef)*hours(i)/(365.*24.)
        else
          sum1=tide_ampc(1,idef)
        end if
        !
        do j=2,tide_mf
          call vuf(tidecon_name(j),vx,ux,fx,i)
          arg=(vx+ux)*twpi-tide_phg(j,idef)*fac
          add=fx*tide_ampc(j,idef)*cos(arg)
          sum1=sum1+add
        end do
        !
        if (tide_nin.ne.0) then
          do k=1,tide_nin
            do k2=1,tide_ninf(k)
              call vuf(tide_konin(k,k2),vx,ux,fx,i)
              arg=(vx+ux)*twpi-tide_phgi(k,k2,idef)*fac
              add=fx*tide_ampci(k,k2,idef)*cos(arg)
              sum1=sum1+add
            end do
          end do
        end if
        !
        predicted(i,idef)=sum1
        !
        resid(i,idef)=datain(i,idef)-sum1
        sdev(idef)=sdev(idef)+resid(i,idef)**2
      end do
      !
      ssq=sdev(idef)
      rmsr(idef)=sqrt(ssq/(n-m))
      sdev(idef)=sqrt(ssq/n)
    enddo
    !
  end subroutine tide_predict
  !/ ------------------------------------------------------------------- /
  subroutine tide_predict_only(itrend,ndef,n,tide_hours, predicted, xlat)
    !
    implicit none
    !
    integer,       intent(in)      :: itrend, ndef, n
    real(kind=8)  , intent(in)     :: tide_hours(n)
    real,          intent(in)      :: xlat
    real,          intent(out)     :: predicted(n,ndef)
    !
    integer                        :: idef, i, k, k2
    integer                        :: j
    real                           :: arg, add, sum1
    real(kind=8)                   :: vx, ux, fx
    do idef=1,ndef
      do i=1,n
        if (itrend.eq.1) then
          sum1=tide_ampc(1,idef)+tide_phg(1,idef)*tide_hours(i)/(365.*24.)
        else
          sum1=tide_ampc(1,idef)
        end if
        !
        do j=2,tide_mf
          call vuf(tidecon_name(j),vx,ux,fx,i)
          arg=(vx+ux)*twpi-tide_phg(j,idef)*fac
          add=fx*tide_ampc(j,idef)*cos(arg)
          sum1=sum1+add
        end do
        !
        if (tide_nin.ne.0) then
          do k=1,tide_nin
            do k2=1,tide_ninf(k)
              call vuf(tide_konin(k,k2),vx,ux,fx,i)
              arg=(vx+ux)*twpi-tide_phgi(k,k2,idef)*fac
              add=fx*tide_ampci(k,k2,idef)*cos(arg)
              sum1=sum1+add
            end do
          end do
        end if
        !
        predicted(i,idef)=sum1
        !
      end do
      !
    enddo
    !
  end subroutine tide_predict_only
  !/
  !/ end of module wmtidemd -------------------------------------------- /
  !/
end module w3tidemd
