!> @file w3iogoncmd
!!
!> @brief write gridded model output as netcdf
!!
!> @author mvertens@ucar.edu, denise.worthen@noaa.gov
!> @date 01-05-2022
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
module w3iogoncdmd
  use w3gdatmd      , only : nk, nx, ny, mapsf, mapsta, nsea
  use w3odatmd      , only : noswll, undef
  use w3odatmd      , only : nds, iaproc, napout
  use netcdf
  implicit none
  private
  public :: w3iogoncd
  ! used/reused in module
  integer               :: isea, ierr, ncid, varid
  integer               :: len_s, len_m, len_p, len_k
  character(len=1024)   :: fname
  real, allocatable, target :: var3ds(:,:,:)
  real, allocatable, target :: var3dm(:,:,:)
  real, allocatable, target :: var3dp(:,:,:)
  real, allocatable, target :: var3dk(:,:,:)
  real, pointer :: var3d(:,:,:)
  !===============================================================================
contains
  !===============================================================================
  subroutine w3iogoncd ()
    use w3odatmd   , only : fnmpre
    use w3gdatmd   , only : filext, trigp, ntri, ungtype, gtype
    use w3servmd   , only : extcde
    use w3wdatmd   , only : w3setw, w3dimw, time, wlv, ice, icef, iceh, berg, ust, ustdir, asf, rhoair
    use w3gdatmd   , only : xgrd, ygrd
    use w3gdatmd   , only : e3df, p2msf, us3df, usspf, w3setg
    use w3odatmd   , only : nogrp, ngrpp, idout, ndst, ndse,  noswll, w3seto
    use w3adatmd   , only : w3seta, w3dima, w3xeta
    use w3adatmd   , only : ainit, dw, ua, ud, as, cx, cy, wn, taua, tauadir
    use w3adatmd   , only : hs, wlm, t02, t0m1, t01, fp0, thm, ths, thp0, wbt, wnmean
    use w3adatmd   , only : dtdyn
    use w3adatmd   , only : fcut, aba, abd, uba, ubd, sxx, syy, sxy
    use w3adatmd   , only : phs, ptp, plp, pdir, psi, pws, pwst, pnr
    use w3adatmd   , only : pthp0, pqp, ppe, pgw, psw, ptm1, pt1, pt2
    use w3adatmd   , only : pep, usero, tauox, tauoy, tauwix, tauwiy
    use w3adatmd   , only : phiaw, phioc, tusx, tusy, prms, tpms
    use w3adatmd   , only : ussx, ussy, mssx, mssy, mssd, mscx, mscy
    use w3adatmd   , only : mscd, qp, tauwnx, tauwny, charn, tws, bhd
    use w3adatmd   , only : phibbl, taubbl, whitecap, bedforms, cge, ef
    use w3adatmd   , only : cflxymax, cflthmax, cflkmax, p2sms, us3d
    use w3adatmd   , only : th1m, sth1m, th2m, sth2m, hsig, phice, tauice
    use w3adatmd   , only : stmaxe, stmaxd, hmaxe, hcmaxe, hmaxd, hcmaxd, ussp, tauocx, tauocy
    use w3adatmd , only : usshx, usshy
    use wav_grdout , only : varatts, outvars
    use w3timemd   , only : set_user_timestring
    use w3odatmd   , only : time_origin, calendar_name, elapsed_secs
    use w3odatmd   , only : use_user_histname, user_histfname
    !todo: use unstr_mesh from wav_shr_mod; currently fails due to ci
    !use wav_shr_mod, only : unstr_mesh
    ! local variables
    integer               :: igrd
    integer    ,target    :: dimid3(3)
    integer    ,target    :: dimid4(4)
    integer    ,pointer   :: dimid(:)
    character(len=12)     :: vname
    character(len=16)     :: user_timestring    !yyyy-mm-dd-sssss
    integer :: n, xtid, ytid, xeid, ztid, stid, mtid, ptid, ktid, timid, varid
    logical :: s_axis = .false., m_axis = .false., p_axis = .false., k_axis = .false.
    !-------------------------------------------------------------------------------
    igrd   = 1
    call w3seto ( igrd, ndse, ndst )
    call w3setg ( igrd, ndse, ndst )
    call w3seta ( igrd, ndse, ndst )  ! sets pointers into wadats in w3adatmd
    call w3xeta ( igrd, ndse, ndst )  ! sets pointers into wadats in w3adatmd
    call w3setw ( igrd, ndse, ndst )  ! sets pointers into wdatas in w3wdatmd
    ! -------------------------------------------------------------
    ! create the netcdf file
    ! -------------------------------------------------------------
    if (use_user_histname) then
      if (len_trim(user_histfname) == 0 ) then
        call extcde (60, msg="user history filename requested but not provided")
      end if
      call set_user_timestring(time,user_timestring)
      fname = trim(user_histfname)//trim(user_timestring)//'.nc'
    else
      write(fname,'(a,i8.8,a1,i6.6,a)')trim(fnmpre),time(1),'.',time(2),'.out_grd.'//trim(filext)//'.nc'
    end if
    len_s = noswll + 1                  ! 0:noswll
    len_m = p2msf(3)-p2msf(2) + 1       ! ?
    len_p = usspf(2)                    ! partitions
    len_k = e3df(3,1) - e3df(2,1) + 1   ! frequencies
    ! define the dimensions required for the requested gridded fields
    do n = 1,size(outvars)
      if (outvars(n)%validout) then
        if(trim(outvars(n)%dims) == 's')s_axis = .true.
        if(trim(outvars(n)%dims) == 'm')m_axis = .true.
        if(trim(outvars(n)%dims) == 'p')p_axis = .true.
        if(trim(outvars(n)%dims) == 'k')k_axis = .true.
      end if
    end do
    ! allocate arrays if needed
    if (s_axis) allocate(var3ds(1:nx,1:ny,len_s))
    if (m_axis) allocate(var3dm(1:nx,1:ny,len_m))
    if (p_axis) allocate(var3dp(1:nx,1:ny,len_p))
    if (k_axis) allocate(var3dk(1:nx,1:ny,len_k))
    ! create the netcdf file
    ierr = nf90_create(trim(fname), nf90_clobber, ncid)
    call handle_err(ierr, 'nf90_create')
    ierr = nf90_def_dim(ncid, 'nx', nx, xtid)
    ierr = nf90_def_dim(ncid, 'ny', ny, ytid)
    ierr = nf90_def_dim(ncid, 'time', nf90_unlimited, timid)
    if (s_axis) ierr = nf90_def_dim(ncid, 'noswll', len_s, stid)
    if (m_axis) ierr = nf90_def_dim(ncid, 'nm'    , len_m, mtid)
    if (p_axis) ierr = nf90_def_dim(ncid, 'np'    , len_p, ptid)
    if (k_axis) ierr = nf90_def_dim(ncid, 'freq'  , len_k, ktid)
    if (gtype .eq. ungtype) then
      ierr = nf90_def_dim(ncid, 'ne'  , ntri, xeid)
      ierr = nf90_def_dim(ncid, 'nn'  ,    3, ztid)
    end if
    ! define the time variable
    ierr = nf90_def_var(ncid, 'time', nf90_double, timid, varid)
    call handle_err(ierr,'def_timevar')
    ierr = nf90_put_att(ncid, varid, 'units', trim(time_origin))
    call handle_err(ierr,'def_time_units')
    ierr = nf90_put_att(ncid, varid, 'calendar', trim(calendar_name))
    call handle_err(ierr,'def_time_calendar')
    ! define the spatial axis variables (lat,lon)
    ierr = nf90_def_var(ncid, 'lon', nf90_double, (/xtid,ytid/), varid)
    call handle_err(ierr,'def_lonvar')
    ierr = nf90_put_att(ncid, varid, 'units', 'degrees_east')
    ierr = nf90_def_var(ncid, 'lat', nf90_double, (/xtid,ytid/), varid)
    call handle_err(ierr,'def_latvar')
    ierr = nf90_put_att(ncid, varid, 'units', 'degrees_north')
    ! add mapsta
    ierr = nf90_def_var(ncid, 'mapsta', nf90_int, (/xtid, ytid, timid/), varid)
    call handle_err(ierr, 'def_mapsta')
    ierr = nf90_put_att(ncid, varid, 'units', 'unitless')
    ierr = nf90_put_att(ncid, varid, 'long_name', 'map status')
    if (gtype .eq. ungtype) then
      ierr = nf90_def_var(ncid, 'nconn', nf90_int, (/ztid,xeid/), varid)
      call handle_err(ierr,'def_nodeconnections')
      ierr = nf90_put_att(ncid, varid, 'units', 'unitless')
      ierr = nf90_put_att(ncid, varid, 'long_name', 'node connectivity')
    end if
    ! define the variables
    dimid3(1:2) = (/xtid, ytid/)
    dimid4(1:2) = (/xtid, ytid/)
    do n = 1,size(outvars)
      if (trim(outvars(n)%dims) == 's') then
        dimid4(3:4) = (/stid, timid/)
        dimid => dimid4
      else if (trim(outvars(n)%dims) == 'm') then
        dimid4(3:4) = (/mtid, timid/)
        dimid => dimid4
      else if (trim(outvars(n)%dims) == 'p') then
        dimid4(3:4) = (/ptid, timid/)
        dimid => dimid4
      else if (trim(outvars(n)%dims) == 'k') then
        dimid4(3:4) = (/ktid, timid/)
        dimid => dimid4
      else
        dimid3(3) = timid
        dimid => dimid3
      end if
      ierr = nf90_def_var(ncid, trim(outvars(n)%var_name), nf90_float, dimid, varid)
      call handle_err(ierr, 'define variable '//trim((outvars(n)%var_name)))
      ierr = nf90_put_att(ncid, varid, 'units'     , trim(outvars(n)%unit_name))
      ierr = nf90_put_att(ncid, varid, 'long_name' , trim(outvars(n)%long_name))
      ierr = nf90_put_att(ncid, varid, '_fillvalue', undef)
    end do
    ! end variable definitions
    ierr = nf90_enddef(ncid)
    call handle_err(ierr, 'end variable definition')
    ! write the time and spatial axis values (lat,lon,time)
    ierr = nf90_inq_varid(ncid,  'lat', varid)
    call handle_err(ierr, 'inquire variable lat ')
    ierr = nf90_put_var(ncid, varid, transpose(ygrd))
    call handle_err(ierr, 'put lat')
    ierr = nf90_inq_varid(ncid,  'lon', varid)
    call handle_err(ierr, 'inquire variable lon ')
    ierr = nf90_put_var(ncid, varid, transpose(xgrd))
    call handle_err(ierr, 'put lon')
    ierr = nf90_inq_varid(ncid,  'time', varid)
    call handle_err(ierr, 'inquire variable time ')
    ierr = nf90_put_var(ncid, varid, elapsed_secs)
    call handle_err(ierr, 'put time')
    if (gtype .eq. ungtype) then
      ierr = nf90_inq_varid(ncid,  'nconn', varid)
      call handle_err(ierr, 'inquire variable nconn ')
      ierr = nf90_put_var(ncid, varid, trigp)
      call handle_err(ierr, 'put trigp')
    end if
    !maps
    ierr = nf90_inq_varid(ncid,  'mapsta', varid)
    call handle_err(ierr, 'inquire variable mapsta ')
    ierr = nf90_put_var(ncid, varid, transpose(mapsta))
    call handle_err(ierr, 'put mapsta')
    ! close the file
    ierr = nf90_close(ncid)
    ! write the requested variables
    do n = 1,size(outvars)
      vname = trim(outvars(n)%var_name)
      if (trim(outvars(n)%dims) == 's') then
        var3d => var3ds
        ! group 4
        if(vname .eq.      'phs') call write_var3d(vname, phs      (1:nsea,0:noswll) )
        if(vname .eq.      'ptp') call write_var3d(vname, ptp      (1:nsea,0:noswll) )
        if(vname .eq.      'plp') call write_var3d(vname, plp      (1:nsea,0:noswll) )
        if(vname .eq.     'pdir') call write_var3d(vname, pdir     (1:nsea,0:noswll) )
        if(vname .eq.      'psi') call write_var3d(vname, psi      (1:nsea,0:noswll) )
        if(vname .eq.      'pws') call write_var3d(vname, pws      (1:nsea,0:noswll) )
        if(vname .eq.      'pdp') call write_var3d(vname, pthp0    (1:nsea,0:noswll) )
        if(vname .eq.      'pqp') call write_var3d(vname, pqp      (1:nsea,0:noswll) )
        if(vname .eq.      'ppe') call write_var3d(vname, ppe      (1:nsea,0:noswll) )
        if(vname .eq.      'pgw') call write_var3d(vname, pgw      (1:nsea,0:noswll) )
        if(vname .eq.      'psw') call write_var3d(vname, psw      (1:nsea,0:noswll) )
        if(vname .eq.     'ptm1') call write_var3d(vname, ptm1     (1:nsea,0:noswll) )
        if(vname .eq.      'pt1') call write_var3d(vname, pt1      (1:nsea,0:noswll) )
        if(vname .eq.      'pt2') call write_var3d(vname, pt2      (1:nsea,0:noswll) )
        if(vname .eq.      'pep') call write_var3d(vname, pep      (1:nsea,0:noswll) )
      else if (trim(outvars(n)%dims) == 'm') then                          ! m axis
        var3d => var3dm
        ! group 6
        if (vname .eq.   'p2sms') call write_var3d(vname, p2sms    (1:nsea,p2msf(2):p2msf(3)) )
      else if (trim(outvars(n)%dims) == 'p') then                          ! partition axis
        var3d => var3dp
        ! group 6
        if (vname .eq.   'usspx') call write_var3d(vname, ussp     (1:nsea,   1:usspf(2)) )
        if (vname .eq.   'usspy') call write_var3d(vname, ussp     (1:nsea,nk+1:nk+usspf(2)) )
      else if (trim(outvars(n)%dims) == 'k') then                           ! freq axis
        var3d => var3dk
        ! group 3
        if(vname .eq.       'ef') call write_var3d(vname, ef       (1:nsea,e3df(2,1):e3df(3,1)) )
        if(vname .eq.     'th1m') call write_var3d(vname, ef       (1:nsea,e3df(2,2):e3df(3,2)) )
        if(vname .eq.    'sth1m') call write_var3d(vname, ef       (1:nsea,e3df(2,3):e3df(3,3)) )
        if(vname .eq.     'th2m') call write_var3d(vname, ef       (1:nsea,e3df(2,4):e3df(3,4)) )
        if(vname .eq.    'sth2m') call write_var3d(vname, ef       (1:nsea,e3df(2,5):e3df(3,5)) )
        !todo: wn has reversed indices (1:nk, 1:nsea)
        ! group 6
        if (vname .eq.   'us3dx') call write_var3d(vname, us3d     (1:nsea,   us3df(2):us3df(3)) )
        if (vname .eq.   'us3dy') call write_var3d(vname, us3d     (1:nsea,nk+us3df(2):nk+us3df(3)) )
      else
        ! group 1
        if (vname .eq.      'dw') call write_var2d(vname, dw       (1:nsea), init0='false')
        if (vname .eq.      'cx') call write_var2d(vname, cx       (1:nsea), init0='false')
        if (vname .eq.      'cy') call write_var2d(vname, cy       (1:nsea), init0='false')
        if (vname .eq.     'uax') call write_var2d(vname, ua       (1:nsea), dir=cos(ud(1:nsea)), init0='false')
        if (vname .eq.     'uay') call write_var2d(vname, ua       (1:nsea), dir=sin(ud(1:nsea)), init0='false')
        if (vname .eq.      'as') call write_var2d(vname, as       (1:nsea), init0='false')
        if (vname .eq.     'wlv') call write_var2d(vname, wlv      (1:nsea), init0='false')
        if (vname .eq.     'ice') call write_var2d(vname, ice      (1:nsea), init0='false')
        if (vname .eq.    'berg') call write_var2d(vname, berg     (1:nsea), init0='false')
        if (vname .eq.    'taux') call write_var2d(vname, taua     (1:nsea), dir=cos(tauadir(1:nsea)), init0='false')
        if (vname .eq.    'tauy') call write_var2d(vname, taua     (1:nsea), dir=sin(tauadir(1:nsea)), init0='false')
        if (vname .eq.  'rhoair') call write_var2d(vname, rhoair   (1:nsea), init0='false')
        if (vname .eq.    'iceh') call write_var2d(vname, iceh     (1:nsea), init0='false')
        if (vname .eq.    'icef') call write_var2d(vname, icef     (1:nsea), init0='false')
        ! group 2
        if (vname .eq.      'hs') call write_var2d(vname, hs       (1:nsea) )
        if (vname .eq.     'wlm') call write_var2d(vname, wlm      (1:nsea) )
        if (vname .eq.     't02') call write_var2d(vname, t02      (1:nsea) )
        if (vname .eq.    't0m1') call write_var2d(vname, t0m1     (1:nsea) )
        if (vname .eq.     't01') call write_var2d(vname, t01      (1:nsea) )
        if (vname .eq.     'fp0') call write_var2d(vname, fp0      (1:nsea) )
        if (vname .eq.     'thm') call write_var2d(vname, thm      (1:nsea) )
        if (vname .eq.     'ths') call write_var2d(vname, ths      (1:nsea) )
        if (vname .eq.    'thp0') call write_var2d(vname, thp0     (1:nsea) )
        if (vname .eq.    'hsig') call write_var2d(vname, hsig     (1:nsea) )
        if (vname .eq.  'stmaxe') call write_var2d(vname, stmaxe   (1:nsea) )
        if (vname .eq.  'stmaxd') call write_var2d(vname, stmaxd   (1:nsea) )
        if (vname .eq.   'hmaxe') call write_var2d(vname, hmaxe    (1:nsea) )
        if (vname .eq.  'hcmaxe') call write_var2d(vname, hcmaxe   (1:nsea) )
        if (vname .eq.   'hmaxd') call write_var2d(vname, hmaxd    (1:nsea) )
        if (vname .eq.  'hcmaxd') call write_var2d(vname, hcmaxd   (1:nsea) )
        if (vname .eq.     'wbt') call write_var2d(vname, wbt      (1:nsea) )
        if (vname .eq.  'wnmean') call write_var2d(vname, wnmean   (1:nsea), init0='false')
        ! group 4
        if(vname .eq.     'pwst') call write_var2d(vname, pwst     (1:nsea) )
        if(vname .eq.      'pnr') call write_var2d(vname, pnr      (1:nsea) )
        ! group 5
        if (vname .eq.    'ustx') call write_var2d(vname, ust      (1:nsea)*asf(1:nsea), dir=cos(ustdir(1:nsea)), usemask='true')
        if (vname .eq.    'usty') call write_var2d(vname, ust      (1:nsea)*asf(1:nsea), dir=sin(ustdir(1:nsea)), usemask='true')
        if (vname .eq.   'charn') call write_var2d(vname, charn    (1:nsea) )
        if (vname .eq.     'cge') call write_var2d(vname, cge      (1:nsea) )
        if (vname .eq.   'phiaw') call write_var2d(vname, phiaw    (1:nsea),   init2='true')
        if (vname .eq.  'tauwix') call write_var2d(vname, tauwix   (1:nsea),   init2='true')
        if (vname .eq.  'tauwiy') call write_var2d(vname, tauwiy   (1:nsea),   init2='true')
        if (vname .eq.  'tauwnx') call write_var2d(vname, tauwnx   (1:nsea),   init2='true')
        if (vname .eq.  'tauwny') call write_var2d(vname, tauwny   (1:nsea),   init2='true')
        if (vname .eq.     'wcc') call write_var2d(vname, whitecap (1:nsea,1), init2='true')
        if (vname .eq.     'wcf') call write_var2d(vname, whitecap (1:nsea,2), init2='true')
        if (vname .eq.     'wch') call write_var2d(vname, whitecap (1:nsea,3), init2='true')
        if (vname .eq.     'wcm') call write_var2d(vname, whitecap (1:nsea,4), init2='true')
        if (vname .eq.     'tws') call write_var2d(vname, tws      (1:nsea) )
        ! group 6
        if (vname .eq.     'sxx') call write_var2d(vname, sxx      (1:nsea) )
        if (vname .eq.     'syy') call write_var2d(vname, syy      (1:nsea) )
        if (vname .eq.     'sxy') call write_var2d(vname, sxy      (1:nsea) )
        if (vname .eq.   'tauox') call write_var2d(vname, tauox    (1:nsea),   init2='true')
        if (vname .eq.   'tauoy') call write_var2d(vname, tauoy    (1:nsea),   init2='true')
        if (vname .eq.     'bhd') call write_var2d(vname, bhd      (1:nsea) )
        if (vname .eq.   'phioc') call write_var2d(vname, phioc    (1:nsea),   init2='true')
        if (vname .eq.    'tusx') call write_var2d(vname, tusx     (1:nsea) )
        if (vname .eq.    'tusy') call write_var2d(vname, tusy     (1:nsea) )
        if (vname .eq.    'ussx') call write_var2d(vname, ussx     (1:nsea) )
        if (vname .eq.    'ussy') call write_var2d(vname, ussy     (1:nsea) )
        if (vname .eq.    'prms') call write_var2d(vname, prms     (1:nsea) )
        if (vname .eq.    'tpms') call write_var2d(vname, tpms     (1:nsea) )
        if (vname .eq. 'tauicex') call write_var2d(vname, tauice   (1:nsea,1) )
        if (vname .eq. 'tauicey') call write_var2d(vname, tauice   (1:nsea,2) )
        if (vname .eq.   'phice') call write_var2d(vname, phice    (1:nsea) )
        if (vname .eq.  'tauocx') call write_var2d(vname, tauocx   (1:nsea) )
        if (vname .eq.  'tauocy') call write_var2d(vname, tauocy   (1:nsea) )
        if (vname .eq.   'usshx') call write_var2d(vname, usshx    (1:nsea) )
        if (vname .eq.   'usshy') call write_var2d(vname, usshy    (1:nsea) )
        ! group 7
        if (vname .eq.    'abax') call write_var2d(vname, aba      (1:nsea), dir=cos(abd(1:nsea)) )
        if (vname .eq.    'abay') call write_var2d(vname, aba      (1:nsea), dir=sin(abd(1:nsea)) )
        if (vname .eq.    'ubax') call write_var2d(vname, uba      (1:nsea), dir=cos(ubd(1:nsea)) )
        if (vname .eq.    'ubay') call write_var2d(vname, uba      (1:nsea), dir=sin(ubd(1:nsea)) )
        if (vname .eq.     'bed') call write_var2d(vname, bedforms (1:nsea,1), init2='true')
        if (vname .eq. 'ripplex') call write_var2d(vname, bedforms (1:nsea,2), init2='true')
        if (vname .eq. 'rippley') call write_var2d(vname, bedforms (1:nsea,3), init2='true')
        if (vname .eq.  'phibbl') call write_var2d(vname, phibbl   (1:nsea),   init2='true')
        if (vname .eq. 'taubblx') call write_var2d(vname, taubbl   (1:nsea,1), init2='true')
        if (vname .eq. 'taubbly') call write_var2d(vname, taubbl   (1:nsea,2), init2='true')
        ! group 8
        if (vname .eq.    'mssx') call write_var2d(vname, mssx     (1:nsea) )
        if (vname .eq.    'mssy') call write_var2d(vname, mssy     (1:nsea) )
        if (vname .eq.    'mscx') call write_var2d(vname, mscx     (1:nsea) )
        if (vname .eq.    'mscy') call write_var2d(vname, mscy     (1:nsea) )
        !todo: remaining variables have inconsistency between shel_inp listing and iogo code
        ! group 9
        if (vname .eq.   'dtdyn') call write_var2d(vname, dtdyn    (1:nsea) )
        if (vname .eq.    'fcut') call write_var2d(vname, fcut     (1:nsea) )
        if (vname .eq.'cflxymax') call write_var2d(vname, cflxymax (1:nsea) )
        if (vname .eq.'cflthmax') call write_var2d(vname, cflthmax (1:nsea) )
        if (vname .eq. 'cflkmax') call write_var2d(vname, cflkmax  (1:nsea) )
        ! group 10
      end if
    end do
    if (s_axis) deallocate(var3ds)
    if (m_axis) deallocate(var3dm)
    if (p_axis) deallocate(var3dp)
    if (k_axis) deallocate(var3dk)
    ! flush the buffers for write
    call w3seta ( igrd, ndse, ndst )
  end subroutine w3iogoncd
  !===============================================================================
  subroutine write_var2d(vname, var, dir, usemask, init0, init2)
    ! write (nsea) array as (nx,ny)
    ! if dir is present, write x or y component of (nsea) array as (nx,ny)
    ! if mask is present and true, use mapsta=1 to mask values
    ! if init0 is present and false, do not initialize values
    ! for mapsta<0. this prevents group 1 variables being set undef over
    ! ice. if init2 is present and true, apply a second initialization to
    ! a subset of variables for where mapsta==2
    character(len=*),           intent(in) :: vname
    real            ,           intent(in) :: var(:)
    real, optional  ,           intent(in) :: dir(:)
    character(len=*), optional, intent(in) :: usemask
    character(len=*), optional, intent(in) :: init0
    character(len=*), optional, intent(in) :: init2
    ! local variables
    real, dimension(nx,ny) :: var2d
    logical                :: lmask, linit0, linit2
    real                   :: varloc
    lmask = .false.
    if (present(usemask)) then
      lmask = (trim(usemask) == "true")
    end if
    linit0 = .true.
    if (present(init0)) then
      linit0 = (trim(init0) == "true")
    end if
    linit2 = .false.
    if (present(init2)) then
      linit2 = (trim(init2) == "true")
    end if
    ! debug
    ! write(nds(1),'(a)')' writing variable ' //trim(vname)//' to history file '//trim(fname)
    var2d = undef
    do isea = 1,nsea
      ! initialization
      varloc = var(isea)
      if (linit0) then
        if (mapsta(mapsf(isea,2),mapsf(isea,1)) < 0) varloc = undef
      end if
      if (linit2) then
        if (mapsta(mapsf(isea,2),mapsf(isea,1)) == 2) varloc = undef
      end if
      if (present(dir)) then
        if (varloc .ne. undef) then
          if (lmask) then
            if (mapsta(mapsf(isea,2),mapsf(isea,1)) == 1) then
              var2d(mapsf(isea,1),mapsf(isea,2)) = varloc*dir(isea)
            end if
          else
            var2d(mapsf(isea,1),mapsf(isea,2)) = varloc*dir(isea)
          end if
        end if
      else
        var2d(mapsf(isea,1),mapsf(isea,2)) = varloc
      end if
    end do
    ierr = nf90_open(trim(fname),   nf90_write,  ncid)
    call handle_err(ierr, 'open '//trim(fname)//' for writing')
    ierr = nf90_inq_varid(ncid,  trim(vname), varid)
    call handle_err(ierr, 'inquire variable '//trim(vname))
    ierr = nf90_put_var(ncid, varid, var2d)
    call handle_err(ierr, 'put variable '//trim(vname))
    ierr = nf90_close(ncid)
  end subroutine write_var2d
  !===============================================================================
  subroutine write_var3d(vname, var, init2)
    ! write (nsea,:) array as (nx,ny,:)
    ! if init2 is present and true, apply a second initialization to
    ! a subset of variables for where mapsta==2
    character(len=*),           intent(in) :: vname
    real            ,           intent(in) :: var(:,:)
    character(len=*), optional, intent(in) :: init2
    ! local variables
    real, allocatable, dimension(:)     :: varloc
    logical                             :: linit2
    integer                             :: lb, ub
    linit2 = .false.
    if (present(init2)) then
      linit2 = (trim(init2) == "true")
    end if
    lb = lbound(var,2)
    ub = ubound(var,2)
    allocate(varloc(lb:ub))
    ! debug
    ! write(nds(1),'(a,2i6)')' writing variable ' //trim(vname)//' to history file ' &
    !    //trim(fname)//' with bounds ',lb,ub
    var3d = undef
    do isea = 1,nsea
      ! initialization
      varloc(:) = var(isea,:)
      if (mapsta(mapsf(isea,2),mapsf(isea,1)) < 0) varloc(:) = undef
      if (linit2) then
        if (mapsta(mapsf(isea,2),mapsf(isea,1)) == 2) varloc(:) = undef
      end if
      var3d(mapsf(isea,1),mapsf(isea,2),:) = varloc(:)
    end do
    ierr = nf90_open(trim(fname),  nf90_write,  ncid)
    call handle_err(ierr, 'open '//trim(fname)//' for writing')
    ierr = nf90_inq_varid(ncid, trim(vname), varid)
    call handle_err(ierr, 'inquire variable '//trim(vname))
    ierr = nf90_put_var(ncid, varid, var3d)
    call handle_err(ierr, 'put variable '//trim(vname))
    ierr = nf90_close(ncid)
    deallocate(varloc)
  end subroutine write_var3d
  !===============================================================================
  subroutine handle_err(ierr,string)
    use w3odatmd  , only : ndse
    use w3servmd  , only : extcde
    ! input/output variables
    integer         , intent(in) :: ierr
    character(len=*), intent(in) :: string
    if (ierr /= nf90_noerr) then
      write(ndse,*) "*** wavewatch iii netcdf error: ",trim(string),':',trim(nf90_strerror(ierr))
      call extcde ( 49 )
    end if
  end subroutine handle_err
end module w3iogoncdmd
