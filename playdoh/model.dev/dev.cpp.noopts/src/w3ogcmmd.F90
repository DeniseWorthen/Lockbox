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
module w3ogcmmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           a. thevenin             |
  !/                  |                        fortran 90 |
  !/                  | last update :         22-mar-2021 |
  !/                  +-----------------------------------+
  !/
  !/    jul-2013 : origination.                       ( version 4.18 )
  !/               for upgrades see subroutines.
  !/    apr-2016 : add comments (j. pianezze)         ( version 5.07 )
  !/ 22-mar-2021 : add extra coupling variables       ( version 7.13 )
  !/
  !/    copyright 2009-2012 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     module used for coupling applications between oceanic model and ww3 with oasis3-mct
  !
  !  2. variables and types :
  !
  !  3. subroutines and functions :
  !
  !      name                   type  scope    description
  !     ----------------------------------------------------------------
  !      snd_fields_to_ocean    subr. public   send fields to ocean model
  !      rcv_fields_from_ocean  subr. public   receive fields from ocean model
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name                type  module    description
  !     ----------------------------------------------------------------
  !      cpl_oasis_send       subr.   w3oacpmd   send fields
  !      cpl_oasis_recv       subr.   w3oacpmd   receive fields
  !     ----------------------------------------------------------------
  !
  !  5. remarks
  !  6. switches :
  !  7. source code :
  !
  !/ ------------------------------------------------------------------- /
  !
  implicit none
  !
  include "mpif.h"
  !
  private
  !
  ! * accessibility
  public snd_fields_to_ocean
  public rcv_fields_from_ocean
  !
contains
  !/ ------------------------------------------------------------------- /
  subroutine snd_fields_to_ocean()
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           a. thevenin             |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    jul-2013 : origination.                    ( version 4.18 )
    !/    apr-2016 : add comments (j. pianezze)      ( version 5.07 )
    !/ 22-mar-2021 : add extra coupling variables    ( version 7.13 )
    !/
    !  1. purpose :
    !
    !     send coupling fields to oceanic model
    !
    !  2. method :
    !  3. parameters :
    !  4. subroutines used :
    !
    !     name             type    module     description
    !     -------------------------------------------------------------------
    !     cpl_oasis_snd    subr.   w3oacpmd   send field to atmos/ocean model
    !     -------------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name            type    module     description
    !     ------------------------------------------------------------------
    !     w3wave           subr.   w3wavemd   wave model
    !     ------------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks :
    !
    !     according to the present implementation, fields are sent at each coupling time step to oasis
    !     consequently, oasis cannot estimate any time average
    !     for such an application, one must estimate the fields at each time step
    !     (or a time step smaller than the coupling time step).
    !     in such conditions, oasis get the information every time step
    !     but only send the information to the other code when the time matches the coupling time.
    !
    !  8. structure :
    !  9. switches :
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    use w3oacpmd,  only: id_oasis_time, il_nb_snd, snd_fld, cpl_oasis_snd
    use w3gdatmd,  only: nseal, mapsta, mapsf
    use w3adatmd,  only: hs, t0m1, t01, thm, bhd, tauox, tauoy, phioc,&
         uba, ubd, tauwix, tauwiy, tusx, tusy, ussx,  &
         ussy, wlm, phibbl,taubbl, charn, tauocx,     &
         tauocy, wnmean
    use w3odatmd,  only: naproc, iaproc, undef
    use constants, only: pi, dera
    !
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                          :: i, isea, ix, iy
    integer, dimension(nseal)        :: mask
    real(kind=8), dimension(nseal,1) :: rla_oasis_snd
    integer                          :: ib_do
    logical                          :: ll_action
    real(kind=8), dimension(nseal)   :: tmp
    !
    !----------------------------------------------------------------------
    ! * executable part
    !
    do i = 1, nseal
      isea = iaproc + (i-1)*naproc
      ix = mapsf(isea,1)
      iy = mapsf(isea,2)
      ! get the mask : 1 - sea 0 - open boundary cells dried cells
      mask(i) = mod(mapsta(iy,ix),2)
    end do
    !
    do ib_do = 1, il_nb_snd
      !
      ! mask - wet-drying
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3_odry') then
        tmp(1:nseal) = 0.0
        where(mask(1:nseal) /= undef) tmp(1:nseal)=mask(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! mean wave period (tmn in s) (m0,-1)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3_t0m1') then
        tmp(1:nseal) = 0.0
        where(t0m1(1:nseal) /= undef) tmp(1:nseal)=t0m1(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! mean wave period (tmn in s) (m0,1)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3__t01') then
        tmp(1:nseal) = 0.0
        where(t01(1:nseal) /= undef) tmp(1:nseal)=t01(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! mean wave number (wnm in m-1)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3__wnm') then
        tmp(1:nseal) = 0.0
        where(wnmean(1:nseal) /= undef) tmp(1:nseal)=wnmean(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! charnock coefficient  (-)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3_ocha') then
        tmp(1:nseal) = 0.0
        where(charn(1:nseal) /= undef) tmp(1:nseal)=charn(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! wave height (hs in m)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3__ohs') then
        tmp(1:nseal) = 0.0
        where(hs(1:nseal) /= undef) tmp(1:nseal)=hs(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! cosinus of mean wave direction (cos(theta) in radians)
      ! ---------------------------------------------------------------------
      ! dir : nautical convention (gridded files) - 0 degree from north, 90 from east
      if (snd_fld(ib_do)%cl_field_name == 'ww3_cdir') then
        tmp(1:nseal) = 0.0
        where(thm(1:nseal) /= undef) tmp(1:nseal)=cos(thm(1:nseal))
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! sinus of mean wave direction (sin(theta) in radians)
      ! ---------------------------------------------------------------------
      ! dir : nautical convention (gridded files) - 0 degree from north, 90 from east
      if (snd_fld(ib_do)%cl_field_name == 'ww3_sdir') then
        tmp(1:nseal) = 0.0
        where(thm(1:nseal) /= undef) tmp(1:nseal)=sin(thm(1:nseal))
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! mean wave direction theta in radians
      ! ---------------------------------------------------------------------
      ! dir : nautical convention (gridded files) - 0 degree from north, 90 from east
      if (snd_fld(ib_do)%cl_field_name == 'ww3__dir') then
        tmp(1:nseal) = 0.0
        where(thm /= undef) tmp(1:nseal)=thm(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! wave-induced bernoulli head pressure (bhd in n.m-1) (j term, smith jpo 2006)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3__bhd') then
        tmp(1:nseal) = 0.0
        where(bhd(1:nseal) /= undef) tmp(1:nseal)=bhd(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! wave-ocean momentum flux (tauox in m2.s-2)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3_twox') then
        tmp(1:nseal) = 0.0
        where(tauox(1:nseal) /= undef) tmp(1:nseal)=tauox(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! wave-ocean momentum flux (tauoy in m2.s-2)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3_twoy') then
        tmp(1:nseal) = 0.0
        where(tauoy(1:nseal) /= undef) tmp(1:nseal)=tauoy(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! wave-ocean total momentum flux (tauocx in pa)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3_tocx') then
        tmp(1:nseal) = 0.0
        where(tauocx(1:nseal) /= undef) tmp(1:nseal)=tauocx(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! wave-ocean total momentum flux (tauocy in pa)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3_tocy') then
        tmp(1:nseal) = 0.0
        where(tauocy(1:nseal) /= undef) tmp(1:nseal)=tauocy(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! wave-to-ocean tke flux (phioc in w.m-2)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3__foc') then
        tmp(1:nseal) = 0.0
        where(phioc(1:nseal) /= undef) tmp(1:nseal)=phioc(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! momentum flux due to bottom friction (taubblx in m2.s-2)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3_tbbx') then
        tmp(1:nseal) = 0.0
        where(taubbl(1:nseal,1) /= undef) tmp(1:nseal)=taubbl(1:nseal,1)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! momentum flux due to bottom friction (taubbly in m2.s-2)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3_tbby') then
        tmp(1:nseal) = 0.0
        where(taubbl(1:nseal,2) /= undef) tmp(1:nseal)=taubbl(1:nseal,2)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! energy flux due to bottom friction (phibbl in w.m-2)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3__fbb') then
        tmp(1:nseal) = 0.0
        where(phibbl(1:nseal) /= undef) tmp(1:nseal)=phibbl(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! rms amplitude of orbital velocity of the waves (ubr in m.s-1)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3__ubr') then
        tmp(1:nseal) = 0.0
        where(uba(1:nseal) /= undef) tmp(1:nseal)=uba(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! x component of the near-bottom rms wave velocity (in m.s-1)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3_ubrx') then
        tmp(1:nseal) = 0.0
        where(uba(1:nseal) /= undef) tmp(1:nseal)=uba(1:nseal)*cos(ubd(1:nseal))
        rla_oasis_snd(:,1) = tmp(1:nseal)
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! y component of the near-bottom rms wave velocity (in m.s-1)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3_ubry') then
        tmp(1:nseal) = 0.0
        where(uba(1:nseal) /= undef) tmp(1:nseal)=uba(1:nseal)*sin(ubd(1:nseal))
        rla_oasis_snd(:,1) = tmp(1:nseal)
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! net wave-supported stress, u component (tauwix in m2.s-2)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3_tawx') then
        tmp(1:nseal) = 0.0
        where(tauwix(1:nseal) /= undef) tmp(1:nseal)=tauwix(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! net wave-supported stress, v component (tauwix in m2.s-2)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3_tawy') then
        tmp(1:nseal) = 0.0
        where(tauwiy(1:nseal) /= undef) tmp(1:nseal)=tauwiy(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! volume transport associated to stokes drift, u component (tusx in m2.s-1)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3_tusx') then
        tmp(1:nseal) = 0.0
        where(tusx(1:nseal) /= undef) tmp(1:nseal)=tusx(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! volume transport associated to stokes drift, v component (tusy in m2.s-1)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3_tusy') then
        tmp(1:nseal) = 0.0
        where(tusy(1:nseal) /= undef) tmp(1:nseal)=tusy(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! surface stokes drift, u component (ussx in m.s-1)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3_ussx') then
        tmp(1:nseal) = 0.0
        where(ussx(1:nseal) /= undef) tmp(1:nseal)=ussx(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! surface stokes drift, v component (ussy in m.s-1)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3_ussy') then
        tmp(1:nseal) = 0.0
        where(ussy(1:nseal) /= undef) tmp(1:nseal)=ussy(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! mean wave length (wlm in m)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3___lm') then
        tmp(1:nseal) = 0.0
        where(wlm(1:nseal) /= undef) tmp(1:nseal)=wlm(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
    enddo
    !/ ------------------------------------------------------------------- /
  end subroutine snd_fields_to_ocean
  !/ ------------------------------------------------------------------- /
  subroutine rcv_fields_from_ocean(id_lcomm, idfld, fxn, fyn, fan)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           a. thevenin             |
    !/                  |                        fortran 90 |
    !/                  | last update :          april-2016 |
    !/                  +-----------------------------------+
    !/
    !/    jul-2013 : origination.                               ( version 4.18 )
    !/    apr-2014 : add idfld, fxn, fyx and fan (m. accensi)   ( version 5.07 )
    !/    apr-2016 : add comments (j. pianezze)                 ( version 5.07 )
    !/
    !  1. purpose :
    !
    !     receive coupling fields from oceanic model
    !
    !  2. method :
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     id_lcomm          char.     i     mpi communicator
    !     idfld             int.      i     name of the exchange fields
    !     fxn               int.     i/o    first exchange field
    !     fyn               int.     i/o    second exchange field
    !     fan               int.     i/o    third exchange field
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     name             type    module     description
    !     -------------------------------------------------------------------
    !     cpl_oasis_rcv    subr.   w3oacpmd   receive fields from atmos/ocean model
    !     w3s2xy           subr.   w3servmd   convert from storage (nsea) to spatial grid (nx, ny)
    !     -------------------------------------------------------------------
    !
    !  5. called by :
    !
    !     name            type    module     description
    !     ------------------------------------------------------------------
    !     w3fldg          subr.   w3fldsmd   manage input fields of depth,
    !                                        current, wind and ice concentration
    !     ------------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks :
    !
    !     idfld   c*3  i/o id string for field type, valid are: 'lev', 'cur' (j=1,2)
    !
    !  8. structure :
    !  9. switches :
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    use w3oacpmd, only: id_oasis_time, il_nb_rcv, rcv_fld, cpl_oasis_rcv
    use w3gdatmd, only: nx, ny, nseal, nsea, mapsf
    use w3odatmd, only: naproc, iaproc
    use w3servmd, only: w3s2xy
    !
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)              :: id_lcomm
    character(len=3), intent(in)     :: idfld
    real, intent(inout)              :: fxn(:,:), fyn(:,:), fan(:,:)
    !
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    logical                          :: ll_action
    integer                          :: ib_do, ib_i, ib_j, il_err
    integer, save                    :: id_oasis_time_wetdryonlyonce = -1
    real(kind=8), dimension(nseal,1) :: rla_oasis_rcv
    real(kind=8), dimension(nseal)   :: tmp, maskt, masku, maskv
    real, dimension(1:nsea)          :: snd_buff,rcv_buff
    !
    !----------------------------------------------------------------------
    ! * executable part
    !
    maskt(:)=1.
    masku(:)=1.
    maskv(:)=1.
    rla_oasis_rcv(:,:) = 0.0
    !
    ! ---------------------------------------------------------------------
    ! perform mask variables
    ! ---------------------------------------------------------------------
    !
    ! for the same coupling time, w3fldg is called for the level and current variables.
    ! as rcv_fields_from_ocean is called from w3fldg, the following test prevents to
    ! exchange the wet-dry variables more than once per coupling time.
    !cval well but it cannot work because maskt,masku,maskv variable are not global variable
    !cval anyway we will give up the exchange of mask, it is not a good idea at all
    if (id_oasis_time > id_oasis_time_wetdryonlyonce) then
      !
      do ib_do = 1, il_nb_rcv
        !
        ! land mask - u
        ! ---------------------------------------------------------------------
        if (rcv_fld(ib_do)%cl_field_name == 'ww3_owdh') then
          call cpl_oasis_rcv(ib_do, id_oasis_time, rla_oasis_rcv, ll_action)
          if (ll_action) then
            maskt(1:nseal)  = rla_oasis_rcv(1:nseal,1)
          endif
        endif
        !
        ! land mask - h
        ! ---------------------------------------------------------------------
        if (rcv_fld(ib_do)%cl_field_name == 'ww3_owdu') then
          call cpl_oasis_rcv(ib_do, id_oasis_time, rla_oasis_rcv, ll_action)
          if (ll_action) then
            masku(1:nseal)  = rla_oasis_rcv(1:nseal,1)
          endif
        endif
        !
        ! land mask - v
        ! ---------------------------------------------------------------------
        if (rcv_fld(ib_do)%cl_field_name == 'ww3_owdv') then
          call cpl_oasis_rcv(ib_do, id_oasis_time, rla_oasis_rcv, ll_action)
          if (ll_action) then
            maskv(1:nseal)  = rla_oasis_rcv(1:nseal,1)
          endif
        endif
        !
      enddo
      !
    endif
    !
    ! ---------------------------------------------------------------------
    ! treatment of the dynamical variables
    ! ---------------------------------------------------------------------
    do ib_do = 1, il_nb_rcv
      !
      ! sea surface height (m)
      ! ---------------------------------------------------------------------
      if (idfld == 'lev') then
        !
        if (rcv_fld(ib_do)%cl_field_name == 'ww3__ssh') then
          call cpl_oasis_rcv(ib_do, id_oasis_time, rla_oasis_rcv, ll_action)
          if (ll_action) then
            tmp(1:nseal) = rla_oasis_rcv(1:nseal,1) * maskt(1:nseal)
            snd_buff(1:nsea) = 0.0
            do ib_i = 1, nseal
              ib_j = iaproc + (ib_i-1)*naproc
              snd_buff(ib_j) = tmp(ib_i)
            enddo
            !
            call mpi_allreduce(snd_buff(1:nsea), &
                 rcv_buff(1:nsea), &
                 nsea,     &
                 mpi_real, &
                 mpi_sum,  &
                 id_lcomm, &
                 il_err)
            !
            ! convert from storage (nsea) to spatial grid (nx, ny)
            call w3s2xy(nsea,nsea,nx,ny,rcv_buff(1:nsea),mapsf,fan)
            !
          endif
        endif
      endif
      !
      ! ocean sea surface current (m.s-1)
      ! ---------------------------------------------------------------------
      if (idfld == 'cur') then
        !
        ! u-component
        ! ---------------------------------------------------------------------
        if (rcv_fld(ib_do)%cl_field_name == 'ww3_ossu') then
          call cpl_oasis_rcv(ib_do, id_oasis_time, rla_oasis_rcv, ll_action)
          if (ll_action) then
            tmp(1:nseal) = rla_oasis_rcv(1:nseal,1) * masku(1:nseal)
            snd_buff(1:nsea) = 0.0
            do ib_i = 1, nseal
              ib_j = iaproc + (ib_i-1)*naproc
              snd_buff(ib_j) = tmp(ib_i)
            enddo
            !
            call mpi_allreduce(snd_buff(1:nsea),       &
                 rcv_buff(1:nsea),       &
                 nsea,     &
                 mpi_real, &
                 mpi_sum,  &
                 id_lcomm, &
                 il_err)
            !
            ! convert from storage (nsea) to spatial grid (nx, ny)
            call w3s2xy(nsea,nsea,nx,ny,rcv_buff(1:nsea),mapsf,fxn)
            !
          endif
        endif
        !
        ! v-component
        ! ---------------------------------------------------------------------
        if (rcv_fld(ib_do)%cl_field_name == 'ww3_ossv') then
          call cpl_oasis_rcv(ib_do, id_oasis_time, rla_oasis_rcv, ll_action)
          if (ll_action) then
            tmp(1:nseal) = rla_oasis_rcv(1:nseal,1) * maskv(1:nseal)
            snd_buff(1:nsea) = 0.0
            do ib_i = 1, nseal
              ib_j = iaproc + (ib_i-1)*naproc
              snd_buff(ib_j) = tmp(ib_i)
            enddo
            !
            call mpi_allreduce(snd_buff(1:nsea),       &
                 rcv_buff(1:nsea),       &
                 nsea,     &
                 mpi_real, &
                 mpi_sum,  &
                 id_lcomm, &
                 il_err)
            !
            ! convert from storage (nsea) to spatial grid (nx, ny)
            call w3s2xy(nsea,nsea,nx,ny,rcv_buff(1:nsea),mapsf,fyn)
            !
          endif
        endif
      endif
    enddo
    !
    id_oasis_time_wetdryonlyonce = id_oasis_time
    !
    !/ ------------------------------------------------------------------- /
  end subroutine rcv_fields_from_ocean
  !/ ------------------------------------------------------------------- /
  !/
end module w3ogcmmd
!/
!/ ------------------------------------------------------------------- /
