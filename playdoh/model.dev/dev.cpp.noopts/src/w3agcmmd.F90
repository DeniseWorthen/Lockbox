!> @file
!> @brief contains module used for coupling applications between atmospheric model
!>  and ww3 with oasis3-mct.
!>
!> @author j. pianezze
!> @date   mar-2021
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
!> @brief module used for coupling applications between atmospheric model
!>  and ww3 with oasis3-mct.
!>
!> @author j. pianezze
!> @date   mar-2021
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3agcmmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           j. pianezze             |
  !/                  |                        fortran 90 |
  !/                  | last update :            mar-2021 |
  !/                  +-----------------------------------+
  !/
  !/        mar-2014 : origination.                       ( version 4.18 )
  !/                   for upgrades see subroutines.
  !/        apr-2016 : add comments (j. pianezze)         ( version 5.07 )
  !/        mar-2021 : add taua and rhoa coupling         ( version 7.13 )
  !/
  !/    copyright 2009-2012 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     module used for coupling applications between atmospheric model and ww3 with oasis3-mct
  !
  !  2. variables and types :
  !
  !  3. subroutines and functions :
  !
  !      name                   type   scope    description
  !     ----------------------------------------------------------------
  !      snd_fields_to_atmos    subr.  public   send fields to atmos model
  !      rcv_fields_from_atmos  subr.  public   receive fields from atmos model
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name                 type    module     description
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
  public snd_fields_to_atmos
  public rcv_fields_from_atmos
  !
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief send coupling fields to atmospheric model.
  !>
  !> @author j. pianezze
  !> @date   apr-2016
  !>
  subroutine snd_fields_to_atmos()
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           j. pianezze             |
    !/                  |                        fortran 90 |
    !/                  | last update :            apr-2016 |
    !/                  +-----------------------------------+
    !/
    !/    mar-2014 : origination.                  ( version 4.18 )
    !/    apr-2016 : add comments (j. pianezze)    ( version 5.07 )
    !/
    !  1. purpose :
    !
    !     send coupling fields to atmospheric model
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
    !     name            type    module     description
    !     ------------------------------------------------------------------
    !     w3wave          subr.   w3wavemd   wave model
    !     ------------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks :
    !  8. structure :
    !  9. switches :
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    use w3oacpmd,  only: id_oasis_time, il_nb_snd, snd_fld, cpl_oasis_snd
    use w3gdatmd,  only: nseal, nsea
    use w3adatmd,  only: cx, cy, charn, hs, fp0, tws
    use w3odatmd,  only: undef, naproc, iaproc
    !
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    real(kind=8), dimension(nseal,1) :: rla_oasis_snd
    integer                          :: ib_do
    logical                          :: ll_action
    real(kind=8), dimension(nseal)   :: tmp
    integer                          :: jsea, isea
    !
    !----------------------------------------------------------------------
    ! * executable part
    !
    do ib_do = 1, il_nb_snd
      !
      ! ocean sea surface current (m.s-1) (u-component)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3_wssu') then
        tmp(1:nseal) = 0.0
        do jsea=1, nseal
          isea=iaproc+(jsea-1)*naproc
          if(cx(isea) /= undef) tmp(jsea)=cx(isea)
        end do
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! ocean sea surface current (m.s-1) (v-component)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3_wssv') then
        tmp(1:nseal) = 0.0
        do jsea=1, nseal
          isea=iaproc+(jsea-1)*naproc
          if(cy(isea) /= undef) tmp(jsea)=cy(isea)
        end do
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! charnock coefficient (-)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3_acha') then
        tmp(1:nseal) = 0.0
        where(charn(1:nseal) /= undef) tmp(1:nseal)=charn(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! significant wave height (m)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3__ahs') then
        tmp(1:nseal) = 0.0
        where(hs(1:nseal) /= undef) tmp(1:nseal)=hs(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! peak frequency (s-1)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3___fp') then
        tmp(1:nseal) = 0.0
        where(fp0(1:nseal) /= undef) tmp(1:nseal)=fp0(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! peak period (s)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3___tp') then
        tmp(1:nseal) = 0.0
        where(fp0(1:nseal) /= undef) tmp(1:nseal)=1./fp0(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
      ! wind sea mean period (s)
      ! ---------------------------------------------------------------------
      if (snd_fld(ib_do)%cl_field_name == 'ww3__fws') then
        tmp(1:nseal) = 0.0
        where(tws(1:nseal) /= undef) tmp(1:nseal)=tws(1:nseal)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      endif
      !
    enddo
    !
    !/ ------------------------------------------------------------------- /
  end subroutine snd_fields_to_atmos
  !>
  !> @brief receive coupling fields from atmospheric model.
  !>
  !> @param[in]    id_lcomm mpi communicator.
  !> @param[in]    idfld    name of the exchange fields.
  !> @param[inout] fxn      first exchange field.
  !> @param[inout] fyn      second exchange field.
  !> @param[inout] fan      third exchange field.
  !>
  !> @author j. pianezze
  !> @date   mar-2021
  !>
  !/ ------------------------------------------------------------------- /
  subroutine rcv_fields_from_atmos(id_lcomm, idfld, fxn, fyn, fan)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           j. pianezze             |
    !/                  |                        fortran 90 |
    !/                  | last update :            mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    mar-2014 : origination.                ( version 4.18 )
    !/    apr-2015 : modification (m. accensi)   ( version 5.07 )
    !/    apr-2016 : add comments (j. pianezze)  ( version 5.07 )
    !/    mar-2021 : add taua and rhoa coupling  ( version 7.13 )
    !/
    !  1. purpose :
    !
    !     receive coupling fields from atmospheric model
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
    real(kind=8), dimension(nseal,1) :: rla_oasis_rcv
    real(kind=8), dimension(nseal)   :: tmp
    real, dimension(1:nsea)          :: snd_buff,rcv_buff
    !
    !----------------------------------------------------------------------
    ! * executable part
    !
    rla_oasis_rcv(:,:) = 0.0
    !
    do ib_do = 1, il_nb_rcv
      if (idfld == 'wnd') then
        !
        ! wind speed at 10m (m.s-1) (u-component)
        ! ----------------------------------------------------------------------
        if (rcv_fld(ib_do)%cl_field_name == 'ww3__u10') then
          call cpl_oasis_rcv(ib_do, id_oasis_time, rla_oasis_rcv, ll_action)
          if (ll_action) then
            tmp(1:nseal) = rla_oasis_rcv(1:nseal,1)
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
            call w3s2xy(nsea,nsea,nx,ny,rcv_buff(1:nsea),mapsf,fxn)
            !
          endif
        endif
        !
        ! wind speed at 10m (m.s-1) (v-component)
        ! ----------------------------------------------------------------------
        if (rcv_fld(ib_do)%cl_field_name == 'ww3__v10') then
          call cpl_oasis_rcv(ib_do, id_oasis_time, rla_oasis_rcv, ll_action)
          if (ll_action) then
            tmp(1:nseal) = rla_oasis_rcv(1:nseal,1)
            snd_buff(1:nsea) = 0.0
            do ib_i = 1, nseal
              ib_j = iaproc + (ib_i-1)*naproc
              snd_buff(ib_j) = tmp(ib_i)
            end do
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
        !
      endif
      if (idfld == 'tau') then
        !
        ! atmospheric momentum (pa) (u-component)
        ! ----------------------------------------------------------------------
        if (rcv_fld(ib_do)%cl_field_name == 'ww3_utau') then
          call cpl_oasis_rcv(ib_do, id_oasis_time, rla_oasis_rcv, ll_action)
          if (ll_action) then
            tmp(1:nseal) = rla_oasis_rcv(1:nseal,1)
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
            call w3s2xy(nsea,nsea,nx,ny,rcv_buff(1:nsea),mapsf,fxn)
            !
          endif
        endif
        !
        ! atmospheric momentum (pa) (v-component)
        ! ----------------------------------------------------------------------
        if (rcv_fld(ib_do)%cl_field_name == 'ww3_vtau') then
          call cpl_oasis_rcv(ib_do, id_oasis_time, rla_oasis_rcv, ll_action)
          if (ll_action) then
            tmp(1:nseal) = rla_oasis_rcv(1:nseal,1)
            snd_buff(1:nsea) = 0.0
            do ib_i = 1, nseal
              ib_j = iaproc + (ib_i-1)*naproc
              snd_buff(ib_j) = tmp(ib_i)
            end do
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
        !
      endif
      if (idfld == 'rho') then
        !
        ! air density (kg.m-3)
        ! ----------------------------------------------------------------------
        if (rcv_fld(ib_do)%cl_field_name == 'ww3_rhoa') then
          call cpl_oasis_rcv(ib_do, id_oasis_time, rla_oasis_rcv, ll_action)
          if (ll_action) then
            tmp(1:nseal) = rla_oasis_rcv(1:nseal,1)
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
    enddo
    !/ ------------------------------------------------------------------- /
  end subroutine rcv_fields_from_atmos
  !/ ------------------------------------------------------------------- /
  !/
end module w3agcmmd
!/
!/ ------------------------------------------------------------------- /
