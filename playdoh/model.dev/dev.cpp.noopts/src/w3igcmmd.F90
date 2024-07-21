!> @file
!> @brief module used for coupling applications between ice model and ww3 with oasis3-mct.
!>
!> @author g. boutin  @date aug-2016
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
!>
!> @brief module used for coupling applications between ice model and ww3 with oasis3-mct.
!>
!> @author g. boutin @date aug-2016
!>
!/ ------------------------------------------------------------------- /
module w3igcmmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           g. boutin               |
  !/                  |                        fortran 90 |
  !/                  | last update :          aug-2016   |
  !/                  +-----------------------------------+
  !/
  !/        aug-2016 : origination (g. boutin)         ( version 5.10 )
  !/
  !/    copyright 2009-2012 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     module used for coupling applications between ice model and ww3 with oasis3-mct
  !
  !  2. variables and types :
  !
  !  3. subroutines and functions :
  !
  !      name                   type   scope    description
  !     ----------------------------------------------------------------
  !      snd_fields_to_ice    subr.  public   send fields to ice model
  !      rcv_fields_from_ice subr.  public   receive fields from ice model
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
  public snd_fields_to_ice
  public rcv_fields_from_ice
  !
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief send coupling fields to ice model.
  !>
  !> @author g. boutin  @date aug-2016
  !>
  subroutine snd_fields_to_ice()
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           g. boutin               |
    !/                  |                        fortran 90 |
    !/                  | last update :            aug-2016 |
    !/                  +-----------------------------------+
    !/
    !/    aug-2016 : origination (g. boutin)    ( version 5.10 )
    !/
    !  1. purpose :
    !
    !     send coupling fields to ice model
    !
    !  2. method :
    !  3. parameters :
    !  4. subroutines used :
    !
    !     name             type    module     description
    !     -------------------------------------------------------------------
    !     cpl_oasis_snd    subr.   w3oacpmd   send field to ice/ocean model
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
    use w3wdatmd,  only: icef
    use w3adatmd,  only: tauice
    use w3odatmd,  only: undef, naproc, iaproc
    !
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    real(kind=8), dimension(nseal,1) :: rla_oasis_snd
    integer                          :: ib_do, ndso
    logical                          :: ll_action
    real(kind=8), dimension(nseal)   :: tmp
    integer                          :: jsea, isea
    !
    !----------------------------------------------------------------------
    ! * executable part
    !
    do ib_do = 1, il_nb_snd
      select case(snd_fld(ib_do)%cl_field_name)
        !
        ! ice floe diameters (m)
        ! ---------------------------------------------------------------------
      case ('ww3_icef')
        tmp(1:nseal) = 0.0
        do jsea=1, nseal
          isea=iaproc+(jsea-1)*naproc
          if(icef(isea) /= undef) tmp(jsea)=icef(isea)
        end do
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      case ('ww3_twix')
        tmp(1:nseal) = 0.0
        where(tauice(1:nseal,1) /= undef) tmp(1:nseal)=tauice(1:nseal,1)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      case ('ww3_twiy')
        tmp(1:nseal) = 0.0
        where(tauice(1:nseal,2) /= undef) tmp(1:nseal)=tauice(1:nseal,2)
        rla_oasis_snd(:,1) = dble(tmp(1:nseal))
        call cpl_oasis_snd(ib_do, id_oasis_time, rla_oasis_snd, ll_action)
      end select
    enddo
    !
    !/ ------------------------------------------------------------------- /
  end subroutine snd_fields_to_ice
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief receive coupling fields from ice model.
  !>
  !> @param[in] id_lcomm  mpi communicator.
  !> @param[in] idfld  name of the exchange fields.
  !> @param[inout] fxn  first exchange field.
  !> @param[inout] fyn  second exchange field.
  !> @param[inout] fan  third exchange field.
  !>
  !> @author g. boutin  @date apr-2016
  !>
  subroutine rcv_fields_from_ice(id_lcomm, idfld, fxn, fyn, fan)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           g. boutin             |
    !/                  |                        fortran 90 |
    !/                  | last update :          april-2016 |
    !/                  +-----------------------------------+
    !/
    !/    aug-2016 : origination (g. boutin)  ( version 5.10 )
    !/
    !  1. purpose :
    !
    !     receive coupling fields from ice model
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
    !     cpl_oasis_rcv    subr.   w3oacpmd   receive fields from ice/ocean model
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
    integer                          :: ib_do, ib_i, ib_j, il_err, ndso
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
      if (idfld == 'ic5') then
        select case (rcv_fld(ib_do)%cl_field_name)
          !
          ! ice floe diameters (m)
          ! ----------------------------------------------------------------------
        case ('ww3__ic5')
          call cpl_oasis_rcv(ib_do, id_oasis_time, rla_oasis_rcv, ll_action)
          if (ll_action) then
            tmp(1:nseal) = rla_oasis_rcv(1:nseal,1)
            snd_buff(1:nsea) = 0.0
            do ib_i = 1, nseal
              ib_j = iaproc + (ib_i-1)*naproc
              snd_buff(ib_j) = tmp(ib_i)
            end do
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
          end if
        end select
        !
        ! ice concentration
        ! ----------------------------------------------------------------------
      else if (idfld == 'ice') then
        select case (rcv_fld(ib_do)%cl_field_name)
        case ('ww3__ice')
          call cpl_oasis_rcv(ib_do, id_oasis_time, rla_oasis_rcv, ll_action)
          if (ll_action) then
            tmp(1:nseal) = rla_oasis_rcv(1:nseal,1)
            snd_buff(1:nsea) = 0.0
            do ib_i = 1, nseal
              ib_j = iaproc + (ib_i-1)*naproc
              snd_buff(ib_j) = tmp(ib_i)
            end do
            !
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
          end if
        end select
        ! ice thickness
        ! ----------------------------------------------------------------------
      else if (idfld == 'ic1') then
        select case (rcv_fld(ib_do)%cl_field_name)
        case ('ww3__ic1')
          call cpl_oasis_rcv(ib_do, id_oasis_time, rla_oasis_rcv, ll_action)
          if (ll_action) then
            tmp(1:nseal) = rla_oasis_rcv(1:nseal,1)
            snd_buff(1:nsea) = 0.0
            do ib_i = 1, nseal
              ib_j = iaproc + (ib_i-1)*naproc
              snd_buff(ib_j) = tmp(ib_i)
            end do
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
          endif
          !
        end select
      end if
    end do
    !
    !/ ------------------------------------------------------------------- /
  end subroutine rcv_fields_from_ice
  !/ ------------------------------------------------------------------- /
  !/
end module w3igcmmd
!/
!/ ------------------------------------------------------------------- /
