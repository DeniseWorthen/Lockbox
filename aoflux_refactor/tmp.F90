module flux_atmocn_driver_mod

  !use shr_kind_mod,          only : R8=>SHR_KIND_R8, IN=>SHR_KIND_IN ! shared kinds
  !use shr_const_mod,         only : shr_const_spval
  !use shr_sys_mod,           only : shr_sys_abort
  !use shr_strconvert_mod,    only : toString
  use flux_atmocn_bulk_mod, only : flux_atmocn_bulk
  use flux_atmocn_ccpp_mod, only : flux_atmocn_ccpp

  implicit none
  public

  integer, private, parameter :: ocn_flux_scheme_bulk = 0
  integer, private, parameter :: ocn_flux_scheme_ccpp = 1

contains

  subroutine flux_atmocn( logunit, nMax, mask,               &
       ocn_surface_flux_scheme=ocn_surface_flux_scheme,      &
       zbot, ubot, vbot, qbot, rbot, tbot, ts,               &
       sen, lat, lwup, taux, tauy, tref, qref, duu10n, evap, &
       us, vs, thbot,                                        &
       gcomp, maintask, usfc, vsfc, psfc, pbot, lwdn, garea, &
       missval,                                              &
       ustar_sv, re_sv, ssq_sv )

    use ufs_kind_mod, only : R8=>SHR_KIND_R8
    use ESMF,         only : ESMF_GridComp

    implicit none

    !-------------------------
    ! required in
    !-------------------------
    integer,  intent(in) :: logunit
    integer,  intent(in) :: nMax
    real(R8), intent(in) :: mask (nMax)
    integer,  intent(in) :: ocn_surface_flux_scheme
    real(R8), intent(in) :: zbot (nMax)
    real(R8), intent(in) :: ubot (nMax)
    real(R8), intent(in) :: vbot (nMax)
    real(R8), intent(in) :: qbot (nMax)
    real(R8), intent(in) :: rbot (nMax)
    real(R8), intent(in) :: tbot (nMax)
    real(R8), intent(in) :: ts   (nMax)


    !-------------------------
    ! required out
    !-------------------------
    real(R8), intent(out) :: sen   (nMax)
    real(R8), intent(out) :: lat   (nMax)
    real(R8), intent(out) :: lwup  (nMax)
    real(R8), intent(out) :: taux  (nMax)
    real(R8), intent(out) :: tauy  (nMax)
    real(R8), intent(out) :: tref  (nMax)
    real(R8), intent(out) :: qref  (nMax)
    real(R8), intent(out) :: duu10n(nMax)
    real(R8), intent(out) :: evap  (nMax)

    !-------------------------
    ! optional in (bulk path)
    !-------------------------
    real(R8), intent(in), optional :: us   (nMax)   ! bulk
    real(R8), intent(in), optional :: vs   (nMax)   ! bulk
    real(R8), intent(in), optional :: thbot(nMax)   ! bulk

    !-------------------------
    ! optional in (ccpp path)
    !-------------------------
    type(ESMF_GridComp), intent(in), optional :: gcomp    ! ccpp
    logical,             intent(in), optional :: maintask ! ccpp
    real(R8), intent(in), optional :: usfc (nMax)         ! ccpp
    real(R8), intent(in), optional :: vsfc (nMax)         ! ccpp
    real(R8), intent(in), optional :: psfc (nMax)         ! ccpp
    real(R8), intent(in), optional :: pbot (nMax)         ! ccpp
    real(R8), intent(in), optional :: lwdn (nMax)         ! ccpp
    real(R8), intent(in), optional :: garea(nMax)         ! ccpp

    !-------------------------
    ! optional in/out
    !-------------------------
    real(R8), intent(in),  optional :: missval
    real(R8), intent(out), optional :: ustar_sv(nMax)
    real(R8), intent(out), optional :: re_sv   (nMax)
    real(R8), intent(out), optional :: ssq_sv  (nMax)

    ! local
    real(R8) :: spval

    spval = 0.0_R8
    if (present(missval)) spval = missval

    if (ocn_surface_flux_scheme == ocn_flux_scheme_bulk) then
!       call flux_atmocn_bulk(...)
  call flux_atmocn_bulk ( logunit=logunit, &
       nMax=aoflux_in%lsize,               &
       mask=aoflux_in%mask,                &
       zbot=aoflux_in%zbot,                &
       ubot=aoflux_in%ubot,                &
       vbot=aoflux_in%vbot,                &
       qbot=aoflux_in%shum,                &
       rbot=aoflux_in%dens,                &
       tbot=aoflux_in%tbot,                &
       ts=aoflux_in%tocn,                  &
       us=aoflux_in%uocn,                  &
       vs=aoflux_in%vocn,                  &
       thbot=aoflux_in%thbot,              &
       ! optional in
       missval=0.0_r8,                     &
       ! out
       sen=aoflux_out%sen,                 &
       lat=aoflux_out%lat,                 &
       lwup=aoflux_out%lwup,               &
       taux=aoflux_out%taux,               &
       tauy=aoflux_out%tauy,               &
       tref=aoflux_out%tref,               &
       qref=aoflux_out%qref,               &
       duu10n=aoflux_out%duu10n,           &
       evap=aoflux_out%evap)
    else if  (ocn_surface_flux_scheme == ocn_flux_scheme_ccpp) then
!       call flux_atmocn_ccpp(...)
  call flux_atmocn_ccpp( logunit=logunit, &
       nMax=aoflux_in%lsize,              &
       mask=aoflux_in%mask,               &
       zbot=aoflux_in%zbot,               &
       ubot=aoflux_in%ubot,               &
       vbot=aoflux_in%vbot,               &
       qbot=aoflux_in%shum,               &
       rbot=aoflux_in%dens,               &
       tbot=aoflux_in%tbot,               &
       ts=aoflux_in%tocn,                 &
       usfc=aoflux_in%usfc,               &
       vsfc=aoflux_in%vsfc,               &
       psfc=aoflux_in%psfc,               &
       pbot=aoflux_in%pbot,               &
       lwdn=aoflux_in%lwdn,               &
       garea=aoflux_in%garea,             &
       gcomp=gcomp,                       &
       maintask=maintask,                 &
       ! optional in
       missval=0.0_r8,                    &
       ! out
       sen=aoflux_out%sen,                &
       lat=aoflux_out%lat,                &
       lwup=aoflux_out%lwup,              &
       taux=aoflux_out%taux,              &
       tauy=aoflux_out%tauy,              &
       tref=aoflux_out%tref,              &
       qref=aoflux_out%qref,              &
       duu10n=aoflux_out%duu10n,          &
       evap=aoflux_out%evap,              &
       ustar_sv=aoflux_out%ustar,         &
       re_sv=aoflux_out%re,               &
       ssq_sv=aoflux_out%ssq)
    end if

    ! If caller provided optional diagnostics but chosen path does not set them:
    if (present(ustar_sv)) ustar_sv = spval
    if (present(re_sv))    re_sv    = spval
    if (present(ssq_sv))   ssq_sv   = spval

  end subroutine flux_atmocn
end module flux_atmocn_driver_mod
