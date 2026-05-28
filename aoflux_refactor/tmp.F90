module flux_atmocn_driver_mod

  use shr_kind_mod,          only : R8=>SHR_KIND_R8, IN=>SHR_KIND_IN ! shared kinds
  use shr_const_mod,         only : shr_const_spval
  use shr_sys_mod,           only : shr_sys_abort
  use shr_strconvert_mod,    only : toString
  use flux_atmocn_Large_mod, only : flux_atmocn_Large
  use flux_atmocn_COARE_mod, only : flux_atmocn_COARE
  use flux_atmocn_UA_mod,    only : flux_atmocn_UA

  implicit none
  public

  integer, private, parameter :: ocn_flux_scheme_large_and_pond = 0
  integer, private, parameter :: ocn_flux_scheme_ccpp = 1

contains

  subroutine flux_atmocn( logunit, nMax, mask,               &
       zbot, ubot, vbot, qbot, rbot, tbot, ts,               &
       ocn_surface_flux_scheme,                              &
       sen, lat, lwup, taux, tauy, tref, qref, duu10n, evap, &
       us, vs, thbot,                                        &
       usfc, vsfc, psfc, pbot, lwdn, garea, gcomp, maintask, &
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
    real(R8), intent(in) :: zbot (nMax)
    real(R8), intent(in) :: ubot (nMax)
    real(R8), intent(in) :: vbot (nMax)
    real(R8), intent(in) :: qbot (nMax)
    real(R8), intent(in) :: rbot (nMax)
    real(R8), intent(in) :: tbot (nMax)
    real(R8), intent(in) :: ts   (nMax)
    integer,  intent(in) :: ocn_surface_flux_scheme

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
    real(R8), intent(in), optional :: usfc (nMax)         ! ccpp
    real(R8), intent(in), optional :: vsfc (nMax)         ! ccpp
    real(R8), intent(in), optional :: psfc (nMax)         ! ccpp
    real(R8), intent(in), optional :: pbot (nMax)         ! ccpp
    real(R8), intent(in), optional :: lwdn (nMax)         ! ccpp
    real(R8), intent(in), optional :: garea(nMax)         ! ccpp
    type(ESMF_GridComp), intent(in), optional :: gcomp    ! ccpp
    logical,             intent(in), optional :: maintask ! ccpp

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

    if (ocn_surface_flux_scheme == ocn_flux_scheme_large_and_pond) then
       call flux_atmocn_bulk(...)
    else if  (ocn_surface_flux_scheme == ocn_flux_scheme_ccpp) then
       call flux_atmocn_ccpp(...)
    end if

    ! If caller provided optional diagnostics but chosen path does not set them:
    if (present(ustar_sv)) ustar_sv = spval
    if (present(re_sv))    re_sv    = spval
    if (present(ssq_sv))   ssq_sv   = spval

  end subroutine flux_atmocn
end module flux_atmocn_driver_mod
