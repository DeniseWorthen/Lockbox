  call flux_atmocn_ccpp(          &
       ! in
       logunit=logunit,           &
       nMax=aoflux_in%lsize,      &
       mask=aoflux_in%mask,       &
       zbot=aoflux_in%zbot,       &
       ubot=aoflux_in%ubot,       &
       vbot=aoflux_in%vbot,       &
       qbot=aoflux_in%shum,       &
       rbot=aoflux_in%dens,       &
       tbot=aoflux_in%tbot,       &
       ts=aoflux_in%tocn,         &
       usfc=aoflux_in%usfc,       &
       vsfc=aoflux_in%vsfc,       &
       psfc=aoflux_in%psfc,       &
       pbot=aoflux_in%pbot,       &
       lwdn=aoflux_in%lwdn,       &
       garea=aoflux_in%garea,     &
       gcomp=gcomp,               &
       maintask=maintask,         &
       ! optional in
       missval=0.0_r8,            &
       ! out
       sen=aoflux_out%sen,        &
       lat=aoflux_out%lat,        &
       lwup=aoflux_out%lwup,      &
       taux=aoflux_out%taux,      &
       tauy=aoflux_out%tauy,      &
       tref=aoflux_out%tref,      &
       qref=aoflux_out%qref,      &
       duu10n=aoflux_out%duu10n,  &
       evap=aoflux_out%evap,      &
       ustar_sv=aoflux_out%ustar, &
       re_sv=aoflux_out%re,       &
       ssq_sv=aoflux_out%ssq)
