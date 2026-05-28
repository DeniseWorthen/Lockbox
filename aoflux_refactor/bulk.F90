  call flux_atmocn ( &
       ! in
       logunit=logunit,                                 &
       nMax=aoflux_in%lsize,                            &
       mask=aoflux_in%mask,                             &
       zbot=aoflux_in%zbot,                             &
       ubot=aoflux_in%ubot,                             &
       vbot=aoflux_in%vbot,                             &
       qbot=aoflux_in%shum,                             &
       rbot=aoflux_in%dens,                             &
       tbot=aoflux_in%tbot,                             &
       ts=aoflux_in%tocn,                               &
       us=aoflux_in%uocn,                               &
       vs=aoflux_in%vocn,                               &
       thbot=aoflux_in%thbot,                           &
       ! optional in
       ocn_surface_flux_scheme=ocn_surface_flux_scheme, &
       missval=0.0_r8,                                  &
       ! out
       sen=aoflux_out%sen,                              &
       lat=aoflux_out%lat,                              &
       lwup=aoflux_out%lwup,                            &
       taux=aoflux_out%taux,                            &
       tauy=aoflux_out%tauy,                            &
       tref=aoflux_out%tref,                            &
       qref=aoflux_out%qref,                            &
       duu10n=aoflux_out%duu10n,                        &
       evap=aoflux_out%evap)
