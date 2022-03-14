program testvars

  implicit none

  ! example list of 'tags' set in shel/multi.inp
  integer, parameter :: ntags = 8
  integer, parameter :: nogrp = 10
  integer, parameter :: maxvars = 24

  ! dims: x,y = nx,ny
  !         s = noswll
  !         b = 'bedforms', currently 3
  !         m = 'p2sms', currently 15 (P2MSF(2):P2MSF(3))
  !         p = partitions, currently 3 (eg 1:USSPF(2),NK+1:NK+USSPF(2))
  !         k = nk, number of freqs (NK, also eg E3DF(2,1):E3DF(3,1))
  !         t = time
   
  !type, public :: varatts
  type :: varatts
    character(len= 5) :: tag
    character(len=10) :: varname
    character(len=48) :: long_name
    character(len= 5) :: unit_name
    character(len= 8) :: dims
    logical           :: validout
  end type
  
  !type(varatts), dimension(nogrp,maxvars), public :: gridoutvar
  type(varatts), dimension(nogrp,maxvars) :: gridoutvar

  integer :: i,j,n,nt,ii
  logical :: s_axis = .false., b_axis = .false., m_axis = .false., p_axis = .false., k_axis = .false.

  character(len=4), dimension(ntags) :: inptags = (/'WND ', 'HS  ', 'FP  ', 'DP  ', 'PHS ', 'PTP ', 'PDIR', 'LAN '/)

   gridoutvar(:,:)%tag = ""
   gridoutvar(:,:)%varname = ""
   gridoutvar(:,:)%long_name = ""
   gridoutvar(:,:)%unit_name = ""
   gridoutvar(:,:)%dims = ""
   gridoutvar(:,:)%validout = .false.

   ! TODO: replace nd units w/ correct values
   gridoutvar(1,1:15) = [ &
    varatts( "DPT  ", "DW        ", "Water depth                                     ", "m    ", "x y t   ", .false.) , &
    varatts( "CUR  ", "CX        ", "Mean current, x-component                       ", "m/s  ", "x y t   ", .false.) , &
    varatts( "CUR  ", "CY        ", "Mean current, y-component                       ", "m/s  ", "x y t   ", .false.) , &
    varatts( "WND  ", "UAX       ", "Mean wind, x-component                          ", "m/s  ", "x y t   ", .false.) , &
    varatts( "WND  ", "UAY       ", "Mean wind, y-component                          ", "m/s  ", "x y t   ", .false.) , &
    varatts( "AST  ", "AS        ", "Air-sea temperature difference                  ", "deg C", "x y t   ", .false.) , &
    varatts( "WLV  ", "WLV       ", "Water levels                                    ", "m    ", "x y t   ", .false.) , &
    varatts( "ICE  ", "ICE       ", "Ice coverage                                    ", "nd   ", "x y t   ", .false.) , &
    varatts( "IBG  ", "BERG      ", "Iceberg-induced damping                         ", "nd   ", "x y t   ", .false.) , &
    varatts( "TAU  ", "TAUAX     ", "Atm momentum x                                  ", "nd   ", "x y t   ", .false.) , &
    varatts( "TAU  ", "TAUAY     ", "Atm momentum y                                  ", "nd   ", "x y t   ", .false.) , &
    varatts( "RHO  ", "RHOAIR    ", "Air density                                     ", "nd   ", "x y t   ", .false.) , &
    varatts( "D50  ", "SED_D50   ", "Median sediment grain size                      ", "nd   ", "x y t   ", .false.) , &
    varatts( "IC1  ", "ICH       ", "Ice thickness                                   ", "nd   ", "x y t   ", .false.) , &
    varatts( "IC5  ", "ICF       ", "Ice floe diameter                               ", "nd   ", "x y t   ", .false.)   &
                        ]

   gridoutvar(2,1:18) = [ &
    varatts( "HS   ", "HS        ", "Significant wave height                         ", "m    ", "x y t   ", .false.) , &
    varatts( "LM   ", "WLM       ", "Mean wave length                                ", "m    ", "x y t   ", .false.) , &
    varatts( "T02  ", "T02       ", "Mean wave period (Tm0,2)                        ", "s    ", "x y t   ", .false.) , &
    varatts( "T0M1 ", "T0M1      ", "Mean wave period (Tm0,-1)                       ", "s    ", "x y t   ", .false.) , &
    varatts( "T01  ", "T01       ", "Mean wave period (Tm0,1)                        ", "s    ", "x y t   ", .false.) , &
    varatts( "FP   ", "FP0       ", "Peak frequency                                  ", "Hz   ", "x y t   ", .false.) , &
    varatts( "DIR  ", "THM       ", "Mean wave direction                             ", "rad  ", "x y t   ", .false.) , &
    varatts( "SPR  ", "THS       ", "Mean directional spread                         ", "rad  ", "x y t   ", .false.) , &
    varatts( "DP   ", "THP0      ", "Peak direction                                  ", "rad  ", "x y t   ", .false.) , &
    varatts( "HSIG ", "HSIG      ", "Infragravity height                             ", "nd   ", "x y t   ", .false.) , &
    varatts( "MXE  ", "STMAXE    ", "Max surface elev (STE)                          ", "m    ", "x y t   ", .false.) , &
    varatts( "MXES ", "STMAXD    ", "St Dev Max surface elev (STE)                   ", "m    ", "x y t   ", .false.) , &
    varatts( "MXH  ", "HMAXE     ", "Max wave height (S.)                            ", "m    ", "x y t   ", .false.) , &
    varatts( "MXHC ", "HCMAXE    ", "Max wave height from crest (STE)                ", "m    ", "x y t   ", .false.) , &
    varatts( "SDMH ", "HMAXD     ", "St Dev of MXC (STE)                             ", "m    ", "x y t   ", .false.) , &
    varatts( "SDMHC", "HCMAXD    ", "St Dev of MXHC (STE)                            ", "m    ", "x y t   ", .false.) , &
    varatts( "WBT  ", "WBT       ", "Dominant wave breaking probability bT           ", "m    ", "x y t   ", .false.) , &
    varatts( "TP   ", "FP0p      ", "Peak period (from peak freq)                    ", "Hz   ", "x y t   ", .false.)   &
                        ]

   gridoutvar(3,1:6) = [ &
    varatts( "EF   ", "EF        ", "1D spectral density                             ", "nd   ", "x y k t ", .false.) , &
    varatts( "TH1M ", "TH1M      ", "Mean wave direction from a1,b2                  ", "nd   ", "x y k t ", .false.) , &
    varatts( "STH1M", "STH1M     ", "Directional spreading from a1,b2                ", "nd   ", "x y k t ", .false.) , &
    varatts( "TH2M ", "TH2M      ", "Mean wave direction from a2,b2                  ", "nd   ", "x y k t ", .false.) , &
    varatts( "STH2M", "STH2M     ", "Directional spreading from a2,b2                ", "nd   ", "x y k t ", .false.) , &
    varatts( "WN   ", "WN        ", "Wavenumber array                                ", "nd   ", "x y k t ", .false.)   &
                        ]

   gridoutvar(4,1:17) = [ &
    varatts( "PH   ", "PHS       ", "Partitioned wave heights                        ", "m    ", "x y s t ", .false.) , &
    varatts( "PTP  ", "PTP       ", "Partitioned peak period                         ", "s    ", "x y s t ", .false.) , &
    varatts( "PLP  ", "PLP       ", "Partitioned peak wave length                    ", "m    ", "x y s t ", .false.) , &
    varatts( "PDIR ", "PDIR      ", "Partitioned mean direction                      ", "nd   ", "x y s t ", .false.) , &
    varatts( "PSPR ", "PSI       ", "Partitioned mean directional spread             ", "nd   ", "x y s t ", .false.) , &
    varatts( "PWS  ", "PWS       ", "Partitioned wind sea fraction                   ", "nd   ", "x y s t ", .false.) , &
    varatts( "PDP  ", "PDP       ", "Peak wave direction of partition                ", "nd   ", "x y s t ", .false.) , &
    varatts( "PQP  ", "PQP       ", "Goda peakdedness parameter of partition         ", "nd   ", "x y s t ", .false.) , &
    varatts( "PPE  ", "PPE       ", "JONSWAP peak enhancement factor of partition    ", "nd   ", "x y s t ", .false.) , &
    varatts( "PGW  ", "PGW       ", "Gaussian frequency width of partition           ", "nd   ", "x y s t ", .false.) , &
    varatts( "PSW  ", "PSW       ", "Spectral width of partition                     ", "nd   ", "x y s t ", .false.) , &
    varatts( "PTM10", "PTM1      ", "Mean wave period (m-1,0) of partition           ", "nd   ", "x y s t ", .false.) , &
    varatts( "PT01 ", "PT1       ", "Mean wave period (m0,1) of partition            ", "nd   ", "x y s t ", .false.) , &
    varatts( "PT02 ", "PT2       ", "Mean wave period (m0,2) of partition            ", "nd   ", "x y s t ", .false.) , &
    varatts( "PEP  ", "PEP       ", "Peak spectral density of partition              ", "nd   ", "x y s t ", .false.) , &
    varatts( "TWS  ", "PWST      ", "Total wind sea fraction                         ", "nd   ", "x y t   ", .false.) , &
    varatts( "PNR  ", "PNR       ", "Number of partitions                            ", "nd   ", "x y t   ", .false.)   &
                        ]

   gridoutvar(5,1:14) = [ &
    varatts( "UST  ", "USTX      ", "Friction velocity x                             ", "m/s  ", "x y t   ", .false.) , &
    varatts( "UST  ", "USTY      ", "Friction velocity y                             ", "m/s  ", "x y t   ", .false.) , &
    varatts( "CHA  ", "CHA       ", "Charnock parameter                              ", "nd   ", "x y t   ", .false.) , &
    varatts( "CGE  ", "CGE       ", "Energy flux                                     ", "nd   ", "x y t   ", .false.) , &
    varatts( "FAW  ", "PHIAW     ", "Air-sea energy flux                             ", "nd   ", "x y t   ", .false.) , &
    varatts( "TAW  ", "TAUWIX    ", "Net wave-supported stress x                     ", "nd   ", "x y t   ", .false.) , &
    varatts( "TAW  ", "TAUWIY    ", "Net wave-supported stress y                     ", "nd   ", "x y t   ", .false.) , &
    varatts( "TWA  ", "TAUWNX    ", "Negative part of the wave-supported stress x    ", "nd   ", "x y t   ", .false.) , &
    varatts( "TWA  ", "TAUWNY    ", "Negative part of the wave-supported stress y    ", "nd   ", "x y t   ", .false.) , &
    varatts( "WCC  ", "WCC       ", "Whitecap coverage                               ", "nd   ", "x y t   ", .false.) , &
    varatts( "WCF  ", "WCF       ", "Whitecap thickness                              ", "nd   ", "x y t   ", .false.) , &
    varatts( "WCH  ", "WCH       ", "Mean breaking height                            ", "nd   ", "x y t   ", .false.) , &
    varatts( "WCM  ", "WCM       ", "Whitecap moment                                 ", "nd   ", "x y t   ", .false.) , &
    varatts( "FW   ", "FWS       ", "Wind sea mean period                            ", "nd   ", "x y t   ", .false.)   &
                        ]

   gridoutvar(6,1:24) = [ &
    varatts( "SXY  ", "SXX       ", "Radiation stresses xx                           ", "nd   ", "x y t   ", .false.) , &
    varatts( "SXY  ", "SYY       ", "Radiation stresses yy                           ", "nd   ", "x y t   ", .false.) , &
    varatts( "SXY  ", "SXY       ", "Radiation stresses xy                           ", "nd   ", "x y t   ", .false.) , &
    varatts( "TWO  ", "TAUOX     ", "Wave to ocean momentum flux x                   ", "nd   ", "x y t   ", .false.) , &
    varatts( "TWO  ", "TAUOY     ", "Wave to ocean momentum flux y                   ", "nd   ", "x y t   ", .false.) , &
    varatts( "BHD  ", "BHD       ", "Bernoulli head (J term)                         ", "nd   ", "x y t   ", .false.) , &
    varatts( "FOC  ", "PHIOC     ", "Wave to ocean energy flux                       ", "nd   ", "x y t   ", .false.) , &
    varatts( "TUS  ", "TUSX      ", "Stokes transport x                              ", "nd   ", "x y t   ", .false.) , &
    varatts( "TUS  ", "TUSY      ", "Stokes transport y                              ", "nd   ", "x y t   ", .false.) , &
    varatts( "USS  ", "USSX      ", "Surface Stokes drift x                          ", "m/s  ", "x y t   ", .false.) , &
    varatts( "USS  ", "USSY      ", "Surface Stokes drift y                          ", "m/s  ", "x y t   ", .false.) , &
    varatts( "P2S  ", "PRMS      ", "Second-order sum pressure                       ", "nd   ", "x y t   ", .false.) , &
    varatts( "P2S  ", "TPMS      ", "Second-order sum pressure                       ", "nd   ", "x y t   ", .false.) , &
    varatts( "USF  ", "US3DX     ", "Spectrum of surface Stokes drift x              ", "nd   ", "x y k t ", .false.) , &
    varatts( "USF  ", "US3DY     ", "Spectrum of surface Stokes drift y              ", "nd   ", "x y k t ", .false.) , &
    varatts( "P2L  ", "P2SMS     ", "Micro seism  source term                        ", "nd   ", "x y m t ", .false.) , &
    varatts( "TWI  ", "TAUICEX   ", "Wave to sea ice stress x                        ", "nd   ", "x y t   ", .false.) , &
    varatts( "TWI  ", "TAUICEY   ", "Wave to sea ice stress y                        ", "nd   ", "x y t   ", .false.) , &
    varatts( "FIC  ", "PHICE     ", "Wave to sea ice energy flux                     ", "nd   ", "x y t   ", .false.) , &
    varatts( "USP  ", "USSPX     ", "Partitioned surface Stokes drift x              ", "nd   ", "x y p t ", .false.) , &
    varatts( "USP  ", "USSPY     ", "Partitioned surface Stokes drift y              ", "nd   ", "x y p t ", .false.) , &
    varatts( "TWC  ", "TAUOCX    ", "Wave to ?? stress x                             ", "nd   ", "x y t   ", .false.) , &
    varatts( "TWC  ", "TAUOCY    ", "Wave to ?? stress y                             ", "nd   ", "x y t   ", .false.) , &
    varatts( "LAN  ", "LANGMT    ", "Turbulent Langmuir number (La_t)                ", "nd   ", "x y t   ", .false.)   &
                        ]

   gridoutvar(7,1:8) = [ &
    varatts( "ABR  ", "ABAX      ", "Near bottom rms wave excursion amplitudes x     ", "m    ", "x y t   ", .false.) , &
    varatts( "ABR  ", "ABAY      ", "Near bottom rms wave excursion amplitudes y     ", "m    ", "x y t   ", .false.) , &
    varatts( "UBR  ", "UBAX      ", "Near bottom rms wave velocities x               ", "m/s  ", "x y t   ", .false.) , &
    varatts( "UBR  ", "UBAY      ", "Near bottom rms wave velocities y               ", "m/s  ", "x y t   ", .false.) , &
    varatts( "BED  ", "Bedforms  ", "Bedforms                                        ", "nd   ", "x y b t ", .false.) , &
    varatts( "FBB  ", "PHIBBL    ", "Energy flux due to bottom friction              ", "nd   ", "x y t   ", .false.) , &
    varatts( "TBB  ", "TAUBBLX   ", "Momentum flux due to bottom friction x          ", "nd   ", "x y t   ", .false.) , &
    varatts( "TBB  ", "TAUBBLY   ", "Momentum flux due to bottom friction y          ", "nd   ", "x y t   ", .false.)   &
                        ]

   gridoutvar(8,1:9) = [ &
    varatts( "MSS  ", "MSSX      ", "Mean square slope x                             ", "nd   ", "x y t   ", .false.) , &
    varatts( "MSS  ", "MSSY      ", "Mean square slope y                             ", "nd   ", "x y t   ", .false.) , &
    varatts( "MSC  ", "MSCX      ", "Spectral level at high frequency tail x         ", "nd   ", "x y t   ", .false.) , &
    varatts( "MSC  ", "MSCY      ", "Spectral level at high frequency tail y         ", "nd   ", "x y t   ", .false.) , &
    varatts( "WL02 ", "WL02X     ", "East/X North/Y mean wavelength component        ", "nd   ", "x y t   ", .false.) , &
    varatts( "WL02 ", "WL02Y     ", "East/X North/Y mean wavelength component        ", "nd   ", "x y t   ", .false.) , &
    varatts( "AXT  ", "ALPXT     ", "Correl sea surface gradients (x,t)              ", "nd   ", "x y t   ", .false.) , &
    varatts( "AYT  ", "ALPYT     ", "Correl sea surface gradients (y,t)              ", "nd   ", "x y t   ", .false.) , &
    varatts( "AXY  ", "ALPXY     ", "Correl sea surface gradients (x,y)              ", "nd   ", "x y t   ", .false.)   &
                        ]

   gridoutvar(9,1:5) = [ &
    varatts( "DTD  ", "DTDYN     ", "Average time step in integration                ", "nd   ", "x y t   ", .false.) , &
    varatts( "FC   ", "FCUT      ", "Cut-off frequency                               ", "nd   ", "x y t   ", .false.) , &
    varatts( "CFX  ", "CFLXYMAX  ", "Max. CFL number for spatial advection           ", "nd   ", "x y t   ", .false.) , &
    varatts( "CFD  ", "CFLTHMAX  ", "Max. CFL number for theta-advection             ", "nd   ", "x y t   ", .false.) , &
    varatts( "CFK  ", "CFLKMAX   ", "Max. CFL number for k-advection                 ", "nd   ", "x y t   ", .false.)  &
                        ]

   gridoutvar(10,1:2) = [ &
    varatts( "U1   ", "U1        ", "User defined 1                                  ", "nd   ", "x y t   ", .false.) , &
    varatts( "U2   ", "U2        ", "User defined 2                                  ", "nd   ", "x y t   ", .false.)  &
                        ]

   do j = 1,nogrp
    n = 0
    do i = 1,maxvars
     !print '(4a,l)',trim(gridoutvar(j,i)%tag)
     if(len_trim(gridoutvar(j,i)%tag) > 0)then
       n = n+1
       !print *,trim(gridoutvar(j,i)%tag),trim(gridoutvar(j,i)%varname), trim(gridoutvar(j,i)%long_name)
     end if
    enddo
    print *,'group ',j,' variables ',n
   end do

   do j = 1,nogrp
    do i = 1,maxvars
     if(len_trim(gridoutvar(j,i)%tag) > 0)then
      do nt = 1,ntags
       if(trim(gridoutvar(j,i)%tag) == trim(inptags(nt)))gridoutvar(j,i)%validout = .true.
      end do
     end if
    end do
   end do

   do j = 1,nogrp
    do i = 1,maxvars
     if(gridoutvar(j,i)%validout)print *,trim(gridoutvar(j,i)%tag),'  ',&
                                         trim(gridoutvar(j,i)%varname),'  ',&
                                         trim(gridoutvar(j,i)%long_name),'  ',&
                                         trim(gridoutvar(j,i)%dims)
     if(gridoutvar(j,i)%validout) then
       if(scan(trim(gridoutvar(j,i)%dims),'s') > 0)s_axis = .true.
       if(scan(trim(gridoutvar(j,i)%dims),'b') > 0)b_axis = .true.
       if(scan(trim(gridoutvar(j,i)%dims),'m') > 0)m_axis = .true.
       if(scan(trim(gridoutvar(j,i)%dims),'p') > 0)p_axis = .true.
       if(scan(trim(gridoutvar(j,i)%dims),'k') > 0)k_axis = .true.
     end if
    end do
   end do


end program testvars
