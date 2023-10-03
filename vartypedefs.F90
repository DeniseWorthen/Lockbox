module vartypedefs

  implicit none

  integer, parameter :: maxvars = 40          !< The maximum number of variables written to a file

  type :: vardefs
     character(len= 10)   :: input_var_name    !< A variable's  input variable name
     character(len= 10)   :: output_var_name   !< A variable's output variable name
     character(len=120)   :: long_name         !< A variable's long name
     character(len= 20)   :: units             !< A variable's unit
     character(len= 10)   :: var_remapmethod   !< A variable's mapping method
     integer              :: var_dimen         !< A variable's dimensionality
     character(len=  2)   :: var_grid          !< A variable's input grid location; all output locations are on cell centers
     character(len= 10)   :: var_pair          !< A variable's pair
     character(len=  2)   :: var_pair_grid
  end type vardefs

  type(vardefs), public :: ocnvars(maxvars)   !< Attribute definitions for the variables

contains

  !> Define the variables written to the tripole grid file
  !!
  !! @author Denise.Worthen@noaa.gov

  subroutine ocnvars_typedefine

    ! local variables
    integer :: ii = 0

    !set defaults
    ocnvars(:)%input_var_name=''
    ocnvars(:)%var_grid  = 'Ct'
    ocnvars(:)%var_remapmethod  = 'bilinear'
    ocnvars(:)%var_dimen = 2
    ocnvars(:)%var_pair = ''
    ocnvars(:)%var_pair_grid = ''
    ocnvars(:)%long_name = ''   ! obtained from input file
    ocnvars(:)%units = ''       ! obtained from input file

    ! 2D states with native grid location on cell centers; remapped bilinearly
    ii = ii + 1; ocnvars(ii)%input_var_name  = 'SSH'
    ii = ii + 1; ocnvars(ii)%input_var_name  = 'SST'
    ii = ii + 1; ocnvars(ii)%input_var_name  = 'SSS'
    ii = ii + 1; ocnvars(ii)%input_var_name  = 'speed'
    !ii = ii + 1; ocnvars(ii)%input_var_name  = 'mld'
    ii = ii + 1; ocnvars(ii)%input_var_name  = 'ePBL'
    ii = ii + 1; ocnvars(ii)%input_var_name  = 'MLD_003'
    ii = ii + 1; ocnvars(ii)%input_var_name  = 'MLD_0125'

    ! 2D fluxes with native grid location on cell centers; remapped conservatively
    ii = ii + 1; ocnvars(ii)%input_var_name  = 'latent'    ; ocnvars(ii)%var_remapmethod  = 'conserve'
    ii = ii + 1; ocnvars(ii)%input_var_name  = 'sensible'  ; ocnvars(ii)%var_remapmethod  = 'conserve'
    ii = ii + 1; ocnvars(ii)%input_var_name  = 'SW'        ; ocnvars(ii)%var_remapmethod  = 'conserve'
    ii = ii + 1; ocnvars(ii)%input_var_name  = 'LW'        ; ocnvars(ii)%var_remapmethod  = 'conserve'
    ii = ii + 1; ocnvars(ii)%input_var_name  = 'evap'      ; ocnvars(ii)%var_remapmethod  = 'conserve'
    ii = ii + 1; ocnvars(ii)%input_var_name  = 'lprec'     ; ocnvars(ii)%var_remapmethod  = 'conserve'
    ii = ii + 1; ocnvars(ii)%input_var_name  = 'fprec'     ; ocnvars(ii)%var_remapmethod  = 'conserve'
    ii = ii + 1; ocnvars(ii)%input_var_name  = 'LWLatSens' ; ocnvars(ii)%var_remapmethod  = 'conserve'
    ii = ii + 1; ocnvars(ii)%input_var_name  = 'Heat_PmE'  ; ocnvars(ii)%var_remapmethod  = 'conserve'

    ! 2D vector states on stagger locations; remapped bilinearly
    ii = ii + 1; ocnvars(ii)%input_var_name  = 'SSU'
                 ocnvars(ii)%var_grid = 'Cu'
                 ocnvars(ii)%var_pair = 'SSV'
                 ocnvars(ii)%var_pair_grid = 'Cv'

    ii = ii + 1; ocnvars(ii)%input_var_name  = 'SSV'
                 ocnvars(ii)%var_grid = 'Cv'
                 ocnvars(ii)%var_pair = 'SSU'
                 ocnvars(ii)%var_pair_grid = 'Cu'

    ! 2D vector fluxes on stagger locations; remapped conservatively
    ii = ii + 1; ocnvars(ii)%input_var_name = 'taux'
                 ocnvars(ii)%var_grid = 'Cu'
                 ocnvars(ii)%var_pair = 'tauy'
                 ocnvars(ii)%var_pair_grid = 'Cv'
                 ocnvars(ii)%var_remapmethod = 'conserve'

    ii = ii + 1; ocnvars(ii)%input_var_name = 'tauy'
                 ocnvars(ii)%var_grid = 'Cv'
                 ocnvars(ii)%var_pair = 'taux'
                 ocnvars(ii)%var_pair_grid = 'Cu'
                 ocnvars(ii)%var_remapmethod = 'conserve'

    ! 3D scalars with native grid location on cell centers; remapped bilinearly
    ii = ii + 1; ocnvars(ii)%input_var_name  = 'temp' ; ocnvars(ii)%var_dimen = 3
    ii = ii + 1; ocnvars(ii)%input_var_name  = 'so'   ; ocnvars(ii)%var_dimen = 3

    ! 3D vectors on stagger locations; remapped bilinearly
    ii = ii + 1; ocnvars(ii)%input_var_name  = 'uo'
                 ocnvars(ii)%var_grid = 'Cu'
                 ocnvars(ii)%var_pair = 'vo'
                 ocnvars(ii)%var_pair_grid = 'Cv'
                 ocnvars(ii)%var_dimen = 3

    ii = ii + 1; ocnvars(ii)%input_var_name  = 'vo'
                 ocnvars(ii)%var_grid = 'Cv'
                 ocnvars(ii)%var_pair = 'uo'
                 ocnvars(ii)%var_pair_grid = 'Cu'
                 ocnvars(ii)%var_dimen = 3

    ! set default output name
    ocnvars(:)%output_var_name = ocnvars(:)%input_var_name

  end subroutine ocnvars_typedefine
end module vartypedefs
