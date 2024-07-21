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
module w3oacpmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           a. thevenin             |
  !/                  |                        fortran 90 |
  !/                  | last update :         22-mar-2021 |
  !/                  +-----------------------------------+
  !/
  !/      july-2013 : origination.                         ( version 4.18 )
  !/                    for upgrades see subroutines.
  !/     april-2016 : add comments (j. pianezze)           ( version 5.07 )
  !/    25-sep-2020 : coupling at t+0 support              ( version 7.10 )
  !/    22-mar-2021 : adds extra coupling fields           ( version 7.13 )
  !/
  !/    copyright 2009-2012 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     generic module used for coupling applications with oasis3-mct
  !
  !  2. variables and types :
  !
  !  3. subroutines and functions :
  !
  !      name                type  scope    description
  !     ----------------------------------------------------------------
  !      cpl_oasis_init      subr. public   initialize the coupling
  !      cpl_oasis_grid      subr. public   grids defintion
  !      cpl_oasis_define    subr. public   partition definition
  !      cpl_oasis_snd       subr. public   send fields to ocean/atmos model
  !      cpl_oasis_rcv       subr. public   receive fields from ocean/atmos model
  !      cpl_oasis_finalize  subr. public   finalize the coupling
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name                 type    module    description
  !     --------------------------------------------------------------------
  !      get_list_exch_field  subr.   w3oacpmd  list of the exchanged fields
  !      strsplit             subr.   w3servmd  splits string into words
  !     --------------------------------------------------------------------
  !
  !  5. remarks
  !
  !     module adapted from wrf-oasis routine implemented by
  !     sebastien masson (ipsl), guillaume samson (legos) and eric maisonnave (cerfacs)
  !
  !  6. switches :
  !  7. source code :
  !
  !/ ------------------------------------------------------------------- /
  !
  use mod_oasis                                      ! oasis3-mct module
  !
  implicit none
  private
  !
  integer               :: il_compid                 ! component model id returned by oasis_init_comp
  character(len=6)      :: cl_model_name = 'wwatch'  ! model name (same as in namcouple)
  integer               :: il_err                    ! return error code
  integer, public       :: il_nb_rcv, il_nb_snd      ! number of coupling fields
  integer, parameter    :: ip_maxfld=50              ! maximum number of coupling fields
  integer               :: nnodes                    ! total numbers of cell in the grid
  !
  type, public          :: cpl_field                 ! type for coupling field information
    character(len = 8) :: cl_field_name             ! name of the coupling field
    integer            :: il_field_id               ! field id
  end type cpl_field
  !
  type(cpl_field), dimension(ip_maxfld), public :: rcv_fld, snd_fld   ! coupling fields
  !
  integer, public       :: id_oasis_time=0           ! time counter for coupling exchanges
  !
  logical, public       :: cplt0                     ! flag for coupling at t+0
  !
  ! * accessibility
  public cpl_oasis_init
  public cpl_oasis_grid
  public cpl_oasis_define
  public cpl_oasis_snd
  public cpl_oasis_rcv
  public cpl_oasis_finalize
  !
contains
  !/ ------------------------------------------------------------------- /
  subroutine cpl_oasis_init(id_lcomm)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           a. thevenin             |
    !/                  |                        fortran 90 |
    !/                  | last update :          april-2016 |
    !/                  +-----------------------------------+
    !/
    !/     jul-2013 : origination.                    ( version 4.18 )
    !/     april-2016 : add comments (j. pianezze)    ( version 5.07 )
    !/
    !  1. purpose :
    !
    !     initialize the coupling
    !
    !  2. method :
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     id_lcomm     int.     o    mpi communicator
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !  5. called by :
    !
    !      name                type     module    description
    !     ----------------------------------------------------------------
    !      ww3_shel            prog.    -         main program
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks :
    !  8. structure :
    !  9. switches :
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    ! * argument
    integer, intent(out) :: id_lcomm                   ! model local communicator
    !
    !----------------------------------------------------------------------
    ! * executable part
    !
    !! initialize the coupling
    call oasis_init_comp(il_compid, cl_model_name, il_err)
    if (il_err /= 0) then
      call oasis_abort(il_compid, 'cpl_oasis_init', 'problem during oasis_init_comp')
    endif
    !
    !! get the value of a local mpi communicator to be used by ww3 for its internal parallelisation
    call oasis_get_localcomm(id_lcomm, il_err)
    if (il_err /= 0) then
      call oasis_abort(il_compid, 'cpl_oasis_init', 'problem during oasis_get_localcomm')
    endif
    !
    !/ ------------------------------------------------------------------- /
  end subroutine cpl_oasis_init
  !/ ------------------------------------------------------------------- /
  subroutine cpl_oasis_grid(ld_master,id_lcomm)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           a. thevenin             |
    !/                  |           v. garnier              |
    !/                  |           m. accensi              |
    !/                  |                        fortran 90 |
    !/                  | last update :          april-2016 |
    !/                  +-----------------------------------+
    !/
    !/    jul-2013 : origination.                    ( version 4.18 )
    !/    april-2016 : add comments (j. pianezze)    ( version 5.07 )
    !/    sept-2016 : correct bug mpi (j. pianezze)  ( version 5.12 )
    !/
    !  1. purpose :
    !
    !     grid data file definition
    !
    !  2. method :
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     ld_master         bool.    i     flag to know the master process
    !     id_lcomm          int.     i     mpi communicator
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !  5. called by :
    !
    !      name                type     module    description
    !     ----------------------------------------------------------------
    !      ww3_shel            prog.    -         main program
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks :
    !  8. structure :
    !  9. switches :
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    use constants, only: radius, dera
    use w3gdatmd,  only: nx, ny, flagll, xgrd, ygrd, mapsta, &
         & hpfac, hqfac, gtype, &
         & ungtype, rlgtype, clgtype, smctype
    include "mpif.h"
    !
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    logical, intent(in) :: ld_master    ! master process or not
    integer, intent(in) :: id_lcomm     ! model local communicator
    !
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer, allocatable :: mask(:,:)
    integer              :: i, ix, iy, nxw, nxe, nys, nyn, inode, ierr_mpi
    real, allocatable    :: lon(:,:),lat(:,:),area(:,:),      &
         corlon(:,:,:),corlat(:,:,:)
    real                 :: factor
    !/ ------------------------------------------------------------------- /
    !
    if (ld_master) then
      !
      !
      ! 0. create grids file
      ! --------------------------------
      call oasis_start_grids_writing(ierr_mpi)
      !
      ! 1. get the lat/lon/corners,areas and masks
      ! -------------------------------------------
      if (gtype .eq. rlgtype .or. gtype .eq. clgtype) then
        !
        if (flagll) then
          factor = 1.
        else
          factor = 1. / (radius * dera)
        end if
        !
        ! 1.1. regular and curvilinear grids
        ! ----------------------------------
        nnodes = nx*ny
        nxw=1
        nxe=nx
        nys=1
        nyn=ny
        !
        ! lat/lon
        allocate ( lon(nnodes,1), lat(nnodes,1) )
        i = 0
        do iy = nys, nyn
          do ix = nxw, nxe
            i = i+1
            lon(i,1)=xgrd(iy,ix)*factor
            lat(i,1)=ygrd(iy,ix)*factor
          end do
        end do
        !
        ! areas, corners
        allocate ( area(nnodes,1), corlon(nnodes,1,4), corlat(nnodes,1,4) )
        i = 0
        do iy = nys, nyn
          do ix = nxw, nxe
            i = i+1
            corlon(i,1,1)=lon(i,1)+hpfac(iy,ix)/2.*factor
            corlon(i,1,2)=lon(i,1)-hpfac(iy,ix)/2.*factor
            corlon(i,1,3)=lon(i,1)-hpfac(iy,ix)/2.*factor
            corlon(i,1,4)=lon(i,1)+hpfac(iy,ix)/2.*factor
            corlat(i,1,1)=lat(i,1)+hqfac(iy,ix)/2.*factor
            corlat(i,1,2)=lat(i,1)+hqfac(iy,ix)/2.*factor
            corlat(i,1,3)=lat(i,1)-hqfac(iy,ix)/2.*factor
            corlat(i,1,4)=lat(i,1)-hqfac(iy,ix)/2.*factor
            area(i,1)=hpfac(iy,ix)*hqfac(iy,ix)
          end do
        end do
        !
        ! model grid mask
        allocate ( mask(nnodes,1) )
        i = 0
        do iy = nys, nyn
          do ix = nxw, nxe
            i = i+1
            ! get the mask : 0 - sea  / 1 - open boundary cells (the land is already excluded)
            if ((mapsta(iy,ix) .eq. 1)) then
              mask(i,1) = 0
            else
              mask(i,1) = 1
            end if
          end do
        end do
        !
      else
        !
        ! 1.3. unstructured grids
        ! ----------------------------------
        write(*,*) 'to be implement for unstructured grids'
        stop
      end if
      !
      call oasis_write_grid('ww3t',nnodes,1,lon,lat)
      call oasis_write_corner('ww3t',nnodes,1,4,corlon,corlat)
      call oasis_write_area('ww3t',nnodes,1,area)
      call oasis_write_mask('ww3t',nnodes,1,mask)
      !
      ! 2. terminate grid writing
      ! -------------------------
      call oasis_terminate_grids_writing()
      !
      deallocate(lon)
      deallocate(lat)
      deallocate(corlon)
      deallocate(corlat)
      deallocate(area)
      deallocate(mask)
      !
    endif
    !
    call mpi_bcast(nnodes,1,mpi_integer,0,id_lcomm,ierr_mpi)
    !
    !/ ------------------------------------------------------------------- /
  end subroutine cpl_oasis_grid
  !/ ------------------------------------------------------------------- /
  subroutine cpl_oasis_define(ndso,rcv_str,snd_str)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           a. thevenin             |
    !/                  |           v. garnier              |
    !/                  |           m. accensi              |
    !/                  |                        fortran 90 |
    !/                  | last update :         08-jun-2018 |
    !/                  +-----------------------------------+
    !/
    !/      jul-2013 : origination.                         ( version 4.18 )
    !/    april-2016 : add coupling for unstructured grids  ( version 5.07 )
    !/                   (r. baraille & j. pianezze)
    !/    april-2016 : add comments (j. pianezze)           ( version 5.07 )
    !/    08-jun-2018 : use init_get_isea                   ( version 6.04 )
    !/
    !  1. purpose :
    !
    !     partition definition and coupling fields declaration
    !
    !  2. method :
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     ndso             int.      i      id. of the output file
    !     rcv_str          char.     i      name of receive fields
    !     snd_str          char.     i      name of send fields
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name                 type    module    description
    !     ----------------------------------------------------------------
    !      get_list_exch_field  subr.   w3oacpmd  list of the exchanged fields
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name                type     module    description
    !     ----------------------------------------------------------------
    !      ww3_shel            prog.    -         main program
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks :
    !  8. structure :
    !  9. switches :
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    use w3gdatmd, only: nseal,nsea, nx, ny, mapsta, mapsf, gtype, &
         & ungtype, rlgtype, clgtype, smctype
    use w3odatmd, only: naproc, iaproc
    use w3parall, only : init_get_isea
    !
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)                          :: ndso
    character(len=1024), intent(in)              :: rcv_str,snd_str
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ib_i,i
    integer                 :: il_part_id      ! partitionid
    integer, allocatable, dimension(:)   :: ila_paral       ! description of the local partition in the global index space
    integer, dimension(4)   :: ila_shape       ! vector giving the min & max index for each dim of the fields
    integer, dimension(2)   :: ila_var_nodims  ! rank of fields & number of bundles (1 with oasis3-mct)
    integer                 :: isea, jsea, ix, iy
    integer                 :: nhxw, nhxe, nhys, nhyn  ! size of the halo at the western, eastern, southern, northern boundaries
    logical                 :: ll_mpi_file     ! to check if there an mpi.txt file for domain decompasition
    !/
    !/ ------------------------------------------------------------------- /
    !/ executable part
    !/
    !
    if (gtype .eq. rlgtype .or. gtype .eq. clgtype) then
      !
      ! 1.1. regular and curvilinear grids
      ! ----------------------------------
      nhxw = 1 ; nhxe = nx ; nhys = 1 ; nhyn = ny
      nhxw = nhxw - 1
      nhxe = nx - nhxe
      nhys = nhys - 1
      nhyn = ny - nhyn
      !
      allocate(ila_paral(2+nseal*2))
      !
      ! * define the partition : oasis orange partition
      ila_paral(1) = 3
      !
      ! * total number of segments of the global domain
      ila_paral(2) = nseal
      !
      do jsea=1, nseal
        call init_get_isea(isea,jsea)
        ix = mapsf(isea,1)
        iy = mapsf(isea,2)
        ila_paral(jsea*2+1) = (iy - nhyn -1)*(nx - nhxe - nhxw) + (ix - nhxw - 1)
        ila_paral(jsea*2+2) = 1
      end do
      !
    else
      !
      ! 1.3. unstructured grids
      ! ----------------------------------
      write(*,*) 'to be verified for unstructured grids'
      stop
      !
      do jsea=1,nseal
        ila_paral(jsea*2+1) = (iaproc-1) + (jsea-1)*naproc
        ila_paral(jsea*2+2) = 1
      end do
      !
    endif
    !
    ! 2. partition definition
    ! ----------------------------------
    call oasis_def_partition(il_part_id, ila_paral,il_err,nnodes)
    if(il_err /= 0) then
      call oasis_abort(il_compid, 'cpl_oasis_define', 'problem during oasis_def_partition')
    endif
    !
    ! 3. coupling fields declaration
    ! ----------------------------------
    ila_shape(:) = (/1, nseal, 1, 1 /)
    !
    ila_var_nodims(1) = 2    ! rank of fields array
    ila_var_nodims(2) = 1    ! always 1 with oasis3-mct 2.0
    !
    call get_list_exch_field(ndso, rcv_fld, snd_fld, il_nb_rcv, il_nb_snd, rcv_str, snd_str)
    !
    ! 3.1 send coupling fields
    ! ----------------------------------
    do ib_i = 1, il_nb_snd
      call oasis_def_var (snd_fld(ib_i)%il_field_id     &
           &            , snd_fld(ib_i)%cl_field_name   &
           &            , il_part_id                    &
           &            , ila_var_nodims                &
           &            , oasis_out                     &
           &            , ila_shape                     &
           &            , oasis_real                    &
           &            , il_err )
      if (il_err /= 0) then
        call oasis_abort(il_compid, 'cpl_oasis_define', 'problem during oasis_def_var')
      endif
    enddo
    !
    ! 3.2 received coupling fields
    ! ----------------------------------
    do ib_i = 1, il_nb_rcv
      call oasis_def_var (rcv_fld(ib_i)%il_field_id    &
           &            , rcv_fld(ib_i)%cl_field_name  &
           &            , il_part_id                   &
           &            , ila_var_nodims               &
           &            , oasis_in                     &
           &            , ila_shape                    &
           &            , oasis_real                   &
           &            , il_err )
      !
      if (il_err /= 0) then
        call oasis_abort(il_compid, 'cpl_oasis_define', 'problem during oasis_def_var')
      endif
    enddo
    !
    ! 4. end of definition phase
    ! ----------------------------------
    call oasis_enddef(il_err)
    if (il_err /= 0) then
      call oasis_abort(il_compid, 'cpl_oasis_define', 'problem during oasis_enddef')
    endif
    !
    !/ ------------------------------------------------------------------- /
  end subroutine cpl_oasis_define
  !/ ------------------------------------------------------------------- /
  subroutine cpl_oasis_snd(id_nb, id_time, rda_field, ld_action)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           a. thevenin             |
    !/                  |                        fortran 90 |
    !/                  | last update :          april-2016 |
    !/                  +-----------------------------------+
    !/
    !/    jul-2013 : origination.                    ( version 4.18 )
    !/    april-2016 : add comments (j. pianezze)    ( version 5.07 )
    !/
    !  1. purpose :
    !
    !     in the model time step loop, each process sends its parts of the coupling field
    !
    !  2. method :
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     id_nb           int.      i       number of the field to be send
    !     id_time         int.      i       atmosphere time-step in seconds
    !     rda_field       real      i       coupling field array to be send
    !     ld_action       bool.     o       action performed
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !  5. called by :
    !
    !      name                  type    module     description
    !     ----------------------------------------------------------------
    !      snd_fields_to_atmos   subr.   w3agcmmd   send fields to atmos. model
    !      snd_fields_to_ocean   subr.   w3ogcmmd   send fields to ocean model
    !      snd_fields_to_ice     subr.   w3igcmmd   send fields to ice model
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks :
    !  8. structure :
    !  9. switches :
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)   :: id_nb                         ! number of the field to be send
    integer, intent(in)   :: id_time                       ! atmosphere time-step in seconds
    real(kind=8), dimension(:,:), intent(in) :: rda_field  ! coupling field array to be send
    logical, intent(out)  :: ld_action                     ! action performed
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer :: il_info                                     ! oasis3-mct info argument
    !/
    !/ ------------------------------------------------------------------- /
    !/ executable part
    !/
    call oasis_put ( snd_fld(id_nb)%il_field_id &
         &              , id_time                    &
         &              , rda_field                  &
         &              , il_info                    &
         &                )
    ld_action = il_info == oasis_sent     .or. il_info == oasis_torest .or.   &
         &           il_info == oasis_sentout  .or. il_info == oasis_torestout
    !/ ------------------------------------------------------------------- /
  end subroutine cpl_oasis_snd
  !/ ------------------------------------------------------------------- /
  subroutine cpl_oasis_rcv(id_nb, id_time, rda_field, ld_action)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           a. thevenin             |
    !/                  |                        fortran 90 |
    !/                  | last update :          april-2016 |
    !/                  +-----------------------------------+
    !/
    !/    jul-2013 : origination.                    ( version 4.18 )
    !/    april-2016 : add comments (j. pianezze)    ( version 5.07 )
    !/
    !  1. purpose :
    !
    !     in the model time step loop, each process receives its parts of the coupling field
    !
    !  2. method :
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     id_nb           int.      i       number of the field to be received
    !     id_time         int.      i       ocean time-step in seconds
    !     rda_field       real      i       coupling field array to be received
    !     ld_action       bool.     o       action performed
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !  5. called by :
    !
    !      name                  type    module     description
    !     ----------------------------------------------------------------
    !      rcv_fields_from_atmos   subr.   w3agcmmd   receive fields from atmos. model
    !      rcv_fields_from_ocean   subr.   w3ogcmmd   receive fields from ocean model
    !      rcv_fields_from_ice     subr.   w3igcmmd   receive fields from ice model
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks :
    !  8. structure :
    !  9. switches :
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)   :: id_nb                          ! number of the field to be received
    integer, intent(in)   :: id_time                        ! ocean time-step in seconds
    real(kind=8), dimension(:,:), intent(out) :: rda_field    ! coupling field array to be received
    logical, intent(out)  :: ld_action                      ! action performed
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer :: il_info                                      ! oasis3-mct info argument
    !/
    !/ ------------------------------------------------------------------- /
    !/ executable part
    !/
    call oasis_get ( rcv_fld(id_nb)%il_field_id &
         &              , id_time                    &
         &              , rda_field                  &
         &              , il_info                    &
         &                )
    !
    ld_action = il_info == oasis_recvd   .or. il_info == oasis_fromrest .or.   &
         &           il_info == oasis_recvout .or. il_info == oasis_fromrestout
    !
    !/ ------------------------------------------------------------------- /
  end subroutine cpl_oasis_rcv
  !/ ------------------------------------------------------------------- /
  subroutine cpl_oasis_finalize
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           a. thevenin             |
    !/                  |                        fortran 90 |
    !/                  | last update :          april-2016 |
    !/                  +-----------------------------------+
    !/
    !/    jul-2013 : origination.                    ( version 4.18 )
    !/  april-2016 : add comments (j. pianezze)      ( version 5.07 )
    !/
    !  1. purpose :
    !
    !     terminate the coupling
    !
    !  2. method :
    !  3. parameters :
    !  4. subroutines used :
    !  5. called by :
    !
    !      name                type     module    description
    !     ----------------------------------------------------------------
    !      ww3_shel            prog.    -         main program
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks :
    !  8. structure :
    !  9. switches :
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/ executable part
    !/
    call oasis_terminate(il_err)
    !
    if (il_err /= 0) then
      call oasis_abort(il_compid, 'cpl_oasis_finalize', 'problem during oasis_terminate')
    endif
    !
    !/ ------------------------------------------------------------------- /
  end subroutine cpl_oasis_finalize
  !/ ------------------------------------------------------------------- /
  subroutine get_list_exch_field(ndso, rcv, snd, id_nb_rcv, id_nb_snd, rcv_str, snd_str)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           a. thevenin             |
    !/                  |           v. garnier              |
    !/                  |           a.c. bennis             |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    jul-2013 : origination.                               ( version 4.18 )
    !/    mar-2014 : j. pianezze (lpo) : add atmospheric fields ( version 5.07 )
    !/    apr-2015 : m. accensi  (lpo) : add fields selection   ( version 5.07 )
    !/    apr-2016 : add comments (j. pianezze)                 ( version 5.07 )
    !/ 22-mar-2021 : adds extra coupling fields                 ( version 7.13 )
    !/
    !  1. purpose :
    !
    !     provides the list of coupling fields
    !
    !  2. method :
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     ndso         int.      i       id. of the output file
    !     rcv          type     i/o      received variables
    !     snd          type     i/o      send variables
    !     id_nb_rcv    int.     i/o      number of received variables
    !     id_nb_snd    int.     i/o      number of send variables
    !     rcv_str      char.     i       name of the received variables
    !     snd_str      char      i       name of the send variables
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name                type    module     description
    !     ----------------------------------------------------------------
    !      strsplit            subr.   w3servmd   splits string into words
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name                type    module     description
    !     ----------------------------------------------------------------
    !      cpl_oasis_define    subr.   w3oacpmd   partition definition
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks :
    !  8. structure :
    !  9. switches :
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    use w3servmd, only: strsplit
    !
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    type(cpl_field), dimension(ip_maxfld), intent (inout)   :: rcv, snd
    integer, intent(inout)                                  :: id_nb_rcv, id_nb_snd
    integer, intent(in)                                     :: ndso
    character(len=1024), intent(in)                         :: rcv_str, snd_str
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    character(len=100)                                      :: out_names(50), teststr
    integer                                                 :: iout
    !/
    !/ ------------------------------------------------------------------- /
    !/ executable part
    !/
    !
    ! 1. coupling fields received by ww3
    ! ----------------------------------
    id_nb_rcv = 0
    !
    out_names(:)=''
    call strsplit(rcv_str,out_names)
    iout=0
    do while (len_trim(out_names(iout+1)).ne.0)
      teststr=out_names(iout+1)
      select case(trim(teststr(1:6)))
        !
        !
        ! ocean model variables
        !
        !
        !
        ! atmosphere model variables
        !
        !
        !
        ! ice model variables
        !
        !
      case default
        write (ndso,1001) trim(teststr(1:6))
      end select
      iout=iout+1
    end do
    !
    ! 2. coupling fields sent by ww3
    ! ----------------------------------
    id_nb_snd = 0
    !
    out_names(:)=''
    call strsplit(snd_str,out_names)
    iout=0
    do while (len_trim(out_names(iout+1)).ne.0)
      teststr=out_names(iout+1)
      select case(trim(teststr(1:6)))
        !
        !
        ! ocean model variables
        !
        !
        ! atmosphere model variables
        !
        !
        !
        ! ice model variables
        !
        !
      case default
        write (ndso,1002) trim(teststr(1:6))
      end select
      iout=iout+1
    end do
    !
    ! formats
    !
1001 format (/' *** wavewatch iii warning in w3oacpmd : '/                       &
         '     requested coupling received field ',a,' was not recognized.'/)
    !
1002 format (/' *** wavewatch iii warning in w3oacpmd : '/                       &
         '     requested coupling sent field ',a,' was not recognized.'/)
    !/
    !/ ------------------------------------------------------------------- /
  end subroutine get_list_exch_field
  !/ ------------------------------------------------------------------- /
  !/
end module w3oacpmd
!/
!/ ------------------------------------------------------------------- /
