!> @file wav_pio
!!
!> @brief Manage WAV PIO 
!!
!> @author Denise.Worthen@noaa.gov
!> @date 08-02-2024

module wav_pio

  use w3odatmd          , only : naproc, iaproc
  use w3adatmd          , only : mpi_comm_wave
  use w3gdatmd          , only : nk, nx, ny, mapsf, mapsta, nsea
  use w3parall          , only : init_get_isea
  use wav_import_export , only : nseal_cpl
  
  ! use w3gdatmd          , only : xgrd, ygrd
  !
  ! use w3odatmd          , only : noswll, undef
  !
  use pio
  use netcdf

  implicit none

  !private

  interface wav_initdecomp
     module procedure wav_initdecomp_2d
     module procedure wav_initdecomp_3d
  end interface wav_initdecomp

  !public :: wav_pio_init
  !public :: wav_initdecomp

  type(iosystem_desc_t) :: wav_pio_subsystem
  type(file_desc_t)     :: pioid
  !type(var_desc_t)      :: vardesc

  !===============================================================================
contains
  !===============================================================================

  subroutine wav_pio_init

    ! pio
    integer :: pio_iotype
    integer :: nprocs
    integer :: istride
    integer :: basetask
    integer :: numiotasks
    integer :: rearranger
    integer :: my_task
    integer :: master_task
    integer :: status
    !-------------------------------------------------------------------------------

    ! TODO: for now, hardwire  the io system
    pioid%fh = -1
    pio_iotype = PIO_IOTYPE_PNETCDF
    nprocs = naproc
    my_task = iaproc - 1
    master_task = 0
    istride = 4
    basetask = 1
    numiotasks = max((nprocs-basetask)/istride,1)
    !numiotasks = 2
    rearranger = PIO_REARR_BOX
    print '(a,4i8)','SETUP ',nprocs,iaproc,my_task,numiotasks, nseal_cpl

    !allocate(io_subsystem)
    !if (localPet == 0) write(logunit,*) trim(subname),' calling pio init'
    !call pio_init(localPet, comm, pio_numiotasks, 0, pio_stride, pio_rearranger, io_subsystem, base=pio_root)

    allocate(wav_pio_subsystem)
    call pio_init(my_task, MPI_COMM_WAVE, numiotasks, master_task, istride, rearranger, &
         wav_pio_subsystem, base=basetask)
    call pio_seterrorhandling(wav_pio_subsystem, PIO_RETURN_ERROR)

  end subroutine wav_pio_init

  !===============================================================================
  subroutine wav_initdecomp_2d(iodesc)

    type(io_desc_t), intent(out) :: iodesc

    integer          :: n
    integer, pointer :: dof2d(:)

    allocate(dof2d(nseal_cpl))
    dof2d = 0
    n = 0
    do jsea = 1,nseal_cpl
      call init_get_isea(isea, jsea)
      ix = mapsf(isea,1)                 ! global ix
      iy = mapsf(isea,2)                 ! global iy
      n = n+1
      dof2d(n) = (iy-1)*nx + ix          ! local index : global index
    end do
    call pio_initdecomp(wav_pio_subsystem, PIO_REAL, (/nx,ny/), dof2d, iodesc)

    deallocate(dof2d)
  end subroutine wav_initdecomp_2d

  !===============================================================================
  subroutine wav_initdecomp_3d(nz, iodesc)

    integer        , intent(in)  :: nz
    type(io_desc_t), intent(out) :: iodesc

    integer          :: k,n
    integer, pointer :: dof3d(:)

    allocate(dof3d(nz*nseal_cpl))
    dof3d = 0
    n = 0
    do k = 1,nz
      do jsea = 1,nseal_cpl
        call init_get_isea(isea, jsea)
        ix = mapsf(isea,1)     ! global ix
        iy = mapsf(isea,2)     ! global iy
        n = n+1
        dof3d(n) = ((iy-1)*nx + ix) + (k-1)*nx*ny ! local index : global index
      end do
    end do
    call pio_initdecomp(wav_pio_subsystem, PIO_REAL, (/nx,ny,nz/), dof3d, iodesc)

    deallocate(dof3d)
  end subroutine wav_initdecomp_3d

  !===============================================================================
  subroutine handle_err(ierr,string)
    use w3odatmd  , only : ndse
    use w3servmd  , only : extcde

    ! input/output variables
    integer         , intent(in) :: ierr
    character(len=*), intent(in) :: string

    integer :: strerror_status
    character(len=pio_max_name) :: err_msg

    if (ierr /= PIO_NOERR) then
      strerror_status = pio_strerror(ierr, err_msg)
      write(ndse,*) "*** WAVEWATCH III netcdf error: ",trim(string),':',trim(err_msg)
      call extcde ( 49 )
    end if
  end subroutine handle_err
end module wav_pio
