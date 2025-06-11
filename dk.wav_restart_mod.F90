!> @file wav_restart_mod
!!
!> @brief Handle WW3 restart files as netCDF using PIO
!!
!> @author Denise.Worthen@noaa.gov
!> @date 08-26-2024
module wav_restart_mod

  use w3parall      , only : init_get_isea
  use w3adatmd      , only : nsealm
  use w3gdatmd      , only : nth, nk, nx, ny, mapsf, nspec, nseal, nsea
  use w3odatmd      , only : ndso, iaproc, addrstflds, rstfldlist, rstfldcnt
  use w3wdatmd      , only : ice
  use wav_pio_mod   , only : pio_iotype, pio_ioformat, wav_pio_subsystem
  use wav_pio_mod   , only : handle_err, wav_pio_initdecomp
#ifdef W3_PDLIB
    use yowNodepool , only : ng
#endif
  use pio
  use netcdf

  implicit none

  private

  type(file_desc_t) :: pioid
  type(var_desc_t)  :: varid
  type(io_desc_t)   :: iodesc2dint
  type(io_desc_t)   :: iodesc2d
  type(io_desc_t)   :: iodesc3dk

  integer(kind=PIO_OFFSET_KIND) :: frame

  public :: write_restart
  public :: read_restart

  ! used/reused in module
  character(len=4)  :: cspec
  character(len=12) :: vname
  integer           :: ik, ith, ix, iy, kk, isea, jsea, ierr, i

  !===============================================================================
contains
  !===============================================================================
  !> Write a WW3 restart file
  !!
  !! @details Called by w3wavemd to write a restart file at a given frequency or
  !! time
  !!
  !! @param[in]     fname    the time-stamped file name
  !! @param[in]     va       the va array
  !! @param[in]     mapsta   the mapsta + 8*mapst2 array
  !!
  !> author DeniseWorthen@noaa.gov
  !> @date 08-26-2024
  subroutine write_restart (fname, va, mapsta)

    use w3odatmd , only : time_origin, calendar_name, elapsed_secs
    use mpi, only : MPI_Wtime, MPI_INFO_NULL, mpi_integer, MPI_SUCCESS, MPI_MAX_PROCESSOR_NAME, &
                    MPI_MAX_ERROR_STRING, mpi_float, MPI_REAL8, MPI_MAX, MPI_Info, MPI_Info_set
    use w3adatmd, only : mpi_comm_wave
    use netcdf

    real            , intent(in) :: va(1:nspec,0:nsealm)
    integer         , intent(in) :: mapsta(ny,nx)
    character(len=*), intent(in) :: fname

    ! local variables
    integer              :: timid, xtid, ytid, ztid, old_mode
    integer              :: nseal_cpl, nmode
    integer              :: dimid3(3)
    integer              :: dimid4(4)
    real   , allocatable :: lva(:,:)
    integer, allocatable :: lmap(:)
    real(kind=8) :: tb1,tb2,tb3,tb4
    real(kind=8) :: te1,te2,te3,te4,walltime(4)
    logical :: multifield=.true.

    character(len=MPI_MAX_PROCESSOR_NAME) :: nodeName
    integer(kind=4) name_len,messageLen,nodeID,nodeComm,nodeRank,RanksPerNode,color
    integer(kind=4) IOComm,IORank,IOCommSize
    character(len=MPI_MAX_ERROR_STRING) :: errMessage
    integer :: i,j,ierr,rank,npes,ncid,mode,varid
    integer(kind=4), allocatable :: counts(:), displs(:)
    integer(kind=4), allocatable :: NodeCounts(:), NodeDispls(:)
    integer(kind=4), allocatable :: IOCounts(:), IODispls(:)
    real, dimension(:,:), allocatable :: NodeVA
    integer, dimension(:), allocatable :: Nodelmap
    integer :: start(3), kount(3), dimIDs(3)
    integer(kind=4) :: info

    !-------------------------------------------------------------------------------

#ifdef W3_PDLIB
    nseal_cpl = nseal - ng
#else
    nseal_cpl = nseal
#endif
    allocate(lmap(1:nseal_cpl))
    lmap(:) = 0

    ! Significant parts of this prep belong in an initialization routine that is called once a job startup.
    ! I have chosen to time only the part that have to be done for every file.

    ! Determine size of all subdomains
    call MPI_Comm_rank(mpi_comm_wave, rank, ierr)
    call MPI_Comm_size(mpi_comm_wave, npes, ierr)
    allocate(counts(npes))
    call MPI_AllGather(nseal_cpl, 1, mpi_integer, counts, 1, mpi_integer, mpi_comm_wave, ierr)
    if(sum(counts) /= nx) write(6,'("Sum of all counts /= nx " 2I8)') nx, sum(counts)

    ! Create a sub-communicator to handle on-node communication.
    ! This is used to gather contributions from other ranks on the same node to a single aggregator
    call MPI_Get_processor_name(nodeName,name_len,ierr)
    !nodeID=digest(trim(nodeName))
    read(nodeName(4:9),*) nodeID ! Danger! This approach will not to work on platforms other than WCOSS2
    call MPI_Comm_split(mpi_comm_wave, nodeID, rank, nodeComm, ierr)
    call MPI_Comm_rank(nodeComm,nodeRank,ierr)
    call MPI_Comm_size(nodeComm,RanksPerNode,ierr)
    !if(nodeRank==0) write(6,'("RanksPerNode ",I4)') RanksPerNode

    ! All ranks on a given node must know the size of the contributions from all other ranks on the same node
    allocate(NodeCounts(RanksPerNode))
    call MPI_AllGather(nseal_cpl, 1, mpi_integer, NodeCounts, 1, mpi_integer, nodeComm, ierr)
    if(ierr /= MPI_SUCCESS) then
      call MPI_Error_string(ierr,errMessage,messageLen,ierr)
      write(6,'("Error from NodeCounts MPI_AllGather: "A)') errMessage
    endif
    !if(nodeRank==0) write(6,'("Total Elements on node ",2I6)') nodeID, sum(NodeCounts)

    ! Compute displacements used in the Gatherv
    allocate(NodeDispls(RanksPerNode))
    NodeDispls=0
    do i=1,RanksPerNode
      NodeDispls(i) = sum(NodeCounts(1:i-1))
    enddo

    ! Check that the count is what we expect
    if(nodeRank==0 .and. NodeDispls(RanksPerNode)+NodeCounts(RanksPerNode) /= sum(NodeCounts)) &
      write(6,'("Last Count+Displ /= sum(NodeCounts) " 2I10)') NodeDispls(RanksPerNode)+NodeCounts(RanksPerNode),sum(NodeCounts)

    !write(6,'("Node Displs ",4I12)') NodeRank, NodeDispls(nodeRank+1), NodeCounts(nodeRank+1)

    ! Allocate an array on one process to collect the node-local contributions.
    if(nodeRank==0) allocate(Nodelmap(1:sum(NodeCounts)))

    ! Collect the node-local contributions to rank zero of the node-local communicator.
#ifdef ORIGINAL_ORDER
    if(nodeRank==0) allocate(NodeVA(1:sum(NodeCounts),1:nspec))
    allocate(lva(1:nseal_cpl,1:nspec))
    tb3=MPI_Wtime()
    do jsea = 1,nseal_cpl
      kk = 0
      do ik = 1,nk
        do ith = 1,nth
          kk = kk + 1
          lva(jsea,kk) = va(kk,jsea)
        end do
      end do
    end do
    call MPI_Gatherv( lva, size(lva), mpi_float, NodeVA, nspec*NodeCounts, nspec*NodeDispls, mpi_float, 0, nodeComm, ierr)
    te3=MPI_Wtime()
#else
    if(nodeRank==0) allocate(NodeVA(1:nspec,1:sum(NodeCounts)))
    allocate(lva(1:nspec,1:nseal_cpl))
    tb3=MPI_Wtime()
    call MPI_Gatherv( va(:,1:nseal_cpl), size(va(:,1:nseal_cpl)), mpi_float, NodeVA, nspec*NodeCounts, nspec*NodeDispls, mpi_float, 0, nodeComm, ierr)
    te3=MPI_Wtime()
#endif
    if(ierr /= MPI_SUCCESS) then
      call MPI_Error_string(ierr,errMessage,messageLen,ierr)
      write(6,'("Error from NodeVA MPI_Gatherv: "A)') errMessage
    endif

    ! Collect the node-local contributions to rank zero of the node-local communicator.
    tb4=MPI_Wtime()
    ! mapsta is global
    do jsea = 1,nseal_cpl
      call init_get_isea(isea, jsea)
      ix = mapsf(isea,1)
      iy = mapsf(isea,2)
      lmap(jsea) = mapsta(iy,ix)
    end do
    call MPI_Gatherv(lmap, size(lmap), mpi_integer, Nodelmap, NodeCounts, NodeDispls, mpi_integer, 0, nodeComm, ierr)
    te4=MPI_Wtime()
    if(ierr /= MPI_SUCCESS) then
      call MPI_Error_string(ierr,errMessage,messageLen,ierr)
      write(6,'("Error from Nodelmap MPI_Gatherv: "A)') errMessage
    endif
    !if(nodeRank==0) write(6,'("After MPI_Gatherv " 2f15.4)')  lmap(1), Nodelmap(1)

    ! Create another sub-communicator from all the aggregators/writers.  We'll need to do
    ! another collection of contributions so we can compute displacements into the file.
    color=0
    if(nodeRank==0) color=1
    call MPI_Comm_split(mpi_comm_wave, color, rank, IOComm, ierr)
    !write(6,'("After MPI_Comm_split IOComm")')

    ! Only aggregators/writers enter here.  Others wait at the Barrier below
    if(nodeRank==0) then

      call MPI_Comm_rank(IOComm,IORank,ierr)
      call MPI_Comm_size(IOComm,IOCommSize,ierr)
      !write(6,'("IOCommSize ",I6)') IOCommSize

      ! All aggregators/writers must know the size of the contributions from all other aggregators/writers
      allocate(IOCounts(IOCommSize))
      call MPI_AllGather(sum(NodeCounts), 1, mpi_integer, IOCounts, 1, mpi_integer, IOComm, ierr)
      !write(6,'("After MPI_AllGather on IOComm")')

      allocate(IODispls(IOCommSize))
      IODispls=0
      do i=1,IOCommSize
        IODispls(i) = sum(IOCounts(1:i-1)) + 1
      enddo
      !write(6,'("IO Displs ",3I12)') IORank, IODispls(IORank+1), IOCounts(IORank+1)

      ! Set offsets and counts used in the NetCDF put_var call
#ifdef ORIGINAL_ORDER
      start = (/ IODispls(IORank+1),    1,  1 /)
      kount = (/ IOCounts(IORank+1),nspec,  1 /)
#else
      start = (/     1, IODispls(IORank+1), 1 /)
      kount = (/ nspec, IOCounts(IORank+1), 1 /)
#endif
      !print*,'Start',IORank,start
      !print*,'Count ',IORank,kount

      call MPI_Info_create(info, ierr)
      if(ierr /= MPI_SUCCESS) then
        call MPI_Error_string(ierr,errMessage,messageLen,ierr)
        write(6,'("Error from MPI_Info_create: "A)') errMessage
      endif
      call MPI_Info_set(info, "romio_cb_write", "disable", ierr)
      if(ierr /= MPI_SUCCESS) then
        call MPI_Error_string(ierr,errMessage,messageLen,ierr)
        write(6,'("Error from MPI_Info_set: "A)') errMessage
      endif

      ! Create new netCDF file
      tb1=MPI_Wtime()
      mode = IOR(nf90_clobber, nf90_netcdf4)
      mode = IOR(mode, nf90_mpiio)
      !call check( nf90_create(trim(fname), mode, ncid, comm=IOComm, info=MPI_INFO_NULL) )
      call check( nf90_create(trim(fname), mode, ncid, comm=IOComm, info=info) )

      ! Needed to prevent the library from filling the vars with default fill values during nf90_enddef()
      call check(nf90_set_fill(ncid, NF90_NOFILL, old_mode))

#ifdef ORIGINAL_ORDER
      call check(nf90_def_dim(ncid, 'nx'   ,    nx, dimIDs(1)))
      call check(nf90_def_dim(ncid, 'nspec', nspec, dimIDs(2)))
#else
      call check(nf90_def_dim(ncid, 'nspec', nspec, dimIDs(1)))
      call check(nf90_def_dim(ncid, 'nx'   ,    nx, dimIDs(2)))
#endif
      call check(nf90_def_dim(ncid, 'time',     1 , dimIDs(3)))

      ! define the time variable
      call check(nf90_def_var(ncid, 'time', NF90_DOUBLE, (/ dimIDs(3) /), varid))
      call check(nf90_put_att(ncid, varid, 'units', trim(time_origin)))
      call check(nf90_put_att(ncid, varid, 'calendar', trim(calendar_name)))

      ! define the nth,nk sizes
      call check(nf90_def_var(ncid, 'nth', NF90_INT, varid))
      call check(nf90_put_att(ncid, varid, 'long_name', 'number of direction bins'))
      call check(nf90_def_var(ncid, 'nk', NF90_INT, varid))
      call check(nf90_put_att(ncid, varid, 'long_name', 'number of frequencies'))

      vname = 'mapsta'
#ifdef ORIGINAL_ORDER
      call check(nf90_def_var(ncid, trim(vname), NF90_INT, (/dimIDs(1), dimIDs(3)/), varid, contiguous=.TRUE.))
#else
      call check(nf90_def_var(ncid, trim(vname), NF90_INT, (/dimIDs(2), dimIDs(3)/), varid, contiguous=.TRUE.))
#endif
      call check(nf90_var_par_access(ncid, varid, nf90_collective) )
      !call check(nf90_put_att(ncid, varid, '_FillValue', nf90_fill_int))

      ! define any requested additional fields
      if (addrstflds) then
        do i = 1,rstfldcnt
          vname = trim(rstfldlist(i))
          call check(nf90_def_var(ncid, trim(vname), NF90_REAL, dimIDs, varid))
          !call check(nf90_put_att(ncid, varid, '_FillValue', nf90_fill_float))
          call check(nf90_var_par_access(ncid, varid, nf90_collective) )
        end do
      end if

      vname = 'va'
      call check(nf90_def_var(ncid, trim(vname), NF90_FLOAT, dimIDs, varid, contiguous=.TRUE.))
      call check(nf90_var_par_access(ncid, varid, nf90_collective) )
      ! end variable definitions

      ! Exit define mode for new file.  File is still open for writing
      call check(nf90_enddef(ncid))
      te1=MPI_Wtime()

      call MPI_Barrier(IOComm, ierr)
      !write(6,'("After IOComm MPI_Barrier")')

      ! Write
      tb2=MPI_Wtime()
      vname = 'mapsta'
      call check(nf90_inq_varid(ncid,  trim(vname), varid))
      call check(nf90_put_var(ncid, varid, Nodelmap, start=(/ IODispls(IORank+1), 1 /), count=(/ IOCounts(IORank+1), 1 /)) )
      !write(6,'("After write mapsta")')

      ! Write large variable
      vname = 'va'
      call check(nf90_inq_varid(ncid, trim(vname), varid) )
      call check(nf90_put_var(ncid, varid, NodeVA, start=start, count=kount) )
      !write(6,'("After write va")')

      ! Close the file. This frees up any internal netCDF resources associated with the file.
      call check( nf90_close(ncid) )
      te2=MPI_Wtime()

      ! Collect timings from all aggregators/writers
      call MPI_Reduce((/ te1-tb1, te2-tb2, te3-tb3, te4-tb4 /), walltime, 4, MPI_REAL8, MPI_MAX, 0, IOComm, ierr)
      if(ierr /= MPI_SUCCESS) print*,'MPI_Reduce ',ierr

      if(IORank == 0) write(6,'("Maximum Walltime for def, write and total " 5f15.4)') walltime, sum(walltime)

      deallocate(IOCounts,IODispls)
      call MPI_Info_free(info, ierr)

    endif

    ! Cleanup
    deallocate(NodeCounts,NodeDispls)
    if(nodeRank==0) deallocate(NodeVA,Nodelmap)

    call MPI_Barrier(mpi_comm_wave, ierr)
    deallocate(lva)
    deallocate(lmap)
    call MPI_Comm_free(nodeComm,ierr)
    call MPI_Comm_free(IOComm,ierr)

    contains
      subroutine check(status)
        integer, intent ( in) :: status

        if(status /= nf90_noerr) then
          print *, status, trim(nf90_strerror(status))
          stop 2
        end if
      end subroutine check

  end subroutine write_restart



  !===============================================================================
  !> Read a WW3 restart file
  !!
  !> @details Called by w3init to read a restart file which is known to exist or to
  !! initialize a set of variables when the filename is "none".
  !!
  !! @param[in]     fname     the time-stamped file name
  !! @param[out]    va        the va array, optional
  !! @param[out]    mapsta    the mapsta array, optional
  !! @param[inout]  mapst2    the mapst2 array, optional
  !!
  !> author DeniseWorthen@noaa.gov
  !> @date 08-26-2024
  subroutine read_restart (fname, va, mapsta, mapst2)

    use mpi_f08
    use w3adatmd    , only : mpi_comm_wave
    use w3gdatmd    , only : sig
    use w3idatmd    , only : icei
    use w3wdatmd    , only : time, tlev, tice, trho, tic1, tic5, wlv, asf, fpis

    character(len=*)  , intent(in)    :: fname
    real   , optional , intent(out)   :: va(1:nspec,0:nsealm)
    integer, optional , intent(out)   :: mapsta(ny,nx)
    integer, optional , intent(inout) :: mapst2(ny,nx)

    ! local variables
    type(MPI_Comm)       :: wave_communicator  ! needed for mpi_f08
    integer, allocatable :: global_input(:), global_output(:)
    integer              :: nseal_cpl
    integer              :: ifill
    real                 :: rfill
    real   , allocatable :: lva(:,:)
    integer, allocatable :: lmap(:)
    integer, allocatable :: lmap2d(:,:)
    integer, allocatable :: st2init(:,:)
    !-------------------------------------------------------------------------------

    ! cold start, set initial values and return.
    if (trim(fname)  == 'none') then
      tlev(1) = -1
      tlev(2) =  0
      tice(1) = -1
      tice(2) =  0
      trho(1) = -1
      trho(2) =  0
      tic1(1) = -1
      tic1(2) =  0
      tic5(1) = -1
      tic5(2) =  0
      wlv     =  0.
      ice     =  0.
      asf     =  1.
      fpis    =  sig(nk)
      if (iaproc == 1) write(ndso,'(a)')' Initializing WW3 at rest '
      return
    end if

    ! read a netcdf restart
    wave_communicator%mpi_val = MPI_COMM_WAVE
#ifdef W3_PDLIB
    nseal_cpl = nseal - ng
#else
    nseal_cpl = nseal
#endif
    allocate(lva(1:nseal_cpl,1:nspec))
    allocate(lmap(1:nseal_cpl))
    allocate(lmap2d(1:ny,1:nx))
    allocate(st2init(1:ny,1:nx))
    lva(:,:) = 0.0
    lmap(:) = 0
    lmap2d(:,:) = 0

    ! save a copy of initial mapst2 from mod_def
    st2init = mapst2

    ! all times are restart times
    tlev = time
    tice = time
    trho = time
    tic1 = time
    tic5 = time
    frame = 1
    ierr = pio_openfile(wav_pio_subsystem, pioid, pio_iotype, trim(fname), pio_nowrite)
    call handle_err(ierr, 'open file '//trim(fname))
    if (iaproc == 1) write(ndso,'(a)')' Reading restart file '//trim(fname)

    ! check the field dimensions and sizes against the current values
    call checkfile()

    ! initialize the decomp
    call wav_pio_initdecomp(iodesc2dint, use_int=.true.)
    call wav_pio_initdecomp(iodesc2d)

    do kk = 1,nspec
      write(cspec,'(i4.4)')kk
      vname = 'va'//cspec
      ierr = pio_inq_varid(pioid, trim(vname), varid)
      call handle_err(ierr, 'inquire variable '//trim(vname))
      call pio_setframe(pioid, varid, frame)
      ierr = pio_get_att(pioid, varid, "_FillValue", rfill)
      call handle_err(ierr, 'get variable _FillValue'//trim(vname))
      call pio_read_darray(pioid, varid, iodesc2d, lva(:,kk), ierr)
      call handle_err(ierr, 'get variable '//trim(vname))
    end do

    va = 0.0
    do jsea = 1,nseal_cpl
      kk = 0
      do ik = 1,nk
        do ith = 1,nth
          kk = kk + 1
          if (lva(jsea,kk) .ne. rfill) then
            va(kk,jsea) = lva(jsea,kk)
          end if
        end do
      end do
    end do

    vname = 'mapsta'
    ierr = pio_inq_varid(pioid, trim(vname), varid)
    call handle_err(ierr, 'inquire variable '//trim(vname))
    call pio_setframe(pioid, varid, frame)
    call pio_read_darray(pioid, varid, iodesc2dint, lmap, ierr)
    call handle_err(ierr, 'get variable '//trim(vname))
    ierr = pio_get_att(pioid, varid, "_FillValue", ifill)
    call handle_err(ierr, 'get variable _FillValue'//trim(vname))

    ! fill global array with PE local values
    allocate(global_input(nsea))
    allocate(global_output(nsea))
    global_input = 0
    global_output = 0
    do jsea = 1,nseal_cpl
      call init_get_isea(isea, jsea)
      if (lmap(jsea) .ne. ifill) then
        global_input(isea) = lmap(jsea)
      end if
    end do
    ! reduce across all PEs to create global array
    call MPI_AllReduce(global_input, global_output, nsea, MPI_INTEGER, MPI_SUM, wave_communicator, ierr)

    ! fill global array on each PE
    do isea = 1,nsea
      ix = mapsf(isea,1)
      iy = mapsf(isea,2)
      lmap2d(iy,ix) = global_output(isea)
    end do
    deallocate(global_input)
    deallocate(global_output)

    mapsta = mod(lmap2d+2,8) - 2
    mapst2 = st2init + (lmap2d-mapsta)/8

    ! read additional global(nsea) restart fields
    if (addrstflds) then
      do i = 1,rstfldcnt
        vname = trim(rstfldlist(i))
        if (vname == 'ice')call read_globalfield(wave_communicator, vname, nseal_cpl, ice(1:nsea), icei)
      end do
    end if

    call pio_syncfile(pioid)
    call pio_freedecomp(pioid, iodesc2d)
    call pio_freedecomp(pioid, iodesc2dint)
    call pio_closefile(pioid)

  end subroutine read_restart

  !===============================================================================
  !>  Write a decomposed array of (nsea) global values
  !!
  !! @param[in]   vname         the variable name
  !! @param[in]   nseal_cpl     the PE local dimension, disregarding halos
  !! @param[in]   global_input  the global array
  !!
  !> author DeniseWorthen@noaa.gov
  !> @date 09-22-2024
  subroutine write_globalfield(vname, nseal_cpl, global_input)

    character(len=*) , intent(in)    :: vname
    integer          , intent(in)    :: nseal_cpl
    real             , intent(in)    :: global_input(:)

    ! local variable
    real, allocatable :: lvar(:)

    allocate(lvar(1:nseal_cpl))

    lvar(:) = 0.0
    do jsea = 1,nseal_cpl
      call init_get_isea(isea, jsea)
      lvar(jsea) = global_input(isea)
    end do

    !write PE local field
    ierr = pio_inq_varid(pioid,  trim(vname), varid)
    call handle_err(ierr, 'inquire variable '//trim(vname))
    call pio_setframe(pioid, varid, int(1,kind=PIO_OFFSET_KIND))
    call pio_write_darray(pioid, varid, iodesc2d, lvar, ierr)
    call handle_err(ierr, 'put variable '//trim(vname))

  end subroutine write_globalfield

  !===============================================================================
  !>  Read a decomposed array of (nsea) global values and return a global field on
  !! each DE
  !!
  !! @param[in]    wave_communicator  the MPI handle
  !! @param[in]    vname              the variable name
  !! @param[in]    nseal_cpl          the PE local dimension, disregarding halos
  !! @param[out]   global_output      the global array, nsea points on each DE
  !! @param[out]   global_2d          the global array, (nx,ny) points on each DE
  !!
  !> author DeniseWorthen@noaa.gov
  !> @date 09-22-2024
  subroutine read_globalfield(wave_communicator, vname, nseal_cpl, global_output, global_2d)

    use mpi_f08

    type(MPI_Comm)   , intent(in)    :: wave_communicator  ! needed for mpi_f08
    character(len=*) , intent(in)    :: vname
    integer          , intent(in)    :: nseal_cpl
    real             , intent(out)   :: global_output(:)
    real             , intent(out)   :: global_2d(:,:)

    ! local variables
    real, allocatable :: global_input(:)
    real              :: rfill
    real, allocatable :: lvar(:)

    allocate(lvar(1:nseal_cpl))
    lvar(:) = 0.0

    ierr = pio_inq_varid(pioid, trim(vname), varid)
    call handle_err(ierr, 'inquire variable '//trim(vname))
    call pio_setframe(pioid, varid, frame)
    call pio_read_darray(pioid, varid, iodesc2d, lvar, ierr)
    call handle_err(ierr, 'get variable '//trim(vname))
    ierr = pio_get_att(pioid, varid, "_FillValue", rfill)
    call handle_err(ierr, 'get variable _FillValue'//trim(vname))

    ! fill global array with PE local values
    allocate(global_input(nsea))
    global_input = 0.0
    global_output = 0.0
    do jsea = 1,nseal_cpl
      call init_get_isea(isea, jsea)
      if (lvar(jsea) .ne. rfill) then
        global_input(isea) = lvar(jsea)
      end if
    end do
    ! reduce across all PEs to create global array
    call MPI_AllReduce(global_input, global_output, nsea, MPI_REAL, MPI_SUM, wave_communicator, ierr)
    deallocate(global_input)

    global_2d = 0.0
    do isea = 1,nsea
      ix = mapsf(isea,1)
      iy = mapsf(isea,2)
      global_2d(ix,iy) = global_output(isea)
    end do

  end subroutine read_globalfield

  !===============================================================================
  !>  Check that a restart file has the expected dimensions and sizes
  !!
  !> author DeniseWorthen@noaa.gov
  !> @date 10-15-2024
  subroutine checkfile()

    use w3odatmd  , only : ndse
    use w3servmd  , only : extcde

    integer :: dimid, ivar
    integer(kind=PIO_OFFSET_KIND) :: dimlen

    ! check dimension nx
    vname = 'nx'
    ierr = pio_inq_dimid(pioid, vname, dimid)
    call handle_err(ierr, 'inquire dimension '//trim(vname))
    ierr = pio_inq_dimlen(pioid, dimid, dimlen)
    if (dimlen /= int(nx,PIO_OFFSET_KIND)) then
      write(ndse,*) '*** WAVEWATCH III restart error: '//trim(vname)//' does not match expected value'
      call extcde ( 49 )
    end if

    ! check dimension ny
    vname = 'ny'
    ierr = pio_inq_dimid(pioid, vname, dimid)
    call handle_err(ierr, 'inquire dimension '//trim(vname))
    ierr = pio_inq_dimlen(pioid, dimid, dimlen)
    if (dimlen /= int(ny,PIO_OFFSET_KIND)) then
      write(ndse,*) '*** WAVEWATCH III restart error: '//trim(vname)//' does not match expected value'
      call extcde ( 49 )
    end if

    ! check number of directions
    vname = 'nth'
    ierr = pio_inq_varid(pioid, vname, varid)
    call handle_err(ierr, 'inquire variable '//trim(vname))
    ierr = pio_get_var(pioid, varid, ivar)
    call handle_err(ierr, 'get variable '//trim(vname))
    if (ivar .ne. nth) then
      write(ndse,*) '*** WAVEWATCH III restart error: '//trim(vname)//' does not match expected value'
      call extcde ( 49 )
    end if

    ! check number of frequencies
    vname = 'nk'
    ierr = pio_inq_varid(pioid, vname, varid)
    call handle_err(ierr, 'inquire variable '//trim(vname))
    ierr = pio_get_var(pioid, varid, ivar)
    call handle_err(ierr, 'get variable '//trim(vname))
    if (ivar .ne. nk) then
      write(ndse,*) '*** WAVEWATCH III restart error: '//trim(vname)//' does not match expected value'
      call extcde ( 49 )
    end if

  end subroutine checkfile

end module wav_restart_mod
