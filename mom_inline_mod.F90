module mom_inline_mod

  use ESMF             , only: ESMF_GridComp, ESMF_Mesh
  use ESMF             , only: ESMF_Clock, ESMF_Time, ESMF_TimeGet, ESMF_ClockGet
  use ESMF             , only: ESMF_KIND_R8, ESMF_SUCCESS, ESMF_LogFoundError
  use ESMF             , only: ESMF_LOGERR_PASSTHRU, ESMF_LOGMSG_INFO, ESMF_LOGWRITE
  use ESMF             , only: ESMF_MAXSTR
  use dshr_mod         , only: dshr_pio_init
  use dshr_strdata_mod , only: shr_strdata_type, shr_strdata_print
  use dshr_strdata_mod , only: shr_strdata_init_from_inline
  use dshr_strdata_mod , only: shr_strdata_advance
  use dshr_methods_mod , only: dshr_fldbun_getfldptr, dshr_fldbun_Field_diagnose
  use dshr_stream_mod  , only: shr_stream_init_from_esmfconfig
  use MOM_cap_methods  , only: ChkErr
  ! debug
  use ESMF, only : ESMF_Finalize, ESMF_END_ABORT

  implicit none
  private

  public mom_inline_init
  public mom_inline_run

  integer :: logunit   ! the logunit on mytask = 0

  character(len=ESMF_MAXSTR), allocatable :: streamfilelist(:)
  character(len=ESMF_MAXSTR), allocatable :: streamfilevars(:,:)

  type(shr_strdata_type) :: sdat
  ! available stream modes
  type(shr_strdata_type) :: sdat_lrunoff
  type(shr_strdata_type) :: sdat_frunoff

  integer :: streamid_lrunoff=0
  integer :: streamid_frunoff=0

  character(len=*), parameter :: u_FILE_u =  __FILE__
contains
  !===============================================================================
  subroutine mom_inline_init(gcomp, model_clock, model_mesh, mytask, streamconfigfile, rc)

    ! input/output parameters
    type(ESMF_GridComp)    , intent(in)  :: gcomp
    type(ESMF_Clock)       , intent(in)  :: model_clock
    type(ESMF_Mesh)        , intent(in)  :: model_mesh
    integer                , intent(in)  :: mytask
    character(len=*)       , intent(in)  :: streamconfigfile
    integer                , intent(out) :: rc

    integer :: ns, nf, nv
    integer :: nstreams, nfiles, nvars

    character(len=*), parameter  :: subname='(mom_inline_init)'
    !----------------------------------------------------------------------

    rc = ESMF_SUCCESS

    if (mytask == 0) then
       open (newunit=logunit, file='log.mom6.cdeps')
    else
       logunit = 6
    end if

    ! CMEPS Init PIO
    call dshr_pio_init(gcomp, sdat, logunit, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! read the available stream definitions, each data stream is one or more data_files
    ! which have the same spatial and temporal coordinates
    ! returns sdat%stream, of type shr_stream_streamType
    call shr_stream_init_from_esmfconfig(streamconfigfile, sdat%stream, logunit, &
         sdat%pio_subsystem, sdat%io_type, sdat%io_format, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    nstreams = size(sdat%stream)
    if (mytask == 0) print *,'XX1 ',nstreams
    ! locate stream data configuration
    do ns = 1,nstreams
       nvars = sdat%stream(ns)%nvars
       do nv = 1,nvars
          if (mytask == 0)print *,'XX1 ',ns,nv,trim(sdat%stream(ns)%varlist(nv)%nameinfile),trim(sdat%stream(ns)%varlist(nv)%nameinmodel)
          if (trim(sdat%stream(ns)%varlist(nv)%nameinmodel) == 'lrunoff') streamid_lrunoff = ns
          if (trim(sdat%stream(ns)%varlist(nv)%nameinmodel) == 'frunoff') streamid_frunoff = ns
       end do
    end do

    if (streamid_lrunoff /= 0) then
    !if (size(sdat_lrunoff(1)) == 0 .and. streamid_lrunoff /= 0) then
       !call init_sdat(sdat_lrunoff, streamid_lrunoff, sdat, rc=rc)
       ! fill file list, etc

       nfiles = sdat%stream(streamid_lrunoff)%nfiles
       nvars = sdat%stream(streamid_lrunoff)%nvars

       allocate(streamfilelist(1:nfiles))
       allocate(streamfilevars(1:nvars,2))

       do nf = 1,nfiles
          streamfilelist(nf) = trim(sdat%stream(streamid_lrunoff)%file(nf)%name)
          if (mytask == 0) print *,'XX1 ',nf,trim(streamfilelist(nf))
       end do
       do nv = 1,nvars
          streamfilevars(nv,1) = trim(sdat%stream(streamid_lrunoff)%varlist(nv)%nameinfile)
          streamfilevars(nv,2) = trim(sdat%stream(streamid_lrunoff)%varlist(nv)%nameinmodel)
          if (mytask == 0) print *,'XX1 ',nv,trim(streamfilevars(nv,1)),' ',trim(streamfilevars(nv,2))
       end do

       ! Set PIO related variables
       sdat_lrunoff%pio_subsystem => sdat%pio_subsystem
       sdat_lrunoff%io_type = sdat%io_type
       sdat_lrunoff%io_format = sdat%io_format

       call shr_strdata_init_from_inline(sdat_lrunoff,                             &
            my_task             = mytask,                                          &
            logunit             = logunit,                                         &
            compname            = 'OCN',                                           &
            model_clock         = model_clock,                                     &
            model_mesh          = model_mesh,                                      &
            stream_name         = 'lrunoff',                                       &
            stream_meshfile     = trim(sdat%stream(streamid_lrunoff)%meshfile),    &
            stream_filenames    = streamfilelist,                                  &
            stream_yearFirst    = sdat%stream(streamid_lrunoff)%yearFirst,         &
            stream_yearLast     = sdat%stream(streamid_lrunoff)%yearLast,          &
            stream_yearAlign    = sdat%stream(streamid_lrunoff)%yearAlign,         &
            stream_fldlistFile  = streamfilevars(:,1),                             &
            stream_fldListModel = streamfilevars(:,2),                             &
            stream_lev_dimname  = trim(sdat%stream(streamid_lrunoff)%lev_dimname), &
            stream_mapalgo      = trim(sdat%stream(streamid_lrunoff)%mapalgo),     &
            stream_offset       = sdat%stream(streamid_lrunoff)%offset,            &
            stream_taxmode      = trim(sdat%stream(streamid_lrunoff)%taxmode),     &
            stream_dtlimit      = sdat%stream(streamid_lrunoff)%dtlimit,           &
            stream_tintalgo     = trim(sdat%stream(streamid_lrunoff)%tInterpAlgo), &
            stream_src_mask     = sdat%stream(streamid_lrunoff)%src_mask_val,      &
            stream_dst_mask     = sdat%stream(streamid_lrunoff)%dst_mask_val,      &
            rc                  = rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       deallocate(streamfilelist)
       deallocate(streamfilevars)
    end if
    !if (size(sdat_frunoff) == 0 .and. streamid_frunoff =/ 0) then
    !   call init_sdat(sdat_frunoff, streamid_frunoff, sdat, rc=rc)
       ! fill file list, etc
       ! sdat_runoff%pio_subsystem => sdat%pio_subsystem
    !end if

    ! init_sdat
    ! type(shr_strdata_type), intent(inout) :: sdat
    ! type(shr_strdata_type), intent(inout) :: sdat
    ! integer, intent(in) :: streamid

    ! initialize the
    !nfiles = sdat%stream(1)%nfiles
    !nvars = sdat%stream(1)%nvars

    !allocate(streamfilelist(1:nfiles))
    !allocate(streamfilevars(1:nvars,2))

    ! ! build the file and variable lists
    ! do ns = 1,nstreams
    !    do nf = 1,nfiles
    !       streamfilelist(ns,nf) = trim(sdat%stream(ns)%file(nf)%name)
    !       if (mytask == 0) print *,'XX1 ',nf,trim(streamfilelist(nf))
    !    end do
    !    do nv = 1,nvars
    !       streamfilevars(ns,nv,1) = trim(sdat%stream(ns)%varlist(nv)%nameinfile)
    !       streamfilevars(ns,nv,2) = trim(sdat%stream(ns)%varlist(nv)%nameinmodel)
    !       if (mytask == 0) print *,'XX1 ',nv,trim(streamfilevars(nv,1)),' ',trim(streamfilevars(nv,2))
    !    end do
    ! end do

    !  if (mytask == 0) then
    !     write(logunit,'(a)')  ' stream settings: '
    !     write(logunit,'(a)' )  '  stream_mesh_filename = '//trim(sdat%stream(1)%meshfile)
    !     do nf = 1,nfiles
    !        write(logunit,'(a)' ) '  stream_filenames = '//trim(streamfilelist(nf))
    !     end do
    !     do nv = 1,nvars
    !        write(logunit,'(a)' ) '  stream_fldlist file,model = '//trim(streamfilevars(nv,1))//'  '//trim(streamfilevars(nv,2))
    !     end do
    !     write(logunit,'(a,i8)')  '  stream_year_first    = ',sdat%stream(1)%yearFirst
    !     write(logunit,'(a,i8)')  '  stream_year_last     = ',sdat%stream(1)%yearLast
    !     write(logunit,'(a,i8)')  '  stream_year_align    = ',sdat%stream(1)%yearAlign
    !     write(logunit,'(a)'   )  ' '
    !  endif

       ! if (streamid_lrunoff > 0) then
       !    allocate(sdat_lrunoff(1))
       !    sdat_lrunoff%stream = sdat%stream(streamid_lrunoff)
       !    sdat_lrunoff%pio = sdat%pio
       !    !sdat_lrunoff%steam_meshfile = trim(sdat%stream(streamid_lrunoff)%meshfile)
       !    !sdat_lrunoff%stream_mapalgo = trim(sdat%stream(streamid_lrunoff)%mapalgo)

       !end if
       !if (streamid_frunoff > 0) then
       !   allocate(sdat_frunoff)
       !   sdat_frunoff = sdat%stream(streamid_frunoff)
       !end if

    !end do


    !call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! call shr_strdata_init_from_inline(sdat,                      &
    !      my_task             = mytask,                           &
    !      logunit             = logunit,                          &
    !      compname            = 'OCN',                            &
    !      model_clock         = model_clock,                      &
    !      model_mesh          = model_mesh,                       &
    !      stream_meshfile     = trim(sdat%stream(1)%meshfile),    &
    !      stream_lev_dimname  = 'null',                           &
    !      stream_mapalgo      = trim(sdat%stream(1)%mapalgo),     &
    !      stream_filenames    = streamfilelist,                   &
    !      stream_fldlistFile  = streamfilevars(:,1),              &
    !      stream_fldListModel = streamfilevars(:,2),              &
    !      stream_yearFirst    = sdat%stream(1)%yearFirst,         &
    !      stream_yearLast     = sdat%stream(1)%yearLast,          &
    !      stream_yearAlign    = sdat%stream(1)%yearAlign ,        &
    !      stream_offset       = 0,                                &
    !      stream_taxmode      = trim(sdat%stream(1)%taxmode),     &
    !      stream_dtlimit      = sdat%stream(1)%dtlimit,           &
    !      stream_tintalgo     = trim(sdat%stream(1)%tinterpalgo), &
    !      rc                  = rc)
    ! if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine mom_inline_init
!===============================================================================

  subroutine mom_inline_run(clock, isc, iec, jsc, jec, output, rc)

    ! input/output variables
    type(ESMF_Clock) ,    intent(in)    :: clock
    integer ,             intent(in)    :: isc                     !< The start i-index of cell centers within
                                                                   !! the computational domain
    integer ,             intent(in)    :: iec                     !< The end i-index of cell centers within the
                                                                   !! computational domain
    integer ,             intent(in)    :: jsc                     !< The start j-index of cell centers within
                                                                   !! the computational domain
    integer ,             intent(in)    :: jec                     !< The end j-index of cell centers within
                                                                   !! the computational domain
    real (ESMF_KIND_R8) , intent(inout) :: output(isc:iec,jsc:jec) !< Output 2D array

    integer ,             intent(out)   :: rc

    ! local variables
    type(ESMF_Time)             :: date
    integer                     :: i,j,n
    character(len=ESMF_MAXSTR)  :: fldname
    integer                     :: year    ! year (0, ...) for nstep+1
    integer                     :: mon     ! month (1, ..., 12) for nstep+1
    integer                     :: day     ! day of month (1, ..., 31) for nstep+1
    integer                     :: sec     ! seconds into current date for nstep+1
    integer                     :: mcdate  ! Current model date (yyyymmdd)
    real(ESMF_KIND_R8), pointer :: dataPtr1d(:)
    !-----------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Advance sdat stream
    call ESMF_ClockGet( clock, currTime=date, rc=rc )
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet(date, yy=year, mm=mon, dd=day, s=sec, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    mcdate = year*10000 + mon*100 + day

    ! field name in file
    fldname = streamfilevars(1,1)

    call shr_strdata_advance(sdat, ymd=mcdate, tod=sec, logunit=logunit, istr='merra2_runoff', rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! Get pointer for stream data that is time and spatially interpolated to model time and grid
    call dshr_fldbun_getFldPtr(sdat%pstrm(1)%fldbun_model, 'DUCMASS', dataPtr1d, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    call dshr_fldbun_Field_diagnose(sdat%pstrm(1)%fldbun_model, trim(fldname), rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    n = 0
    do j = jsc,jec
       do i = isc,iec
          n = n + 1
          output(i,j)  = output(i,j) + dataPtr1d(n)
       end do
    end do

  end subroutine mom_inline_run

end module mom_inline_mod
