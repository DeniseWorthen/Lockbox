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

  implicit none
  private

  public mom_inline_init
  public mom_inline_run

  integer :: logunit   ! the logunit on mytask = 0

  character(len=ESMF_MAXSTR), allocatable :: streamfilelist(:)
  character(len=ESMF_MAXSTR), allocatable :: streamfilevars(:,:)

  type(shr_strdata_type) :: sdat    ! input data stream

  integer, parameter :: nfiles_max = 100  ! maximum number of stream files

  character(len=*), parameter :: u_FILE_u =  __FILE__

  !===============================================================================
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

    integer :: ns, nf, nv, fcnt, scnt, vcnt
    integer :: nstreams, nfiles, nvars
    character(len=*), parameter  :: subname='(mom_inline_init)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    if (mytask == 0) then
       open (newunit=logunit, file='log.mom6.cdeps')
    else
       logunit = 6
    end if

    ! CMEPS Init PIO
    call dshr_pio_init(gcomp, sdat, logunit, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    call shr_stream_init_from_esmfconfig(streamconfigfile, sdat%stream, logunit, &
         sdat%pio_subsystem, sdat%io_type, sdat%io_format, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    nstreams = size(sdat%stream)
    if (mytask == 0) print *,'XX1 nstreams = ',nstreams
    nfiles = size(sdat%stream%nfiles)
    ! abort if stream mesh is not same for all files?

    !do ns = 1,nstreams
    !   if (mytask == 0) print *,'XX1 ',ns,sdat%stream(ns)%nfiles
    !end do
    if (mytask == 0) print *,'XX1 nfiles = ',nfiles
    !nfiles = sdat%stream(1)%nfiles
    nvars = size(sdat%stream%nvars)
    if (mytask == 0) print *,'XX1 nvars = ',nvars
    ! abort if stream filevars not same for all files?
    allocate(streamfilelist(1:nfiles))
    allocate(streamyears(1:nfiles,3))          ! first, last, align
    allocate(streaminterpolation(1:nfiles,3))  ! mapalgo, taxmode, tinterpalgo
    allocate(streadtlimit(1:nfiles))
    allocate(streamfilevars(1:nvars,2))

    ! build the stream lists
    fcnt = 0
    vcnt = 0
    do ns = 1,nstreams
       do nf = 1,nfiles
          fcnt = fcnt + 1
          streammeshfile(fcnt) = trim(sdat%stream(ns)%meshfile)
          streamfilelist(fcnt) = trim(sdat%stream(ns)%file(nf)%name)
          streamyears(fcnt,1) = sdat%stream(ns)%yearFirst
          streamyears(fcnt,2) = sdat%stream(ns)%yearLast
          streamyears(fcnt,3) = sdat%stream(ns)%yearAlign
          streaminterpolation(fcnt,1) = trim(sdat%stream(ns)%mapalgo)
          streaminterpolation(fcnt,2) = trim(sdat%stream(ns)%taxmode)
          streaminterpolation(fcnt,3) = trim(sdat%stream(ns)%tinterpalgo)
          streamdtlimit(fcnt) = sdat%stream(ns)%dtlimit
       end do
       do nv = 1,nvars
          vcnt = vcnt + 1
          streamfilevars(vcnt,1) = trim(sdat%stream(ns)%varlist(nv)%nameinfile)
          streamfilevars(vcnt,2) = trim(sdat%stream(ns)%varlist(nv)%nameinmodel)
       end do
    end do

    ! do checks=== mesh file are same, interpolation modes all the same, filevars all the same?
    ! if (mytask == 0) then
    !    write(logunit,'(a)')  ' stream settings: '
    !    write(logunit,'(a)' )  '  stream_mesh_filename = '//trim(sdat%stream(1)%meshfile)
    !    do nf = 1,nfiles
    !       write(logunit,'(a)' ) '  stream_filenames = '//trim(streamfilelist(nf))
    !       write(logunit,'(a,i8)')  '  stream_year_first    = ',sdat%stream(1)%yearFirst
    !       write(logunit,'(a,i8)')  '  stream_year_last     = ',sdat%stream(1)%yearLast
    !       write(logunit,'(a,i8)')  '  stream_year_align    = ',sdat%stream(1)%yearAlign
    !    end do
    !    do nv = 1,nvars
    !       write(logunit,'(a)' ) '  stream_fldlist file,model = '//trim(streamfilevars(nv,1))//'  '//trim(streamfilevars(nv,2))
    !    end do
    !    write(logunit,'(a)'   )  ' '
    ! endif

    ! initialize sdat
    call shr_strdata_init_from_inline(sdat,              &
         my_task             = mytask,                   &
         logunit             = logunit,                  &
         compname            = 'OCN',                    &
         model_clock         = model_clock,              &
         model_mesh          = model_mesh,               &
         stream_meshfile     = streammeshfile,           &
         stream_lev_dimname  = 'null',                   &
         stream_mapalgo      = streaminterpolation(:,1), &
         stream_filenames    = streamfilelist,           &
         stream_fldlistFile  = streamfilevars(:,1),      &
         stream_fldListModel = streamfilevars(:,2),      &
         stream_yearFirst    = streamyears(:,1),         &
         stream_yearLast     = streamyears(:,2),         &
         stream_yearAlign    = streamyears(:.3) ,        &
         stream_offset       = 0,                        &
         stream_taxmode      = streaminterpolation(:,2), &
         stream_dtlimit      = streamdtlimit,            &
         stream_tintalgo     = streaminterpolation(:,3), &
         rc                  = rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

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
    call dshr_fldbun_getFldPtr(sdat%pstrm(1)%fldbun_model, trim(fldname), dataPtr1d, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    call dshr_fldbun_Field_diagnose(sdat%pstrm(1)%fldbun_model, 'mom6_inline_run', trim(fldname), rc=rc)
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
