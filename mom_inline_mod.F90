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
  use dshr_methods_mod , only: dshr_fldbun_getfldptr
  use dshr_stream_mod  , only: shr_stream_init_from_esmfconfig
  use MOM_cap_methods  , only: ChkErr

  implicit none
  private

  public mom_inline_init
  public mom_inline_run

  !--------------------------------------------------------------------------
  ! Private data
  !--------------------------------------------------------------------------

  type config
     integer                                 :: year_first
     integer                                 :: year_last
     integer                                 :: year_align
     integer                                 :: offset
     real(ESMF_KIND_R8)                      :: dtlimit
     character(len=ESMF_MAXSTR)              :: mesh_filename
     character(len=ESMF_MAXSTR), allocatable :: data_filename(:)
     character(len=ESMF_MAXSTR), allocatable :: fld_list(:)
     character(len=ESMF_MAXSTR), allocatable :: fld_list_model(:)
     character(len=ESMF_MAXSTR)              :: mapalgo
     character(len=ESMF_MAXSTR)              :: taxmode
     character(len=ESMF_MAXSTR)              :: tintalgo
     character(len=ESMF_MAXSTR)              :: name
  end type config

  !type(config)           :: stream  ! stream configuration
  type(shr_strdata_type) :: sdat    ! input data stream

  character(len=*), parameter :: u_FILE_u =  __FILE__
contains

  subroutine mom_inline_init(gcomp, clock, mesh, mytask, logunit, streamconfigfile, rc)

    ! input/output parameters
    type(ESMF_GridComp)    , intent(in)  :: gcomp
    type(ESMF_Clock)       , intent(in)  :: clock
    type(ESMF_Mesh)        , intent(in)  :: mesh
    integer                , intent(in)  :: logunit
    integer                , intent(in)  :: mytask
    character(len=*)       , intent(in)  :: streamconfigfile
    integer                , intent(out) :: rc

    integer                                 :: nstreams, streamid, l
    !type(shr_strdata_type)                  :: sdat_config
    character(len=ESMF_MAXSTR), allocatable :: fileList(:), varList(:,:)
    character(len=ESMF_MAXSTR)              :: streamfilename, suffix, fldname
    character(len=*), parameter  :: subname='(mom_inline_init)'


    ! CMEPS Init PIO
    call dshr_pio_init(gcomp, sdat, logunit, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    !if(mytask == 0)sdat%mainproc = .true.

    !nstreams = size(sdat_config%stream)
    !if (mytask == 0)print *,'XX0 ',nstreams
    !if (.not. allocated(sdat)) allocate(sdat(nstreams))

    ! print out sdat_config info
    !if (mytask == 0) then
    !   call shr_strdata_print(sdat_config%stream,'fake runoff ')
    !endif

    ! Read stream configuration file
    ! TODO: At this point it only suports ESMF config format (XML?)
    !streamfilename = 'stream.config'
    call shr_stream_init_from_esmfconfig(streamconfigfile, sdat%stream, logunit, &
         sdat%pio_subsystem, sdat%io_type, sdat%io_format, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    if (mytask == 0)print *,'XX0 return from init_from_esmfconfig ',size(sdat%pstrm),size(sdat%stream)

    if (mytask == 0) then
       !print *,'XX0 ',trim(sdat%stream%file%name)
       print *,'XX0 ',trim(sdat%pstrm(1)%stream_meshfile)
       !print *,'XX0 ',trim(sdat%stream%file)
    end if

    ! initialize sdat
    ! call shr_strdata_init_from_inline(sdat,               &
    !      my_task             = mytask,                   &
    !      logunit             = logunit,                   &
    !      compname            = 'OCN',                     &
    !      model_clock         = clock,                     &
    !      model_mesh          = mesh,                      &
    !      stream_meshfile     = sdat%stream%stream_meshfile,           &
    !      stream_lev_dimname  = 'null',                    &
    !      stream_mapalgo      = trim(sdat%stream%mapalgo),      &
    !      stream_filenames    = trim(sdat%stream%file%name), &
    !      stream_fldlistFile  = trim(sdat%fieldlist_stream)/),  &
    !      stream_fldListModel = trim(sdat%fieldlist_model)/),  &
    !      stream_yearFirst    = sdat%stream_yearFirst,          &
    !      stream_yearLast     = sdat%stream_yearLast,           &
    !      stream_yearAlign    = sdat%stream_yearAlign ,         &
    !      stream_offset       = 0,                         &
    !      stream_taxmode      = trim(sdat%stream_taxmode),      &
    !      stream_dtlimit      = 1.5_dbl_kind,              &
    !      stream_tintalgo     = 'linear',                  &
    !      rc                  = rc)
    ! if (ChkErr(rc,__LINE__,u_FILE_u)) return




    !  if (mytask == 0) then
    !    write(logunit,'(a)'   ) ' '
    !    write(logunit,'(a,i8)')  'stream settings:'
    !    write(logunit,'(a,a)' )  '  stream_data_filename = ',trim(stream_filenames(1))
    !    write(logunit,'(a,a)' )  '  stream_mesh_filename = ',trim(stream_meshfile)
    !    write(logunit,'(a,a,a)') '  stream_varlist       = ',trim(stream_fldlistfile(1)), trim(stream_fldlistmodel(1))
    !    write(logunit,'(a,i8)')  '  stream_year_first    = ',stream_yearFirst
    !    write(logunit,'(a,i8)')  '  stream_year_last     = ',stream_yearLast
    !    write(logunit,'(a,i8)')  '  stream_year_align    = ',stream_yearAlign
    !    write(logunit,'(a)'   )  ' '
    ! endif

     ! ! Initialize the cdeps data type sdat_ndep
     ! call shr_strdata_init_from_inline(sdat_ndep,                    &
     !      my_task             = iam,                                 &
     !      logunit             = iulog,                               &
     !      compname            = 'ATM',                               &
     !      model_clock         = model_clock,                         &
     !      model_mesh          = model_mesh,                          &
     !      stream_meshfile     = trim(streammesh_filename),     &
     !      stream_filenames    = (/trim(streamdata_filename)/), &
     !      stream_yearFirst    = streamyear_first,              &
     !      stream_yearLast     = streamyear_last,               &
     !      stream_yearAlign    = streamyear_align,              &
     !      stream_fldlistFile  = stream_varlist_ndep,                 &
     !      stream_fldListModel = stream_varlist_ndep,                 &
     !      stream_lev_dimname  = 'null',                              &
     !      stream_mapalgo      = 'bilinear',                          &
     !      stream_offset       = 0,                                   &
     !      stream_taxmode      = 'cycle',                             &
     !      stream_dtlimit      = 1.0e30_r8,                           &
     !      stream_tintalgo     = 'linear',                            &
     !      stream_name         = 'Nitrogen deposition data ',         &
     !      rc                  = rc)


    ! do ns = 1,nstreams
    !    sdat(ns)%nfiles =

    ! ! Allocate temporary variable to store file names in the stream
    ! allocate(fileList(sdat_config%stream(streamid)%nfiles))
    ! allocate(varList(sdat_config%stream(streamid)%nvars,2))

    ! do l = 1, sdat_config%stream(streamid)%nfiles
    !    fileList(l) = trim(sdat_config%stream(streamid)%file(l)%name)
    !    !if (mytask) write(logunit,'(a,i2,2x,a)') trim(subname)//": file     ", l, trim(fileList(l))
    ! end do
    ! do l = 1, sdat_config%stream(streamid)%nvars
    !    varList(l,1) = trim(sdat_config%stream(streamid)%varlist(l)%nameinfile)
    !    varList(l,2) = trim(sdat_config%stream(streamid)%varlist(l)%nameinmodel)
    !    !if (mytask) write(logunit,'(a,i2,2x,a)') trim(subname)//": variable ", l, trim(varList(l,1))//" -> "//trim (varList(l,2))
    ! end do

    ! ! Fill file and variable lists with data
    ! do l = 1, sdat_config%stream(streamid)%nfiles
    !    fileList(l) = trim(sdat_config%stream(streamid)%file(l)%name)
    !    !if (mytask) write(logunit,'(a,i2,2x,a)') trim(subname)//": file     ", l, trim(fileList(l))
    ! end do
    ! do l = 1, sdat_config%stream(streamid)%nvars
    !    varList(l,1) = trim(sdat_config%stream(streamid)%varlist(l)%nameinfile)
    !    varList(l,2) = trim(sdat_config%stream(streamid)%varlist(l)%nameinmodel)
    !    !if (mytask) write(logunit,'(a,i2,2x,a)') trim(subname)//": variable ", l, trim(varList(l,1))//" -> "//trim (varList(l,2))
    ! end do
    ! if (mytask == 0)print *,'XX0 calling shr_strdata_init_from_inline',sdat_config%mainproc



    ! call shr_strdata_init_from_inline(sdat,         &
    !      my_task = mytask,                                                  &
    !      logunit = logunit,                                                 &
    !      compname = 'OCN', model_clock=clock, model_mesh=mesh,              &
    !      stream_meshfile=trim(sdat_config%stream(streamid)%meshfile),       &
    !      stream_filenames=filelist,                                         &
    !      stream_yearFirst=sdat_config%stream(streamid)%yearFirst,           &
    !      stream_yearLast=sdat_config%stream(streamid)%yearLast,             &
    !      stream_yearAlign=sdat_config%stream(streamid)%yearAlign,           &
    !      stream_fldlistFile=varList(:,1),                                   &
    !      stream_fldListModel=varlist(:,2),                                  &
    !      stream_lev_dimname=trim(sdat_config%stream(streamid)%lev_dimname), &
    !      stream_mapalgo=trim(sdat_config%stream(streamid)%mapalgo),         &
    !      stream_offset=sdat_config%stream(streamid)%offset,                 &
    !      stream_taxmode=trim(sdat_config%stream(streamid)%taxmode),         &
    !      stream_dtlimit=sdat_config%stream(streamid)%dtlimit,               &
    !      stream_tintalgo=trim(sdat_config%stream(streamid)%tInterpAlgo),    &
    !      stream_name='test',                                                &
    !      stream_src_mask=sdat_config%stream(streamid)%src_mask_val,         &
    !      stream_dst_mask=sdat_config%stream(streamid)%dst_mask_val,         &
    !      rc=rc)
    if (mytask == 0)print *,'XX0 return shr_strdata_init_from_inline'

    ! print out sdat info
    !if (mytask == 0) then
    !   call shr_strdata_print(sdat,'fake runoff ')
    !endif

  end subroutine mom_inline_init

  subroutine mom_inline_run(clock, isc, iec, jsc, jec, output, logunit, rc)

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

    integer ,             intent(in)    :: logunit
    integer ,             intent(out)   :: rc

    ! local variables
    type(ESMF_Time)     :: date
    integer             :: i,j,n,nfld
    integer             :: jjcpl
    integer             :: year    ! year (0, ...) for nstep+1
    integer             :: mon     ! month (1, ..., 12) for nstep+1
    integer             :: day     ! day of month (1, ..., 31) for nstep+1
    integer             :: sec     ! seconds into current date for nstep+1
    integer             :: mcdate  ! Current model date (yyyymmdd)
    real(ESMF_KIND_R8), pointer   :: dataPtr1d(:)
    !-----------------------------------------------------------------------

    ! Advance sdat stream
    call ESMF_ClockGet( clock, currTime=date, rc=rc )
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet(date, yy=year, mm=mon, dd=day, s=sec, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    mcdate = year*10000 + mon*100 + day

    call shr_strdata_advance(sdat, ymd=mcdate, tod=sec, logunit=logunit, istr='merra2_runoff', rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! Get pointer for stream data that is time and spatially interpolated to model time and grid
    call dshr_fldbun_getFldPtr(sdat%pstrm(1)%fldbun_model, 'DUCMASS', dataPtr1d, rc=rc)
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
