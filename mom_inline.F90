module mom_inline_mod


  use dshr_mod             , only: dshr_pio_init
  use dshr_strdata_mod     , only: shr_strdata_type
  use dshr_strdata_mod     , only: shr_strdata_init_from_inline
  use dshr_strdata_mod     , only: shr_strdata_advance
  use dshr_stream_mod      , only: shr_stream_init_from_esmfconfig

  implicit none
  private

  public mom_inline_init
  public mom_inline_run

  type(shr_strdata_type)          :: sdat    ! stream dat
  ! need array to hold dust input

  ! character(*),parameter :: u_FILE_u = __FILE__

  subroutine mom_inline_init(gcomp, clock, mesh, rc)

    ! lots of use statments....

    ! CDEPS Inline initialization

    ! input/output parameters
    type(ESMF_GridComp)    , intent(in)  :: gcomp
    type(ESMF_Clock)       , intent(in)  :: clock
    type(ESMF_Mesh)        , intent(in)  :: mesh
    integer                , intent(out) :: rc


    ! CMEPS Init PIO
    call dshr_pio_init(gcomp, sdat, logunit, rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! Read stream configuration file
    ! TODO: At this point it only suports ESMF config format (XML?)
    streamfilename = 'stream.config'
    call shr_stream_init_from_esmfconfig(streamfilename, sdat%stream, logunit, &
         sdat%pio_subsystem, sdat%io_type, sdat%io_format, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

  end subroutine mom_inline_init

  subroutine mom_inline_run
  end subroutine mom_inline_run

end module mom_inline_mod
