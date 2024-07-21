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
module w3nmluprstrmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           m. accensi              |
  !/                  |                                   |
  !/                  |                        fortran 90 |
  !/                  | last update :         06-oct-2020 |
  !/                  +-----------------------------------+
  !/
  !/    for updates see subroutines.
  !/
  !  1. purpose :
  !
  !     manages namelists from configuration file ww3_uprstr.nml for ww3_uprstr program
  !
  !/ ------------------------------------------------------------------- /
  ! module defaults
  implicit none
  public
  ! restart time
  type nml_restart_t
    character(15)               :: restarttime
  end type nml_restart_t
  ! update approach
  type nml_update_t
    character(5)                :: updproc
    real                        :: prcntg
    real                        :: prcntgcap
    real                        :: thrwsea
    character(30)               :: file
  end type nml_update_t
  ! miscellaneous
  character(256)                :: msg
  integer                       :: ndsn
contains
  !/ ------------------------------------------------------------------- /
  subroutine w3nmluprstr (ndsi, infile, nml_restart, nml_update, ierr)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         06-oct-2020 |
    !/                  +-----------------------------------+
    !/
    !
    !  1. purpose :
    !
    !     reads all the namelist to define the output field
    !
    !  2. method :
    !
    !     see source term routines.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !      ndsi        int.
    !      infile      char.
    !      nml_restart type.
    !      nml_update  type.
    !      ierr        int.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !      read_restart_nml
    !      read_update_nml
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name       type   module   description
    !     ----------------------------------------------------------------
    !      ww3_uprstr prog.  n/a      update restart file
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3odatmd, only: ndse
    implicit none
    integer, intent(in)                         :: ndsi
    character*(*), intent(in)                   :: infile
    type(nml_restart_t), intent(inout)          :: nml_restart
    type(nml_update_t), intent(inout)           :: nml_update
    integer, intent(out)                        :: ierr
    ierr = 0
    ! open namelist log file
    ndsn = 3
    open (ndsn, file=trim(infile)//'.log', form='formatted', iostat=ierr)
    if (ierr.ne.0) then
      write (ndse,'(a)') 'error: open full nml file '//trim(infile)//'.log failed'
      return
    end if
    ! open input file
    open (ndsi, file=trim(infile), form='formatted', status='old', iostat=ierr)
    if (ierr.ne.0) then
      write (ndse,'(a)') 'error: open input file '//trim(infile)//' failed'
      return
    end if
    ! read restart time namelist
    call read_restart_nml (ndsi, nml_restart)
    call report_restart_nml (nml_restart)
    ! read update approach namelist
    call read_update_nml (ndsi, nml_update)
    call report_update_nml (nml_update)
    ! close namelist files
    close (ndsi)
    close (ndsn)
  end subroutine w3nmluprstr
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_restart_nml (ndsi, nml_restart)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         06-oct-2020 |
    !/                  +-----------------------------------+
    !/
    !  1. purpose :
    !
    !
    !  2. method :
    !
    !     see source term routines.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !      ndsi         int.
    !      nml_restart  type.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name        type   module  description
    !     ----------------------------------------------------------------
    !      w3nmluprstr subr.   n/a    namelist configuration routine.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3odatmd, only: ndse
    use w3servmd, only: extcde
    implicit none
    integer, intent(in)                 :: ndsi
    type(nml_restart_t), intent(inout)    :: nml_restart
    ! locals
    integer                                :: ierr
    type(nml_restart_t) :: restart
    namelist /restart_nml/ restart
    ierr = 0
    ! set default values
    restart%restarttime  = '19680607 120000'
    ! read restart namelist
    rewind (ndsi)
    read (ndsi, nml=restart_nml, iostat=ierr, iomsg=msg)
    if (ierr.ne.0) then
      write (ndse,'(a,/a)') &
           'error: read_restart_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (1)
    end if
    ! save namelist
    nml_restart = restart
  end subroutine read_restart_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_update_nml (ndsi, nml_update)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         06-oct-2020 |
    !/                  +-----------------------------------+
    !/
    !  1. purpose :
    !
    !
    !  2. method :
    !
    !     see source term routines.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !      ndsi         int.
    !      nml_update   type.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name        type   module  description
    !     ----------------------------------------------------------------
    !      w3nmluprstr subr.   n/a    namelist configuration routine.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3odatmd, only: ndse
    use w3servmd, only: extcde
    implicit none
    integer, intent(in)                 :: ndsi
    type(nml_update_t), intent(inout)   :: nml_update
    ! locals
    integer                                :: ierr
    type(nml_update_t) :: update
    namelist /update_nml/ update
    ierr = 0
    ! set default values for update approach
    ! as set, these would run the update but not correct
    ! any spectra (scalar correction by factor of 1.0)
    update%updproc    = 'upd0f'      ! update type
    update%prcntg     = 1.0          ! scalar correction factor (1.0=no correction)
    update%prcntgcap  = 10.0         ! cap on correction factor
    update%thrwsea    = 0.7          ! energy threshold for wind-sea dominance
    update%file       = 'anl.grbtxt' ! corrected analysed swh field file
    ! read file namelist
    rewind (ndsi)
    read (ndsi, nml=update_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (ndse,'(a,/a)') &
           'error: read_update_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (2)
    end if
    ! save namelist
    nml_update = update
  end subroutine read_update_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_restart_nml (nml_restart)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                        fortran 90 |
    !/                  | last update :         06-oct-2020 |
    !/                  +-----------------------------------+
    !/
    !/
    !  1. purpose :
    !
    !
    !  2. method :
    !
    !     see source term routines.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !      nml_restart  type.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name        type   module  description
    !     ----------------------------------------------------------------
    !      w3nmluprstr subr.   n/a    namelist configuration routine.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    type(nml_restart_t), intent(in) :: nml_restart
    write (msg,'(a)') 'restart % '
    write (ndsn,'(a)')
    write (ndsn,10) trim(msg),'restarttime = ', trim(nml_restart%restarttime)
10  format (a,2x,a,a)
  end subroutine report_restart_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_update_nml (nml_update)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                        fortran 90 |
    !/                  | last update :         06-oct-2020 |
    !/                  +-----------------------------------+
    !/
    !/
    !  1. purpose :
    !
    !
    !  2. method :
    !
    !     see source term routines.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !      nml_update  type.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name        type   module  description
    !     ----------------------------------------------------------------
    !      w3nmluprstr subr.   n/a    namelist configuration routine.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    type(nml_update_t), intent(in) :: nml_update
    write (msg,'(a)') 'update % '
    write (ndsn,'(a)')
    write (ndsn,10) trim(msg),'updproc   = ', trim(nml_update%updproc)
    ! prcntg only used by upd0f
    if (trim(nml_update%updproc) .eq. 'upd0f') then
      write (ndsn,11) trim(msg),'prcntg    = ', nml_update%prcntg
    else
      write (ndsn,11) trim(msg),'prcntgcap = ', nml_update%prcntgcap
      ! thrwsea only used by upd5/6
      if ((trim(nml_update%updproc) .eq. 'upd5') .or. &
           (trim(nml_update%updproc) .eq. 'upd6')) then
        write (ndsn,11) trim(msg),'thrwsea   = ', nml_update%thrwsea
      endif
      write (ndsn,10) trim(msg),'file      = ', trim(nml_update%file)
    endif
10  format (a,2x,a,a)
11  format (a,2x,a,f5.3)
  end subroutine report_update_nml
  !/ ------------------------------------------------------------------- /
end module w3nmluprstrmd
!/ ------------------------------------------------------------------- /
