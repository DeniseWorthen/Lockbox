!> @file w3ounfmetamd.f90
!> @brief user configurable netcdf meta-data for ww3_ounf.
!> @author chris bunney @date 02-nov-2020
!/ ------------------------------------------------------------------- /
!> @brief manages user configurable netcdf meta-data for ww3_ounf program.
!>
!> @details default netcdf meta data is provided for each ww3 output variable
!>    and is stored intentally via the meta_t type. the meta values are
!>    grouped by component (max 3), field (ifi) and group (ifj).
!>
!>    the user can override this meta data via an input text file
!>    with the filename `ounfmeta.inp`.
!>
!>    entries in the file are formatted as follows:
!>
!>    @verbatim
!>    meta [ ifi [ ifj ]  |  fldid ]   [ ifc ]
!>      attr_name = attr_value
!>      attr_name = attr_value
!>      extra_attr = extra_value [type]
!>    ... repeated as many times as required.
!>    @endverbatim
!>
!>    an output field is selected using the meta keyword followed by
!>    either an [ifi, ifj] integer pair or a fieldid string. optionally,
!>    either form may be followed by an integer value to select the
!>    component in multi-component fields (such as wind).
!>
!>    blank lines and comments lines (starting with $) are ignored.
!>
!>    attr_name is the netcdf attribute name that you wish to override.
!>    this can be one of the following:
!>        -  "varnm"
!>        -  "ename"
!>        -  "standard_name", or "varns"
!>        -  "long_name" or "varnl"
!>        -  "globwave_name" or "varng"
!>        -  "direction_reference", "dir_ref" or "varnd"
!>        -  "comment" or "varnc"
!>        -  "units"
!>        -  "valid_min" or "vmin"
!>        -  "valid_max" or "vmax"
!>        -  "scale_factor" or "fsc"
!>
!>    any other attribute name is assumed to be an optional "extra"
!>    attribute. this extra attribute can take an optional "type"
!>    keyworkd to specify the variable tpye of the metadata. if
!>    no type is supplied, it defaults to a characer type. valid
!>    types are one of ["c", "r", "i"] for character/string,
!>    real/float or integer values respectively.
!>
!>    global meta data can be specified with a special "meta global" line:
!>
!>    @verbatim
!>    meta global
!>      extra_attr = extra_value [type]
!>      extra_attr = extra_value [type]
!>    @endverbatim
!>
!>    a "coordinate reference system" (crs) can be specified for all output
!>    fields using the "crs" keyword. as a minimum, the "grid_mapping_name"
!>    attribute must be specified. if the crs section is defined, all output
!>    fields will have a "grid_mapping" attribute added referencing the
!>    crs variable. "crs_vaname" will be created as a scalar nf90_char
!>    variable in the output file.
!>
!>    @verbatim
!>    crs <crs_varname>
!>      grid_mapping_name = <mapping name>
!>      attr = value
!>      attr = value
!>    @endverbatim
!>
!>    note: all keywords and "field name id" strings (e.g. hs) are
!>    case insensitive. all netcdf attribute names are case sensitive.
!>
!>    partitioned outputs are handles slightly differently; one meta data
!>    entry is used for all partitions of a field. the metadata is made
!>    specific to a particular partition via template strings. there are
!>    two built-in template strings: spart and ipart. these provide a
!>    "string" description (e.g. "wind sea", "primary swell", etc) or an
!>    integer partition number. these can be references in the meta data
!>    using the template name surrounded by < .. >, e.g. \<spart\>
!>
!>    it is also possible to supply user defined partitioned parameter
!>    template strings in the ounfmeta.inp file using the template
!>    keyword, as below:
!>
!>    @verbatim
!>    template <template-name>
!>      string for partition 0
!>      string for partition 1
!>      string for partition 2
!>      string for partition 3
!>      ... etc
!>    @endverbatim
!>
!>    specifying the <template-name> with a trailing underscore will
!>    provide an underscore seperated (_) string, rather than space
!>    seperated.
!>
!>    example ounfmeta.inp file:
!>
!>    @verbatim
!>    $ lines starting with dollars are comments.
!>    $ the line starts a meta-data section for the depth field
!>    meta dpt
!>      standard_name = depth
!>      long_name = "can be quoted string"
!>      comment = or an unquoted string
!>      vmax = 999.9
!>
!>    $ next one is hsig (group 2, field 1)
!>    meta 2 1
!>      varns = "sig. wave height"
!>      varnl = "this is long name"
!>
!>    $ next one is second component of wind. it also sets an
!>    $ "extra" meta data value (height - a float)
!>    meta wnd 2
!>      standard_name = "v-wind"
!>      height = 10.0 "r"
!>
!>    $ user defined partitioned parameters template strings:
!>    template partstr
!>      wind wave
!>      primary swell
!>      secondary swell
!>
!>    $ use partition templates in partitioned hs field:
!>    $ (spart and ipart are built-in)
!>    meta phs
!>      standard_name = "<spart_>_sigificant_wave_height"
!>      long_name = "<partstr>"
!>      partition_number = "<ipart>"
!>
!>    $ coordinate reference system:
!>    crs crs
!>      grid_mapping_name = "latitude_longitude"
!>      semi_major_axis = 6371000.0 f
!>      inverse_flattening = 0 f
!>
!>    $ global metadata:
!>    meta global
!>      institution = ukmo
!>      comment "space seperated strings should be quoted" c
!>      version = 1.0 r
!>    @endverbatim
!>
!> @author chris bunney @date 02-nov-2020
!>
!> ### change log
!>   date      | ver  | comments
!> ------------|------|---------
!> 02-nov-2020 | 7.12 | creation
!> 26-jan-2021 | 7.12 | added tp and alternative dir/mag metadata for directional fields.
!> 16-dec-2020 | 7.12 | added user partition templates and coordinate reference system.
!> 02-feb-2021 | 7.12 | improved partitioned parameter template string implementation.
!> 22-mar-2021 | 7.12 | add extra coupling fields
!> 02-sep-2021 | 7.12 | add coordinates attribute
!>
module w3ounfmetamd
  !/
  !/    02-nov-2020 : creation                            ( version 7.12 )
  !/    26-jan-2021 : added tp and alternative dir/mag    ( version 7.12 )
  !/                  metadata for directional fields.
  !/    16-dec-2020 : added user partition templates      ( version 7.12 )
  !/                  and coordinate reference system.
  !/                  freeform meta data uses linked list.
  !/    02-feb-2021 : improved partitioned parameter      ( version 7.12 )
  !/                  template string implementation.
  !/    22-mar-2021 : adds extra coupling fields          ( version 7.13 )
  !/    02-sep-2021 : add coordinates attribute           ( version 7.12 )
  !/
  !/ ------------------------------------------------------------------- /
  !/
  use netcdf
  use constants, only: tpiinv
  use w3gdatmd, only: sig, nk, gtype, ungtype
  use w3odatmd, only: ptmeth, ptfcut, nogrp, noge, ngrpp,           &
       ndse, fnmpre, noswll
  use w3servmd, only: extcde, str_to_upper
  use w3metamd
  implicit none
  public
  logical, private :: debug = .false. !< control debug output to screen
  !> meta-data input filename
  character(len=*), parameter :: fn_meta = "ounfmeta.inp"
  !> string token for integer partition number
  character(len=*), parameter :: ipart_token  = "<ipart>"
  !> string token for partition descriptive string (space separated)
  character(len=*), parameter :: spart_token  = "<spart>"
  !> string token for partition descriptive string (underscore separated)
  character(len=*), parameter :: spart_token_ = "<spart_>"
  !> type for storing ww3 netcdf metadata for a variable
  type meta_t
    real :: fsc                             !< scaling factor for data
    real :: vmin                            !< "valid_min" attribute
    real :: vmax = unsetr                   !< "valid_max" attribute
    character(len=24)  :: units = unsetc    !< si units for field
    character(len=50)  :: ename = unsetc    !< field name used in output filename
    character(len=80)  :: varnm = unsetc, & !< netcdf variable name
         varnl = unsetc    !< "long_name" attibute
    character(len=120) :: varns = unsetc, & !< "standard_name" attribute
         varng = unsetc, & !< "globwave_name" attribute
         varnd = unsetc    !< "direction_convention" attribute
    character(len=512) :: varnc = unsetc    !< "comment attribute
    type(meta_list_t) :: extra              !< list of user defined meta data
    ! for updating meta only:
    integer :: ifi = 0, &           !< group index to update
         ifj = 0, &           !< field index to update
         ifc = 1              !< component index to update
    character(len=6) :: fldid = ''  !< field id to update
  endtype meta_t
  !> type for storage of meta data aggregated by component (nfield)
  type field_t
    type(meta_t), pointer :: meta(:) !< pointer to meta data for field
  end type field_t
  !> type for storage of meta data aggregated by field (ifi)
  type group_t
    type(field_t), allocatable :: field(:) !< pointer to fields in group
  end type group_t
  !> storage for meta data aggregated by group (ifj)
  type(group_t), allocatable :: group(:)
  !> storage for the global meta data (free form)
  type(meta_list_t) :: global_meta
  !> flag for using default (true) or user-defined (false) global meta data
  logical           :: fl_default_gbl_meta = .true.
  ! storage for coordinate reference system (crs) metadata:
  character(len=128) :: crs_name = '' !< coordinate reference system (crs) name
  type(meta_list_t)  :: crs_meta !< meta data list for crs
  logical            :: crs_is_default = .false. !< true if crs set by this module
  !> "coordinates" attribute - for defining auxiliary coordinates (for all variables)
  character(len=256) :: coords_attr = ''
  !> type for storing partitioned parameter template strings.
  !> defined as a linked-list
  type part_tmpl_t
    character(len=128)              :: tmpl         !< placeholder
    character(len=128), allocatable :: part_text(:) !< partition description
    integer(kind=2)                 :: np           !< num parts (max noswll)
    type(part_tmpl_t), pointer      :: next         !< linkedlist pointer
  end type part_tmpl_t
  !> user-defined partitionted paratmeters template strings
  type(part_tmpl_t), pointer :: part_tmpl
  integer            :: ncvartype   !< netcdf variable type (2=int, 3=real, 4=depends)
  character(len=30)  :: dircom      !< directional convention comment
  character(len=128) :: partcom     !< partitioning method comment
  character(len=15)  :: snamep(5)   !< part. standard name templates
  !> flag for vector (true) or direction/magnitude (false) convention
  !> for directional fields
  logical, private   :: vector
  logical            :: flrtd = .false. !< flag for rototed pole grid
contains
  !/ ------------------------------------------------------------------- /
  !> @brief allocates space for the meta_t arrays and sets some defaults.
  !>
  !> @details by default, directional fields will be set up to output
  !>    a magnitude and direction field. alternatively, if vec is set to
  !>    true, then u/v vectors will be generated instead.
  !>
  !> @note - vector output is currently only implemented for the
  !>    "current" and "wind" fields.
  !>
  !> @param vec output vectors for directional fields rather than
  !>    direction/magnitude.
  !>
  !> @author chris bunney @date 09-mar-2020
  !/ ------------------------------------------------------------------- /
  subroutine init_meta(vec)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           c. bunney               |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    09-nov-2020 : creation                            ( version 7.12 )
    !/    22-mar-2021 : added vector flag                   ( version 7.12 )
    !/
    !
    !  1. purpose :
    !
    !     allocates space for the meta_t arrays and sets some constants.
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    logical, intent(in), optional  :: vec
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    logical :: flgnml
    integer :: i, j
    vector = .true.
    if(present(vec)) vector = vec
    ! 1. allocate nested group, field structure:
    allocate(group(nogrp))
    do i = 1,nogrp
      allocate(group(i)%field(noge(i)))
      do j = 1,noge(i)
        allocate(group(i)%field(j)%meta(3)) ! hardcode to 3 components for the moment
      enddo
    enddo
    ! 1.1 make sure partitioned template pointer is null (i.e. empty list)
    nullify(part_tmpl)
    ! 2. set direction convention:
    dircom = ""
    !  set partitioning method comment and standard name templates:
    if( ptmeth .le. 3 ) then
      snamep(1) = 'wind'
      snamep(2) = 'primary swell'
      snamep(3) = 'secondary swell'
      snamep(4) = 'tertiary swell'
      snamep(5) = 'swell'
    else
      snamep(1) = 'wind'
      snamep(2) = 'swell'
      snamep(3:5) = ''
    endif
    if ( ptmeth .eq. 1 ) then
      partcom = "wind sea and swells defined using topographic " //   &
           "partitions and partition wave-age cut-off "     //   &
           "(wwiii default scheme)"
    else if ( ptmeth .eq. 2 ) then
      partcom = "wind sea and swells defined using topographic " //   &
           "partitions and spectral wave-age cut-off"
    else if ( ptmeth .eq. 3 ) then
      partcom = "wave components defined using topographic "     //   &
           "partitions only"
    else if ( ptmeth .eq. 4 ) then
      partcom = "wind sea and swell defined using spectral "     //   &
           "wave-age cut-off"
    else if ( ptmeth .eq. 5 ) then
      write(partcom, '("wave components defined using ",f5.3,'   //   &
           '"hz spectral frequency cutoff")') ptfcut
    else
      write(partcom, '("ptm_",i1,"_unknown")') ptmeth
    endif
    ! 3. set the default values for the ounf netcdf meta data.
    call default_meta()
    ! set the default coordiante reference system (if applicable)
    call default_crs_meta()
    ! if the ounfmeta.inp exists, read this in to override defaults:
    inquire(file=trim(fnmpre)//"ounfmeta.inp", exist=flgnml)
    if(flgnml) then
      call read_meta()
    endif
  end subroutine init_meta
  !
  !/ ------------------------------------------------------------------- /
  !> @brief de-allocates memory used for the meta_t arrays
  !>
  !> @author chris bunney @date 09-nov-2020
  !/ ------------------------------------------------------------------- /
  subroutine teardown_meta()
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           c. bunney               |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         09-nov-2020 |
    !/                  +-----------------------------------+
    !/
    !/    09-nov-2020 : creation                            ( version 7.12 )
    !/
    !
    !  1. purpose :
    !
    !     de-allocates memory used for the meta_t arrays
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer :: i, j
    do i = 1,nogrp
      do j = 1,noge(i)
        deallocate(group(i)%field(j)%meta)
      enddo
      deallocate(group(i)%field)
    enddo
    deallocate(group)
    call del_meta_list(global_meta)
    call del_meta_list(crs_meta)
  end subroutine teardown_meta
  !/ ------------------------------------------------------------------- /
  !> @brief reads the next valid line from the user meta input file.
  !>
  !> @details lines are repeatedly read in from the input file until
  !>    a valid input line is reached. blank lines and comment lines
  !>    (starting with $) are skipped.
  !>
  !>    if the end of file is reached before any valid line is read
  !>    then eof is set to true.
  !>
  !>    if the next valid line is a new section marker (meta or template)
  !>    then the new_section flag is set to true.
  !>
  !> @param[in]     ndmi   unit number of input file
  !> @param[out]    buf    next input line read from file
  !> @param[in,out] iline  line number of file
  !> @param[out]    eof    true if end-of-file is reached.
  !> @param[out]    new_section  true if new section marker found
  !>
  !> @author chris bunney @date 09-nov-2020
  !/ ------------------------------------------------------------------- /
  subroutine next_line(ndmi, buf, iline, eof, new_section)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           c. bunney               |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         09-nov-2020 |
    !/                  +-----------------------------------+
    !/
    !/    09-nov-2020 : creation                            ( version 7.12 )
    !/
    !
    !  1. purpose :
    !
    !     reads the next valid line from the user meta input file.
    !
    !  2. method :
    !     lines are repeatedly read in from the input file until
    !     a valid input line is reached. blank lines and comment lines
    !     (starting with $) are skipped.
    !
    !     if the end of file is reached before any valid line is read
    !     then eof is set to true.
    !
    !     if the next valid line is a new section marker (meta or template)
    !     then the new_section flag is set to true.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       ndmi    int.  i  unit number of input file
    !       buf    char.  o  next input line read from file
    !       iline   int. i/o line number of file
    !       eof    bool.  o  true if end-of-file is reached.
    !       new_section
    !              bool.  o  true if new section marker found
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    integer, intent(in)       :: ndmi
    character(*), intent(out) :: buf
    integer, intent(inout)    :: iline
    logical, intent(out)      :: eof
    logical, intent(out), optional :: new_section
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer :: ierr
    character(len=10) :: test
    eof = .false.
    ! keep reading from file until we read a line that is not:
    !   - a blank line
    !   - a comment line (starting with $)
    !   - the end of the file
    do
      read(ndmi, '(a)', iostat=ierr, err=101, end=100) buf
      iline = iline + 1
      ! remove any tab characters from buffer (replace with a space)
      call notabs(buf)
      ! empty line?
      if(trim(buf) == '') then
        if(debug) write(*,'(i5,1x,a20)') iline, '[blank line]'
        cycle
      endif
      if(trim(buf) == "$ debug on") then
        write(*,'(i5,1x,a20)') iline, '[debug on]'
        debug = .true.
        cycle
      endif
      if(trim(buf) == "$ debug off") then
        write(*,'(i5,1x,a20)') iline, '[debug off]'
        debug = .false.
        cycle
      endif
      ! read first token on line:
      read(buf, *) test
      ! check for comment:
      if(test(1:1) == "$" .or. trim(buf) == '') then
        if(debug) write(*,'(i5,1x,a20)') iline, '[comment line]'
        cycle
      endif
      ! check if is section header
      if(present(new_section)) then
        call str_to_upper(test)
        select case(test)
        case ("meta", "template", "crs")
          new_section = .true.
        case default
          new_section = .false.
        end select
      endif
      ! anything else can be considered the "next line"
      return
    enddo
    !/    escape locations
    !
    !     end of file
100 continue
    buf = ''
    eof = .true.
    return
    !
    !     i/o error
101 continue
    write(ndse, 1000) fn_meta, iline, ierr
    call extcde(10)
    !
1000 format (/' *** wavewatch iii error in w3ounfmeta : '/           &
         '     error reading metadata file'/                    &
         '     filename = ', a /                                &
         '     line no = ', i5 /                                &
         '     iostat =',i5 /)
    !
  end subroutine next_line
  !/ ------------------------------------------------------------------- /
  !> @brief replaces tab characters in a string with a space.
  !>
  !> @remark assumes ascii encoding (tab character is ascii value 9)
  !>
  !> @param[in,out] str character string to process
  !>
  !> @author chris bunney @date 02-nov-2020
  !/ ------------------------------------------------------------------- /
  subroutine notabs(str)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           c. bunney               |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         02-nov-2020 |
    !/                  +-----------------------------------+
    !/
    !/    02-nov-2020 : creation                            ( version 7.12 )
    !/
    !
    !  1. purpose :
    !
    !     replaces tab characters in a string with a space.
    !
    !  2. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       str    char.  i/o  character string to process
    !     ----------------------------------------------------------------
    !
    !  3. remarks :
    !
    !     assumes ascii encoding! tab character is ascii value 9.
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    character(*), intent(inout) :: str
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer, parameter :: ascii_tab = 9
    integer :: slen
    integer :: i
    !
    slen = len_trim(str)
    do i=1,slen
      if(ichar(str(i:i)) == ascii_tab) then
        str(i:i) = ' '
      endif
    enddo
  end subroutine notabs
  !/ ------------------------------------------------------------------- /
  !> @brief replaces single characters in a string.
  !>
  !> @returns a new string
  !>
  !> @param[in]  str  character string to process
  !> @param[in]  c    character to search for
  !> @param[in]  rep  character to substitute
  !>
  !> @author chris bunney @date 02-feb-2021
  !/ ------------------------------------------------------------------- /
  function replace_char(str, c, rep) result(ostr)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           c. bunney               |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         02-fev-2021 |
    !/                  +-----------------------------------+
    !/
    !/    02-feb-2021 : creation                            ( version 7.12 )
    !/
    !
    !  1. purpose :
    !
    !     replaces single characters in a string. returns a new string,
    !
    !  2. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       str    chararr  i  character string to process
    !       c      char     i  character to search for
    !       rep    char     i  character to substitute
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    character(*)        :: str
    character           :: c, rep
    character(len(str)) :: ostr
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer :: i
    ostr = trim(str)
    do
      i = index(trim(ostr), c)
      if(i .le. 0) exit
      ostr(i:i) = rep
    enddo
  end function replace_char
  !/ ------------------------------------------------------------------- /
  !> @brief reads meta data entries from the ountmeta.inp file
  !>
  !> @details this is the main entry routine for parsing the ounfmeta.inp
  !>    file. values read from the file will be used to update or add to
  !>    the default meta data values set in the default_meta()
  !>    subroutine.
  !>
  !> @author chris bunney @date 26-jan-2021
  !/ ------------------------------------------------------------------- /
  subroutine read_meta()
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           c. bunney               |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         26-jan-2021 |
    !/                  +-----------------------------------+
    !/
    !/    09-nov-2020 : creation                            ( version 7.12 )
    !/    26-jan-2021 : added tp and alternative dir/mag    ( version 7.12 )
    !/                  metadata for directional fields.
    !/
    !
    !  1. purpose :
    !
    !     reads meta data entries from the ountmeta.inp file and update
    !     default values set via the default_meta subroutine.
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer ::  ierr, i, ndmi
    character(len=256) :: buf
    type(meta_t), pointer :: pmeta
    character(len=32) :: test, testu
    integer :: ifi, ifj, ifc
    integer :: iline, mcnt
    logical :: eof
    ndmi = 60
    iline = 0
    mcnt = 0
    open(unit=ndmi, file=trim(fnmpre)//trim(fn_meta),                &
         status="old", iostat=ierr)
    if(ierr .ne. 0) then
      write(ndse, 5010) trim(fnmpre)//trim(fn_meta), ierr
      call extcde(10)
    endif
    ! loop over file, skipping comments or blank lines, until we find
    ! a meta line.
    do
      call next_line(ndmi, buf, iline, eof)
      if(eof) exit
      ! read first token on line:
      read(buf, *) test
      ! a new meta-data section will start with the keyword "meta"
      testu = test
      call str_to_upper(testu)
      if(testu == "meta") then
        mcnt = mcnt + 1
        if(debug) write(*,'(i5,1x,a20,1x,a)') iline, '[meta header]', trim(buf)
        ! get the ifi, ifj, ifc values from the header:
        i = index(buf, trim(test)) + 4 ! handles lower/mixed-case meta keyword
        call decode_header(buf(i:), iline, ifi, ifj, ifc)
        if(ifi .eq. -1) then
          write(ndse, 5011) trim(buf(i:)), trim(fn_meta), iline
          call extcde(10)
        endif
        ! ifi = 999 is a section for the "global" meta data
        if(ifi .eq. 999) then
          call read_freeform_meta_list(ndmi, iline, global_meta)
          cycle
        endif
        ! error checking on size of ifj, icomp, ipart.
        if(ifi .lt. 1 .or. ifi .gt. nogrp) then
          write(ndse,5013) nogrp, trim(fn_meta), iline
          call extcde(1)
        endif
        if(ifj .lt. 1 .or. ifj .gt. noge(ifi)) then
          write(ndse,5014) noge(ifi), trim(fn_meta), iline
          call extcde(1)
        endif
        if(ifc .lt. 1 .or. ifc .gt. 3) then
          write(ndse,5015) trim(fn_meta), iline
          call extcde(1)
        endif
        ! select correct variable metadata entry:
        pmeta => group(ifi)%field(ifj)%meta(ifc)
        ! update the metadata with values from file:
        call read_meta_pairs(ndmi, pmeta, iline)
      else if(testu == "template") then
        backspace(ndmi) ! we will reprocess this line
        call read_part_tmpl(ndmi, iline)
        cycle
      else if(testu == "crs") then
        backspace(ndmi) ! we will reprocess this line
        call read_crs_meta(ndmi, iline)
        cycle
      else
        ! anything else is a syntax error
        write(ndse, 5012) trim(fn_meta), iline, trim(buf)
        call extcde(10)
      endif
    enddo
    close(ndmi)
    !write(*, 5000) mcnt, n_gblmeta, n_crsmeta
    return
    !
5000 format(/' read in: ',i3,' variable metadata entries' /          &
         '     and: ',i3,' global meta data entries' /           &
         '     and: ',i3,' crs meta data entries' /)
    !
5010 format (/' *** wavewatch iii error in w3ounfmeta : '/           &
         '     error opening metadata file'/                    &
         '     filename = ', a /                                &
         '     iostat =', i5 /)
    !
5011 format (/' *** wavewatch iii error in w3ounfmeta : '/           &
         '     unknown field id: ',a /                          &
         '     filename = ', a /                                &
         '     line =', i5 /)
    !
5012 format (/' *** wavewatch iii error in w3ounfmeta : '/           &
         '     syntax error ' /                                 &
         '     filename = ', a /                                &
         '     line =', i5 /                                    &
         '  => ', a /)
    !
5013 format (/' *** wavewatch iii error in w3ounfmeta : ' /          &
         '     ifi value should be in range 1,',i2 /            &
         '     filename = ', a /                                &
         '     line =', i5 /                                    &
         '  => ', a /)
    !
5014 format (/' *** wavewatch iii error in w3ounfmeta : ' /          &
         '     ifj value should be in range 1,',i2 /            &
         '     filename = ', a /                                &
         '     line =', i5 /                                    &
         '  => ', a /)
    !
5015 format (/' *** wavewatch iii error in w3ounfmeta : ' /          &
         '     ifc value should be in range 1,3' /              &
         '     filename = ', a /                                &
         '     line =', i5 /                                    &
         '  => ', a /)
    !
  end subroutine read_meta
  !/ ------------------------------------------------------------------- /
  !> @brief decode the meta header line.
  !>
  !> @details the internal ww3 field can be specified either as an
  !>    [ifi, ifj] integer combination, or a field id tag (such as "hs").
  !>
  !>    both forms can also specify an optional component (ifc) integer
  !>    value for multi-component fields (defaults to 1).
  !>
  !>    field name id tags are case-insensitive, hs == hs == hs.
  !>
  !> @param[in]  buf    input header string (without leading meta tag)
  !> @param[in]  iline  line number (for error reporting)
  !> @param[out] ifi    output group number
  !> @param[out] ifj    output field number
  !> @param[out] ifc    component number (defaults to 1)
  !>
  !> @author chris bunney @date 02-feb-2021
  !/ ------------------------------------------------------------------- /
  subroutine decode_header(buf, iline, ifi, ifj, ifc)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           c. bunney               |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         02-feb-2021 |
    !/                  +-----------------------------------+
    !/
    !/    09-nov-2020 : creation                            ( version 7.12 )
    !/    02-feb-2021 : nodefault option for global meta    ( version 7.12 )
    !/
    !
    !  1. purpose :
    !
    !     decode the meta header line.
    !
    !  2. method:
    !
    !     the internal ww3 field can be specified either as an [ifi, ifj]
    !     integer combination, or a field id tag (such as "hs").
    !
    !     both forms can also specify an optional component (ifc) integer
    !     value for multi-component fields (defaults to 1).
    !
    !     field name id tags are case-insensitive, hs == hs == hs.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       buf    char.  i  input header string (without leading meta tag)
    !       iline   int.  i  line number (for error reporting)
    !       ifi     int.  o  output group number
    !       ifj     int.  o  output field number
    !       ifc     int.  o  component number (defaults to 1)
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    use w3iogomd, only: w3fldtoij
    implicit none
    character(*), intent(in) :: buf
    integer, intent(in) :: iline
    integer, intent(out) :: ifi, ifj, ifc
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer :: ierr, i
    character(len=10) :: fld, ofld, opt
    ifi = 0
    ifj = 1
    ifc = 1
    fld = ''
    ! is first value an int?
    read(buf, *, iostat=ierr) ifi
    if(ierr .eq. 0) then
      ! try reading 3 values:
      read(buf, *, iostat=ierr) ifi, ifj, ifc
      if(ierr .ne. 0) then
        ! try just two values:
        read(buf, *, iostat=ierr) ifi, ifj
      endif
    else
      ! try reading field id plus component
      read(buf, *, iostat=ierr) fld, ifc
      if(ierr .ne. 0) then
        ! try just fldid
        read(buf, *, iostat=ierr) fld
      endif
    endif
    if(ierr .ne. 0) then
      write(ndse, 6000) trim(fn_meta), iline, trim(buf)
      call extcde(10)
    endif
    ofld = fld
    call str_to_upper(fld) ! field names are case insensitive
    ! if string value (fldid), then decode into ifi,ifj value:
    if(fld /= '') then
      ! special case for "global" attributes
      if(trim(fld) == "global") then
        if(debug) write(*,'(6x,a20,1x,a)') '[global meta sec.]', trim(buf)
        ifi = 999  ! marker for global section
        ! check for any options:
        i = index(buf, trim(ofld)) + len_trim(ofld)
        opt = adjustl(buf(i:))
        call str_to_upper(opt)
        select case(trim(opt))
        case("")
          continue ! no option
        case("nodefault")
          fl_default_gbl_meta = .false.
          if(debug) write(*,'(6x,a20,1x,a)') '[global meta]', 'defaults disabled'
        case default
          write(ndse, *) "unknown global extra option: [", trim(opt), "]"
        end select
      else
        if(debug) write(*,'(6x,a20,1x,a)') '[decoding field id]', trim(buf)
        call w3fldtoij(fld, ifi, ifj, 1, 1, 1)
      endif
    endif
    if(debug) write(*,'(6x,a20,1x,i4,2i2)') '[ifi, ifj, ifc]', ifi,ifj,ifc
    !
6000 format (/' *** wavewatch iii error in w3ounfmeta : '/           &
         '     syntax error in section header. ' /              &
         '     filename = ', a /                                &
         '     line no =', i5 /                                 &
         '  => ', a /)
    !
  end subroutine decode_header
  !/ ------------------------------------------------------------------- /
  !> @brief reads in attribute name/value pairs and updates the relevant
  !>    values in the meta type.
  !>
  !> @details keeps looping over input lines in file until next meta section
  !>    or eof is found. splits meta pairs on the = character.
  !>
  !>    note - the "extra" metadata pair can also provide a variable
  !>    type ("c", "i", or "r"; for character, int or real respectively)
  !>
  !> @param[in]      ndmi  unit number of metadata input file
  !> @param[out]     meta  pointer to meta type
  !> @param[in,out]  iline current line number in file
  !>
  !> @author chris bunney @date 09-11-2020
  !/ ------------------------------------------------------------------- /
  subroutine read_meta_pairs(ndmi, meta, iline)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           c. bunney               |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         09-nov-2020 |
    !/                  +-----------------------------------+
    !/
    !/    09-nov-2020 : creation                            ( version 7.12 )
    !/
    !
    !  1. purpose :
    !
    !     reads in attribute name/value pairs and updates the relevant
    !     values in the meta type.
    !
    !  2. method:
    !
    !     keeps looping over input lines in file until next meta section
    !     or eof is found. splits meta pairs on the = character.
    !
    !     note - the "extra" metadata pair can also provide a variable
    !     type ("c", "i", or "r"; for character, int or real respectively)
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       ndmi      int.   i  unit number of metadata input file
    !       meta  int.ptr.   o  pointer to meta type
    !       iline     int. i/o  current line number in file
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    integer, intent(in)                  :: ndmi
    type(meta_t), intent(inout), pointer :: meta
    integer, intent(inout)               :: iline
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !
    character(len=256) :: buf
    character(len=128) :: attn, attv, tmp
    character(len=16)  :: att_type!, test
    integer            :: i, ierr
    real               :: r
    logical            :: eof, new
    type(meta_pair_t)  :: extra
    ! keep reading lines until we hit eof or anoter meta keyword
    do
      call next_line(ndmi, buf, iline, eof, new_section=new)
      if(eof) then
        backspace(ndmi)
        return
      endif
      if(new) then
        if(debug) write(*,'(i5,1x,a20)') iline, '[--end of section--]'
        iline = iline - 1
        backspace(ndmi)
        exit
      endif
      if(debug) write(*,'(i5,1x,a20,1x,a)') iline, '[meta pair]', trim(buf)
      ! meta data should be formatted as "attr_name = attr_value"
      i = index(buf, "=")
      if( i .lt. 1 ) then
        write(ndse, 7000) fn_meta, iline, trim(buf)
        call extcde(10)
      endif
      attn = adjustl(buf(1:i-1))
      attv = adjustl(buf(i+1:))
      ! some compilers won't read an "empty" string unless quoted:
      if(trim(attv) == '') then
        attv='""'
      endif
      ierr = 0
      select case(trim(attn))
        ! character variables
        ! note: using internal reads will allow the use of quote marks in strings
      case("varnm")
        read(attv, *, iostat=ierr) meta%varnm
      case("ename")
        read(attv, *, iostat=ierr) meta%ename
      case("standard_name", "varns")
        read(attv, *, iostat=ierr) meta%varns
      case("long_name", "varnl")
        read(attv, *, iostat=ierr) meta%varnl
      case("globwave_name", "varng")
        read(attv, *, iostat=ierr) meta%varng
      case("direction_reference", "dir_ref", "varnd")
        read(attv, *, iostat=ierr) meta%varnd
      case("comment", "varnc")
        read(attv, *, iostat=ierr) meta%varnc
      case("units")
        read(attv, *, iostat=ierr) meta%units
        ! real variables
      case("valid_min", "vmin")
        read(attv, *, iostat=ierr) meta%vmin
      case("valid_max", "vmax")
        read(attv, *, iostat=ierr) meta%vmax
      case("scale_factor", "fsc")
        read(attv, *, iostat=ierr) meta%fsc
        ! default case will be the "extra" meta data variable
      case default
        tmp = attv
        call get_attval_type(tmp, iline, attv, att_type)
        if(debug) then
          write(*,'(i5,1x,a20,1x,a)') iline, '[meta extra]', &
               trim(attn)//' = '//trim(attv)//' (type: '//trim(att_type)//")"
        endif
        extra%attname = trim(attn)
        extra%attval = trim(attv)
        extra%type = trim(att_type)
        call meta_list_append(meta%extra, extra)
      end select
      if(ierr /= 0) then
        write(ndse, 7002) fn_meta, iline, trim(buf)
        call extcde(10)
      endif
    enddo
    return
    !
7000 format (/' *** wavewatch iii error in w3ounfmeta : '/           &
         '     syntax error in metadata file ' /                &
         '     should be "attr_name = attr_value" ' /           &
         '     filename = ', a /                                &
         '     line no =', i5 /                                 &
         '  => ', a /)
    !
7002 format (/' *** wavewatch iii error in w3ounfmeta : '/           &
         '     io error reading attribute' /                    &
         '     filename = ', a /                                &
         '     line no =', i5 /                                 &
         '  => ', a /)
    !
  end subroutine read_meta_pairs
  !/ ------------------------------------------------------------------- /
  !> @brief gets the attribute value and optional variable type from
  !>    the passed in string.
  !>
  !> @details if two freeform values can be read from the input string,
  !>    it is assumed to be a value and type, otherwise if only one value
  !>    can be read the type is assumed to be "character".
  !>
  !>    it is important to quote strings if they contain spaces.
  !>
  !>    valid types are "c" "r/f", and "i" for character, real/float and
  !>     integer values.
  !>
  !> @param[in]   buf       input string to process
  !> @param[in]   iline     line number (for error reporting)
  !> @param[out]  attv      attribute value
  !> @param[out]  att_type  attribute type
  !>
  !> @author chris bunney @date 09-nov-2020
  !/ ------------------------------------------------------------------- /
  subroutine get_attval_type(buf, iline, attv, att_type)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           c. bunney               |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         09-nov-2020 |
    !/                  +-----------------------------------+
    !/
    !/    09-nov-2020 : creation                            ( version 7.12 )
    !/
    !
    !  1. purpose :
    !
    !     gets the attribute value and optional variable type from
    !     the passed in string.
    !
    !  2. method:
    !
    !     if two freeform values can be read from the input string, it is
    !     assumed to be a value and type, otherwise if only one value can
    !     be read the type is assumed to be "character".
    !
    !     it is important to quote strings if they contain spaces.
    !
    !     valid types are "c" "r/f", and "i" for character, real/float and
    !     integer values.
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       buf      char.   i  input string to process
    !       iline    int.    i  line number (for error reporting)
    !       attv     char.   o  attribute value
    !       att_type char.   o  attribute type
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    character(*), intent(in)  :: buf
    integer, intent(in)       :: iline
    character(*), intent(out) :: attv, att_type
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !
    real :: r
    integer :: i, ierr
    ! get attribute and type (default to "c" if no type set)
    att_type = 'c'
    attv = ''
    read(buf, *, iostat=ierr) attv, att_type
    if(ierr /= 0) read(buf, *, iostat=ierr) attv
    ! check attr values are valid w.r.t. attr type
    select case(trim(att_type))
    case("i")
      read(attv, *, iostat=ierr) i
      if(ierr .ne. 0) then
        write(ndse, 8001) "integer", trim(fn_meta), iline, trim(attv)
        call extcde(10)
      endif
    case("r", "f")
      read(attv, *, iostat=ierr) r
      if(ierr .ne. 0) then
        write(ndse, 8001) "real/float", trim(fn_meta), iline, trim(attv)
        call extcde(10)
      endif
    case("c")
      ! always ok.
    case default
      write(ndse, 8002) trim(fn_meta), iline, trim(buf)
      call extcde(10)
    end select
    !
8001 format (/' *** wavewatch iii error in w3ounfmeta : '/           &
         '     value is not a valid ', a /                      &
         '     filename = ', a /                                &
         '     line no =', i5 /                                 &
         '  => ', a /)
    !
8002 format (/' *** wavewatch iii error in w3ounfmeta : '/           &
         '     attribute type should be one of [c,i,r] '/       &
         '     filename = ', a /                                &
         '     line no =', i5 /                                 &
         '  => ', a /)
    !
  end subroutine get_attval_type
  !/ ------------------------------------------------------------------- /
  !> @brief reads in freeform attribute name/value pairs.
  !>
  !> @details keeps looping over input lines in file until next section
  !>    or eof is found. splits meta pairs on the `=` character.
  !>
  !>    freeform metadata pairs can also provide a variable type
  !>    ("c", "i", or "r"; for character, int or real respectively).
  !>    string values with spaces should be quoted.
  !>
  !> @param[in]      ndmi      unit number of metadata input file
  !> @param[in,out]  iline     current line number in file
  !> @param[in,out]  metalist  a meta_list_t object to append to
  !>
  !> @author chris bunney @date 16-dec-2020
  !/ ------------------------------------------------------------------- /
  subroutine read_freeform_meta_list(ndmi, iline, metalist)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           c. bunney               |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         16-dec-2020 |
    !/                  +-----------------------------------+
    !/
    !/    16-dec-2020 : creation                            ( version 7.12 )
    !/
    !
    !  1. purpose :
    !
    !     reads in freeform attribute name/value pairs.
    !
    !  2. method:
    !
    !     keeps looping over input lines in file until next section
    !     or eof is found. splits meta pairs on the = character.
    !
    !     freeform metadata pairs can also provide a variable type
    !     ("c", "i", or "r"; for character, int or real respectively).
    !     string values with spaces should be quoted.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       ndmi     char.   i  unit number of metadata input file
    !       iline     int. i/o  current line number in file
    !       metalist type. i/o  a meta_list_t object to append to
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    integer, intent(in)               :: ndmi
    integer, intent(inout)            :: iline
    type(meta_list_t), intent(inout)  :: metalist
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    character(len=256) :: buf
    character(len=128) :: attn, attv, tmp
    character(len=16)  :: att_type
    integer            :: i, ierr
    real               :: r
    logical            :: eof, new
    type(meta_pair_t)  :: meta
    !
    ! keep reading lines until we hit eof or anoter meta keyword
    do
      call next_line(ndmi, buf, iline, eof, new_section=new)
      if(eof) then
        backspace(ndmi)
        return
      endif
      if(new) then
        if(debug) write(*,'(i5,1x,a20)') iline, '[--end of section--]'
        iline = iline - 1
        backspace(ndmi)
        exit
      endif
      ! split attr name/value pair
      i = index(buf, "=")
      if( i .lt. 1 ) then
        write(ndse, 9000) trim(fn_meta), iline, trim(buf)
        call extcde(10)
      endif
      attn = adjustl(buf(1:i-1))
      tmp = adjustl(buf(i+1:))
      ! get type, if set:
      call get_attval_type(tmp, iline, attv, att_type)
      if(debug) then
        write(*,'(i5,1x,a20,1x,a)') iline, '[freeform meta]', &
             trim(attn)//' = '//trim(attv)//' (type: '//trim(att_type)//")"
      endif
      meta%attname = trim(attn)
      meta%attval = trim(attv)
      meta%type = trim(att_type)
      call meta_list_append(metalist, meta)
    enddo
    !
9000 format (/' *** wavewatch iii error in w3ounfmeta : '/           &
         '     syntax error in metadata file ' /                &
         '     should be "attr_name = attr_value" ' /           &
         '     filename = ', a /                                &
         '     line no =', i5 /                                 &
         '  => ', a /)
    !
  end subroutine read_freeform_meta_list
  !/ ------------------------------------------------------------------- /
  !> @brief reads in metadata for the coordinate reference system (crs).
  !>
  !> @details the "grid_mapping_name" must be supplied as an attribute.
  !>
  !> @param[in]     ndmi   unit number of metadata input file
  !> @param[in,out] iline  current line number in file
  !>
  !> @author chris bunney @date 07-dec-2020
  !/ ------------------------------------------------------------------- /
  subroutine read_crs_meta(ndmi, iline)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           c. bunney               |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         07-dec-2020 |
    !/                  +-----------------------------------+
    !/
    !/    07-dec-2020 : creation                            ( version 7.12 )
    !/
    !
    !  1. purpose :
    !
    !     reads in metadata for the coordinate reference system (crs)
    !     scalar variable. the "grid_mapping_name" must be supplied as
    !     an attribute.
    !
    !  2. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       ndmi     char.   i  unit number of metadata input file
    !       iline     int. i/o  current line number in file
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    integer, intent(in)     :: ndmi
    integer, intent(inout)  :: iline
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    character(len=128) :: buf, prev_name
    integer            :: i, ierr
    prev_name = crs_name
    ! re-read header line (we only want the second field)
    read(ndmi, '(a)') buf
    read(buf, *, iostat=ierr) crs_name, crs_name
    if(ierr /= 0 ) then
      write(ndse,1000)
      write(ndse,2000) trim(fn_meta), iline, trim(buf)
      call extcde(10)
    endif
    if(debug) write(*,'(i5,1x,a20,1x,a)') iline, '[crs id]', trim(crs_name)
    if(crs_meta%n .ne. 0) then
      if(crs_is_default) then
        write(ndse,1001) trim(prev_name)
        crs_is_default = .false.
      else
        write(ndse,1002) trim(prev_name)
      endif
      write(ndse,2000) trim(fn_meta), iline, trim(buf)
      call del_meta_list(crs_meta)
    endif
    call read_freeform_meta_list(ndmi, iline, crs_meta)
    ! check that "grid_mapping_name" is defined
    if(.not.  meta_list_has_attr(crs_meta, "grid_mapping_name")) then
      write(ndse, 1003)
      write(ndse, 2000) trim(fn_meta), iline, ""
      call extcde(10)
    endif
    return
1000 format (/' *** wavewatch iii error in w3ounfmeta : '/           &
         '     error reading crs header - missing crs name?' )
    !
1001 format (/' *** warning : user defined crs section will ' /      &
         '     overide default crs definition for grid' /       &
         '     prev crs = ', a )
    !
1002 format (/' *** warning : duplicate crs section will ' /         &
         '     override previous crs definition' /              &
         '     prev crs = ', a )
    !
1003 format (/' *** wavewatch iii error in w3ounfmeta : '/           &
         '     crs section does not contain mandatory '/        &
         '     attribute "grid_mapping_name"' )
2000 format ( '     filename = ', a /                                &
         '     line no  = ', i5 /                               &
         '  => ', a /)
    !
  end subroutine read_crs_meta
  !/ ------------------------------------------------------------------- /
  !> @brief set up a default coordinate reference system for the grid
  !>
  !> @details the default coordinate reference system (crs) will be defined
  !>    based on the type of grid the model is formulated on, e.g.
  !>    regular lat-lon, rotated pole, etc.
  !>
  !> @remark see "grid mappings" section of cf conventions:
  !>  - https://cfconventions.org/data/cf-conventions/cf-conventions-1.7/build/ch05s06.html
  !>  - https://cfconventions.org/data/cf-conventions/cf-conventions-1.7/build/apf.html
  !>
  !> @author chris bunney @date 25-may-2021
  !/ ------------------------------------------------------------------- /
  subroutine default_crs_meta()
    implicit none
    type(meta_pair_t) :: meta
    if(flrtd) then
    else if(gtype .eq. ungtype) then
      !       ! what do we want for unstructure grids?
    else
      ! lat/lon grid
      crs_name = 'crs'
      call meta_list_append(crs_meta,                                &
           'grid_mapping_name', 'latitude_longitude')
      ! todo: default to a spherical earth?
      call meta_list_append(crs_meta,                                &
           'semi_major_axis', 6371000.0)
      call meta_list_append(crs_meta,                                &
           'inverse_flattening', 0.0)
    endif
  end subroutine default_crs_meta
  !/ ------------------------------------------------------------------- /
  !> @brief get the meta data for a particular field.
  !>
  !> @details the required field is specified using the group (ifi) and
  !>    field (ifj) index. optionally, the component (icomp) and partition
  !>    (ipart) numbers can be specified for vector/tensor or partitioned
  !>    parameter fields. if not specified, these default to 1.
  !>    a copy of the meta-data is returned, rather than a pointer.
  !>    this is because in the case of paritioned parameters, the metadata
  !>    will be updated with the partition number.
  !>
  !> @param[in]  ifi    output group number
  !> @param[in]  ifj    output field number
  !> @param[in]  icomp  component number (defaults to 1)
  !> @param[in]  ipart  partition number (defaults to 1)
  !>
  !> @author chris bunney @date 02-nov-2020
  !/ ------------------------------------------------------------------- /
  function getmeta(ifi, ifj, icomp, ipart) result(meta)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           c. bunney               |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         02-nov-2020 |
    !/                  +-----------------------------------+
    !/
    !/    09-nov-2020 : creation                            ( version 7.12 )
    !/
    !
    !  1. purpose :
    !
    !     returns a meta_t type containig the netcdf matadata for the
    !     requested field
    !
    !  2. method :
    !
    !     a copy of the meta-data is returned, rather than a pointer. this
    !     is because in the case of paritioned parameters, the metadata
    !     will be updated with the partition number.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       ifi     int.  i  output group number
    !       ifj     int.  i  output field number
    !       icomp   int.  i  component number (defaults to 1)
    !       ipart   int.  i  partition number (defaults to 1)
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    integer, intent(in) :: ifi, ifj
    integer, intent(in), optional :: icomp, ipart
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer :: ifp, ifc
    type(meta_t) :: meta ! not pointer as we might need to modify it
    ifc = 1
    ifp = 1
    if(present(icomp)) ifc = icomp
    if(present(ipart)) ifp = ipart
    ! error checking on size of ifj, icomp, ipart.
    if(ifi .lt. 1 .or. ifi .gt. nogrp) then
      write(ndse,1000) nogrp
      call extcde(1)
    endif
    if(ifj .lt. 1 .or. ifj .gt. noge(ifi)) then
      write(ndse,1001) noge(ifi)
      call extcde(1)
    endif
    if(ifc .lt. 1 .or. ifc .gt. 3) then
      write(ndse,1002)
      call extcde(1)
    endif
    meta = meta_deep_copy(group(ifi)%field(ifj)%meta(ifc))
    ! for partitioned data, expand in the partition number:
    if(ifi .eq. 4) then
      call add_partno(meta, ifp)
    endif
    return
1000 format (/' *** wavewatch iii error in w3ounfmeta : '            / &
         '     getmeta: ifi value should be in range 1,',i2     / )
    !
1001 format (/' *** wavewatch iii error in w3ounfmeta : '           / &
         '     getmeta: ifj value should be in range 1,',i2    / )
    !
1002 format (/' *** wavewatch iii error in w3ounfmeta : '           / &
         '     getmeta: ifc value should be in range 1,3'      / )
    !
  end function getmeta
  !/ ------------------------------------------------------------------- /
  !> @brief reads in a template section from file.
  !>
  !> @details this section defines a list of text strings that will be
  !>    used to replace a "placeholder string" when generating metadata
  !>    for partitioned parameters.
  !>
  !>    format of section is:
  !>
  !>    \code
  !>      template <placeholder_string>
  !>        value for partition ipart=0
  !>        value for partition ipart=1
  !>        value for partition ipart=2
  !>        ...
  !>        value for partition ipart=n
  !>    \endcode
  !>
  !> @param[in,out]  ndmi  unit number
  !> @param[in,out]  iline line number
  !>
  !> @author chris bunney @date 04-dec-2020
  !/ ------------------------------------------------------------------- /
  subroutine read_part_tmpl(ndmi, iline)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           c. bunney               |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         04-dec-2020 |
    !/                  +-----------------------------------+
    !/
    !/    04-dec-2020 : creation                            ( version 7.12 )
    !/
    !
    !  1. purpose :
    !
    !     reads in a template section from file.
    !     this section defines a list of text strings that will be used
    !     to replace a "placeholder string" when generating metadata for
    !     partitioned parameters.
    !
    !     format of section is:
    !
    !       template <placeholder_string>
    !         value for partition ipart=0
    !         value for partition ipart=1
    !         value for partition ipart=2
    !         ...
    !         value for partition ipart=n
    !
    !  2. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       ndmi    int.  i/o  unit number
    !       iline   int.  i/o  line number
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    integer, intent(in)               :: ndmi
    integer, intent(inout)            :: iline
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    character(len=256) :: buf, id
    integer            :: ierr
    logical            :: eof, new
    type(part_tmpl_t), pointer  :: p
    ! re-read meta line to get template string id (the 2nd field)
    read(ndmi, '(a)') buf
    read(buf, *, iostat=ierr) id, id
    if(ierr /= 0) then
      write(ndse, 1000) fn_meta, iline, buf
      call extcde(10)
    endif
    id = "<" // trim(id) // ">"
    if(debug) write(*,'(i5,1x,a20,1x,a)') iline, '[template id]', trim(id)
    ! extend list of partition template types:
    if(associated(part_tmpl)) then
      ! got to end of list
      p => part_tmpl
      do while(associated(p%next))
        p => p%next
      enddo
      allocate(p%next)
      p => p%next
    else
      allocate(part_tmpl)
      p => part_tmpl
    endif
    ! set template id and read template strings from file:
    p%tmpl = trim(id)
    allocate(p%part_text(0:noswll))
    nullify(p%next)
    p%np = 0
    do
      call next_line(ndmi, buf, iline, eof, new_section=new)
      if(eof) then
        backspace(ndmi)
        return
      endif
      if(new) then
        ! start of new meta data entry
        if(debug) write(*,'(i5,1x,a20)') iline, '[--end of section--]'
        iline = iline - 1
        backspace(ndmi)
        exit
      endif
      ! check we have not exceeded noswll
      if(p%np .gt. noswll) then
        write(*,*) "too many partition entries (noswll=",noswll,"). ignoring"
        cycle
      endif
      ! add string to array of partition text
      if(debug) then
        write(*,'(i5,1x,a20,1x,i1,1x,a)') iline, '[part template]', &
             p%np, trim(buf)
      endif
      p%part_text(p%np) = trim(adjustl(buf)) ! zero indexed
      p%np = p%np + 1
    enddo
    return
    !
1000 format (/' *** wavewatch iii error in w3ounfmeta : '/           &
         '     error reading part header - missing template id?'/ &
         '     filename = ', a /                                &
         '     line no =', i5 /                                 &
         '  => ', a /)
    !
  end subroutine read_part_tmpl
  !/ ------------------------------------------------------------------- /
  !> @brief prints the patition templates to screen (for debug use).
  !> @author chris bunney @date 04-dec-2020
  !/ ------------------------------------------------------------------- /
  subroutine print_part_tmpl()
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           c. bunney               |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         04-dec-2020 |
    !/                  +-----------------------------------+
    !/
    !/    04-dec-2020 : creation                            ( version 7.12 )
    !/
    !
    !  1. purpose :
    !
    !     prints the patition templates to screen (for debug use).
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    type(part_tmpl_t), pointer :: p
    integer :: i
    print*,'=============='
    if(.not. associated(part_tmpl)) then
      print*,'empty partition list'
      return
    endif
    p => part_tmpl
    do
      print*,p%tmpl
      do i=0,p%np - 1
        print*,'   - ',i,trim(p%part_text(i))
      enddo
      if(.not. associated(p%next)) exit
      p => p%next
    enddo
    print*,'=============='
  end subroutine print_part_tmpl
  !/ ------------------------------------------------------------------- /
  !> @brief adds partition number to meta-data.
  !>
  !> @details replaces all instances of "<ipart>" in the provided meta data
  !>     with the partition number ipart.
  !>
  !> @param[in]  meta   meta data type
  !> @param[in]  ipart  partition number
  !>
  !> @author chris bunney @date 02-nov-2020
  !/ ------------------------------------------------------------------- /
  subroutine add_partno(meta, ipart)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           c. bunney               |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         02-nov-2020 |
    !/                  +-----------------------------------+
    !/
    !/    09-nov-2020 : creation                            ( version 7.12 )
    !/
    !
    !  1. purpose :
    !
    !     adds partition number to meta-data.
    !
    !  2. method :
    !
    !     replaces all instances of "<ipart>" in the provided meta data with
    !     the partition number ipart.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       meta  meta_t  i  meta data type
    !       ipart   int.  i  partition number
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    type(meta_t), intent(inout) :: meta
    integer, intent(in) :: ipart
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    character(len=80) :: tmp
    integer :: i, j
    type(meta_pair_t), pointer :: p
    call partno_string_sub(meta%ename, ipart)
    call partno_string_sub(meta%varnm, ipart)
    call partno_string_sub(meta%varnl, ipart)
    call partno_string_sub(meta%varns, ipart)
    call partno_string_sub(meta%varng, ipart)
    call partno_string_sub(meta%varnc, ipart)
    call partno_string_sub(meta%varnd, ipart)
    if(meta%extra%n .gt. 0) then
      p => meta%extra%head
      do
        call partno_string_sub(p%attname, ipart)
        if(p%type .eq. "c") then
          call partno_string_sub(p%attval, ipart)
        endif
        if(.not. associated(p%next)) exit
        p => p%next
      enddo
    endif
  end subroutine add_partno
  !/ ------------------------------------------------------------------- /
  !> @brief performs string substition of placeholder strings with
  !>    partition number specfic values.
  !>
  !> @details the placeholder \<ipart\> is automatically replaced with the
  !>    partition number (0, 1, 2, etc).
  !>
  !>    other template placeholders can be defined in the ounfmeta.inp
  !>    file by the user.
  !>
  !> @param[in,out]  instr  input string
  !> @param[in]      ipart  partition number
  !>
  !> @author chris bunney @date 02-nov-2020
  !/ ------------------------------------------------------------------- /
  subroutine partno_string_sub(instr, ipart)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           c. bunney               |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         02-nov-2020 |
    !/                  +-----------------------------------+
    !/
    !/    09-nov-2020 : creation                            ( version 7.12 )
    !/
    !
    !  1. purpose :
    !
    !     performs string substition of placeholder strings with partition
    !     number specfic values.
    !
    !     the placeholder <ipart> is automatically replaced with the
    !     partition number (0, 1, 2, etc).
    !
    !     other template placeholders can be defined in the ounfmeta.inp
    !     file by the user.
    !
    !  2. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       instr  char.  i/o  input string
    !       ipart   int.  i    partition number
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    character(len=*), intent(inout) :: instr
    integer, intent(in) :: ipart
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer :: i, j, isn
    type(part_tmpl_t), pointer :: p
    character(len=512) :: tmpl
    isn = ipart + 1
    if(ptmeth .le. 3) then
      if (isn .gt. 5) isn = 5
    else
      if (isn .gt. 2) isn = 2
    endif
    ! set partition number (built-in ipart template)
    i = index(instr, ipart_token)
    j = i + len_trim(ipart_token)
    if(i .gt. 0) then
      write(tmpl, '(a,i1,a)') instr(1:i-1), ipart, instr(j:len(instr))
      instr = tmpl
    endif
    ! set standard name string (built-in spart template)
    i = index(instr, spart_token)
    j = i + len_trim(spart_token)
    if(i .gt. 0) then
      instr = instr(1:i-1) // trim(snamep(isn)) // instr(j:len(instr))
    endif
    ! also try underscore separated version: <spart_>
    i = index(instr, spart_token_)
    j = i + len_trim(spart_token_)
    if(i .gt. 0) then
      instr = instr(1:i-1) // trim(replace_char(snamep(isn), " ", "_")) &
           // instr(j:len(instr))
    endif
    ! merge in user defined partition templates (if any):
    if(.not. associated(part_tmpl)) return
    p => part_tmpl
    do
      i = index(instr, trim(p%tmpl))
      j = i + len_trim(p%tmpl)
      if(i .gt. 0) then
        if(ipart .ge. p%np) then
          write(ndse, 1000) trim(p%tmpl), p%np, ipart
          call extcde(10)
        endif
        instr = instr(1:i-1) // trim(p%part_text(ipart)) // instr(j:len(instr))
      endif
      ! try "underscore" version <tmpl_>:
      i = len_trim(p%tmpl)
      tmpl = p%tmpl(1:i-1) // "_>"
      i = index(instr, trim(tmpl))
      j = i + len_trim(tmpl)
      if(i .gt. 0) then
        instr = instr(1:i-1) // trim(replace_char(p%part_text(ipart), " ", "_")) &
             // instr(j:len(instr))
      endif
      if(.not. associated(p%next)) exit
      p => p%next
    enddo
    return
1000 format (/' *** wavewatch iii error in w3ounfmeta : '            / &
         '     not enough user defined entries for template'    / &
         '     template id     : ',a                            / &
         '     num entries     : ',i2                           / &
         '     reqested ipart* : ',i2                           / &
         '     (*note: ipart is zero-refernced)'                / &
         '     please update your ounfmeta.inp file.'           /)
  end subroutine partno_string_sub
  !/ ------------------------------------------------------------------- /
  !> @brief writes the meta-data entries for a variable.
  !>
  !> @details attribute pairs defined in meta are written to the netcdf
  !>    variable specificed in the varid handle.
  !>
  !>    there are two stages to the write - first all "mandatory" or
  !>    "pre-defined" attributes are written out (those defined in the
  !>    meta_t type). secondly, if there is any user-defined "extra"
  !>    freeform meta data defined, this is written out via a separate
  !>    call to write_freeform_meta_list().
  !>
  !> @param[in,out]  ncid   netcdf file id
  !> @param[in,out]  varid  netcdf variable id
  !> @param[in]      meta   meta data type
  !> @param[out]     err    error value
  !>
  !> @author chris bunney @date 02-nov-2020
  !/ ------------------------------------------------------------------- /
  subroutine write_meta(ncid, varid, meta, err)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           c. bunney               |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         02-nov-2020 |
    !/                  +-----------------------------------+
    !/
    !/    09-nov-2020 : creation                            ( version 7.12 )
    !/
    !
    !  1. purpose :
    !
    !     writes the meta-data entries for a variable.
    !
    !  2. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       ncid    int.  i/o  netcdf file id
    !       varid   int.  i/o  netcdf variable id
    !       meta    int.  i    meta data type
    !       err     int.  o    error value
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    integer, intent(in) :: ncid, varid
    type(meta_t), intent(in) :: meta
    integer, intent(out) :: err
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer :: ival, val
    real :: rval
    !/
    err = nf90_put_att(ncid, varid, 'long_name', meta%varnl)
    if(err /= nf90_noerr) return
    if(meta%varns .ne. '' .and. meta%varns .ne. unsetc) then
      err = nf90_put_att(ncid, varid, 'standard_name', meta%varns)
      if(err /= nf90_noerr) return
    endif
    if(meta%varng .ne. '' .and. meta%varng .ne. unsetc) then
      err = nf90_put_att(ncid, varid, 'globwave_name', meta%varng)
      if(err /= nf90_noerr) return
    endif
    err = nf90_put_att(ncid, varid, 'units', meta%units)
    if(err /= nf90_noerr) return
    ! fill value dependent on variable type
    if(ncvartype .eq. 2) then
      err = nf90_put_att(ncid, varid, '_fillvalue', nf90_fill_short)
    else
      err = nf90_put_att(ncid, varid, '_fillvalue', nf90_fill_float)
    end if
    if(err /= nf90_noerr) return
    err = nf90_put_att(ncid, varid, 'scale_factor', meta%fsc)
    if(err /= nf90_noerr) return
    err = nf90_put_att(ncid, varid, 'add_offset', 0.)
    if(err /= nf90_noerr) return
    ! for variables with vartype short, the valid min/max
    ! are scaled by scale_factor and converted to integers.
    ! if vartype is float, then no scaling is performed and
    ! valid min/max are written out directly as floats.
    if(ncvartype .eq. 2) then
      val = nint(meta%vmin / meta%fsc)
      err = nf90_put_att(ncid, varid,'valid_min', val)
      if(err /= nf90_noerr) return
      val = nint(meta%vmax / meta%fsc)
      err = nf90_put_att(ncid, varid,'valid_max', val)
      if(err /= nf90_noerr) return
    else
      err = nf90_put_att(ncid, varid,'valid_min', meta%vmin)
      if(err /= nf90_noerr) return
      err = nf90_put_att(ncid, varid,'valid_max', meta%vmax)
      if(err /= nf90_noerr) return
    endif
    if(meta%varnc .ne. '' .and. meta%varnc .ne. unsetc) then
      err = nf90_put_att(ncid, varid, 'comment', meta%varnc)
      if(err /= nf90_noerr) return
    endif
    if(meta%varnd .ne. '' .and. meta%varnd .ne. unsetc) then
      err = nf90_put_att(ncid, varid, 'direction_reference', meta%varnd)
      if(err /= nf90_noerr) return
    end if
    if(crs_name .ne. '' .and. crs_name .ne. unsetc) then
      err = nf90_put_att(ncid, varid, 'grid_mapping', crs_name)
      if(err /= nf90_noerr) return
    endif
    if(coords_attr .ne. '' .and. coords_attr .ne. unsetc) then
      err = nf90_put_att(ncid, varid, 'coordinates', adjustl(coords_attr))
      if(err /= nf90_noerr) return
    endif
    if (meta%extra%n .gt. 0) then
      call write_freeform_meta_list(ncid, varid, meta%extra, err)
      if(err /= nf90_noerr) return
    endif
    return
    !
  end subroutine write_meta
  !/ ------------------------------------------------------------------- /
  !> @brief writes the user meta-data entries for the global attributes.
  !>
  !> @details global meta-data is stored as a meta-data list, so this
  !>    is essentially a convenience/legacy function that calls the
  !>    write_freeform_meta_list() subroutine.
  !>
  !> @param[in]   ncid  netcdf file id
  !> @param[out]  err   error value
  !>
  !> @author chris bunney @date 09-nov-2020
  !/ ------------------------------------------------------------------- /
  subroutine write_global_meta(ncid, err)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           c. bunney               |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         09-nov-2020 |
    !/                  +-----------------------------------+
    !/
    !/    09-nov-2020 : creation                            ( version 7.12 )
    !/
    !
    !  1. purpose :
    !
    !     writes the user meta-data entries for the global attributes
    !
    !  2. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       ncid    int.  i/o  netcdf file id
    !       err     int.  o    error value
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    integer, intent(in) :: ncid
    integer, intent(out) :: err
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    call write_freeform_meta_list(ncid, nf90_global, global_meta, err)
  end subroutine write_global_meta
  !/ ------------------------------------------------------------------- /
  !> @brief writes the freeform user meta-data entries for a netcdf variable
  !>
  !> @param[in,out]  ncid      netcdf file id
  !> @param[in,out]  varid     netcdf variable id
  !> @param[in]      metalist  meta_list_t object to write
  !> @param[out]     err       error value
  !>
  !> @author chris bunney @date 16-dec-2020
  !/ ------------------------------------------------------------------- /
  subroutine write_freeform_meta_list(ncid, varid, metalist, err)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           c. bunney               |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         16-dec-2020 |
    !/                  +-----------------------------------+
    !/
    !/    16-dec-2020 : creation                            ( version 7.12 )
    !/
    !
    !  1. purpose :
    !
    !     writes the freeform user meta-data entries for a netcdf variable
    !
    !  2. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       ncid      int.  i/o  netcdf file id
    !       varid     int.  i/o  netcdf file id
    !       metalist  type. i    meta_list_t object to write
    !       err       int.  o    error value
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    integer, intent(in) :: ncid, varid
    type(meta_list_t), intent(in) :: metalist
    integer, intent(out) :: err
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer :: i, ival
    real    :: rval
    type(meta_pair_t), pointer :: p
    if(metalist%n .eq. 0) return
    p => metalist%head
    ! loop over global metadata pairs:
    do
      if (p%attname .eq. '' .or.                      &
           p%attname .eq. unsetc) cycle
      select case(p%type)
      case('i')
        read(p%attval, *) ival
        err = nf90_put_att(ncid, varid, p%attname, ival)
        if(err /= nf90_noerr) return
      case('r', 'f')
        read(p%attval, *) rval
        err = nf90_put_att(ncid, varid, p%attname, rval)
        if(err /= nf90_noerr) return
      case('c')
        err = nf90_put_att(ncid, varid, p%attname,    &
             p%attval)
        if(err /= nf90_noerr) return
      case default
        write(ndse,1000) p%type
        call extcde(10)
      end select
      if(.not. associated(p%next)) exit
      p => p%next
    enddo
    !
1000 format (/' *** wavewatch iii error in w3ounfmeta : '            / &
         '     write_freeform_meta: unknown attribute'          / &
         '     data type: ', a1         / )
    !
  end subroutine write_freeform_meta_list
  !/ ------------------------------------------------------------------- /
  !> @brief writes meta-data to the screen - for debugging purposes.
  !>
  !> @param[in]  meta   meta data type
  !>
  !> @author chris bunney @date 09-nov-2020
  !/ ------------------------------------------------------------------- /
  subroutine print_meta(meta)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           c. bunney               |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         02-nov-2020 |
    !/                  +-----------------------------------+
    !/
    !/    09-nov-2020 : creation                            ( version 7.12 )
    !/
    !
    !  1. purpose :
    !
    !     writes meta-data to the screen - for debugging purposes.
    !
    !  2. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       varid   int.  i/o  netcdf variable id
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    type(meta_t), intent(in) :: meta
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    type(meta_pair_t), pointer :: p
    write(*,*) meta%varnm
    write(*,"(a20,':',1x,a)") "standard name", trim(meta%varns)
    write(*,"(a20,':',1x,a)") "long name", trim(meta%varnl)
    write(*,"(a20,':',1x,a)") "units", trim(meta%units)
    write(*,"(a20,':',1x,a)") "globwave name", trim(meta%varng)
    write(*,"(a20,':',1x,a)") "direction conv", trim(meta%varnd)
    write(*,"(a20,':',1x,a)") "comment", trim(meta%varnc)
    write(*,"(a20,':',1x,2f12.3)") "min/max", meta%vmin, meta%vmax
    if(meta%extra%n .gt. 0) then
      p => meta%extra%head
      do
        write(*,"(a20,':',1x,a)") trim(p%attname), trim(p%attval)
        if(.not. associated(p%next)) exit
        p => p%next
      enddo
    endif
  end subroutine print_meta
  !/ ------------------------------------------------------------------- /
  !> @brief performs "deep" copy of a meta_t type.
  !>
  !> @details a "deep" copy ensures that the linked list data in the extra
  !> field is copied, rather than just copying the pointer.
  !>
  !> calls copy_meta_list() internally to copy the extra linked list.
  !>
  !> @returns a new meta_t variable
  !>
  !> @param[in]  meta   meta data structure to copy
  !>
  !> @author chris bunney @date 16-dec-2020
  !/ ------------------------------------------------------------------- /
  function meta_deep_copy(meta) result(copy)
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           c. bunney               |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         16-dec-2020 |
    !/                  +-----------------------------------+
    !/
    !/    16-dec-2020 : creation                            ( version 7.12 )
    !/
    !
    !  1. purpose :
    !
    !     performs "deep" copy of a meta_t type. this ensures that the
    !     linked list data in the extra field is copied, rather than just
    !     copying the pointer.
    !
    !  2. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       meta   meta_t.  i   meta data structure to copy
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    type(meta_t), intent(in) :: meta
    type(meta_t) :: copy
    ! shallow copy first:
    copy = meta
    ! now deep copy the extra field (is pointer)
    copy%extra = copy_meta_list(meta%extra)
  end function meta_deep_copy
  !/ ------------------------------------------------------------------- /
  !> @brief populates the default meta data for ww3_ounf output fields.
  !>
  !> @remark vmin and vmax are now set in the units of the output field.
  !>    previously, they were set with scaled values based on the scaling
  !>    factor fsc. the scaling is now performed (if necessary) in the
  !>    write_meta subroutine.
  !>
  !> @remark fsc (scale factor) is only applied to data and valid_min/max
  !>    if the netcdf variable type is nf90_short.
  !>
  !> @author chris bunney @date 22-mar-2021
  !/ ------------------------------------------------------------------- /
  subroutine default_meta()
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           c. bunney               |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    02-nov-2020 : creation                            ( version 7.12 )
    !/    22-mar-2021 : adds extra coupling fields          ( version 7.13 )
    !/
    !
    !  1. purpose :
    !
    !     populates the default meta data for ww3_ounf.
    !
    !  2. remarks :
    !
    !     vmin and vmax are now set in the units of the output field.
    !     previously, they were set with scaled values based on the scaling
    !     factor fsc. the scaling is now performed (if necessary) in the
    !     write_meta subroutine.
    !
    !     fsc (scale factor) is only applied to data and valid_min/max if
    !     the netcdf variable type is nf90_short.
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    type(meta_t), pointer :: meta(:)
    !
    !----------group 1 ----------------
    !
    ! ifi=1, ifj=1, dpt
    meta => group(1)%field(1)%meta
    meta(1)%fsc    = 0.5
    meta(1)%units  = 'm'
    meta(1)%ename  = '.dpt'
    meta(1)%varnm = 'dpt'
    meta(1)%varnl ='depth'
    meta(1)%varns ='depth'
    meta(1)%varng ='depth'
    meta(1)%varnc =''
    meta(1)%varnd =''
    meta(1)%vmin = -45000
    meta(1)%vmax = 70000
    ! ifi=1, ifj=2, cur
    meta => group(1)%field(2)%meta
    meta(1)%ename  = '.cur'
    meta(1)%varnd = dircom
    if(vector) then
      meta(1)%fsc    = 0.01
      meta(1)%units  = 'm s-1'
      meta(1)%vmin = -9.9
      meta(1)%vmax =  9.9
      meta(1)%varnm='ucur'
      meta(1)%varnl='eastward current'
      meta(1)%varns='eastward_sea_water_velocity'
      meta(1)%varng='eastward_sea_water_velocity'
      meta(1)%varnc='cur=sqrt(u**2+v**2)'
      ! second component
      meta(2) = meta(1)
      meta(2)%varnm='vcur'
      meta(2)%varnl='northward current'
      meta(2)%varns='northward_sea_water_velocity'
      meta(2)%varng='northward_sea_water_velocity'
      meta(2)%varnc='cur=sqrt(u**2+v**2)'
    else
      meta(1)%fsc    = 0.01
      meta(1)%units  = 'm s-1'
      meta(1)%vmin = 0
      meta(1)%vmax = 10.0
      meta(1)%varnm='cspd'
      meta(1)%varnl='current speed'
      meta(1)%varns='sea_water_speed'
      meta(1)%varng='sea_water_speed'
      ! second component
      meta(2) = meta(1)
      meta(2)%fsc = 0.1
      meta(2)%varnm='cdir'
      meta(2)%units= 'degrees'
      meta(2)%varnl='current direction (toward)'
      meta(2)%varns='direction_of_sea_water_velocity'
      meta(2)%varng='direction_of_sea_water_velocity'
      meta(2)%vmin = 0
      meta(2)%vmax = 360.0
    endif ! vector
    ! ifi=1, ifj=3
    meta => group(1)%field(3)%meta
    meta(1)%ename  = '.wnd'
    meta(1)%varnd = dircom
    if(vector) then
      meta(1)%fsc    = 0.1
      meta(1)%units  = 'm s-1'
      meta(1)%varnm='uwnd'
      meta(1)%varnl='eastward_wind'
      meta(1)%varns='eastward_wind'
      meta(1)%varng='eastward_wind'
      meta(1)%varnc='wind=sqrt(u10**2+v10**2)'
      meta(1)%vmin = -99.0
      meta(1)%vmax =  99.0
      ! second component
      meta(2) = meta(1)
      meta(2)%varnm='vwnd'
      meta(2)%varnl='northward_wind'
      meta(2)%varns='northward_wind'
      meta(2)%varng='northward_wind'
      meta(2)%varnc='wind=sqrt(u10**2+v10**2)'
    else
      meta(1)%fsc = 0.01
      meta(1)%units= 'm s-1'
      meta(1)%varnm='wspd'
      meta(1)%varnl='wind speed'
      meta(1)%varns='wind_speed'
      meta(1)%varng='wind_speed'
      meta(1)%vmin = 0.0
      meta(1)%vmax = 100.0
      ! second component
      meta(2) = meta(1)
      meta(2)%fsc = 0.1
      meta(2)%varnm='wdir'
      meta(2)%units='degrees'
      meta(2)%varnl='wind direction (from)'
      meta(2)%varns='wind_from_direction'
      meta(2)%varng='wind_from_direction'
      meta(2)%vmin = 0.0
      meta(2)%vmax = 360.0
    endif ! vector
    ! ifi=1, ifj=4, ast
    meta => group(1)%field(4)%meta
    meta(1)%fsc    = 0.1
    meta(1)%ename = '.ast'
    meta(1)%units = 'k'
    meta(1)%varnm='ast'
    meta(1)%varnl='air sea temperature difference'
    !meta(1)%varns='air_sea_temperature_difference'
    meta(1)%varns=''
    meta(1)%varng='air_sea_temperature_difference'
    meta(1)%vmin = -200.0
    meta(1)%vmax = 200.0
    ! ifi=1, ifj=5, wlv
    meta => group(1)%field(5)%meta
    meta(1)%fsc    = 0.01
    meta(1)%units  = 'm'
    meta(1)%ename  = '.wlv'
    meta(1)%varnm='wlv'
    meta(1)%varnl='sea surface height above sea level'
    meta(1)%varns='sea_surface_height_above_mean_sea_level'
    meta(1)%varng='sea_surface_height_above_sea_level'
    meta(1)%vmin = 0
    meta(1)%vmax = 100
    ! ifi=1, ifj=6, ice
    meta => group(1)%field(6)%meta
    meta(1)%fsc    = 0.001
    meta(1)%units  = '1'
    meta(1)%ename  = '.ice'
    meta(1)%varnm='ice'
    meta(1)%varnl='sea ice area fraction'
    meta(1)%varns='sea_ice_area_fraction'
    meta(1)%varng='sea_ice_area_fraction'
    meta(1)%vmin = 0
    meta(1)%vmax = 1
    ! ifi=1, ifj=7, ibg
    meta => group(1)%field(7)%meta
    meta(1)%fsc    = 0.0001
    meta(1)%units  = 'km-1'
    meta(1)%ename  = '.ibg'
    meta(1)%varnm='ibg'
    meta(1)%varnl='icebergs_damping'
    !meta(1)%varns='icebergs_induced_attenuation_scale_for_waves'
    meta(1)%varns=''
    meta(1)%varng='icebergs_damping'
    meta(1)%vmin = 0
    meta(1)%vmax = 3.2
    ! ifi=1, ifj=8, taua
    meta => group(1)%field(8)%meta
    meta(1)%fsc    = 0.01
    meta(1)%units  = 'pa'
    meta(1)%ename  = '.taua'
    meta(1)%varnm='utaua'
    meta(1)%varnl='surface_downward_eastward_stress'
    meta(1)%varns='surface_downward_eastward_stress'
    meta(1)%varng='surface_downward_eastward_stress'
    meta(2)%varnc='taua=sqrt(utaua**2+vtaua**2)'
    meta(1)%vmin = -320
    meta(1)%vmax =  320
    meta(1)%varnd = dircom
    ! second component
    meta(2) = meta(1)
    meta(2)%varnm='vtaua'
    meta(2)%varnl='surface_downward_northward_stress'
    meta(2)%varns='surface_downward_northward_stress'
    meta(2)%varng='surface_downward_northward_stress'
    meta(2)%varnc='taua=sqrt(utaua**2+vtaua**2)'
    ! ifi=1, ifj=9, rho
    meta => group(1)%field(9)%meta
    meta(1)%fsc    = 0.0001
    meta(1)%units  = 'kg m-3'
    meta(1)%ename  = '.rhoa'
    meta(1)%varnm='rhoa'
    meta(1)%varnl='air_density'
    meta(1)%varns='air_density'
    meta(1)%varng='air_density'
    meta(1)%vmin = 0
    meta(1)%vmax = 2
    ! ifi=1, ifj=10, d50
    ! ifi=1, ifj=11, ic1
    !
    !----------group 2 ----------------
    !
    ! ifi=2, ifj=1, hs
    meta => group(2)%field(1)%meta
    meta(1)%fsc    = 0.002
    meta(1)%units  = 'm'
    meta(1)%ename  = '.hs'
    meta(1)%varnm='hs'
    meta(1)%varnl='significant height of wind and swell waves'
    meta(1)%varns='sea_surface_wave_significant_height'
    meta(1)%varng='significant_wave_height'
    meta(1)%vmin = 0
    meta(1)%vmax = 64
    ! ifi=2, ifj=2, lm
    meta => group(2)%field(2)%meta
    meta(1)%fsc    = 1.
    meta(1)%units  = 'm'
    meta(1)%ename  = '.lm'
    meta(1)%varnm='lm'
    meta(1)%varnl='mean wave length'
    !meta(1)%varns='mean_wave_length'
    meta(1)%varns=''
    meta(1)%varng='mean_wave_length'
    meta(1)%vmin = 0
    meta(1)%vmax = 3200
    ! ifi=2, ifj=3, t02
    meta => group(2)%field(3)%meta
    meta(1)%fsc    = 0.01
    meta(1)%units  = 's'
    meta(1)%ename  = '.t02'
    meta(1)%varnm='t02'
    meta(1)%varnl='mean period t02'
    meta(1)%varns='sea_surface_wind_wave_mean_period' // &
         '_from_variance_spectral_density_second_frequency_moment'
    meta(1)%varng='mean_period_t02'
    meta(1)%vmin = 0
    meta(1)%vmax = 50
    ! ifi=2, ifj=4, t0m1
    meta => group(2)%field(4)%meta
    meta(1)%fsc    = 0.01
    meta(1)%units  = 's'
    meta(1)%ename  = '.t0m1'
    meta(1)%varnm='t0m1'
    meta(1)%varnl='mean period t0m1'
    meta(1)%varns='sea_surface_wind_wave_mean_period_from_variance' // &
         '_spectral_density_inverse_frequency_moment'
    meta(1)%varng='mean_period_t0m1'
    meta(1)%vmin = 0
    meta(1)%vmax = 50
    ! ifi=2, ifj=5, t01
    meta => group(2)%field(5)%meta
    meta(1)%fsc    = 0.01
    meta(1)%units  = 's'
    meta(1)%ename  = '.t01'
    meta(1)%varnm='t01'
    meta(1)%varnl='mean period t01'
    meta(1)%varns='sea_surface_wind_wave_mean_period_from_variance' // &
         '_spectral_density_first_frequency_moment'
    meta(1)%varng='mean_period_t01'
    meta(1)%vmin = 0
    meta(1)%vmax = 50
    ! ifi=2, ifj=6, fp
    meta => group(2)%field(6)%meta
    meta(1)%fsc    = 0.001
    meta(1)%units  = 's-1'
    meta(1)%ename  = '.fp'
    meta(1)%varnm='fp'
    meta(1)%varnl='wave peak frequency'
    !meta(1)%varns='sea_surface_wave_peak_frequency'
    meta(1)%varns=''
    meta(1)%varng='dominant_wave_frequency'
    meta(1)%vmin = 0
    meta(1)%vmax = 10
    ! ifi=2, ifj=7, dir
    meta => group(2)%field(7)%meta
    meta(1)%fsc    = 0.1
    meta(1)%units  = 'degree'
    meta(1)%ename  = '.dir'
    meta(1)%varnm='dir'
    meta(1)%varnl='wave mean direction'
    meta(1)%varns='sea_surface_wave_from_direction'
    meta(1)%varng='wave_from_direction'
    meta(1)%varnd=dircom
    meta(1)%vmin = 0
    meta(1)%vmax = 360
    ! ifi=2, ifj=8, spr
    meta => group(2)%field(8)%meta
    meta(1)%fsc    = 0.1
    meta(1)%units  = 'degree'
    meta(1)%ename  = '.spr'
    meta(1)%varnm='spr'
    meta(1)%varnl='directional spread'
    meta(1)%varns='sea_surface_wave_directional_spread'
    meta(1)%varng='directional_spread'
    meta(1)%vmin = 0
    meta(1)%vmax = 90
    ! ifi=2, ifj=9, dp
    meta => group(2)%field(9)%meta
    meta(1)%fsc    = 1.
    meta(1)%units  = 'degree'
    meta(1)%ename  = '.dp'
    meta(1)%varnm='dp'
    meta(1)%varnl='peak direction'
    meta(1)%varns='sea_surface_wave_peak_direction'
    meta(1)%varng='dominant_wave_direction'
    meta(1)%varnd=dircom
    meta(1)%vmin = 0
    meta(1)%vmax = 360
    ! ifi=2, ifj=10, hig
    meta => group(2)%field(10)%meta
    meta(1)%fsc    = 0.0002
    meta(1)%units  = 'm'
    meta(1)%ename  = '.hig'
    meta(1)%varnm='hig'
    meta(1)%varnl='infragravity_wave_height'
    !meta(1)%varns='sea_surface_wave_infragravity_significant_height'
    meta(1)%varns=''
    meta(1)%varng='infragravity_significant_wave_height'
    meta(1)%vmin = 0
    meta(1)%vmax = 1.0
    ! ifi=2, ifj=11, mxe
    meta => group(2)%field(11)%meta
    meta(1)%fsc    = 0.002
    meta(1)%units  = 'm'
    meta(1)%ename  = '.mxe'
    meta(1)%varnm='stmaxe'
    meta(1)%varnl='expected maximum sea surface elevation (nonlinear,2nd order)'
    !meta(1)%varns='expected maximum sea surface elevation (nonlinear,2nd order)'
    meta(1)%varns=''
    meta(1)%varng='expected maximum sea surface elevation (nonlinear,2nd order)'
    meta(1)%vmin = 0
    meta(1)%vmax = 64
    ! ifi=2, ifj=12, mxes
    meta => group(2)%field(12)%meta
    meta(1)%fsc    = 0.002
    meta(1)%units  = 'm'
    meta(1)%ename  = '.mxes'
    meta(1)%varnm='stmaxd'
    meta(1)%varnl='standard deviation of maximum sea surface elevation (nonlinear,2nd order)'
    !meta(1)%varns='std of expected maximum sea surface elevation (nonlinear,2nd order)'
    meta(1)%varns=''
    meta(1)%varng='standard deviation of maximum sea surface elevation (nonlinear,2nd order)'
    meta(1)%vmin = 0
    meta(1)%vmax = 64
    ! ifi=2, ifj=13, mxh
    meta => group(2)%field(13)%meta
    meta(1)%fsc    = 0.002
    meta(1)%units  = 'm'
    meta(1)%ename  = '.mxh'
    meta(1)%varnm='hmaxe'
    meta(1)%varnl='expected maximum wave height (linear, 1st order)'
    !meta(1)%varns='expected maximum wave height (linear, 1st order)'
    meta(1)%varns=''
    meta(1)%varng='expected maximum wave height (linear, 1st order)'
    meta(1)%vmin = 0
    meta(1)%vmax = 64
    ! ifi=2, ifj=14, mxhc
    meta => group(2)%field(14)%meta
    meta(1)%fsc    = 0.002
    meta(1)%units  = 'm'
    meta(1)%ename  = '.mxhc'
    meta(1)%varnm='hcmaxe'
    meta(1)%varnl='expected maximum wave height from crest (linear, 1st order)'
    !meta(1)%varns='expected maximum wave height from crest (linear, 1st order)'
    meta(1)%varns=''
    meta(1)%varng='expected maximum wave height from crest (linear, 1st order)'
    meta(1)%vmin = 0
    meta(1)%vmax = 64
    ! ifi=2, ifj=15, sdmh
    meta => group(2)%field(15)%meta
    meta(1)%fsc    = 0.002
    meta(1)%units  = 'm'
    meta(1)%ename  = '.sdmh'
    meta(1)%varnm='hmaxd'
    meta(1)%varnl='std of maximum wave height (linear, 1st order)'
    !meta(1)%varns='std of maximum wave height (linear, 1st order)'
    meta(1)%varns=''
    meta(1)%varng='std of maximum wave height (linear, 1st order)'
    meta(1)%vmin = 0
    meta(1)%vmax = 64
    ! ifi=2, ifj=16, sdmhc
    meta => group(2)%field(16)%meta
    meta(1)%fsc    = 0.002
    meta(1)%units  = 'm'
    meta(1)%ename  = '.sdmhc'
    meta(1)%varnm='hcmaxd'
    meta(1)%varnl='std of maximum wave height from crest (linear, 1st order)'
    !meta(1)%varns='std of maximum wave height from crest (linear, 1st order)'
    meta(1)%varns=''
    meta(1)%varng='std of maximum wave height from crest (linear, 1st order)'
    meta(1)%vmin = 0
    meta(1)%vmax = 64
    ! ifi=2, ifj=17, wbt
    meta => group(2)%field(17)%meta
    meta(1)%fsc    = 0.001
    meta(1)%units  = '1'
    meta(1)%ename  = '.wbt'
    meta(1)%varnm='wbt'
    meta(1)%varnl='dominant wave breaking probability'
    !meta(1)%varns='dominant_wave_breaking_probability'
    meta(1)%varns=''
    meta(1)%varng='dominant_wave_breaking_probability'
    meta(1)%vmin = 0
    meta(1)%vmax = 1
    ! ifi=2, ifj=18, tp
    meta => group(2)%field(18)%meta
    meta(1)%fsc    = 0.01
    meta(1)%units  = 's'
    meta(1)%ename  = '.tp'
    meta(1)%varnm='tp'
    meta(1)%varnl='wave peak period'
    meta(1)%varns='sea_surface_wave_peak_period'
    meta(1)%varng='dominant_wave_period'
    meta(1)%vmin = 0
    meta(1)%vmax = 50
    ! ifi=2, ifj=19
    meta => group(2)%field(19)%meta
    meta(1)%fsc    = 0.001
    meta(1)%units  = 'm-1'
    meta(1)%ename  = '.wnm'
    meta(1)%varnm='wnm'
    meta(1)%varnl='mean wave number'
    meta(1)%varns=''
    meta(1)%varng=''
    meta(1)%vmin = 0
    meta(1)%vmax = 32
    !
    !---------- group 3 ----------------
    !
    ! ifi=3, ifj=1, ef
    meta => group(3)%field(1)%meta
    meta(1)%varnm='ef'
    meta(1)%varnl='wave_elevation_spectrum'
    meta(1)%varns='sea_surface_wave_variance_spectral_density'
    if (ncvartype.le.3) then
      meta(1)%units  = 'log10(m2 s+1e-12)'
      !meta(1)%varns='base_ten_logarithm_of_power_spectral_density_of_surface_elevation'
      meta(1)%varnc='base_ten_logarithm'
      meta(1)%fsc = 0.0004
      meta(1)%vmin = -12.
      meta(1)%vmax = 12.
    else
      meta(1)%units  = 'm2 s'
      meta(1)%fsc = 1.
      meta(1)%vmin = 0.
      meta(1)%vmax = 1.e12
    end if
    meta(1)%ename  = '.ef'
    meta(1)%varng = meta(1)%varns
    ! ifi=3, ifj=2, th1m
    meta => group(3)%field(2)%meta
    ! information for spectral
    meta(1)%fsc     = 0.1
    meta(1)%varnm='th1m'
    meta(1)%varnl='mean wave direction frequency spectrum'
    !meta(1)%varns='sea_surface_wave_from_direction_frequency_spectrum'
    meta(1)%varns=''
    meta(1)%varng = meta(1)%varns
    meta(1)%varnd=dircom
    meta(1)%units  = 'degree'
    meta(1)%ename  = '.th1m'
    meta(1)%vmin = 0
    meta(1)%vmax = 360
    ! ifi=3, ifj=3, sth1m
    meta => group(3)%field(3)%meta
    ! information for spectral
    meta(1)%fsc     = 0.01
    meta(1)%varnm='sth1m'
    meta(1)%varnl='spreading frequency spectrum'
    !meta(1)%varns='sea_surface_wave_spreading_spectrum'
    meta(1)%varns=''
    meta(1)%varng = meta(1)%varns
    meta(1)%units  = 'degree'
    meta(1)%ename  = '.sth1m'
    meta(1)%vmin = 0
    meta(1)%vmax = 90
    ! ifi=3, ifj=4, th2m
    meta => group(3)%field(4)%meta
    meta(1)%fsc     = 0.1
    meta(1)%varnm='th2m'
    meta(1)%varnl='second mean wave direction frequency spectrum'
    !meta(1)%varns='sea_surface_wave_from_direction_frequency_spectrum_from_second_moments'
    meta(1)%varns=''
    meta(1)%varng = meta(1)%varns
    meta(1)%varnd=dircom
    meta(1)%units  = 'degree'
    meta(1)%ename  = '.th2m'
    meta(1)%vmin = 0
    meta(1)%vmax = 360
    ! ifi=3, ifj=5, sth2m
    meta => group(3)%field(5)%meta
    meta(1)%fsc     = 0.01
    meta(1)%varnm='sth2m'
    meta(1)%varnl='second spreading frequency spectrum'
    !meta(1)%varns='sea_surface_wave_spreading_spectrum_from_second_moments'
    meta(1)%varns=''
    meta(1)%varng = meta(1)%varns
    meta(1)%units  = 'degree'
    meta(1)%ename  = '.sth2m'
    meta(1)%vmin = 0
    meta(1)%vmax = 90
    ! ifi=3, ifj=6, wn
    meta => group(3)%field(6)%meta
    ! information for spectral
    meta(1)%fsc    = 0.001
    meta(1)%units  = 'm-1'
    meta(1)%ename  = '.wn'
    meta(1)%varnm='wn'
    meta(1)%varnl='wave numbers'
    !meta(1)%varns='wave_numbers'
    meta(1)%varns=''
    meta(1)%varng='wave_numbers'
    meta(1)%vmin = 0
    meta(1)%vmax = 32
    !
    !---------- group 4 ----------------
    !
    ! ifi=4, ifj=1, phs
    meta => group(4)%field(1)%meta
    meta(1)%fsc    = 0.002
    meta(1)%units  = 'm'
    meta(1)%ename = '.phs'// ipart_token
    meta(1)%varnm = 'phs'// ipart_token
    meta(1)%varnl = 'wave significant height partition '// ipart_token
    meta(1)%varns = 'sea_surface_'// spart_token_ //'_wave_significant_height'
    meta(1)%varng = 'significant_wave_height_partition_'// ipart_token
    meta(1)%varnc = partcom
    meta(1)%vmin = 0
    meta(1)%vmax = 64
    ! ifi=4, ifj=2, ptp
    meta => group(4)%field(2)%meta
    meta(1)%fsc = 0.01
    meta(1)%units = 's'
    meta(1)%ename = '.ptp'// ipart_token
    meta(1)%varnm = 'ptp'// ipart_token
    meta(1)%varnl = 'peak period partition '// ipart_token
    meta(1)%varns = 'sea_surface_'// spart_token_ //'_wave_period_at_variance' // &
         '_spectral_density_maximum'
    meta(1)%varng = 'dominant_wave_period_partition_'// ipart_token
    meta(1)%varnc = partcom
    meta(1)%vmin = 0
    meta(1)%vmax = 100
    ! ifi=4, ifj=3, plp
    meta => group(4)%field(3)%meta
    meta(1)%fsc = 1.
    meta(1)%units = 'm'
    meta(1)%ename = '.plp'// ipart_token
    meta(1)%varnm = 'plp'// ipart_token
    meta(1)%varnl = 'peak wave length partition '// ipart_token
    !meta(1)%varns = 'peak_wave_length_partition_'// spart_token_
    meta(1)%varns = ''
    meta(1)%varng = 'peak_wave_length_partition_'// ipart_token
    meta(1)%varnc = partcom
    meta(1)%vmin = 0
    meta(1)%vmax = 10000
    ! ifi=4, ifj=4, pdir
    meta => group(4)%field(4)%meta
    meta(1)%fsc = 0.1
    meta(1)%units = 'degree'
    meta(1)%ename =  '.pdir'// ipart_token
    meta(1)%varnm =  'pdir'// ipart_token
    meta(1)%varnl = 'wave mean direction partition '// ipart_token
    meta(1)%varns = 'sea_surface_'// spart_token_ //'_wave_from_direction'
    meta(1)%varng = 'wave_from_direction_partition_'// ipart_token
    meta(1)%varnc = partcom
    meta(1)%varnd = dircom
    meta(1)%vmin = 0
    meta(1)%vmax = 360
    ! ifi=4, ifj=5, pspr
    meta => group(4)%field(5)%meta
    meta(1)%fsc = 0.1
    meta(1)%units = 'degree'
    meta(1)%ename = '.pspr'// ipart_token
    meta(1)%varnm = 'pspr'// ipart_token
    meta(1)%varnl = 'directional spread partition '// ipart_token
    meta(1)%varns = 'sea_surface_'// spart_token_ //'_wave_diectional_spread'
    meta(1)%varng = 'directional_spread_partition_'// ipart_token
    meta(1)%varnc = partcom
    meta(1)%vmin = 0
    meta(1)%vmax = 90
    ! ifi=4, ifj=6, pws
    meta => group(4)%field(6)%meta
    meta(1)%fsc = 0.001
    meta(1)%units = '1'
    meta(1)%ename = '.pws'// ipart_token
    meta(1)%varnm = 'pws'// ipart_token
    meta(1)%varnl = 'wind sea fraction in partition '// ipart_token
    !meta(1)%varns = 'wind_sea_fraction_in_partition_'// ipart_token
    meta(1)%varns = ''
    meta(1)%varng = 'wind_sea_fraction_in_partition_'// ipart_token
    meta(1)%varnc = partcom
    meta(1)%vmin = 0
    meta(1)%vmax = 1
    ! ifi=4, ifj=7, pdp
    meta => group(4)%field(7)%meta
    meta(1)%fsc = 0.1
    meta(1)%units = 'degree'
    meta(1)%ename = '.pdp'// ipart_token
    meta(1)%varnm = 'pdp'// ipart_token
    meta(1)%varnl = 'peak direction partition '// ipart_token
    meta(1)%varns = 'sea_surface_'// spart_token_ //'_wave_from_direction_at_variance' // &
         '_spectral_density_maximum'
    meta(1)%varng = 'dominant_wave_from_direction_partition_'// ipart_token
    meta(1)%varnc = partcom
    meta(1)%varnd = dircom
    meta(1)%vmin = 0
    meta(1)%vmax = 360
    ! ifi=4, ifj=8, pqp
    meta => group(4)%field(8)%meta
    meta(1)%fsc = 0.01
    meta(1)%units = '1'
    meta(1)%ename = '.pqp'// ipart_token
    meta(1)%varnm = 'pqp'// ipart_token
    meta(1)%varnl = 'peakedness partition '// ipart_token
    !meta(1)%varns = 'sea_surface_wave_peakedness_partition_'// ipart_token
    meta(1)%varns = ''
    meta(1)%varng = 'wave_peakedness_partition_'// ipart_token
    meta(1)%varnc = partcom
    meta(1)%vmin = 0
    meta(1)%vmax = 320
    ! ifi=4, ifj=9, ppe
    meta => group(4)%field(9)%meta
    meta(1)%fsc = 0.01
    meta(1)%units = '1'
    meta(1)%ename = '.ppe'// ipart_token
    meta(1)%varnm = 'ppe'// ipart_token
    meta(1)%varnl = 'peak enhancement factor partition '// ipart_token
    !meta(1)%varns = 'wave_peak_enhancement_factor_partition_'// ipart_token
    meta(1)%varns = ''
    meta(1)%varng = 'wave_peak_enhancement_factor_partition_'// ipart_token
    meta(1)%varnc = 'jonswap peak enhancement factor; ' // partcom
    meta(1)%varnd = ''
    meta(1)%vmin = 0
    meta(1)%vmax = 320
    ! ifi=4, ifj=10, pgw
    meta => group(4)%field(10)%meta
    meta(1)%fsc = 0.0001
    meta(1)%units = 's-1'
    meta(1)%ename =  '.pgw'// ipart_token
    meta(1)%varnm = 'pgw'// ipart_token
    meta(1)%varnl = 'frequency width partition '// ipart_token
    !meta(1)%varns = 'gaussian_frequency_spread_partition_'// ipart_token
    meta(1)%varns = ''
    meta(1)%varng = 'gaussian_frequency_spread_partition_'// ipart_token
    meta(1)%varnc = 'gaussian least-square fit to ' // &
         'omni-directional spectral partition; ' // partcom
    meta(1)%vmin = 0
    meta(1)%vmax = 3.2
    ! ifi=4, ifj=11, psw
    meta => group(4)%field(11)%meta
    meta(1)%fsc = 0.0001
    meta(1)%units = '1'
    meta(1)%ename = '.psw'// ipart_token
    meta(1)%varnm = 'psw'// ipart_token
    meta(1)%varnl = 'spectral width partition '// ipart_token
    !meta(1)%varns = 'sea_surface_wave_spectral_width_partition_'// ipart_token
    meta(1)%varns = ''
    meta(1)%varng = 'wave_spectral_width_partition_'// ipart_token
    meta(1)%varnc = partcom
    meta(1)%vmin = 0
    meta(1)%vmax = 3.2
    ! ifi=4, ifj=12, ptm10
    meta => group(4)%field(12)%meta
    meta(1)%fsc = 0.01
    meta(1)%units = 's'
    meta(1)%ename = '.ptm10c'// ipart_token
    meta(1)%varnm = 'ptm10c'// ipart_token
    meta(1)%varnl = 'mean period tm10 partition '// ipart_token
    meta(1)%varns = 'sea_surface_'// spart_token_ //'_wave_mean_period_from_variance' // &
         '_spectral_density_inverse_frequency_moment'
    meta(1)%varng = 'mean_wave_period_tm10_partition_'// ipart_token
    meta(1)%varnc = partcom
    meta(1)%vmin = 0
    meta(1)%vmax = 100
    ! ifi=4, ifj=13, pt01
    meta => group(4)%field(13)%meta
    meta(1)%fsc = 0.01
    meta(1)%units = 's'
    meta(1)%ename = '.pt01c'// ipart_token
    meta(1)%varnm = 'pt01c'// ipart_token
    meta(1)%varnl = 'mean period t01 partition '// ipart_token
    meta(1)%varns = 'sea_surface_'// spart_token_ //'_wave_mean_period_from_variance' // &
         '_spectral_density_first_frequency_moment'
    meta(1)%varng = 'mean_wave_period_t01_partition_'// ipart_token
    meta(1)%varnc = partcom
    meta(1)%vmin = 0
    meta(1)%vmax = 100
    ! ifi=4, ifj=14, pt02
    meta => group(4)%field(14)%meta
    meta(1)%fsc = 0.01
    meta(1)%units = 's'
    meta(1)%ename = '.pt02c'// ipart_token
    meta(1)%varnm = 'pt02c'// ipart_token
    meta(1)%varnl = 'mean period t02 partition '// ipart_token
    meta(1)%varns = 'sea_surface_'// spart_token_ //'_wave_mean_period_from_variance' // &
         '_spectral_density_second_frequency_moment'
    meta(1)%varng = 'mean_wave_period_t02_partition_'// ipart_token
    meta(1)%varnc = partcom
    meta(1)%vmin = 0
    meta(1)%vmax = 100
    ! ifi=4, ifj=15, pep
    meta => group(4)%field(15)%meta
    meta(1)%fsc = 0.02
    meta(1)%units = 'm2 s rad-1'
    meta(1)%ename = '.pep'// ipart_token
    meta(1)%varnm = 'pep'// ipart_token
    meta(1)%varnl = 'energy at peak frequency partition '// ipart_token
    !meta(1)%varns = 'sea_surface_wave_energy_at_variance_spectral_density_maximum_partition_'// ipart_token
    meta(1)%varns = ''
    meta(1)%varng = 'wave_energy_at_variance_spectral_density_maximum_partition_'// ipart_token
    meta(1)%varnc = partcom
    meta(1)%vmin = 0
    meta(1)%vmax = 200
    ! ifi=4, ifj=16, tws
    meta => group(4)%field(16)%meta
    meta(1)%fsc = 0.001
    meta(1)%units = '1'
    meta(1)%ename = '.tws'
    meta(1)%varnm = 'tws'
    meta(1)%varnl = 'wind sea fraction'
    !meta(1)%varns = 'wind_sea_fraction'
    meta(1)%varns = ''
    meta(1)%varng = 'wind_sea_fraction'
    meta(1)%varnc = partcom
    meta(1)%vmin = 0
    meta(1)%vmax = 1
    ! ifi=4, ifj=17, pnr
    meta => group(4)%field(17)%meta
    meta(1)%fsc = 1.
    meta(1)%units = '1'
    meta(1)%ename = '.pnr'
    meta(1)%varnm = 'pnr'
    meta(1)%varnl = 'number of wave partitions'
    !meta(1)%varns = 'number_of_wave_partitions'
    meta(1)%varns = ''
    meta(1)%varng = 'number_of_wave_partitions'
    meta(1)%varnc = partcom
    meta(1)%vmin = 0
    meta(1)%vmax = 100
    !
    !---------- group 5 ----------------
    !
    ! ifi=5, ifj=1, ust
    meta => group(5)%field(1)%meta
    ! first component
    meta(1)%fsc    = 0.001
    meta(1)%ename  = '.ust'
    meta(1)%units  = 'm s-1'
    meta(1)%varnm='uust'
    meta(1)%varnl='eastward friction velocity'
    !meta(1)%varns='eastward_friction_velocity'
    meta(1)%varns=''
    meta(1)%varng='eastward_friction_velocity'
    meta(1)%varnc='ust=sqrt(uust**2+vust**2)'
    meta(1)%varnd=dircom
    meta(1)%vmin = -99.0
    meta(1)%vmax =  99.0
    ! second component
    meta(2) = meta(1)
    meta(2)%varnm='vust'
    meta(2)%varnl='northward friction velocity'
    !meta(2)%varns='northward_friction_velocity'
    meta(2)%varns=''
    meta(2)%varng='northward_friction_velocity'
    ! ifi=5, ifj=2, cha
    meta => group(5)%field(2)%meta
    meta(1)%fsc    = 1.e-5
    meta(1)%units  = '1'
    meta(1)%ename  = '.cha'
    meta(1)%varnm='cha'
    meta(1)%varnl='charnock coefficient for surface roughness length for momentum in air'
    meta(1)%varns='charnock_coefficient_for_surface_roughness_length_for_momentum_in_air'
    meta(1)%varng='charnock_coefficient'
    meta(1)%vmin = 0
    meta(1)%vmax = 0.327
    ! ifi=5, ifj=3, cge
    meta => group(5)%field(3)%meta
    meta(1)%fsc    = 0.1           !0.01
    meta(1)%units  = 'kw m-1'
    meta(1)%ename  = '.cge'
    meta(1)%varnm='cge'
    meta(1)%varnl='wave energy flux'
    !meta(1)%varns='sea_surface_wind_wave_energy_flux'
    meta(1)%varns=''
    meta(1)%varng='wave_energy_flux'
    meta(1)%vmin = 0
    meta(1)%vmax = 999
    ! ifi=5, ifj=4, faw
    meta => group(5)%field(4)%meta
    meta(1)%fsc    = 0.1
    meta(1)%units  = 'w m-2'
    meta(1)%ename  = '.faw'
    meta(1)%varnm='faw'
    meta(1)%varnl='wind to wave energy flux'
    meta(1)%varns='wind_mixing_energy_flux_into_sea_water'
    meta(1)%varng='wind_to_wave_energy_flux'
    meta(1)%vmin = 0
    meta(1)%vmax = 999
    ! ifi=5, ifj=5, taw
    meta => group(5)%field(5)%meta
    ! first component
    meta(1)%fsc    = 0.000001
    meta(1)%units  = 'm2 s-2'
    meta(1)%ename  = '.taw'
    meta(1)%varnm='utaw'
    meta(1)%varnl='eastward wave supported wind stress'
    !meta(1)%varns='eastward_wave_supported_wind_stress'
    meta(1)%varns=''
    meta(1)%varnc='taw=sqrt(utaw**2+vtaw**2)'
    meta(1)%varng='eastward_wave_supported_wind_stress'
    meta(1)%varnd=dircom
    meta(1)%vmin = -0.032
    meta(1)%vmax =  0.032
    ! second component
    meta(2) = meta(1)
    meta(2)%varnm='vtaw'
    meta(2)%varnl='northward wave supported wind stress'
    !meta(2)%varns='northward_wave_supported_wind_stress'
    meta(2)%varns=''
    meta(2)%varng='northward_wave_supported_wind_stress'
    meta(2)%varnc='taw=sqrt(utaw**2+vtaw**2)'
    ! ifi=5, ifj=6, twa
    meta => group(5)%field(6)%meta
    ! first component
    meta(1)%fsc    = 0.0001
    meta(1)%ename  = '.twa'
    meta(1)%units  = 'm2 s-2'
    meta(1)%varnm='utwa'
    meta(1)%varnl='eastward wave to wind stress'
    !meta(1)%varns='eastward_wave_to_wind_stress'
    meta(1)%varns=''
    meta(1)%varng='eastward_wave_to_wind_stress'
    meta(1)%varnc='twa=sqrt(utwa**2+vtwa**2)'
    meta(1)%varnd=dircom
    meta(1)%vmin = -3.2
    meta(1)%vmax =  3.2
    ! second component
    meta(2) = meta(1)
    meta(2)%varnm='vtwa'
    meta(2)%varnl='northward wave to wind stress'
    !meta(2)%varns='northward_wave_to_wind_stress'
    meta(2)%varns=''
    meta(2)%varng='northward_wave_to_wind_stress'
    meta(2)%varnc='twa=sqrt(utwa**2+vtwa**2)'
    ! ifi=5, ifj=7, wcc
    meta => group(5)%field(7)%meta
    meta(1)%fsc    = 0.0001
    meta(1)%units  = '1'
    meta(1)%ename  = '.wcc'
    meta(1)%varnm='wcc'
    meta(1)%varnl='whitecap coverage'
    !meta(1)%varns='whitecap_coverage'
    meta(1)%varns=''
    meta(1)%varng='whitecap_coverage'
    meta(1)%varnc=''
    meta(1)%varnd=''
    meta(1)%vmin = 0
    meta(1)%vmax = 1
    ! ifi=5, ifj=8, wcf
    meta => group(5)%field(8)%meta
    meta(1)%fsc    = 0.001
    meta(1)%units  = 'm'
    meta(1)%ename  = '.wcf'
    meta(1)%varnm='wcf'
    meta(1)%varnl='whitecap foam thickness'
    !meta(1)%varns='whitecap_foam_thickness'
    meta(1)%varns=''
    meta(1)%varng='whitecap_foam_thickness'
    meta(1)%vmin = 0
    meta(1)%vmax = 10
    ! ifi=5, ifj=9, wch
    meta => group(5)%field(9)%meta
    meta(1)%fsc    = 0.002
    meta(1)%units  = 'm'
    meta(1)%ename  = '.wch'
    meta(1)%varnm='wch'
    meta(1)%varnl='significant breaking wave height'
    !meta(1)%varns='significant_breaking_wave_height'
    meta(1)%varns=''
    meta(1)%varng='significant_breaking_wave_height'
    meta(1)%vmin = 0
    meta(1)%vmax = 64
    ! ifi=5, ifj=10, wcm
    meta => group(5)%field(10)%meta
    meta(1)%fsc    = 0.0001
    meta(1)%units  = '1'
    meta(1)%ename  = '.wcm'
    meta(1)%varnm='wcm'
    meta(1)%varnl='whitecap moment'
    !meta(1)%varns='whitecap_moment'
    meta(1)%varns=''
    meta(1)%varng='whitecap_moment'
    meta(1)%vmin = 0
    meta(1)%vmax = 1
    ! ifi=5, ifj=11, fws
    meta => group(5)%field(11)%meta
    meta(1)%fsc    = 0.002
    meta(1)%units  = 's'
    meta(1)%ename  = '.fws'
    meta(1)%varnm='fws'
    meta(1)%varnl='wind_sea_mean_period_t0m1'
    meta(1)%varns='sea_surface_wind_wave_mean_period_from_variance' // &
         '_spectral_density_inverse_frequency_moment'
    meta(1)%varng='wind_sea_mean_period_t0m1'
    meta(1)%varnc=''
    meta(1)%varnd=''
    meta(1)%vmin = 0
    meta(1)%vmax = 64
    !
    !---------- group 6 ----------------
    !
    ! ifi=6, ifj=1, sxy
    meta => group(6)%field(1)%meta
    meta(1)%fsc  = 10.
    meta(1)%units = 'n m-1'
    meta(1)%ename = '.sxy'
    meta(1)%varnd = dircom
    meta(1)%vmin = -30000
    meta(1)%vmax = 30000
    ! first component
    meta(1)%varnm='sxx'
    meta(1)%varnl='radiation stress component sxx'
    !meta(1)%varns='radiation_stress_component_sxx'
    meta(1)%varns=''
    ! s6cond component
    meta(2) = meta(1)
    meta(2)%varnm='syy'
    meta(2)%varnl='radiation stress component syy'
    !meta(2)%varns='radiation_stress_component_syy'
    meta(2)%varns=''
    ! third component
    meta(3) = meta(1)
    meta(3)%fsc = 1.
    meta(3)%varnm='sxy'
    meta(3)%varnl='radiation stress component sxy'
    !meta(3)%varns='radiation_stress_component_sxy'
    meta(3)%varns=''
    ! ifi=6, ifj=2, two
    meta => group(6)%field(2)%meta
    meta(1)%fsc    = 0.000001
    meta(1)%units  = 'm2 s-2'
    meta(1)%ename  = '.two'
    meta(1)%vmin = -0.032
    meta(1)%vmax =  0.032
    meta(1)%varnd = dircom
    ! first component
    meta(1)%varnm='utwo'
    meta(1)%varnl='eastward wave to ocean stress'
    !meta(1)%varns='eastward_wave_to_ocean_stress'
    meta(1)%varns=''
    meta(1)%varng='eastward_wave_to_ocean_stress'
    meta(1)%varnc='two=sqrt(utwo**2+vtwo**2)'
    ! second component
    meta(2) = meta(1)
    meta(2)%varnm='vtwo'
    meta(2)%varnl='northward wave to ocean stress'
    !meta(2)%varns='northward_wave_to_ocean_stress'
    meta(2)%varns=''
    meta(2)%varng='northward_wave_to_ocean_stress'
    meta(2)%varnc='two=sqrt(utwo**2+vtwo**2)'
    ! ifi=6, ifj=3, bhd
    meta => group(6)%field(3)%meta
    meta(1)%fsc    = 0.1
    meta(1)%units  = 'm2 s-2'
    meta(1)%ename  = '.bhd'
    meta(1)%varnm='bhd'
    meta(1)%varnl='radiation pressure (bernouilli head)'
    !meta(1)%varns='radiation_pressure'
    meta(1)%varns=''
    meta(1)%varng='radiation_pressure'
    meta(1)%vmin = 0
    meta(1)%vmax = 100
    ! ifi=6, ifj=4, foc
    meta => group(6)%field(4)%meta
    meta(1)%fsc    = 0.1
    meta(1)%units  = 'w m-2'
    meta(1)%ename  = '.foc'
    meta(1)%varnm='foc'
    meta(1)%varnl='wave to ocean energy flux'
    !meta(1)%varns='wave_to_ocean_energy_flux'
    meta(1)%varns=''
    meta(1)%varng='wave_to_ocean_energy_flux'
    meta(1)%vmin = 0
    meta(1)%vmax = 999
    ! ifi=6, ifj=5, tus
    meta => group(6)%field(5)%meta
    meta(1)%fsc    = 0.001
    meta(1)%units  = 'm2 s-1'
    meta(1)%ename  = '.tus'
    meta(1)%varnd = dircom
    meta(1)%vmin = -32.0 ! c hansen: the former values of +-9.9 might be
    meta(1)%vmax = 32.0  ! exceeded more frequently in real storms
    ! first component
    meta(1)%varnm='utus'
    meta(1)%varnl='eastward stokes transport'
    !meta(1)%varns='eastward_stokes_transport'
    meta(1)%varns=''
    meta(1)%varng='eastward_stokes_transport'
    meta(1)%varnc='tus=sqrt(utus**2+vtus**2)'
    ! second component
    meta(2) = meta(1)
    meta(2)%varnm='vtus'
    meta(2)%varnl='northward stokes transport'
    !meta(2)%varns='northward_stokes_transport'
    meta(2)%varns=''
    meta(2)%varng='northward_stokes_transport'
    meta(2)%varnc='tus=sqrt(utus**2+vtus**2)'
    ! ifi=6, ifj=6, uss
    meta => group(6)%field(6)%meta
    meta(1)%fsc    = 0.0005
    meta(1)%units  = 'm s-1'
    meta(1)%ename  = '.uss'
    ! first component
    meta(1)%varnm='uuss'
    meta(1)%varnl='eastward surface stokes drift'
    meta(1)%varns='sea_surface_wave_stokes_drift_eastward_velocity'
    meta(1)%varng='eastward_surface_stokes_drift'
    meta(1)%varnc='uss=sqrt(uuss**2+vuss**2)'
    meta(1)%varnd=dircom
    meta(1)%vmin = -4.95
    meta(1)%vmax =  4.95
    ! second component
    meta(2) = meta(1)
    meta(2)%varnm='vuss'
    meta(2)%varnl='northward surface stokes drift'
    meta(2)%varns='sea_surface_wave_stokes_drift_northward_velocity'
    meta(2)%varng='northward_surface_stokes_drift'
    write(meta(2)%varnc,'(a,f8.4,a,f8.4,a)') 'frequency range ',sig(1)*tpiinv,' to ',sig(nk)*tpiinv,' hz'
    ! ifi=6, ifj=7, p2s
    meta => group(6)%field(7)%meta
    meta(1)%fsc    = 0.01
    meta(1)%ename  = '.p2s'
    meta(1)%units  = 'm4'
    meta(1)%vmin = -150
    meta(1)%vmax = 320
    ! first component
    meta(1)%varnl='power spectral density of equivalent surface pressure'
    !meta(1)%varns='power_spectral_density_of_equivalent_surface_pressure'
    meta(1)%varns=''
    meta(1)%varng='power_spectral_density_of_equivalent_surface_pressure'
    meta(1)%varnm='fp2s'
    ! second component
    meta(2) = meta(1)
    meta(2)%varnm='pp2s'
    meta(2)%units= 's-1'
    meta(2)%varnl='peak period of power spectral density of equivalent surface pressure'
    !meta(2)%varns='peak_period_of_power_spectral_density_of_equivalent_surface_pressure'
    meta(2)%varns=''
    meta(2)%varng='peak_period_of_power_spectral_density_of_equivalent_surface_pressure'
    ! ifi=6, ifj=8, usf
    meta => group(6)%field(8)%meta
    meta(1)%units = 'm s-1 hz-1'
    meta(1)%fsc = 0.0005
    meta(1)%ename = '.usf'
    meta(1)%vmin = -4.95
    meta(1)%vmax =  4.95
    meta(1)%varnd = dircom
    ! first component
    meta(1)%varnm='uusf'
    meta(1)%varnl='eastward spectral variance of surface stokes drift'
    !meta(1)%varns='eastward_spectral_variance_of_surface_stokes_drift'
    meta(1)%varns=''
    meta(1)%varnc='usf=sqrt(uusf**2+vusf**2)'
    meta(1)%varng='eastward_spectral_variance_of_surface_stokes_drift'
    ! second component
    meta(2) = meta(1)
    meta(2)%varnm='vusf'
    meta(2)%varnl='northward spectral variance of surface stokes drift'
    !meta(2)%varns='northward_spectral_variance_of_surface_stokes_drift'
    meta(2)%varns=''
    meta(2)%varng='northward_spectral_variance_of_surface_stokes_drift'
    meta(2)%varnc='usf=sqrt(uusf**2+vusf**2)'
    ! ifi=6, ifj=9, p2l
    meta => group(6)%field(9)%meta
    ! information for spectral microseismic generation data (2nd file)
    meta(1)%fsc = 0.0004
    meta(1)%varnm='p2l'
    meta(1)%varnl='base ten logarithm of power spectral density of equivalent surface pressure'
    !meta(1)%varns='base_ten_logarithm_of_power_spectral_density_of_equivalent_surface_pressure'
    meta(1)%varns=''
    meta(1)%varng='base_ten_logarithm_of_power_spectral_density_of_equivalent_surface_pressure'
    if (ncvartype.eq.2) then
      meta(1)%units='log10(pa2 m2 s+1e-12)'
      meta(1)%vmin = -12.
      meta(1)%vmax = 12.
    else
      meta(1)%units='pa2 m2 s'
      meta(1)%varnl='power spectral density of equivalent surface pressure'
      !meta(1)%varns='power_spectral_density_of_equivalent_surface_pressure'
      meta(1)%varng='power_spectral_density_of_equivalent_surface_pressure'
      meta(1)%vmin = 0.
      meta(1)%vmax = 1.e12
    endif
    meta(1)%varnc=''
    meta(1)%varnd=''
    meta(1)%ename='.p2l'
    ! ifi=6, ifj=10, twi
    meta => group(6)%field(10)%meta
    meta(1)%fsc = 0.000001
    meta(1)%units = 'm2 s-2'
    meta(1)%ename = '.tic'
    meta(1)%vmin = -0.032
    meta(1)%vmax =  0.032
    meta(1)%varnd = dircom
    ! first component
    meta(1)%varnl='eastward wave to sea ice stress'
    meta(1)%varnm='utic'
    !meta(1)%varns='eastward_wave_to_sea_ice_stress'
    meta(1)%varns=''
    meta(1)%varng='eastward_wave_to_sea_ice_stress'
    meta(1)%varnc='two=sqrt(utwo**2+vtwo**2)'
    ! second component
    meta(2) = meta(1)
    meta(2)%varnm='vtic'
    meta(2)%varnl='northward wave to sea ice stress'
    !meta(2)%varns='northward_wave_to_sea_ice_stress'
    meta(2)%varns=''
    meta(2)%varng='northward_wave_to_sea_ice_stress'
    meta(2)%varnc='two=sqrt(utwo**2+vtwo**2)'
    ! ifi=6, ifj=11, fic
    meta => group(6)%field(11)%meta
    meta(1)%fsc    = 0.1
    meta(1)%units  = 'w m-2'
    meta(1)%ename  = '.fic'
    meta(1)%varnm='fic'
    meta(1)%varnl='wave to sea ice energy flux'
    !meta(1)%varns='wave_to_sea_ice_energy_flux'
    meta(1)%varns=''
    meta(1)%varng='wave_to_sea_ice_energy_flux'
    meta(1)%vmin = 0
    meta(1)%vmax = 999
    ! ifi=6, ifj=12, usp
    meta => group(6)%field(12)%meta
    meta(1)%units   = 'm s-1'
    meta(1)%fsc    = 0.0005
    meta(1)%ename  = '.usp'
    meta(1)%varnd  = dircom
    meta(1)%vmin = -9.99
    meta(1)%vmax = 9.98
    ! first component
    meta(1)%varnm='ussp'
    meta(1)%varnl='eastward partitioned surface stokes drift'
    !meta(1)%varns='eastward_partitioned_surface_stokes_drift'
    meta(1)%varns=''
    meta(1)%varng='eastward_partitioned_surface_stokes_drift'
    meta(1)%varnc='usp=sqrt(ussp**2+vssp**2)'
    ! second component
    meta(2) = meta(1)
    meta(2)%varnm='vssp'
    meta(2)%varnl='northward partitioned surface stokes drift'
    !meta(2)%varns='northward_partitioned_surface_stokes_drift'
    meta(2)%varns=''
    meta(2)%varng='northward_partitioned_surface_stokes_drift'
    meta(2)%varnc='usp=sqrt(ussp**2+vssp**2)'
    ! ifi=6, ifj=13
    meta => group(6)%field(13)%meta
    meta(1)%units  = 'pa'
    meta(1)%fsc    = 0.01
    meta(1)%ename  = '.toc'
    meta(1)%vmin   = -320
    meta(1)%vmax   =  320
    meta(1)%varnd  = dircom
    ! first component
    meta(1)%varnm='utoc'
    meta(1)%varnl='eastward total wave to ocean stres'
    meta(1)%varns=''
    meta(1)%varng=''
    meta(1)%varnc='toc=sqrt(utoc**2+vtoc**2)'
    ! second component
    meta(2) = meta(1)
    meta(2)%varnm='vtoc'
    meta(2)%varnl='northward total wave to ocean stres'
    meta(2)%varns=''
    meta(2)%varng=''
    meta(2)%varnc='toc=sqrt(utoc**2+vtoc**2)'
    !
    !---------- group 7 ----------------
    !
    ! ifi=7, ifj=1, abr
    meta => group(7)%field(1)%meta
    meta(1)%fsc    = 0.01
    meta(1)%ename  = '.abr'
    meta(1)%units  = 'm'
    meta(1)%vmin = -180
    meta(1)%vmax = 180
    meta(1)%varnd = dircom
    ! first component
    meta(1)%varnm='uabr'
    meta(1)%varnl='rms of bottom displacement amplitude zonal'
    !meta(1)%varns='rms_of_bottom_displacement_amplitude_zonal'
    meta(1)%varns=''
    meta(1)%varng='rms_of_bottom_displacement_amplitude_zonal'
    meta(1)%varnc='abr=sqrt(uabr**2+vabr**2)'
    ! second component
    meta(2) = meta(1)
    meta(2)%varnm='vabr'
    meta(2)%varnl='rms of bottom displacement amplitude meridional'
    !meta(2)%varns='rms_of_bottom_displacement_amplitude_meridional'
    meta(2)%varns=''
    meta(2)%varng='rms_of_bottom_displacement_amplitude_meridional'
    meta(2)%varnc='abr=sqrt(uabr**2+vabr**2)'
    ! ifi=7, ifj=2, ubr
    meta => group(7)%field(2)%meta
    meta(1)%fsc    = 0.01
    meta(1)%ename  = '.ubr'
    meta(1)%units  = 'm s-1'
    meta(1)%vmin = -180
    meta(1)%vmax = 180
    meta(1)%varnd = dircom
    ! first component
    meta(1)%varnm='uubr'
    meta(1)%varnl='rms of bottom velocity amplitude zonal'
    !meta(1)%varns='rms_of_bottom_velocity_amplitude_zonal'
    meta(1)%varns=''
    meta(1)%varng='rms_of_bottom_velocity_amplitude_zonal'
    meta(1)%varnc='ubr=sqrt(uubr**2+vubr**2)'
    ! second component
    meta(2) = meta(1)
    meta(2)%varnm='vubr'
    meta(2)%varnl='rms of bottom velocity amplitude meridional'
    !meta(2)%varns='rms_of_bottom_velocity_amplitude_meridional'
    meta(2)%varns=''
    meta(2)%varng='rms_of_bottom_velocity_amplitude_meridional'
    ! ifi=7, ifj=3, bed
    meta => group(7)%field(3)%meta
    meta(1)%fsc    = 0.001
    meta(1)%units  = 'm'
    meta(1)%ename  = '.bed'
    meta(1)%vmin = 0
    meta(1)%vmax = 30
    meta(1)%varnd = dircom
    ! first component
    meta(1)%varnm='bed'
    meta(1)%varnl='bottom roughness'
    !meta(1)%varns='sea bottom roughness length'
    meta(1)%varns=''
    meta(1)%varng='ripple_wavelength'
    meta(1)%varnc='ripple_length=sqrt(ripplex**2+rippley**2)'
    ! second component
    meta(2) = meta(1)
    meta(2)%varnm='ripplex'
    meta(2)%varnl='eastward sea bottom ripple wavelength'
    !meta(2)%varns='eastward_ripple_wavelength'
    meta(2)%varns=''
    meta(2)%varng='eastward_ripple_wavelength'
    meta(2)%varnc='ripple_length=sqrt(ripplex**2+rippley**2)'
    ! third component
    meta(3) = meta(1)
    meta(3)%varnm='rippley'
    meta(3)%varnl='northward sea bottom ripple wavelength'
    !meta(3)%varns='northward_ripple_wavelength'
    meta(3)%varns=''
    meta(3)%varng='northward_ripple_wavelength'
    meta(3)%varnc='ripple_length=sqrt(ripplex**2+rippley**2)'
    ! ifi=7, ifj=4, fbb
    meta => group(7)%field(4)%meta
    meta(1)%fsc    = 0.1
    meta(1)%units  = 'w m-2'
    meta(1)%ename  = '.fbb'
    meta(1)%varnm='fbb'
    meta(1)%varnl='wave dissipation in bbl'
    !meta(1)%varns='wave_energy_dissipation_in_bottom_boundary_layer'
    meta(1)%varns=''
    meta(1)%varng='wave_dissipation_in_bbl'
    meta(1)%vmin = 0
    meta(1)%vmax = 999
    ! ifi=7, ifj=5, tbb
    meta => group(7)%field(5)%meta
    meta(1)%fsc    = 0.000001
    meta(1)%units  = 'm2 s-2'
    meta(1)%ename  = '.tbb'
    meta(1)%vmin = -0.032
    meta(1)%vmax =  0.032
    meta(1)%varnd = dircom
    ! first component
    meta(1)%varnm='utbb'
    meta(1)%varnl='eastward wave to bbl stress'
    !meta(1)%varns='eastward_wave_to_bottom_boundary_layer_stress'
    meta(1)%varns=''
    meta(1)%varng='eastward_wave_to_bbl_stress'
    meta(1)%varnc='tbb=sqrt(utbb**2+vtbb**2)'
    ! second component
    meta(2) = meta(1)
    meta(2)%varnm='vtbb'
    meta(2)%varnl='northward wave to bbl stress'
    !meta(2)%varns='northward_wave_to_bottom_boundary_layer_stress'
    meta(2)%varns=''
    meta(2)%varng='northward_wave_to_bbl_stress'
    meta(2)%varnc='tbb=sqrt(utbb**2+vtbb**2)'
    !
    !---------- group 8 ----------------
    ! ifi=8, ifj=1, mss
    meta => group(8)%field(1)%meta
    meta(1)%fsc    = 0.00001
    meta(1)%ename  = '.mss'
    meta(1)%units  = '1'
    meta(1)%vmin = 0
    meta(1)%vmax = 0.3
    meta(1)%varnd = dircom
    write(meta(1)%varnc,'(a,f8.4,a,f8.4,a)') 'frequency range ',sig(1)*tpiinv,' to ',sig(nk)*tpiinv,' hz'
    ! first component
    meta(1)%varnm='mssu'
    meta(1)%varnl='downwave mean square slope'
    meta(1)%varns='sea_surface_wave_mean_square_upwave_slope'
    meta(1)%varng='x_mean_square_slope'
    meta(1)%varnc='mss=mssu+mssc'
    ! second component
    meta(2) = meta(1)
    meta(2)%varnm='mssc'
    meta(2)%varnl='crosswave mean square slope'
    meta(2)%varns='sea_surface_wave_mean_square_crosswave_slope'
    meta(2)%varng='y_mean_square_slope'
    ! ifi=8, ifj=2, msc
    meta => group(8)%field(2)%meta
    meta(1)%fsc    = 1e-7
    meta(1)%ename  = '.msc'
    meta(1)%units  = '1'
    meta(1)%vmin = 0
    meta(1)%vmax = 0.003
    meta(1)%varnd = dircom
    ! first component
    meta(1)%varnm='mscx'
    meta(1)%varnl='eastward phillips constant'
    !meta(1)%varns='eastward_phillips_constant'
    meta(1)%varns=''
    meta(1)%varng='eastward_phillips_constant'
    meta(1)%varnc='msc=mscx+mscy'
    ! second component
    meta(2) = meta(1)
    meta(2)%varnm='mscy'
    meta(2)%varnl='northward phillips constant'
    !meta(2)%varns='northward_phillips_constant'
    meta(2)%varns=''
    meta(2)%varng='northward_phillips_constant'
    meta(2)%varnc='msc=mscx+mscy'
    ! ifi=8, ifj=3, msd
    meta => group(8)%field(3)%meta
    meta(1)%fsc    = 0.1
    meta(1)%units  = 'degree'
    meta(1)%ename  = '.msd'
    meta(1)%varnm='mssd'
    meta(1)%varnl='u direction for mss'
    meta(1)%varns='sea_surface_mean_square_upwave_slope_direction'
    meta(1)%varng='sea_surface_wave_dominant_mean_square_slope_direction'
    write(meta(1)%varnc,'(a,f8.4,a,f8.4,a)') 'frequency range ',sig(1)*tpiinv,' to ',sig(nk)*tpiinv,' hz'
    meta(1)%varnd = dircom
    meta(1)%vmin = 0
    meta(1)%vmax = 360
    ! ifi=8, ifj=4, mcd
    meta => group(8)%field(4)%meta
    meta(1)%fsc    = 0.1
    meta(1)%units  = 'degree'
    meta(1)%ename  = '.mcd'
    meta(1)%varnm='mscd'
    meta(1)%varnl='x direction for msc'
    !meta(1)%varns='sea_surface_wave_dominant_mean_square_slope_direction_in_highest_frequency'
    meta(1)%varns=''
    meta(1)%varng='sea_surface_wave_dominant_mean_square_slope_direction_in_highest_frequency'
    meta(1)%varnd = dircom
    meta(1)%vmin = 0
    meta(1)%vmax = 360
    ! ifi=8, ifj=5, qp
    meta => group(8)%field(5)%meta
    meta(1)%fsc    = 0.001
    meta(1)%units  = '1'
    meta(1)%ename  = '.qp'
    meta(1)%varnm='qp'
    meta(1)%varnl='peakedness'
    !meta(1)%varns='sea_surface_wave_peakedness'
    meta(1)%varns=''
    meta(1)%varng='wave_peakedness'
    meta(1)%varnc='goda wave peakedness parameter'
    meta(1)%vmin = 0
    meta(1)%vmax = 32
    ! ifi=8, ifj=6, qkk
    meta => group(8)%field(6)%meta
    meta(1)%fsc    = 0.05
    meta(1)%units  = 'm/rad'
    meta(1)%ename  = '.qkk'
    meta(1)%varnm='qkk'
    meta(1)%varnl='k-peakedness'
    !meta(1)%varns='sea_surface_wave_peakedness'
    meta(1)%varns=''
    meta(1)%varng='wavenumber_peakedness'
    meta(1)%varnc='2d wavenumber peakedness'
    meta(1)%vmin = 0
    meta(1)%vmax = 1600
    ! ifi=8, ifj=7, skw
    meta => group(8)%field(7)%meta
    meta(1)%fsc    = 0.00001
    meta(1)%units  = '1'
    meta(1)%ename  = '.skw'
    meta(1)%varnm='skw'
    meta(1)%varnl='skewness'
    !meta(1)%varns='sea_surface_wave_peakedness'
    meta(1)%varns=''
    meta(1)%varng='skewness of p(z,sx,sy=0)'
    meta(1)%varnc='skewness of p(z,sx,sy=0)'
    meta(1)%vmin = 0
    meta(1)%vmax = 1
    ! ifi=8, ifj=8, emb
    meta => group(8)%field(8)%meta
    meta(1)%fsc    = 0.00001
    meta(1)%units  = '1'
    meta(1)%ename  = '.emb'
    meta(1)%varnm='emb'
    meta(1)%varnl='em-bias'
    !meta(1)%varns='sea_surface_wave_peakedness'
    meta(1)%varns=''
    meta(1)%varng='em bias coefficient'
    meta(1)%varnc='em bias coefficient'
    meta(1)%vmin = -1
    meta(1)%vmax = 1    
    ! ifi=8, ifj=7, skw
    meta => group(8)%field(9)%meta
    meta(1)%fsc    = 0.00001
    meta(1)%units  = '1'
    meta(1)%ename  = '.emc'
    meta(1)%varnm='emc'
    meta(1)%varnl='trackerbias'
    !meta(1)%varns='sea_surface_wave_peakedness'
    meta(1)%varns=''
    meta(1)%varng='tracker bias coefficient'
    meta(1)%varnc='tracker bias coefficient'
    meta(1)%vmin = -1
    meta(1)%vmax = 1    !
    !
    !---------- group 9 ----------------
    !
    ! ifi=9, ifj=1, dtd
    meta => group(9)%field(1)%meta
    meta(1)%fsc    = 0.1
    meta(1)%units  = 'min.'
    meta(1)%ename  = '.dtd'
    meta(1)%varnm='dtd'
    meta(1)%varnl='dynamic time step'
    !meta(1)%varns='dynamic_time_step'
    meta(1)%varns=''
    meta(1)%varng='dynamic_time_step'
    meta(1)%vmin = 0
    meta(1)%vmax = 3200
    ! ifi=9, ifj=2, fc
    meta => group(9)%field(2)%meta
    meta(1)%fsc    = 0.001
    meta(1)%units  = 's-1'
    meta(1)%ename  = '.fc'
    meta(1)%varnm='fc'
    meta(1)%varnl='cut off frequency'
    !meta(1)%varns='cut_off_frequency'
    meta(1)%varns=''
    meta(1)%varng='cut_off_frequency'
    meta(1)%vmin = 0
    meta(1)%vmax = 8
    ! ifi=9, ifj=3, cfx
    meta => group(9)%field(3)%meta
    meta(1)%fsc    = 0.01
    meta(1)%units  = '1'
    meta(1)%ename  = '.cfx'
    meta(1)%varnm='cfx'
    meta(1)%varnl='maximum cfl for spatial advection'
    !meta(1)%varns='maximum_cfl_for_spatial_advection'
    meta(1)%varns=''
    meta(1)%varng='maximum_cfl_for_spatial_advection'
    meta(1)%vmin = 0
    meta(1)%vmax = 320
    ! ifi=9, ifj=4, cfd
    meta => group(9)%field(4)%meta
    meta(1)%fsc    = 0.01
    meta(1)%units  = '1'
    meta(1)%ename  = '.cfd'
    meta(1)%varnm='cfd'
    meta(1)%varnl='maximum cfl for direction advection'
    !meta(1)%varns='maximum_cfl_for_direction_advection'
    meta(1)%varns=''
    meta(1)%varng='maximum_cfl_for_direction_advection'
    meta(1)%vmin = 0
    meta(1)%vmax = 320
    ! ifi=9, ifj=5, cfk
    meta => group(9)%field(5)%meta
    meta(1)%fsc    = 0.01
    meta(1)%units  = '1'
    meta(1)%ename  = '.cfk'
    meta(1)%varnm='cfk'
    meta(1)%varnl='maximum cfl for frequency advection'
    !meta(1)%varns='maximum_cfl_for_frequency_advection'
    meta(1)%varns=''
    meta(1)%varng='maximum_cfl_for_frequency_advection'
    meta(1)%vmin = 0
    meta(1)%vmax = 320
    !
    ! ------ group 10 (user defined) -------
    !
    ! ifi=10, ifj=1
    meta => group(10)%field(1)%meta
    meta(1)%fsc    = 0.1
    meta(1)%units  = 'm'
    meta(1)%vmin = 0
    meta(1)%vmax = 0
    write (meta(1)%ename,'(a2,i2.2)') '.u'
    write (meta(1)%varnm,'(a1,i2.2)') 'u'
    write (meta(1)%varnl,'(a12,i2.2)') 'user_defined'
    write (meta(1)%varns,'(a12,i2.2)') 'user_defined'
    write (meta(1)%varng,'(a12,i2.2)') 'user_defined'
    !
  end subroutine default_meta
  !/ ------------------------------------------------------------------- /
end module w3ounfmetamd
