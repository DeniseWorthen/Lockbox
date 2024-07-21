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
module w3fldsmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |            a. chawla              |
  !/                  |                        fortran 90 |
  !/                  | last update :         22-mar-2021 |
  !/                  +-----------------------------------+
  !/
  !/    30-nov-1999 : upgrade to fortran 90               ( version 2.00 )
  !/    25-jan-2002 : data assimilation set up.           ( version 2.17 )
  !/    26-dec-2002 : continuously moving grid.           ( version 3.02 )
  !/    04-sep-2003 : bug fix w3flhd.                     ( version 3.04 )
  !/    27-dec-2004 : multiple grid version.              ( version 3.06 )
  !/    05-jul-2005 : correct first level/ice.            ( version 3.07 )
  !/    27-jun-2006 : adding file name preamble.          ( version 3.09 )
  !/    09-oct-2007 : make file header optional.          ( version 3.13 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
  !/                  (w. e. rogers & t. j. campbell, nrl)
  !/    04-apr-2010 : adding icebergs with isi.           ( version 3.14 )
  !/    06-dec-2010 : change from global (logical) to iclo (integer) to
  !/                  specify index closure for a grid.   ( version 3.14 )
  !/                  (t. j. campbell, nrl)
  !/    30-oct-2012 : implement tidal analysis            ( version 4.08 )
  !/                  (f. ardhuin)
  !/    26-dec-2012 : modified obsolete declarations.     ( version 4.of )
  !/     5-mar-2012 : cleanup of tidal analysis           ( version 4.09 )
  !/    24-apr-2015 : adding oasis coupling calls         ( version 5.07 )
  !/                  (m. accensi & f. ardhuin, ifremer)
  !/    20-jan-2017 : update to new w3gsrumd apis         ( version 6.02 )
  !/    05-jun-2018 : adds debugfls                       ( version 6.04 )
  !/    22-mar-2021 : adds momentum and density input     ( version 7.13 )
  !/
  !/    copyright 2009-2012 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     gathers a set of routines to manage input fields of depth,
  !     current, wind, ice concentration, atmospheric momentum, and
  !     air density
  !
  !  2. variables and types :
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3fldo    subr. public   open data file.
  !      w3fldg    subr. public.  read/write data file (fields).
  !      w3fldd    subr. public.  read/write data file (data).
  !      w3fldp    subr. public.  generic field interpolation.
  !      w3fldh    subr. public.  process homogeneous fields.
  !      w3fldm    subr. public.  process moving grid data.
  !      w3fldtide subr. public.  read/write tidal constituents
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      strace    subr. w3servmd subroutine tracing.           ( !/s )
  !      tick21    subr. w3timemd increment the clock.
  !      dsec21    r.f.  w3timemd calculate time differnces.
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !
  !     - by design, these routines do not use the wavewatch iii data
  !       structure. with this approach, they can be used in a straight-
  !       forward way in other programs to generate wavewatch iii input
  !       data sets directly from such programs.
  !
  !  6. switches :
  !
  !  7. source code :
  !
  !/ ------------------------------------------------------------------- /
  public
contains
  !/ ------------------------------------------------------------------- /
  subroutine w3fldo ( inxout, idfld, nds, ndst, ndse, nx, ny,     &
       gtype, ierr, fext, fpre, fhdr, tideflagin )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |            a. chawla              |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    15-jan-1999 : final fortran 77                    ( version 1.18 )
    !/    30-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    24-jan-2001 : flat grid version (formats only)    ( version 2.06 )
    !/    24-jan-2002 : assimilation data added.            ( version 2.17 )
    !/    27-dec-2004 : multiple grid version.              ( version 3.06 )
    !/    27-jun-2006 : adding file name preamble.          ( version 3.09 )
    !/    09-oct-2007 : make file header optional.          ( version 3.13 )
    !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    04-apr-2010 : adding iceberg field.               ( version 3.14 )
    !/    09-sep-2012 : implement tidal cons. (f. ardhuin ) ( version 4.09 )
    !/    26-dec-2012 : modified obsolete declarations.     ( version 4.11 )
    !/    22-mar-2021 : adds momentum and density input     ( version 7.13 )
    !/
    !  1. purpose :
    !
    !     open and prepare wavewatch iii field files as used by the
    !     generic shell and the field preprocessor.
    !
    !  2. method :
    !
    !     the file header contains a general wavewatch iii id string,
    !     a field id string and the dimensions of the grid. if a file
    !     is opened to be read, these parameters are all checked.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       inxout  c*(*) i  test string for read/write, valid are:
    !                        'read' and 'write'.
    !       idfld   c*3  i/o id string for field type, valid are: 'ic1',
    !                        'ic2', 'ic3', 'ic4', 'ic5', 'mdn', 'mth',
    !                        'mvs', 'lev', 'cur', 'wnd', 'wns', 'ice',
    !                        'tau', 'rho', 'isi', and 'dtn'.
    !       nds     int.  i  dataset number for fields file.
    !       ndst    int.  i  dataset number for test output.
    !       ndse    int.  i  dataset number for error output.
    !                        (no output if ndse < 0).
    !       nx, ny  int.  i  discrete grid dimensions.                      !       gtype   int.  i  integer flag indicating type of grid.     /a
    
    !       nx      int. i/o record length.                                 !       gtype   int.  i  undefined value.                          /b
    
    !       ierr    int.  o  error indicator.
    !                         0 : no errors.
    !                         1 : illegal inxout.
    !                         2 : illegal id.
    !                         3 : error in opening file.
    !                         4 : write error in file.
    !                         5 : read error in file.
    !                         6 : premature eof in read.
    !                         7 : unexpected file identifier read.
    !                         8 : unexpected field identifier read.
    !                         9 : unexpected grid dimensions read.
    !                        10 : unexpected data info.
    !     ----------------------------------------------------------------
    !      a) for output fields.
    !      b) for input data.
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
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      ww3_prep  prog.   n/a    input data preprocessor.
    !      ww3_shel  prog.   n/a    basic wave model driver.
    !      ......    prog.   n/a    any other program that reads or
    !                               writes wavewatch iii data files.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     see end of subroutine.
    !
    !  7. remarks :
    !
    !     - on read, the id 'wnd' may be changed to 'wns' (including
    !       stability data).
    !     - on read, the id 'ice' may be changed to 'isi' (including
    !       iceberg data).
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !     !/t  enable test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !
    use w3odatmd, only : iaproc
    use constants, only: file_endian
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)                 :: nds, ndst, ndse, ny
    integer, intent(inout)              :: nx
    integer, intent(out)                :: ierr
    integer, intent(inout)              :: gtype
    character(len=3), intent(inout)     :: idfld
    character, intent(in)               :: inxout*(*)
    character, intent(in), optional     :: fext*(*), fpre*(*)
    logical, intent(in), optional       :: fhdr
    integer, intent(inout), optional    :: tideflagin
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: nxt, nyt, gtypet, i
    integer                 :: filler(3)
    logical                 :: write
    character(len=3)        :: tsfld
    character(len=11)       :: form = 'unformatted'
    character(len=13)       :: tsstr, idstr = 'wavewatch iii'
    character(len=20)       :: tempxt
    character(len=30)       :: fname
    logical                 :: fdhdr = .true.
    integer                 :: tideflag = 0
    logical                 ::  tideok = .false.
    !
    ! 'form' is used for initial testing of new files only.
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !
    ! test input parameters ---------------------------------------------- *
    !
    filler(:)=0
    if ( present(tideflagin) ) then
      tideflag = tideflagin
    else
      tideflag = 0
    end if
    if (inxout.ne.'read' .and. inxout.ne.'write') goto 801
    if ( idfld.ne.'ic1' .and. idfld.ne.'ic2' .and.                  &
         idfld.ne.'ic3' .and. idfld.ne.'ic4' .and.                  &
         idfld.ne.'ic5' .and. idfld.ne.'mdn' .and.                  &
         idfld.ne.'mth' .and. idfld.ne.'mvs' .and.                  &
         idfld.ne.'lev' .and. idfld.ne.'cur' .and.                  &
         idfld.ne.'wnd' .and. idfld.ne.'wns' .and.                  &
         idfld.ne.'ice' .and. idfld.ne.'tau' .and.                  &
         idfld.ne.'rho' .and. idfld.ne.'dt0' .and.                  &
         idfld.ne.'dt1' .and. idfld.ne.'dt2' .and.                  &
         idfld.ne.'isi' )    goto 802
    !
    if ( present(fext) ) then
      tempxt = fext
      i      = len_trim(fext)
    else
      tempxt = 'ww3'
      i      = 3
    end if
    !
    if ( present(fhdr) ) then
      fdhdr = fhdr
    end if
    !
    ! set internal variables --------------------------------------------- *
    !
    if ( idfld.eq.'lev' ) then
      fname = 'level.' // tempxt(:i)
      i     = i + 6
    else if ( idfld.eq.'cur' ) then
      fname = 'current.' // tempxt(:i)
      i     = i + 8
    else if ( idfld.eq.'wnd' .or. idfld.eq.'wns' ) then
      fname = 'wind.' // tempxt(:i)
      i     = i + 5
    else if ( idfld.eq.'ice' .or. idfld.eq.'isi' ) then
      fname = 'ice.' // tempxt(:i)
      i     = i + 4
    else if ( idfld.eq.'tau' ) then
      fname = 'momentum.' // tempxt(:i)
      i     = i + 9
    else if ( idfld.eq.'rho' ) then
      fname = 'density.' // tempxt(:i)
      i     = i + 8
    else if ( idfld.eq.'dt0' ) then
      fname = 'data0.' // tempxt(:i)
      i     = i + 6
    else if ( idfld.eq.'dt1' ) then
      fname = 'data1.' // tempxt(:i)
      i     = i + 6
    else if ( idfld.eq.'dt2' ) then
      fname = 'data2.' // tempxt(:i)
      i     = i + 6
    else if ( idfld.eq.'mdn' ) then
      fname = 'muddens.' // tempxt(:i)
      i     = i + 8
    else if ( idfld.eq.'mth' ) then
      fname = 'mudthk.' // tempxt(:i)
      i     = i + 7
    else if ( idfld.eq.'mvs' ) then
      fname = 'mudvisc.' // tempxt(:i)
      i     = i + 8
    else if ( idfld.eq.'ic1' ) then
      fname = 'ice1.' // tempxt(:i)
      i     = i + 5
    else if ( idfld.eq.'ic2' ) then
      fname = 'ice2.' // tempxt(:i)
      i     = i + 5
    else if ( idfld.eq.'ic3' ) then
      fname = 'ice3.' // tempxt(:i)
      i     = i + 5
    else if ( idfld.eq.'ic4' ) then
      fname = 'ice4.' // tempxt(:i)
      i     = i + 5
    else if ( idfld.eq.'ic5' ) then
      fname = 'ice5.' // tempxt(:i)
      i     = i + 5
    end if
    !
    write  = inxout .eq. 'write'
    !
    !
    ! open file ---------------------------------------------------------- *
    !
    if ( write ) then
      if ( present(fpre) ) then
        open (nds,file=fpre//fname(:i),form=form, convert=file_endian, &
             err=803, iostat=ierr)
      else
        open (nds,file=fname(:i),form=form,convert=file_endian, &
             err=803,iostat=ierr)
      end if
    else
      if ( present(fpre) ) then
        open (nds,file=fpre//fname(:i),form=form,convert=file_endian, &
             status='old',err=803,iostat=ierr)
      else
        open (nds,file=fname(:i),form=form,convert=file_endian,       &
             status='old',err=803,iostat=ierr)
      end if
    end if
    !
    ! process test data -------------------------------------------------- *
    !
    if ( write ) then
      if ( fdhdr ) then
        if ( form .eq. 'unformatted' ) then
          !
          ! the "filler" was added for compatibility with old binary forcing files
          ! it is now also used for tidal info ...
          !
          write (nds,err=804,iostat=ierr)                      &
               idstr, idfld, nx, ny, gtype, filler(1:2), tideflag
        else
          write (nds,900,err=804,iostat=ierr)                  &
               idstr, idfld, nx, ny, gtype, filler(1:2), tideflag
        end if
      end if
    else
      if ( form .eq. 'unformatted' ) then
        read (nds,end=806,err=805,iostat=ierr)                  &
             tsstr, tsfld, nxt, nyt, gtypet, filler(1:2), tideflag
      else
        read (nds,900,end=806,err=805,iostat=ierr)              &
             tsstr, tsfld, nxt, nyt, gtypet, filler(1:2), tideflag
      end if
      if ((filler(1).ne.0.or.filler(2).ne.0).and.tideflag.ge.0) tideflag=0
      if (tideflag.ne.0.and.(.not.tideok)) then
        goto 810
      end if
      !
      if ( idstr .ne. tsstr ) goto 807
      if (( idfld.eq.'wnd' .and. tsfld.eq.'wns') .or.             &
           ( idfld.eq.'ice' .and. tsfld.eq.'isi')  ) then
        idfld  = tsfld
      end if
      if ( idfld .ne. tsfld ) goto 808
      if ( idfld(1:2) .ne. 'dt' ) then
        if ( nx.ne.nxt .or. ny.ne.nyt ) then
          goto 809
        else
          nx     = nxt
          if (gtype.le.4) gtype  = gtypet
        end if
      end if
    end if
    !
    ! file ok ------------------------------------------------------------ *
    !
    ierr   = 0
    if ( present(tideflagin) ) then
      tideflagin = tideflag
    end if
    return
    !
    ! error escape locations
    !
801 continue
    if ( ndse .ge. 0 ) write (ndse,1001) inxout
    ierr   = 1
    return
    !
802 continue
    if ( ndse .ge. 0 ) write (ndse,1002) idfld
    ierr   = 2
    return
    !
803 continue
    if ( ndse .ge. 0 ) write (ndse,1003) idfld, ierr
    ierr   = 3
    return
    !
804 continue
    if ( ndse .ge. 0 ) write (ndse,1004) idfld, ierr
    ierr   = 4
    return
    !
805 continue
    if ( ndse .ge. 0 ) write (ndse,1005) idfld, ierr
    ierr   = 5
    return
    !
806 continue
    if ( ndse .ge. 0 ) write (ndse,1006) idfld
    ierr   = 6
    return
    !
807 continue
    if ( ndse .ge. 0 ) write (ndse,1007) tsstr, idstr
    ierr   = 7
    return
    !
808 continue
    if ( ndse .ge. 0 ) write (ndse,1008) tsfld, idfld
    ierr   = 8
    return
    !
809 continue
    if ( ndse .ge. 0 ) write (ndse,1009)                      &
         nxt, nyt, gtypet,               &
         nx , ny , gtype
    ierr   = 9
    return
    !
810 continue
    if ( ndse .ge. 0 ) write (ndse,1010)                      &
         filler(1:2),tideflag
    ierr   = 10
    return
    !
    ! formats
    !
900 format (1x,a13,1x,a3,6i12)
    !
1001 format (/' *** wavewatch iii error in w3fldo : '/         &
         '     illegal inxout string : ',a/)
1002 format (/' *** wavewatch iii error in w3fldo : '/         &
         '     illegal field id string : ',a/)
1003 format (/' *** wavewatch iii error in w3fldo : '/         &
         '     error in opening ',a,' file, iostat =',i6/)
1004 format (/' *** wavewatch iii error in w3fldo : '/         &
         '     error in writing to ',a,' file, iostat =',i6/)
1005 format (/' *** wavewatch iii error in w3fldo : '/         &
         '     error in reading ',a,' file, iostat =',i6/)
1006 format (/' *** wavewatch iii error in w3fldo : '/         &
         '     premature end of ',a,' file'/)
1007 format (/' *** wavewatch iii error in w3fldo : '/         &
         '     illegal file id string >',a,'<'/           &
         '                  should be >',a,'<'/)
1008 format (/' *** wavewatch iii error in w3fldo : '/         &
         '     illegal field id string >',a,'<'/          &
         '                   should be >',a,'<'/)
1009 format (/' *** wavewatch iii error in w3fldo : '/         &
         '     incompatible grid data : ',3(1x,i10)/             &
         '                  should be : ',3(1x,i10)/)
1010 format (/' *** wavewatch iii error in w3fldo : '/          &
         '     filler indicates use of tidal constituents',3i4, /&
         '     for this the code should be compiled with tide switch'/)
    !
    !/
    !/ end of w3fldo  ---------------------------------------------------- /
    !/
  end subroutine w3fldo
  !/ ------------------------------------------------------------------- /
  subroutine w3fldtide1 ( inxout, nds, ndst, ndse, nx, ny, idfld, ierr )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           f. ardhuin              |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    24-sep-2012 : creation                            ( version 4.09 )
    !/    30-jun-2013 : split in 2 subroutines              ( version 4.11 )
    !/    22-mar-2021 : adds momentum and density input     ( version 7.13 )
    !/
    !  1. purpose :
    !
    !     reads and writes tidal consituents
    !
    !  2. method :
    !
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       inxout  c*(*) i  test string for read/write, valid are:
    !                        'read' and 'write'.
    !       idfld   c*3  i/o id string for field type, valid are:
    !                        'lev', 'cur', 'wnd', 'wns', 'ice', 'isi',
    !                        'tau', 'rho', and 'dtn'.
    !       nds     int.  i  dataset number for fields file.
    !       ndst    int.  i  dataset number for test output.
    !       ndse    int.  i  dataset number for error output.
    !                        (no output if ndse < 0).
    !       nx, ny  int.  i  discrete grid dimensions.                      !       ierr    int.  o  error indicator.
    
    !                         0 : no errors.
    !                         1 : illegal inxout.
    !     ----------------------------------------------------------------
    !      a) for output fields.
    !      b) for input data.
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
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      ww3_prep  prog.   n/a    input data preprocessor.
    !      ww3_prnc  prog.   n/a    netcdf input data preprocessor.
    !      ww3_shel  prog.   n/a    basic wave model driver.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     see end of subroutine.
    !
    !  7. remarks :
    !
    !     - on read, the id 'wnd' may be changed to 'wns' (including
    !       stability data).
    !     - on read, the id 'ice' may be changed to 'isi' (including
    !       iceberg data).
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !     !/t  enable test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !
    use w3idatmd
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)                 :: nds, ndst, ndse, nx, ny
    character(len=3), intent(inout)     :: idfld
    character*(*), intent(in)           :: inxout
    integer, intent(out)                :: ierr
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    logical                 :: write
    integer                 :: i, ix
    !
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! test input parameters ---------------------------------------------- *
    !
    if (inxout.ne.'read' .and. inxout.ne.'write') goto 801
    if ( idfld.ne.'lev' .and. idfld.ne.'cur' .and.                  &
         idfld.ne.'wnd' .and. idfld.ne.'wns' .and.                  &
         idfld.ne.'ice' .and. idfld.ne.'tau' .and.                  &
         idfld.ne.'rho' .and. idfld.ne.'dt0' .and.                  &
         idfld.ne.'dt1' .and. idfld.ne.'dt2' .and.                  &
         idfld.ne.'isi' )    goto 802
    write  = inxout .eq. 'write'
    !
    !
    ! file ok ------------------------------------------------------------ *
    !
    ierr   = 0
    return
    !
    ! error escape locations
    !
801 continue
    if ( ndse .ge. 0 ) write (ndse,1001) inxout
    ierr   = 1
    return
    !
802 continue
    if ( ndse .ge. 0 ) write (ndse,1002) idfld
    ierr   = 2
    return
    !
804 continue
    if ( ndse .ge. 0 ) write (ndse,1004) idfld, ierr
    ierr   = 4
    return
    !
805 continue
    if ( ndse .ge. 0 ) write (ndse,1005) idfld, ierr
    ierr   = 5
    return
    !
806 continue
    if ( ndse .ge. 0 ) write (ndse,1006) idfld
    ierr   = 6
    return
    !
    ! formats
    !
1001 format (/' *** wavewatch iii error in w3fldtide1 : '/        &
         '     illegal inxout string : ',a/)
1002 format (/' *** wavewatch iii error in w3fldtide1 : '/        &
         '     illegal field id string : ',a/)
1004 format (/' *** wavewatch iii error in w3fldtide1 : '/        &
         '     error in writing to ',a,' file, iostat =',i6/)
1005 format (/' *** wavewatch iii error in w3fldtide1 : '/        &
         '     error in reading ',a,' file, iostat =',i6/)
1006 format (/' *** wavewatch iii error in w3fldtide1 : '/        &
         '     premature end of ',a,' file'/)
    !/
    !/ end of w3fldo  ---------------------------------------------------- /
    !/
  end subroutine w3fldtide1
  !/ ------------------------------------------------------------------- /
  subroutine w3fldtide2 ( inxout, nds, ndst, ndse, nx, ny, idfld, idat, ierr )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           f. ardhuin              |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    24-sep-2012 : creation                            ( version 4.09 )
    !/    30-jun-2013 : split in 2 subroutines              ( version 4.11 )
    !/    22-mar-2021 : adds momentum and density input     ( version 7.13 )
    !/
    !  1. purpose :
    !
    !     reads and writes tidal constituents
    !
    !  2. method :
    !
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       inxout  c*(*) i  test string for read/write, valid are:
    !                        'read' and 'write'.
    !       idfld   c*3  i/o id string for field type, valid are:
    !                        'lev', 'cur', 'wnd', 'wns', 'ice', 'isi',
    !                        'tau', 'rho',  and 'dtn'.
    !       nds     int.  i  dataset number for fields file.
    !       ndst    int.  i  dataset number for test output.
    !       ndse    int.  i  dataset number for error output.
    !                        (no output if ndse < 0).
    !       nx, ny  int.  i  discrete grid dimensions.                      !       idat    int.  i  equal to 1 if w3idatmd arrays are to be filled
    
    !       ierr    int.  o  error indicator.
    !                         0 : no errors.
    !                         1 : illegal inxout.
    !     ----------------------------------------------------------------
    !      a) for output fields.
    !      b) for input data.
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
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      ww3_prep  prog.   n/a    input data preprocessor.
    !      ww3_prnc  prog.   n/a    netcdf input data preprocessor.
    !      ww3_shel  prog.   n/a    basic wave model driver.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     see end of subroutine.
    !
    !  7. remarks :
    !
    !     - on read, the id 'wnd' may be changed to 'wns' (including
    !       stability data).
    !     - on read, the id 'ice' may be changed to 'isi' (including
    !       iceberg data).
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !     !/t  enable test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !
    use w3idatmd
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)                 :: nds, ndst, ndse, nx, ny, idat
    character(len=3), intent(inout)     :: idfld
    character*(*), intent(in)           :: inxout
    integer, intent(out)                :: ierr
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    logical                 :: write
    integer                 :: i, ix, tide_mf1
    character(len=100)      :: list(70)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! test input parameters ---------------------------------------------- *
    !
    if (inxout.ne.'read' .and. inxout.ne.'write') goto 801
    if ( idfld.ne.'lev' .and. idfld.ne.'cur' .and.                  &
         idfld.ne.'wnd' .and. idfld.ne.'wns' .and.                  &
         idfld.ne.'ice' .and. idfld.ne.'tau' .and.                  &
         idfld.ne.'rho' .and. idfld.ne.'dt0' .and.                  &
         idfld.ne.'dt1' .and. idfld.ne.'dt2' .and.                  &
         idfld.ne.'isi' )    goto 802
    write  = inxout .eq. 'write'
    !
    !
    ! file ok ------------------------------------------------------------ *
    !
    ierr   = 0
    return
    !
    ! error escape locations
    !
801 continue
    if ( ndse .ge. 0 ) write (ndse,1001) inxout
    ierr   = 1
    return
    !
802 continue
    if ( ndse .ge. 0 ) write (ndse,1002) idfld
    ierr   = 2
    return
    !
804 continue
    if ( ndse .ge. 0 ) write (ndse,1004) idfld, ierr
    ierr   = 4
    return
    !
805 continue
    if ( ndse .ge. 0 ) write (ndse,1005) idfld, ierr
    ierr   = 5
    return
    !
806 continue
    if ( ndse .ge. 0 ) write (ndse,1006) idfld
    ierr   = 6
    return
    !
807 continue
    ierr   = 7
    return
    !
    ! formats
    !
1001 format (/' *** wavewatch iii error in w3fldtide2 : '/         &
         '     illegal inxout string : ',a/)
1002 format (/' *** wavewatch iii error in w3fldtide2 : '/         &
         '     illegal field id string : ',a/)
1004 format (/' *** wavewatch iii error in w3fldtide2 : '/         &
         '     error in writing to ',a,' file, iostat =',i6/)
1005 format (/' *** wavewatch iii error in w3fldtide2 : '/         &
         '     error in reading ',a,' file, iostat =',i6/)
1006 format (/' *** wavewatch iii error in w3fldtide2 : '/         &
         '     premature end of ',a,' file'/)
    !/
    !/ end of w3fldo  ---------------------------------------------------- /
    !/
  end subroutine w3fldtide2
  !/ ------------------------------------------------------------------- /
  subroutine w3fldg (inxout, idfld, nds, ndst, ndse, mx, my,      &
       nx, ny, t0, tn, tf0, fx0, fy0, fa0,          &
       tfn, fxn, fyn, fan, ierr, flagsc             &
       )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         13-aug-2021 |
    !/                  +-----------------------------------+
    !/
    !/    15-jan-1999 : final fortran 77                    ( version 1.18 )
    !/    30-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    05-jul-2005 : correct first level/ice.            ( version 3.07 )
    !/    04-apr-2010 : adding icebergs in isi              ( version 3.14 )
    !/    26-dec-2012 : modified obsolete declarations.     ( version 4.11 )
    !/    24-apr-2015 : adding oasis coupling calls         ( version 5.07 )
    !/                  (m. accensi & f. ardhuin, ifremer)
    !/    25-sep-2020 : receive coupled fields at t+0       ( version 7.10 )
    !/    22-mar-2021 : adds momentum and density input     ( version 7.13 )
    !/    13-aug-2021 : allow scalar fields to be time      ( version 7.14 )
    !/                  interpolated
    !/
    !  1. purpose :
    !
    !     update input fields in the wavewatch iii generic shell from a
    !     wavewatch iii shell data file or write from preprocessor.
    !
    !  2. method :
    !
    !     read from file opened by w3fldo.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       inxout  c*(*)  i   test string for read/write, valid are:
    !                          'read' and 'write'.
    !       idfld   c*3    i   id string for field type, valid are: 'ic1',
    !                          'ic2', 'ic3', 'ic4', 'ic5', 'mdn', 'mth', 'mvs',
    !                          'lev', 'cur', 'wnd', 'wns', 'ice', 'isi',
    !                          'tau', and 'rho'.
    !       nds     int.   i   dataset number for fields file.
    !       ndst    int.   i   dataset number for test output.
    !       ndse    int.   i   dataset number for error output.
    !                          (no error output if ndse < 0 ).
    !       mx,my   int.   i   array dimensions output fields.
    !       nx,ny   int.   i   discrete grid dimensions.
    !       t0-n    i.a.   i   time interval considered (dummy for write).
    !       tf0-n   i.a.  i/o  field times (tfn dummy for write).
    !       fxx     r.a.  i/o  input fields (fxn dummy for write).
    !            subtypes: fx0, fy0, fa0, fxn, fyn, fan
    !                    (meaning is inferred from context as follows)
    !                      "0" denotes "prior time level"
    !                      "n" denotes "next time level"
    !                      "x" denotes x in a vector
    !                      "y" denotes y in a vector
    !                      "a" denotes scalar
    !       ierr    int.   o   error indicator,
    !                          -1 past last data
    !                           0 ok,
    !                           1 : illegal inxout.
    !                           2 : illegal idfld.
    !                           3 : error in writing time.
    !                           4 : error in writing field.
    !                           5 : error in reading time.
    !                           6 : premature eof reading field.
    !                           7 : error reading field.
    !       flagsc  log.  i/o  flag for coupling field
    !       coupl_comm int. i  mpi communicator for coupling
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr.   id.    subroutine tracing.
    !      tick21    subr. w3timemd advance time.
    !      dsec21    func.   id.    difference between times.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      ww3_prep  prog.   n/a    input data preprocessor.
    !      ww3_shel  prog.   n/a    basic wave model driver.
    !      ......    prog.   n/a    any other program that reads or
    !                               writes wavewatch iii data files.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     see end of subroutine.
    !
    !  7. remarks :
    !
    !     - saving of previous fields needed only for reading of 2-d fields.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !     !/t  enable test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use w3timemd
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)          :: nds, ndst, ndse, mx, my,        &
         nx, ny, t0(2), tn(2)
    integer, intent(inout)       :: tf0(2), tfn(2)
    integer, intent(out)         :: ierr
    real, intent(inout)          :: fx0(mx,my), fy0(mx,my),         &
         fxn(mx,my), fyn(mx,my),         &
         fa0(mx,my), fan(mx,my)
    character, intent(in)        :: inxout*(*)
    character(len=3), intent(in) :: idfld
    logical, intent(inout), optional        :: flagsc
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ix, iy, j, istat
    real                    :: dttst
    logical                 :: write, fl2d, flfrst, flbe, flst,    &
         flinterp, flcoupl
    logical, parameter      :: flagsc_default = .false.
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !/
    ierr   = 0
    !
    !
    ! test input parameters ---------------------------------------------- *
    !
    if (inxout.ne.'read' .and. inxout.ne.'write') goto 801
    if ( idfld.ne.'ic1' .and. idfld.ne.'ic2' .and.                  &
         idfld.ne.'ic3' .and. idfld.ne.'ic4' .and.                  &
         idfld.ne.'ic5' .and. idfld.ne.'mdn' .and.                  &
         idfld.ne.'mth' .and. idfld.ne.'mvs' .and.                  &
         idfld.ne.'lev' .and. idfld.ne.'cur' .and.                  &
         idfld.ne.'wnd' .and. idfld.ne.'wns' .and.                  &
         idfld.ne.'ice' .and. idfld.ne.'isi' .and.                  &
         idfld.ne.'tau' .and. idfld.ne.'rho' )    goto 802
    !
    ! set internal variables --------------------------------------------- *
    !
    write  = inxout .eq. 'write'
    fl2d   = idfld.eq.'cur' .or. idfld.eq.'wnd' .or. idfld.eq.'wns' &
         .or. idfld.eq.'isi' .or. idfld.eq.'tau'
    flbe   = idfld.eq.'isi'
    flst   = idfld.eq.'wns'
    if ( .not. present(flagsc) ) then
      flcoupl=flagsc_default
    else
      flcoupl=flagsc
    end if
    ! this flag is necessary to define the field at the start and end time
    ! of integration for the first time step which is integrated on 0
    ! to be able to output integrated variables like cha, ust, taw
    flinterp = idfld.eq.'cur' .or. idfld.eq.'wnd' .or. idfld.eq.'wns' &
         .or. idfld.eq.'tau' .or. idfld.eq.'rho'
    ! if the model is coupled, no interpolation in time must be done
    if (flcoupl) flinterp = .false.
    flfrst = tfn(1) .eq. -1
    !
    !
    ! loop over times / fields ========================================== *
    !
    do
      !
      ! shift fields (interpolated fields only)
      !
      if ( (.not.write) .and. flinterp ) then
        !
        tf0(1) = tfn(1)
        tf0(2) = tfn(2)
        ! unless tfn has been changed in the do loop, the following line is essentally
        !       "if not.flfrst"
        if ( tfn(1) .ne. -1 ) then
          do ix=1, nx
            do iy=1, ny
              fx0(ix,iy) = fxn(ix,iy)
              if (fl2d) fy0(ix,iy) = fyn(ix,iy)
            end do
            if( flst .or. .not.fl2d ) then
              do iy=1, ny
                fa0(ix,iy) = fan(ix,iy)
              end do
            end if
          end do
        end if
        !
      end if
      !
      ! process fields, write --------------------------------------------- *
      !
      if ( write ) then
        !
        write (nds,err=803,iostat=istat) tf0
        if ( .not. fl2d ) then
          j      = 1
          write (nds,err=804,iostat=istat)                      &
               ((fa0(ix,iy),ix=1,nx),iy=1,ny)
        else
          j      = 1
          write (nds,err=804,iostat=istat)                      &
               ((fx0(ix,iy),ix=1,nx),iy=1,ny)
          j      = 2
          write (nds,err=804,iostat=istat)                      &
               ((fy0(ix,iy),ix=1,nx),iy=1,ny)
          j      = 3
          if ( flst ) write (nds,err=804,iostat=istat)          &
               ((fa0(ix,iy),ix=1,nx),iy=1,ny)
        end if
        !
        exit
        !
        ! process fields, read ---------------------------------------------- *
        !
      else
        !
            !
          read (nds,end=800,err=805,iostat=istat) tfn
          if ( .not. fl2d ) then
            ! note: "j" here does *not* refer to data type, wlev etc.
            !       it refers to the dimension.
            j      = 1
            read (nds,end=806,err=807,iostat=istat)               &
                 ((fan(ix,iy),ix=1,nx),iy=1,ny)
          else
            j      = 1
            read (nds,end=806,err=807,iostat=istat)               &
                 ((fxn(ix,iy),ix=1,nx),iy=1,ny)
            j      = 2
            read (nds,end=806,err=807,iostat=istat)               &
                 ((fyn(ix,iy),ix=1,nx),iy=1,ny)
            ! this was added for isi files to store ice in fan and berg in fyn
            if (flbe) fan(:,:) = fxn(:,:)
            ! this was added for wns files to store wnd in fxn & fyn and ast in fan
            j      = 3
            if ( flst ) read (nds,end=806,err=807,iostat=istat)   &
                 ((fan(ix,iy),ix=1,nx),iy=1,ny)
          end if
        !
        ! check time, branch back if necessary
        !
        dttst  = dsec21 ( t0 , tfn )
        ! exit if the time is the first time and the field is not interpolated in time
        if ( .not.flinterp .and. flfrst .and. dttst .eq. 0. ) exit
        ! exit if the time of the input field is larger than the current time
        if ( dttst .gt. 0. ) exit
        !
      end if
      !
    end do
    !
    ! branch point for eof and interpolated fields (forcing current, wind or winds)
    !
300 continue
    ! if the field is interpolated in time and the start time of interpolation is not set
    ! save the time and field values at the start time and field of interpolation
    if ( .not.write .and. flinterp .and. tf0(1) .eq. -1 ) then
      !
      tf0(1) = t0(1)
      tf0(2) = t0(2)
      !
      do ix=1, nx
        do iy=1, ny
          fx0(ix,iy) = fxn(ix,iy)
          if (fl2d) fy0(ix,iy) = fyn(ix,iy)
        end do
        if( flst .or. .not.fl2d ) then
          do iy=1, ny
            fa0(ix,iy) = fan(ix,iy)
          end do
        end if
      end do
      !
    end if
    !
    ! branch point for eof and not interpolated fields (coupled fields, ice, lev, ...)
    !
500 continue
    !
    !
    ! process fields, end ----------------------------------------------- *
    !
    return
    !
    ! eof escape location (have read to end of file)
    !
800 continue
    ierr   = -1
    !
    if ( flinterp ) then
      tfn(1) = tn(1)
      tfn(2) = tn(2)
      call tick21 ( tfn , 1. )
    end if
    !
    if ( flinterp ) then
      goto 300
    else
      goto 500
    end if
    !
    !
    ! error escape locations
    !
801 continue
    if ( ndse .ge. 0 ) write (ndse,1001) inxout
    ierr   = 1
    return
    !
802 continue
    if ( ndse .ge. 0 ) write (ndse,1002) idfld
    ierr   = 2
    return
    !
803 continue
    if ( ndse .ge. 0 ) write (ndse,1003) istat
    ierr   = 3
    return
    !
804 continue
    if ( ndse .ge. 0 ) write (ndse,1004) j, istat
    ierr   = 4
    return
    !
805 continue
    if ( ndse .ge. 0 ) write (ndse,1005) istat
    ierr   = 5
    return
    !
806 continue
    if ( ndse .ge. 0 ) write (ndse,1006) j, istat
    ierr   = 6
    return
    !
807 continue
    if ( ndse .ge. 0 ) write (ndse,1007) j, istat
    ierr   = 7
    return
    !
    ! formats
    !
1001 format (/' *** wavewatch iii error in w3fldg : '/               &
         '     illegal inxout string : ',a/)
1002 format (/' *** wavewatch iii error in w3fldg : '/               &
         '     illegal field id string : ',a/)
1003 format (/' *** wavewatch iii error in w3fldg : '/               &
         '     error in writing time, iostat =',i6/)
1004 format (/' *** wavewatch iii error in w3fldg : '/               &
         '     error in writing field ',i1,', iostat =',i6/)
1005 format (/' *** wavewatch iii error in w3fldg : '/               &
         '     error in reading time, iostat =',i6/)
1006 format (/' *** wavewatch iii error in w3fldg : '/               &
         '     prmature eof reading field ',i1,', iostat =',i6/)
1007 format (/' *** wavewatch iii error in w3fldg : '/               &
         '     error in reading field ',i1,', iostat =',i6/)
    !
    !/
    !/ end of w3fldg ----------------------------------------------------- /
    !/
  end subroutine w3fldg
  !/ ------------------------------------------------------------------- /
  subroutine w3fldd (inxout, idfld, nds, ndst, ndse, time, td,    &
       nr, nd, ndout, data, ierr )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         26-dec-2012 |
    !/                  +-----------------------------------+
    !/
    !/    24-jan-2002 : origination.                        ( version 2.17 )
    !/    26-dec-2012 : modified obsolete declarations.     ( version 4.11 )
    !/
    !  1. purpose :
    !
    !     update assimilation data in the wavewatch iii generic shell from
    !     a wavewatch iii shell data file or write from preprocessor.
    !
    !  2. method :
    !
    !     read from file opened by w3fldo.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       inxout  c*(*)  i   test string for read/write, valid are:
    !                           'write'  write a data field to file.
    !                           'size'   get the number of records of
    !                                    next data set.
    !                           'read'   read the data set found by
    !                                    'size' after allocating proper
    !                                    data array.
    !       idfld   c*3    i   id string for field type, valid are:
    !                          'dt0', 'dt1', and 'dt2'.
    !       nds     int.   i   dataset number for fields file.
    !       ndst    int.   i   dataset number for test output.
    !       ndse    int.   i   dataset number for error output.
    !                          (no error output if ndse < 0 ).
    !       time    i.a.   i   minimum time for data.
    !       td      i.a.  i/o  data time.
    !       nr,nd   int.   i   array dimensions.
    !       ndout   int.   o   number of data to be read next.
    !       data    r.a.  i/o  data array.
    !       ierr    int.   o   error indicator,
    !                          -1 past last data
    !                           0 ok,
    !                           1 : illegal inxout.
    !                           2 : illegal idfld.
    !                           3 : error in writing time.
    !                           4 : error in writing data.
    !                           5 : error in reading time.
    !                           6 : premature eof reading data.
    !                           7 : error reading data.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr.   id.    subroutine tracing.
    !      tick21    subr. w3timemd advance time.
    !      dsec21    func.   id.    difference between times.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      ww3_prep  prog.   n/a    input data preprocessor.
    !      ww3_shel  prog.   n/a    basic wave model driver.
    !      ......    prog.   n/a    any other program that reads or
    !                               writes wavewatch iii data files.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     see end of subroutine.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !     !/t  enable test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use w3timemd
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)          :: nds, ndst, ndse, time(2), nr, nd
    integer, intent(inout)       :: td(2), ndout
    integer, intent(out)         :: ierr
    real, intent(inout)          :: data(nr,nd)
    character, intent(in)        :: inxout*(*)
    character(len=3), intent(in) :: idfld
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: istat, nrt
    real                    :: dttst
    logical                 :: write, size
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !/
    ierr   = 0
    !
    !
    ! test input parameters ---------------------------------------------- *
    !
    if ( inxout.ne.'read' .and. inxout.ne.'write' .and.             &
         inxout.ne.'size' ) goto 801
    if ( idfld.ne.'dt0' .and. idfld.ne.'dt1' .and.                  &
         idfld.ne.'dt2' )    goto 802
    !
    ! set internal variables --------------------------------------------- *
    !
    write  = inxout .eq. 'write'
    size   = inxout .eq. 'size'
    !
    !
    ! process fields, write --------------------------------------------- *
    !
    if ( write ) then
      !
      write (nds,err=803,iostat=istat) td, nd
      write (nds,err=804,iostat=istat) data
      !
      ! process fields, read size ----------------------------------------- *
      !
    else if ( size ) then
      !
100   continue
      read (nds,end=800,err=805,iostat=istat) td, ndout
      !
      ! check time, read and branch back if necessary
      !
      dttst  = dsec21 ( time , td )
      if ( dttst.lt.0. .or. ndout.eq.0 ) then
        if (ndout.gt.0) read (nds,end=806,err=807,iostat=istat)
        goto 100
      end if
      !
      ! process fields, read data ----------------------------------------- *
      !
    else
      !
      read (nds,end=806,err=807,iostat=istat) data
    end if
    !
    ! process fields, end ----------------------------------------------- *
    !
    return
    !
    ! eof escape location
    !
800 continue
    ierr   = -1
    return
    !
    ! error escape locations
    !
801 continue
    if ( ndse .ge. 0 ) write (ndse,1001) inxout
    ierr   = 1
    return
    !
802 continue
    if ( ndse .ge. 0 ) write (ndse,1002) idfld
    ierr   = 2
    return
    !
803 continue
    if ( ndse .ge. 0 ) write (ndse,1003) istat
    ierr   = 3
    return
    !
804 continue
    if ( ndse .ge. 0 ) write (ndse,1004) istat
    ierr   = 4
    return
    !
805 continue
    if ( ndse .ge. 0 ) write (ndse,1005) istat
    ierr   = 5
    return
    !
806 continue
    if ( ndse .ge. 0 ) write (ndse,1006) istat
    ierr   = 6
    return
    !
807 continue
    if ( ndse .ge. 0 ) write (ndse,1007) istat
    ierr   = 7
    return
    !
    ! formats
    !
1001 format (/' *** wavewatch iii error in w3fldd : '/               &
         '     illegal inxout string : ',a/)
1002 format (/' *** wavewatch iii error in w3fldd : '/               &
         '     illegal field id string : ',a/)
1003 format (/' *** wavewatch iii error in w3fldd : '/               &
         '     error in writing time, iostat =',i6/)
1004 format (/' *** wavewatch iii error in w3fldd : '/               &
         '     error in writing data, iostat =',i6/)
1005 format (/' *** wavewatch iii error in w3fldd : '/               &
         '     error in reading time, iostat =',i6/)
1006 format (/' *** wavewatch iii error in w3fldd : '/               &
         '     prmature eof reading data, iostat =',i6/)
1007 format (/' *** wavewatch iii error in w3fldd : '/               &
         '     error in reading data, iostat =',i6/)
    !
    !/
    !/ end of w3fldd ----------------------------------------------------- /
    !/
  end subroutine w3fldd
  !/ ------------------------------------------------------------------- /
  subroutine w3fldp ( ndsm, ndst, ndse, ierr, flagll,             &
       mx, my, nx, ny,                             &
       tlat, tlon,     mapovr, iland, mxi, myi,    &
       nxi, nyi, closed, alat, alon, mask,         &
       rd11, rd21, rd12, rd22, ix1, ix2, iy1, iy2 )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         30-oct-2009 |
    !/                  +-----------------------------------+
    !/
    !/    08-feb-1999 : final fortran 77                    ( version 1.18 )
    !/    30-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    20-jan-2017 : update to new w3gsrumd apis         ( version 6.02 )
    !/
    !  1. purpose :
    !
    !     general purpose routine for interpolating data of an irregular
    !     grid given by alat and alon to a target grid given by tlat and tlon.
    !
    !  2. method :
    !
    !     use the grid search and remapping utilities (w3gsrumd).
    !     bi-linear interpolation.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       ndsm    int.  i  unit number message  output (disabled if 0).
    !       ndst    int.  i  unit number test output.
    !       ndse    int.  i  unit number error output.
    !       ierr    int.  o  error indicator (number of lost points due
    !                        to ap conflicts).
    !       flagll  log.  i  coordinate system flag (t=lat/lon, f=cartesian)
    !       mx,my   int.  i  array dimensions for output type arrays.
    !       nx,ny   int.  i  id. actual field syze.
    !       tlat    r.a.  i  y-coordinates of output grid.
    !       tlon    r.a.  i  x-coordinates of output grid.
    !       mapovr  i.a. i/o overlay map, the value of a grid point is
    !                        incremeted by 1 of the corresponding grid
    !                        point of the output grid is covered by the
    !                        input grid. land points are masked out by
    !                        setting them to iland.
    !       iland   int.  i  value for land points in mapovr (typically<0)
    !       mxi,myi int.  i  array dimensions for input fields.
    !       nxi,nyi int.  i  id. actual field sizes.
    !       closed  log.  i  flag for closed longitude range in input.
    !       alat    r.a.  i  y-coordinates of input grid.
    !       alon    r.a. i/o x-coordinates of input grid.
    !                        (will be modified if closed)
    !       mask    i.a.  i  land-sea mask for input field (0=land).
    !       rdnn    r.a.  o  interpolation factors (see below).
    !       ixn,iyn i.a.  o  interpolation addresses (see below).
    !     ----------------------------------------------------------------
    !
    !                             rd12|          |rd22
    !                     iy2       --+----------+--
    !                                 |          |
    !                                 |          |
    !                                 |          |
    !                                 |          |
    !                     iy1       --+----------+--
    !                             rd11|          |rd21
    !
    !                                ix1        ix2
    !
    !
    !     internal parameters
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr.   id.    subroutine tracing.
    !      tick21    subr. w3timemd advance time.
    !      dsec21    func.   id.    difference between times.
    !      w3gsuc    func. w3gsrumd create grid-search-utility object
    !      w3gsud    subr. w3gsrumd destroy grid-search-utility object
    !      w3grmp    func. w3gsrumd compute interpolation weights
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      ww3_prep  prog.   n/a    input data preprocessor.
    !      ......    prog.   n/a    any other program that reads or
    !                               writes wavewatch iii data files.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !     - land points in the input grid are taken out of the interp.
    !       algorithm. if this results in zero weight factors through the
    !       interpolation box in the input grid, the closest 2 sea point
    !       for an extended 4x4 grid are used for interpolation, weighted
    !       by the inverse distance.
    !     - the "closed" variable comes from ww3_prep.inp and is associated
    !       with the input grid (e.g. grid that winds are provided on).
    !       it is a logical, not an integer, so it only allows two cases:
    !       no closure, or simple closure. "ww3_prep" only supports these
    !       two (not tripole).
    !
    !  8. structure :
    !
    !     -----------------------------------------------------------------
    !      1.  initializations.
    !        a initialize counters and factors.
    !        b setup logical mask
    !        c create grid-search-utility object
    !      2.  loop over output grid
    !        a check if sea point
    !        b find enclosing cell and compute interpolation weights using
    !          w3grmp
    !        c non-masked or partially masked cell
    !        d fully masked cell
    !        e update overlay map
    !      2.  finalizations.
    !        a final output
    !        b destroy grid-search-utility object
    !     -----------------------------------------------------------------
    !
    !  9. switches :
    !
    !     !/s   enable subroutine tracing.
    !
    !     !/t   enable limited test output.
    !     !/t1  enable full debugging in w3grmp
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use w3gsrumd
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: ndsm, ndst, ndse, mx, my, nx, ny,    &
         mxi, myi, nxi, nyi, mask(mxi,myi)
    integer, intent(inout)  :: mapovr(mx,my), iland
    integer, intent(out)    :: ierr, ix1(mx,my), ix2(mx,my),        &
         iy1(mx,my), iy2(mx,my)
    real, intent(in)        :: tlat(my,mx), tlon(my,mx)
    real, intent(in)   ,target :: alat(mxi,myi)
    real, intent(inout),target :: alon(mxi,myi)
    real, intent(out)       :: rd11(mx,my), rd12(mx,my),            &
         rd21(mx,my), rd22(mx,my)
    logical, intent(in)     :: flagll, closed
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    type(t_gsu)             :: gsu
    integer                 :: ix, iy, i, j, nnbr, ii(4), jj(4),    &
         mskc, ifound, imask, icor1
    real                    :: rr(4), x, y
    real, pointer           :: plat(:,:), plon(:,:)
    logical                 :: ingrid, lmsk(mxi,myi)
    logical                 :: ldbg = .false.
    integer, parameter      :: nnbr_max = 2
    integer                 :: iclo
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !
    ! 1.  initializations ------------------------------------------------ *
    ! 1.a initialize counters and factors
    !
    ierr   = 0
    ifound = 0
    imask  = 0
    icor1  = 0
    iclo   = iclo_none
    if ( flagll .and. closed ) iclo = iclo_smpl
    !
    do  ix=1, nx
      do  iy=1, ny
        rd11(ix,iy) = 0.
        rd12(ix,iy) = 0.
        rd21(ix,iy) = 0.
        rd22(ix,iy) = 0.
        ix1(ix,iy)  = 1
        ix2(ix,iy)  = 1
        iy1(ix,iy)  = 1
        iy2(ix,iy)  = 1
      end do
    end do
    !
    ! 1.b setup logical mask
    !
    lmsk = mask .eq. 0
    !
    ! 1.c create grid-search-utility object for input grid
    !
    plat => alat
    plon => alon
    gsu = w3gsuc( .true., flagll, iclo, plon, plat )
    !
    !
    ! 2.  loop over output grid ------------------------------------------ *
    !
    do iy=1, ny
      do  ix=1, nx
        !
        x = tlon(iy,ix)
        y = tlat(iy,ix)
        !
        ! 2.a check if sea point
        !
        if ( mapovr(ix,iy) .ne. iland ) then
          !
          ! 2.b find enclosing cell and compute interpolation weights
          !
          nnbr = nnbr_max
          ingrid = w3grmp( gsu, x, y, ii, jj, rr, &
               mask=lmsk, mskc=mskc, nnbr=nnbr, debug=ldbg )
          !
          if ( ingrid ) then
            !
            ! 2.c non-masked or partially masked cell: simply store the weights
            !
            if ( mskc.eq.mskc_none .or. mskc.eq.mskc_part ) then
              !
              if ( mskc.eq.mskc_part ) imask  = imask + 1
              !
              ! ..... here we switch from counter-clockwise order to column-major
              ix1 (ix,iy) = ii(1)
              ix2 (ix,iy) = ii(2)
              iy1 (ix,iy) = jj(1)
              iy2 (ix,iy) = jj(4)
              rd11(ix,iy) = rr(1)
              rd21(ix,iy) = rr(2)
              rd12(ix,iy) = rr(4)
              rd22(ix,iy) = rr(3)
              !
              ! 2.d fully masked cell
              !
            else !mskc.eq.mskc_full
              !
              imask  = imask + 1
              !
              if ( nnbr .gt. 0 ) then
                icor1  = icor1 + 1
                ix1 (ix,iy) = ii(1)
                iy1 (ix,iy) = jj(1)
                rd11(ix,iy) = rr(1)
                if ( nnbr .gt. 1 ) then
                  ix1 (ix,iy) = ii(2)
                  iy1 (ix,iy) = jj(2)
                  rd22(ix,iy) = rr(2)
                end if
              else
                ierr   = ierr + 1
                write (ndse,910) ix, iy, x, y,    &
                     ii(1), ii(2), jj(1), jj(2)
              end if ! nnbr
              !
            end if ! mskc
            !
            !
            ! 2.e update overlay map
            !
            mapovr(ix,iy) = mapovr(ix,iy) + 1
            ifound  = ifound + 1
            !
          end if ! ingrid
        endif ! sea-point
        !
        ! ... end loop over output grid -------------------------------------- *
        !
      end do
    end do
    !
    ! 3.  finalizations -------------------------------------------------- *
    ! 3.a final output
    !
    if (ndsm.ne.0) write (ndsm,900) ifound, imask, icor1, ierr
    !
    ! 3.b destroy grid-search-utility object
    !
    call w3gsud(gsu)
    !
    return
    !
    ! formats
    !
900 format (/' *** message w3fldp: final sea point count     :',i8/ &
         '                     interpolation across shore:',i8/ &
         '                     corrected coastal points  :',i8/ &
         '                     uncorrectable c. points   :',i8/)
    !
910 format ( ' *** warning w3fldp : sea point on land mask ', &
         '(could not be corrected)'/                 &
         '     coordinates in output grid :',2i4,2f8.2/   &
         '     x-counters in input grid   :',2i4/         &
         '     y-counters in input grid   :',2i4)
    !
    !
    !
    !/
    !/ end of w3fldp ----------------------------------------------------- /
    !/
  end subroutine w3fldp
  !/ ------------------------------------------------------------------- /
  subroutine w3fldh (j, ndst, ndse, mx, my, nx, ny, t0, tn,       &
       nh, nhm, tho, ha, hd, hs, tf0, fx0, fy0, fs0,&
       tfn, fxn, fyn, fsn, ierr)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    15-jan-1999 : final fortran 77                    ( version 1.18 )
    !/    30-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    04-sep-2003 : bug fix par. list declaration.      ( version 3.04 )
    !/    05-jul-2005 : correct first level/ice.            ( version 3.07 )
    !/    15-may-2018 : allow homog ice.                    ( version 6.05 )
    !/    22-mar-2021 : adds momentum and density input     ( version 7.13 )
    !/
    !  1. purpose :
    !
    !     update homogeneous input fields for the wavewatch iii generic
    !     shell.
    !
    !  2. method :
    !
    !     variables defining the homogeneous fields are transfered through
    !     the parameter list (see section 3).
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       j       int    i   field number of input field as in shell.
    !                          -7 : ice parameter 1
    !                          -6 : ice parameter 2
    !                          -5 : ice parameter 3
    !                          -4 : ice parameter 4
    !                          -3 : ice parameter 5
    !                          -2 : mud parameter 1
    !                          -1 : mud parameter 2
    !                           0 : mud parameter 3
    !                           1 : water levels
    !                           2 : currents
    !                           3 : winds
    !                           4 : ice
    !                           5 : atmospheric momentum
    !                           6 : air density
    !                          10 : moving grid
    !       ndst    int.   i   unit number test output.
    !       ndse    int.   i   unit number error messages.
    !                          (no output if ndse < 0).
    !       mx,my   int.   i   array dimensions output fields.
    !       nx,ny   int.   i   field dimensions output fields.
    !       t0-n    i.a.   i   time interval considered.
    !       nh      int.  i/o  number of homogeneous fields j.
    !       nhm     int.   i   array dimension corresponding to nh.
    !       tho     i.a.  i/o  times for all homogeneous fields left.
    !       ha      r.a.  i/o  id. amplitude.
    !       hd      r.a.  i/o  id. direction (degr., naut.).
    !       hs      r.a.  i/o  id. air-sea temperature difference (degr.).
    !       tf0-n   i.a.  i/o  times of input fields
    !       fxx     r.a.  i/o  input fields (x, y, scalar)
    !       ierr    int.   o   error indicator,
    !                           0 ok,
    !                           1 illegal field number
    !                          -1 past last data
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr.   id.    subroutine tracing.
    !      tick21    subr. w3timemd advance time.
    !      dsec21    func.   id.    difference between times.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      ww3_shel  prog.   n/a    basic wave model driver.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     - see end of subroutine.
    !     - array dimensions not checked.
    !
    !  7. remarks :
    !
    !     - no homogeneous ice fields available.
    !     - previous fields needed only for 2-d fields.
    !
    !  8. structure :
    !
    !       see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !     !/t  enable test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use w3timemd
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: j, ndst, ndse, mx, my, nx, ny,       &
         t0(2), tn(2), nhm
    integer, intent(inout)  :: nh, tho(2,-7:10,nhm), tf0(2), tfn(2)
    integer, intent(out)    :: ierr
    real, intent(inout)     :: ha(nhm,-7:10), hd(nhm,-7:10), hs(nhm,-7:10), &
         fx0(mx,my), fy0(mx,my), fs0(mx,my),  &
         fxn(mx,my), fyn(mx,my), fsn(mx,my)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ix, iy, i
    real                    :: x, y, dir, dttst, dera
    logical                 :: flfrst
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ierr   = 0
    dera   = atan(1.)/45.
    !
    !
    ! test field id number for validity
    !
    if ( j.lt.-7 .or. j .gt.10 ) goto 801
    flfrst = tfn(1) .eq. -1
    !
    !
    ! loop over times / fields ========================================== *
    !
    do
      !
      ! shift fields
      !
      tf0(1) = tfn(1)
      tf0(2) = tfn(2)
      if ( tfn(1) .ne. -1 ) then
        if ( (j .eq. 2) .or. (j .eq. 5) ) then
          do ix=1, nx
            do iy=1, ny
              fx0(ix,iy) = fxn(ix,iy)
              fy0(ix,iy) = fyn(ix,iy)
            end do
          end do
        else if ( j .eq. 3 ) then
          do ix=1, nx
            do iy=1, ny
              fx0(ix,iy) = fxn(ix,iy)
              fy0(ix,iy) = fyn(ix,iy)
              fs0(ix,iy) = fsn(ix,iy)
            end do
          end do
        end if
      end if
      !
      ! new field
      !
      if ( nh .ne. 0. ) then
        tfn(1) = tho(1,j,1)
        tfn(2) = tho(2,j,1)
        ! ic* md* lev ice
        if ( (j.le.1) .or. (j.eq.4) .or. (j.eq.6) ) then
          do ix=1, nx
            do iy=1, ny
              fsn(ix,iy) = ha(1,j)
            end do
          end do
        end if
        ! cur
        if ( (j .eq. 2) .or. (j .eq. 5) ) then
          dir    = ( 270. - hd(1,j) ) * dera
          x      = ha(1,j) * cos(dir)
          y      = ha(1,j) * sin(dir)
          do ix=1, nx
            do iy=1, ny
              fxn(ix,iy) = x
              fyn(ix,iy) = y
            end do
          end do
        end if
        ! wnd
        if ( j .eq. 3 ) then
          dir    = ( 270. - hd(1,j) ) * dera
          x      = ha(1,j) * cos(dir)
          y      = ha(1,j) * sin(dir)
          do ix=1, nx
            do iy=1, ny
              fxn(ix,iy) = x
              fyn(ix,iy) = y
              fsn(ix,iy) = hs(1,j)
            end do
          end do
        end if
        !
        ! shift data arrays
        !
        do i=1, nh-1
          tho(1,j,i) = tho(1,j,i+1)
          tho(2,j,i) = tho(2,j,i+1)
          ha(i,j)    = ha(i+1,j)
          hd(i,j)    = hd(i+1,j)
          hs(i,j)    = hs(i+1,j)
        end do
        nh      = nh - 1
        !
      else
        !
        tfn(1) = tn(1)
        tfn(2) = tn(2)
        call tick21 ( tfn , 1. )
        ierr   = -1
        !
      end if
      !
      ! check time
      !
      dttst  = dsec21 ( t0 , tfn )
      ! exit if field time is later than run time
      if ( dttst .gt. 0. ) exit
      ! exit if field is ic* or md* or lev or ice
      ! and first forcing field has been stored
      ! at start run time
      if ( j.le.(1).or.(j.eq.4).or.(j.eq.6) ) then
        if (flfrst .and. dttst.eq.0. ) exit
      end if
    end do
    !
    ! check if first field
    !
    if ( j.ne.1 .and. tfn(1) .eq. -1 ) then
      tf0(1) = t0(1)
      tf0(2) = t0(2)
      !
      do ix=1, nx
        do iy=1, ny
          fx0(ix,iy) = fxn(ix,iy)
          fy0(ix,iy) = fyn(ix,iy)
          fs0(ix,iy) = fsn(ix,iy)
        end do
      end do
    end if
    !
    !
    return
    !
    ! error escape locations
    !
801 continue
    if ( ndse .ge. 0 ) write (ndse,1001) j
    ierr   = 1
    return
    !
    ! formats
    !
1001 format (/' *** wavewatch iii error in w3fldh : '/               &
         '     illegal field id nr : ',i4/)
    !
    !/
    !/ end of w3fldh ----------------------------------------------------- /
    !/
  end subroutine w3fldh
  !/ ------------------------------------------------------------------- /
  subroutine w3fldm (j, ndst, ndse, t0, tn, nh, nhm, tho, ha, hd, &
       tf0, a0, d0, tfn, an, dn, ierr)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         26-dec-2002 |
    !/                  +-----------------------------------+
    !/
    !/    26-dec-2002 : origination.                        ( version 3.02 )
    !/
    !  1. purpose :
    !
    !     update moving grid info for the wavewatch iii generic
    !     shell.
    !
    !  2. method :
    !
    !     variables defining the homogeneous fields are transfered through
    !     the parameter list (see section 3).
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       j       int    i   field number, should be 4.
    !       ndst    int.   i   unit number test output.
    !       ndse    int.   i   unit number error messages.
    !                          (no output if ndse < 0).
    !       t0-n    i.a.   i   time interval considered.
    !       nh      int.  i/o  number of homogeneous fields j.
    !       nhm     int.   i   array dimension corresponding to nh.
    !       tho     i.a.  i/o  times for all homogeneous fields left.
    !       ha      r.a.  i/o  id. amplitude.
    !       hd      r.a.  i/o  id. direction (degr., naut.).
    !       tf0-n   i.a.  i/o  times of input fields
    !       a/d0/n  r.a.  i/o  input data.
    !       ierr    int.   o   error indicator,
    !                           0 ok,
    !                           1 illegal field number
    !                          -1 past last data
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr.   id.    subroutine tracing.
    !      tick21    subr. w3timemd advance time.
    !      dsec21    func.   id.    difference between times.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      ww3_shel  prog.   n/a    basic wave model driver.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     - see end of subroutine.
    !     - array dimensions not checked.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !       see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !     !/t  enable test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use w3timemd
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: j, ndst, ndse, t0(2), tn(2), nhm
    integer, intent(inout)  :: nh, tho(2,-7:10,nhm), tf0(2), tfn(2)
    integer, intent(out)    :: ierr
    real, intent(inout)     :: ha(nhm,-7:10), hd(nhm,-7:10), a0, an, d0, dn
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: i
    real                    :: dttst, dera
    logical                 :: flfrst
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ierr   = 0
    dera   = atan(1.)/45.
    !
    !
    ! test field id number for validity
    !
    if ( j .ne. 4 ) goto 801
    flfrst = tfn(1) .eq. -1
    !
    !
    ! backward branch point ============================================= *
    !
100 continue
    !
    ! shift data
    !
    tf0(1) = tfn(1)
    tf0(2) = tfn(2)
    if ( tfn(1) .ne. -1 ) then
      a0     = an
      d0     = dn
    end if
    !
    ! new field
    !
    if ( nh .ne. 0. ) then
      tfn(1) = tho(1,j,1)
      tfn(2) = tho(2,j,1)
      an     = ha(1,j)
      dn     = ( 90. - hd(1,j) ) * dera
      !
      ! shift data arrays
      !
      do i=1, nh-1
        tho(1,j,i) = tho(1,j,i+1)
        tho(2,j,i) = tho(2,j,i+1)
        ha(i,j)    = ha(i+1,j)
        hd(i,j)    = hd(i+1,j)
      end do
      nh      = nh - 1
      !
    else
      !
      tfn(1) = tn(1)
      tfn(2) = tn(2)
      call tick21 ( tfn , 1. )
      ierr   = -1
      !
    end if
    !
    ! check time
    !
    dttst  = dsec21 ( t0 , tfn )
    if ( dttst .le. 0. ) goto 100
    !
    ! check if first field
    !
    if ( tf0(1).eq.-1 ) then
      tf0(1) = t0(1)
      tf0(2) = t0(2)
      a0     = an
      d0     = dn
    end if
    !
    !
    return
    !
    ! error escape locations
    !
801 continue
    if ( ndse .ge. 0 ) write (ndse,1001) j
    ierr   = 1
    return
    !
    ! formats
    !
1001 format (/' *** wavewatch iii error in w3fldm : '/               &
         '     illegal field id nr : ',i4/)
    !
    !/
    !/ end of w3fldm ----------------------------------------------------- /
    !/
  end subroutine w3fldm
  !/
  !/ end of module w3fldsmd -------------------------------------------- /
  !/
end module w3fldsmd
