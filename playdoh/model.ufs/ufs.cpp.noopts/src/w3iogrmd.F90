!> @file
!> @brief reading/writing of model definition file.
!>
!> @author h. l. tolman
!> @author f. ardhuin
!> @date   15-apr-2020
!>
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
!>
!> @brief reading/writing of model definition file.
!>
!> @details arrays allocated here on read or ing ww3_grid on write.
!>
!> @author h. l. tolman
!> @author f. ardhuin
!> @date   15-apr-2020
!>
module w3iogrmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  !            f. ardhuin             !
  !/                  |                        fortran 90 |
  !/                  | last update :         15-apr-2020 |
  !/                  +-----------------------------------+
  !/
  !/    for updates see w3iogr documentation.
  !/
  !  1. purpose :
  !
  !     reading/writing of model definition file .
  !
  !  2. variables and types :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      vergrd    c*10  private  model definition file version number.
  !      idstr     c*35  private  model definition file id string.
  !     ----------------------------------------------------------------
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3iogr    subr. public   read/write model definition file.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      w3setg    subr. w3gdatmd point to data structure for spatial gr.
  !      w3dimx    subr.    id.   set up arrays for spatial grid.
  !      w3dims    subr.    id.   set array dimensions for a spec. grid.
  !      w3seto    subr. w3odatmd point to data structure for spatial gr.
  !      w3dmo5    subr.    id.   set array dimensions.
  !      inptab    subr. w3src2md fill interpolation tables for
  !                               dispersion relation.
  !      distab    subr. w3dispmd input coefficient lookup table.
  !      insnl1    subr. w3snl1md initialization of the dia.
  !      insnl2    subr. w3snl2md initialization of wrt.
  !      insnl3    subr. w3snl3md initialization of gmd.
  !      insnl5    subr. w3snl5md initialization of gke.
  !      insnls    subr. w3snlsmd initialization of nonlinear `smoother'.
  !      strace    subr. w3servmd subroutine tracing.
  !      extcde    subr. w3servmd abort program with exit code.
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !
  !     - arrays allocated here on read or ing ww3_grid on write.
  !
  !  6. switches :
  !
  !     see subroutine.
  !
  !  7. source code :
  !
  !/ ------------------------------------------------------------------- /
  ! module default
  implicit none
  public
  !/
  !/ private parameter statements (id strings)
  !/
  character(len=10), parameter, private :: vergrd = '2021-08-06'
  character(len=35), parameter, private ::                        &
       idstr = 'wavewatch iii model definition file'
  !/
  !/ public variables
  !/
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief reading and writing of the model definition file.
  !>
  !> @details the file is opened within the routine, the name is pre-defined
  !>  and the unit number is given in the parameter list. the model
  !>  definition file is written using unformatted write statements.
  !>
  !> @param[in] inxout  test string for read/write.
  !> @param[in] ndsm    file unit number.
  !> @param[in] imod    model number for w3gdat etc.
  !> @param[in] fext    file extension to be used.
  !>
  !> @author h. l. tolman
  !> @author f. ardhuin
  !> @date   19-oct-2020
  subroutine w3iogr ( inxout, ndsm, imod, fext )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  !            f. ardhuin             !
    !/                  |                        fortran 90 |
    !/                  | last update :         19-oct-2020 |
    !/                  +-----------------------------------+
    !/
    !/    14-jan-1999 : distributed fortran 77 version.     ( version 1.18 )
    !/    04-feb-2000 : upgrade to fortran 90               ( version 2.00 )
    !/                  major changes to logistics.
    !/    14-feb-2000 : exact-nl added.                     ( version 2.01 )
    !/    09-jan-2001 : flat grid option.                   ( version 2.06 )
    !/    02-feb-2001 : exact-nl version 3.0                ( version 2.07 )
    !/    27-feb-2001 : third propagation scheme added.     ( version 2.08 )
    !/    16-mar-2001 : fourth propagation scheme added.    ( version 2.09 )
    !/    29-mar-2001 : sub-grid islands added.             ( version 2.10 )
    !/    11-jan-2002 : sub-grid ice added.                 ( version 2.15 )
    !/    09-may-2002 : switch clean up.                    ( version 2.21 )
    !/    27-aug-2002 : exact-nl version 4.0                ( version 2.22 )
    !/    26-nov-2002 : adding first vdia and mdia.         ( version 3.01 )
    !/    01-aug-2003 : adding moving grid gse correction.  ( version 3.03 )
    !/    08-mar-2004 : multiple grid version.              ( version 3.06 )
    !/    04-may-2005 : change to mpi_comm_wave.            ( version 3.07 )
    !/    24-jun-2005 : add mapst2 processing.              ( version 3.07 )
    !/    09-nov-2005 : remove soft boundary options.       ( version 3.08 )
    !/    23-jun-2006 : add w3sln1 parameters.              ( version 3.09 )
    !/    27-jun-2006 : adding file name preamble.          ( version 3.09 )
    !/    25-jul-2006 : reorder for 'grid' option to read   ( version 3.10 )
    !/                  spectral data also.
    !/    28-oct-2006 : add partitioning pars.              ( version 3.10 )
    !/    26-mar-2007 : add partitioning pars.              ( version 3.11 )
    !/    16-apr-2006 : add miche limiter pars.             ( version 3.11 )
    !/    25-apr-2007 : adding battjes-janssen sdb.         ( version 3.11 )
    !/    09-oct-2007 : adding wam cycle 4+ sin and sds.    ( version 3.13 )
    !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
    !/    30-oct-2009 : fix ndst arg in call to w3dmo5.     ( version 3.14 )
    !/                  (t. j. campbell, nrl)
    !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    23-dec-2009 : addition of cou namelists           ( version 3.14 )
    !/    31-oct-2010 : implement unstructured grids        ( version 3.14 )
    !/                  (a. roland and f. ardhuin)
    !/    06-dec-2010 : change from global (logical) to iclose (integer) to
    !/                  specify index closure for a grid.   ( version 3.14 )
    !/                  (t. j. campbell, nrl)
    !/    12-jun-2012 : add /rtd option or rotated grid option.
    !/                  (jian-guo li)                       ( version 4.06 )
    !/    13-jul-2012 : move gmd (snl3) and nonlinear filter (snls)
    !/                  from 3.15 (hlt).                    ( version 4.08 )
    !/    12-dec-2012 : adding smc grid.  jg_li             ( version 4.08 )
    !/    19-dec-2012 : add noswll to file.                 ( version 4.11 )
    !/    01-jul-2013 : document uq / uno switches in file  ( version 4.12 )
    !/    10-sep-2013 : add ig1 parameters                  ( version 4.12 )
    !/    16-sep-2013 : add arctic part in smc grid.        ( version 4.12 )
    !/    11-nov-2013 : make smc and rtd grids compatible.  ( version 4.13 )
    !/    06-mar-2014 : writes out a help message on error  ( version 4.18 )
    !/    10-mar-2014 : add ic2 parameters                  ( version 5.01 )
    !/    29-may-2014 : add ic3 parameters                  ( version 5.01 )
    !/    20-aug-2016 : add iobpa                           ( version 5.12 )
    !/    08-mar-2018 : add fswnd for smc grid.             ( version 6.02 )
    !/    05-jun-2018 : add pdlib/debuginit and implcit scheme parameters
    !/                  for unstructured grids              ( version 6.04 )
    !/    27-jul-2018 : added ptmeth and ptfcut parameters  ( version 6.05 )
    !/                  (c. bunney, ukmo)
    !/    18-aug-2018 : s_{ice} ic5 (q. liu)                ( version 6.06 )
    !/    26-aug-2018 : uost (mentaschi et al. 2015, 2018)  ( version 6.06 )
    !/    15-apr-2020 : adds optional opt-out for cfl on bc ( version 7.08 )
    !/    18-jun-2020 : adds 360-day calendar option        ( version 7.08 )
    !/    19-oct-2020 : add aircmin, airgb parameters       ( version 7.08 )
    !/    07-07-2021  : s_{nl} gke nl5 (q. liu)             ( version 7.12 )
    !/    19-jul-2021 : momentum and air density support    ( version 7.14 )
    !/
    !/    copyright 2009-2013 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    !  1. purpose :
    !
    !     reading and writing of the model definition file.
    !
    !  2. method :
    !
    !     the file is opened within the routine, the name is pre-defined
    !     and the unit number is given in the parameter list. the model
    !     definition file is written using unformatted write statements.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       inxout  c*(*)  i   test string for read/write, valid are:
    !                         'read',  'write' and 'grid'.
    !       ndsm    int.   i   file unit number.
    !       imod    int.   i   model number for w3gdat etc.
    !       fext    c*(*)  i   file extension to be used.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     see above.
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3init    subr. w3initmd wave model initialization routine.
    !      ......    prog.   n/a    all wavewatch iii aux programs and
    !                               drivers.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !       tests on inxout, file status and on array dimensions.
    !
    !  7. remarks :
    !
    !     - the model definition file has the pre-defined name
    !       'mod_def.filext'.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/mpi  mpi calls
    !
    !     !/lnn  select source terms
    !     !/stn
    !     !/nln
    !     !/btn
    !     !/dbn
    !     !/trn
    !     !/bsn
    !     !/xxn
    !
    !     !/s    enable subroutine tracing.
    !     !/t    enable test output
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3servmd, only : print_memcheck
    use constants
    use w3gdatmd
    use w3adatmd, only: mpi_comm_wave
    use w3odatmd
    use w3timemd, only: caltype
    use w3servmd, only: extcde
    use w3dispmd
    !
    include "mpif.h"
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)             :: ndsm
    integer, intent(in), optional   :: imod
    character, intent(in)           :: inxout*(*)
    character, intent(in), optional :: fext*(*)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: igrd, ierr, i, j, mth, mk, isea, ix, iy
    integer                 :: iext, ipre
    integer, allocatable    :: maptmp(:,:)
    integer                 :: ierr_mpi, ip
    logical                 :: write, fltest = .false., testll,     &
         flsnl2 = .false.
    logical, save           :: flinp = .false. , fldisp = .false.,  &
         flis  = .false.
    character(len=10)       :: vertst
    character(len=13)       :: tempxt
    character(len=30)       :: tname0, tname1, tname2, tname3,      &
         tname4, tname5, tname6,              &
         tnamep, tnameg, tnamef, tnamei
    character(len=30)       :: fname0, fname1, fname2, fname3,      &
         fname4, fname5, fname6,              &
         fnamep, fnameg, fnamef, fnamei
    character(len=35)       :: idtst
    character(len=60)       :: message(5)
    logical                 :: global
    real, allocatable       :: xgrd4(:,:), ygrd4(:,:)
    integer                 :: memunit
    !/
    !/ ------------------------------------------------------------------- /
    !/
    memunit = 740+iaproc
    !
    call print_memcheck(memunit, 'memcheck_____:'//' wiogr section 1')
    message =(/ '     mod def file was generated with a different    ', &
         '     ww3 version or using a different switch file.  ',        &
         '     make sure ww3_grid is compiled with same switch',        &
         '     as ww3_shel or ww3_multi, run ww3_grid again   ',        &
         '     and then try again the program you just used.  '/)
    !
    tnamef = '------------------------------'
    tname0 = '------------------------------'
    tname1 = '------------------------------'
    tname2 = '------------------------------'
    tname3 = '------------------------------'
    tname4 = '------------------------------'
    tname5 = '------------------------------'
    tname6 = '------------------------------'
    tnamep = '------------------------------'
    tnameg = '------------------------------'
    tnamei = '------------------------------'
    !
    !
    fnamef = tnamef
    fname0 = tname0
    fname1 = tname1
    fname2 = tname2
    fname3 = tname3
    fname4 = tname4
    fname5 = tname5
    fname6 = tname6
    fnamep = tnamep
    fnameg = tnameg
    fnamei = tnamei
    !
    !
    ! test input parameters ---------------------------------------------- *
    !
    if ( present(imod) ) then
      igrd   = imod
    else
      igrd   = 1
    end if
    !
    if ( present(fext) ) then
      tempxt = fext
    else
      tempxt = 'ww3'
    end if
    !
    if (inxout.ne.'read' .and. inxout.ne.'write'                    &
         .and. inxout.ne.'grid') then
      if ( iaproc .eq. naperr ) write (ndse,900) inxout
      call extcde ( 1 )
    end if
    !
    write  = inxout .eq. 'write'
    !
    !
    call w3seto ( igrd, ndse, ndst )
    call w3setg ( igrd, ndse, ndst )
    filext = tempxt
    call print_memcheck(memunit, 'memcheck_____:'//' wiogr section 2')
    !
    ! open file ---------------------------------------------------------- *
    !
    iext   = len_trim(filext)
    ipre   = len_trim(fnmpre)
    !
    !ar: add debugflag      write(*,*) 'file=', fnmpre(:ipre)//'mod_def.'//filext(:iext)
    if ( write ) then
      open (ndsm,file=fnmpre(:ipre)//'mod_def.'//filext(:iext),   &
           form='unformatted', convert=file_endian,err=800,iostat=ierr)
    else
      open (ndsm,file=fnmpre(:ipre)//'mod_def.'//filext(:iext),   &
           form='unformatted', convert=file_endian,status='old',err=800,iostat=ierr)
    endif
    !
    rewind ( ndsm )
    !
    ! dimensions and test information --------------------------------------
    !
    if ( write ) then
      write (ndsm)                                               &
           idstr, vergrd, nx, ny, nsea, nth, nk,                 &
           nbi, nfbpo, gname, fname0, fname1, fname2, fname3,    &
           fname4, fname5, fname6, fnamep, fnameg,               &
           fnamef, fnamei
      !
      !
      write (ndsm)                                               &
           (nbo(i),i=0,nfbpo), (nbo2(i),i=0,nfbpo)
    else
      read (ndsm,end=801,err=802,iostat=ierr)                    &
           idtst, vertst, nx, ny, nsea, mth, mk,                 &
           nbi, nfbpo, gname, fname0, fname1, fname2, fname3,    &
           fname4, fname5, fname6, fnamep, fnameg,               &
           fnamef, fnamei
      !
      !
      nk     = mk
      nth    = mth
      nk2    = nk + 2
      nspec  = nk * nth
      !
      if ( idtst .ne. idstr ) then
        if ( iaproc .eq. naperr )                               &
             write (ndse,901) idtst, idstr
        call extcde ( 10 )
      end if
      if ( vertst .ne. vergrd ) then
        if ( iaproc .eq. naperr )                               &
             write (ndse,902) vertst, vergrd
        call extcde ( 11 )
      end if
      if ( nfbpo .gt. 9 ) then
        if ( iaproc .eq. naperr )                               &
             write (ndse,904) nfbpo, 9
        call extcde ( 13 )
      end if
      if ( fname0 .ne. tname0 ) then
        if ( iaproc .eq. naperr )                               &
             write (ndse,905) 0, filext(:iext), fname0, tname0, &
             message
        call extcde ( 14 )
      end if
      if ( fname1 .ne. tname1 ) then
        if ( iaproc .eq. naperr )                               &
             write (ndse,905) 1, filext(:iext), fname1, tname1, &
             message
        call extcde ( 15 )
      end if
      if ( fname2 .ne. tname2 ) then
        if ( iaproc .eq. naperr )                               &
             write (ndse,905) 2, filext(:iext), fname2, tname2, &
             message
        call extcde ( 16 )
      end if
      if ( fname3 .ne. tname3 ) then
        if ( iaproc .eq. naperr )                               &
             write (ndse,905) 3, filext(:iext), fname3, tname3, &
             message
        call extcde ( 17 )
      end if
      if ( fnamei .ne. tnamei ) then
        if ( iaproc .eq. naperr )                               &
             write (ndse,905) 3, filext(:iext), fnamei, tnamei, &
             message
        call extcde ( 17 )
      end if
      if ( fname4 .ne. tname4 ) then
        if ( iaproc .eq. naperr )                               &
             write (ndse,905) 4, filext(:iext), fname4, tname4, &
             message
        call extcde ( 18 )
      end if
      if ( fname5 .ne. tname5 ) then
        if ( iaproc .eq. naperr )                               &
             write (ndse,905) 5, filext(:iext), fname5, tname5, &
             message
        call extcde ( 19 )
      end if
      if ( fname6 .ne. tname6 ) then
        if ( iaproc .eq. naperr )                               &
             write (ndse,905) 6, filext(:iext), fname6, tname6, &
             message
        call extcde ( 20 )
      end if
      if ( fnamep .ne. tnamep ) then
        if ( iaproc .eq. naperr )                               &
             write (ndse,906) fnamep, tnamep
        call extcde ( 22 )
      end if
      if ( fnameg .ne. tnameg ) then
        if ( iaproc .eq. naperr )                               &
             write (ndse,907) fnameg, tnameg, message
        call extcde ( 22 )
      end if
      if ( fnamef .ne. tnamef ) then
        if ( iaproc .eq. naperr )                               &
             write (ndse,908) filext(:iext), fnamef, tnamef, message
        call extcde ( 24 )
      end if
      !
      read (ndsm,end=801,err=802,iostat=ierr)                   &
           (nbo(i),i=0,nfbpo), (nbo2(i),i=0,nfbpo)
      !
    endif
    call print_memcheck(memunit, 'memcheck_____:'//' wiogr section 3')
    !
    ! parameters in modules  --------------------------------------------- *
    !                                                   module w3gdat grid
    !
    allocate ( maptmp(ny,nx) )
    !
    if ( write ) then
      maptmp = mapsta + 8*mapst2
      write (ndsm)                                            &
           gtype, flagll, iclose
      !
      ! writes different kind of information depending on grid type
      !
      select case ( gtype )
        !!li  smctype shares info with rlgtype.   jgli12oct2020
      case ( rlgtype, smctype )
        write (ndsm)                                          &
             sx, sy, x0, y0
      case ( clgtype )
        write (ndsm)                                          &
             real(xgrd), real(ygrd)
      case (ungtype)
        write (ndsm)                                          &
             fsn, fspsi,fsfct,fsnimp,fstotalimp,fstotalexp,   &
             fsbccfl, fsrefraction, fsfreqshift, fssource,    &
             do_change_wlv, solverthr_stp, crit_dep_stp,      &
             ntri,countot, countri, nnz,                      &
             b_jgs_terminate_maxiter,                         &
             b_jgs_terminate_difference,                      &
             b_jgs_terminate_norm,                            &
             b_jgs_limiter,                                   &
             b_jgs_block_gauss_seidel,                        &
             b_jgs_use_jacobi,                                &
             b_jgs_maxiter,                                   &
             b_jgs_pmin,                                      &
             b_jgs_diff_thr,                                  &
             b_jgs_norm_thr,                                  &
             b_jgs_nlevel,                                    &
             b_jgs_source_nonlinear
        !init countcon and iobdp to zero, it needs to be set somewhere or
        !removed
        countcon=0
        iobdp=0  
        write (ndsm)                                          &
             x0, y0, sx, sy, dxymax, xgrd, ygrd, trigp, tria, &
             len, ien, angle0, angle, si, maxx, maxy,         &
             dxymax, index_cell, ccon, countcon, ie_cell,     &
             pos_cell, iobp, iobpa, iobdp, iobpd, iaa, jaa, posi
      end select !gtype
      !
      write (ndsm)                                            &
           zb, maptmp, mapfs, mapsf, trflag
      !
      !
      if ( trflag .ne. 0 ) write (ndsm) trnx, trny
      write (ndsm)                     &
           dtcfl, dtcfli, dtmax, dtmin, dmin, ctmax,              &
           fice0, ficen, ficel, pfmove, fldry, flcx, flcy, flcth, &
           flck, flsou, flbpi, flbpo, clats, clatis, cthg0s,      &
           stexu, steyu, stedu, iicehmin, iicehinit, iicedisp,    &
           icescales(1:4), caltype, cmprtrck, iicehfac, iicehdisp,&
           iiceddisp, iicefdisp, btbeta,                          &
           aaircmin, aairgb
      write(ndsm)gridshift
      !!        write(ndsm)                                                 &
      !!             coug_2d, coug_rad3d, coug_us3d
    else
      call print_memcheck(memunit, 'memcheck_____:'//' wiogr section 4')
      read (ndsm,end=801,err=802,iostat=ierr)                      &
           gtype, flagll, iclose
      !!li      if (.not.ginit) call w3dimx ( igrd, nx, ny, nsea, ndse, ndst )
      if (.not.ginit) call w3dimx ( igrd, nx, ny, nsea, ndse, ndst &
           )
      !
      ! reads different kind of information depending on grid type
      !
      select case ( gtype )
        !!li  smctype shares info with rlgtype.   jgli12oct2020
      case ( rlgtype, smctype )
        read (ndsm,end=801,err=802,iostat=ierr)                 &
             sx, sy, x0, y0
        do ix=1,nx
          xgrd(:,ix) = real(x0 + real(ix-1)*sx)
        end do
        do iy=1,ny
          ygrd(iy,:) = real(y0 + real(iy-1)*sy)
        end do
      case ( clgtype )
        allocate(xgrd4(ny,nx),ygrd4(ny,nx)); xgrd4 = 0.; ygrd4 = 0.
        read (ndsm,end=801,err=802,iostat=ierr)               &
             xgrd4, ygrd4
        xgrd = xgrd4
        ygrd = ygrd4
        deallocate(xgrd4, ygrd4)
        !set sx, sy, x0, y0 to large values if curvilinear grid
        x0 = huge(x0); y0 = huge(y0)
        sx = huge(sx); sy = huge(sy)
      case (ungtype)
        read (ndsm,end=801,err=802,iostat=ierr)               &
             fsn, fspsi,fsfct,fsnimp,fstotalimp,fstotalexp,   &
             fsbccfl, fsrefraction, fsfreqshift, fssource,    &
             do_change_wlv, solverthr_stp, crit_dep_stp,      &
             ntri,countot, countri, nnz,                      &
             b_jgs_terminate_maxiter,                         &
             b_jgs_terminate_difference,                      &
             b_jgs_terminate_norm,                            &
             b_jgs_limiter,                                   &
             b_jgs_block_gauss_seidel,                        &
             b_jgs_use_jacobi,                                &
             b_jgs_maxiter,                                   &
             b_jgs_pmin,                                      &
             b_jgs_diff_thr,                                  &
             b_jgs_norm_thr,                                  &
             b_jgs_nlevel,                                    &
             b_jgs_source_nonlinear
        if (.not. guginit) then
          call w3dimug ( igrd, ntri, nx, countot, nnz, ndse, ndst )
        end if
        call print_memcheck(memunit, 'memcheck_____:'//' wiogr section 5')
        read (ndsm,end=801,err=802,iostat=ierr)               &
             x0, y0, sx, sy, dxymax, xgrd, ygrd, trigp, tria, &
             len, ien, angle0, angle, si, maxx, maxy,         &
             dxymax, index_cell, ccon, countcon, ie_cell,     &
             pos_cell, iobp, iobpa, iobdp, iobpd, iaa, jaa, posi
        call print_memcheck(memunit, 'memcheck_____:'//' wiogr section 6')
      end select !gtype
      !
      if (gtype.ne.ungtype) call w3gntx ( igrd, ndse, ndst )
      read (ndsm,end=801,err=802,iostat=ierr)   &
           zb, maptmp, mapfs, mapsf, trflag
      !
      !
      mapsta = mod(maptmp+2,8) - 2
      mapst2 = (maptmp-mapsta) / 8
      mapsf(:,3) = mapsf(:,2) + (mapsf(:,1)-1)*ny
      if ( trflag .ne. 0 ) then
        read (ndsm,end=801,err=802,iostat=ierr) trnx, trny
      end if
      read (ndsm,end=801,err=802,iostat=ierr)                     &
           dtcfl, dtcfli, dtmax, dtmin, dmin, ctmax,              &
           fice0, ficen, ficel, pfmove, fldry, flcx, flcy,        &
           flcth, flck, flsou, flbpi, flbpo, clats, clatis,       &
           cthg0s, stexu, steyu, stedu, iicehmin, iicehinit,      &
           iicedisp, icescales(1:4), caltype, cmprtrck, iicehfac, &
           iiceddisp, iicehdisp, iicefdisp, btbeta,               &
           aaircmin, aairgb
      read(ndsm,end=801,err=802,iostat=ierr)gridshift
      !
      !
    end if
    call print_memcheck(memunit, 'memcheck_____:'//' wiogr section 7')
    !
    !
    deallocate ( maptmp )
    !
    !
    ! spectral parameters ------------------------------------------------ *
    !                                                 module w3gdatmd sgrd
    !
    if ( write ) then
      write (ndsm)                                                  &
           mapwn, mapth, dth, th, esin, ecos, es2, esc, ec2,        &
           xfr, fr1, sig, sig2, dsip, dsii, dden, dden2, fte,       &
           ftf, ftwn, fttr, ftwl, facti1, facti2, fachfa, fachfe
    else
      if (.not.sinit) call w3dims ( igrd, nk, nth, ndse, ndst )
      read (ndsm,end=801,err=802,iostat=ierr)                       &
           mapwn, mapth, dth, th, esin, ecos, es2, esc, ec2,        &
           xfr, fr1, sig, sig2, dsip, dsii, dden, dden2, fte,       &
           ftf, ftwn, fttr, ftwl, facti1, facti2, fachfa, fachfe
    end if
    !
    !
    ! langmuir mixing parameterization --------------
    if ( write ) then
      write (ndsm)                                                &
        lmpenabled, sdtail, hslmode
      else
        read (ndsm,end=801,err=802,iostat=ierr)                     &
             lmpenabled, sdtail, hslmode
    end if
    ! --------------
    !
    ! output flags for 3d parameters ------------------------------------- *
    !                                                 module w3gdatmd
    if ( write ) then
      write (ndsm)                                                &
           e3df, p2msf, us3df,usspf, ussp_wn
    else
      read (ndsm,end=801,err=802,iostat=ierr)                     &
           e3df, p2msf, us3df,usspf, ussp_wn
    end if
    if ( inxout .eq. 'grid' ) then
      close (ndsm)
      return
    end if
    !
    ! parameters for output boundary points ------------------------------ *
    !                                                 module w3odatmd out5
    !
    if ( write ) then
      write (ndsm)                                               &
           xbpo, ybpo, rdbpo, ipbpo, isbpo
    else
      call w3dmo5 ( igrd, ndse, ndst, 2 )
      read (ndsm,end=801,err=802,iostat=ierr)                    &
           xbpo, ybpo, rdbpo, ipbpo, isbpo
    end if
    !
    !
    ! parameters for spectral partitioning  ------------------------------ *
    !                                                 module w3odatmd out6
    !
    if ( write ) then
      write (ndsm)                                               &
           ihmax, hspmin, wsmult, wscut, flcomb, noswll,         &
           ptmeth, ptfcut
    else
      read (ndsm,end=801,err=802,iostat=ierr)                    &
           ihmax, hspmin, wsmult, wscut, flcomb, noswll,         &
           ptmeth, ptfcut
    end if
    !
    !
    ! numerical parameters ----------------------------------------------- *
    !                                                 module w3gdatmd npar
    !
    if ( write ) then
      write (ndsm)                                               &
           facp, xrel, xflt, fxfm, fxpm, xft, xfc, facsd, fhmax, &
           ffacberg, delab, fwtable
    else
      read (ndsm,end=801,err=802,iostat=ierr)                    &
           facp, xrel, xflt, fxfm, fxpm, xft, xfc, facsd, fhmax, &
           ffacberg, delab, fwtable
    end if
    !
    !
    ! source term parameters --------------------------------------------- *
    !                                                 module w3gdatmd sflp
    !                                                 module w3gdatmd slnp
    !                                                 module w3gdatmd srcp
    !                                                 module w3gdatmd snlp
    !                                                 module w3gdatmd sbtp
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    ! ... nonlinear interactions
    !
    !
    !
    !
    !
    ! (ql: inxout = grid option ?)
    !
    !
    !
    ! layered barriers needed for file management in xnl_init
    !
    if ( flsnl2 .and. .not.write ) then
      do ip=1, iaproc-1
        call mpi_barrier (  mpi_comm_wave, ierr_mpi )
      end do
    end if
    if ( flsnl2 .and. .not.write ) then
      do ip=iaproc, naproc-1
        call mpi_barrier (  mpi_comm_wave, ierr_mpi )
      end do
    end if
    !
    ! ... bottom friction ...
    !
    !
    !
    !
    ! ... depth induced breaking ...
    !
    call print_memcheck(memunit, 'memcheck_____:'//' wiogr section 8')
    !
    !
    !
    ! propagation scheme ------------------------------------------------- *
    !                                                 module w3gdatmd prop
    !
    !
    !
    !
    !
    ! interpolation tables ( fill locally ) ----------------------------- *
    !                                                      module w3dispmd
    !
    if ( .not.write .and. .not.fldisp ) then
      call distab
      fldisp = .true.
    end if
    !
    close ( ndsm )
    call print_memcheck(memunit, 'memcheck_____:'//' wiogr section 9')
    !
    return
    !
    ! escape locations read errors --------------------------------------- *
    !
800 continue
    if ( iaproc .eq. naperr ) write (ndse,1000) filext(:iext), ierr
    call extcde ( 50 )
    !
801 continue
    if ( iaproc .eq. naperr ) write (ndse,1001) filext(:iext)
    call extcde ( 51 )
    !
802 continue
    if ( iaproc .eq. naperr ) write (ndse,1002) filext(:iext), ierr, &
         message
    call extcde ( 52 )
    !
    ! formats
    !
900 format (/' *** wavewatch iii error in w3iogr :'/         &
         '     ilegal inxout value: ',a/)
901 format (/' *** wavewatch iii error in w3iogr :'/         &
         '     ilegal idstr, read : ',a/                     &
         '                  check : ',a/)
902 format (/' *** wavewatch iii error in w3iogr :'/         &
         '     ilegal vergrd, read : ',a/                    &
         '                   check : ',a/)
904 format (/' *** wavewatch iii error in w3iogr :'/         &
         '     ilegal nfbpo read : ',i8/                     &
         '                 check : ',i8/)
905 format (/' *** wavewatch iii error in w3iogr :'/         &
         '     unexpected source term identifier',i2/        &
         '          in mod_def.',a,' file : ',a/             &
         '    expected from switch file : ',a,/              &
         5(a,/) /)
    !               '     check consistency of switches in programs'/)
906 format (/' *** wavewatch iii error in w3iogr :'/         &
         '     unexpected propagation scheme identifier'/    &
         '                in file :',a/                      &
         '               expected :',a/                      &
         '     check consistency of switches in programs'/)
907 format (/' *** wavewatch iii error in w3iogr :'/         &
         '     unexpected gse aleviation identifier'/        &
         '                in file :',a/                      &
         '               expected :',a/                      &
         , 5(a,/) /)
    !               '     check consistency of switches in programs'/)
908 format (/' *** wavewatch iii error in w3iogr :'/         &
         '     unexpected flux parameterization identifier'/ &
         '         in mod_def.',a,' :',a/                    &
         '               expected :',a/                      &
         , 5(a,/) /)
    !               '     check consistency of switches in programs'/)
    !
1000 format (/' *** wavewatch iii error in w3iogr : '/       &
         '     error in opening mod_def.',a,' file'/         &
         '     iostat =',i5/)
1001 format (/' *** wavewatch iii error in w3iogr : '/       &
         '     premature end of mod_def.',a,' file'/)
1002 format (/' *** wavewatch iii error in w3iogr : '/,      &
         '     error in reading from mod_def.',a,' file'/    &
         '     iostat =',i5,                                 &
         5(a,/) /)
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !/
    !/ end of w3iogr ----------------------------------------------------- /
    !/
  end subroutine w3iogr
  !/
  !/ end of module w3iogrmd -------------------------------------------- /
  !/
end module w3iogrmd
