!> @file
!> @brief process point output.
!>
!> @author h. l. tolman  @date 05-jun-2018
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
!> @brief process point output.
!>
!> @details allocation of allocatable arrays takes place at different
!>  places throughout the code, in w3iopp on write, and in w3iopo on
!>  read.
!>
!> @author h. l. tolman  @date 05-jun-2018
!>
module w3iopomd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         05-jun-2018 |
  !/                  +-----------------------------------+
  !/
  !/    25-jan-2001 : origination.                        ( version 2.00 )
  !/    24-jan-2001 : flat grid version.                  ( version 2.06 )
  !/    11-jun-2001 : clean-up.                           ( version 2.11 )
  !/    10-nov-2004 : multiple grid version.              ( version 3.06 )
  !/    27-jun-2006 : adding file name preamble.          ( version 3.09 )
  !/    25-jul-2006 : adding grid id per point.           ( version 3.10 )
  !/    01-may-2007 : move o7a output from w3init.        ( version 3.11 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/    30-oct-2009 : implement run-time grid selection.  ( version 3.14 )
  !/                  (w. e. rogers & t. j. campbell, nrl)
  !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
  !/                  (w. e. rogers & t. j. campbell, nrl)
  !/    29-oct-2010 : implement unstructured grid         ( version 3.14.4 )
  !/                  (a. roland and f. ardhuin)
  !/    06-dec-2010 : change from global (logical) to iclose (integer) to
  !/                  specify index closure for a grid.   ( version 3.14 )
  !/                  (t. j. campbell, nrl)
  !/    12-jun-2012 : add /rtd option or rotated grid option.
  !/                  (jian-guo li)                       ( version 4.06 )
  !/    02-sep-2012 : clean up of open bc for ug grids    ( version 4.07 )
  !/    25-feb-2013 : itout=0 bug correction for ug grids ( version 4.08 )
  !/    11-nov-2013 : smc and rotated grid incorporated in the main
  !/                  trunk                               ( version 4.13 )
  !/    05-jun-2018 : add setup                           ( version 6.04 )
  !/    04-oct-2019 : optional one file per output stride ( version 7.00 )
  !/                  (r. padilla-hernandez & j.h. alves)
  !/
  !/    copyright 2009 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     process point output.
  !
  !  2. variables and types :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      veropt    c*10  private  point output file version number.
  !      idstr     c*31  private  point output file id string.
  !     ----------------------------------------------------------------
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3iopp    subr. public   preprocessing of point output req.
  !      w3iope    subr. public   extract point data from grid.
  !      w3iopo    subr. public   point data io.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      w3seto    subr. w3odatmd data structure management.
  !      w3setg    subr. w3gdatmd data structure management.
  !      w3setw    subr. w3wdatmd data structure management.
  !      w3dmo2    subr. w3odatmd data structure management.
  !      strace    subr. w3servmd subroutine tracing.
  !      extcde    subr. w3servmd program abort with exit code.
  !      mpi_startall, mpiwaitall
  !                subr.          mpi persistent communication routines.
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !
  !     - allocation of allocatable arrays takes place at different
  !       places throughout the code, in w3iopp on write, and in
  !       w3iopo on read.
  !
  !  6. switches :
  !
  !       !/s     enable subroutine tracing.
  !       !/t     enable test output.
  !
  !       !/shrd  switch for shared / distributed memory architecture.
  !       !/dist  id.
  !       !/mpi   mpi message passing.
  !
  !       !/o7a   diagnostic output for output points.
  !
  !  7. source code :
  !
  !/ ------------------------------------------------------------------- /
  public
  !/
  !/ private parameter statements (id strings)
  !/
  character(len=10), parameter, private :: veropt = '2021-04-06'
  character(len=31), parameter, private ::                        &
       idstr = 'wavewatch iii point output file'
  !> dimension name for the netcdf point output file, for nopts, the
  !> number of output points.
  character(*), parameter, private :: dname_nopts = 'nopts'
  !> dimension name for the netcdf point output file, for nspec.
  character(*), parameter, private :: dname_nspec = 'nspec'
  !> dimension name for the netcdf point output file, for vsize. this
  !> is for the vector size for points, which is 2.
  character(*), parameter, private :: dname_vsize = 'vsize'
  !> dimension name for the netcdf point output file, for
  !> namelen. this is the length of the ptnme strings, which contains
  !> the names of the points.
  character(*), parameter, private :: dname_namelen = 'namelen'
  !> dimension name for the netcdf point output file, for grdidlen,
  !> this is the length of the grdid character array.
  character(*), parameter, private :: dname_grdidlen = 'grdidlen'
  !> dimension name for the netcdf point output file, for time
  character(*), parameter, private :: dname_time = 'time'
  !> dimension name for the netcdf point output file, for ww3time
  character(*), parameter, private :: dname_ww3time = 'ww3time'
  !> variable name for the netcdf point output file, for nk.
  character(*), parameter, private :: vname_nk = 'nk'
  !> variable name for the netcdf point output file, for mth.
  character(*), parameter, private :: vname_nth = 'nth'
  !> variable name for the netcdf point output file, for ptloc.
  character(*), parameter, private :: vname_ptloc = 'ptloc'
  !> variable name for the netcdf point output file, for ptnme.
  character(*), parameter, private :: vname_ptnme = 'ptnme'
  !> variable name for the netcdf point output file, for time. 
  character(*), parameter, private :: vname_time = 'time'
  !> variable name for the netcdf point output file, for ww3time.
  character(*), parameter, private :: vname_ww3time = 'ww3time'
  !> variable name for the netcdf point output file, for dpo.
  character(*), parameter, private :: vname_dpo = 'dpo'
  !> variable name for the netcdf point output file, for wao.
  character(*), parameter, private :: vname_wao = 'wao'
  !> variable name for the netcdf point output file, for wdo.
  character(*), parameter, private :: vname_wdo = 'wdo'
  !> variable name for the netcdf point output file, for tauao.
  character(*), parameter, private :: vname_tauao = 'tauao'
  !> variable name for the netcdf point output file, for taudo.
  character(*), parameter, private :: vname_taudo = 'taudo'
  !> variable name for the netcdf point output file, for dairo.
  character(*), parameter, private :: vname_dairo = 'dairo'
  !> variable name for the netcdf point output file, for zet_seto.
  character(*), parameter, private :: vname_zet_seto = 'zet_seto'
  !> variable name for the netcdf point output file, for aso.
  character(*), parameter, private :: vname_aso = 'aso'
  !> variable name for the netcdf point output file, for cao.
  character(*), parameter, private :: vname_cao = 'cao'
  !> variable name for the netcdf point output file, for cdo.
  character(*), parameter, private :: vname_cdo = 'cdo'
  !> variable name for the netcdf point output file, for iceo.
  character(*), parameter, private :: vname_iceo = 'iceo'
  !> variable name for the netcdf point output file, for iceho.
  character(*), parameter, private :: vname_iceho = 'iceho'
  !> variable name for the netcdf point output file, for icefo.
  character(*), parameter, private :: vname_icefo = 'icefo'
  !> variable name for the netcdf point output file, for grdid.
  character(*), parameter, private :: vname_grdid = 'grdid'
  !> variable name for the netcdf point output file, for spco.
  character(*), parameter, private :: vname_spco = 'spco'
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief preprocessing of point output.
  !>
  !> @details check location of points in grid and calculate interpolation
  !>  factors.
  !>
  !> @param[in]    npt     number of output points in input.
  !> @param[inout] xpt     x (longitude) coordinates of output points.
  !> @param[inout] ypt     y (latitude) coordinates of output points.
  !> @param[in]    pnames  names of output points.
  !> @param[in]    imod    grid id number.
  !>
  !> @author h. l. tolman  @date 02-sep-2012
  !>
  subroutine w3iopp ( npt, xpt, ypt, pnames, imod )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         02-sep-2012 |
    !/                  +-----------------------------------+
    !/
    !/    14-jan-1999 : distributed fortran 77 version.     ( version 1.18 )
    !/    30-dec-1999 : upgrade to fortran 90               ( version 2.00 )
    !/                  major changes to logistics.
    !/    24-jan-2001 : flat grid version.                  ( version 2.06 )
    !/    09-nov-2004 : multiple grid version.              ( version 3.06 )
    !/    25-jul-2006 : adding grid id per point.           ( version 3.10 )
    !/    01-may-2007 : move o7a output from w3init.        ( version 3.11 )
    !/    30-oct-2009 : implement run-time grid selection.  ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    06-dec-2010 : change from global (logical) to iclose (integer) to
    !/                  specify index closure for a grid.   ( version 3.14 )
    !/                  (t. j. campbell, nrl)
    !/    12-jun-2012 : add /rtd option or rotated grid option.
    !/                  (jian-guo li)                       ( version 4.06 )
    !/    02-sep-2012 : clean up of open bc for ug grids    ( version 4.07 )
    !/    01-mar-2018 : add option to unrotate spectra      ( version 6.02 )
    !/                  from rtd grid models
    !/
    !  1. purpose :
    !
    !     preprocessing of point output.
    !
    !  2. method :
    !
    !     check location of points in grid and calculate interpolation
    !     factors.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       npt     int.   i   number of output points in input.
    !       xpt     r.a.  i/o  x (longitude) coordinates of output points.
    !       ypt     r.a.  i/o  id. y.
    !       pnames  c*40   i   names of output points.
    !       imod    int.   i   grid id number.
    !     ----------------------------------------------------------------
    !
    !     local data
    !     ----------------------------------------------------------------
    !       acc     real  "accuracy" factor to determine if output point
    !                     is grid point.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     see module documentation.
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3init    subr. w3initmd wave model initialization routine.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     - warnings for points out of the grid or on land.
    !
    !  7. remarks :
    !
    !     - the output points are obtained by bi-linear interpolation from
    !       the spectra at the grid points. given the possibility of ice
    !       coverage, the actual interpolation factors can only be
    !       determined at the actual output time. hence only the basic
    !       bilinear interpolation factors are stored.
    !     - implementation of the /o7a diagnostic output section is
    !       currently incomplete and non-functional for curvilinear grids
    !       and/or tripole grids
    !
    !  8. structure :
    !
    !     -------------------------------------------
    !      determine grid range
    !      do for all defined points
    !      -----------------------------------------
    !        check if point within grid
    !        calculate interpolation data
    !        check if point not on land
    !        store interpolation data
    !     -------------------------------------------
    !
    !  9. switches :
    !
    !       !/s     enable subroutine tracing.
    !       !/t     test output.
    !
    !       !/o7a   diagnostic output for output points.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gsrumd
    use w3gdatmd, only: nth, nk, nspec, nx, ny, x0, y0, sx, gsu,&
         rlgtype, clgtype, ungtype, gtype, flagll,   &
         iclose,iclose_none,iclose_smpl,iclose_trpl, &
         mapsta, mapfs, filext, zb, trnx, trny
    use w3gdatmd, only: trigp,maxx, maxy, dxymax
    use w3odatmd, only: w3dmo2
    use w3odatmd, only: ndse, ndst, iaproc, naperr, napout, screen, &
         nopts, ptloc, ptnme, grdid, iptint, ptifac
    use w3servmd, only: extcde
    use w3triamd
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)          :: npt, imod
    real, intent(inout)          :: xpt(npt), ypt(npt)
    character(len=40),intent(in) :: pnames(npt)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    logical                 :: ingrid
    integer                 :: ipt, j, k
    integer                 :: ix1, iy1, ixs, iys
    integer                 :: ix(4), iy(4)   ! indices of points used in interp.
    real                    :: rd(4)          ! interpolation coefficient
    real, parameter         :: acc = 0.05
    real                    :: factor
    integer                 :: itout          ! triangle index in unstructured grids
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    if ( flagll ) then
      factor = 1.
    else
      factor = 1.e-3
    end if
    !
    call w3dmo2 ( imod, ndse, ndst, npt )
    grdid  = filext
    !
    nopts  = 0
    !
    !
    !   removed by f.a. 2011/04/04  /t      call w3gsup( gsu, ndst )
    !
    ! loop over output points
    !
    do ipt=1, npt
      !
      !
      !
      !     check if point within grid and compute interpolation weights
      !
      if (gtype .ne. ungtype) then
        ingrid = w3grmp( gsu, xpt(ipt), ypt(ipt), ix, iy, rd )
      else
        call is_in_ungrid(imod, dble(xpt(ipt)), dble(ypt(ipt)), itout, ix, iy, rd)
        ingrid = (itout.gt.0)
      end if
      !
      if ( .not.ingrid ) then
        if ( iaproc .eq. naperr ) then
          if ( flagll ) then
            write (ndse,1000) xpt(ipt), ypt(ipt), pnames(ipt)
          else
            write (ndse,1001) xpt(ipt), ypt(ipt), pnames(ipt)
          end if
        end if
        cycle
      end if
      !
      !
      !     check if point not on land
      !
      if ( mapsta(iy(1),ix(1)) .eq. 0 .and. &
           mapsta(iy(2),ix(2)) .eq. 0 .and. &
           mapsta(iy(3),ix(3)) .eq. 0 .and. &
           mapsta(iy(4),ix(4)) .eq. 0 ) then
        if ( iaproc .eq. naperr ) then
          if ( flagll ) then
            write (ndse,1002) xpt(ipt), ypt(ipt), pnames(ipt)
          else
            write (ndse,1003) xpt(ipt), ypt(ipt), pnames(ipt)
          end if
        end if
        cycle
      end if
      !
      !     store interpolation data
      !
      nopts  = nopts + 1
      !
      ptloc (1,nopts) = xpt(ipt)
      ptloc (2,nopts) = ypt(ipt)
      !
      do k = 1,4
        iptint(1,k,nopts) = ix(k)
        iptint(2,k,nopts) = iy(k)
        ptifac(k,nopts) = rd(k)
      end do
      ptnme(nopts) = pnames(ipt)
      !
    end do ! end loop over output points (ipt).
    !
    !
    ! diagnostic output
    !
    !
    return
    !
    ! formats
    !
    !
1000 format (/' *** wavewatch-iii warning :'/                   &
         '     output point out of grid : ',2f10.3,2x,a/   &
         '     point skippped '/)
1001 format (/' *** wavewatch-iii warning :'/                   &
         '     output point out of grid : ',2e10.3,2x,a/   &
         '     point skippped '/)
    !
1002 format (/' *** wavewatch-iii warning :'/                   &
         '     output point on land : ',2f10.3,2x,a/       &
         '     point skippped '/)
1003 format (/' *** wavewatch-iii warning :'/                   &
         '     output point on land : ',2e10.3,2x,a/       &
         '     point skippped '/)
    !
    !/
    !/ end of w3iopp ----------------------------------------------------- /
    !/
  end subroutine w3iopp
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief extract point output data and store in output commons.
  !>
  !> @details this action is taken from an earlier version of w3iopo
  !>  so that the point output postprocessor does not need the full
  !>  sea-point grid to be able to run.  note that the output spectrum
  !>  is f(f,theta). interpolation is performed for this spectrum.
  !>
  !> @param[in] a  action spectra on storage grid.
  !>
  !> @author h. l. tolman  @date 12-jun-2012
  !>
  subroutine w3iope ( a )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         12-jun-2012 |
    !/                  +-----------------------------------+
    !/
    !/    12-jan-1999 : distributed fortran 77 version.     ( version 1.18 )
    !/    25-jan-2000 : upgrade to fortran 90               ( version 2.00 )
    !/                  major changes to logistics.
    !/    11-jun-2001 : clean-up.                           ( version 2.11 )
    !/    09-nov-2004 : multiple grid version.              ( version 3.06 )
    !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    29-oct-2010 : implement unstructured grids        ( version 3.14.4 )
    !/                  (a. roland and f. ardhuin)
    !/    12-jun-2012 : add /rtd option or rotated grid option.
    !/                  (jian-guo li)                       ( version 4.06 )
    !/    01-mar-2018 : add option to unrotate spectra      ( version 6.02 )
    !/                  from rtd grid models
    !/    19-jul-2021 : momentum and air density support    ( version 7.14 )
    !/
    !  1. purpose :
    !
    !     extract point output data and store in output commons. this
    !     action is taken from an earlier version of w3iopo so that the
    !     point output postprocessor does not need the full sea-point
    !     grid to be able to run.
    !       note that the output spectrum is f(f,theta). interpolation
    !     is performed for this spectrum.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a       r.a.   i   action spectra on storage grid.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     see module documentation.
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3wave    subr. w3wavemd actual wave model routine.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !       none.
    !
    !  7. remarks :
    !
    !     - to allow for dynamic ice edges, interpolation factors are
    !       calculated for every time step separately.
    !     - wind current and depth data are interpolated ignoring ice,
    !       spectrum is interpolated removing ice points.
    !     - spectra are left in par list to allow for change of shape of
    !       arrays.
    !     - imod is not passed to this routine. since it is used only
    !       in w3wave, it is assumed that the pointer are set
    !       appropriately outside this routine.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/shrd  switch for shared / distributed memory architecture.
    !     !/dist  id.
    !     !/mpi   switch for message passing method.
    !
    !     !/s     enable subroutine tracing.
    !     !/t     test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    use w3gdatmd, only: nk, nth, sig, nx, ny, nsea, nseal,          &
         mapsta, mapfs
    use w3wdatmd, only: ice, iceh, icef
    use w3adatmd, only: cg, dw, ua, ud, as, cx, cy,                 &
         sp => sppnt
    use w3odatmd, only: ndst, nopts, iptint, ptifac, il, iw, ii,    &
         dpo, wao, wdo, aso, cao, cdo, iceo, iceho,  &
         icefo, spco, naproc
    use w3odatmd, only: irqpo2
    use w3servmd, only: extcde
    !
    implicit none
    !
    include "mpif.h"
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: a(nth,nk,0:nseal)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: i, ix1, iy1, ix(4), iy(4), j, is(4), &
         im(4), ik, ith, isp
    integer                 :: ioff, ierr_mpi
    integer                 :: stat(mpi_status_size,4*nopts)
    real                    :: rd(4), rds, rdi, facrd,              &
         wndx, wndy, curx, cury, fac1(nk),    &
         fac2(nk), fac3(nk), fac4(nk)
    integer                 :: jsea, isea
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    cx(0)  = 0.
    cy(0)  = 0.
    !
    ! loop over spectra -------------------------------------------------- *
    !
    do i=1, nopts
      !
      !
      ! unpack interpolation data
      !
      ix(:)  = iptint(1,:,i)
      iy(:)  = iptint(2,:,i)
      rd(:)  = ptifac(:,i)
      !
      !
      !
      ! correct for land and ice and get sea point counters
      !
      il(i)  = 0
      iw(i)  = 0
      ii(i)  = 0
      rds    = 0.
      rdi    = 0.
      !
      do j=1, 4
        is(j)  = mapfs (iy(j),ix(j))
        im(j)  = mapsta(iy(j),ix(j))
        if ( im(j).gt.0 ) then
          iw(i)  = iw(i) + 1
          rds    = rds + rd(j)
        else
          if ( im(j).lt.0 ) then
            ii(i)  = ii(i) + 1
            rdi    = rdi + rd(j)
          else
            il(i)  = il(i) + 1
            rd(j)  = 0.
          end if
        end if
      end do
      !
      ! depth, wind and current, ignore ice
      !
      if ( rds+rdi .gt. 1.e-7 ) then
        facrd  = 1. / (rds+rdi)
        rd     = rd * facrd
      end if
      !
      !
      ! interpolate ice depth, wind, stresses, rho air and current
      !
      if (.not. lpdlib) then
        icefo(i) = 0
        do j=1, 4
          isea = mapfs(iy(j),ix(j))
          icefo(i) = icefo(i) + rd(j)*icef(jsea)
        end do
      else
        icefo(i) = rd(1)*icef(is(1)) + rd(2)*icef(is(2)) +          &
             rd(3)*icef(is(3)) + rd(4)*icef(is(4))
      end if
      iceo(i) = rd(1)*ice(is(1)) + rd(2)*ice(is(2)) +               &
           rd(3)*ice(is(3)) + rd(4)*ice(is(4))
      iceho(i) = rd(1)*iceh(is(1)) + rd(2)*iceh(is(2)) +            &
           rd(3)*iceh(is(3)) + rd(4)*iceh(is(4))
      !
      dpo(i) = rd(1)*dw(is(1)) + rd(2)*dw(is(2)) +                  &
           rd(3)*dw(is(3)) + rd(4)*dw(is(4))
      !
      !
      wndx   = rd(1) * ua(is(1)) * cos(ud(is(1))) +                 &
           rd(2) * ua(is(2)) * cos(ud(is(2))) +                 &
           rd(3) * ua(is(3)) * cos(ud(is(3))) +                 &
           rd(4) * ua(is(4)) * cos(ud(is(4)))
      wndy   = rd(1) * ua(is(1)) * sin(ud(is(1))) +                 &
           rd(2) * ua(is(2)) * sin(ud(is(2))) +                 &
           rd(3) * ua(is(3)) * sin(ud(is(3))) +                 &
           rd(4) * ua(is(4)) * sin(ud(is(4)))
      !
      wao(i) = sqrt ( wndx**2 + wndy**2 )
      if ( wao(i).gt.1.e-7 ) then
        wdo(i) = atan2(wndy,wndx)
      else
        wdo(i) = 0.
      end if
      !
      aso(i) = rd(1)*as(is(1)) + rd(2)*as(is(2)) +                  &
           rd(3)*as(is(3)) + rd(4)*as(is(4))
      !
      curx   = rd(1)*cx(is(1)) + rd(2)*cx(is(2)) +                  &
           rd(3)*cx(is(3)) + rd(4)*cx(is(4))
      cury   = rd(1)*cy(is(1)) + rd(2)*cy(is(2)) +                  &
           rd(3)*cy(is(3)) + rd(4)*cy(is(4))
      !
      cao(i) = sqrt ( curx**2 + cury**2 )
      if ( cao(i).gt.1.e-7 ) then
        cdo(i) = atan2(cury,curx)
      else
        cdo(i) = 0.
      end if
      !
      ! interp. weights for spectra, no ice points (spectra by def. zero)
      !
      if ( rds .gt. 1.e-7 ) then
        facrd  = (rds+rdi) / rds
        rd     = rd * facrd
      end if
      !
      !
      ! extract spectra, shared memory version
      !        (done in separate step for mpp compatibility)
      !
      !
      ! extract spectra, distributed memory version(s)
      !
      ioff   = 1 + 4*(i-1)
      call mpi_startall ( 4, irqpo2(ioff), ierr_mpi )
      call mpi_waitall  ( 4, irqpo2(ioff), stat, ierr_mpi )
      !
      ! interpolate spectrum
      !
      do ik=1, nk
        fac1(ik) = tpi * sig(ik) / cg(ik,is(1))
        fac2(ik) = tpi * sig(ik) / cg(ik,is(2))
        fac3(ik) = tpi * sig(ik) / cg(ik,is(3))
        fac4(ik) = tpi * sig(ik) / cg(ik,is(4))
      end do
      !
      do ik=1,nk
        do ith=1,nth
          isp    = ith + (ik-1)*nth
          spco(isp,i) = rd(1) * sp(ith,ik,1) * fac1(ik)             &
               + rd(2) * sp(ith,ik,2) * fac2(ik)             &
               + rd(3) * sp(ith,ik,3) * fac3(ik)             &
               + rd(4) * sp(ith,ik,4) * fac4(ik)
        end do
      end do
      !
      !
      ! fa commented out: bug
      !at line 1974 of file w3arrymd.f90
      !fortran runtime error: index '52' of dimension 1 of array 'pnum2' above upper bound of 51
      !
    end do
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3iope ----------------------------------------------------- /
    !/
  end subroutine w3iope
  !/ ------------------------------------------------------------------- /
  !> read or write point output.
  !>
  !> this subroutine can either read or write the point output file,
  !> depending on the value of the first parameter.
  !>
  !> when reading, the entire file is read with one call to this
  !> subroutine.
  !>
  !> when writing, this subroutine can either write one timestep or
  !> the whole model run. this is an option in the input file. if the
  !> entire model run is to be written, then ofiles(2) is 0. if only
  !> one timestep is to be written, then ofiles(2) is 1.
  !>
  !> if ofiles(2) is 0, the output file is names out_pnt.ww3. if
  !> ofiles(2) is 1, the output file is named timetag.out_pnt.ww3.
  !>
  !> the format of the point output file is:
  !> size (bytes) | type | variable | meaning
  !> -------------|------|----------|--------
  !> 40 | character*40 | idtst | id string
  !> 4 | integer | vertst | model definition file version number
  !> 4 | integer | nk | number of discrete wavenumbers  
  !> 4 | integer | nth | number of discrete directions. 
  !> 4 | integer | nopts | number of output points.
  !> 8*nopts | real(2,nopts) | ptloc | point locations
  !> 7*nopts | character*7 | ptnme | point names
  !> 8 | integer(2) | time | valid time
  !> reclen*nopts | * | * | records
  !>
  !> each record contains:
  !> size (bytes) | type | variable | meaning
  !> -------------|------|----------|--------
  !> 4 | integer | iw | number of water points in interpolation box for output point.
  !> 4 | integer | ii | number of ice points in interpolation box for output point.
  !> 4 | integer | il | number of land points in interpolation box for output point.
  !> 4 | real | dpo | interpolated depths.
  !> 4 | real | wao | interpolated wind speeds.
  !> 4 | real | wdo | interpolated wind directions.
  !> 4 | real | tauao | (w3_flx5 only) interpolated atmospheric stresses.
  !> 4 | real | taudo | (w3_flx5 only) interpolated atmospheric stress directions.
  !> 4 | real | dairo | (w3_flx5 only) interpolated rho atmosphere.
  !> 4 | real | zet_seto | (w3_setup only) used for wave setup.
  !> 4 | real | aso | interpolated air-sea temperature difference
  !> 4 | real | cao | interpolated current speeds.
  !> 4 | real | cdo | interpolated current directions.
  !> 4 | real | iceo | interpolated ice concentration.
  !> 4 | real | iceho | interpolated ice thickness.
  !> 4 | real | icefo | interpolated ice floe.
  !> 13 | char | grdid | originating grid id
  !> 4 | real | spco(j,i),j=1,nspec | output spectra
  !>
  !> in the event of error, extcde() will be called with the following exit codes:
  !> - 1 inxout must be 'read' or 'write'.
  !> - 2 unexpectedly changed from write to read in subsequent call.
  !> - 10 unexpected idstr
  !> - 11 unexpected veropt
  !> - 12 unexpected mk or mth
  !> - 20 error opening file.
  !> - 21 unexpected end of file during read.
  !> - 22 error reading file.
  !> - 23 unexpected end of file during read.
  !>
  !> @param[in] inxout string indicating read/write. must be 'read' or
  !> 'write'.
  !> @param[in] ndsop this is set by this subroutine to the netcdf
  !> file id (ncid) of the opened file. user does not have to
  !> initialize this value, and should not change it.
  !> @param[out] iotst error code:
  !> - 0 no error.
  !> - -1 unexpected end of file when reading.
  !> @param[in] imod model number for w3gdat etc.
  !>
  !> @author h. l. tolman  @date 25-jul-2006
  subroutine w3iopo ( inxout, ndsop, iotst, imod &
      )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         25-jul-2006 |
    !/                  +-----------------------------------+
    !/
    !/    07-jan-1999 : distributed fortran 77 version.     ( version 1.18 )
    !/    30-dec-1999 : upgrade to fortran 90               ( version 2.00 )
    !/                  major changes to logistics.
    !/    10-nov-2004 : multiple grid version.              ( version 3.06 )
    !/    27-jun-2006 : adding file name preamble.          ( version 3.09 )
    !/    25-jul-2006 : adding grid id per point.           ( version 3.10 )
    !/    27-aug-2015 : adding interpolation for the ice.   ( version 5.10 )
    !/    19-jul-2021 : momentum and air density support    ( version 7.14 )
    !/
    !  1. purpose :
    !
    !     read/write point output.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       inxout  c*(*)  i   test string for read/write, valid are:
    !                          'read' and 'write'.
    !       ndsop   int.   i   file unit number. for binary
    !       ndsoa   int.   i   file unit number. for ascii
    !       iotst   int.   o   test indictor for reading.
    !                           0 : data read.
    !                          -1 : past end of file.
    !       imod    i(o)   i   model number for w3gdat etc.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     see module documentation.
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3wave    subr. w3wavemd actual wave model routine.
    !      ww3_outp  prog.   n/a    postprocessing for point output.
    !      gx_outp   prog.   n/a    grads postprocessing for point output.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !       tests on inxout, file status and on array dimensions.
    !
    !  7. remarks :
    !
    !     - the output file has the pre-defined name 'out_pnt.filext'.
    !     - in mpp version of model data is supposed to be gatherd at the
    !       correct processor before the routine is called.
    !     - no error output filtering needed.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/shrd  switch for shared / distributed memory architecture.
    !     !/dist  id.
    !
    !     !/s     enable subroutine tracing.
    !     !/t     test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: w3setg
    use w3wdatmd, only: w3setw
    use w3odatmd, only: w3seto, w3dmo2
    !/
    use w3gdatmd, only: nth, nk, nspec, filext
    use w3wdatmd, only: time
    use w3odatmd, only: ndst, ndse, ipass => ipass2, nopts, iptint, &
         il, iw, ii, ptloc, ptifac, dpo, wao, wdo,   &
         aso, cao, cdo, spco, ptnme, o2init, fnmpre, &
         grdid, iceo, iceho, icefo
    use w3odatmd, only :  ofiles
    !/
    !/
    use w3servmd, only: extcde
    use constants, only: file_endian
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)           :: ndsop
    integer, intent(out)          :: iotst
    integer, intent(in), optional :: imod
    character, intent(in)         :: inxout*(*)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: igrd, ierr, mk, mth, i, j
    logical,save            :: write
    character(len=31)       :: idtst
    character(len=10)       :: vertst
    !/
    character(len=15) :: timetag
    !/
    !/ ------------------------------------------------------------------- /
    !/
    ipass  = ipass + 1
    iotst  = 0
    !
    ! test input parameters ---------------------------------------------- *
    !
    if ( present(imod) ) then
      igrd   = imod
    else
      igrd   = 1
    end if
    !
    call w3seto ( igrd, ndse, ndst )
    call w3setg ( igrd, ndse, ndst )
    call w3setw ( igrd, ndse, ndst )
    !
    if (inxout.ne.'read' .and. inxout.ne.'write' ) then
      write (ndse,900) inxout
      call extcde ( 1 )
    end if
    !
    ! first pass to this file and we are only writing 1 file for all time     
    if ( ipass.eq.1  .and. ofiles(2) .eq. 0) then
      write  = inxout.eq.'write'
    else
      if ( write .and. inxout.eq.'read' ) then
        write (ndse,901) inxout
        call extcde ( 2 )
      end if
    end if
    !
    ! open file ---------------------------------------------------------- *
    !
    if ( ipass.eq.1 .and. ofiles(2) .eq. 0 ) then
      i      = len_trim(filext)
      j      = len_trim(fnmpre)
      if ( write ) then
        open (ndsop,file=fnmpre(:j)//'out_pnt.'//filext(:i),    &
             form='unformatted', convert=file_endian,err=800,iostat=ierr)
      else
        open (ndsop,file=fnmpre(:j)//'out_pnt.'//filext(:i),    &
             form='unformatted', convert=file_endian,err=800,iostat=ierr,status='old')
      end if
      !
      rewind ( ndsop )
      !
      ! test info ---------------------------------------------------------- *
      ! ( ipass = 1 )
      !
      if ( write ) then
        write (ndsop)                                           &
             idstr, veropt, nk, nth, nopts
      else
        read (ndsop,end=801,err=802,iostat=ierr)                &
             idtst, vertst, mk, mth, nopts
        !
        if ( idtst .ne. idstr ) then
          write (ndse,902) idtst, idstr
          call extcde ( 10 )
        end if
        if ( vertst .ne. veropt ) then
          write (ndse,903) vertst, veropt
          call extcde ( 11 )
        end if
        if (nk.ne.mk .or. nth.ne.mth) then
          write (ndse,904) mk, mth, nk, nth
          call extcde ( 12 )
        end if
        if ( .not. o2init )                                     &
             call w3dmo2 ( igrd, ndse, ndst, nopts )
      end if
      !
      !
      ! point specific info ------------------------------------------------ *
      ! ( ipass = 1 )
      !
      if ( write ) then
        write (ndsop)                                           &
             ((ptloc(j,i),j=1,2),i=1,nopts), (ptnme(i),i=1,nopts)
      else
        read  (ndsop,end=801,err=802,iostat=ierr)               &
             ((ptloc(j,i),j=1,2),i=1,nopts), (ptnme(i),i=1,nopts)
      end if
      !
      !
    end if
    !
    !
    if ( ipass.ge. 1  .and. ofiles(2) .eq. 1) then
      write  = inxout.eq.'write'
    else
      if ( write .and. inxout.eq.'read' ) then
        write (ndse,901) inxout
        call extcde ( 2 )
      end if
    end if
    ! open file ---------------------------------------------------------- *
    !
    if ( ipass.ge.1 .and. ofiles(2) .eq. 1) then
      !
      i      = len_trim(filext)
      j      = len_trim(fnmpre)
      ! create timetag for file name using yyyymmdd.hhmms prefix
      write(timetag,"(i8.8,'.'i6.6)")time(1),time(2)
      !
      if ( write ) then
        open (ndsop,file=fnmpre(:j)//timetag//'.out_pnt.'   &
             //filext(:i),form='unformatted', convert=file_endian,err=800,iostat=ierr)
      end if
      !
      rewind ( ndsop )
      !
      !
      ! test info ---------------------------------------------------------- *
      ! ( ipass ge.1 .and. ofiles(2) .eq. 1)
      !
      if ( write ) then
        write (ndsop)                                           &
            idstr, veropt, nk, nth, nopts
      else
        read (ndsop,end=801,err=802,iostat=ierr)                &
             idtst, vertst, mk, mth, nopts
        !
        if ( idtst .ne. idstr ) then
          write (ndse,902) idtst, idstr
          call extcde ( 10 )
        end if
        if ( vertst .ne. veropt ) then
          write (ndse,903) vertst, veropt
          call extcde ( 11 )
        end if
        if (nk.ne.mk .or. nth.ne.mth) then
          write (ndse,904) mk, mth, nk, nth
          call extcde ( 12 )
        end if
        if ( .not. o2init )                                     &
             call w3dmo2 ( igrd, ndse, ndst, nopts )
      end if
      !
      !
      ! point specific info ------------------------------------------------ *
      ! ( ipass ge.1 .and. ofiles(2) .eq. 1)
      !
      if ( write ) then
        write (ndsop)                                           &
             ((ptloc(j,i),j=1,2),i=1,nopts), (ptnme(i),i=1,nopts)
      else
        read  (ndsop,end=801,err=802,iostat=ierr)               &
             ((ptloc(j,i),j=1,2),i=1,nopts), (ptnme(i),i=1,nopts)
      end if
      !
      !
    end if
    !
    !
    ! time --------------------------------------------------------------- *
    !
    if ( write ) then
      write (ndsop)                            time
    else
      read (ndsop,end=803,err=802,iostat=ierr) time
    end if
    !
    !
    !
    ! loop over spectra -------------------------------------------------- *
    !
    do i=1, nopts
      !
      if ( write ) then
        ! set iw, ii and il to 0 because it is not used and gives &
        ! outlier values in out_pnt.points
        iw(i) = 0
        ii(i) = 0
        il(i) = 0
        write (ndsop)                                            &
             iw(i), ii(i), il(i), dpo(i), wao(i), wdo(i),      &
             aso(i), cao(i), cdo(i), iceo(i), iceho(i),        &
             icefo(i), grdid(i), (spco(j,i),j=1,nspec)
      else
        read (ndsop,end=801,err=802,iostat=ierr)                 &
             iw(i), ii(i), il(i), dpo(i), wao(i), wdo(i),      &
             aso(i), cao(i), cdo(i), iceo(i), iceho(i),        &
             icefo(i), grdid(i), (spco(j,i),j=1,nspec)
      end if
      !
    end do
    if (ofiles(2) .eq. 1)  close (ndsop)
    !
    return
    !
    ! escape locations read errors
    !
800 continue
    write (ndse,1000) ierr
    call extcde ( 20 )
    !
801 continue
    write (ndse,1001)
    call extcde ( 21 )
    !
802 continue
    write (ndse,1002) ierr
    call extcde ( 22 )
    !
803 continue
    iotst  = -1
    return
    !
    ! formats
    !
900 format (/' *** wavewatch iii error in w3iopo :'/                &
         '     ilegal inxout value: ',a/)
901 format (/' *** wavewatch iii error in w3iopo :'/                &
         '     mixed read/write, last request: ',a/)
902 format (/' *** wavewatch iii error in w3iopo :'/                &
         '     ilegal idstr, read : ',a/                        &
         '                  check : ',a/)
903 format (/' *** wavewatch iii error in w3iopo :'/                &
         '     ilegal veropt, read : ',a/                       &
         '                   check : ',a/)
904 format (/' *** wavewatch iii error in w3iopo :'/                &
         '     error in spectra, mk, mth : ',2i8/               &
         '              array dimensions : ',2i8/)
    !
1000 format (/' *** wavewatch iii error in w3iopo : '/               &
         '     error in opening file'/                          &
         '     iostat =',i5/)
1001 format (/' *** wavewatch iii error in w3iopo : '/               &
         '     premature end of file'/)
1002 format (/' *** wavewatch iii error in w3iopo : '/               &
         '     error in reading from file'/                     &
         '     iostat =',i5/)
    !
    !/
    !/ end of w3iopo ----------------------------------------------------- /
    !/
  end subroutine w3iopo
  !/
  !/ end of module w3iopomd -------------------------------------------- /
  !/
end module w3iopomd
