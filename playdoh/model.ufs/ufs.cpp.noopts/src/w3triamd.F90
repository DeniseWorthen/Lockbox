!> @file
!> @brief reads triangle and unstructured grid information.
!>
!> @author f. ardhuin
!> @author a. roland
!> @date   26-jan-2014
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
!> @brief reads triangle and unstructured grid information.
!>
!> @details look for namelist with name name in unit nds and read if found.
!>
!> @author f. ardhuin
!> @author a. roland
!> @date   26-jan-2014
!>
module w3triamd
  !/ -------------------------------------------------------------------
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |       f. ardhuin and a. roland    |
  !/                  |                        fortran 90 |
  !/                  | last update :          26-jan-2014|
  !/                  +-----------------------------------+
  !/
  !/    15-mar-2007 : origination.                        ( version 3.13 )
  !/    25-aug-2011 : modification of boundary treatment  ( version 4.04 )
  !/    30-aug-2012 : automatic detection of open bc      ( version 4.08 )
  !/    02-sep-2012 : clean up of open bc for ug grids    ( version 4.08 )
  !/    14-oct-2013 : correction  of latitude factor      ( version 4.12 )
  !/    26-jan-2014 : correction  interpolation weights   ( version 4.18 )
  !/    21-apr-2016 : new algorithm to detect boundary    ( version 5.12 )
  !/
  !
  !  1. purpose :
  !
  !      reads triangle and unstructured grid information
  !
  !  2. method :
  !
  !     look for namelist with name name in unit nds and read if found.
  !
  !  3. parameters :
  !
  !
  !  4. subroutines used :
  !
  !      name               type  module   description
  !     ------------------------------------------------------------------------------------
  !      readtri            subr. internal read unstructured grid data from .grd .tri formatted files.
  !      readmsh            subr.   id.    read unstructured grid data from msh format
  !      count              subr. internal count connection.
  !      spatial_grid       subr.   id.    calculate surfaces.
  !      nvectri            subr.   id.    define cell normals and angles and edge length
  !      coordmax           subr.   id.    calculate  useful grid elements
  !      area_si            subr.   id.    define connections
  !     ------------------------------------------------------------------------------------
  !
  !
  !  5. called by :
  !
  !     program in which it is contained.
  !
  !  6. error messages :
  !
  !  7. remarks :
  !     the only point index which is needed is ix and nx stands for the total number of grid point.
  !     iy and ny are not needed anymore, they are set to 1 in the unstructured case
  !     some noticeable arrays are:
  !                     trigp  : give the vertices of each triangle
  !  8. structure :
  !
  !  9. switches :
  !       !/pr3   : enables unstructured meshes (temporary, will be replace by unstructured switch)
  ! 10. source code :
  !
  !/ ------------------------------------------------------------------- /
  public
  !      use constants
  !      use w3gdatmd, only: w3nmod, w3setg
  !      use w3odatmd, only: w3nout, w3seto, w3dmo5
  !      use w3iogrmd, only: w3iogr
  !     use w3servmd, only: itrace, nextln, extcde
  !      use w3arrymd, only: ina2r, ina2i
  !      use w3dispmd, only: distab
  !      use w3gdatmd
  !      use w3odatmd, only: ndse, ndst, ndso
  !      use w3odatmd, only: nbi, nbi2, nfbpo, nbo, nbo2, flbpi, flbpo,  &
  !                         ipbpo, isbpo, xbpo, ybpo, rdbpo, fnmpre
  !---------------------------------------------------------------------
  !
  !c
  !        integer :: node_num
  !        integer :: dim_num
  !        integer :: triangle_order
  !        integer :: triangle_num
  !        integer :: bound_edge_num
  !        integer :: bound_num
  !c
  !        logical,save, allocatable :: edge_boundary(:)
  !        logical,save, allocatable :: node_boundary(:)
  !        integer,save, allocatable :: edge_nums(:)
  !        integer,save, allocatable :: boundary_node_index(:)
  !c
  !        integer,save, allocatable :: triangle_node(:,:)
  !        integer,save, allocatable :: edge(:,:)
  !        integer,save, allocatable :: edge_index(:,:)
  !        integer,save, allocatable :: iobp_aux(:)
  integer, save               :: n_outside_boundary
  integer, save, allocatable  :: outside_boundary(:)
contains
  !/ -------------------------------------------------------------------/
  !>
  !> @brief reads triangle and unstructured grid information from gmsh files.
  !>
  !> @details calls the subroutines needed to compute grid connectivity.
  !>  look for namelist with name name in unit nds and read if found.
  !>
  !> @param[in] nds    data set number used for search.
  !> @param[in] fname  name of namelist.
  !>
  !> @author f. ardhuin
  !> @author a. roland
  !> @date   06-jun-2018
  !>
  subroutine readmsh(nds,fname)
    !/ -------------------------------------------------------------------
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           f. ardhuin              |
    !/                  |           a. roland               |
    !/                  |                        fortran 90 |
    !/                  | last update :          06-jun-2018|
    !/                  +-----------------------------------+
    !/
    !/    15-feb-2008 : origination.                        ( version 3.13 )
    !/    25-aug-2011 : change of method for iobpd          ( version 4.04 )
    !/    06-jun-2018 : add debuginit/pdlib/debugstp/debugsetiobp
    !/                                                      ( version 6.04 )
    !/
    !
    !  1. purpose :
    !
    !      reads triangle and unstructured grid information from gmsh files
    !      calls the subroutines needed to compute grid connectivity
    !
    !  2. method :
    !
    !     look for namelist with name name in unit nds and read if found.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       nds     int.   i   data set number used for search.
    !       name    c*4    i   name of namelist.
    !       status  c*20   o   status at end of routine,
    !                            '(default values)  ' if no namelist found.
    !                            '(user def. values)' if namelist read.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name               type  module   description
    !     ------------------------------------------------------------------------------------
    !      nextln             subr.
    !      count              subr. internal count connection.
    !      spatial_grid       subr.   id.    calculate surfaces.
    !      nvectri            subr.   id.    define cell normals and angles and edge length
    !      coordmax           subr.   id.    calculate  useful grid elements
    !      area_si            subr.   id.    define connections
    !     ----------------------------------------------------------------
    !
    !
    !
    !  5. called by :
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3grid    prog.          model configuration program
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !     the only point index which is needed is ix and nx stands for the total number of grid point.
    !     iy and ny are not needed anymore, they are set to 1 in the unstructured case
    !     some noticeable arrays are:
    !                     trigp  : give the vertices of each triangle
    !     gmsh file gives too much information that is not necessarily required so data processing is needed (data sort and nesting).
    !  8. structure :
    !
    !  9. switches :
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3odatmd, only: ndse, ndst, ndso
    use w3gdatmd, only: zb, xgrd, ygrd, ntri, nx, countot, trigp, nnz, w3dimug
    use w3servmd, only: itrace, nextln, extcde
    use constants, only: lpdlib
    use w3odatmd, only: iaproc
    !
    implicit none
    !/
    !/ parameter list
    !/
    integer, intent(in)                :: nds
    character(60), intent(in)          :: fname
    !/
    !/ local parameters
    !/
    integer                            :: i,j,k, nodes, nelts, id, kid
    integer                            :: id1, id2, kid1, itmp(3)
    integer                            :: i1, i2, i3
    integer(kind=4)                    :: ind,eltype,ntag, inode
    character                          :: comstr*1, space*1 = ' ', cels*64
    real, allocatable                  :: tags(:)
    character(len=64), allocatable     :: els(:)
    character(len=120)                 :: line
    character(len=50)                  :: chtmp
    character(len=10)                  :: a, b, c
    integer,allocatable                :: nels(:), trigptmp1(:,:), trigptmp2(:,:)
    integer(kind=4),allocatable        :: ifound(:), vertex(:), boundtmp(:)
    double precision, allocatable      :: xybtmp1(:,:),xybtmp2(:,:)
    real                               :: z
    open(nds,file = fname,status='old')
    read (nds,'(a)') comstr
    if (comstr.eq.' ') comstr = '$'
    call nextln(comstr, nds, ndse)
    read(nds,*) i,j,k
    call nextln(comstr, nds, ndse)
    lpdlib = .false.
    !
    ! read number of nodes and nodes from gmsh files
    !
    read(nds,*) nodes
    allocate(xybtmp1(3,nodes))
    do i= 1, nodes
      read(nds,*) j, xybtmp1(1,i), xybtmp1(2,i), xybtmp1(3,i)
    end do
    !
    ! read number of elements and elements from gmsh files
    !
    allocate(boundtmp(nodes))
    n_outside_boundary = 0
    call nextln(comstr, nds, ndse)
    read(nds,*) nelts
    allocate(trigptmp1(3,nelts))
    j = 0
    do i= 1, nelts
      read(nds,'(a100)') line
      read(line,*) ind,eltype,ntag
      allocate(tags(ntag))
      select case (eltype)
        !
        ! eltype = 15 : boundary points  (this is used to make the difference
        !                                between the outside polygon and islands)
        !
      case(15)
        read(line,*) ind,eltype,ntag,tags,inode
        n_outside_boundary = n_outside_boundary +1
        boundtmp(n_outside_boundary)=inode
        !
        ! eltype = 2 : triangles
        !
      case (2)
        j = j + 1
        read(line,*)  ind,eltype,ntag,tags,itmp
        trigptmp1(1:3,j) = itmp
      end select
      deallocate(tags)
    end do
    !
    ! organizes the grid data structure
    !
    allocate(outside_boundary(n_outside_boundary))
    outside_boundary(:) = boundtmp(1:n_outside_boundary)
    ntri = j
    allocate(ifound(nodes))
    ifound = 0
    !
    ! verifies that the nodes are used in at least one triangle
    !
    do k = 1, ntri
      i1 = trigptmp1(1,k)
      i2 = trigptmp1(2,k)
      i3 = trigptmp1(3,k)
      ifound(i1)= ifound(i1) + 1
      ifound(i2)= ifound(i2) + 1
      ifound(i3)= ifound(i3) + 1
    end do
    j = 0
    allocate(trigptmp2(3,ntri),vertex(nodes),xybtmp2(3,nodes))
    vertex(:)=0
    xybtmp2 = 0
    do i = 1, nodes
      if( ifound(i) .gt. 0) then
        j = j+1
        xybtmp2(:,j) = xybtmp1(:,i)
        vertex(i) = j
      end if
    end do
    !
    ! number of nodes after clean up
    !
    nx = j
    !
    do i = 1, ntri
      i1 = trigptmp1(1,i)
      i2 = trigptmp1(2,i)
      i3 = trigptmp1(3,i)
      trigptmp2(1,i) = vertex(i1)
      trigptmp2(2,i) = vertex(i2)
      trigptmp2(3,i) = vertex(i3)
    end do
    !
    deallocate(xybtmp1,ifound,trigptmp1)
    deallocate(vertex)
    !
    !count points connections to allocate array in w3dimug
    !
    call count(trigptmp2)
    call w3dimug ( 1, ntri, nx, countot, nnz, ndse, ndst )
    !
    ! fills arrays
    !
    do i = 1, nx
      xgrd(1,i) = xybtmp2(1,i)
      ygrd(1,i) = xybtmp2(2,i)
      zb(i)     = xybtmp2(3,i)
    end do
    !
    do i=1, ntri
      itmp = trigptmp2(:,i)
      trigp(:,i) = itmp
    end do
    !
    deallocate(trigptmp2,xybtmp2)
    !
    ! call the various routines which define the point spotting strategy
    !
    call spatial_grid
    call nvectri
    call coordmax
      call area_si(1)
    !
    close(nds)
  end subroutine readmsh
  !/--------------------------------------------------------------------/
  !>
  !> @brief reads triangle and unstructured grid information from gmsh files.
  !>
  !> @details calls the subroutines needed to compute grid connectivity.
  !>  look for namelist with name name in unit nds and read if found.
  !>
  !> @param[in] nds    data set number used for search.
  !> @param[in] fname  name of namelist.
  !>
  !> @author
  !> @date
  !>
  subroutine readmsh_iobp(nds,fname)
    !/ -------------------------------------------------------------------
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           a. roland               |
    !/                  |           f. ardhuin              |
    !/                  |                        fortran 90 |
    !/                  | last update :          06-jun-2018|
    !/                  +-----------------------------------+
    !/
    !/    15-feb-2008 : origination.                        ( version 3.13 )
    !/    25-aug-2011 : change of method for iobpd          ( version 4.04 )
    !/    06-jun-2018 : add debuginit/pdlib/debugstp/debugsetiobp
    !/                                                      ( version 6.04 )
    !/
    !
    !  1. purpose :
    !
    !      reads triangle and unstructured grid information from gmsh files
    !      calls the subroutines needed to compute grid connectivity
    !
    !  2. method :
    !
    !     look for namelist with name name in unit nds and read if found.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       nds     int.   i   data set number used for search.
    !       name    c*4    i   name of namelist.
    !       status  c*20   o   status at end of routine,
    !                            '(default values)  ' if no namelist found.
    !                            '(user def. values)' if namelist read.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name               type  module   description
    !     ------------------------------------------------------------------------------------
    !      nextln             subr.
    !      count              subr. internal count connection.
    !      spatial_grid       subr.   id.    calculate surfaces.
    !      nvectri            subr.   id.    define cell normals and angles and edge length
    !      coordmax           subr.   id.    calculate  useful grid elements
    !      area_si            subr.   id.    define connections
    !     ----------------------------------------------------------------
    !
    !
    !
    !  5. called by :
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3grid    prog.          model configuration program
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !     the only point index which is needed is ix and nx stands for the total number of grid point.
    !     iy and ny are not needed anymore, they are set to 1 in the unstructured case
    !     some noticeable arrays are:
    !                     trigp  : give the vertices of each triangle
    !     gmsh file gives too much information that is not necessarily required so data processing is needed (data sort and nesting).
    !  8. structure :
    !
    !  9. switches :
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3odatmd, only: ndse, ndst, ndso
    use w3gdatmd
    use w3servmd, only: itrace, nextln, extcde
    use constants, only: lpdlib
    use w3odatmd, only: iaproc
    !
    implicit none
    !/
    !/ parameter list
    !/
    integer, intent(in)                :: nds
    character(60), intent(in)          :: fname
    !/
    !/ local parameters
    !/
    integer                            :: i,j,k, nodes
    logical                            :: lfile_exists
    character                          :: comstr*1, space*1 = ' ', cels*64
    double precision, allocatable      :: xybtmp1(:,:)
    inquire(file=fname, exist=lfile_exists)
    if (.not. lfile_exists) return
    open(nds,file = fname,status='old')
    read (nds,'(a)') comstr
    if (comstr.eq.' ') comstr = '$'
    call nextln(comstr, nds, ndse)
    read(nds,*) i,j,k
    call nextln(comstr, nds, ndse)
    !
    ! read number of nodes and nodes from gmsh files
    !
    read(nds,*) nodes
    allocate(xybtmp1(3,nodes))
    do i= 1, nodes
      read(nds,*) j, xybtmp1(1,i), xybtmp1(2,i), xybtmp1(3,i)
      if (int(xybtmp1(3,i)) .eq. 3) iobp(i) = 3
    end do
    !
    close(nds)
  end subroutine readmsh_iobp
  !/--------------------------------------------------------------------/
  !>
  !> @brief boundary status (code duplication).
  !>
  !> @param[out] status
  !>
  !> @author mathieu dutour-sikiric
  !> @author aron roland
  !> @date   01-may-2018
  !>
  subroutine get_boundary_status(status)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                                   |
    !/                  | mathieu dutour-sikiric (irb)      |
    !/                  | aron roland (bgs it&e gmbh)       |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         01-mai-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-mai-2018 : origination.                        ( version 6.04 )
    !/
    !  1. purpose : boundary status (code duplication)
    !  2. method :
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
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
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks
    !  8. structure :
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    use w3gdatmd, only : trigp, ntri, nx
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    integer*2, intent(out) :: status(nx)
    integer :: collected(nx), nextvert(nx), prevvert(nx)
    integer :: isfinished, inext, iprev
    integer :: ipnext, ipprev, znext, ip, i, ie
    status(:) = 0
    do ie=1,ntri
      do i=1,3
        if (i.eq.1) then
          iprev=3
        else
          iprev=i-1
        end if
        if (i.eq.3) then
          inext=1
        else
          inext=i+1
        end if
        ip=trigp(i,ie)
        ipnext=trigp(inext,ie)
        ipprev=trigp(iprev,ie)
        if (status(ip).eq.0) then
          status(ip)=1
          prevvert(ip)=ipprev
          nextvert(ip)=ipnext
        end if
      end do
    end do
    status(:)=0
    do
      collected(:)=0
      do ie=1,ntri
        do i=1,3
          if (i.eq.1) then
            iprev=3
          else
            iprev=i-1
          end if
          if (i.eq.3) then
            inext=1
          else
            inext=i+1
          end if
          ip=trigp(i,ie)
          ipnext=trigp(inext,ie)
          ipprev=trigp(iprev,ie)
          if (status(ip).eq.0) then
            znext=nextvert(ip)
            if (znext.eq.ipprev) then
              collected(ip)=1
              nextvert(ip)=ipnext
              if (nextvert(ip).eq.prevvert(ip)) then
                status(ip)=1
              end if
            end if
          end if
        end do
      end do
      isfinished=1
      do ip=1,nx
        if ((collected(ip).eq.0).and.(status(ip).eq.0)) then
          status(ip)=-1
        end if
        if (status(ip).eq.0) then
          isfinished=0
        end if
      end do
      if (isfinished.eq.1) then
        exit
      end if
    end do
  end subroutine get_boundary_status
  !/ -------------------------------------------------------------------/
  !>
  !> @brief reads open boundary information for unst grids following
  !>  gmesh type format.
  !>
  !> @param[in]    nds      data set number used for search.
  !> @param[in]    fname    file name.
  !> @param[inout] tmpsta   status map to be updated (for obc,tmpsta = 2).
  !> @param[out]   ugobcok  flag for proper reading of obc file.
  !>
  !> @author f. ardhuin
  !> @date   14-mar-2018
  !>
  subroutine readmshobc(nds, fname, tmpsta, ugobcok)
    !/ -------------------------------------------------------------------
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           f. ardhuin              |
    !/                  |                        fortran 90 |
    !/                  | last update :          14-mar-2018|
    !/                  +-----------------------------------+
    !/
    !/    14-mar-2018 : origination.                        ( version 6.02 )
    !/
    !
    !  1. purpose :
    !
    !      reads open boundary information for unst grids following gmesh type format
    !
    !  2. method :
    !
    !     reads an ascii file
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       nds     int.   i   data set number used for search.
    !       fname   char*60 i  file name
    !       tmpsta  char*60 i/o status map to be updated (for obc, tmpsta = 2)
    !       ugobcok logical o   flag for proper reading of obc file
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used : none
    !
    !  5. called by :
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3grid    prog.          model configuration program
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !  9. switches :
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: nx, ny, ccon , countcon
    use w3odatmd, only: ndse, ndst, ndso
    use w3servmd, only: itrace, nextln, extcde
    implicit none
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)                :: nds
    character(60), intent(in) :: fname
    integer, intent(inout)    :: tmpsta(ny,nx)
    logical, intent(out)      :: ugobcok
    !/
    !/ local parameters
    !/
    integer                            :: i, ierr
    integer(kind=4)                    :: ind,ntag, inode
    character                          :: comstr*1, space*1 = ' ', cels*64
    real, allocatable                  :: tags(:)
    character(len=120)                 :: line
    ugobcok=.false.
    open(nds,file = fname,status='old')
    read (nds,'(a)') comstr
    if (comstr.eq.' ') comstr = '$'
    call nextln(comstr, nds, ndse)
    ierr = 0
    do while (ierr.eq.0)
      read (nds,'(a100)',end=2001,err=2002,iostat=ierr) line
      read(line,*,iostat=ierr) ind,ntag
      if (ierr.eq.0) then
        allocate(tags(ntag))
        read(line,*,iostat=ierr) ind,ntag,tags,inode
        if (ierr.eq.0) then
          tmpsta(1,inode)=2
          deallocate(tags)
        else
          goto 2001
        end if
      end if
    end do
    close(nds)
    ugobcok=.true.
    return
    !
2001 continue
    write (ndse,1001)
    call extcde ( 61 )
    !
2002 continue
    write (ndse,1002) ierr
    call extcde ( 62 )
1001 format (/' *** wavewatch iii error in readmshobc : '/          &
         '     premature end of file in reading ',a/)
1002 format (/' *** wavewatch iii error in readmshobc : '/          &
         '     error in reading ',a,'  iostat =',i8/)
  end subroutine readmshobc
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief defines open boundary points based on depth.
  !>
  !> @details a boundary node has more node around it than triangles.
  !>
  !> @param[inout] tmpsta  status map to be updated (for obc, tmpsta = 2).
  !> @param[in]    zbin
  !> @param[in]    zlim
  !>
  !> @author f. ardhuin
  !> @date   30-aug-2012
  !>
  subroutine ug_getopenboundary(tmpsta,zbin,zlim)
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           f. ardhuin              |
    !/                  |                        fortran 90 |
    !/                  | last update :          30-aug-2012|
    !/                  +-----------------------------------+
    !/
    !/    30-aug-2012 : adpatation from shom-ifremer program( version 4.07 )
    !/
    !
    !  1. purpose: defines open boundary points based on depth
    !  2. method : a boundary node has more node around it than triangles
    !
    !
    !  3. parameters :
    !     tmpsta: status map to be updated (for obc, tmpsta = 2)
    !
    !  4. subroutines used :
    !
    !  5. called by :
    !
    !       name      type  module   description
    !     ----------------------------------------------------------------
    !      w3grid    prog.          model configuration program
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !
    !  8. structure :
    !
    !  9. switches :
    !
    ! 10. source code :
    use w3gdatmd, only: nx, ny, ccon, countcon, iobp
    implicit none
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(inout)  :: tmpsta(ny,nx)
    real   , intent(in)     :: zbin(ny,nx)
    real   , intent(in)     :: zlim
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 ::  ibc, ix
    integer                 ::  mask(nx)
    integer*2               ::  status(nx)
    !
    mask(:)=1
    call set_iobp (mask, status)
    !
    do ibc = 1, n_outside_boundary
      ix = outside_boundary(ibc)
      !write(*,*) 'test1', ix, tmpsta(1,ix), ccon(ix), countcon(ix), zbin(1,ix), zlim
      ! outside_boundary(ibc) is defined over the full nodes nodes indexes
      ! whereas tmpsta and zbin are defined over the clean up list of nodes nx
      if ((ix.ne.0).and.(ix.le.nx)) then
        if ( (tmpsta(1,ix).eq.1) .and. (status(ix).eq.0) .and. (zbin(1,ix) .lt. zlim)) tmpsta(1,ix) = 2
      end if
    end do
    !
  end subroutine ug_getopenboundary
  !/ ------------------------------------------------------------------- /
  !/----------------------------------------------------------------------
  !>
  !> @brief calculates triangle areas and reorders the triangles to have
  !>  them oriented counterclockwise.
  !>
  !> @details the triangle surface calculation is based on cross product.
  !>
  !> @author a. roland
  !> @author f. ardhuin
  !> @date   31-aug-2011
  !>
  subroutine spatial_grid
    !/ -------------------------------------------------------------------
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |      a. roland  (bgs it&e gbmh)   |
    !/                  |      f. ardhuin (ifremer)         |
    !/                  |                        fortran 90 |
    !/                  | last update :          31-aug-2011|
    !/                  +-----------------------------------+
    !/
    !/    15-may-2007 : origination: adjustment from the wwm code       ( version 3.13 )
    !/    31-aug-2011 : simplfies the cross products                    ( version 4.05 )
    !/
    !
    !  1. purpose :
    !
    !      calculates triangle areas and reorders the triangles to have them
    !      oriented counterclockwise
    !
    !  2. method :
    !
    !     the triangle surface calculation is based on cross product.
    !
    !  3. parameters :
    !
    !  4. subroutines used :
    !
    !  5. called by :
    !
    !       name      type  module   description
    !     ----------------------------------------------------------------
    !      readtri    subr. internal  unstructured mesh definition.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !     this part of code is adapted from the wwm wave model develop at the darmstadt university
    !     (aaron roland)
    !
    !  8. structure :
    !
    !  9. switches :
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd
    use w3odatmd, only: ndse
    implicit none
    !
    !local parameters
    !
    real              :: tl1, tl2, tl3, tmptrigp
    integer           :: i1, i2, i3
    integer           :: k
    real*8            :: pt(3,2)
    !/ ------------------------------------------------------------------- /
    do k = 1, ntri
      i1 = trigp(1,k)
      i2 = trigp(2,k)
      i3 = trigp(3,k)
!ar: todo call this only for global grid 
      call fix_periodcity(i1,i2,i3,xgrd,ygrd,pt)
      !
      ! cross product of edge-vector  (orientated anticlockwise)
      !
      tria(k) = real( (pt(2,2)-pt(1,2)) & 
           *(pt(1,1)-pt(3,1))      &    
           +(pt(3,2)-pt(1,2))      &    
           *(pt(2,1)-pt(1,1))      )*0.5
      !
      ! test on negative triangle area, which means that the orientiation is not as assumed to be anticw.
      ! therefore we swap the nodes !!!
      !
      if (tria(k) .lt. tiny(1.)) then
        tmptrigp = trigp(2,k)
        trigp(2,k) = trigp(3,k)
        trigp(3,k) = tmptrigp
        i2 = trigp(2,k)
        i3 = trigp(3,k)
        tria(k) = -1.d0*tria(k)
      end if
    end do
  end subroutine spatial_grid
  !/--------------------------------------------------------------------/
  !
  !/--------------------------------------------------------------------/
  !>
  !> @brief calculate cell tools: inward normal, angles and length of edges.
  !>
  !> @details to get inward pointing normals, triangle are glanced through
  !>  anti-clockwisely.
  !>
  !> @author a. roland
  !> @date   15-may-2008
  !>
  subroutine nvectri
    !/ -------------------------------------------------------------------
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           a. roland               |
    !/                  |                        fortran 90 |
    !/                  | last update :          15-may-2008|
    !/                  +-----------------------------------+
    !/
    !/    15-may-2007 : origination: adjustment from the wwm code       ( version 3.13 )
    !/
    !
    !  1. purpose :
    !
    !      calculate cell tools: inward normal, angles and length of edges.
    !
    !  2. method :
    !      to get inward pointing normals, triangle are glanced through anti-clockwisely
    !
    !
    !  3. parameters :
    !
    !  4. subroutines used :
    !
    !  5. called by :
    !
    !       name      type  module   description
    !     ----------------------------------------------------------------
    !      readtri    subr. internal  unstructured mesh definition.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !  9. switches :
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd
    use constants
    implicit none
    !
    !local parameter
    !
    integer :: ip, ie
    integer :: i1, i2, i3, i11, i22, i33
    !
    real*8    :: p1(2), p2(2), p3(2)
    real*8    :: r1(2), r2(2), r3(2)
    real*8    :: n1(2), n2(2), n3(2)
    real*8    :: tmp(3)
    real*8    :: tmpinv(3)
    real*8    :: pt(3,2)
    !/ ------------------------------------------------------------------- /
    !
    do ie = 1, ntri
      !
      ! vertices
      !
      i1 = trigp(1,ie)
      i2 = trigp(2,ie)
      i3 = trigp(3,ie)
      call fix_periodcity(i1,i2,i3,xgrd,ygrd,pt)
      p1(1) = pt(1,1)
      p1(2) = pt(1,2)
      p2(1) = pt(2,1)
      p2(2) = pt(2,2)
      p3(1) = pt(3,1)
      p3(2) = pt(3,2)
      !
      ! i1 -> i2, i2 -> i3, i3 -> i1 (anticlockwise orientation is preserved)
      !
      r1 = p3-p2
      r2 = p1-p3
      r3 = p2-p1
      n1(1) = (-r1(2))
      n1(2) = ( r1(1))
      n2(1) = (-r2(2))
      n2(2) = ( r2(1))
      n3(1) = (-r3(2))
      n3(2) = ( r3(1))
      !
      ! edges length
      !
      len(ie,1) = dsqrt(r1(1)**2+r1(2)**2)
      len(ie,2) = dsqrt(r2(1)**2+r2(2)**2)
      len(ie,3) = dsqrt(r3(1)**2+r3(2)**2)
      !
      ! inward normal used for propagation (not normalized)
      !
      ien(ie,1) = n1(1)
      ien(ie,2) = n1(2)
      ien(ie,3) = n2(1)
      ien(ie,4) = n2(2)
      ien(ie,5) = n3(1)
      ien(ie,6) = n3(2)
    end do
  end subroutine nvectri
  !/---------------------------------------------------------------------------
  !/------------------------------------------------------------------------
  !>
  !> @brief calculate global and maximum number of connection for array
  !>  allocations.
  !>
  !> @param[in] trigptemp  temporary array of triangle vertices.
  !>
  !> @author a. roland
  !> @author f. ardhuin
  !> @date   15-may-2008
  !>
  subroutine count(trigptemp)
    !/ -------------------------------------------------------------------
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           a. roland               |
    !/                  |           f. ardhuin              |
    !/                  |                        fortran 90 |
    !/                  | last update :          15-may-2008|
    !/                  +-----------------------------------+
    !/
    !/    15-may-2007 : origination.                        ( version 3.13 )
    !/
    !
    !  1. purpose :
    !
    !      calculate global and maximum number of connection for array allocations .
    !
    !  2. method :
    !
    !  3. parameters :
    !     parameter list
    !     ----------------------------------------------------------------
    !       ntri         int.   i   total number of triangle.
    !       trigptemp    int    i   temporary array of triangle vertices
    !       countri      int    o   maximum number of connected triangle
    !                               for a given points
    !       countot      int    o   global number of triangle connection
    !                               for the whole grid.
    !     ----------------------------------------------------------------
    !  4. subroutines used :
    !
    !  5. called by :
    !
    !       name      type  module   description
    !     ----------------------------------------------------------------
    !      readtri    subr. internal  unstructured mesh definition.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !  9. switches :
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd
    implicit none
    !/ parameter list
    integer,intent(in) :: trigptemp(:,:)
    !/ ------------------------------------------------------------------- /
    !/ local parameter
    integer               :: conn(nx)
    integer               :: counter, ip, ie, i, j, n(3)
    !/------------------------------------------------------------------------
    countri=0
    countot=0
    conn(:)= 0
    !
    !calculate the number of connected triangles for a given point.
    !
    do ie = 1,ntri
      n(:) = 0.
      n(1) = trigptemp(1,ie)
      n(2) = trigptemp(2,ie)
      n(3) = trigptemp(3,ie)
      conn(n(1)) = conn(n(1)) + 1
      conn(n(2)) = conn(n(2)) + 1
      conn(n(3)) = conn(n(3)) + 1
    enddo
    countri = maxval(conn)
    !
    ! calculate the global number of connections available through the mesh
    !
    j=0
    do  ip=1,nx
      do i=1,conn(ip)
        j=j+1
      enddo
    enddo
    countot=j
  end subroutine count
  !/----------------------------------------------------------------------------
  !>
  !> @brief calculate first point and last point coordinates, and minimum and
  !>  maximum edge length.
  !>
  !> @author f. ardhuin
  !> @date   15-may-2008
  !>
  subroutine coordmax
    !/ -------------------------------------------------------------------
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           f. ardhuin              |
    !/                  |                        fortran 90 |
    !/                  | last update :          15-may-2008|
    !/                  +-----------------------------------+
    !/
    !/    15-may-2007 : origination.                        ( version 3.13 )
    !/
    !  1. purpose :
    !
    !      calculate first point and last point coordinates, and minimum and maximum edge length.
    !
    !  2. method :
    !
    !  3. parameters :
    !
    !  4. subroutines used :
    !
    !  5. called by :
    !
    !       name      type  module   description
    !     ----------------------------------------------------------------
    !      readtri    subr. internal  unstructured mesh definition.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !  9. switches :
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd
    implicit none
    !
    ! maximum of coordinates s
    !
    maxx = maxval(xgrd(1,:))
    maxy = maxval(ygrd(1,:))
    !
    ! minimum of coordinates
    !
    x0 = minval(xgrd(1,:))
    y0 = minval(ygrd(1,:))
    !
    !maximum and minimum length of edges
    !
    dxymax = maxval(len(:,:))
    sx = minval(len(:,:))
    sy = sx
    !
  end subroutine coordmax
  !-------------------------------------------------------------------------
  !>
  !> @brief define optimized connection arrays (points and triangles) for
  !>  spatial propagation schemes.
  !>
  !> @details the storage is optimize especially considering the iterative solver used.
  !>  the schemes used are vertex-centered, a point has to be considered within its
  !>  median dual cell. for a given point, the surface of the dual cell is one third
  !>  of the sum of the surface of connected triangles.
  !>
  !>  this routine is from wwm developed in darmstadt(aaron roland).
  !>
  !> @param[in] imod  model number to point to.
  !>
  !> @author a. roland
  !> @date   23-aug-2011
  !>
  subroutine area_si(imod)
    !/ -------------------------------------------------------------------
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           a. roland               |
    !/                  |                        fortran 90 |
    !/                  | last update :          23-aug-2011|
    !/                  +-----------------------------------+
    !/
    !/    15-may-2007 : origination: adjustment from the wwm code       ( version 3.13 )
    !/    23-aug-2011 : removes double entries in vneigh                ( version 4.04 )
    !/
    !
    !  1. purpose :
    !
    !      define optimized connection arrays (points and triangles) for spatial propagation schemes.
    !
    !  2. method :
    !
    !  3. parameters :
    !
    !  4. subroutines used :
    !
    !  5. called by :
    !
    !       name      type  module   description
    !     ----------------------------------------------------------------
    !      readtri    subr. internal  unstructured mesh definition.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !     the storage is optimize especially considering the iterative solver used.
    !     the schemes used are vertex-centered, a point has to be considered within its
    !     median dual cell. for a given point, the surface of the dual cell is one third
    !     of the sum of the surface of connected triangles.
    !     this routine is from wwm developped in darmstadt(aaron roland)
    !
    !  8. structure :
    !
    !  9. switches :
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd
    implicit none
    !/ input
    integer, intent(in) :: imod
    !/ local parameters
    integer :: counter,ifound,alreadyfound
    integer :: i, j, k, ii
    integer :: ip, ie, pos, pos_i, pos_j, pos_k, ip_i, ip_j, ip_k
    integer :: i1, i2, i3, ip2, chilf(nx)
    integer :: tmp(nx), cellvertex(nx,countri,2)
    integer :: count_max
    double precision   :: tria03
    integer, allocatable :: ptable(:,:)
    !/ ------------------------------------------------------------------- /
    si(:) = 0.d0
    !
    ccon(:) = 0     ! number of connected elements
    do ie = 1, ntri
      i1 = trigp(1,ie)
      i2 = trigp(2,ie)
      i3 = trigp(3,ie)
      ccon(i1) = ccon(i1) + 1
      ccon(i2) = ccon(i2) + 1
      ccon(i3) = ccon(i3) + 1
      tria03 = 1./3. * tria(ie)
      si(i1) = si(i1) + tria03
      si(i2) = si(i2) + tria03
      si(i3) = si(i3) + tria03
    enddo
    cellvertex(:,:,:) = 0 ! stores for each node the elementnumbers of the connected elements
    ! and the position of the node in the element index
    chilf = 0
    do ie = 1, ntri
      do j=1,3
        i = trigp(j,ie)!ine(j,ie)
        chilf(i) = chilf(i)+1
        cellvertex(i,chilf(i),1) = ie
        cellvertex(i,chilf(i),2) = j
      end do
    enddo
    !
    ! second step in storage, the initial 3d array cellvertex, is transformed in a 1d array
    ! the global index is j . from now, all the computation step based on these arrays must
    ! abide by the conservation of the 2 loop algorithm (points + connected triangles)
    !
    index_cell(1)=1
    j = 0
    do ip = 1, nx
      do i = 1, ccon(ip)
        j = j + 1
        ie_cell(j)  = cellvertex(ip,i,1)
        pos_cell(j) = cellvertex(ip,i,2)
      end do
      index_cell(ip+1)=j+1
    end do
    if (.not. fsnimp) return
    j = 0
    do ip = 1, nx
      do i = 1, ccon(ip)
        j = j + 1
      end do
    end do
    count_max = j
    allocate(ptable(count_max,7))
    j = 0
    ptable(:,:) = 0.
    do ip = 1, nx
      do i = 1, ccon(ip)
        j = j + 1
        ie    = ie_cell(j)
        pos   = pos_cell(j)
        i1 = trigp(1,ie)
        i2 = trigp(2,ie)
        i3 = trigp(3,ie)
        if (pos == 1) then
          pos_j = 2
          pos_k = 3
        else if (pos == 2) then
          pos_j = 3
          pos_k = 1
        else
          pos_j = 1
          pos_k = 2
        end if
        ip_i = ip
        ip_j = trigp(pos_j,ie)
        ip_k = trigp(pos_k,ie)
        ptable(j,1) = ip_i
        ptable(j,2) = ip_j
        ptable(j,3) = ip_k
        ptable(j,4) = pos
        ptable(j,5) = pos_j
        ptable(j,6) = pos_k
        ptable(j,7) = ie
      end do
    end do
    !        write(*,'("+trace......",a)') 'set up sparse matrix pointer ... count nonzero entry'
    j = 0
    k = 0
    do ip = 1, nx
      tmp(:) = 0
      do i = 1, ccon(ip)
        j = j + 1
        ip_j  = ptable(j,2)
        ip_k  = ptable(j,3)
        pos   = ptable(j,4)
        tmp(ip)   = 1
        tmp(ip_j) = 1
        tmp(ip_k) = 1
      end do
      k = k + sum(tmp)
    end do
    nnz => grids(imod)%nnz
    nnz = k
    !          write(*,'("+trace......",a)') 'set up sparse matrix pointer ... setup pointer'
    allocate (grids(imod)%jaa(nnz))
    allocate (grids(imod)%iaa(nx+1))
    allocate (grids(imod)%posi(3,count_max))
    jaa   => grids(imod)%jaa
    iaa   => grids(imod)%iaa
    posi  => grids(imod)%posi
    j = 0
    k = 0
    iaa(1) = 1
    jaa    = 0
    do ip = 1, nx ! run through all rows
      tmp = 0
      do i = 1, ccon(ip)         ! check how many entries there are ...
        j = j + 1                ! this is the same j index as in ie_cell
        ip_j  = ptable(j,2)
        ip_k  = ptable(j,3)
        tmp(ip)   = 1
        tmp(ip_j) = 1
        tmp(ip_k) = 1
      end do
      do i = 1, nx               ! run through all columns
        if (tmp(i) .gt. 0) then  ! this is true only for the connected points
          k = k + 1
          jaa(k) = i
        end if
      end do
      iaa(ip + 1) = k + 1
    end do
    posi = 0
    j = 0
    do ip = 1, nx
      do i = 1, ccon(ip)
        j = j + 1
        ip_j  = ptable(j,2)
        ip_k  = ptable(j,3)
        do k = iaa(ip), iaa(ip+1) - 1
          if (ip   == jaa(k)) posi(1,j)  = k
          if (ip_j == jaa(k)) posi(2,j)  = k
          if (ip_k == jaa(k)) posi(3,j)  = k
          if (k == 0) then
            write(*,*) 'error in area_si k .eq. 0'
            stop
          end if
        end do
      end do
    end do
    deallocate(ptable)
  end subroutine area_si
  !>
  !> @brief determine whether a point is inside or outside an
  !>  unstructured grid, and returns index of triangle and
  !>  interpolation weights.
  !>
  !> @details this is the analogue for triangles of the function w3grmp.
  !>
  !>  using barycentric coordinates defined as the ratio of triangle
  !>  algebric areas which are positive or negative.  computes the 3
  !>  interpolation weights for each triangle until they are all positive.
  !>
  !> @param[in]  imod   model number to point to.
  !> @param[in]  xtin   x-coordinate of target point.
  !> @param[in]  ytin   y-coordinate of target point.
  !> @param[out] itout  model number to point to.
  !> @param[out] is     i indices of vertices of enclosing grid cell.
  !> @param[out] js     j indices of vertices of enclosing grid cell.
  !> @param[out] rw     array of interpolation weights.
  !>
  !> @author mathieu dutour sikiric, irb
  !> @author aron roland, z&p
  !> @author fabrice ardhuin
  !> @date   26-jan-2014
  !>
  subroutine is_in_ungrid(imod, xtin, ytin, itout, is, js, rw)
    !/ -------------------------------------------------------------------
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |      mathieu dutour sikiric, irb  |
    !/                  |                 aron roland, z&p  |
    !/                  |             fabrice ardhuin       |
    !/                  |                        fortran 90 |
    !/                  | last update :          26-jan-2014|
    !/                  +-----------------------------------+
    !/
    !/ adapted from other subroutine
    !/    15-oct-2007 : origination.                        ( version 3.13 )
    !/    21-sep-2012 : uses same interpolation as regular  ( version 4.08 )
    !/    26-jan-2014 : correcting bug in rw                ( version 4.18 )
    !/
    !  1. purpose :
    !
    !      determine whether a point is inside or outside an unstructured grid,
    !      and returns index of triangle and interpolation weights
    !      this is the analogue for triangles of the function w3grmp
    !
    !  2. method :
    !
    !     using barycentric coordinates defined as the ratio of triangle algebric areas
    !     which are positive or negative.
    !     computes the 3 interpolation weights for each triangle until they are all positive
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       imod    int.   i   model number to point to.
    !       xtin    real   i   x-coordinate of target point.
    !       ytin    real   i   y-coordinate of target point.
    !       itout    int.   i   model number to point to.
    !       is,js   i.a.   o   (i,j) indices of vertices of enclosing grid cell.
    !       rw      r.a.   o   array of interpolation weights.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     none
    !
    !  5. called by :
    !
    !     wmglow, w3iopp, wmiopp, ww3_gint
    !
    !  6. error messages :
    !
    !     - error checks on previous setting of variable.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !  9. switches :
    !
    !     !/s    enable subroutine tracing.
    !     !/t    enable test output
    !
    ! 10. source code :
    !
    !  2. method :
    !
    !     using barycentric coordinates. each coefficient depends on the mass of its related point in the interpolation.
    !
    !  3. parameters :
    !
    !  4. subroutines used :
    !
    !  5. called by :
    !
    !       name      type  module   description
    !     ----------------------------------------------------------------
    !      w3iopp    subr. internal  preprocessing of point output.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !      this subroutine is adjusted from crest code (fabrice ardhuin)
    !      for a given output point, the algorithm enable to glance through all the triangles
    !      to find the one the point belong to, and then make interpolation.
    !
    !  8. structure :
    !
    !  9. switches :
    !
    !       !/llg   spherical grid.
    !       !/xyg   carthesian grid.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd
    use w3servmd, only: extcde
    use w3odatmd, only: ndse
    implicit none
    !/ ------------------------------------------------------------------- /
    ! parameter list
    integer, intent(in)            :: imod
    double precision, intent(in)   :: xtin, ytin
    integer, intent(out)           :: itout
    integer, intent(out)           :: is(4), js(4)
    real, intent(out)              :: rw(4)
    !/ ------------------------------------------------------------------- /
    !local parameters
    double precision             :: x1, x2, x3
    double precision             :: y1, y2, y3
    double precision             :: s1, s2, s3, sg1, sg2, sg3
    real*8                       :: pt(3,2)
    integer                      :: itri
    integer                      :: i1, i2, i3
    integer                      :: nbfound
    !
    itout = 0
    nbfound=0
    itri = 0
    do while (nbfound.eq.0.and.itri.lt.grids(imod)%ntri)
      itri = itri +1
      i1=grids(imod)%trigp(1,itri)
      i2=grids(imod)%trigp(2,itri)
      i3=grids(imod)%trigp(3,itri)
      call fix_periodcity(i1,i2,i3,grids(imod)%xgrd,grids(imod)%ygrd,pt)
      ! coordinates of the first vertex a
      x1 = pt(1,1)
      y1 = pt(1,2)
      ! coordinates of the 2nd vertex b
      x2 = pt(2,1)
      y2 = pt(2,2)
      !coordinates of the 3rd vertex c
      x3 = pt(3,1)
      y3 = pt(3,2)
      !with m = (xtin,ytin) the target point ...
      !vector product of ab and ac
      sg3=(y3-y1)*(x2-x1)-(x3-x1)*(y2-y1)
      !vector product of ab and am
      s3=(ytin-y1)*(x2-x1)-(xtin-x1)*(y2-y1)
      !vector product of bc and ba
      sg1=(y1-y2)*(x3-x2)-(x1-x2)*(y3-y2)
      !vector product of bc and bm
      s1=(ytin-y2)*(x3-x2)-(xtin-x2)*(y3-y2)
      !vector product of ca and cb
      sg2=(y2-y3)*(x1-x3)-(x2-x3)*(y1-y3)
      !vector product of ca and cm
      s2=(ytin-y3)*(x1-x3)-(xtin-x3)*(y1-y3)
      if ((s1*sg1.ge.0).and.(s2*sg2.ge.0).and.(s3*sg3.ge.0)) then
        itout=itri
        nbfound=nbfound+1
        is(1)=i1
        is(2)=i2
        is(3)=i3
        is(4)=1
        js(:)=1
        rw(1)=s1/sg1
        rw(2)=s2/sg2
        rw(3)=1.-rw(1)-rw(2)  !s3/sg3
        rw(4)=0.
      end if
    enddo
  end subroutine is_in_ungrid
  !/ -------------------------------------------------------------------
  !>
  !> @brief determine whether a point is inside or outside an
  !>  unstructured grid, and returns index of triangle and
  !>  interpolation weights.
  !>
  !> @details this is the analogue for triangles of the function w3grmp.
  !>
  !>  using barycentric coordinates defined as the ratio of triangle
  !>  algebric areas which are positive or negative.  computes the 3
  !>  interpolation weights for each triangle until they are all positive.
  !>
  !> @param[in]  imod   model number to point to.
  !> @param[in]  xtin   x-coordinate of target point.
  !> @param[in]  ytin   y-coordinate of target point.
  !> @param[in]  force
  !> @param[out] itout  model number to point to.
  !> @param[out] is     i indices of vertices of enclosing grid cell.
  !> @param[out] js     j indices of vertices of enclosing grid cell.
  !> @param[out] rw     array of interpolation weights.
  !>
  !> @author mathieu dutour sikiric, irb
  !> @author aron roland, z&p
  !> @author fabrice ardhuin
  !> @date   26-jan-2014
  !>
  subroutine is_in_ungrid2(imod, xtin, ytin, force, itout, is, js, rw)
    !/ -------------------------------------------------------------------
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |      mathieu dutour sikiric, irb  |
    !/                  |                 aron roland, z&p  |
    !/                  |             fabrice ardhuin       |
    !/                  |                        fortran 90 |
    !/                  | last update :          26-jan-2014|
    !/                  +-----------------------------------+
    !/
    !/ adapted from other subroutine
    !/    15-oct-2007 : origination.                        ( version 3.13 )
    !/    21-sep-2012 : uses same interpolation as regular  ( version 4.08 )
    !/    26-jan-2014 : correcting bug in rw                ( version 4.18 )
    !/
    !  1. purpose :
    !
    !      determine whether a point is inside or outside an unstructured grid,
    !      and returns index of triangle and interpolation weights
    !      this is the analogue for triangles of the function w3grmp
    !
    !  2. method :
    !
    !     using barycentric coordinates defined as the ratio of triangle algebric areas
    !     which are positive or negative.
    !     computes the 3 interpolation weights for each triangle until they are all positive
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       imod    int.   i   model number to point to.
    !       xtin    real   i   x-coordinate of target point.
    !       ytin    real   i   y-coordinate of target point.
    !       itout    int.   i   model number to point to.
    !       is,js   i.a.   o   (i,j) indices of vertices of enclosing grid cell.
    !       rw      r.a.   o   array of interpolation weights.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     none
    !
    !  5. called by :
    !
    !     wmglow, w3iopp, wmiopp, ww3_gint
    !
    !  6. error messages :
    !
    !     - error checks on previous setting of variable.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !  9. switches :
    !
    !     !/s    enable subroutine tracing.
    !     !/t    enable test output
    !
    ! 10. source code :
    !
    !  2. method :
    !
    !     using barycentric coordinates. each coefficient depends on the mass of its related point in the interpolation.
    !
    !  3. parameters :
    !
    !  4. subroutines used :
    !
    !  5. called by :
    !
    !       name      type  module   description
    !     ----------------------------------------------------------------
    !      w3iopp    subr. internal  preprocessing of point output.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !      this subroutine is adjusted from crest code (fabrice ardhuin)
    !      for a given output point, the algorithm enable to glance through all the triangles
    !      to find the one the point belong to, and then make interpolation.
    !
    !  8. structure :
    !
    !  9. switches :
    !
    !       !/llg   spherical grid.
    !       !/xyg   carthesian grid.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd
    use w3servmd, only: extcde
    use w3odatmd, only: ndse
    implicit none
    !/ ------------------------------------------------------------------- /
    ! parameter list
    integer, intent(in)            :: imod, force
    double precision, intent(in)            :: xtin, ytin
    integer, intent(out)           :: itout
    integer, intent(out)           :: is(4), js(4)
    real, intent(out)              :: rw(4)
    !/ ------------------------------------------------------------------- /
    !local parameters
    double precision             :: x1, x2, x3, d1, d2, d3, distmin, ddmin
    double precision             :: s1, s2, s3, sg1, sg2, sg3, smin, ssum
    double precision             :: y1, y2, y3
    integer                      :: itri, itris
    integer                      :: i1, i2, i3
    integer                      :: nbfound
    logical                      :: mapstaok
    !
    itout = 0
    nbfound=0
    itri = 0
    itris = 1
    ssum = 0
    smin = 0
    do while (nbfound.eq.0.and.itri.lt.grids(imod)%ntri)
      itri = itri +1
      i1=grids(imod)%trigp(1,itri)
      i2=grids(imod)%trigp(2,itri)
      i3=grids(imod)%trigp(3,itri)
      ! coordinates of the first vertex a
      x1=grids(imod)%xgrd(1,i1)
      y1=grids(imod)%ygrd(1,i1)
      ! coordinates of the 2nd vertex b
      x2=grids(imod)%xgrd(1,i2)
      y2=grids(imod)%xgrd(1,i2)
      !coordinates of the 3rd vertex c
      x3=grids(imod)%xgrd(1,i3)
      y3=grids(imod)%ygrd(1,i3)
      !with m = (xtin,ytin) the target point ...
      !vector product of ab and ac
      sg3=(y3-y1)*(x2-x1)-(x3-x1)*(y2-y1)
      !vector product of ab and am
      s3=(ytin-y1)*(x2-x1)-(xtin-x1)*(y2-y1)
      !vector product of bc and ba
      sg1=(y1-y2)*(x3-x2)-(x1-x2)*(y3-y2)
      !vector product of bc and bm
      s1=(ytin-y2)*(x3-x2)-(xtin-x2)*(y3-y2)
      !vector product of ca and cb
      sg2=(y2-y3)*(x1-x3)-(x2-x3)*(y1-y3)
      !vector product of ca and cm
      s2=(ytin-y3)*(x1-x3)-(xtin-x3)*(y1-y3)
      !     ssum = abs(s1*sg1)+abs(s2*sg2)+abs(s3*sg3)
      mapstaok = ((grids(imod)%mapsta(1,i1).ge.1).and. &
           (grids(imod)%mapsta(1,i2).ge.1).and.(grids(imod)%mapsta(1,i3).ge.1))
      if (force.lt.2) mapstaok =.true.
      ssum = (xtin-(x1+x2+x3)/3.)**2+(ytin-(y1+y2+y2)/3.)**2
      if (smin.eq.0.and. mapstaok ) smin=ssum
      !write(6,*) 'ssum',itri,mapstaok,ssum,smin
      if (ssum.lt.smin .and.  mapstaok  ) then
        smin=ssum
        itris=itri
      endif
      if ((s1*sg1.ge.0).and.(s2*sg2.ge.0).and.(s3*sg3.ge.0)) then
        itout=itri
        nbfound=nbfound+1
        is(1)=i1
        is(2)=i2
        is(3)=i3
        is(4)=1
        js(:)=1
        rw(1)=s1/sg1
        rw(2)=s2/sg2
        rw(3)=1.-rw(1)-rw(2)  !s3/sg3
        rw(4)=0.
      end if
    enddo
    if (itout.eq.0.and.force.gt.0) then
      itri=itris
      i1=grids(imod)%trigp(1,itri)
      i2=grids(imod)%trigp(2,itri)
      i3=grids(imod)%trigp(3,itri)
      ! coordinates of the first vertex a
      x1=grids(imod)%xgrd(1,i1)
      y1=grids(imod)%ygrd(1,i1)
      ! coordinates of the 2nd vertex b
      x2=grids(imod)%xgrd(1,i2)
      y2=grids(imod)%ygrd(1,i2)
      !coordinates of the 3rd vertex c
      x3=grids(imod)%xgrd(1,i3)
      y3=grids(imod)%ygrd(1,i3)
      d1=(xtin-x1)**2+(ytin-y1)**2
      d2=(xtin-x2)**2+(ytin-y2)**2
      d3=(xtin-x3)**2+(ytin-y3)**2
      if (d1.le.d2.and.d1.le.d3) is(1)=i1
      if (d2.le.d1.and.d2.le.d3) is(1)=i2
      if (d3.le.d2.and.d3.le.d1) is(1)=i3
      is(2:4)=1
      js(:)=1
      rw(1)=1
      rw(2:4)=0.
      itout=itri
    endif
  end subroutine is_in_ungrid2
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief calculate gradients at a point via its connection.
  !>
  !> @details using linear shape function this is a basis on which
  !>  all advection schemes in roland (2008) are checked.
  !>
  !> @param[in]  param  depth or current field (indices 0 to nsea).
  !> @param[out] diffx  x gradient (indices 1 to nx).
  !> @param[out] diffy  y gradient (indices 1 to ny).
  !>
  !> @author f. ardhuin
  !> @author a. roland
  !> @date   14-oct-2013
  !>
  subroutine ug_gradients (param, diffx, diffy)
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           f. ardhuin              |
    !/                  |           a. roland               |
    !/                  |                        fortran 90 |
    !/                  | last update :          14-oct-2013|
    !/                  +-----------------------------------+
    !/
    !/    15-nov-2007 : origination.                        ( version 3.13 )
    !/    31-oct-2010 : merging of 4.03 with 3.14-ifremer   ( version 4.04 )
    !/    08-nov-2011 : correction for zero grad. on contour( version 4.04 )
    !/    14-oct-2013 : correction  of latitude factor      ( version 4.12 )
    !/    01-mai-2018 : using linear shape function for gradients [ version 6.04)
    !/
    !
    !  1. purpose: calculate gradients at a point via its connection.
    !  2. method : using linear shape function this is a basis on which
    !              all advection schemes in roland (2008) are checked.
    !
    !  3. parameters :
    !     param : depth or current field (indices 0 to nsea)
    !     diffx :  x gradient            (indices 1 to nx)
    !     diffy :  y gradient            (indices 1 to nx)
    !
    !  4. subroutines used :
    !
    !  5. called by :
    !
    !       name      type  module   description
    !     ----------------------------------------------------------------
    !      w3wave    subr.          actual wind wave routine
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !      this subroutine is adjusted from wwm code (aaron roland)
    !
    !  8. structure :
    !
    !  9. switches :
    !
    ! 10. source code :
    use constants
    use w3gdatmd, only : trigp, ntri, nx, nsea, mapfs, clatis, &
         mapsta, angle, flagll,  iobp, ien, tria, nseal, ntri
    use w3adatmd, only : nsealm
    implicit none
    real, intent(in)     :: param(0:nsea)
    real, intent(out)  :: diffx(:,:), diffy(:,:)
    ! local parameters
    integer              :: vertices(3), ni(3), ni_gl(3)
    real                 :: tmp1(3), tmp2(3)
    integer              :: i, ix, ie, ie_gl
    real                 :: var(3), fact, latmean
    real                 :: diffxtmp, diffytmp
    real                 :: dedx(3), dedy(3)
    real                 :: dvdxie, dvdyie
    real                 :: wei(nx), wei_local(nseal)
    real*8               :: rtmp(nseal)
    diffx = 0.
    diffy = 0.
    !
    if (flagll) then
      fact=1./(dera*radius)
    else
      fact=1.
    end if
      wei = 0.
      do ie = 1, ntri
        ni = trigp(:,ie)
        latmean = 1./3. * sum(clatis(mapfs(1,ni)))
        wei(ni) = wei(ni) + 2.*tria(ie)
        dedx(1)     = ien(ie,1)
        dedx(2)     = ien(ie,3)
        dedx(3)     = ien(ie,5)
        dedy(1)     = ien(ie,2)
        dedy(2)     = ien(ie,4)
        dedy(3)     = ien(ie,6)
        var         = param(mapfs(1,ni)) * fact
        dvdxie      = dot_product( var,dedx)
        dvdyie      = dot_product( var,dedy)
        diffx(1,ni) = diffx(1,ni) + dvdxie * latmean
        diffy(1,ni) = diffy(1,ni) + dvdyie
      end do
      diffx(1,:) = diffx(1,:)/wei
      diffy(1,:) = diffy(1,:)/wei
    !
  end subroutine ug_gradients
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief ugtype nesting initialization.
  !>
  !> @param[in]    distmin
  !> @param[inout] flok
  !>
  !> @author aron roland
  !> @author mathieu dutour-sikiric
  !> @date   01-jun-2018
  !>
  subroutine w3nestug(distmin,flok)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                                   |
    !/                  | aron roland (bgs it&e gmbh)       |
    !/                  | mathieu dutour-sikiric (irb)      |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :        01-june-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-june-2018 : origination.                        ( version 6.04 )
    !/
    !  1. purpose : ugtype nesting initialization
    !  2. method :
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
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
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks
    !  8. structure :
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    use w3odatmd, only: nbi, ndse, isbpi, xbpi, ybpi
    use w3gdatmd, only: nx, xgrd, ygrd, mapsta, mapfs, mapsf
    real, intent(in)         :: distmin
    logical, intent(inout)         :: flok
    integer                   :: i, j, jmemo, is, ix,  n, ix1(nbi)
    real                      :: dist, dist0
    !
    n = 0
    !
    !1. look for input boundary point index
    ! warning: if land points are included as boundary points to abide by the nest
    ! file, their status should be -2.
    !
    ix1 = 0
    isbpi = 1
    do ix = 1, nx
      if (abs(mapsta (1,ix)) .eq. 2) then
        n = n + 1
        if (n.gt.nbi) then
          write(ndse,*) 'error: boundary node index > nbi ... nest.ww3 file is not consistent with mod_def.ww3'
          stop
        endif
        ix1(n) = ix
      end if
    end do
    !
    !2. matches the model grid points (where mapsta = 2) with the points in nest.ww3
    !   for this, we use the nearest point in the nest file.
    !
    do i = 1, nbi
      dist0 = huge(1.)
      is = 1
      do j = 1, n
        dist = (xbpi(i) - xgrd(1,ix1(j)))**2 + (ybpi(i) - ygrd(1,ix1(j)))**2
        if (dist.lt.dist0) then
          is = mapfs(1,ix1(j))
          dist0 = dist
          jmemo = j
        end if
      end do
      dist0 = sqrt(dist0)
      if (dist0.le.distmin) then
        isbpi(i) = is
      else
        flok=.true.
      end if
    end do
    if ( n .ne. nbi) then
      write(ndse ,900) n, nbi
      do j=1,n
        write(6,*) 'this point has mapsta=2:',isbpi(j)
      end do
      isbpi(n+1:nbi)=isbpi(1)
    end if
900 format (/' *** wavewatch iii error in w3iobc : '/                &
         '     number of mapsta=2 differs from number in nest.ww3    '/                &
         '     check nest.ww3 and ww3_grid.inp ',2i8/)
  end subroutine w3nestug
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief setup boundary pointer.
  !>
  !> @param[inout] mask
  !> @param[inout] status
  !>
  !> @author aron roland
  !> @author mathiew dutour-sikiric
  !> @date   01-jun-2018
  !>
  subroutine set_iobp (mask, status)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                                   |
    !/                  | aron roland (bgs it&e gmbh)       |
    !/                  | mathieu dutour-sikiric (irb)      |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :        01-june-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-june-2018 : origination.                        ( version 6.04 )
    !/
    !  1. purpose : setup boundary pointer
    !  2. method :
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
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
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks
    !  8. structure :
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    !/
    !
    use constants
    !
    !
    use w3gdatmd, only: nx, ntri, trigp
    use w3odatmd, only: iaproc
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)   :: mask(nx)
    integer*2, intent(out)  :: status(nx)
    !
    integer :: collected(nx), nextvert(nx), prevvert(nx)
    integer          :: isfinished !, inext, iprev
    integer :: inext(3), iprev(3)
    integer          :: znext, ip, i, ie, ipnext, ipprev, count
    integer nb0, nb1, nbm1
    status = -1
    inext=(/ 2, 3, 1 /) !iprev=1+mod(i+1,3)
    iprev=(/ 3, 1, 2 /) !inext=1+mod(i,3)
    do ie=1,ntri
      ! if one of the points of the triangle is masked out (land) then do as if triangle does not exist...
      !        if ((mask(trigp(1,ie)).gt.0).and.(mask(trigp(2,ie)).gt.0).and.(mask(trigp(3,ie)).gt.0)) then
      do i=1,3
        ip=trigp(i,ie)
        call triang_indexes(i, ipnext, ipprev)
        !ipnext=trigp(inext(i),ie)
        !ipprev=trigp(iprev(i),ie)
        if (status(ip).eq.-1) then
          status(ip)=1
          prevvert(ip)=ipprev
          nextvert(ip)=ipnext
        end if
      end do
      !        endif
    end do
    status(:)=-1
    !
    count = 0
    do
      count = count + 1
      collected(:)=0
      do ie=1,ntri
        !        if ((mask(trigp(1,ie)).gt.0).and.(mask(trigp(2,ie)).gt.0).and.(mask(trigp(3,ie)).gt.0)) then
        do i=1,3
          ip=trigp(i,ie)
          call triang_indexes(i, ipnext, ipprev)
          !ipnext=trigp(inext(i),ie)
          !ipprev=trigp(iprev(i),ie)
          if (status(ip).eq.-1) then
            znext=nextvert(ip)
            if (znext.eq.ipprev) then
              collected(ip)=1
              nextvert(ip)=ipnext
              if (nextvert(ip).eq.prevvert(ip)) then
                status(ip)=1
              end if
            end if
          end if
        end do
        !            end if ! end of test on mask
      end do
      !
      ! checks that all nodes have been treated ...
      !
      isfinished=1
      do ip=1,nx
        if (mask(ip).le.0) then
          status(ip)=0
        else
          if ((collected(ip).eq.0).and.(status(ip).eq.-1)) then
            status(ip)=0
          end if
          if (status(ip).eq.-1) then
            isfinished=0
          end if
        endif
      end do
      if (isfinished.eq.1) then
        exit
      end if
    end do
    status = 1
    call get_boundary(nx, ntri, trigp, status, prevvert, nextvert)
    !#ifdef mpi_parall_grid
    !      call exchange_p2di(status)
    !#endif
  end subroutine set_iobp
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief find boundary points.
  !>
  !> @param[in]    mnp
  !> @param[in]    mne
  !> @param[in]    trigp
  !> @param[inout] iobp
  !> @param[inout] neighbor_prev
  !> @param[inout] neighbor_next
  !>
  !> @author aron roland
  !> @author mathieu dutour-sikiric
  !> @date   01-jun-2018
  !>
  subroutine get_boundary(mnp, mne, trigp, iobp, neighbor_prev,       &
       &   neighbor_next)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                                   |
    !/                  | aron roland (bgs it&e gmbh)       |
    !/                  | mathieu dutour-sikiric (irb)      |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :        01-june-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-june-2018 : origination.                        ( version 6.04 )
    !/
    !  1. purpose : find boundary points
    !  2. method :
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
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
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks
    !  8. structure :
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    use w3servmd, only: extcde
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer, intent(in)             :: mnp, mne, trigp(3,mne)
    integer*2, intent(inout)        :: iobp(mnp)
    integer, intent(inout)          :: neighbor_prev(mnp)
    integer, intent(inout)          :: neighbor_next(mnp)
    integer, pointer :: status(:)
    integer, pointer :: collected(:)
    integer, pointer :: nextvert(:)
    integer, pointer :: prevvert(:)
    integer :: ie, i, ip, ip2, ip3
    integer :: isfinished, inext, iprev, istat
    integer :: ipnext, ipprev, znext, zprev
    allocate(status(mnp), stat=istat)
    check_alloc_status ( istat )
    allocate(collected(mnp), stat=istat)
    check_alloc_status ( istat )
    allocate(prevvert(mnp), stat=istat)
    check_alloc_status ( istat )
    allocate(nextvert(mnp), stat=istat)
    check_alloc_status ( istat )
    neighbor_next = 0
    neighbor_prev = 0
    !  now computing the next items
    status = 0
    nextvert = 0
    prevvert = 0
    do ie=1,mne
      do i=1,3
        call triang_indexes(i, inext, iprev)
        ip=trigp(i,ie)
        ipnext=trigp(inext,ie)
        ipprev=trigp(iprev,ie)
        if (status(ip).eq.0) then
          status(ip)=1
          prevvert(ip)=ipprev
          nextvert(ip)=ipnext
        end if
      end do
    end do
    status(:)=0
    do
      collected(:)=0
      do ie=1,mne
        do i=1,3
          call triang_indexes(i, inext, iprev)
          ip=trigp(i,ie)
          ipnext=trigp(inext,ie)
          ipprev=trigp(iprev,ie)
          if (status(ip).eq.0) then
            znext=nextvert(ip)
            if (znext.eq.ipprev) then
              collected(ip)=1
              nextvert(ip)=ipnext
              if (nextvert(ip).eq.prevvert(ip)) then
                status(ip)=1
              end if
            end if
          end if
        end do
      end do
      isfinished=1
      do ip=1,mnp
        if ((collected(ip).eq.0).and.(status(ip).eq.0)) then
          status(ip)=-1
          neighbor_next(ip)=nextvert(ip)
        end if
        if (status(ip).eq.0) then
          isfinished=0
        end if
      end do
      if (isfinished.eq.1) then
        exit
      end if
    end do
    !  now computing the prev items
    status = 0
    nextvert = 0
    prevvert = 0
    do ie=1,mne
      do i=1,3
        call triang_indexes(i, inext, iprev)
        ip=trigp(i,ie)
        ipnext=trigp(inext,ie)
        ipprev=trigp(iprev,ie)
        if (status(ip).eq.0) then
          status(ip)=1
          prevvert(ip)=ipprev
          nextvert(ip)=ipnext
        end if
      end do
    end do
    status(:)=0
    do
      collected(:)=0
      do ie=1,mne
        do i=1,3
          call triang_indexes(i, inext, iprev)
          ip=trigp(i,ie)
          ipnext=trigp(inext,ie)
          ipprev=trigp(iprev,ie)
          if (status(ip).eq.0) then
            zprev=prevvert(ip)
            if (zprev.eq.ipnext) then
              collected(ip)=1
              prevvert(ip)=ipprev
              if (prevvert(ip).eq.nextvert(ip)) then
                status(ip)=1
              end if
            end if
          end if
        end do
      end do
      isfinished=1
      do ip=1,mnp
        if ((collected(ip).eq.0).and.(status(ip).eq.0)) then
          status(ip)=-1
          neighbor_prev(ip)=prevvert(ip)     ! new code
        end if
        if (status(ip).eq.0) then
          isfinished=0
        end if
      end do
      if (isfinished.eq.1) then
        exit
      end if
    end do
    !  now making checks
    do ip=1,mnp
      ip2=neighbor_next(ip)
      if (ip2.gt.0) then
        ip3=neighbor_prev(ip2)
        if (abs(ip3 - ip).gt.0) then
          write(*,*) 'ip=', ip, ' ip2=', ip2, ' ip3=', ip3
          write(*,*) 'we have a dramatic inconsistency'
          stop
        end if
      end if
    end do
    !   now assigning the boundary iobp array
    do ip=1,mnp
      if (status(ip).eq.-1 .and. iobp(ip) .eq. 1) then
        iobp(ip)=0
      end if
    end do
    deallocate(status, stat=istat)
    check_dealloc_status ( istat )
    deallocate(collected, stat=istat)
    check_dealloc_status ( istat )
    deallocate(nextvert, stat=istat)
    check_dealloc_status ( istat )
    deallocate(prevvert, stat=istat)
    check_dealloc_status ( istat )
  end subroutine get_boundary
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief set indices of the triangle.
  !>
  !> @param[in]  i
  !> @param[out] inext
  !> @param[out] iprev
  !>
  !> @author aron roland
  !> @author mathieu dutour-sikiric
  !> @date   01-jun-2018
  !>
  subroutine triang_indexes(i, inext, iprev)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                                   |
    !/                  | aron roland (bgs it&e gmbh)       |
    !/                  | mathieu dutour-sikiric (irb)      |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :        01-june-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-june-2018 : origination.                        ( version 6.04 )
    !/
    !  1. purpose : set indices of the triangle
    !  2. method :
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
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
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks
    !  8. structure :
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    !
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    integer, intent(in)  :: i
    integer, intent(out) :: inext, iprev
    if (i.eq.1) then
      inext=3
    else
      inext=i-1
    end if
    if (i.eq.3) then
      iprev=1
    else
      iprev=i+1
    end if
  end subroutine triang_indexes
  !/ ------------------------------------------------------------------- /
  
  !>
  !> @brief redefines the values of the boundary points and angle pointers
  !>  based on the mapsta array.
  !>
  !> @details adapted boundary detection from a. roland and m. dutour (wwm code).
  !>
  !> @author fabrice ardhuin
  !> @author aron roland
  !> @date   17-apr-2016
  !>
  subroutine set_ug_iobp()
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |        fabrice ardhuin            |
    !/                  |        aron roland                |
    !/                  |                        fortran 90 |
    !/                  | last update :         17-apr-2016 |
    !/                  +-----------------------------------+
    !/
    !/    23-aug-2011 : origination.                        ( version 4.04 )
    !/    17-apr-2016 : uses optimized boundary detection   ( version 5.10 )
    !/
    !  1. purpose :
    !
    !     redefines the values of the boundary points and angle pointers
    !     based on the mapsta array
    !
    !  2. method :
    !
    !     adapted boundary detection from a. roland and m. dutour (wwm code)
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !     local variables.
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      ww3_grid  prog. ww3_grid grid preprocessor
    !      w3ulev    subr. w3updtmd water level update
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !       none.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !
    !  9. switches :
    !
    !       !/s     enable subroutine tracing.
    !
    !
    ! 10. source code :
    !/ ------------------------------------------------------------------- /
    !/
    !
    use constants
    !
    !
    use w3gdatmd, only: nx, ny, nsea, mapfs,                        &
         nk, nth, dth, xfr, mapsta, countri,         &
         ecos, esin, ien, ntri, trigp,               &
         iobp,iobpd, iobpa,                          &
         angle0, angle
    use w3odatmd, only: tbpi0, tbpin, flbpi
    use w3adatmd, only: cg, cx, cy, atrnx, atrny, itime, cflxymax
    use w3idatmd, only: flcur
    use w3odatmd, only : iaproc
    implicit none
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ith, ix, i, j, ip, ie, ndirsum
    real (kind = 8)         :: cossum, sinsum
    real (kind = 8)         :: dirmin, dirmax, shift, tempo, dircoast
    real (kind = 8)         :: x1, x2, y1, y2, dxp1, dxp2, dxp3
    real (kind = 8)         :: dyp1, dyp2, dyp3, edet1, edet2, evx, evy
    real(kind=8), parameter :: thr    = tiny(1.)
    integer                 :: i1, i2, i3
    integer                 :: itmp(nx), nextvert(nx), prevvert(nx)
    character(60) :: fname
    !/ ------------------------------------------------------------------- /
    !
    ! 1.  preparations --------------------------------------------------- *
    ! 1.a set constants
    !
    !
    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! 2.  searches for boundary points
    !
    itmp = mapsta(1,:)
    call set_iobp(itmp, iobp)
    fname = 'meshbnd.msh'
    call readmsh_iobp(23456,fname)
    !
    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! 3. defines directions pointing into land or sea
    !
    iobpd(:,:) = 0
    iobpa(:)   = 0
    !
    do ip=1,nx
      if (mapsta(1,ip).eq.2) then
        iobpa(ip) = 1
        iobp(ip)  = 2
      endif
    end do
    do ie = 1,ntri
      i1   =   trigp(1,ie)
      i2   =   trigp(2,ie)
      i3   =   trigp(3,ie)
      dxp1 =   ien(ie,6)
      dyp1 = - ien(ie,5)
      dxp2 =   ien(ie,2)
      dyp2 = - ien(ie,1)
      dxp3 =   ien(ie,4)
      dyp3 = - ien(ie,3)
      do ith=1,nth
        evx=ecos(ith)
        evy=esin(ith)
        do i=1,3
          if (i.eq.1) then
            x1=   dxp1
            y1=   dyp1
            x2= - dxp3
            y2= - dyp3
            ip=   i1
          end if
          if (i.eq.2) then
            x1 =   dxp2
            y1 =   dyp2
            x2 = - dxp1
            y2 = - dyp1
            ip =   i2
          end if
          if (i.eq.3) then
            x1 =   dxp3
            y1 =   dyp3
            x2 = - dxp2
            y2 = - dyp2
            ip =   i3
          end if
          if (iobp(ip) .eq. 0) then ! physical boundary
            edet1 = thr-x1*evy+y1*evx
            edet2 = thr+x2*evy-y2*evx
            if ((edet1.gt.0.).and.(edet2.gt.0.)) then
              ! this is the case of waves going towards the boundary ...
              iobpd(ith,ip)=1
            endif
          else ! water ...
            iobpd(ith,ip)=1
          end if
        end do
      end do
    end do
    do ip = 1, nx
      if ( iobpa(ip) .eq. 1 .or. iobp(ip) .eq. 3 .or. iobp(ip) .eq. 4) iobpd(:,ip) = 1
    end do
    !2do: recode for mpi
    !        if (lbcwa .or. lbcsp) then
    !          if (.not. any(iobp .eq. 2)) then
    !            call wwm_abort('you imposed boundary conditions but in the boundary file are no nodes with flag = 2')
    !          endif
    !        endif
    !#ifdef mpi_parall_grid
    !      call exchange_p2di(iobwb)
    !      do id = 1, mdc
    !        iwild = iobpd(id,:)
    !        call exchange_p2di(iwild)
    !        iobpd(id,:) = iwild
    !      enddo
    !#endif
    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! 3. updates the reflection direction and sharp / flat shoreline angle
    !
    ! recomputes the angles used in the gradients estimation
    !
    !
    return
  end subroutine set_ug_iobp
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief adjust element longitude coordinates for elements straddling the
  !>  dateline with distance of ~360 degrees.
  !>
  !> @details detect if element has nodes on both sides of dateline and adjust
  !>  coordinates so that all nodes have the same sign.
  !>
  !> @param[in]  i1
  !> @param[in]  i2
  !> @param[in]  i3
  !> @param[in]  xgrd
  !> @param[in]  ygrd
  !> @param[out] pt
  !>
  !> @author steven brus
  !> @author ali abdolali
  !> @date   21-may-2020
  !>
  subroutine fix_periodcity(i1,i2,i3,xgrd,ygrd,pt)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |        steven brus                |
    !/                  |        ali abdolali               |
    !/                  |                        fortran 90 |
    !/                  | last update :         21-may-2020 |
    !/                  +-----------------------------------+
    !/
    !/    21-may-2020 : origination.                        ( version 6.07 )
    !/
    !/
    !  1. purpose :
    !
    !     adjust element longitude coordinates for elements straddling the
    !     dateline with distance of ~360 degrees
    !
    !  2. method :
    !
    !     detect if element has nodes on both sides of dateline and adjust
    !     coordinates so that all nodes have the same sign
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    implicit none
    integer, intent(in) :: i1, i2, i3
    double precision, intent(in) :: xgrd(:,:), ygrd(:,:)
    real*8, intent(out) :: pt(3,2)
    !     ----------------------------------------------------------------
    !
    !     local variables.
    !     ----------------------------------------------------------------
    integer :: i
    integer :: r1gt180, r2gt180, r3gt180
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !  5. called by :
    !
    !      name         type  module   description
    !     ----------------------------------------------------------------
    !      spatial_grid subr. w3triam  triangle area calculation
    !      nvectri      subr. w3triam  edge length, angle, normal calcuation
    !      is_in_ungrid subr. w3triam  point in element calculation
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !       none.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !  9. switches :
    !
    ! 10. source code :
    !/ ------------------------------------------------------------------- /
    pt(1,1) = xgrd(1,i1)
    pt(1,2) = ygrd(1,i1)
    pt(2,1) = xgrd(1,i2)
    pt(2,2) = ygrd(1,i2)
    pt(3,1) = xgrd(1,i3)
    pt(3,2) = ygrd(1,i3)
    r1gt180 = merge(1, 0, abs(pt(3,1)-pt(2,1)).gt.180)
    r2gt180 = merge(1, 0, abs(pt(1,1)-pt(3,1)).gt.180)
    r3gt180 = merge(1, 0, abs(pt(2,1)-pt(1,1)).gt.180)
    ! if r1gt180+r2gt180+r3gt180 .eq. 0 the element does not cross the dateline
    ! if r1gt180+r2gt180+r3gt180 .eq. 1 the element contains the pole
    ! if r1gt180+r2gt180+r3gt180 .eq. 2 the element crosses the dateline
    if ( r1gt180 + r2gt180 == 2 ) then
      pt(3,1)=pt(3,1)-sign(360.0d0,(pt(3,1)-pt(2,1)))
    else if ( r2gt180 + r3gt180 == 2 ) then
      pt(1,1)=pt(1,1)-sign(360.0d0,(pt(1,1)-pt(2,1)))
    else if ( r1gt180 + r3gt180 == 2 ) then
      pt(2,1)=pt(2,1)-sign(360.0d0,(pt(2,1)-pt(3,1)))
    endif
    return
  end subroutine fix_periodcity
end module w3triamd
