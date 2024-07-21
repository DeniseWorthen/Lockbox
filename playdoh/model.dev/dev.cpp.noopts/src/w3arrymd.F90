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
module w3arrymd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         30-oct-2009 |
  !/                  +-----------------------------------+
  !/
  !/    copyright 2009 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     in this module all service routines for in and output (binary
  !     and test) of arrays are gathered.
  !
  !  2. variables and types :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      icol      int.  private  number of collums four array output
  !                               (if not 80, 132 assumed).
  !      nfrmax    int.  private  max number of frequencies in 1d
  !                               print plots of spectra.
  !     ----------------------------------------------------------------
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      ina2r     subr. public   read 2d real array.
  !      ina2i     subr. public   read 2d integer array.
  !      outa2r    subr. public   write 2d real array.
  !      outa2i    subr. public   write 2d integer array.
  !      outrea    subr. public   print out 1d real array.
  !      outint    subr. public   print out 1d integer array.
  !      outmat    subr. public   print out 2d real array.
  !      prtblk    subr. public   print a block-type table of a 2d
  !                               real array.
  !      prt1ds    subr. public   print plot of 1d spectrum.
  !      prt1dm    subr. public   print plot of 1d spectra.
  !      prt2ds    subr. public   print plot of 2d spectrum.
  !      angstr    subr. prt2ds   convert direction to string.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      strace    subr. w3servmd subroutine tracing.            ( !/s )
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !
  !  6. switches :
  !
  !       !/s    enable subroutine tracing troughout module.
  !       !/t    switch on test output for ina2r/i and outa2r/i.
  !
  !  7. source code :
  !
  !/ ------------------------------------------------------------------- /
  public
  !
  integer, parameter, private :: icol   = 80
  integer, parameter, private :: nfrmax = 50
  integer, parameter, private :: nfm2   = nfrmax+1
  !
contains
  !/ ------------------------------------------------------------------- /
  subroutine ina2r  (array, mx, my, lx, hx, ly, hy,              &
       nds, ndst, ndse, idfm, rform, idla, vsc, vof)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         30-oct-2009 |
    !/                  +-----------------------------------+
    !/                                  based on inar2d by n.booij, dut.
    !/
    !/    31-mar-1993 : final fortran 77                    ( version 1.18 )
    !/    29-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    30-oct-2009 : implement add offset argument.      ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    20-jan-2017 : add error exit using extcde.        ( version 6.02 )
    !/
    !  1. purpose :
    !
    !     reads 2-d array of pre-described layout and format.
    !
    !  3. parameter list
    !     ----------------------------------------------------------------
    !       array   r.a.   o   array to be read.
    !       mx,my   int.   i   declared size of array.
    !       lx,hx   int.   i   range of x-counters to be read.
    !       ly,hy   int.   i   range of y-counters to be read.
    !       nds     int.   i   unit number for dataset with array.
    !       ndst    int.   i   unit number for test output.
    !       ndse    int.   i   unit number for error messages.
    !       idfm    int.   i   format indicator.
    !                           idfm = 1 : free format.
    !                           idfm = 2 : fixed format rform.
    !                           idfm = 3 : unformatted.
    !       rform   c*(*)  i   format, if idfm = 2
    !       idla    int.   i   lay out indicator.
    !                           idla = 1 : read for iy=ly-hy, ix=lx-hx,
    !                                      ix line by ix line.
    !                           idla = 2 : idem, one read statement.
    !                           idla = 3 : read for iy=hy-ly, ix=lx,hx,
    !                                      ix line by ix line.
    !                           idla = 4 : idem, one read statement.
    !       vsc     real   i   scaling factor (multiplication).
    !       vof     real   i   add offset.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     see mudule documentation.
    !
    !  5. called by :
    !
    !     any.
    !
    !  6. error messages :
    !
    !     see error escape locations at end of routine.
    !
    !  8. structure :
    !
    !     see comments in code.
    !
    !  9. switches :
    !
    !     !/s   enable subroutine tracing.
    !     !/t   dump of input parameters in parameter list.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use w3servmd, only: extcde
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: mx, my, lx, hx, ly, hy, nds, ndst,  &
         ndse, idfm, idla
    real, intent(in)        :: vsc, vof
    character, intent(in)   :: rform*(*)
    real, intent(out)       :: array(mx,my)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: iidfm, iidla, ix, iy, istat
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !
    if (idfm.lt.1 .or. idfm.gt.3) then
      iidfm = 1
    else
      iidfm = idfm
    end if
    if (idla.lt.1 .or. idla.gt.4) then
      iidla = 1
    else
      iidla = idla
    end if
    !
    ! free format read :
    !
    if (iidfm.eq.1) then
      if (iidla.eq.1) then
        do iy=ly, hy
          read (nds,*,end=800,err=801,iostat=istat)     &
               (array(ix,iy),ix=lx,hx)
        end do
      else if (iidla.eq.2) then
        read (nds,*,end=800,err=801,iostat=istat)     &
             ((array(ix,iy),ix=lx,hx),iy=ly,hy)
      else if (iidla.eq.3) then
        do iy=hy, ly, -1
          read (nds,*,end=800,err=801,iostat=istat)     &
               (array(ix,iy),ix=lx,hx)
        end do
      else
        read (nds,*,end=800,err=801,iostat=istat)     &
             ((array(ix,iy),ix=lx,hx),iy=hy,ly,-1)
      end if
      !
      ! fixed format read :
      !
    else if (iidfm.eq.2) then
      if (iidla.eq.1) then
        do iy=ly, hy
          read (nds,rform,end=800,err=801,iostat=istat) &
               (array(ix,iy),ix=lx,hx)
        end do
      else if (iidla.eq.2) then
        read (nds,rform,end=800,err=801,iostat=istat) &
             ((array(ix,iy),ix=lx,hx),iy=ly,hy)
      else if (iidla.eq.3) then
        do iy=hy, ly, -1
          read (nds,rform,end=800,err=801,iostat=istat) &
               (array(ix,iy),ix=lx,hx)
        end do
      else
        read (nds,rform,end=800,err=801,iostat=istat) &
             ((array(ix,iy),ix=lx,hx),iy=hy,ly,-1)
      end if
      !
      ! unformat read :
      !
    else
      if (iidla.eq.1) then
        do iy=ly, hy
          read (nds,end=800,err=801,iostat=istat)       &
               (array(ix,iy),ix=lx,hx)
        end do
      else if (iidla.eq.2) then
        read (nds,end=800,err=801,iostat=istat)       &
             ((array(ix,iy),ix=lx,hx),iy=ly,hy)
      else if (iidla.eq.3) then
        do iy=hy, ly, -1
          read (nds,end=800,err=801,iostat=istat)       &
               (array(ix,iy),ix=lx,hx)
        end do
      else
        read (nds,end=800,err=801,iostat=istat)       &
             ((array(ix,iy),ix=lx,hx),iy=hy,ly,-1)
      end if
    end if
    !
    ! scaling :
    !
    do ix=lx, hx
      do iy=ly, hy
        array(ix,iy) = vsc * array(ix,iy) + vof
      end do
    end do
    !
    return
    !
    ! escape locations read errors :
    !
800 continue
    write (ndse,900)
    call extcde ( istat )
    !
801 continue
    write (ndse,901) istat
    call extcde ( istat )
    !
    ! formats
    !
900 format (/' *** error ina2r : '/                           &
         '     premature end of file'/)
901 format (/' *** error ina2r : '/                           &
         '     error in reading from file'/               &
         '     iostat =',i5/)
    !
    !/
    !/ end of ina2r  ----------------------------------------------------- /
    !/
  end subroutine ina2r
  !/ ------------------------------------------------------------------- /
  subroutine ina2i  (array, mx, my, lx, hx, ly, hy,              &
       nds, ndst, ndse, idfm, rform, idla, vsc, vof)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         30-oct-2009 |
    !/                  +-----------------------------------+
    !/                                  based on inar2d by n.booij, dut.
    !/
    !/    31-mar-1993 : final fortran 77                    ( version 1.18 )
    !/    29-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    30-oct-2009 : implement add offset argument.      ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    20-jan-2017 : add error exit using extcde.        ( version 6.02 )
    !/
    !  1. purpose :
    !
    !     like ina2r , integer array, vsc and vof, see ina2r .
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use w3servmd, only: extcde
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: mx, my, lx, hx, ly, hy, nds, ndst,  &
         ndse, idfm, idla, vsc, vof
    integer, intent(out)    :: array(mx,my)
    character, intent(in)   :: rform*(*)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: iidfm, iidla, ix, iy, istat
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !
    if (idfm.lt.1 .or. idfm.gt.3) then
      iidfm = 1
    else
      iidfm = idfm
    end if
    if (idla.lt.1 .or. idla.gt.4)then
      iidla = 1
    else
      iidla = idla
    end if
    !
    ! free format read :
    !
    if (iidfm.eq.1) then
      if (iidla.eq.1) then
        do iy=ly, hy
          read (nds,*,end=800,err=801,iostat=istat)     &
               (array(ix,iy),ix=lx,hx)
        end do
      else if (iidla.eq.2) then
        read (nds,*,end=800,err=801,iostat=istat)     &
             ((array(ix,iy),ix=lx,hx),iy=ly,hy)
      else if (iidla.eq.3) then
        do iy=hy, ly, -1
          read (nds,*,end=800,err=801,iostat=istat)     &
               (array(ix,iy),ix=lx,hx)
        end do
      else
        read (nds,*,end=800,err=801,iostat=istat)     &
             ((array(ix,iy),ix=lx,hx),iy=hy,ly,-1)
      end if
      !
      ! fixed format read :
      !
    else if (iidfm.eq.2) then
      if (iidla.eq.1) then
        do iy=ly, hy
          read (nds,rform,end=800,err=801,iostat=istat) &
               (array(ix,iy),ix=lx,hx)
        end do
      else if (iidla.eq.2) then
        read (nds,rform,end=800,err=801,iostat=istat) &
             ((array(ix,iy),ix=lx,hx),iy=ly,hy)
      else if (iidla.eq.3) then
        do iy=hy, ly, -1
          read (nds,rform,end=800,err=801,iostat=istat) &
               (array(ix,iy),ix=lx,hx)
        end do
      else
        read (nds,rform,end=800,err=801,iostat=istat) &
             ((array(ix,iy),ix=lx,hx),iy=hy,ly,-1)
      end if
      !
      ! unformat read :
      !
    else
      if (iidla.eq.1) then
        do iy=ly, hy
          read (nds,end=800,err=801,iostat=istat)       &
               (array(ix,iy),ix=lx,hx)
        end do
      else if (iidla.eq.2) then
        read (nds,end=800,err=801,iostat=istat)       &
             ((array(ix,iy),ix=lx,hx),iy=ly,hy)
      else if (iidla.eq.3) then
        do iy=hy, ly, -1
          read (nds,end=800,err=801,iostat=istat)       &
               (array(ix,iy),ix=lx,hx)
        end do
      else
        read (nds,end=800,err=801,iostat=istat)       &
             ((array(ix,iy),ix=lx,hx),iy=hy,ly,-1)
      end if
    end if
    !
    ! scaling :
    !
    do ix=lx, hx
      do iy=ly, hy
        array(ix,iy) = vsc * array(ix,iy) + vof
      end do
    end do
    !
    return
    !
    ! escape locations read errors :
    !
800 continue
    write (ndse,900)
    call extcde ( istat )
    !
801 continue
    write (ndse,901) istat
    call extcde ( istat )
    !
    ! formats
    !
900 format (/' *** error ina2i : '/                           &
         '     premature end of file'/)
901 format (/' *** error ina2i : '/                           &
         '     error in reading from file'/               &
         '     iostat =',i5/)
    !
    !/
    !/ end of ina2i  ----------------------------------------------------- /
    !/
  end subroutine ina2i
  !/ ------------------------------------------------------------------- /
  subroutine outa2r (array, mx, my, lx, hx, ly, hy,               &
       nds, ndst, ndse, idfm, rform, idla, vsc, vof)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         30-oct-2009 |
    !/                  +-----------------------------------+
    !/
    !/    31-mar-1993 : final fortran 77                    ( version 1.18 )
    !/    29-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    21-feb-2008 ; bug fix idfm=1, idla=2 writing      ( version 3.13 )
    !/    30-oct-2009 ; fix non-integer loop bound.         ( version 3.14 )
    !/                  (t. j. campbell, nrl)
    !/    30-oct-2009 : implement add offset argument.      ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    20-jan-2017 : add error exit using extcde.        ( version 6.02 )
    !/
    !  1. purpose :
    !
    !     writes 2-d array of pre-described layout and format. "inverse"
    !     version of ina2r . for documentation see ina2r .
    !
    !     n.b. - array_out <= ( array_in - vof ) / vsc
    !          - no error trapping on write.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use w3servmd, only: extcde
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: mx, my, lx, hx, ly, hy, nds, ndst,  &
         ndse, idfm, idla
    real, intent(in)        :: vsc, vof, array(mx,my)
    character, intent(in)   :: rform*(*)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: iidfm, iidla, ix, iy, istat
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !
    if (idfm.lt.1 .or. idfm.gt.3) then
      iidfm = 1
    else
      iidfm = idfm
    end if
    if (idla.lt.1 .or. idla.gt.4) then
      iidla = 1
    else
      iidla = idla
    end if
    !
    ! free format write :
    !
    if (iidfm.eq.1) then
      if (iidla.eq.1) then
        do iy=ly, hy
          write (nds,*,err=800,iostat=istat)              &
               ((array(ix,iy)-vof)/vsc,ix=lx,hx)
        end do
      else if (iidla.eq.2) then
        write (nds,*,err=800,iostat=istat)                &
             (((array(ix,iy)-vof)/vsc,ix=lx,int(hx/vsc)),iy=ly,hy)
      else if (iidla.eq.3) then
        do iy=hy, ly, -1
          write (nds,*,err=800,iostat=istat)              &
               ((array(ix,iy)-vof)/vsc,ix=lx,hx)
        end do
      else
        write (nds,*,err=800,iostat=istat)                &
             (((array(ix,iy)-vof)/vsc,ix=lx,hx),iy=hy,ly,-1)
      end if
      !
      ! fixed format write :
      !
    else if (iidfm.eq.2) then
      if (iidla.eq.1) then
        do iy=ly, hy
          write (nds,rform,err=800,iostat=istat)          &
               ((array(ix,iy)-vof)/vsc,ix=lx,hx)
        end do
      else if (iidla.eq.2) then
        write (nds,rform,err=800,iostat=istat)            &
             (((array(ix,iy)-vof)/vsc,ix=lx,hx),iy=ly,hy)
      else if (iidla.eq.3) then
        do iy=hy, ly, -1
          write (nds,rform,err=800,iostat=istat)          &
               ((array(ix,iy)-vof)/vsc,ix=lx,hx)
        end do
      else
        write (nds,rform,err=800,iostat=istat)            &
             (((array(ix,iy)-vof)/vsc,ix=lx,hx),iy=hy,ly,-1)
      end if
      !
      ! unformat write :
      !
    else
      if (iidla.eq.1) then
        do iy=ly, hy
          write (nds,err=800,iostat=istat)                &
               ((array(ix,iy)-vof)/vsc,ix=lx,hx)
        end do
      else if (iidla.eq.2) then
        write (nds,err=800,iostat=istat)                  &
             (((array(ix,iy)-vof)/vsc,ix=lx,hx),iy=ly,hy)
      else if (iidla.eq.3) then
        do iy=hy, ly, -1
          write (nds,err=800,iostat=istat)                &
               ((array(ix,iy)-vof)/vsc,ix=lx,hx)
        end do
      else
        write (nds,err=800,iostat=istat)                  &
             (((array(ix,iy)-vof)/vsc,ix=lx,hx),iy=hy,ly,-1)
      end if
    end if
    !
    return
    !
    ! escape locations write errors :
    !
800 continue
    write (ndse,900) istat
    call extcde ( istat )
    !
    ! formats
    !
900 format (/' *** error outa2r : '/                          &
         '     error in writing to file'/                 &
         '     iostat =',i5/)
    !
    !/
    !/ end of outa2r ----------------------------------------------------- /
    !/
  end subroutine outa2r
  !/ ------------------------------------------------------------------- /
  subroutine outa2i (array, mx, my, lx, hx, ly, hy,              &
       nds, ndst, ndse, idfm, rform, idla, vsc, vof)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         30-oct-2009 |
    !/                  +-----------------------------------+
    !/
    !/    31-mar-1993 : final fortran 77                    ( version 1.18 )
    !/    29-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    30-oct-2009 : implement add offset argument.      ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    20-jan-2017 : add error exit using extcde.        ( version 6.02 )
    !/
    !  1. purpose :
    !
    !     like outa2r, integer array, vsc and vof, see outa2r.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use w3servmd, only: extcde
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: mx, my, lx, hx, ly, hy, nds, ndst,  &
         ndse, idfm, idla, array(mx,my)
    integer, intent(in)     :: vsc, vof
    character, intent(in)   :: rform*(*)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: iidfm, iidla, ix, iy, istat
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !
    if (idfm.lt.1 .or. idfm.gt.3) then
      iidfm = 1
    else
      iidfm = idfm
    end if
    if (idla.lt.1 .or. idla.gt.4) then
      iidla = 1
    else
      iidla = idla
    end if
    !
    ! free format write :
    !
    if (iidfm.eq.1) then
      if (iidla.eq.1) then
        do iy=ly, hy
          write (nds,*,err=800,iostat=istat)              &
               ((array(ix,iy)-vof)/vsc,ix=lx,hx)
        end do
      else if (iidla.eq.2) then
        write (nds,*,err=800,iostat=istat)                &
             (((array(ix,iy)-vof)/vsc,ix=lx,hx),iy=ly,hy)
      else if (iidla.eq.3) then
        do iy=hy, ly, -1
          write (nds,*,err=800,iostat=istat)              &
               ((array(ix,iy)-vof)/vsc,ix=lx,hx)
        end do
      else
        write (nds,*,err=800,iostat=istat)                &
             (((array(ix,iy)-vof)/vsc,ix=lx,hx),iy=hy,ly,-1)
      end if
      !
      ! fixed format write :
      !
    else if (iidfm.eq.2) then
      if (iidla.eq.1) then
        do iy=ly, hy
          write (nds,rform,err=800,iostat=istat)          &
               ((array(ix,iy)-vof)/vsc,ix=lx,hx)
        end do
      else if (iidla.eq.2) then
        write (nds,rform,err=800,iostat=istat)            &
             (((array(ix,iy)-vof)/vsc,ix=lx,hx),iy=ly,hy)
      else if (iidla.eq.3) then
        do iy=hy, ly, -1
          write (nds,rform,err=800,iostat=istat)          &
               ((array(ix,iy)-vof)/vsc,ix=lx,hx)
        end do
      else
        write (nds,rform,err=800,iostat=istat)            &
             (((array(ix,iy)-vof)/vsc,ix=lx,hx),iy=hy,ly,-1)
      end if
      !
      ! unformat write :
      !
    else
      if (iidla.eq.1) then
        do iy=ly, hy
          write (nds,err=800,iostat=istat)                &
               ((array(ix,iy)-vof)/vsc,ix=lx,hx)
        end do
      else if (iidla.eq.2) then
        write (nds,err=800,iostat=istat)                  &
             (((array(ix,iy)-vof)/vsc,ix=lx,hx),iy=ly,hy)
      else if (iidla.eq.3) then
        do iy=hy, ly, -1
          write (nds,err=800,iostat=istat)                &
               ((array(ix,iy)-vof)/vsc,ix=lx,hx)
        end do
      else
        write (nds,err=800,iostat=istat)                  &
             (((array(ix,iy)-vof)/vsc,ix=lx,hx),iy=hy,ly,-1)
      end if
    end if
    !
    return
    !
    ! escape locations write errors :
    !
800 continue
    write (ndse,900) istat
    call extcde ( istat )
    !
    ! formats
    !
900 format (/' *** error outa2i : '/                          &
         '     error in writing to file'/                 &
         '     iostat =',i5/)
    !
    !/
    !/ end of outa2i ----------------------------------------------------- /
    !/
  end subroutine outa2i
  !/ ------------------------------------------------------------------- /
  subroutine outrea (nds,array,dim,aname)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         29-nov-1999 |
    !/                  +-----------------------------------+
    !/                        original versions g. ph. van vledder
    !/                                          p. h. willems
    !/
    !/    29-mar-1993 : final fortran 77                    ( version 1.18 )
    !/    29-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/
    !  1. purpose :
    !
    !     print contents of a 1-d real array, see outint.
    !
    !/ ------------------------------------------------------------------- /
    !/
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: nds, dim
    real, intent(in)        :: array(dim)
    character, intent(in)   :: aname*(*)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: i, k
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    write (nds,8000) aname
    !
    if (icol.eq.80) then
      !
      write (nds,8005) (i, i=1, 5)
      write (nds,8010)
      do k=0, dim, 5
        if (dim-k.ge.5) then
          write (nds,'(1x,i4,a,5e12.4,a)')                  &
               k,'  |',(array(i),i= k+1, k+5),'  |'
        else
          write (nds,'(1x,t71,''|'',t2,i4,a,5e12.4)')       &
               k,'  |',(array(i),i= k+1, dim)
        end if
      end do
      write (nds,8010)
      !
    else
      !
      write (nds,9005) (i, i=1, 10)
      write (nds,9010)
      do k=0, dim, 10
        if (dim-k.ge.10) then
          write (nds,'(1x,i4,a,10e12.4,a)')                 &
               k,'  |',(array(i),i= k+1, k+10),'  |'
        else
          write (nds,'(1x,t131,''|'',t2,i4,a,10e12.4)')     &
               k,'  |',(array(i),i= k+1, dim)
        end if
      end do
      write (nds,9010)
    end if
    !
    return
    !
8000 format (/,1x,'a r r a y   d u m p  (real) / name: ',a)
8005 format (8x,5i12)
8010 format (7x,'+',62('-'),'+')
9005 format (8x,10i12)
9010 format (7x,'+',122('-'),'+')
    !/
    !/ end of outrea ----------------------------------------------------- /
    !/
  end subroutine outrea
  !/ ------------------------------------------------------------------- /
  subroutine outint ( nds, iarray, dim, aname )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         29-mar-1993 |
    !/                  +-----------------------------------+
    !/                        original versions g. ph. van vledder
    !/                                          p. h. willems
    !/
    !/    29-mar-1993 : final fortran 77                    ( version 1.18 )
    !/    29-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/
    !  1. purpose :
    !
    !     print contents of a 1-d integer array.
    !
    !  2. method :
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       nds     int.   i   output unit number.
    !       iarray  i.a.   i   array to be printed.
    !       dim     int.   i   number of elements to be printed.
    !       aname   c*(*)  i   name of array.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     see mudule documentation.
    !
    !  5. called by :
    !
    !       anny routine or program.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: nds, dim, iarray(dim)
    character, intent(in)   :: aname*(*)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: i, k
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    write (nds,8000) aname
    !
    !  ------- 80 columns -----
    !
    if (icol.eq.80) then
      write (nds,8005) (i, i=1, 5)
      write (nds,8010)
      do k=0, dim, 5
        if (dim-k.ge.5) then
          write (nds,'(1x,i4,a,5i12,a)')                    &
               k,'  |',(iarray(i),i= k+1, k+5),'  |'
        else
          write (nds,'(1x,t71,''|'',t2,i4,a,5i12)')         &
               k,'  |',(iarray(i),i= k+1, dim)
        end if
      end do
      write (nds,8010)
    else
      !
      !    ---- 132 columns ----
      !
      write (nds,9005) (i, i=1, 10)
      write (nds,9010)
      do k=0, dim, 10
        if (dim-k.ge.10) then
          write (nds,'(1x,i4,a,10i12,a)')                   &
               k,'  |',(iarray(i),i= k+1, k+10),'  |'
        else
          write (nds,'(1x,t131,''|'',t2,i4,a,10i12)')       &
               k,'  |',(iarray(i),i= k+1, dim)
        end if
      end do
      write (nds,9010)
    end if
    !
    return
    !
8000 format (/,1x,'a r r a y   d u m p  (integer) / name: ',a)
8005 format (8x,5i12)
8010 format (7x,'+',62('-'),'+')
9005 format (8x,10i12)
9010 format (7x,'+',122('-'),'+')
    !/
    !/ end of outint ----------------------------------------------------- /
    !/
  end subroutine outint
  !/ ------------------------------------------------------------------- /
  subroutine outmat (nds,a,mx,nx,ny,mname)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         29-nov-1999 |
    !/                  +-----------------------------------+
    !/                        original versions g. ph. van vledder
    !/
    !/    29-mar-1993 : final fortran 77                    ( version 1.18 )
    !/    29-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/
    !  1. purpose :
    !
    !     print contents of a 2-d real array.
    !
    !  2. method :
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       nds     int.   i   output unit number.
    !       a       r.a.   i   matrix to be printed.
    !       mx      int.   i   dimension of first index.
    !       nx      int.   i   number of points for first index.
    !       ny      int.   i   number of points for scond index.
    !       mname   c*(*)  i   name of matrix.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     see mudule documentation.
    !
    !  5. called by :
    !
    !       anny routine or program.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: nds, mx, nx, ny
    real, intent(in)        :: a(mx,ny)
    character, intent(in)   :: mname*(*)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: lblok, nblok, iblok, ix, ix1, ix2, iy
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    write(nds,8000) mname
    !
    !  ------ 80 columns -----
    !
    if(icol.eq.80) then
      lblok = 6
      nblok = (nx-1)/lblok + 1
      do iblok = 1,nblok
        ix1 = (iblok-1)*lblok + 1
        ix2 = ix1 + lblok - 1
        if(ix2.gt.nx) ix2 = nx
        write(nds,8001) (ix,ix = ix1,ix2)
        write(nds,8002)
        do iy = 1,ny
          write(nds,8003) iy,(a(ix,iy),ix = ix1,ix2)
        end do
        write(nds,8002)
      end do
    else
      !
      !   ---- 132 columns ----
      !
      lblok = 12
      nblok = (nx-1)/lblok + 1
      do iblok = 1,nblok
        ix1 = (iblok-1)*lblok + 1
        ix2 = ix1 + lblok - 1
        if(ix2.gt.nx) ix2 = nx
        write(nds,9001) (ix,ix = ix1,ix2)
        write(nds,9002)
        do iy = 1,ny
          write(nds,9003) iy,(a(ix,iy),ix = ix1,ix2)
        end do
        write(nds,9002)
      end do
    end if
    !
    return
    !
    ! formats
    !
8000 format(/,1x,' m a t r i x   d u m p  (real) / name: ',a)
8001 format(9x,6i10)
8002 format(1x,6x,'+',62('-'),'+')
8003 format(1x,t71,'|',t2,i5,' | ',12e10.3)
9001 format(9x,12i10)
9002 format(1x,6x,'+',122('-'),'+')
9003 format(1x,t131,'|',t2,i5,' | ',12e10.3)
    !/
    !/ end of outmat ----------------------------------------------------- /
    !/
  end subroutine outmat
  !/ ------------------------------------------------------------------- /
  subroutine prtblk (nds, nx, ny, mx, f, map, map0, fsc,    &
       ix1, ix2, ix3, iy1, iy2, iy3, prvar, prunit)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         29-nov-1999 |
    !/                  +-----------------------------------+
    !/
    !/    04-jun-1996 : final fortran 77                    ( version 1.18 )
    !/    29-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/
    !  1. purpose :
    !
    !     print a block-type table of a two-dimensional field using a
    !     land-sea array.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       nds     int.   i   file unit number.
    !       nx, ny  int.   i   x and y range of arrays.
    !       my      int.   i   actual x size of arrays.
    !       f       r.a.   i   array to pr presented.
    !       map     i.a.   i   map array for land points.
    !       map0    int.   i   map value for land points in map.
    !       fsc     real   i   scaling factor.
    !       ix1-3   int.   i   firts, last, increment grid points in x
    !                          direction.
    !       iy1-3   int.   i   id. y direction.
    !       prvar   c*(*)  i   name of variable.
    !       prunit  c*(*)  i   units of spectrum.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     see mudule documentation.
    !
    !  5. called by :
    !
    !     any program.
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     ------------------------------------------------
    !       check if automatic scaling
    !       if automatic scaling : get extermata
    !       print heading
    !       print table
    !       print ending
    !     ------------------------------------------------
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing using strace.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: nds, nx, ny, mx, map(mx,ny), map0,  &
         ix1, ix2, ix3, iy1, iy2, iy3
    real, intent(in)        :: f(mx,ny), fsc
    character, intent(in)   :: prvar*(*), prunit*(*)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ix, iy, jj, jm, k1, lx, i
    real                    :: fmax, rr
    logical                 :: flscle
    character               :: pnum*5, stra*5, pnum2*2, stra3*3
    dimension               :: pnum(25), pnum2(61)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! check scaling
    !
    flscle = (fsc.le.0.)
    !
    ! extremata
    !
    if (flscle) then
      fmax   = 1.e-15
      do ix=1, nx
        do iy=1, ny
          if ( map(ix,iy) .ne. map0 )                       &
               fmax   = max ( fmax , abs(f(ix,iy)) )
        end do
      end do
    end if
    !
    ! normalized print plot -----------------------------------------------
    !
    if (flscle) then
      !
      ! heading
      !
      write (nds,901) prvar, fmax, prunit
      !
      stra   = '     '
      jj     = 0
      do ix = ix1, ix2, ix3
        jj = jj + 1
      end do
      lx = jj
      write (nds,911)
      write (nds,912) (ix,ix=ix1,ix2,2*ix3)
      pnum2(1) = '--'
      write (nds,910) stra, ' +', (pnum2(1), i=1, lx), '-+'
      !
      ! write table
      !
      jm = 0
      do iy = iy2, iy1, iy3*(-1)
        !
        jj = 0
        do ix = ix1, ix2, ix3
          jj = jj + 1
          if (map(ix,iy).eq.map0) then
            pnum2(jj) = '  '
          else
            rr = 10.*f(ix,iy)/fmax
            write (stra, fmt='(i2,3x)') int(rr*1.000001)
            pnum2(jj) = stra(1:2)
            if (pnum2(jj).eq.'10' .or. pnum2(jj).eq.'**' .or. &
                 f(ix,iy).eq.fmax) then
              if ( rr .lt. 0. ) then
                pnum2(jj) = '-*'
              else
                pnum2(jj) = ' *'
              end if
            end if
          end if
        end do
        !
        if (jm.eq.0) then
          write (stra, fmt='(i5)') iy
          jm   = 2
        else
          stra = '     '
          jm   = jm-1
        end if
        !
        lx = jj
        write (nds,910) stra, ' |', (pnum2(i), i=1, lx), ' |'
      end do
      !
      stra     = '     '
      pnum2(1) = '--'
      write (nds,910) stra, ' +', (pnum2(1), i=1, lx), '-+'
      write (nds,912) (ix,ix=ix1,ix2,2*ix3)
      write (nds,911)
      !
      ! non-normalized print plot -------------------------------------------
      !
    else
      !
      ! heading
      !
      write (nds,900) prvar, fsc, prunit
      !
      jj = 0
      pnum(1) = '     '
      do ix = ix1, ix2, ix3
        jj = jj + 1
      end do
      lx = jj
      write (nds,921)
      write (nds,922) (ix,ix=ix1,ix2,ix3)
      stra3   = '   '
      pnum(1) = '-----'
      write (nds,920) stra3, ' +', (pnum(1), i=1, lx), '-+   '
      !
      ! write table
      !
      jm = 0
      do iy = iy2, iy1, iy3*(-1)
        if (jm.eq.0) then
          write (stra3, fmt='(i3)') iy
          jm = 2
        else
          stra3  = '   '
          jm = jm-1
        end if
        !
        jj = 0
        do ix = ix1, ix2, ix3
          jj = jj + 1
          if (map(ix,iy).eq.map0) then
            pnum(jj) = '     '
          else
            rr     = f(ix,iy)
            k1 = nint (rr / fsc)
            write (stra, fmt='(i5)') k1
            pnum(jj) = stra
          end if
        end do
        !
        lx = jj
        write (nds,920) stra3, ' |', (pnum(i), i=1, lx), ' |   '
      end do
      !
      stra3   = '   '
      pnum(1) = '-----'
      write (nds,920) stra3, ' +', (pnum(1), i=1, lx), '-+   '
      write (nds,922) (ix,ix=ix1,ix2,ix3)
      write (nds,921)
      !
    end if
    !
    return
    !
    ! formats
    !
900 format (/, ' variable: ',a,' units: ',e10.3,1x,a)
901 format (/, ' variable: ',a,' max.: ',e10.3,1x,a)
    !
910 format (1x,a5,63a2)
911 format (' ')
912 format (6x,32i8)
    !
920 format (1x,a3,a2,25a5)
921 format (' ')
922 format (6x,25i5)
    !/
    !/ end of prtblk ----------------------------------------------------- /
    !/
  end subroutine prtblk
  !/ ------------------------------------------------------------------- /
  subroutine prt1ds (nds, nfr, e, fr, ufr, nlines, ftopi,    &
       prvar, prunit, pntnme)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         29-nov-1999 |
    !/                  +-----------------------------------+
    !/
    !/    10-mar-1992 : final fortran 77                    ( version 1.18 )
    !/    29-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/
    !  1. purpose :
    !
    !     produces a print plot of a 1-d spectrum.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       nds     int.   i   file unit number.
    !       nfr     int.   i   number of frequencies.
    !       e       r.a.   i   spectral densities.
    !       fr      r.a.   i   frequencies.
    !       ufr     c*(*)  i   if 'hz', frequencies in hz, otherwise in
    !                          rad/s (n.b., does not re-scale spectrum).
    !       nlines  int.   i   hight of plot in lines.
    !       ftopi   real   i   highest value of density in plot,
    !                          if ftopi.le.0., automatic scaling.
    !       prvar   c*(*)  i   name of variable.
    !       prunit  c*(*)  i   units of spectrum.
    !       pntnme  c*(*)  i   name of location.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     see mudule documentation.
    !
    !  5. called by :
    !
    !       any routine.
    !
    !  6. error messages :
    !
    !       none.
    !
    !  7. remarks :
    !
    !     - paperwidth is "set" by nfrmax.
    !
    !  8. structure :
    !
    !     ------------------------------------------------
    !       initializations and preparations.
    !       determine maximum of spectra.
    !       scaling / normalization.
    !       printing of spectrum
    !       ----------------------------------------------
    !         print id
    !         print heading
    !         print table
    !         print ending
    !     ------------------------------------------------
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing using strace.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: nds, nfr, nlines
    real, intent(in)        :: ftopi, e(nfr), fr(nfr)
    character, intent(in)   :: prvar*(*), prunit*(*), pntnme*(*),  &
         ufr*(*)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: nfrb, ifr, il, il0
    real, save              :: topfac = 1.1
    real                    :: ftop, rlines, facfr, fsc, fline,    &
         emax, emin, extr, floc
    logical                 :: flscle
    character               :: stra*10, stra2*2, pnum2*2
    dimension               :: pnum2(nfm2)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ftop   = ftopi
    !
    nfrb   = min (nfr,50)
    rlines = real(nlines)
    flscle = ftop.le.0.
    !
    if (ufr.eq.'hz') then
      facfr  = 1.
    else
      facfr  = 0.159155
    end if
    !
    ! maximum of 1-d spectrum
    !
    emax   = 0.
    emin   = 0.
    !
    do ifr=1, nfr
      emax = max ( emax , e(ifr) )
      emin = min ( emin , e(ifr) )
    end do
    !
    if (emax.eq.0. .and. emin.eq.0.) then
      emax   =  1.e-20
      emin   = -1.e-20
    end if
    !
    if (emax.gt.abs(emin)) then
      extr   = emax
    else
      extr   = emin
    end if
    !
    ! scaling / normalization
    !
    if (flscle) then
      if (emax.gt.abs(emin)) then
        floc   = emax * topfac
        fsc    = floc / real(nint(emax/(emax-emin)*rlines))
      else
        floc   = emin * topfac
        fsc    = floc / real(nint(emin/(emax-emin)*rlines))
        floc   = ftop + rlines*fsc
        if (emax.lt.0.01*fsc) ftop = 0.
      end if
    else
      floc   = ftop
      fsc    = floc  / rlines
      if (emax*emin.lt.0) fsc = 2.*fsc
      if (emax.lt.0.01*fsc) floc = 0.
    end if
    !
    il0   = mod ( nint(floc/fsc) , 2 ) + 1
    !
    ! print id
    !
    write (nds,900) pntnme, prvar, extr, prunit
    !
    ! print heading
    !
    fline  = floc
    if (mod(nlines+il0,2).eq.0) then
      write (stra, fmt='(e10.3)') fline
    else
      stra=  '          '
    end if
    !
    do ifr=1, nfrb
      if ( nint( (e(ifr)-fline)/fsc ) .eq.0) then
        pnum2(ifr) = '-*'
      else
        pnum2(ifr) = '--'
      end if
    end do
    !
    pnum2(nfrb+1) = '-+'
    stra2 = ' +'
    write (nds,910) stra, stra2, (pnum2(ifr),ifr=1, nfrb+1)
    !
    ! print table
    !
    do il = 1, nlines-1
      fline  = floc - fsc * real(il)
      if (abs(fline).lt.0.01*fsc) fline = 0.
      if (mod(nlines+il0-il,2).eq.0) then
        write (stra, fmt='(e10.3)') fline
        stra2 =  ' +'
      else
        stra  =  '          '
        stra2 =  ' |'
      end if
      do ifr=1, nfrb
        if (abs(fline).lt.0.1*fsc) then
          pnum2(nfrb+1) = '-|'
          if ( nint( (e(ifr)-fline)/fsc ) .eq.0) then
            pnum2(ifr) = '-*'
          else
            pnum2(ifr) = '--'
          end if
        else
          pnum2(nfrb+1) = ' |'
          if ( nint( (e(ifr)-fline)/fsc ) .eq.0) then
            pnum2(ifr) = ' *'
          else
            pnum2(ifr) = '  '
          end if
        end if
      end do
      write (nds,910) stra, stra2, (pnum2(ifr),ifr=1, nfrb+1)
    end do
    !
    ! write ending
    !
    fline  = floc - fsc * real(il)
    if (abs(fline).lt.0.01*fsc) fline = 0.
    write (stra, fmt='(e10.3)') fline
    if (mod(il0,2).eq.0) then
      write (stra, fmt='(e10.3)') fline
    else
      stra  =  '          '
    end if
    stra2         = ' +'
    pnum2(nfrb+1) = '-+'
    !
    do ifr=1, nfrb
      if ( nint( (e(ifr)-fline)/fsc ) .eq.0) then
        pnum2(ifr) = '-*'
      else if ( mod (ifr-2,4) .eq. 0 ) then
        pnum2(ifr) = '-|'
      else
        pnum2(ifr) = '--'
      end if
    end do
    !
    write (nds,910) stra, stra2, (pnum2(ifr),ifr=1, nfrb+1)
    write (nds,911) (fr(ifr)*facfr,ifr=2,nfrb,4)
    write (nds,920)
    !
    return
    !
    ! formats
    !
900 format (/'  location : ',a                                &
         /'  spectrum : ',a,'  extreme value : ',e10.3,1x,a/)
    !
910 format (a10,a2,60a2)
911 format (10x,15f8.3)
    !
920 format (' ')
    !/
    !/ end of prt1ds ----------------------------------------------------- /
    !/
  end subroutine prt1ds
  !/ ------------------------------------------------------------------- /
  subroutine prt1dm (nds, nfr, ne, e, fr, ufr, nlines, ftopi, &
       prvar, prunit, pntnme)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         17-apr-1992 |
    !/                  +-----------------------------------+
    !/
    !/    17-apr-1992 : final fortran 77                    ( version 1.18 )
    !/    29-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/
    !  1. purpose :
    !
    !     produces a print plot of several 1-d spectra.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       nds     int.   i   file unit number.
    !       nfr     int.   i   number of frequencies.
    !       ne      int.   i   number of spectra.
    !       e       r.a.   i   spectral densities.
    !       fr      r.a.   i   frequencies.
    !       ufr     c*     i   if 'hz', frequencies in hz, otherwise in
    !                          rad/s
    !       nlines  int.   i   hight of plot in lines.
    !       ftopi   real   i   highest value of density in plot,
    !                          if ftop.le.0., automatic scaling.
    !       prvar   c*(*)  i   name of variable.
    !       prunit  c*(*)  i   units of spectrum.
    !       pntnme  c*(*)  i   name of location.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     see mudule documentation.
    !
    !  5. called by :
    !
    !       any routine.
    !
    !  6. error messages :
    !
    !       none.
    !
    !  7. remarks :
    !
    !     - paperwidth is "set" by nfrmax.
    !
    !  8. structure :
    !
    !     ------------------------------------------------
    !       initializations and preparations.
    !       determine maximum of spectrum.
    !       scaling / normalization.
    !       printing of spectrum
    !       ----------------------------------------------
    !         print id
    !         print heading
    !         print table
    !         print ending
    !     ------------------------------------------------
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing using strace.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: nds, nfr, ne, nlines
    real, intent(in)        :: ftopi, e(nfr,ne), fr(nfr)
    character, intent(in)   :: prvar*(*), prunit*(*), pntnme*(*),  &
         ufr*(*)
    dimension               :: prvar(ne)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer, parameter      :: nfrmax = 100
    integer, parameter      :: nfm2   = nfrmax+1
    integer                 :: nfrb, ifr, ie, il
    real, save              :: topfac = 1.1
    real                    :: ftop, rlines, facfr, fsc, fline,    &
         emax, emin, extr, floc
    logical                 :: flscle
    character               :: stra*10, stra2*2, strax*2, pnum2*2
    dimension               :: pnum2(nfm2)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! test output, echo input
    !
    !
    ftop   = ftopi
    nfrb   = min (nfr,50)
    rlines = real(nlines)
    flscle = ftop.le.0.
    !
    if (ufr.eq.'hz') then
      facfr  = 1.
    else
      facfr  = 0.159155
    end if
    !
    ! maximum of 1-d spectrum
    !
    emax   = 0.
    emin   = 0.
    !
    do ie=1, ne
      do ifr=1, nfr
        emax = max ( emax , e(ifr,ie) )
        emin = min ( emin , e(ifr,ie) )
      end do
    end do
    !
    if (emax.eq.0. .and. emin.eq.0.) then
      emax   =  1.e-20
      emin   = -1.e-20
    end if
    !
    if (emax.gt.abs(emin)) then
      extr   = emax
    else
      extr   = emin
    end if
    !
    ! scaling / normalization
    !
    if (flscle) then
      if (emax.gt.abs(emin)) then
        ftop   = emax * topfac
        fsc    = ftop / real(nint(emax/(emax-emin)*rlines))
      else
        ftop   = emin * topfac
        fsc    = ftop / real(nint(emin/(emax-emin)*rlines))
        ftop   = ftop + rlines*fsc
        if (abs(ftop).lt.0.01*fsc) ftop = 0.
      end if
    else
      fsc    = ftop  / rlines
      if (emax*emin.lt.0) fsc = 2.*fsc
      if (emax.eq.0.) ftop = 0.
    end if
    !
    ! print id
    !
    write (nds,900) pntnme, extr, prunit
    !
    ! print heading
    !
    fline  = ftop
    if (mod(nlines,2).eq.0) then
      write (stra, fmt='(e10.3)') fline
    else
      stra=  '          '
    end if
    !
    do ifr=1, nfrb
      pnum2(ifr) = '--'
      do ie=1, ne
        if ( nint( (e(ifr,ie)-fline)/fsc ) .eq.0) then
          if (ie.lt.10) then
            write (strax,'(a1,i1)') '-', ie
          else
            write (strax,'(i2)') ie
          end if
          pnum2(ifr) = strax
        end if
      end do
    end do
    !
    pnum2(nfrb+1) = '-+'
    stra2 = ' +'
    write (nds,910) stra, stra2, (pnum2(ifr),ifr=1, nfrb+1)
    !
    ! print table
    !
    pnum2(nfrb+1) = ' |'
    !
    do il = 1, nlines-1
      fline  = ftop - fsc * real(il)
      if (abs(fline).lt.0.01*fsc) fline = 0.
      if (mod(nlines-il,2).eq.0) then
        write (stra, fmt='(e10.3)') fline
        stra2 =  ' +'
      else
        stra  =  '          '
        stra2 =  ' |'
      end if
      do ifr=1, nfrb
        pnum2(nfrb+1) = ' |'
        if (abs(fline).lt.0.1*fsc) then
          pnum2(ifr) = '--'
          pnum2(nfrb+1) = '-+'
          do ie=1, ne
            if ( nint( (e(ifr,ie)-fline)/fsc ) .eq.0) then
              if (ie.lt.10) then
                write (strax,'(a1,i1)') '-', ie
              else
                write (strax,'(i2)') ie
              end if
              pnum2(ifr) = strax
            end if
          end do
        else
          pnum2(ifr) = '  '
          do ie=1, ne
            if ( nint( (e(ifr,ie)-fline)/fsc ) .eq.0) then
              write (strax,'(i2)') ie
              pnum2(ifr) = strax
            end if
          end do
        end if
      end do
      write (nds,910) stra, stra2, (pnum2(ifr),ifr=1, nfrb+1)
    end do
    !
    ! write ending
    !
    fline  = ftop - fsc * real(il)
    if (abs(fline).lt.0.01*fsc) fline = 0.
    write (stra, fmt='(e10.3)') fline
    stra2         = ' +'
    pnum2(nfrb+1) = '-+'
    !
    do ifr=1, nfrb
      if ( mod (ifr-2,4) .eq. 0 ) then
        pnum2(ifr) = '-|'
      else
        pnum2(ifr) = '--'
      end if
      do ie=1, ne
        if ( nint( (e(ifr,ie)-fline)/fsc ) .eq.0) then
          if (ie.lt.10) then
            write (strax,'(a1,i1)') '-', ie
          else
            write (strax,'(i2)') ie
          end if
          pnum2(ifr) = strax
        end if
      end do
    end do
    !
    write (nds,910) stra, stra2, (pnum2(ifr),ifr=1, nfrb+1)
    write (nds,911) (fr(ifr)*facfr,ifr=2,nfrb,4)
    write (nds,920)
    write (nds,921) (prvar(ie),ie=1,ne)
    write (nds,920)
    if (flscle) ftop = 0.
    !
    return
    !
    ! formats
    !
900 format (/'  location : ',a                                &
         /'  extreme value : ',e10.3,1x,a/)
    !
910 format (a10,a2,60a2)
911 format (10x,15f8.3)
    !
920 format (' ')
921 format (10x,'spectra : ',10(a,'  ')/)
    !/
    !/ end of prt1dm ----------------------------------------------------- /
    !/
  end subroutine prt1dm
  !/ ------------------------------------------------------------------- /
  subroutine prt2ds (nds, nfr0, nfr, nth, e, fr, ufr, facsp, fsc, &
       rrcut, prvar, prunit, pntnme)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         29-nov-1999 |
    !/                  +-----------------------------------+
    !/
    !/    07-jun-1996 : final fortran 77                    ( version 1.18 )
    !/    29-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/
    !  1. purpose :
    !
    !     prints a block type table of a 2-d spectrum. input considers
    !     cartesian directions, output according to meteorological
    !     conventions (compass direction where waves come from).
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       nds     int.   i   file unit number.
    !       nfr0    int.   i   array size for freq.
    !       nfr     int.   i   number of frequencies.
    !       nth     int.   i   number of frequencies.
    !       e       r.a.   i   spectral densities.
    !       fr      r.a.   i   frequencies.
    !       ufr     c*(*)  i   if 'hz', frequencies in hz, otherwise in
    !                          rad/s
    !       facsp   real   i   conversion factor to obtain (hz,degr)
    !                          spectrum from e
    !       fsc     real   i   scale factor, if fsc.eq.0. automatic
    !                          scaling for "compressed" block.
    !       rrcut   real   i   relative cut-off for printing.
    !       prvar   c*(*)  i   name of variable.
    !       prunit  c*(*)  i   units of spectrum.
    !       pntnme  c*(*)  i   name of location.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !       angstr (internal)
    !
    !  5. called by :
    !
    !       any program.
    !
    !  6. error messages :
    !
    !       none.
    !
    !  7. remarks :
    !
    !       pnum2: dimensioning changed from 51 to 71 due to "subscript out
    !           of range" fault (sep 28 2012)
    !
    !  8. structure :
    !
    !     ------------------------------------------------
    !       initializations and preparations.
    !       determine maximum of spectrum.
    !       scaling / normalization.
    !       do for normalized or non-norm. spectrum
    !       ----------------------------------------------
    !         print id
    !         print heading
    !         print table
    !         print ending
    !     ------------------------------------------------
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing using strace.
    !     !/t  diagnostic test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: nds, nfr0, nfr, nth
    real, intent(in)        :: e(nfr0,*), fr(*), facsp, fsc, rrcut
    character, intent(in)   :: prvar*(*), prunit*(*), pntnme*(*),  &
         ufr*(*)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ifr, ith, nfrb, intang, ithsec
    logical                 :: flscle
    real                    :: facfr, emax, emin, dthdeg, rr, rrc
    character               :: pnum*5, stra*5, strang*5, pnum2*2,  &
         stra2*2
    dimension               :: pnum(25), pnum2(101)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !
    ! initialisations
    !
    flscle = .false.
    if (fsc.eq.0.) then
      flscle = .true.
      rrc    = rrcut * 10.
    end if
    !
    if (ufr.eq.'hz') then
      facfr  = 1.
    else
      facfr  = 0.159155
    end if
    !
    ! maximum of spectrum
    !
    emax   = 1.e-20
    emin   = 0.
    !
    do ifr=1, nfr
      do ith=1, nth
        emax = max ( emax , e(ifr,ith) )
        emin = min ( emin , e(ifr,ith) )
      end do
    end do
    !
    emax = max (emax, abs(emin) )
    !
    dthdeg = 360. / real(nth)
    !
    ! normalized spectra :  = = = = = = = = = = = = = = = = = = = = = =
    !
    if (flscle) then
      !
      ! write id
      !
      write (nds,900) pntnme, prvar, emax*facsp, prunit
      !
      ! write head
      !
      nfrb  = min (nfr,50)
      write (nds,910) (fr(ifr)*facfr,ifr=2,nfrb,4)
      !
      do ifr=1, nfr
        if ( mod((ifr-2),4) .eq. 0) then
          pnum2(ifr) = '-|'
        else
          pnum2(ifr) = '--'
        end if
      end do
      !
      pnum2(nfrb+1) = '-+'
      write (nds,920) (pnum2(ifr),ifr=1, nfrb+1)
      !
      ! write table
      !
      ithsec = nth + 1
      !
      do ith= nth, 1, -1
        intang = 270 - nint (dthdeg*real(ith-1))
        if (intang.lt.0) then
          ithsec = ith
          cycle
        end if
        call angstr (intang, strang, 4, 2)
        do ifr=1, nfrb
          rr     = e(ifr,ith)/emax
          if (e(ifr,ith).eq.emax .or. rr.ge.1.) then
            pnum2(ifr) = ' *'
          else if (-e(ifr,ith).eq.emax .or. rr.le.-1.) then
            pnum2(ifr) = ' #'
          else if (abs(rr).lt.rrc) then
            pnum2(ifr) = '  '
          else if ((rr*10.).lt.0. .and. (rr*10.).gt.-1.) then
            pnum2(ifr) = '-0'
          else
            write (stra2, fmt='(i2)') int (rr*10.)
            pnum2(ifr) = stra2
          end if
        end do
        pnum2(nfrb+1) = ' |'
        write (nds,930) strang, (pnum2(ifr),ifr=1, nfrb+1)
      end do
      !
      do ith= nth, ithsec, -1
        intang = 630 - nint (dthdeg*real(ith-1))
        call angstr (intang, strang, 4, 2)
        do ifr=1, nfrb
          rr     = e(ifr,ith)/emax
          if (e(ifr,ith).eq.emax .or. rr.ge.1.) then
            pnum2(ifr) = ' *'
          else if (-e(ifr,ith).eq.emax .or. rr.le.-1.) then
            pnum2(ifr) = ' #'
          else if (abs(rr).lt.rrc) then
            pnum2(ifr) = '  '
          else if ((rr*10.).lt.0. .and. (rr*10.).gt.-1.) then
            pnum2(ifr) = '-0'
          else
            write (stra2, fmt='(i2)') int (rr*10.)
            pnum2(ifr) = stra2
          end if
        end do
        pnum2(nfrb+1) = ' |'
        write (nds,930) strang, (pnum2(ifr),ifr=1, nfrb+1)
      end do
      !
      ! write ending:
      !
      pnum2(1) = '--'
      pnum2(2) = '-+'
      write (nds,920) (pnum2(1),ifr=1, nfrb), pnum2(2)
      write (nds,950)
      !
      ! scaled spectra :  = = = = = = = = = = = = = = = = = = = = = = = =
      !
    else
      !
      ! write id
      !
      write (nds,901) pntnme, prvar, fsc, prunit,           &
           emax*facsp, prunit
      !
      ! write heading
      !
      nfrb  = min (nfr,25)
      !
      write (nds,911) (fr(ifr)*facfr,ifr=2,nfrb,2)
      pnum(1) = '-----'
      pnum(2) = '--   '
      !
      if (nfrb.lt.25) then
        write (nds,921) (pnum(1),ifr=1, nfrb), pnum(2)
      else
        write (nds,921) (pnum(1),ifr=1, nfrb)
      end if
      !
      !     write table :
      !
      ithsec = nth + 1
      !
      do ith= nth, 1, -1
        intang = 270 - nint (dthdeg*real(ith-1))
        if (intang.lt.0) then
          ithsec = ith
          cycle
        end if
        call angstr (intang, strang, 4, 2)
        do ifr=1, nfrb
          rr = e(ifr,ith)
          if (abs(rr/emax).lt.rrcut) then
            pnum(ifr) = '     '
          else
            write (stra, fmt='(i5)') nint (rr*facsp/fsc)
            pnum(ifr) = stra
          end if
        end do
        write (nds,931) strang, (pnum(ifr),ifr=1, nfrb)
      end do
      !
      do ith= nth, ithsec, -1
        intang = 630 - nint (dthdeg*real(ith-1))
        call angstr (intang, strang, 4, 2)
        do ifr=1, nfrb
          rr = e(ifr,ith)
          if (abs(rr/emax).lt.rrcut) then
            pnum(ifr) = '     '
          else
            write (stra, fmt='(i5)') nint (rr*facsp/fsc)
            pnum(ifr) = stra
          end if
        end do
        write (nds,931) strang, (pnum(ifr),ifr=1, nfrb)
      end do
      !
      !     write ending :
      !
      pnum(1) = '-----'
      pnum(2) = '--   '
      if (nfrb.lt.25) then
        write (nds,921) (pnum(1),ifr=1, nfrb), pnum(2)
      else
        write (nds,921) (pnum(1),ifr=1, nfrb)
      end if
      write (nds,950)
      !
    end if
    !
    return
    !
    ! formats
    !
900 format (/'  location : ',a/                               &
         '  spectrum : ',a,' (normalized) ',              &
         '  maximum value : ',e10.3,1x,a/)
901 format (/'  location : ',a/                               &
         '  spectrum : ',a,'  units : ',e10.3,1x,a,        &
         '  maximum value : ',e10.3,1x,a/)
    !
910 format (5x,'  ang.|  frequencies (hz) '/                  &
         5x,'  deg.|',f6.3,15f8.3)
920 format (5x,'  ----+',60a2)
930 format (5x,' ',a4,' |',60a2)
    !
911 format ('  ang.|  frequencies (hz) '/                     &
         '  deg.|',12f10.3)
921 format ('  ----|',25a5)
931 format (' ',a4,' |',25a5)
    !
950 format (' ')
    !
    !/
    !/ internal subroutine angstr ---------------------------------------- /
    !/
  contains
    !/
    !/ ------------------------------------------------------------------- /
    subroutine angstr (iang, sang, ilen, inum)
      !/
      !/                  +-----------------------------------+
      !/                  | wavewatch iii           noaa/ncep |
      !/                  |           h. l. tolman            |
      !/                  |                        fortran 90 |
      !/                  | last update :         29-nov-1999 |
      !/                  +-----------------------------------+
      !/
      !/    10-mar-1992 : final fortran 77                    ( version 1.18 )
      !/    29-nov-1999 : upgrade to fortran 90               ( version 2.00 )
      !
      !     input  : iang --> integer angle (degrees)
      !              ilen --> string length
      !              inum --> <1 : only four main directions
      !                        1 : n,e,s,w and numerical output
      !                        2 : eight main directions
      !                       >2 : eight directions + numerical output
      !     output : sang --> string
      !
      !/ ------------------------------------------------------------------- /
      !/
      !
      implicit none
      !/
      !/ ------------------------------------------------------------------- /
      !/ parameter list
      !/
      integer, intent(in)     :: iang, ilen, inum
      character, intent(out)  :: sang*(*)
      !/
      !/ ------------------------------------------------------------------- /
      !/ local parameters
      !/
      integer                 :: i, j
      character               :: saux*4
      !/
      !/ ------------------------------------------------------------------- /
      !/
      !     numerical :
      !
      if (inum.eq.1 .or. inum.ge.3) then
        write (saux, fmt='(i4)') iang
      else
        saux = '    '
      end if
      !
      !     string :
      !
      if (iang.eq.0) then
        saux = '   n'
      else if (iang.eq.90) then
        saux = '   e'
      else if (iang.eq.180) then
        saux = '   s'
      else if (iang.eq.270) then
        saux = '   w'
      else if (inum.ge.2) then
        if (iang.eq.45) then
          saux = '  ne'
        else if (iang.eq.135) then
          saux = '  se'
        else if (iang.eq.225) then
          saux = '  sw'
        else if (iang.eq.315) then
          saux = '  nw'
        end if
      end if
      !
      !     auxilary string to output :
      !
      do i=1, ilen-4
        sang = ' '
      end do
      j = 0
      do i=ilen-3, ilen
        j = j + 1
        sang(i:i) = saux(j:j)
      end do
      return
      !/
      !/ end of angstr ----------------------------------------------------- /
      !/
    end subroutine angstr
    !/
    !/ end of prt2ds ----------------------------------------------------- /
    !/
  end subroutine prt2ds
  !/
  !/ end of module w3arrymd -------------------------------------------- /
  !/
end module w3arrymd
