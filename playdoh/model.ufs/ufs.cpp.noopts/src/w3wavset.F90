!> @file
!> @brief contains module w3wavset for implicit solution of wave
!>        setup problem.
!>
!> @author aron roland
!> @author mathieu dutour-sikiric
!> @date 1-jun-2018
!>
!     ----------------------------------------------------------------
!>
!> @brief implicit solution of wave setup problem following
!>        dingemans for structured and unstructured grids.
!>
!> @author aron roland
!> @author mathieu dutour-sikiric
!> @date 1-jun-2018
!>
      module w3wavset
!/                  +-----------------------------------+
!/                  | wavewatch iii           noaa/ncep |
!/                  |                                   |
!/                  | aron roland (bgs it&e gmbh)       |
!/                  | mathieu dutour-sikiric (irb)      |
!/                  |                                   |
!/                  |                        fortran 95 |
!/                  | last update :         1-june-2018 |
!/                  +-----------------------------------+
!/
!/    01-june-2016 : origination                        ( version 6.04 )
!/
!  1. purpose : implicit solution of wave setup problem following
!               dingemans for structured and unstructured grids
!
!  2. method :  to be described
!
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
      use yowdatapool, only: rkind
!/
!/ ------------------------------------------------------------------- /
!/ parameter list
!/
!/ ------------------------------------------------------------------- /
!/ local parameters
!/
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
      logical :: do_wave_setup = .true.
      contains
!/ ------------------------------------------------------------------- /
!>
!> @brief differentiate xy, using linear shape function.
!>
!> @param[in]  var
!> @param[out] dvdx
!> @param[out] dvdy
!>
!> @author aron roland
!> @author mathieu dutour-sikiric
!> @date 1-may-2018
!>
      subroutine differentiate_xydir_native(var, dvdx, dvdy)
!/
!/                  +-----------------------------------+
!/                  | wavewatch iii           noaa/ncep |
!/                  |                                   |
!/                  | aron roland (bgs it&e gmbh)       |
!/                  | mathieu dutour-sikiric (irb)      |
!/                  |                                   |
!/                  |                        fortran 90 |
!/                  | last update :         01-mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-mai-2018 : origination.                        ( version 6.04 )
!/
!  1. purpose : differentiate xy
!  2. method : linear shape function
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
      use yowexchangemodule, only : pdlib_exchange1dreal
      use yownodepool,    only : pdlib_ien, pdlib_tria, npa
      use yowelementpool, only : ine, ne
      use w3gdatmd, only : mapsta
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
      real(rkind), intent(in)  :: var(npa)
      real(rkind), intent(out) :: dvdx(npa), dvdy(npa)
      integer           :: ni(3)
      integer           :: ie, i1, i2, i3, ip
      real(rkind)           :: dedy(3),dedx(3)
      real(rkind)           :: dvdxie, dvdyie
      real(rkind)           :: wei(npa), ew
      integer           :: ix
      wei  = 0.0
      dvdx = 0.0
      dvdy = 0.0
      do ie = 1, ne
        ni = ine(:,ie)
        i1 = ine(1,ie)
        i2 = ine(2,ie)
        i3 = ine(3,ie)
        wei(ni) = wei(ni) + 2.*pdlib_tria(ie)
        dedx(1) = pdlib_ien(1,ie)
        dedx(2) = pdlib_ien(3,ie)
        dedx(3) = pdlib_ien(5,ie)
        dedy(1) = pdlib_ien(2,ie)
        dedy(2) = pdlib_ien(4,ie)
        dedy(3) = pdlib_ien(6,ie)
        dvdxie  = dot_product( var(ni),dedx)
        dvdyie  = dot_product( var(ni),dedy)
        dvdx(ni) = dvdx(ni) + dvdxie
        dvdy(ni) = dvdy(ni) + dvdyie
      end do
      do ix=1,npa
        ew=wei(ix)
        dvdx(ix)=dvdx(ix) / ew
        dvdy(ix)=dvdy(ix) / ew
      end do
      call pdlib_exchange1dreal(dvdx)
      call pdlib_exchange1dreal(dvdy)
      end subroutine
!/ ------------------------------------------------------------------- /
!>
!> @brief differentiate xy based on mapsta, using linear shape function.
!>
!> @param[in]  var
!> @param[out] dvdx
!> @param[out] dvdy
!>
!> @author aron roland
!> @author mathieu dutour-sikiric
!> @date 1-may-2018
!>
      subroutine differentiate_xydir_mapsta(var, dvdx, dvdy)
!/
!/                  +-----------------------------------+
!/                  | wavewatch iii           noaa/ncep |
!/                  |                                   |
!/                  | aron roland (bgs it&e gmbh)       |
!/                  | mathieu dutour-sikiric (irb)      |
!/                  |                                   |
!/                  |                        fortran 90 |
!/                  | last update :         01-mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-mai-2018 : origination.                        ( version 6.04 )
!/
!  1. purpose : differentiate xy based on mapsta
!  2. method : linear shape function
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
      use yowexchangemodule, only : pdlib_exchange1dreal
      use yownodepool,    only : pdlib_ien, pdlib_tria, npa, iplg
      use yowelementpool, only : ine, ne
      use w3gdatmd, only : mapsta
      use w3parall, only: init_get_isea
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
      real(rkind), intent(in)  :: var(npa)
      real(rkind), intent(out) :: dvdx(npa), dvdy(npa)
      integer           :: ni(3)
      integer           :: ie, i1, i2, i3, ip, ix
      real(rkind)           :: dedy(3),dedx(3)
      real(rkind)           :: dvdxie, dvdyie
      real(rkind)           :: wei(npa), ew
      integer           :: ix1, ix2, ix3, isea
      wei  = 0.0
      dvdx = 0.0
      dvdy = 0.0
      do ie = 1, ne
        ni = ine(:,ie)
        i1 = ine(1,ie)
        i2 = ine(2,ie)
        i3 = ine(3,ie)
        ix1=iplg(i1)
        ix2=iplg(i2)
        ix3=iplg(i3)
        if ((mapsta(1,ix1) .gt. 0).and.(mapsta(1,ix2) .gt. 0).and.(mapsta(1,ix3) .gt. 0)) then
          wei(ni) = wei(ni) + 2.*pdlib_tria(ie)
          dedx(1) = pdlib_ien(1,ie)
          dedx(2) = pdlib_ien(3,ie)
          dedx(3) = pdlib_ien(5,ie)
          dedy(1) = pdlib_ien(2,ie)
          dedy(2) = pdlib_ien(4,ie)
          dedy(3) = pdlib_ien(6,ie)
          dvdxie  = dot_product( var(ni),dedx)
          dvdyie  = dot_product( var(ni),dedy)
          dvdx(ni) = dvdx(ni) + dvdxie
          dvdy(ni) = dvdy(ni) + dvdyie
        end if
      end do
      do ip=1,npa
        ix=iplg(ip)
        ew=wei(ip)
        if (ew .gt. 0 .and. mapsta(1,ix) .gt. 0) then
          dvdx(ip)=dvdx(ip) / ew
          dvdy(ip)=dvdy(ip) / ew
        else
          dvdx(ip)=0.
          dvdy(ip)=0.
        endif
      end do
      do ip=1,npa
        ix=iplg(ip)
        if (mapsta(1,ix) .lt. 0) then
          dvdx(ip)=0.
          dvdy(ip)=0.
        end if
      end do
      call pdlib_exchange1dreal(dvdx)
      call pdlib_exchange1dreal(dvdy)
      end subroutine
!/ ------------------------------------------------------------------- /
!>
!> @brief driver routine for xydir.
!>
!> @param[in]  var
!> @param[out] dvdx
!> @param[out] dvdy
!>
!> @author mathieu dutour-sikiric
!> @author aron roland
!> @date 1-may-2018
!>
      subroutine differentiate_xydir(var, dvdx, dvdy)
!/
!/                  +-----------------------------------+
!/                  | wavewatch iii           noaa/ncep |
!/                  |                                   |
!/                  | mathieu dutour-sikiric (irb)      |
!/                  | aron roland (bgs it&e gmbh)       |
!/                  |                        fortran 90 |
!/                  | last update :         01-mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-mai-2018 : origination.                        ( version 6.04 )
!/
!  1. purpose : driver routine for xydir
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
      use yownodepool, only: npa
      implicit none
!/
!/ ------------------------------------------------------------------- /
!/ parameter list
!/
!/ ------------------------------------------------------------------- /
!/ local parameters
!/
      real(rkind), intent(in)  :: var(npa)
      real(rkind), intent(out) :: dvdx(npa), dvdy(npa)
!/
!/ ------------------------------------------------------------------- /
!/
!
      call differentiate_xydir_mapsta(var, dvdx, dvdy)
!      call differentiate_xydir_native(var, dvdx, dvdy)
      end subroutine
!/ ------------------------------------------------------------------- /
!>
!> @brief setup boundary pointer.
!>
!> @param[out] f_x
!> @param[out] f_y
!> @param[out] dwnx
!>
!> @author aron roland
!> @author mathieu dutour-sikiric
!> @date 1-may-2018
!>
      subroutine trig_compute_lh_stress(f_x, f_y, dwnx)
!/
!/                  +-----------------------------------+
!/                  | wavewatch iii           noaa/ncep |
!/                  |                                   |
!/                  | aron roland (bgs it&e gmbh)       |
!/                  | mathieu dutour-sikiric (irb)      |
!/                  |                                   |
!/                  |                        fortran 90 |
!/                  | last update :         01-mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-mai-2018 : origination.                        ( version 6.04 )
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
      use constants, only: grav, dwat
      use yownodepool, only: npa, iplg
      use w3gdatmd, only : mapfs
      use w3adatmd, only: sxx, sxy, syy, wn, cg
      use w3parall, only: init_get_isea
      use w3odatmd, only : iaproc
      use w3gdatmd, only : nseal, mapsta
      use w3adatmd, only: dw
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
      real(rkind), intent(out) :: f_x(npa), f_y(npa), dwnx(npa)
      real(rkind) :: h
      real(rkind) :: sxx_x, sxx_y
      real(rkind) :: sxy_x, sxy_y
      real(rkind) :: syy_x, syy_y
      integer i, ip, ix
      integer jsea, isea
      real(rkind) :: u_x1(npa), u_y1(npa)
      real(rkind) :: u_x2(npa), u_y2(npa)
      real(rkind) :: sxx_p(npa), sxy_p(npa), syy_p(npa)
      real(rkind) :: esxx, esxy, esyy
      integer :: sxxmethod = 1
      sxx_p=0
      sxy_p=0
      syy_p=0
      dwnx=0
      do jsea=1,nseal
        ip = jsea ! we remove the z_status because now nx = nsea
        ix=iplg(ip)
        isea=mapfs(1,ix)
        if (sxxmethod .eq. 1) then
          esxx=sxx(jsea)/(dwat*grav)
          esxy=sxy(jsea)/(dwat*grav)
          esyy=syy(jsea)/(dwat*grav)
        end if
        sxx_p(ip)=dble(esxx)
        sxy_p(ip)=dble(esxy)
        syy_p(ip)=dble(esyy)
        dwnx(ip)=dw(isea)
      end do
      !
      call differentiate_xydir(sxx_p, u_x1, u_y1)
      call differentiate_xydir(sxy_p, u_x2, u_y2)
      f_x = -u_x1 - u_y2
      !
      call differentiate_xydir(syy_p, u_x1, u_y1)
      f_y = -u_y1 - u_x2
      end subroutine
!/ ------------------------------------------------------------------- /
!>
!> @brief differentiate other way around.
!>
!> @param[in] ie
!> @param[in] i1
!> @param[inout] ugrad
!> @param[inout] vgrad
!>
!> @author mathieu dutour-sikiric
!> @author aron roland
!> @date 1-may-2018
!>
      subroutine trig_compute_diff(ie, i1, ugrad, vgrad)
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
!  1. purpose : differentiate other way around ...
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
      use yowelementpool, only: ine
      use yownodepool,    only: x, y, pdlib_tria
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
      integer, intent(in) :: ie, i1
      real(rkind), intent(inout) :: ugrad, vgrad
      real(rkind) :: h
      integer i2, i3, ip1, ip2, ip3
      integer :: pos_trick(3,2)
      pos_trick(1,1) = 2
      pos_trick(1,2) = 3
      pos_trick(2,1) = 3
      pos_trick(2,2) = 1
      pos_trick(3,1) = 1
      pos_trick(3,2) = 2
      i2=pos_trick(i1, 1)
      i3=pos_trick(i1, 2)
      ip1=ine(i1, ie)
      ip2=ine(i2, ie)
      ip3=ine(i3, ie)
      h=2.0*pdlib_tria(ie)
      ugrad=-(y(ip3) - y(ip2))/h
      vgrad= (x(ip3) - x(ip2))/h
      end subroutine
!/ ------------------------------------------------------------------- /
!>
!> @brief setup system matrix for solutions of wave setup eq.
!>
!> @param[in]  fx
!> @param[in]  fy
!> @param[in]  dwnx
!> @param[out] aspar
!> @param[out] b
!> @param[in]  active
!> @param[out] activesec
!>
!> @author mathieu dutour-sikiric
!> @author aron roland
!> @date 1-may-2018
!>
      subroutine trig_wave_setup_compute_system(aspar, b, fx, fy, dwnx, active, activesec)
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
!  1. purpose : setup system matrix for solutions of wave setup eq.
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
      use yowelementpool, only: ine, ne
      use yownodepool, only: pdlib_nnz, pdlib_ja_ie, pdlib_tria, npa, np
      use yownodepool, only: pdlib_i_diag
      use yownodepool, only: iplg
      use w3odatmd, only : iaproc
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
      real(rkind), intent(in)  :: fx(npa), fy(npa), dwnx(npa)
      real(rkind), intent(out) :: aspar(pdlib_nnz)
      real(rkind), intent(out) :: b(npa)
      integer, intent(in)  :: active(npa)
      integer, intent(out)  :: activesec(npa)
      integer :: pos_trick(3,2), pos_shift(3,3)
      integer i1, i2, i3, ip1, ip2, ip3
      integer idx, idx1, idx2, idx3
      integer ie, ip, i, j, k, ipp, jpp
      real(rkind) :: edep, efx, efy, escal, efact, earea
      real(rkind) :: ugrad, vgrad, ugrad1, vgrad1
      real(rkind) :: eoff
      logical doprintout
      integer sumactive
      integer lidx(2), kidx(2), jdx
      integer ipglob1, ipglob2, ipglob3
      pos_trick(1,1) = 2
      pos_trick(1,2) = 3
      pos_trick(2,1) = 3
      pos_trick(2,2) = 1
      pos_trick(3,1) = 1
      pos_trick(3,2) = 2
      aspar=0
      b=0
      do i=1,3
        do j=1,3
          k= i-j+1
          if (k .le. 0) then
            k=k+3
          end if
          if (k .ge. 4) then
            k=k-3
          end if
          pos_shift(i,j)=k
        end do
      end do
      do i=1,3
        jdx=0
        do idx=1,3
          k=pos_shift(i,idx)
          if (k .ne. i) then
            jdx=jdx+1
            lidx(jdx)=idx
            kidx(jdx)=k
          end if
        end do
        pos_shift(i,lidx(1))=kidx(2)
        pos_shift(i,lidx(2))=kidx(1)
      end do
      activesec=0
      do ie=1,ne
        ip1=ine(1,ie)
        ip2=ine(2,ie)
        ip3=ine(3,ie)
        efx =(fx(ip1) + fx(ip2) + fx(ip3))/3
        efy =(fy(ip1) + fy(ip2) + fy(ip3))/3
        sumactive=active(ip1) + active(ip2) + active(ip3)
        if (sumactive .eq. 3) then
          activesec(ip1)=1
          activesec(ip2)=1
          activesec(ip3)=1
          edep=(dwnx(ip1) + dwnx(ip2) + dwnx(ip3))/3.0
          earea=pdlib_tria(ie)
          efact=edep*earea
          do i1=1,3
            i2=pos_trick(i1,1)
            i3=pos_trick(i1,2)
            ip1=ine(i1,ie)
            ip2=ine(i2,ie)
            ip3=ine(i3,ie)
            idx1=pdlib_ja_ie(i1,1,ie)
            idx2=pdlib_ja_ie(i1,2,ie)
            idx3=pdlib_ja_ie(i1,3,ie)
            call trig_compute_diff(ie, i1, ugrad1, vgrad1)
            escal=ugrad1*efx + vgrad1*efy
            b(ip1) = b(ip1) + escal*earea
            !
            do idx=1,3
              k=pos_shift(i1, idx)
              call trig_compute_diff(ie, k, ugrad, vgrad)
              escal=ugrad*ugrad1 + vgrad*vgrad1
              j=pdlib_ja_ie(i1,idx,ie)
              aspar(j)=aspar(j) + efact*escal
            end do
          end do
        end if
      end do
      doprintout=.true.
      if (doprintout .eqv. .true.) then
        do ip=1,np
          eoff=0
        end do
      end if
      end subroutine
!/ ------------------------------------------------------------------- /
!>
!> @brief preconditioner.
!>
!> @param[in]  aspar
!> @param[in]  thein
!> @param[out] theout
!> @param[in]  active
!> @param[in]  activesec
!>
!> @author mathieu dutour-sikiric
!> @author aron roland
!> @date 1-may-2018
!>
      subroutine trig_wave_setup_apply_precond(aspar, thein, theout, active, activesec)
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
!  1. purpose : preconditioner
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
      use yowexchangemodule, only : pdlib_exchange1dreal
      use yownodepool, only: pdlib_nnz, pdlib_ia, pdlib_ja, pdlib_i_diag
      use yownodepool, only: npa
      use w3odatmd, only : iaproc
      use w3odatmd, only : iaproc
      use yownodepool, only: iplg
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
      real(rkind), intent(in) :: aspar(pdlib_nnz)
      real(rkind), intent(in) :: thein(npa)
      real(rkind), intent(out) :: theout(npa)
      integer, intent(in) :: active(npa), activesec(npa)
      integer ip, j1, j, jp, j2
      real(rkind) :: ecoeff
      integer :: theprecond = 2
      if (theprecond .eq. 0) then
        theout=thein
      end if
      if (theprecond .eq. 1) then
        theout=0
        do ip=1,npa
          if (active(ip) .eq. 1) then
            j1=pdlib_i_diag(ip)
            do j=pdlib_ia(ip),pdlib_ia(ip+1)-1
              jp=pdlib_ja(j)
              if (activesec(jp) .eq. 1) then
                if (j .eq. j1) then
                  ecoeff=1.0/aspar(j)
                else
                  j2=pdlib_i_diag(jp)
                  ecoeff=-aspar(j) /(aspar(j1)*aspar(j2))
                end if
                theout(ip)=theout(ip) + ecoeff*thein(jp)
              end if
            end do
          end if
        end do
      end if
      if (theprecond .eq. 2) then
        do ip=1,npa
          if (activesec(ip) .eq. 1) then
            j=pdlib_i_diag(ip)
            theout(ip)=thein(ip)/aspar(j)
          else
            theout(ip)=thein(ip)
          end if
        end do
      end if
      call pdlib_exchange1dreal(theout)
      end subroutine
!/ ------------------------------------------------------------------- /
!>
!> @brief
!>
!> @param[in]  aspar
!> @param[in]  thein
!> @param[out] theout
!> @param[in]  active
!> @param[in]  activesec
!>
!> @author mathieu dutour-sikiric
!> @author aron roland
!> @date 1-may-2018
!>
      subroutine trig_wave_setup_apply_fct(aspar, thein, theout, active, activesec)
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
!  1. purpose : compute off diagonal contr.
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
      use yowexchangemodule, only : pdlib_exchange1dreal
      use yownodepool, only: pdlib_ia, pdlib_ja, pdlib_nnz
      use yownodepool, only: np, npa
      use w3gdatmd, only: nseal
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
      real(rkind), intent(in) :: aspar(pdlib_nnz)
      real(rkind), intent(in) :: thein(npa)
      real(rkind), intent(out) :: theout(npa)
      integer, intent(in) :: active(npa), activesec(npa)
      integer ip, j, jp
      real(rkind) :: ecoeff
      theout=0
      do ip=1,npa
        if (activesec(ip) .eq. 1) then
          do j=pdlib_ia(ip),pdlib_ia(ip+1)-1
            jp=pdlib_ja(j)
            ecoeff=aspar(j)
            theout(ip)=theout(ip) + ecoeff*thein(jp)
          end do
        end if
      end do
      call pdlib_exchange1dreal(theout)
      end subroutine
!/ ------------------------------------------------------------------- /
!>
!> @brief scalar product plus exchange.
!>
!> @param[in]     v1
!> @param[in]     v2
!> @param[inout]  escal
!>
!> @author mathieu dutour-sikiric
!> @author aron roland
!> @date 1-may-2018
!>
      subroutine trig_wave_setup_scalar_prod(v1, v2, escal)
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
!  1. purpose : scalar prod. + exchange
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
      use w3gdatmd, only: nx
      use w3adatmd, only: mpi_comm_wcmp
      use yowdatapool, only: rtype, istatus
      use yownodepool, only: np, npa
      use w3odatmd, only : iaproc, naproc, ntproc
      use w3gdatmd, only: nseal
      use mpi, only : mpi_sum
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
      real(rkind), intent(in) :: v1(npa), v2(npa)
      real(rkind), intent(inout) :: escal
      integer ip
      real(rkind) :: lscal_loc(1), lscal_gl(1)
      integer ierr
      lscal_loc = 0
      do ip=1,np
        lscal_loc(1) = lscal_loc(1) + v1(ip)*v2(ip)
      end do
      call mpi_allreduce(lscal_loc,lscal_gl,1,rtype,mpi_sum,mpi_comm_wcmp,ierr)
      escal = lscal_gl(1)
      end subroutine
!/ ------------------------------------------------------------------- /
!>
!> @brief poisson equation solver.
!>
!> @param[in]  aspar
!> @param[in]  b
!> @param[out] theout
!> @param[in]  active
!> @param[in]  activesec
!>
!> @author mathieu dutour-sikiric
!> @author aron roland
!> @date 1-may-2018
!>
      subroutine trig_wave_setup_solve_poisson_neumann_dir(aspar, b, theout, active, activesec)
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
!  1. purpose : poisson eq. solver
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
      use yownodepool, only: pdlib_nnz
      use w3gdatmd, only: nseal, solverthr_stp
      use w3odatmd, only : iaproc
      use yownodepool, only: np, npa
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
      real(rkind), intent(in) :: aspar(pdlib_nnz)
      real(rkind), intent(in) :: b(npa)
      real(rkind), intent(out) :: theout(npa)
      integer, intent(in) :: active(npa), activesec(npa)
      real(rkind) :: v_x(npa), v_r(npa), v_z(npa), v_p(npa), v_y(npa)
      real(rkind) :: uo, un, alphav, h1, h2
      real(rkind) :: enorm, beta
      real(rkind) :: solverthr
      integer ip, nbiter
      solverthr = solverthr_stp
      nbiter=0
      v_x=0
      v_r=b
      call trig_wave_setup_apply_precond(aspar, v_r, v_z, active, activesec)
      v_p=v_z
      call trig_wave_setup_scalar_prod(v_z, v_r, uo)
      call trig_wave_setup_scalar_prod(b, b, enorm)
      if (enorm .le. solverthr) then
        theout=v_x
        return
      end if
      do
        nbiter=nbiter + 1
        call trig_wave_setup_apply_fct(aspar, v_p, v_y, active, activesec)
        call trig_wave_setup_scalar_prod(v_p, v_y, h2)
        alphav=uo/h2
        !
        do ip=1,npa
          v_x(ip) = v_x(ip) + alphav * v_p(ip)
          v_r(ip) = v_r(ip) - alphav * v_y(ip)
        end do
        !
        call trig_wave_setup_scalar_prod(v_r, v_r, enorm)
        if (enorm .le. solverthr) then
          exit
        end if
        !
        call trig_wave_setup_apply_precond(aspar, v_r, v_z, active, activesec)
        call trig_wave_setup_scalar_prod(v_z, v_r, un)
        !
        beta=un/uo
        uo=un
        !
        do ip=1,npa
          v_p(ip)=v_z(ip) + beta * v_p(ip)
        end do
      end do
      theout=v_x
      end subroutine
!/ ------------------------------------------------------------------- /
!>
!> @brief set mean value.
!>
!> @param[inout] thevar
!>
!> @author mathieu dutour-sikiric
!> @author aron roland
!> @date 1-may-2018
!>
      subroutine trig_set_meanvalue_to_zero(thevar)
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
!  1. purpose : set. mean value
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
      use yownodepool, only: pdlib_si
      use w3gdatmd, only: nx, si
      use w3gdatmd, only: nseal
      use w3adatmd, only: mpi_comm_wcmp
      use w3odatmd, only : iaproc, naproc, ntproc
      use yowdatapool, only: rtype, istatus
      use yownodepool, only: np, npa
      use mpi, only : mpi_sum
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
      real(rkind), intent(inout) :: thevar(npa)
      real(rkind) :: sum_si_var, sum_si, themean
      integer ip, ierr
      real(rkind) :: evect_loc(2), evect_gl(2)
      integer iproc
      sum_si_var=0
      sum_si=0
      do ip=1,np
        sum_si_var = sum_si_var + pdlib_si(ip)*thevar(ip)
        sum_si     = sum_si     + pdlib_si(ip)
      end do
      evect_loc(1)=sum_si_var
      evect_loc(2)=sum_si
      call mpi_allreduce(evect_loc,evect_gl,2,rtype,mpi_sum,mpi_comm_wcmp,ierr)
      sum_si_var=evect_gl(1)
      sum_si    =evect_gl(2)
      themean=sum_si_var/sum_si
      do ip=1,npa
        thevar(ip)=thevar(ip) - themean
      end do
      end subroutine
!/ ------------------------------------------------------------------- /
!>
!> @brief compute active node for setup comp.
!>
!> @param[in] dwnx
!> @param[out] active
!>
!> @author aron roland
!> @author mathieu dutour-sikiric
!> @date 1-may-2018
!>
      subroutine compute_active_node(dwnx, active)
!/
!/                  +-----------------------------------+
!/                  | wavewatch iii           noaa/ncep |
!/                  |                                   |
!/                  | aron roland (bgs it&e gmbh)       |
!/                  | mathieu dutour-sikiric (irb)      |
!/                  |                                   |
!/                  |                        fortran 90 |
!/                  | last update :         01-mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-mai-2018 : origination.                        ( version 6.04 )
!/
!  1. purpose : compute active node for setup comp.
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
      use w3gdatmd, only : crit_dep_stp
      use yownodepool, only: pdlib_nnz, pdlib_ia, pdlib_ja, iplg, npa, np
      use w3odatmd, only : iaproc
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
      real(rkind), intent(in) :: dwnx(npa)
      integer, intent(out) :: active(npa)
      integer ip, eact
      do ip=1,npa
        if (dwnx(ip) .ge. crit_dep_stp) then
          eact=1
        else
          eact=0
        end if
        active(ip)=eact
      end do
      end subroutine
!/ ------------------------------------------------------------------- /
!>
!> @brief setup computation.
!>
!> @author mathieu dutour-sikiric
!> @author aron roland
!> @date 1-may-2018
!>
      subroutine trig_wave_setup_computation
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
!  1. purpose : setup computation
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
      use yownodepool, only: pdlib_nnz, pdlib_ia, pdlib_ja, iplg, npa, np
      use w3gdatmd, only : mapfs
      use w3parall, only : synchronize_global_array
      use w3adatmd, only: dw
      use w3gdatmd, only: nseal, nsea, nx
      use w3wdatmd, only: zeta_setup
      use w3odatmd, only : iaproc, naproc, ntproc
      use w3parall, only: init_get_isea
      use yowexchangemodule, only : pdlib_exchange1dreal
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
!      call w3setg
      real(rkind) :: zeta_work(npa)
      real(rkind) :: zeta_work_all(nx)
      real(rkind) :: f_x(npa), f_y(npa), dwnx(npa)
      real(rkind) :: aspar(pdlib_nnz), b(npa)
      integer i, isea, jsea, ix, ip, ip_glob
      integer :: active(npa), activesec(npa)
      real(rkind) max_val, min_val
!   zeta_setup is allocated on 1:nsea
!   zeta_work is on 1:npa
      zeta_work=0
      do ip=1,npa
        ix=iplg(ip)
        isea=mapfs(1,ix)
        if (isea .gt. 0) then
          zeta_work(ip)=zeta_setup(isea)
        end if
      end do
      call trig_compute_lh_stress(f_x, f_y, dwnx)
      call compute_active_node(dwnx, active)
      call trig_wave_setup_compute_system(aspar, b, f_x, f_y, dwnx, active, activesec)
!      call trig_set_meanvalue_to_zero(b)
      call trig_wave_setup_solve_poisson_neumann_dir(aspar, b, zeta_work, active, activesec)
      call trig_set_meanvalue_to_zero(zeta_work)
      call pdlib_exchange1dreal(zeta_work)
      max_val = -100000000
      min_val = -100000000
      do ip=1,npa
        ix=iplg(ip)
        isea=mapfs(1,ix)
        if (isea .gt. 0) then
           zeta_setup(isea) = zeta_work(ip)
           max_val = max(max_val, zeta_work(ip))
           min_val = max(min_val, zeta_work(ip))
        end if
      end do
      zeta_work_all = 0.
      do ip = 1, npa
        isea = iplg(ip)
        zeta_work_all(isea) = zeta_work(ip)
      end do
      call synchronize_global_array(zeta_work_all)
      do ix = 1, nx
        zeta_setup(ix) = zeta_work_all(ix)
      end do
      if (iaproc .eq. 1) then
        write(6666)  1. 
        write(6666)  (zeta_work_all(ix), zeta_work_all(ix), zeta_work_all(ix), ix = 1, nx)
      endif
      end subroutine
!/ ------------------------------------------------------------------- /
!>
!> @brief wave setup for fd grids.
!>
!> @param[in] imod
!>
!> @author mathieu dutour-sikiric
!> @author aron roland
!> @date 1-may-2018
!>
      subroutine preparation_fd_scheme(imod)
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
!  1. purpose : wave setup for fd grids
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
      use yownodepool, only: pdlib_nnz, pdlib_ia, pdlib_ja, pdlib_i_diag
      use w3gdatmd, only: nx, ny, nsea, mapsf, grids
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
      integer, intent(in) :: imod
      integer in, isea, nbedge
      integer ix, iy, idx
      integer neighmat(4,2)
      integer, allocatable :: stat_sealand(:,:)
      integer, allocatable :: edges(:,:)
      integer ixn, jxn, jsea, j
      !
      allocate(grids(imod)%neigh(nsea,4))
      grids(imod)%neigh=0
      allocate(stat_sealand(nx,ny))
      stat_sealand=0
      do isea=1,nsea
        ix=mapsf(isea,1)
        iy=mapsf(isea,2)
        stat_sealand(ix,iy)=isea
      end do
      neighmat(1,1)=1
      neighmat(1,2)=0
      neighmat(2,1)=-1
      neighmat(2,2)=0
      neighmat(3,1)=0
      neighmat(3,2)=1
      neighmat(4,1)=0
      neighmat(4,2)=-1
      nbedge=0
      pdlib_nnz=0
      do isea=1,nsea
        ix=mapsf(isea,1)
        iy=mapsf(isea,2)
        idx=0
        do in=1,4
          ixn=ix+neighmat(in,1)
          jxn=ix+neighmat(in,2)
          jsea=stat_sealand(ixn,jxn)
          if (jsea .gt. 0) then
            idx=idx+1
            grids(imod)%neigh(isea,idx)=jsea
            if (jsea < isea) then
              nbedge=nbedge+1
            end if
            pdlib_nnz=pdlib_nnz+1
          end if
        end do
        pdlib_nnz=pdlib_nnz+1
      end do
      !
      grids(imod)%nbedge=nbedge
      allocate(grids(imod)%edges(nbedge,2))
      idx=0
      do isea=1,nsea
        ix=mapsf(isea,1)
        iy=mapsf(isea,2)
        do in=1,4
          ixn=ix+neighmat(in,1)
          jxn=ix+neighmat(in,2)
          jsea=stat_sealand(ixn,jxn)
          if (jsea .gt. 0) then
            if (jsea < isea) then
              idx=idx+1
              grids(imod)%edges(idx,1)=jsea
              grids(imod)%edges(idx,2)=isea
            end if
          end if
        end do
      end do
      !
      allocate(pdlib_ia(nsea+1))
      allocate(pdlib_ja(pdlib_nnz))
      allocate(pdlib_i_diag(nsea))
      pdlib_ia(1)=1
      j=0
      do isea=1,nsea
        do in=1,4
          ixn=ix+neighmat(in,1)
          jxn=ix+neighmat(in,2)
          jsea=stat_sealand(ixn,jxn)
          if (jsea .gt. 0) then
            j=j+1
            pdlib_ja(j)=jsea
          end if
        end do
        j=j+1
        pdlib_ja(j)=isea
        pdlib_i_diag(isea)=j
        pdlib_ia(isea+1)=j+1
      end do
      end subroutine
!/ ------------------------------------------------------------------- /
!>
!> @brief compute off diagonal for fd grids.
!>
!> @param[in]  aspar
!> @param[in]  thein
!> @param[out] theout
!>
!> @author mathieu dutour-sikiric
!> @author aron roland
!> @date 1-may-2018
!>
      subroutine fd_wave_setup_apply_fct(aspar, thein, theout)
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
!  1. purpose : comp. off diagonal for fd grids
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
      use w3gdatmd, only: nx, nnz, iaa, jaa, nsea
      use yownodepool, only: pdlib_ia, pdlib_ja
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
      real(rkind), intent(in) :: aspar(nnz)
      real(rkind), intent(in) :: thein(nsea)
      real(rkind), intent(out) :: theout(nsea)
      integer ip, j, jp
      real(rkind) :: ecoeff
      theout=0
      do ip=1,nsea
        do j=pdlib_ia(ip),pdlib_ia(ip+1)-1
          jp=pdlib_ja(j)
          ecoeff=aspar(j)
          theout(ip)=theout(ip) + ecoeff*thein(jp)
        end do
      end do
      end subroutine
!/ ------------------------------------------------------------------- /
!>
!> @brief preconditioning for fd grids.
!>
!> @param[in]  aspar
!> @param[in]  thein
!> @param[out] theout
!>
!> @author mathieu dutour-sikiric
!> @author aron roland
!> @date 1-may-2018
!>
      subroutine fd_wave_setup_apply_precond(aspar, thein, theout)
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
!  1. purpose : precond. for fd grids
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
      use yownodepool, only: pdlib_nnz, pdlib_ia, pdlib_ja, pdlib_i_diag
      use w3gdatmd, only: nsea
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
      real(rkind), intent(in) :: aspar(pdlib_nnz)
      real(rkind), intent(in) :: thein(nsea)
      real(rkind), intent(out) :: theout(nsea)
      integer ip, j1, j, jp, j2
      real(rkind) :: ecoeff
      integer :: theprecond = 0
      if (theprecond .eq. 0) then
        theout=thein
      end if
      if (theprecond .eq. 1) then
        theout=0
        do ip=1,nsea
          j1=pdlib_i_diag(ip)
          do j=pdlib_ia(ip),pdlib_ia(ip+1)-1
            jp=pdlib_ja(j)
            if (j .eq. j1) then
              ecoeff=1.0/aspar(j)
            else
              j2=pdlib_i_diag(jp)
              ecoeff=-aspar(j) /(aspar(j1)*aspar(j2))
            end if
            theout(ip)=theout(ip) + ecoeff*thein(jp)
          end do
        end do
      end if
      if (theprecond .eq. 2) then
        do ip=1,nsea
          j=pdlib_i_diag(ip)
          theout(ip)=thein(ip)/aspar(j)
        end do
      end if
      end subroutine
!/ ------------------------------------------------------------------- /
!>
!> @brief radiation stresses for fd grids.
!>
!> @param[out] sxx_t
!> @param[out] sxy_t
!> @param[out] syy_t
!>
!> @author mathieu dutour-sikiric
!> @author aron roland
!> @date 1-may-2018
!>
      subroutine fd_collect_sxx_xy_yy(sxx_t, sxy_t, syy_t)
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
!  1. purpose : rad. stresses for fd grids
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
      use w3adatmd, only: sxx, sxy, syy
      use w3gdatmd, only: nsea, nseal
      use w3odatmd, only : iaproc, naproc
      use yowdatapool, only: rtype, istatus
      use w3adatmd, only: mpi_comm_wcmp
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
      integer isea, jsea
      integer ierr
      real(rkind), intent(out) :: sxx_t(nsea), sxy_t(nsea), syy_t(nsea)
      real(rkind) :: sxx_p(nseal), sxy_p(nseal), syy_p(nseal)
      real(rkind), allocatable :: rvect(:)
      integer iproc, nseal_loc
      do isea=1,nseal
        sxx_p(isea)=sxx(isea)
        sxy_p(isea)=sxy(isea)
        syy_p(isea)=syy(isea)
      end do
      if (iaproc .eq. 1) then
        do jsea=1,nseal
          isea=1 + (jsea-1)*naproc
          sxx_t(isea)=sxx_p(jsea)
          sxy_t(isea)=sxy_p(jsea)
          syy_t(isea)=syy_p(jsea)
        end do
        do iproc=2,naproc
          nseal_loc=1 + (nsea-iproc)/naproc
          allocate(rvect(nseal_loc))
          call mpi_recv(rvect,nseal_loc,rtype, iproc-1, 83, mpi_comm_wcmp, istatus, ierr)
          do jsea=1,nseal_loc
            isea = iproc + (jsea-1)*naproc
            sxx_t(isea)=rvect(jsea)
          end do
          call mpi_recv(rvect,nseal_loc,rtype, iproc-1, 89, mpi_comm_wcmp, istatus, ierr)
          do jsea=1,nseal_loc
            isea = iproc + (jsea-1)*naproc
            sxy_t(isea)=rvect(jsea)
          end do
          call mpi_recv(rvect,nseal_loc,rtype, iproc-1, 97, mpi_comm_wcmp, istatus, ierr)
          do jsea=1,nseal_loc
            isea = iproc + (jsea-1)*naproc
            syy_t(isea)=rvect(jsea)
          end do
          deallocate(rvect)
        end do
      else
        call mpi_send(sxx_p,nseal,rtype, 0, 83, mpi_comm_wcmp, ierr)
        call mpi_send(sxy_p,nseal,rtype, 0, 83, mpi_comm_wcmp, ierr)
        call mpi_send(syy_p,nseal,rtype, 0, 83, mpi_comm_wcmp, ierr)
      end if
      end subroutine
!/ ------------------------------------------------------------------- /
!>
!> @brief setup fluxes.
!>
!> @param[in] sxx_t
!> @param[in] sxy_t
!> @param[in] syy_t
!> @param[out] fx
!> @param[out] fy
!>
!> @author mathieu dutour-sikiric
!> @author aron roland
!> @date 1-may-2018
!>
      subroutine fd_compute_lh_stress(sxx_t, sxy_t, syy_t, fx, fy)
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
!  1. purpose : setup fluxes
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
      use w3gdatmd, only: nx, ny, nsea, neigh
      use w3adatmd, only: sxx, sxy, syy
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
      real(rkind), intent(in) :: sxx_t(nsea), sxy_t(nsea), syy_t(nsea)
      real(rkind), intent(out) :: fx(nsea), fy(nsea)
      real(rkind) :: h
      real(rkind) :: sxx_x, sxx_y
      real(rkind) :: sxy_x, sxy_y
      real(rkind) :: syy_x, syy_y
      real(rkind) :: efx, efy
      real(rkind) :: ugrad, vgrad
      integer ie, i1, i2, i3, ip1, ip2, ip3
      integer isea, jsea1, jsea2, jsea3, jsea4
      integer neighmat(4,2)
      real(rkind) dist_x, dist_y
      !
      neighmat(1,1)=1
      neighmat(1,2)=0
      neighmat(2,1)=-1
      neighmat(2,2)=0
      neighmat(3,1)=0
      neighmat(3,2)=1
      neighmat(4,1)=0
      neighmat(4,2)=-1
      fx=0
      fy=0
      do isea=1,nsea
        jsea1=neigh(isea,1)
        jsea2=neigh(isea,2)
        jsea3=neigh(isea,3)
        jsea4=neigh(isea,4)
        sxx_x=0
        sxx_y=0
        sxy_x=0
        sxy_y=0
        syy_x=0
        syy_y=0
        if ((jsea1 .gt. 0).and.(jsea2 .gt. 0)) then
          sxx_x=(sxx(jsea1) - sxx(jsea2))/(2*dist_x)
          sxy_x=(sxy(jsea1) - sxy(jsea2))/(2*dist_x)
          syy_x=(sxy(jsea1) - syy(jsea2))/(2*dist_x)
        end if
        if ((jsea1 .gt. 0).and.(jsea2 .eq. 0)) then
          sxx_x=(sxx(jsea1) - sxx(isea ))/dist_x
          sxy_x=(sxy(jsea1) - sxy(isea ))/dist_x
          syy_x=(sxy(jsea1) - syy(isea ))/dist_x
        end if
        if ((jsea1 .eq. 0).and.(jsea2 .gt. 0)) then
          sxx_x=(sxx(isea ) - sxx(jsea2))/dist_x
          sxy_x=(sxy(isea ) - sxy(jsea2))/dist_x
          syy_x=(sxy(isea ) - syy(jsea2))/dist_x
        end if
        if ((jsea3 .gt. 0).and.(jsea4 .gt. 0)) then
          sxx_x=(sxx(jsea3) - sxx(jsea4))/(2*dist_y)
          sxy_x=(sxy(jsea3) - sxy(jsea4))/(2*dist_y)
          syy_x=(sxy(jsea3) - syy(jsea4))/(2*dist_y)
        end if
        if ((jsea3 .eq. 0).and.(jsea4 .gt. 0)) then
          sxx_x=(sxx(isea ) - sxx(jsea4))/dist_y
          sxy_x=(sxy(isea ) - sxy(jsea4))/dist_y
          syy_x=(sxy(isea ) - syy(jsea4))/dist_y
        end if
        if ((jsea3 .gt. 0).and.(jsea4 .gt. 0)) then
          sxx_x=(sxx(jsea3) - sxx(isea ))/dist_y
          sxy_x=(sxy(jsea3) - sxy(isea ))/dist_y
          syy_x=(sxy(jsea3) - syy(isea ))/dist_y
        end if
        efx=-sxx_x - sxy_y
        efy=-syy_y - sxy_x
        fx(isea)=efx
        fy(isea)=efy
      end do
      end subroutine
!/ ------------------------------------------------------------------- /
!>
!> @brief differences on fd grids.
!>
!> @param[in]  iedge
!> @param[in]  isea
!> @param[inout] ugrad
!> @param[inout] vgrad
!> @param[inout] dist
!>
!> @author mathieu dutour-sikiric
!> @author aron roland
!> @date 1-may-2018
!>
      subroutine fd_compute_diff(iedge, isea, ugrad, vgrad, dist)
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
!  1. purpose : differences on fd grids
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
      use w3gdatmd, only: mapsf, edges
      use w3gdatmd, only: xgrd, ygrd
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
      integer, intent(in) :: iedge, isea
      real(rkind), intent(inout) :: ugrad, vgrad, dist
      real(rkind) :: h
      integer i2, i3, ip1, ip2, ip3
      integer ix1, iy1, ix2, iy2
      integer isea1, isea2
      real(rkind) deltax, deltay
      !
      isea1=edges(iedge,1)
      isea2=edges(iedge,2)
      ix1=mapsf(isea1,1)
      iy1=mapsf(isea1,2)
      ix2=mapsf(isea2,1)
      iy2=mapsf(isea2,2)
      deltax=xgrd(ix1,iy1) - xgrd(ix2,iy2)
      deltay=ygrd(ix1,iy1) - ygrd(ix2,iy2)
      dist=sqrt(deltax*deltax + deltay*deltay)
      if (isea .eq. isea1) then
        ugrad= deltax/dist
        vgrad= deltay/dist
      else
        ugrad=-deltax/dist
        vgrad=-deltay/dist
      end if
      end subroutine
!/ ------------------------------------------------------------------- /
!>
!> @brief setup matrix on fd grids.
!>
!> @param[out]  aspar
!> @param[out]  b
!> @param[in] fx
!> @param[in] fy
!>
!> @author mathieu dutour-sikiric
!> @author aron roland
!> @date 1-may-2018
!>
      subroutine fd_wave_setup_compute_system(aspar, b, fx, fy)
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
!  1. purpose : setup matrix on fd grids
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
      use yownodepool, only: pdlib_nnz
      use w3gdatmd, only: nx, ny, nsea, nbedge, edges
      use w3adatmd, only: dw
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
      real(rkind), intent(in)  :: fx(nsea), fy(nsea)
      real(rkind), intent(out) :: aspar(pdlib_nnz)
      real(rkind), intent(out) :: b(nx)
      integer :: pos_trick(3,2), pos_shift(3,3)
      integer i1, i2, i3, ip1, ip2, ip3
      integer idx, idx1, idx2, idx3
      integer ie, ip, i, j, k, ipp, jpp
      real(rkind) :: edep, efx, efy, escal, efact, elen
      real(rkind) :: ugrad, vgrad, ugrad1, vgrad1, dist1, dist2
      integer lidx(2), kidx(2), jdx
      integer isearel, jsearel, isea, jsea, iedge
      !
      aspar=0
      b=0
      do iedge=1,nbedge
        isea=edges(iedge,1)
        jsea=edges(iedge,2)
        edep=(dw(isea) + dw(jsea))/2.0
        efx =(fx(isea) + fx(jsea))/2.0
        efy =(fy(isea) + fy(jsea))/2.0
        do i=1,2
          isearel=edges(iedge,i)
          call fd_compute_diff(iedge, isearel, ugrad1, vgrad1, dist1)
          escal=ugrad1*efx + vgrad1*efy
          b(isearel) = b(isearel) + escal*dist1
          !
          do j=1,2
            jsearel=edges(iedge,j)
            call fd_compute_diff(iedge, jsearel, ugrad, vgrad, dist2)
            escal=ugrad*ugrad1 + vgrad*vgrad1
            aspar(j)=aspar(j)+efact*escal
          end do
        end do
      end do
      end subroutine
!/ ------------------------------------------------------------------- /
!>
!> @brief scalar product.
!>
!> @param[in]    v1
!> @param[in]    v2
!> @param[inout] escal
!>
!> @author mathieu dutour-sikiric
!> @author aron roland
!> @date 1-may-2018
!>
      subroutine fd_wave_setup_scalar_prod(v1, v2, escal)
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
!  1. purpose : scalar prod.
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
      use w3gdatmd, only: nx
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
      real(rkind), intent(in) :: v1(nx), v2(nx)
      real(rkind), intent(inout) :: escal
      integer ip
      escal=0
      do ip=1,nx
        escal=escal + v1(ip)*v2(ip)
      end do
      end subroutine
!/ ------------------------------------------------------------------- /
!>
!> @brief poisson solver on fd grids.
!>
!> @param[in]    aspar
!> @param[in]    b
!> @param[out] theout
!>
!> @author mathieu dutour-sikiric
!> @author aron roland
!> @date 1-may-2018
!>
      subroutine fd_wave_setup_solve_poisson_neumann_dir(aspar, b, theout)
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
!  1. purpose : possoin solver on fd grids
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
      use yownodepool, only: pdlib_nnz
      use w3gdatmd, only: nx
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
      real(rkind), intent(in) :: aspar(pdlib_nnz)
      real(rkind), intent(in) :: b(nx)
      real(rkind), intent(out) :: theout(nx)
      real(rkind) :: v_x(nx), v_r(nx), v_z(nx), v_p(nx), v_y(nx)
      real(rkind) :: uo, un, alphav, h1, h2
      real(rkind) :: enorm, beta
      real(rkind) :: solverthr
      integer ip, nbiter
      solverthr=0.00000001
      nbiter=0
      v_x=0
      v_r=b
      call fd_wave_setup_apply_precond(aspar, v_r, v_z)
      v_p=v_z
      call fd_wave_setup_scalar_prod(v_z, v_r, uo)
      do
        nbiter=nbiter + 1
        call fd_wave_setup_apply_fct(aspar, v_p, v_y)
        call fd_wave_setup_scalar_prod(v_p, v_y, h2)
        alphav=uo/h2
        !
        do ip=1,nx
          v_x(ip) = v_x(ip) + alphav * v_p(ip)
          v_r(ip) = v_r(ip) - alphav * v_y(ip)
        end do
        !
        call fd_wave_setup_scalar_prod(v_r, v_r, enorm)
        if (enorm .le. solverthr) then
          exit
        end if
        !
        call fd_wave_setup_apply_precond(aspar, v_r, v_z)
        call fd_wave_setup_scalar_prod(v_z, v_r, un)
        !
        beta=un/uo
        uo=un
        !
        do ip=1,nx
          v_p(ip)=v_z(ip) + beta * v_p(ip)
        end do
      end do
      theout=v_x
      end subroutine
!/ ------------------------------------------------------------------- /
!>
!> @brief set mean value.
!>
!> @param[inout] thevar
!>
!> @author mathieu dutour-sikiric
!> @author aron roland
!> @date 1-may-2018
!>
      subroutine fd_set_meanvalue_to_zero(thevar)
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
!  1. purpose : set meanvalue
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
      use w3gdatmd, only: nx, si
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
      real(rkind), intent(inout) :: thevar(nx)
      real(rkind) :: sum_si_var, sum_si, themean
      integer ip
      sum_si_var=0
      sum_si=0
      do ip=1,nx
        sum_si_var = sum_si_var + si(ip)*thevar(ip)
        sum_si     = sum_si     + si(ip)
      end do
      themean=sum_si_var/sum_si
      do ip=1,nx
        thevar(ip)=thevar(ip) - themean
      end do
      end subroutine
!/ ------------------------------------------------------------------- /
!>
!> @brief wave setup comp on fd grids.
!>
!> @param[inout] thevar
!>
!> @author mathieu dutour-sikiric
!> @author aron roland
!> @date 1-may-2018
!>
      subroutine fd_wave_setup_computation
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
!  1. purpose : wave setup comp. on fd grids
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
      use yownodepool, only: pdlib_nnz
      use w3gdatmd, only: nx, nsea, nseal
      use w3wdatmd, only: zeta_setup
      use yowdatapool, only: rtype, istatus
      use w3adatmd, only: mpi_comm_wcmp
      use w3odatmd, only : iaproc, naproc
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
      real(rkind) :: zeta_work(nsea)
      real(rkind) :: f_x(nsea), f_y(nsea)
      real(rkind) :: aspar(pdlib_nnz), b(nx)
      integer isea, iproc
      real(rkind) :: sxx_t(nsea), sxy_t(nsea), syy_t(nsea)
      integer ierr
      call fd_collect_sxx_xy_yy(sxx_t, sxy_t, syy_t)
      if (iaproc .eq. 1) then
        call fd_compute_lh_stress(sxx_t, sxy_t, syy_t, f_x, f_y)
        do isea=1,nsea
          zeta_work(isea)=zeta_setup(isea)
        end do
        call fd_wave_setup_compute_system(aspar, b, f_x, f_y)
        call fd_wave_setup_solve_poisson_neumann_dir(aspar, b, zeta_work)
        call fd_set_meanvalue_to_zero(zeta_work)
        do iproc=2,naproc
          call mpi_send(zeta_work,nsea,rtype, iproc-1, 23, mpi_comm_wcmp, ierr)
        end do
      else
        call mpi_recv(zeta_work,nseal,rtype, 0, 23, mpi_comm_wcmp, istatus, ierr)
      end if
      do isea=1,nsea
         zeta_setup(isea)=zeta_work(isea)
      end do
      end subroutine
!/ ------------------------------------------------------------------- /
!>
!> @brief general driver.
!>
!> @author aron roland
!> @author mathieu dutour-sikiric
!> @date 1-may-2018
!>
      subroutine wave_setup_computation
!/
!/                  +-----------------------------------+
!/                  | wavewatch iii           noaa/ncep |
!/                  |                                   |
!/                  | aron roland (bgs it&e gmbh)       |
!/                  | mathieu dutour-sikiric (irb)      |
!/                  |                                   |
!/                  |                        fortran 90 |
!/                  | last update :         01-mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-mai-2018 : origination.                        ( version 6.04 )
!/
!  1. purpose : general driver
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
      use w3gdatmd, only: nsea, nseal
      use w3gdatmd, only: gtype, ungtype
      use w3odatmd, only : iaproc, naproc, ntproc
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
      integer isea, jsea
      real(rkind), allocatable :: zeta_work(:)
      if (iaproc .le. naproc) then
        if (do_wave_setup) then
          if (gtype .eq. ungtype) then
            call trig_wave_setup_computation
          else
            call fd_wave_setup_computation
          end if
        end if
      end if
      end subroutine
!/ ------------------------------------------------------------------- /
      end module
!/ ------------------------------------------------------------------- /
