subroutine zero_out(mask, field)

  integer, intent(in)         :: mask(:,:)
  real(kind=8), intent(inout) :: field(:,:)
  integer, intent(out)        :: icnt
  
  !local variables
  integer :: icnt, , ij, k

  icnt = 0
  do ij = 1,size(field,2)
     if ( mask(ij) .eq. 0 .and. sum(field1(:,ij)) .ne. 0.0)  then
        icnt = icnt + 1
        field(:,ij) = 0.0
     end if
  end do

  !do ij = 1,size(mask,2)
   !  do k = 1,size(mask,1)        
   !     if (mask(k,ij) .eq. 0 .and. field(k,ij) .ne. 0.0) then
   !        field(k,ij) = 0.0
   !        icnt = icnt + 1
   !     end if
   !  end do
  !end do

end subroutine zero_out
! on return
print (logfile,'(a,i8)') 'removed ',icnt,' locations of variable ',trim(name),' on land '

subroutine phantom_ice(field1, field2, icnt)

  real(kind=8), intent(in)    :: field1(:,:)
  real(kind=8), intent(inout) :: field2(:,:)
  integer, intent(out)        :: icnt

  !local variables
  integer :: ij, k

  icnt = 0
  do ij = 1,size(field1,2)
     if (sum(field1(:,ij) .eq. 0.0) .and. sum(field2(:,ij) .ne. 0.0) ) then
        icnt = icnt + 1
        field2(:,ij) = 0.0
     end if
  end do
  
end subroutine phantom_ice
  
