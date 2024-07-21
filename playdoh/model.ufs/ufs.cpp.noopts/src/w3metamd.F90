!> @file
!> @brief dynamic storage for meta data attribute/value pairs.
!> @author chris bunney @date 16-dec-2020
!/ ------------------------------------------------------------------- /
!/
!> @brief dynamic storage for meta data attribute/value pairs.
!>
!> @details provides types for handling "meta data" (attribute/value pairs)
!>     and a linked list construct for dynamic storage.
!>
!> ### change log
!>   date      | ver  | comments
!> ------------|------|---------
!> 16-dec-2020 | 7.12 | creation
!>
!> @author chris bunney @date 16-dec-2020
!>
!/ ------------------------------------------------------------------- /
module w3metamd
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
  !  1. purpose :
  !
  !     provides types for handling "meta data" (attribute/value pairs)
  !     and a linked list construct for dynamic storage.
  !/ ------------------------------------------------------------------- /
  !> value to represent "unset" character variable
  character(len=*), parameter :: unsetc = "unset"
  !> value to represent "unset" real variable
  real, parameter             :: unsetr = huge(1.0)
  !> type for storing a user defined metadata pair as linked list element
  type meta_pair_t
    character(len=64)  :: attname = unsetc  !< attribute name
    character(len=120) :: attval = unsetc   !< attribute value
    character          :: type = 'c'        !< attribute type (c,i,f/r)
    type(meta_pair_t), pointer  :: next     !< pointer to next node
  end type meta_pair_t
  !> linked list of meta data pairs
  type meta_list_t
    type (meta_pair_t), pointer :: head => null(), tail => null()
    integer                     :: n = 0    !< num elements in list
  end type meta_list_t
  !> interface to facilitate adding real/int/character values to list
  interface meta_list_append
    module procedure meta_list_append_m !< append a meta_pair_t
    module procedure meta_list_append_r !< append a real value
    module procedure meta_list_append_i !< append an integer value
    module procedure meta_list_append_c !< append a character value
  end interface meta_list_append
contains
  !/ ------------------------------------------------------------------- /
  !> @brief deletes all entries in list.
  !>
  !> @param[in,out] list the list to clear.
  !>
  !> @author chris bunney
  !/ ------------------------------------------------------------------- /
  subroutine del_meta_list(list)
    implicit none
    type(meta_list_t), intent(inout) :: list
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    type(meta_pair_t), pointer :: p
    if(.not. associated(list%head)) return
    do
      nullify(p)
      if(associated(list%head%next)) p => list%head%next
      deallocate(list%head)
      if(.not. associated(p)) exit
      list%head => p
    enddo
    nullify(list%head)
    nullify(list%tail)
    list%n = 0
  end subroutine del_meta_list
  !/ ------------------------------------------------------------------- /
  !> @brief create a deep copy of a meta data list
  !>
  !> @details a "deep copy" ensures that a copy is made of the underlying
  !>    linked list, rather than a simply copy of the pointers to the
  !>    existing list.
  !>
  !> @param[in] list the list to copy
  !>
  !> @returns a new meta_list_t
  !>
  !> @author chris bunney
  !>
  !/ ------------------------------------------------------------------- /
  function copy_meta_list(list) result(copy)
    implicit none
    type(meta_list_t), intent(in) :: list
    type(meta_list_t)             :: copy
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    type(meta_pair_t), pointer :: p
    nullify(copy%head)
    nullify(copy%tail)
    copy%n = 0
    if(list%n .eq. 0) return
    ! deep copy list
    p => list%head
    do
      call meta_list_append_m(copy, p)
      if(.not. associated(p%next)) exit
      p => p%next
    enddo
  end function copy_meta_list
  !/ ------------------------------------------------------------------- /
  !> @brief prints meta pairs in list to screen
  !>
  !> @param[in] list  linked list of meta data to print
  !>
  !> @author chris bunney
  !/ ------------------------------------------------------------------- /
  subroutine print_meta_list(list)
    implicit none
    type(meta_list_t), intent(in) :: list
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    type(meta_pair_t), pointer :: p
    if(.not. associated(list%head)) then
      write(*,*) 'list empty.'
      return
    endif
    p => list%head
    do
      write(*, '(a," [",a1,"] : ", a)') trim(p%attname), p%type,      &
           trim(p%attval)
      if(.not. associated(p%next)) exit
      p => p%next
    enddo
  end subroutine print_meta_list
  !/ ------------------------------------------------------------------- /
  !> @brief append meta_pair_t object to list
  !>
  !> @param[in,out] list  the list to append to
  !> @param[in]     meta  the meta_pair_t object to append.
  !>
  !> @author chris bunney
  !/ ------------------------------------------------------------------- /
  subroutine meta_list_append_m(list, meta)
    implicit none
    type(meta_list_t), intent(inout) :: list
    type(meta_pair_t), intent(in) :: meta
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    type(meta_pair_t), pointer :: p
    allocate(p)
    ! empty list?
    if(list%n .eq. 0) then
      !if(.not. associated(list%head)) then
      list%head => p
    else
      list%tail%next => p
    endif
    list%tail => p
    p%attname = meta%attname
    p%attval = meta%attval
    p%type = meta%type
    nullify(p%next)
    list%n = list%n + 1
  end subroutine meta_list_append_m
  !/ ------------------------------------------------------------------- /
  !> @brief append real value attribute to list
  !>
  !> @param[in,out] list     the list to append to
  !> @param[in]     attname  the attribute name
  !> @param[in]     rval     the attribute value (real)
  !>
  !> @author chris bunney
  !/ ------------------------------------------------------------------- /
  subroutine meta_list_append_r(list, attname, rval)
    implicit none
    type(meta_list_t), intent(inout) :: list
    character(*), intent(in)         :: attname
    real, intent(in)                 :: rval
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    type(meta_pair_t) :: meta
    meta%attname = attname
    write(meta%attval,*) rval
    meta%type = 'r'
    call meta_list_append(list, meta)
  end subroutine meta_list_append_r
  !/ ------------------------------------------------------------------- /
  !> @brief append integer value attribute to list
  !>
  !> @param[in,out] list     the list to append to
  !> @param[in]     attname  the attribute name
  !> @param[in]     ival     the attribute value (integer)
  !>
  !> @author chris bunney
  !/ ------------------------------------------------------------------- /
  subroutine meta_list_append_i(list, attname, ival)
    implicit none
    type(meta_list_t), intent(inout) :: list
    character(*), intent(in)         :: attname
    integer, intent(in)              :: ival
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    type(meta_pair_t) :: meta
    meta%attname = attname
    write(meta%attval,*) ival
    meta%type = 'i'
    call meta_list_append(list, meta)
  end subroutine meta_list_append_i
  !/ ------------------------------------------------------------------- /
  !> @brief append character string value attribute to list
  !>
  !> @param[in,out] list     the list to append to
  !> @param[in]     attname  the attribute name
  !> @param[in]     sval     the attribute value (character string)
  !>
  !> @author chris bunney
  !/ ------------------------------------------------------------------- /
  subroutine meta_list_append_c(list, attname, sval)
    implicit none
    type(meta_list_t), intent(inout) :: list
    character(*), intent(in)         :: attname, sval
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    type(meta_pair_t) :: meta
    meta%attname = attname
    meta%attval = sval
    meta%type = 'c'
    call meta_list_append(list, meta)
  end subroutine meta_list_append_c
  !/ ------------------------------------------------------------------- /
  !> @brief find (first) entry in list with matching attname
  !>
  !> @param[in]   list   list to search
  !> @param[in]   attn   attribute name to search for
  !> @param[out]  meta   meta data type to store matched result in
  !> @param[out]  err    error status (0=found, 1=empty list, 2=not found)
  !>
  !> @author chris bunney
  !/ ------------------------------------------------------------------- /
  subroutine meta_list_find_attr(list, attn, meta, err)
    implicit none
    type(meta_list_t), intent(in)           :: list
    character(*), intent(in)                :: attn
    type(meta_pair_t), pointer, intent(out) :: meta
    integer, intent(out)                    :: err
    err = 0
    ! empty list?
    if(.not. associated(list%head)) then
      err = 1
      return
    endif
    meta => list%head
    do
      if(trim(meta%attname) == trim(attn)) return
      if(.not. associated(meta%next)) exit
      meta => meta%next
    enddo
    ! not found
    nullify(meta)
    err = 2
  end subroutine meta_list_find_attr
  !/ ------------------------------------------------------------------- /
  !> @brief tests whether list contains an entry with specified attname
  !>
  !> @param[in]  list  the list to search
  !> @param[in]  attn  attribute name to search for
  !>
  !> @returns logical: true if match found, false otherwise.
  !>
  !> @author chris bunney
  !/ ------------------------------------------------------------------- /
  function meta_list_has_attr(list, attn) result(found)
    implicit none
    type(meta_list_t), intent(in)           :: list
    character(*), intent(in)                :: attn
    logical                                 :: found
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    type(meta_pair_t), pointer :: p
    found = .false.
    ! empty list?
    if(.not. associated(list%head)) then
      return
    endif
    p => list%head
    do
      if(trim(p%attname) == trim(attn)) then
        found = .true.
        return
      endif
      if(.not. associated(p%next)) exit
      p => p%next
    enddo
  end function meta_list_has_attr
  !/ ------------------------------------------------------------------- /
end module w3metamd
!/ ------------------------------------------------------------------- /
