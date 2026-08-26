  !call handlefiles(isroot, state_n%filename, 'complete', l_use_filesize, rc=rc)
subroutine handlefiles(isroot, fname, use_filesize, mode, rc=rc)

  logical,            intent(in)  :: isroot
  character, len=(*), intent(in)  :: fname
  logical,            intent(in)  :: use_filesize
  character(len=*),   intent(in)  :: mode
  !integer,            intent(out) :: rc

  !rc = 0

  select case (mode)

  case ('create')
     if (isroot) then
        call create_schema(fname)
        if (use_filesize) then
           call write_record(fname)
        else
           call write_padding(fname)
        endif
     endif

  case ('complete')
     if (isroot) then
        if (use_filesize) then
           call write_bulk_data(fname)   ! fsize grows past createsize
        else
           call write_record(fname)      ! nlen 0->1
        endif
     endif

  case('create-complete')
     if (isroot) then
        call create_schema(fname)
        if (use_filesize) then
           call write_record(fname)
           call write_bulk_data(fname)   ! fsize grows past createsize
        else
           call write_padding(fname)
           call write_record(fname)      ! nlen 0->1
        endif
     endif

  case default
     if (isroot) then
        print '(A)',' ERROR: unknown case '
        stop
     endif
  end select

end subroutine handlefiles
