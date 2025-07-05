subroutine initialize_steam(sdatm, sdats, sid, sdatname, compname, mytask, logunit, rc)

  ! intialize a single stream
  !type...(inout) :: sdatm
  !   type..(inout) :: sdats


  nfiles = sdatm%stream(sid)%nfiles
  nvars = sdatm%stream(sid)%nvars

  allocate(filelist(1:nfiles))
  allocate(streamfilevars(1:nvars,2))

  do nf = 1,nfiles
     filelist(nf) = trim(sdatm%stream(sid)%file(nf)%name)
     if (mytask == 0) print *,'XX1 ',nf,trim(filelist(nf))
  end do
  do nv = 1,nvars
     filevars(nv,1) = trim(sdatm%stream(sid)%varlist(nv)%nameinfile)
     filevars(nv,2) = trim(sdatm%stream(sid)%varlist(nv)%nameinmodel)
     if (mytask == 0) print *,'XX1 ',nv,trim(streamfilevars(nv,1)),' ',trim(streamfilevars(nv,2))
  end do

  ! Set PIO related variables
  sdats%pio_subsystem => sdatm%pio_subsystem
  sdats%io_type = sdatm%io_type
  sdats%io_format = sdatm%io_format

  call shr_strdata_init_from_inline(sdats,                        &
       my_task             = mytask,                              &
       logunit             = logunit,                             &
       compname            = trim(compname)                       &
       model_clock         = model_clock,                         &
       model_mesh          = model_mesh,                          &
       stream_name         = trim(sdatname)                       &
       stream_meshfile     = trim(sdatm%stream(sid)%meshFile),    &
       stream_filenames    = filelist,                            &
       stream_yearFirst    = sdatm%stream(sid)%yearFirst,         &
       stream_yearLast     = sdatm%stream(sid)%yearLast,          &
       stream_yearAlign    = sdatm%stream(sid)%yearAlign,         &
       stream_fldlistFile  = filevars(:,1),                       &
       stream_fldListModel = filevars(:,2),                       &
       stream_lev_dimname  = trim(sdatm%stream(sid)%lev_dimname), &
       stream_mapalgo      = trim(sdatm%stream(sid)%mapAlgo),     &
       stream_offset       = sdatm%stream(sid)%offset,            &
       stream_taxmode      = trim(sdatm%stream(sid)%taxmode),     &
       stream_dtlimit      = sdatm%stream(sid)%dtlimit,           &
       stream_tintalgo     = trim(sdatm%stream(sid)%tInterpAlgo), &
       stream_src_mask     = sdatm%stream(sid)%src_mask_val,      &
       stream_dst_mask     = sdatm%stream(sid)%dst_mask_val,      &
       rc                  = rc)
  if (chkerr(rc,__LINE__,u_FILE_u)) return
end if
  deallocate(filelist)
  deallocate(filevars)


end subroutine intialize_stream
