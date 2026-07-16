
        tmpname = "MOM6_OUTPUT/ocn_2021_03_22_06_30.nc"
        importexport = get_importexport(currTime, nextTime, rc=rc)
        call get_file_state(mpicomm, is_root_pe(), root_pe(), trim(tmpname), nlen=nlen, &
             fsize=fsize, rc=rc)
        rc = merge(ESMF_SUCCESS, ESMF_FAILURE, rc == 0)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        if(is_root_pe()) then
          inquire(file=trim(tmpname), exist=existflag)
          if (existflag) then
            print '(a,i6,i16)','XXX  '//trim(tmpname)//' exist '//trim(importexport)//' ',nlen,fsize
          endif
        endif

        tmpname = "MOM6_OUTPUT/ocn_2021_03_22_07_00.nc"
        importexport = get_importexport(currTime, nextTime, rc=rc)
        call get_file_state(mpicomm, is_root_pe(), root_pe(), trim(tmpname), nlen=nlen, &
             fsize=fsize, rc=rc)
        rc = merge(ESMF_SUCCESS, ESMF_FAILURE, rc == 0)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        if(is_root_pe()) then
          inquire(file=trim(tmpname), exist=existflag)
          if (existflag) then
            print '(a,i6,i16)','XXX  '//trim(tmpname)//' exist '//trim(importexport)//' ',nlen,fsize
          endif
        endif

        tmpname = "MOM6_OUTPUT/ocn_2021_03_22_07_30.nc"
        importexport = get_importexport(currTime, nextTime, rc=rc)
        call get_file_state(mpicomm, is_root_pe(), root_pe(), trim(tmpname), nlen=nlen, &
             fsize=fsize, rc=rc)
        rc = merge(ESMF_SUCCESS, ESMF_FAILURE, rc == 0)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        if(is_root_pe()) then
          inquire(file=trim(tmpname), exist=existflag)
          if (existflag) then
            print '(a,i6,i16)','XXX  '//trim(tmpname)//' exist '//trim(importexport)//' ',nlen,fsize
          endif
        endif

        tmpname = "MOM6_OUTPUT/ocn_2021_03_22_08_00.nc"
        importexport = get_importexport(currTime, nextTime, rc=rc)
        call get_file_state(mpicomm, is_root_pe(), root_pe(), trim(tmpname), nlen=nlen, &
             fsize=fsize, rc=rc)
        rc = merge(ESMF_SUCCESS, ESMF_FAILURE, rc == 0)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        if(is_root_pe()) then
          inquire(file=trim(tmpname), exist=existflag)
          if (existflag) then
            print '(a,i6,i16)','XXX  '//trim(tmpname)//' exist '//trim(importexport)//' ',nlen,fsize
          endif
        endif

        tmpname = "MOM6_OUTPUT/ocn_2021_03_22_08_30.nc"
        importexport = get_importexport(currTime, nextTime, rc=rc)
        call get_file_state(mpicomm, is_root_pe(), root_pe(), trim(tmpname), nlen=nlen, &
             fsize=fsize, rc=rc)
        rc = merge(ESMF_SUCCESS, ESMF_FAILURE, rc == 0)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        if(is_root_pe()) then
          inquire(file=trim(tmpname), exist=existflag)
          if (existflag) then
            print '(a,i6,i16)','XXX  '//trim(tmpname)//' exist '//trim(importexport)//' ',nlen,fsize
          endif
        endif

        tmpname = "MOM6_OUTPUT/ocn_2021_03_22_09_00.nc"
        importexport = get_importexport(currTime, nextTime, rc=rc)
        call get_file_state(mpicomm, is_root_pe(), root_pe(), trim(tmpname), nlen=nlen, &
             fsize=fsize, rc=rc)
        rc = merge(ESMF_SUCCESS, ESMF_FAILURE, rc == 0)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        if(is_root_pe()) then
          inquire(file=trim(tmpname), exist=existflag)
          if (existflag) then
            print '(a,i6,i16)','XXX  '//trim(tmpname)//' exist '//trim(importexport)//' ',nlen,fsize
          endif
        endif

        tmpname = "MOM6_OUTPUT/ocn_2021_03_22_09_30.nc"
        importexport = get_importexport(currTime, nextTime, rc=rc)
        call get_file_state(mpicomm, is_root_pe(), root_pe(), trim(tmpname), nlen=nlen, &
             fsize=fsize, rc=rc)
        rc = merge(ESMF_SUCCESS, ESMF_FAILURE, rc == 0)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        if(is_root_pe()) then
          inquire(file=trim(tmpname), exist=existflag)
          if (existflag) then
            print '(a,i6,i16)','XXX  '//trim(tmpname)//' exist '//trim(importexport)//' ',nlen,fsize
          endif
        endif
