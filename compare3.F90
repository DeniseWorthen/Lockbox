module mapping_validator
    use stdlib_sorting, only: sort
    implicit none

contains

    subroutine validate_mapping(name, src, map, rel_tol, abs_tol, med_tol)
        character(len=*), intent(in) :: name
        real, intent(in)             :: src(:), map(:)
        real, intent(in)             :: rel_tol  ! e.g., 0.02 (2% range error)
        real, intent(in)             :: abs_tol  ! e.g., 1e-7 (absolute floor)
        real, intent(in)             :: med_tol  ! e.g., 0.05 (5% of spread shift)

        real    :: s_min, s_max, s_med, s_avg, s_iqr
        real    :: m_min, m_max, m_med, m_avg, m_iqr
        real    :: range_diff, range_thresh, med_shift, med_thresh
        integer :: s_spikes, m_spikes

        ! 1. Get stats for both
        call get_spike_stats(src, s_min, s_max, s_med, s_avg, s_iqr, s_spikes)
        call get_spike_stats(map, m_min, m_max, m_med, m_avg, m_iqr, m_spikes)

        ! 2. Range Validation (Scaling Check)
        range_diff   = abs((m_max - m_min) - (s_max - s_min))
        range_thresh = abs_tol + (rel_tol * (s_max - s_min))

        ! 3. Median Validation (Bias/Offset Check)
        ! We check if the median moved by more than 'med_tol' relative to the IQR
        med_shift  = abs(m_med - s_med)
        med_thresh = abs_tol + (med_tol * s_iqr)

        print "(A, A15, A, I3, A, I3, A, F8.4, A, F8.4)", &
            "[TEST] ", name, " | Spikes:", s_spikes, "/", m_spikes, &
            " | R_Diff:", range_diff, " | M_Shift:", med_shift

        ! 4. Hard Stops
        if (range_diff > range_thresh) then
            print *, "FAIL: Range mismatch for ", name
            error stop "Range Error Exceeded Tolerance"
        end if

        if (med_shift > med_thresh) then
            print *, "FAIL: Median bias detected for ", name
            print *, "Source Med:", s_med, " Mapped Med:", m_med
            error stop "Median Shift Exceeded Tolerance"
        end if

    end subroutine validate_mapping

    subroutine get_spike_stats(arr, r_min, r_max, r_med, r_avg, r_iqr, n_spikes)
        real, intent(in)  :: arr(:)
        real, intent(out) :: r_min, r_max, r_med, r_avg, r_iqr
        integer, intent(out) :: n_spikes
        real, allocatable :: tmp(:), clean(:)
        logical, allocatable :: mask(:)
        integer :: n, n_c

        n = size(arr)
        allocate(tmp(n)); tmp = arr; call sort(tmp)

        ! IQR logic
        r_iqr = tmp(nint(0.75*n)) - tmp(nint(0.25*n))

        allocate(mask(n))
        ! 3.0 x IQR to define spikes
        mask = (tmp >= tmp(nint(0.25*n)) - 3.0*r_iqr) .and. &
               (tmp <= tmp(nint(0.75*n)) + 3.0*r_iqr)

        n_spikes = n - count(mask)
        clean = pack(tmp, mask)
        n_c = size(clean)

        r_min = clean(1)
        r_max = clean(n_c)
        r_avg = sum(clean) / real(n_c)
        r_med = clean(n_c/2 + 1)

        deallocate(tmp, mask, clean)
    end subroutine get_spike_stats

end module mapping_validator


subroutine validate_mapping(name, src, map, rel_in, abs_in, med_in)
    real, intent(in), optional :: rel_in, abs_in, med_in
    real :: r_t, a_t, m_t

    ! Use provided value or a sensible default
    r_t = 0.01 ; if (present(rel_in)) r_t = rel_in
    a_t = 1e-7 ; if (present(abs_in)) a_t = abs_in
    m_t = 0.05 ; if (present(med_in)) m_t = med_in

    ! ... rest of logic using r_t, a_t, m_t ...
  end subroutine validate_mapping



  ! Standard defaults for most geophysical/engineering data
  !call validate_mapping("Temp", src, map, rel_tol=0.01, abs_tol=1e-6, med_tol=0.02)
