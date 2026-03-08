subroutine get_spike_report_stats(arr, r_min, r_max, r_median, r_avg, n_spikes)
    use stdlib_sorting, only: sort
    implicit none

    ! Inputs/Outputs
    real, intent(in)  :: arr(:)
    real, intent(out) :: r_min, r_max, r_median, r_avg
    integer, intent(out) :: n_spikes

    ! Internal variables
    real, allocatable    :: tmp(:), clean(:)
    logical, allocatable :: mask(:)
    real    :: q1, q3, iqr, lower_f, upper_f
    integer :: n, n_clean, mid

    n = size(arr)
    n_spikes = 0
    if (n == 0) return

    ! 1. Sort a copy for IQR calculation
    allocate(tmp(n))
    tmp = arr
    call sort(tmp)

    if (n >= 4) then
        ! Calculate Extreme Outlier Fences (3.0 * IQR)
        q1 = tmp(max(1, nint(0.25 * n)))
        q3 = tmp(max(1, nint(0.75 * n)))
        iqr = q3 - q1
        lower_f = q1 - (3.0 * iqr)
        upper_f = q3 + (3.0 * iqr)

        ! 2. Create mask and count spikes
        allocate(mask(n))
        mask = (tmp >= lower_f) .and. (tmp <= upper_f)

        ! Number of spikes is total size minus number of values that pass the mask
        n_spikes = n - count(mask)

        ! 3. Extract inliers
        clean = pack(tmp, mask)
    else
        ! Small array fallback: no spikes removed
        clean = tmp
        n_spikes = 0
    end if

    n_clean = size(clean)

    ! 4. Compute Statistics
    if (n_clean > 0) then
        r_min = clean(1)
        r_max = clean(n_clean)
        r_avg = sum(clean) / real(n_clean)

        mid = n_clean / 2 + 1
        if (mod(n_clean, 2) == 0) then
            r_median = (clean(mid-1) + clean(mid)) / 2.0
        else
            r_median = clean(mid)
        end if
    end if

    if (allocated(tmp)) deallocate(tmp)
    if (allocated(mask)) deallocate(mask)
end subroutine get_spike_report_stats
