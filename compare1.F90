subroutine get_trimmed_stats(arr, r_min, r_max, r_median, r_avg)
    use stdlib_sorting, only: sort
    implicit none

    ! Inputs/Outputs
    real, intent(in)  :: arr(:)
    real, intent(out) :: r_min, r_max, r_median, r_avg

    ! Internal variables
    real, allocatable :: tmp(:)
    integer :: n, n_cut, i_start, i_end, n_subset, mid

    n = size(arr)
    if (n == 0) return

    ! 1. Create a copy to sort (to keep original array intact)
    allocate(tmp(n))
    tmp = arr

    ! 2. Sort using stdlib_sorting
    call sort(tmp)

    ! 3. Calculate 2% indices
    ! n_cut is the number of elements to remove from EACH end
    n_cut = nint(n * 0.02)
    i_start = 1 + n_cut
    i_end   = n - n_cut

    ! Safety check for very small arrays
    if (i_start > i_end) then
        i_start = 1
        i_end = n
    end if

    n_subset = i_end - i_start + 1

    ! 4. Calculate Statistics
    r_min = tmp(i_start)
    r_max = tmp(i_end)
    r_avg = sum(tmp(i_start:i_end)) / real(n_subset)

    ! 5. Calculate Median of the trimmed range
    mid = i_start + (n_subset / 2)
    if (mod(n_subset, 2) == 0) then
        ! Even number of elements: average of the two middle ones
        r_median = (tmp(mid-1) + tmp(mid)) / 2.0
    else
        ! Odd number of elements
        r_median = tmp(mid)
    end if

    deallocate(tmp)
end subroutine get_trimmed_stats
