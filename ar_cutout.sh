program multi_proc_ncks
    use mpi
    implicit none
    integer :: ierr, my_rank, total_procs
    character(len=200) :: ncks_cmd
    integer :: final_calc = 101  ! Your calculated result

    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ierr)

    ! ... parallel calculations happen here ...

    ! Only the root task writes the command file
    if (my_rank == 0) then
        write(ncks_cmd, '("ncks -d time,", I0, " in.nc out.nc")') final_calc
        open(unit=10, file='temp_ncks.sh', status='replace')
        write(10, '(A)') trim(ncks_cmd)
        close(10)
    end if

    call MPI_FINALIZE(ierr)
end program



#!/bin/bash
#SBATCH --ntasks=64

# 1. Run the parallel program
srun ./my_mpi_executable

# 2. Run the generated command ONLY if it exists
# Since the script runs as a single shell process after srun finishes,
# it will execute ncks exactly once.
if [ -f temp_ncks.sh ]; then
    echo "Executing generated ncks command..."
    source temp_ncks.sh
    rm temp_ncks.sh  # Clean up
else
    echo "Warning: temp_ncks.sh not found (did Rank 0 fail?)"
fi
