program test_module_block_data
  !! Unit test driver for module_block_data
  !! Tests block data operations across multiple grid decompositions and block sizes
  
  use ESMF,              only: ESMF_KIND_R8, ESMF_SUCCESS
  use block_control_mod, only: block_control_type, create_block_control
  use module_block_data,  only: block_data_copy, block_data_fill
  
  implicit none
  
  ! Define test configurations
  integer, parameter :: num_configs = 8
  integer, parameter :: max_grid_size = 16
  
  type :: test_config_type
    integer :: nx, ny                  !! Grid dimensions
    integer :: inpes, jnpes            !! Decomposition
    integer :: blocksize               !! Block size
    character(len=64) :: description   !! Configuration description
  end type test_config_type
  
  type(test_config_type) :: configs(num_configs)
  type(block_control_type) :: block_control
  
  integer :: config_idx, rc, test_count, test_passed
  integer :: current_test_count, current_test_passed
  
  ! Initialize test counters
  test_count = 0
  test_passed = 0
  
  print *, "=========================================="
  print *, "Unit Tests: module_block_data"
  print *, "Testing Multiple Decompositions & Block Sizes"
  print *, "=========================================="
  print *, " "
  
  ! Define test configurations
  call setup_test_configurations(configs)
  
  ! Run tests for each configuration
  do config_idx = 1, num_configs
    print *, "=========================================="
    print *, "Configuration ", config_idx, " of ", num_configs
    print *, "=========================================="
    print *, "Grid Configuration:"
    print *, "  Grid Size: ", configs(config_idx)%nx, " x ", configs(config_idx)%ny
    print *, "  Decomposition (inpes x jnpes): ", configs(config_idx)%inpes, " x ", configs(config_idx)%jnpes
    print *, "  Block Size: ", configs(config_idx)%blocksize
    print *, "  Description: ", trim(configs(config_idx)%description)
    print *, " "
    
    ! Initialize block control structure
    call initialize_block_control(block_control, &
                                   configs(config_idx)%nx, &
                                   configs(config_idx)%ny, &
                                   configs(config_idx)%inpes, &
                                   configs(config_idx)%jnpes, &
                                   configs(config_idx)%blocksize)
    
    ! Initialize per-configuration counters
    current_test_count = 0
    current_test_passed = 0
    
    ! Test 1: Block structure initialization
    current_test_count = current_test_count + 1
    call test_block_initialization(block_control, current_test_count, current_test_passed, config_idx)
    
    ! Test 2: 1D to 2D real8 copy operation
    current_test_count = current_test_count + 1
    call test_1d_to_2d_copy_r8(block_control, current_test_count, current_test_passed, config_idx)
    
    ! Test 3: 1D integer to 2D real8 copy operation
    current_test_count = current_test_count + 1
    call test_1d_to_2d_copy_i4(block_control, current_test_count, current_test_passed, config_idx)
    
    ! Test 4: 2D fill operation
    current_test_count = current_test_count + 1
    call test_2d_fill(block_control, current_test_count, current_test_passed, config_idx)
    
    ! Accumulate counts
    test_count = test_count + current_test_count
    test_passed = test_passed + current_test_passed
    
    ! Print configuration summary
    print *, "Config ", config_idx, " Results: ", current_test_passed, " / ", current_test_count, " passed"
    print *, " "
    
    ! Clean up block control
    call cleanup_block_control(block_control)
  end do
  
  ! Print overall summary
  print *, "=========================================="
  print *, "Overall Test Summary"
  print *, "=========================================="
  print *, "Total Tests Passed: ", test_passed, " / ", test_count
  print *, "=========================================="
  
  if (test_passed == test_count) then
    print *, "All tests passed!"
    stop 0
  else
    print *, "Some tests failed!"
    stop 1
  end if

contains

  !============================================================================
  ! Setup test configurations
  !============================================================================
  subroutine setup_test_configurations(configs)
    type(test_config_type), intent(out) :: configs(:)
    
    ! Configuration 1: Small 8x8 grid, 2x4 decomposition, blocksize 8
    configs(1)%nx = 8
    configs(1)%ny = 8
    configs(1)%inpes = 2
    configs(1)%jnpes = 4
    configs(1)%blocksize = 8
    configs(1)%description = "8x8, inpes=2, jnpes=4, bs=8"
    
    ! Configuration 2: 8x8 grid, 2x2 decomposition, blocksize 16
    configs(2)%nx = 8
    configs(2)%ny = 8
    configs(2)%inpes = 2
    configs(2)%jnpes = 2
    configs(2)%blocksize = 16
    configs(2)%description = "8x8, inpes=2, jnpes=2, bs=16"
    
    ! Configuration 3: 8x8 grid, 1x8 decomposition (linear, uneven X)
    configs(3)%nx = 8
    configs(3)%ny = 8
    configs(3)%inpes = 1
    configs(3)%jnpes = 8
    configs(3)%blocksize = 8
    configs(3)%description = "8x8, inpes=1, jnpes=8, bs=8 (linear in Y)"
    
    ! Configuration 4: 8x8 grid, 4x2 decomposition (uneven both ways)
    configs(4)%nx = 8
    configs(4)%ny = 8
    configs(4)%inpes = 4
    configs(4)%jnpes = 2
    configs(4)%blocksize = 8
    configs(4)%description = "8x8, inpes=4, jnpes=2, bs=8 (uneven both)"
    
    ! Configuration 5: 16x16 grid, 1x4 decomposition (linear, uneven Y)
    configs(5)%nx = 16
    configs(5)%ny = 16
    configs(5)%inpes = 1
    configs(5)%jnpes = 4
    configs(5)%blocksize = 16
    configs(5)%description = "16x16, inpes=1, jnpes=4, bs=16 (linear in Y)"
    
    ! Configuration 6: 16x16 grid, 3x3 decomposition (uneven both ways)
    configs(6)%nx = 16
    configs(6)%ny = 16
    configs(6)%inpes = 3
    configs(6)%jnpes = 3
    configs(6)%blocksize = 16
    configs(6)%description = "16x16, inpes=3, jnpes=3, bs=16 (uneven both)"
    
    ! Configuration 7: 8x8 grid, 2x6 decomposition (blocksize remainders in Y)
    configs(7)%nx = 8
    configs(7)%ny = 8
    configs(7)%inpes = 2
    configs(7)%jnpes = 6
    configs(7)%blocksize = 8
    configs(7)%description = "8x8, inpes=2, jnpes=6, bs=8 (uneven Y division)"
    
    ! Configuration 8: 16x16 grid, 2x8 decomposition (many blocks, uneven X)
    configs(8)%nx = 16
    configs(8)%ny = 16
    configs(8)%inpes = 2
    configs(8)%jnpes = 8
    configs(8)%blocksize = 16
    configs(8)%description = "16x16, inpes=2, jnpes=8, bs=16 (many blocks)"
    
  end subroutine setup_test_configurations

  !============================================================================
  ! Initialize block control structure
  !============================================================================
  subroutine initialize_block_control(block, nx, ny, inpes, jnpes, blocksize)
    type(block_control_type), intent(out) :: block
    integer, intent(in) :: nx, ny, inpes, jnpes, blocksize
    
    integer :: nblocks, iblock, jblock, block_id
    integer :: i, j, istart, jstart, iend, jend
    integer :: npts, ipt
    integer :: bx, by  ! block width and height within this MPI task
    integer :: block_width, block_height
    
    ! This subroutine simulates a SINGLE MPI task's view of the domain
    ! The full global domain is (nx*inpes) x (ny*jnpes)
    ! This task is responsible for nx x ny portion
    
    ! Calculate number of blocks within this MPI task
    nblocks = inpes * jnpes
    
    ! Allocate block control arrays
    allocate(block%blksz(nblocks))
    allocate(block%index(nblocks))
    allocate(block%index(1)%ii(blocksize))
    allocate(block%index(1)%jj(blocksize))
    
    ! Set local domain bounds for this MPI task
    block%isc = 1
    block%iec = nx
    block%jsc = 1
    block%jec = ny
    block%ni = nx
    block%nj = ny
    block%nblocks = nblocks
    
    ! Create blocks in a row-major order within this MPI task's domain
    block_id = 1
    do jblock = 1, jnpes
      do iblock = 1, inpes
        ! Calculate block bounds within this task's local domain
        istart = (iblock - 1) * (nx / inpes) + 1
        iend = min(iblock * (nx / inpes), nx)
        jstart = (jblock - 1) * (ny / jnpes) + 1
        jend = min(jblock * (ny / jnpes), ny)
        
        ! Number of points in this block
        npts = (iend - istart + 1) * (jend - jstart + 1)
        block%blksz(block_id) = npts
        
        ! Allocate index arrays for this block
        if (block_id > 1) then
          allocate(block%index(block_id)%ii(npts))
          allocate(block%index(block_id)%jj(npts))
        end if
        
        ! Populate local domain indices for this block
        ipt = 1
        do j = jstart, jend
          do i = istart, iend
            block%index(block_id)%ii(ipt) = i
            block%index(block_id)%jj(ipt) = j
            ipt = ipt + 1
          end do
        end do
        
        block_id = block_id + 1
      end do
    end do
    
    print *, "Block Control Initialized (MPI task local domain):"
    print *, "  Local domain: isc=", block%isc, " iec=", block%iec, " jsc=", block%jsc, " jec=", block%jec
    print *, "  Number of blocks: ", nblocks
    print *, "  Block sizes range from: 1 to ", maxval(block%blksz)
    
  end subroutine initialize_block_control
  
  !============================================================================
  ! Clean up block control structure
  !============================================================================
  subroutine cleanup_block_control(block)
    type(block_control_type), intent(inout) :: block
    integer :: i
    
    if (allocated(block%blksz)) deallocate(block%blksz)
    if (allocated(block%index)) then
      do i = 1, size(block%index)
        if (allocated(block%index(i)%ii)) deallocate(block%index(i)%ii)
        if (allocated(block%index(i)%jj)) deallocate(block%index(i)%jj)
      end do
      deallocate(block%index)
    end if
    
  end subroutine cleanup_block_control
  
  !============================================================================
  ! TEST 1: Block initialization
  !============================================================================
  subroutine test_block_initialization(block, test_num, passed_count, config_idx)
    type(block_control_type), intent(in) :: block
    integer, intent(in) :: test_num, config_idx
    integer, intent(inout) :: passed_count
    
    integer :: total_pts, computed_total
    integer :: i
    
    print *, "  [Config ", config_idx, "] Test ", test_num, ": Block Structure Initialization"
    
    ! Check that block size array is properly initialized
    if (.not. allocated(block%blksz)) then
      print *, "    FAILED: blksz array not allocated"
      return
    end if
    
    ! Check grid dimensions
    if (block%ni /= block%iec - block%isc + 1 .or. block%nj /= block%jec - block%jsc + 1) then
      print *, "    FAILED: Grid dimensions incorrect"
      return
    end if
    
    ! Check that total points match grid size
    total_pts = (block%iec - block%isc + 1) * (block%jec - block%jsc + 1)
    computed_total = sum(block%blksz)
    if (computed_total /= total_pts) then
      print *, "    FAILED: Total points mismatch"
      print *, "      Expected: ", total_pts, " Computed: ", computed_total
      return
    end if
    
    ! Check that global indices are within domain bounds
    do i = 1, block%nblocks
      if (minval(block%index(i)%ii) < block%isc) then
        print *, "    FAILED: I index below isc in block ", i
        return
      end if
      if (maxval(block%index(i)%ii) > block%iec) then
        print *, "    FAILED: I index exceeds iec in block ", i
        return
      end if
      if (minval(block%index(i)%jj) < block%jsc) then
        print *, "    FAILED: J index below jsc in block ", i
        return
      end if
      if (maxval(block%index(i)%jj) > block%jec) then
        print *, "    FAILED: J index exceeds jec in block ", i
        return
      end if
    end do
    
    print *, "    PASSED"
    passed_count = passed_count + 1
    
  end subroutine test_block_initialization
  
  !============================================================================
  ! TEST 2: 1D to 2D real8 copy
  !============================================================================
  subroutine test_1d_to_2d_copy_r8(block, test_num, passed_count, config_idx)
    type(block_control_type), intent(in) :: block
    integer, intent(in) :: test_num, config_idx
    integer, intent(inout) :: passed_count
    
    real(ESMF_KIND_R8), allocatable :: grid_dest(:,:)
    real(ESMF_KIND_R8), allocatable :: source_1d(:)
    real(ESMF_KIND_R8) :: scale_factor
    integer :: i, rc, total_pts, block_id, ipt, idx_i, idx_j
    logical :: test_pass
    
    print *, "  [Config ", config_idx, "] Test ", test_num, ": 1D to 2D Real8 Copy Operation"
    
    ! Calculate grid dimensions
    total_pts = (block%iec - block%isc + 1) * (block%jec - block%jsc + 1)
    
    ! Allocate arrays
    allocate(grid_dest(block%isc:block%iec, block%jsc:block%jec))
    allocate(source_1d(total_pts))
    
    ! Initialize source array with sequential values
    do i = 1, total_pts
      source_1d(i) = real(i, ESMF_KIND_R8)
    end do
    
    ! Initialize destination with zeros
    grid_dest = 0.0_ESMF_KIND_R8
    
    ! Apply scale factor
    scale_factor = 2.0_ESMF_KIND_R8
    
    ! Copy data for each block
    test_pass = .true.
    do block_id = 1, block%nblocks
      call block_data_copy(grid_dest, source_1d, block, block_id, scale_factor, 1, rc)
      if (rc /= ESMF_SUCCESS) then
        print *, "    FAILED: block_data_copy returned rc=", rc, " for block ", block_id
        test_pass = .false.
        exit
      end if
    end do
    
    if (test_pass) then
      ! Verify results by checking a few values
      do i = 1, min(10, block%blksz(1))
        idx_i = block%index(1)%ii(i)
        idx_j = block%index(1)%jj(i)
        if (abs(grid_dest(idx_i, idx_j) - (real(i, ESMF_KIND_R8) * scale_factor)) > 1.0e-10_ESMF_KIND_R8) then
          print *, "    FAILED: Data value mismatch at block point ", i
          test_pass = .false.
          exit
        end if
      end do
    end if
    
    if (test_pass) then
      print *, "    PASSED"
      passed_count = passed_count + 1
    else
      print *, "    FAILED: 1D to 2D copy verification failed"
    end if
    
    deallocate(grid_dest, source_1d)
    
  end subroutine test_1d_to_2d_copy_r8
  
  !============================================================================
  ! TEST 3: 1D integer to 2D real8 copy
  !============================================================================
  subroutine test_1d_to_2d_copy_i4(block, test_num, passed_count, config_idx)
    type(block_control_type), intent(in) :: block
    integer, intent(in) :: test_num, config_idx
    integer, intent(inout) :: passed_count
    
    real(ESMF_KIND_R8), allocatable :: grid_dest(:,:)
    integer, allocatable :: source_i4(:)
    integer :: i, rc, block_id, ipt, idx_i, idx_j, total_pts
    logical :: test_pass
    
    print *, "  [Config ", config_idx, "] Test ", test_num, ": 1D Integer to 2D Real8 Copy Operation"
    
    ! Calculate grid dimensions
    total_pts = (block%iec - block%isc + 1) * (block%jec - block%jsc + 1)
    
    ! Allocate arrays
    allocate(grid_dest(block%isc:block%iec, block%jsc:block%jec))
    allocate(source_i4(total_pts))
    
    ! Initialize source array with sequential integer values
    do i = 1, total_pts
      source_i4(i) = i * 10
    end do
    
    ! Initialize destination with zeros
    grid_dest = 0.0_ESMF_KIND_R8
    
    ! Copy data for each block
    test_pass = .true.
    do block_id = 1, block%nblocks
      call block_data_copy(grid_dest, source_i4, block, block_id, offset=1, rc=rc)
      if (rc /= ESMF_SUCCESS) then
        print *, "    FAILED: block_data_copy (i4) returned rc=", rc, " for block ", block_id
        test_pass = .false.
        exit
      end if
    end do
    
    if (test_pass) then
      ! Verify results
      do i = 1, min(10, block%blksz(1))
        idx_i = block%index(1)%ii(i)
        idx_j = block%index(1)%jj(i)
        if (abs(grid_dest(idx_i, idx_j) - real(i * 10, ESMF_KIND_R8)) > 1.0e-10_ESMF_KIND_R8) then
          print *, "    FAILED: Data value mismatch at point ", i
          test_pass = .false.
          exit
        end if
      end do
    end if
    
    if (test_pass) then
      print *, "    PASSED"
      passed_count = passed_count + 1
    else
      print *, "    FAILED: Integer to real8 copy verification failed"
    end if
    
    deallocate(grid_dest, source_i4)
    
  end subroutine test_1d_to_2d_copy_i4
  
  !============================================================================
  ! TEST 4: 2D fill operation
  !============================================================================
  subroutine test_2d_fill(block, test_num, passed_count, config_idx)
    type(block_control_type), intent(in) :: block
    integer, intent(in) :: test_num, config_idx
    integer, intent(inout) :: passed_count
    
    real(ESMF_KIND_R8), allocatable :: grid_data(:,:)
    real(ESMF_KIND_R8) :: fill_value
    integer :: rc, block_id
    logical :: test_pass
    
    print *, "  [Config ", config_idx, "] Test ", test_num, ": 2D Fill Operation"
    
    ! Allocate and initialize grid
    allocate(grid_data(block%isc:block%iec, block%jsc:block%jec))
    grid_data = 0.0_ESMF_KIND_R8
    
    ! Fill value
    fill_value = 99.0_ESMF_KIND_R8
    
    ! Fill each block with the fill value
    test_pass = .true.
    do block_id = 1, block%nblocks
      call block_data_fill(grid_data, block, block_id, fill_value, rc)
      if (rc /= ESMF_SUCCESS) then
        print *, "    FAILED: block_data_fill returned rc=", rc, " for block ", block_id
        test_pass = .false.
        exit
      end if
    end do
    
    if (test_pass) then
      ! Verify that all values were filled
      if (all(abs(grid_data - fill_value) < 1.0e-10_ESMF_KIND_R8)) then
        print *, "    PASSED"
        passed_count = passed_count + 1
      else
        print *, "    FAILED: Not all grid points filled correctly"
      end if
    end if
    
    deallocate(grid_data)
    
  end subroutine test_2d_fill

end program test_module_block_data
