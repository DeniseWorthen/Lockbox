# MOM Output Logging Refactor Handoff

## Goal
Refactor `mom_cap_outputlog.F90` so output logging supports:
- namelist-driven configuration instead of `NUOPC_CompAttributeGet`
- layout-partitioned history output
- both averaged and instantaneous output modes
- callable helper routines instead of inline control-flow-heavy logic
- unit-testable logic for filename construction and completeness aggregation

## Agreed Design Decisions
- Read namelist on root PE, normalize/validate on root, then broadcast parsed config to all PEs.
- Keep the existing per-part completeness semantics:
  - unlimited dimension complete when `nlen > 0`
  - if the file had nonzero unlimited dimension at creation, also require `filesize > createsize`
- For layout-partitioned history output, use the restart-style aggregation pattern:
  - build the expected list of part files
  - test each part independently
  - declare the logical output complete only when `all(allDone)` is true
- Prefer data-driven output definitions over hard-coded branches.
- Prefer callable helper routines so logic can be unit-tested without running the full cap.

## Proposed Derived Types

```fortran
integer, parameter :: MODE_AVERAGE = 1
integer, parameter :: MODE_INSTANT = 2

integer, parameter :: TIME_RULE_INTERVAL_MIDPOINT = 1
integer, parameter :: TIME_RULE_VALID_TIME        = 2
integer, parameter :: TIME_RULE_PREV_RING         = 3

type :: output_spec_type
  character(len=32)        :: name
  character(len=32)        :: alarm_name
  character(len=256)       :: output_dir
  character(len=64)        :: file_prefix
  integer                  :: frequency_hours
  integer                  :: mode
  integer                  :: time_rule
  logical                  :: layout_enabled
  integer                  :: layout_x
  integer                  :: layout_y
  integer                  :: nparts
  logical                  :: track_filesize
  type(ESMF_TimeInterval)  :: fhoffset
  type(ESMF_TimeInterval)  :: filename_fhoffset
end type output_spec_type

type :: output_state_type
  logical                  :: chkfile_nextAdvance
  character(len=256)       :: basename
  character(len=256), allocatable :: filenames(:)
  integer, allocatable     :: createsize(:)
  logical, allocatable     :: use_filesize(:)
  logical, allocatable     :: part_done(:)
  type(ESMF_Time)          :: time_lastrestart
end type output_state_type

type :: output_part_status_type
  logical :: exists
  integer :: nlen
  integer :: createsize
  integer :: currsize
end type output_part_status_type
```

## Proposed Module-Level State

```fortran
type(output_spec_type), allocatable  :: output_specs(:)
type(output_state_type), allocatable :: output_states(:)
integer                               :: n_outputs
logical                               :: debug
character(len=256)                    :: restartdir
```

Notes:
- `output_spec_type` is static configuration.
- `output_state_type` is mutable runtime state.
- `output_part_status_type` exists to decouple file probing from completion decisions, which is useful for tests.

## Proposed Helper API

### Configuration and setup

```fortran
subroutine read_outputlog_namelist(nml_path, specs, n_specs, debug, restartdir, rc)
  character(len=*), intent(in)                 :: nml_path
  type(output_spec_type), allocatable, intent(out) :: specs(:)
  integer, intent(out)                         :: n_specs
  logical, intent(out)                         :: debug
  character(len=*), intent(out)                :: restartdir
  integer, intent(out)                         :: rc
end subroutine
```

Responsibility:
- Root-only read of `input.nml`
- fill temporary arrays/scalars
- construct `specs(:)`
- apply defaults that are purely namelist-related

```fortran
subroutine normalize_output_specs(specs, n_specs, output_dir_default, rc)
  type(output_spec_type), intent(inout) :: specs(:)
  integer, intent(in)                   :: n_specs
  character(len=*), intent(in)          :: output_dir_default
  integer, intent(out)                  :: rc
end subroutine
```

Responsibility:
- fill defaults
- compute `nparts` from `layout_x * layout_y` or equivalent
- compute/validate time rules and other derived config

```fortran
subroutine validate_output_specs(specs, n_specs, rc)
  type(output_spec_type), intent(in) :: specs(:)
  integer, intent(in)                :: n_specs
  integer, intent(out)               :: rc
end subroutine
```

Responsibility:
- check for invalid combinations
- examples:
  - unsupported mode
  - zero/negative frequency
  - layout enabled but invalid dimensions
  - duplicate names or alarm collisions

```fortran
subroutine broadcast_outputlog_config(vm, specs, n_specs, debug, restartdir, rc)
  type(ESMF_VM), intent(in)                         :: vm
  type(output_spec_type), allocatable, intent(inout) :: specs(:)
  integer, intent(inout)                            :: n_specs
  logical, intent(inout)                            :: debug
  character(len=*), intent(inout)                   :: restartdir
  integer, intent(out)                              :: rc
end subroutine
```

Responsibility:
- all PEs receive the exact same parsed configuration
- may require broadcasting fields individually if derived types are awkward to send directly

```fortran
subroutine init_output_states(specs, states, lastrestart, rc)
  type(output_spec_type), intent(in)                :: specs(:)
  type(output_state_type), allocatable, intent(out) :: states(:)
  type(ESMF_Time), intent(in)                       :: lastrestart
  integer, intent(out)                              :: rc
end subroutine
```

Responsibility:
- allocate per-output runtime state
- allocate per-part arrays based on `specs(i)%nparts`

### Time and filename construction

```fortran
subroutine get_output_reference_time(spec, currTime, nextTime, prevRing, atStopTime, refTime, rc)
  type(output_spec_type), intent(in) :: spec
  type(ESMF_Time), intent(in)        :: currTime
  type(ESMF_Time), intent(in)        :: nextTime
  type(ESMF_Time), intent(in)        :: prevRing
  logical, intent(in)                :: atStopTime
  type(ESMF_Time), intent(out)       :: refTime
  integer, intent(out)               :: rc
end subroutine
```

Responsibility:
- isolate averaging-mode-specific timestamp selection
- this should own the distinction between average and instantaneous naming

```fortran
subroutine build_output_basename(spec, refTime, basename, rc)
  type(output_spec_type), intent(in) :: spec
  type(ESMF_Time), intent(in)        :: refTime
  character(len=*), intent(out)      :: basename
  integer, intent(out)               :: rc
end subroutine
```

Responsibility:
- convert one output spec plus one logical reference time into the base filename stem
- example result: `ocn_20260101.030000`

```fortran
subroutine build_output_part_filenames(spec, basename, filenames, rc)
  type(output_spec_type), intent(in)                 :: spec
  character(len=*), intent(in)                       :: basename
  character(len=256), allocatable, intent(out)      :: filenames(:)
  integer, intent(out)                               :: rc
end subroutine
```

Responsibility:
- for non-layout output, return one filename
- for layout output, return all expected part filenames
- this routine should encode the layout suffix convention in one place

### File metadata and completeness

```fortran
subroutine capture_create_metadata(vm, filenames, createsize, initial_nlen, rc)
  type(ESMF_VM), intent(in)                     :: vm
  character(len=*), intent(in)                  :: filenames(:)
  integer, intent(out)                          :: createsize(:)
  integer, intent(out)                          :: initial_nlen(:)
  integer, intent(out)                          :: rc
end subroutine
```

Responsibility:
- root probes each expected part
- all PEs receive consistent sentinel or real values
- this generalizes the existing create-time filesize logic to multiple parts

```fortran
subroutine probe_output_part(vm, filename, part_status, rc)
  type(ESMF_VM), intent(in)               :: vm
  character(len=*), intent(in)            :: filename
  type(output_part_status_type), intent(out) :: part_status
  integer, intent(out)                    :: rc
end subroutine
```

Responsibility:
- root probes one file
- broadcast a consistent `part_status`
- keeps filesystem access isolated from decision logic

```fortran
logical function output_part_is_complete(part_status, use_filesize)
  type(output_part_status_type), intent(in) :: part_status
  logical, intent(in)                       :: use_filesize
end function
```

Responsibility:
- pure decision logic if possible
- preserve current semantics:
  - false if file absent
  - true if `nlen > 0` and filesize tracking not required
  - true if `nlen > 0` and `currsize > createsize` when filesize tracking is required

```fortran
subroutine check_output_family_complete(vm, filenames, use_filesize, createsize, part_done, family_complete, rc)
  type(ESMF_VM), intent(in)              :: vm
  character(len=*), intent(in)           :: filenames(:)
  logical, intent(in)                    :: use_filesize(:)
  integer, intent(in)                    :: createsize(:)
  logical, intent(out)                   :: part_done(:)
  logical, intent(out)                   :: family_complete
  integer, intent(out)                   :: rc
end subroutine
```

Responsibility:
- loops over all expected parts
- populates `part_done(:)`
- sets `family_complete = all(part_done)`
- mirrors restart `allDone` logic

### Run-loop orchestration

```fortran
subroutine arm_output_check(spec, state, nextTime, rc)
  type(output_spec_type), intent(in)    :: spec
  type(output_state_type), intent(inout):: state
  type(ESMF_Time), intent(in)           :: nextTime
  integer, intent(out)                  :: rc
end subroutine
```

Responsibility:
- set `chkfile_nextAdvance`
- compute basename and filenames for the next check window
- do not do actual file probing unless needed for create metadata capture

```fortran
subroutine update_create_metadata(vm, spec, state, rc)
  type(ESMF_VM), intent(in)             :: vm
  type(output_spec_type), intent(in)    :: spec
  type(output_state_type), intent(inout):: state
  integer, intent(out)                  :: rc
end subroutine
```

Responsibility:
- populate `state%createsize(:)` and `state%use_filesize(:)` from initial file observations

```fortran
subroutine evaluate_output_completion(vm, spec, state, family_complete, rc)
  type(ESMF_VM), intent(in)             :: vm
  type(output_spec_type), intent(in)    :: spec
  type(output_state_type), intent(inout):: state
  logical, intent(out)                  :: family_complete
  integer, intent(out)                  :: rc
end subroutine
```

Responsibility:
- wrapper around `check_output_family_complete`
- keeps `outputlog_run` short

```fortran
subroutine log_completed_output(logTime, startTime, spec, state, lastrestart, rc)
  type(ESMF_Time), intent(in)           :: logTime
  type(ESMF_Time), intent(in)           :: startTime
  type(output_spec_type), intent(in)    :: spec
  type(output_state_type), intent(in)   :: state
  type(ESMF_Time), intent(in)           :: lastrestart
  integer, intent(out)                  :: rc
end subroutine
```

Responsibility:
- perform existing `log_restart_fh` call(s)
- isolate naming/logging conventions from completion checking

## Proposed `outputlog_run` shape

```fortran
do i = 1, n_outputs
  call maybe_refresh_alarm(...)
  if (alarm_rang) then
    call arm_output_check(output_specs(i), output_states(i), nextTime, rc)
    call update_create_metadata(vm, output_specs(i), output_states(i), rc)
  end if

  if (output_states(i)%chkfile_nextAdvance) then
    call evaluate_output_completion(vm, output_specs(i), output_states(i), family_complete, rc)
    if (family_complete) then
      call log_completed_output(currTime_or_prevring, startTime, output_specs(i), output_states(i), lastrestart, rc)
      output_states(i)%chkfile_nextAdvance = .false.
    end if
  end if
end do
```

The goal is for `outputlog_run` to coordinate, not implement the details inline.

## Unit-Testing Seams

Best first test targets:
- `normalize_output_specs`
- `validate_output_specs`
- `get_output_reference_time`
- `build_output_basename`
- `build_output_part_filenames`
- `output_part_is_complete`

These should be designed to avoid direct VM or filesystem dependencies when possible.

Recommended split for testability:
- probing routine returns `output_part_status_type`
- pure or near-pure completion routine consumes `output_part_status_type`

That gives you a path to unit tests without needing real NetCDF files for every case.

## Suggested Refactor Order
- Extract helper routines from the current inline history logic with no semantic change.
- Introduce `output_spec_type` and `output_state_type` while still preserving current single-file average behavior.
- Replace `CompAttributeGet` configuration with root-read namelist and broadcast.
- Generalize history outputs from one file to `nparts` files using `all(part_done)`.
- Add instantaneous mode by changing the time-selection and filename-building helpers, not the monitoring core.

## Naming Guidance
To avoid ambiguity with `is_root_pe()`, prefer config field names like:
- `file_prefix`
- `output_prefix`
- `name_root`

Avoid naming a config field simply `root`.
