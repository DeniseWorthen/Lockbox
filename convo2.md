
What likely happened is:

main module defines outputlog_type
helper module needs that type for readnml
main module also uses the helper module
That creates:

main -> helper
helper -> main
and Fortran will not allow that cycle.

The cleanest way out is to split the shared data model from the orchestration.

A good dependency shape is:


mom_outputlog_types
  ^
  |
mom_outputlog_helpers
  ^
  |
MOM_cap_outputlog



So:

mom_outputlog_types contains only derived types and simple constants
mom_outputlog_helpers contains namelist parsing and ESMF-independent helper routines
MOM_cap_outputlog contains the ESMF/NUOPC-facing orchestration
The important design point is: do not make readnml take the full runtime olog object if that object contains ESMF state or run-loop state.

Instead, split your current type into at least two types.

For example:
module mom_outputlog_types
  implicit none
  private
  public :: output_spec_type, output_state_type

  type :: output_spec_type
    character(len=32)  :: name
    character(len=256) :: output_dir
    character(len=64)  :: file_prefix
    integer            :: frequency_hours
    integer            :: mode
    logical            :: layout_enabled
    integer            :: layout_x
    integer            :: layout_y
    integer            :: nparts
  end type output_spec_type

  type :: output_state_type
    logical            :: chkfile_nextAdvance
    character(len=256) :: filename
    integer, allocatable :: createsize(:)
    logical, allocatable :: use_filesize(:)
  end type output_state_type
end module mom_outputlog_types

Then your helper module only needs output_spec_type:

module mom_outputlog_helpers
  use mom_outputlog_types, only : output_spec_type
  implicit none
contains
  subroutine read_outputlog_nml(path, specs, n_specs, rc)
    character(len=*), intent(in) :: path
    type(output_spec_type), allocatable, intent(out) :: specs(:)
    integer, intent(out) :: n_specs
    integer, intent(out) :: rc
  end subroutine
end module mom_outputlog_helpers

And your main module uses both:

module MOM_cap_outputlog
  use mom_outputlog_types,   only : output_spec_type, output_state_type
  use mom_outputlog_helpers, only : read_outputlog_nml
  use ESMF
  ...
end module MOM_cap_outputlog



The deeper suggestion is this: your namelist reader should probably not know about outputlog_type at all.

If your goal is to test helpers without ESMF, then the helper layer should operate on an ESMF-free configuration type, something like output_spec_type. The main module can then translate that config into runtime state, alarms, VM-aware data, and so on.

So instead of:

readnml(..., olog, ...)
prefer:

readnml(..., specs, ...)
init_olog_from_specs(specs, olog, ...)




If your current outputlog_type mixes config and runtime state, that is the real source of friction. Split it into:

output_spec_type
Configuration only, no ESMF types, no live state.

output_runtime_type or output_state_type
Runtime flags, filenames, create sizes, maybe alarms if needed.

If you still want a single public “logical output” object later, you can always compose them:




type :: outputlog_type
  type(output_spec_type)  :: spec
  type(output_state_type) :: state
  type(ESMF_Alarm)        :: alarm
end type outputlog_type





Yes. The split I would use for this code is:

- `spec`: things that describe what an output family is
- `state`: things that describe where the current run stands for that output family

That boundary matters because `spec` should be readable from namelist and testable without ESMF, while `state` is runtime bookkeeping.

A concrete split for your current logic would look like this.

**Spec**
This is static or quasi-static configuration. It should not change during the run except at initialization.

```fortran
type :: output_spec_type
  character(len=32)  :: name
  character(len=32)  :: alarm_name

  ! Output identity and naming
  character(len=256) :: output_dir
  character(len=64)  :: file_prefix
  integer            :: mode
  integer            :: time_rule

  ! Scheduling/configuration
  integer            :: frequency_hours

  ! Layout/file partitioning
  logical            :: layout_enabled
  integer            :: layout_x
  integer            :: layout_y
  integer            :: nparts

  ! Completion policy
  logical            :: allow_filesize_check

  ! Derived timing metadata
  integer            :: opt_n
end type output_spec_type
```

What belongs here from your current code:

- `alarm_name`
- `opt_n`
- output frequency
- output mode: average vs instantaneous
- filename policy inputs
- layout configuration
- output directory
- whether this output family may require filesize-based completion

What does **not** belong here:

- current filename for this cycle
- whether the alarm rang on the last step
- create sizes from the current candidate files
- last completed restart time for this specific pending output check

Those are runtime state.

**State**
This is mutable run-time bookkeeping for one output family.

```fortran
type :: output_state_type
  logical                  :: chkfile_nextAdvance
  logical                  :: active

  ! Current logical output under observation
  character(len=256)       :: basename
  character(len=256), allocatable :: filenames(:)

  ! Per-part status for the current logical output
  integer, allocatable     :: createsize(:)
  logical, allocatable     :: use_filesize(:)
  logical, allocatable     :: part_done(:)

  ! Logging/bookkeeping
  type(ESMF_Time)          :: time_lastrestart
end type output_state_type
```

What belongs here from your current code:

- `chkfile_nextAdvance`
- current `filename`
- current `createsize`
- current `use_filesize`
- current “which files are done” information for layout outputs
- `time_lastrestart`

For layout-aware history output, scalar fields like `filename`, `createsize`, and `use_filesize` really want to become arrays or a basename-plus-array design.

**ESMF-Dependent Runtime**
There is one more thing: `alarm` and `TimeInterval` fields.

Those do not belong in the namelist-oriented helper layer. You have two options.

Option A, keep them in a third orchestration type:

```fortran
type :: output_runtime_type
  type(ESMF_Alarm)        :: alarm
  type(ESMF_TimeInterval) :: fhoffset
  type(ESMF_TimeInterval) :: filename_fhoffset
end type output_runtime_type
```

Then the main module owns:

```fortran
type(output_spec_type), allocatable    :: specs(:)
type(output_state_type), allocatable   :: states(:)
type(output_runtime_type), allocatable :: runtime(:)
```

Option B, fold `fhoffset`, `filename_fhoffset`, and `alarm` into `state`.

I would not do that unless you want the state type to be ESMF-dependent. Since you said you want helpers testable without ESMF, keep those out of the helper-facing types.

So the cleaner split is really three-way:

- `spec`: testable config
- `state`: testable mutable output-tracking state
- `runtime`: ESMF objects and other cap-facing machinery

If you want only two conceptual buckets, then think of `runtime` as belonging to the orchestrator rather than the helper API.

**Mapping From Current `outputlog_type`**
Your current type is:

```fortran
type :: outputlog_type
  character(len=14)       :: alarm_name
  integer                 :: opt_n
  logical                 :: chkfile_nextAdvance
  logical                 :: use_filesize
  character(len=256)      :: filename
  integer                 :: createsize
  type(ESMF_Alarm)        :: alarm
  type(ESMF_TimeInterval) :: fhoffset
  type(ESMF_TimeInterval) :: filename_fhoffset
  type(ESMF_Time)         :: time_lastrestart
end type outputlog_type
```

I would map it like this:

Into `output_spec_type`:
- `alarm_name`
- `opt_n`

Into `output_state_type`:
- `chkfile_nextAdvance`
- `use_filesize` becomes `use_filesize(:)` or at least remains state
- `filename` becomes `basename` and/or `filenames(:)`
- `createsize` becomes `createsize(:)`
- `time_lastrestart`

Into `output_runtime_type`:
- `alarm`
- `fhoffset`
- `filename_fhoffset`

That is the most defensible split.

**What `readnml` Should Return**
Your namelist reader should return only `output_spec_type` data. Not state. Not alarms.

For example:

```fortran
subroutine read_outputlog_nml(nmlfile, specs, n_specs, debug, restartdir, rc)
  character(len=*), intent(in) :: nmlfile
  type(output_spec_type), allocatable, intent(out) :: specs(:)
  integer, intent(out) :: n_specs
  logical, intent(out) :: debug
  character(len=*), intent(out) :: restartdir
  integer, intent(out) :: rc
end subroutine
```

Then in the main module:

```fortran
call read_outputlog_nml(..., specs, n_specs, debug, restartdir, rc)
call normalize_output_specs(specs, n_specs, rc)
call init_output_states(specs, states, lastrestart, rc)
call init_output_runtime(specs, mclock, runtime, rc)
```

That avoids circular dependency and gives you clean testing seams.

**What Helpers Should Operate On**
Helpers in the non-ESMF module should mostly use `output_spec_type` plus plain scalars/arrays.

Examples:

- `build_output_basename(spec, ...)`
- `build_output_part_filenames(spec, basename, ...)`
- `validate_output_specs(specs, ...)`
- `family_complete_from_status(part_status, ...)`

If a helper needs `ESMF_Time`, decide whether that helper really belongs in the ESMF-free layer. If not, keep a small wrapper in the main/orchestration layer that converts time state and calls a plain helper.

**Practical Rule**
When deciding whether a field is `spec` or `state`, ask:

- Is this read from namelist or derived once at init? Then `spec`.
- Does this change as the model advances? Then `state`.
- Is this an ESMF object needed to wire the cap? Then orchestration/runtime, not helper config.

If you want, I can next turn this into a concrete three-module layout with skeleton module declarations and `use` relationships for mom_cap_outputlog.F90.