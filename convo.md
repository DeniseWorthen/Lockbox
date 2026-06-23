For this refactor, I would structure the code around a logical “output spec” rather than around the current hard-coded assumptions in mom_cap_outputlog.F90. Each spec would represent one output family and contain:

mode: instantaneous or average
frequency
filename rule or root
whether layout partitioning is enabled
expected part count or layout shape
time offset logic for filename construction
whether filesize-at-creation must be tracked for completion
Then the runtime flow becomes much cleaner:

Root reads input.nml.
Root builds a normalized list of output specs.
Root validates combinations like mode + frequency + layout + filename pattern.
Root broadcasts the finalized specs to all ranks.
The existing run loop iterates over specs instead of hard-coded frequencies and special cases.
That gives you a clean separation between policy and mechanics:

input.nml defines what outputs exist and how they are named.
The code defines how to monitor them.
The completeness test remains the same per part.
Aggregate completion becomes all(expected_parts_complete) for layout outputs.
The important design move is to stop treating “average output” and “instantaneous output” as branches sprinkled through the control flow. Instead, make them data-driven through per-spec fields like filename timestamp rule and completion behavior. Then the monitoring code can stay generic.

A practical schema direction for input.nml is:

global defaults: output directory, debug, maybe default layout behavior
repeated per-output entries: mode, frequency, filename root/pattern, layout info
Even if Fortran namelists make true repeated records awkward, you can still model this with parallel arrays indexed by output definition. That is much better than encoding more behavior in ad hoc conditionals.

So the restructuring I would aim for is:

Replace hard-coded n_freq/freq assumptions with a configurable array of output definitions.
Expand outputlog_type into something closer to a configurable output-spec-plus-runtime-state object.
Reuse the restart-style allDone aggregation for layout-partitioned history outputs.
Keep namelist read on root and broadcast the parsed spec list.
The main architectural rule is: mode and filename/layout policy should be data; completion checking should be code.

If you want, I can help sketch the new outputlog_type fields and a concrete input.nml layout that would support both instantaneous and averaged outputs without making the monitor logic brittle.


In practice, I would prefer structured fields over a free-form pattern string if possible. For example:

file_root = 'ocn'
avg_mode = 'average' or instantaneous
time_label = 'interval_midpoint' or valid_time
layout_suffix = true
Then the code owns the filename construction logic. That is safer than asking the namelist to provide arbitrary filename templates.

So “root” is the static prefix; “rule” is the recipe for producing the final filename from that prefix and the output semantics.

A concrete example:

Root: ocn
Average rule: ocn_YYYYMMDD.HHMMSS.nc
Instantaneous rule: maybe same shape, but timestamp computed from current valid time instead of averaging midpoint
Layout-partitioned rule: ocn_YYYYMMDD.HHMMSS.partNN.nc or whatever MOM actually emits
If you want, I can help turn that into a small set of explicit namelist fields that avoids an overly general filename-template system.





Sketching:
A good refactor target is to turn mom_cap_outputlog.F90 from a control-flow-heavy module into a small coordinator around a set of callable helpers. That will make the layout and averaging-mode expansion manageable, and it will give you seams for unit tests.

I would reshape it around three layers:

Configuration parsing and normalization
Filename and expectation building
Completion evaluation and logging decisions
A concrete structure could look like this.

Data Model
Expand the current outputlog_type into two concepts:

output_spec_type: static configuration for one output family
output_state_type: runtime state for one output family
Example fields for output_spec_type:

alarm_name
frequency_hours
mode
file_prefix
output_dir
layout_enabled
layout_x
layout_y
nparts
use_filesize
filename_time_rule
fhoffset
filename_fhoffset


Example fields for output_state_type:

chkfile_nextAdvance
last_filename_base
createsize(:)
part_complete(:)
time_lastrestart
That separation matters because unit tests should be able to exercise config interpretation without needing clock state, VM state, or filesystem state.



Refactor Shape
Keep mom_cap_outputlog.F90 as the public entrypoint module, but move inline logic into routines like:

read_outputlog_namelist
normalize_output_specs
broadcast_output_specs
init_output_specs
build_output_basename
build_output_part_filenames
capture_create_metadata
check_output_part_complete
check_output_family_complete
should_log_output
log_completed_output
Then outputlog_run becomes mostly orchestration:

Callable Routines
These are the main routines I would extract first.

read_outputlog_namelist(gcomp, specs, rc) or better read_outputlog_namelist(nmlfile, specs, rc)

Root-only read
Parse namelist into temporary arrays/scalars
Fill an array of output_spec_type
Apply defaults
Validate combinations
broadcast_output_specs(vm, specs, rc)

Broadcast normalized config to all PEs
Do not leave partially interpreted state on non-root
build_output_basename(spec, valid_time, basename, rc)

Encapsulates the difference between instantaneous and averaged output
This is where midpoint-time versus valid-time logic belongs
build_output_part_filenames(spec, basename, filenames, rc)


For non-layout output, returns one filename
For layout output, returns all expected part filenames
Mirrors the restart-style “enumerate all expected pieces” pattern
capture_create_metadata(vm, filenames, createsize, initial_nlen, rc)

Root does inquire and get_unlimited_len
Broadcasts metadata to all ranks
One element per part
Lets you preserve the current “filesize > createsize” behavior
check_output_part_complete(filename, use_filesize, createsize, rc)

Very small routine
One file in, one logical out
This is the easiest unit-test target if you later add test doubles around file probing
check_output_family_complete(vm, spec, filenames, createsize, allDone, rc)

Loops over all expected pieces
Reuses current completeness semantics per piece
Returns all(allDone)
log_completed_output(spec, logical_time, startTime, lastrestart, filenames, rc)

Keeps log formatting and naming out of the monitoring logic




Namelist Design
You probably want configuration that describes output families, not just scalars. Since Fortran namelists are awkward for nested structures, parallel arrays are acceptable.

For example, conceptually:

n_outputs
output_mode(i)
output_freq(i)
output_prefix(i)
output_layout_x(i)
output_layout_y(i)
output_time_rule(i)
Then normalize_output_specs turns those arrays into an array of output_spec_type.


Testing Strategy
Since you want unit testing but have not decided exactly how yet, the safest design is to create routines that can be tested without ESMF clocks, MPI, or real files whenever possible.

Best test targets:

normalize_output_specs
build_output_basename
build_output_part_filenames
get_output_reference_time
Those can be close to pure logic.

Next tier:

check_output_family_complete
This becomes testable if you separate probing from aggregation. A good pattern is:

one routine probes file state
another routine decides completion from an array of file states
For example:

probe_output_parts(...) -> part_status(:)
family_complete_from_status(part_status, use_filesize) -> logical
That second routine is ideal for unit tests.


If you want stronger testability, define a small status record per part:

exists
nlen
createsize
currsize
Then your completion logic becomes a pure decision function over data, not over files. That is the cleanest route to unit tests.

Minimal First Refactor
Do not try to solve everything at once. I would stage it like this:

Extract helpers from existing inline history logic without changing behavior.
Introduce output_spec_type and move hard-coded current settings into one spec array.
Replace CompAttributeGet configuration with root-read namelist plus broadcast.
Generalize single-file history into multi-part history using allDone.
Add instantaneous mode by changing only naming/reference-time helpers.
That order keeps each step falsifiable and limits the risk of mixing architectural change with semantic change.
