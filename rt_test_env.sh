
!bash rt_test_env.sh -n your_new_test -c intel -a your_account -u


#!/bin/bash
set -euo pipefail

usage() {
  cat << EOF
Usage: $0 -n <test_name> [-c <compiler>] [-m <machine>] [-a <account>] [-o <output_dir>] [-w <whitelist_file>] [-u] [-v]

Validate a single regression test's exported variables and rendered templates
without compiling the model or submitting a batch job.

Options:
  -n  Test name from tests/, for example control_c48
  -c  Compiler name (default: intel)
  -m  Override detected MACHINE_ID
  -a  Account name to store in ACCNR
  -o  Output directory for generated env and rendered files
  -w  File with regex patterns for expected new/changed variables
  -u  Write variable-diff reports to highlight potential unexpected exports
  -v  Verbose shell tracing
  -h  Show this help
EOF
}

die() {
  echo "$@" >&2
  exit 1
}

write_env_snapshot() {
  local output_file=$1
  : > "${output_file}"

  while IFS= read -r var_name; do
    case ${var_name} in
      AFLAGS|BASH*|DIRSTACK|EPOCH*|EUID|FUNCNAME|GROUPS|HOME|HOSTNAME|HOSTTYPE|IFS|LINENO|LOGNAME|MACHTYPE|OLDPWD|OPT*|OSTYPE|PATH|PIPESTATUS|PPID|PS4|PWD|SECONDS|SHELL|SHELLOPTS|SHLVL|TERM|TMPDIR|UID|USER|_)
        continue
        ;;
    esac

    if [[ ${var_name} =~ ^[A-Z][A-Z0-9_]*$ ]]; then
      printf 'export %s=%q\n' "${var_name}" "${!var_name}" >> "${output_file}"
    fi
  done < <(compgen -v | sort)
}

write_var_table() {
  local output_file=$1
  : > "${output_file}"

  while IFS= read -r var_name; do
    case ${var_name} in
      AFLAGS|BASH*|DIRSTACK|EPOCH*|EUID|FUNCNAME|GROUPS|HOME|HOSTNAME|HOSTTYPE|IFS|LINENO|LOGNAME|MACHTYPE|OLDPWD|OPT*|OSTYPE|PATH|PIPESTATUS|PPID|PS4|PWD|SECONDS|SHELL|SHELLOPTS|SHLVL|TERM|TMPDIR|UID|USER|_)
        continue
        ;;
    esac

    if [[ ${var_name} =~ ^[A-Z][A-Z0-9_]*$ ]]; then
      printf '%s\t%s\n' "${var_name}" "${!var_name}" >> "${output_file}"
    fi
  done < <(compgen -v | sort)
}

matches_whitelist() {
  local var_name=$1
  local whitelist_file=$2

  [[ -n ${whitelist_file} ]] || return 1
  [[ -f ${whitelist_file} ]] || die "Whitelist file ${whitelist_file} does not exist"

  while IFS= read -r pattern || [[ -n ${pattern} ]]; do
    [[ -n ${pattern} ]] || continue
    [[ ${pattern} == \#* ]] && continue
    if [[ ${var_name} =~ ${pattern} ]]; then
      return 0
    fi
  done < "${whitelist_file}"

  return 1
}

generate_var_reports() {
  local baseline_file=$1
  local final_file=$2
  local report_prefix=$3
  local whitelist_file=$4

  local new_vars_file="${report_prefix}.new_vars.txt"
  local changed_vars_file="${report_prefix}.changed_vars.txt"
  local unexpected_file="${report_prefix}.unexpected_vars.txt"
  local candidate_file="${report_prefix}.candidate_vars.txt"

  comm -13 \
    <(cut -f1 "${baseline_file}" | sort) \
    <(cut -f1 "${final_file}" | sort) \
    > "${new_vars_file}"

  awk -F '\t' '
    NR==FNR { before[$1]=$2; next }
    ($1 in before) && before[$1] != $2 { print $1 "\t" before[$1] "\t" $2 }
  ' "${baseline_file}" "${final_file}" > "${changed_vars_file}"

  cat /dev/null > "${candidate_file}"
  cat /dev/null > "${unexpected_file}"

  if [[ -s ${new_vars_file} ]]; then
    cat "${new_vars_file}" >> "${candidate_file}"
  fi

  if [[ -s ${changed_vars_file} ]]; then
    cut -f1 "${changed_vars_file}" >> "${candidate_file}"
  fi

  if [[ -s ${candidate_file} ]]; then
    sort -u "${candidate_file}" -o "${candidate_file}"
    while IFS= read -r var_name || [[ -n ${var_name} ]]; do
      [[ -n ${var_name} ]] || continue
      if [[ -n ${whitelist_file} ]]; then
        if ! matches_whitelist "${var_name}" "${whitelist_file}"; then
          printf '%s\n' "${var_name}" >> "${unexpected_file}"
        fi
      else
        printf '%s\n' "${var_name}" >> "${unexpected_file}"
      fi
    done < "${candidate_file}"
  fi
}

resolve_machine_settings() {
  ACCNR=${ACCNR:-${ACCOUNT:-}}
  case ${MACHINE_ID} in
    wcoss2|acorn)
      DISKNM="/lfs/h2/emc/nems/noscrub/emc.nems/RT"
      QUEUE="dev"
      PARTITION=""
      SCHEDULER="pbs"
      ;;
    gaeac5)
      DISKNM="/gpfs/f5/epic/world-shared/UFS-WM_RT"
      QUEUE="normal"
      PARTITION="c5"
      SCHEDULER="slurm"
      ;;
    gaeac6)
      DISKNM="/gpfs/f6/bil-fire8/world-shared/role.epic/UFS-WM_RT"
      QUEUE="normal"
      PARTITION="c6"
      SCHEDULER="slurm"
      ;;
    hera)
      DISKNM="/scratch3/NAGAPE/epic/role.epic/UFS-WM_RT"
      QUEUE="batch"
      PARTITION=""
      SCHEDULER="slurm"
      ;;
    ursa)
      DISKNM="/scratch4/NAGAPE/epic/role-epic/UFS-WM_RT"
      QUEUE="batch"
      PARTITION="u1-compute"
      SCHEDULER="slurm"
      ;;
    orion)
      DISKNM="/work2/noaa/epic/UFS-WM_RT"
      QUEUE="batch"
      PARTITION="orion"
      SCHEDULER="slurm"
      ;;
    hercules)
      DISKNM="/work2/noaa/epic/hercules/UFS-WM_RT"
      QUEUE="batch"
      PARTITION="hercules"
      SCHEDULER="slurm"
      ;;
    derecho)
      DISKNM="/glade/derecho/scratch/epicufsrt/ufs-weather-model/RT/"
      QUEUE="main"
      PARTITION=""
      SCHEDULER="pbs"
      ;;
    noaacloud)
      DISKNM="/contrib/ufs-weather-model/RT"
      QUEUE="batch"
      PARTITION=""
      SCHEDULER="slurm"
      ;;
    linux)
      DISKNM="${DISKNM:-${PATHTR}}"
      QUEUE="batch"
      PARTITION=""
      SCHEDULER="slurm"
      ;;
    *)
      die "Unknown MACHINE_ID ${MACHINE_ID}. Use -m to override if needed."
      ;;
  esac

  export ACCNR DISKNM QUEUE PARTITION SCHEDULER
}

render_fv3_run() {
  : > fv3_run
  if [[ -n ${FV3_RUN:-} ]]; then
    local config_name
    for config_name in ${FV3_RUN}; do
      [[ -f ${PATHRT}/fv3_conf/${config_name} ]] || die "Missing FV3 config ${PATHRT}/fv3_conf/${config_name}"
      atparse < "${PATHRT}/fv3_conf/${config_name}" >> fv3_run
    done
  else
    die "No FV3_RUN set in test file ${TEST_NAME}"
  fi
}

render_main_templates() {
  if [[ ${DO_UGWP_V1:-.false.} == .true. ]]; then
    export HIDE_UGWPV0='!'
    export HIDE_UGWPV1=' '
  else
    export HIDE_UGWPV0=' '
    export HIDE_UGWPV1='!'
  fi

  if [[ ${GFSv17opn} == .true. ]]; then
    export HIDE_AIAU=' '
    export HIDE_LIAU='!'
  else
    export HIDE_AIAU=' '
    export HIDE_LIAU=' '
  fi

  if [[ ${DATM_CDEPS} = 'true' ]] || [[ ${FV3} = 'true' ]] || [[ ${S2S} = 'true' ]] || [[ ${MPAS} = 'true' ]]; then
    if [[ ${HAFS} = 'false' ]] || [[ ${FV3} = 'true' && ${HAFS} = 'true' ]]; then
      [[ -f ${PATHRT}/parm/${INPUT_NML:-input.nml.IN} ]] || die "Missing parm file ${PATHRT}/parm/${INPUT_NML:-input.nml.IN}"
      atparse < "${PATHRT}/parm/${INPUT_NML:-input.nml.IN}" > input.nml
    fi
  fi

  [[ -n ${MODEL_CONFIGURE:-} ]] || die "MODEL_CONFIGURE is not set by test ${TEST_NAME}"
  [[ -f ${PATHRT}/parm/${MODEL_CONFIGURE} ]] || die "Missing model configure file ${PATHRT}/parm/${MODEL_CONFIGURE}"
  atparse < "${PATHRT}/parm/${MODEL_CONFIGURE}" > model_configure

  if [[ ${ESMF_THREADING} == true ]]; then
    compute_petbounds_and_tasks_esmf_threading
  else
    compute_petbounds_and_tasks_traditional_threading
  fi

  [[ -n ${UFS_CONFIGURE:-} ]] || die "UFS_CONFIGURE is not set by test ${TEST_NAME}"
  [[ -f ${PATHRT}/parm/${UFS_CONFIGURE} ]] || die "Missing UFS configure file ${PATHRT}/parm/${UFS_CONFIGURE}"
  (
    atparse < "${PATHRT}/parm/${UFS_CONFIGURE}" > ufs.configure
    if [[ ${ESMF_THREADING} != true ]]; then
      sed -i -e "/_omp_num_threads:/d" ufs.configure
    fi
  )

  render_nested_template INPUT_NEST02_NML 02
  render_nested_template INPUT_NEST03_NML 03
  render_nested_template INPUT_NEST04_NML 04
  render_nested_template INPUT_NEST05_NML 05
  render_nested_template INPUT_NEST06_NML 06

  if [[ -n ${DIAG_TABLE:-} ]]; then
    [[ -f ${PATHRT}/parm/diag_table/${DIAG_TABLE} ]] || die "Missing diag table ${PATHRT}/parm/diag_table/${DIAG_TABLE}"
    atparse < "${PATHRT}/parm/diag_table/${DIAG_TABLE}" > diag_table
  fi

  if [[ -n ${FIELD_TABLE:-} ]]; then
    [[ -f ${PATHRT}/parm/field_table/${FIELD_TABLE} ]] || die "Missing field table ${PATHRT}/parm/field_table/${FIELD_TABLE}"
    cp "${PATHRT}/parm/field_table/${FIELD_TABLE}" field_table
  fi
}

render_application_templates() {
  if [[ -f fv3_run ]]; then
    mkdir() {
      command mkdir -p "$@"
    }
    cp() {
      return 0
    }
    mv() {
      return 0
    }
    ln() {
      return 0
    }
    rm() {
      return 0
    }
    rsync() {
      return 0
    }
    ls() {
      return 0
    }
    source ./fv3_run
    unset -f mkdir
    unset -f cp
    unset -f mv
    unset -f ln
    unset -f rm
    unset -f rsync
    unset -f ls
  fi

  if [[ ${AQM} == .true. ]]; then
    [[ -f ${PATHRT}/parm/aqm/${aqm_rc_file} ]] || die "Missing AQM rc file ${PATHRT}/parm/aqm/${aqm_rc_file}"
    cp "${PATHRT}/parm/aqm/${aqm_rc_file}" ./aqm.rc
  fi

  [[ -f ${PATHRT}/parm/fd_ufs.yaml ]] || die "Missing field dictionary ${PATHRT}/parm/fd_ufs.yaml"
  cp "${PATHRT}/parm/fd_ufs.yaml" fd_ufs.yaml

  if [[ ${CPLWAV} == .true. ]]; then
    if [[ ${GFSv17opn} == .false. ]]; then
      [[ -f ${PATHRT}/parm/ww3_shel.nml.IN ]] || die "Missing WW3 template ${PATHRT}/parm/ww3_shel.nml.IN"
      [[ -f ${PATHRT}/parm/ww3_points.list ]] || die "Missing WW3 points list ${PATHRT}/parm/ww3_points.list"
      atparse < "${PATHRT}/parm/ww3_shel.nml.IN" > ww3_shel.nml
      cp "${PATHRT}/parm/ww3_points.list" .
    fi
  fi

  if [[ ${CPLCHM} == .true. ]]; then
    if [[ ${BMIC} == .true. ]]; then
      cp "${PATHRT}"/parm/gocart/gefs/*.rc .
      [[ -f ${PATHRT}/parm/gocart/gefs/AERO_HISTORY.rc.IN ]] || die "Missing AERO history template ${PATHRT}/parm/gocart/gefs/AERO_HISTORY.rc.IN"
      atparse < "${PATHRT}/parm/gocart/gefs/AERO_HISTORY.rc.IN" > AERO_HISTORY.rc
    else
      cp "${PATHRT}"/parm/gocart/*.rc .
      [[ -f ${PATHRT}/parm/gocart/AERO_HISTORY.rc.IN ]] || die "Missing AERO history template ${PATHRT}/parm/gocart/AERO_HISTORY.rc.IN"
      atparse < "${PATHRT}/parm/gocart/AERO_HISTORY.rc.IN" > AERO_HISTORY.rc
    fi
  fi

  if [[ ${DATM_CDEPS} = 'true' ]] || [[ ${S2S} = 'true' ]]; then
    if [[ ${HAFS} = 'false' ]]; then
      mkdir -p INPUT
      [[ -f ${PATHRT}/parm/ice_in.IN ]] || die "Missing ice template ${PATHRT}/parm/ice_in.IN"
      [[ -f ${PATHRT}/parm/${MOM6_INPUT:-MOM_input_${OCNRES}.IN} ]] || die "Missing MOM6 input template ${PATHRT}/parm/${MOM6_INPUT:-MOM_input_${OCNRES}.IN}"
      [[ -f ${PATHRT}/parm/diag_table/${DIAG_TABLE:-diag_table_template.IN} ]] || die "Missing diag table ${PATHRT}/parm/diag_table/${DIAG_TABLE:-diag_table_template.IN}"
      [[ -f ${PATHRT}/parm/MOM6_data_table.IN ]] || die "Missing MOM6 data table ${PATHRT}/parm/MOM6_data_table.IN"
      atparse < "${PATHRT}/parm/ice_in.IN" > ice_in
      atparse < "${PATHRT}/parm/${MOM6_INPUT:-MOM_input_${OCNRES}.IN}" > INPUT/MOM_input
      atparse < "${PATHRT}/parm/diag_table/${DIAG_TABLE:-diag_table_template.IN}" > diag_table
      atparse < "${PATHRT}/parm/MOM6_data_table.IN" > data_table
    fi
  fi

  if [[ ${HAFS} = 'true' ]] && [[ ${DATM_CDEPS} = 'false' ]]; then
    [[ -f ${PATHRT}/parm/diag_table/${DIAG_TABLE:-diag_table_template.IN} ]] || die "Missing diag table ${PATHRT}/parm/diag_table/${DIAG_TABLE:-diag_table_template.IN}"
    atparse < "${PATHRT}/parm/diag_table/${DIAG_TABLE:-diag_table_template.IN}" > diag_table
  fi

  if [[ "${DIAG_TABLE_ADDITIONAL:-}Q" != Q ]]; then
    [[ -f ${PATHRT}/parm/diag_table/${DIAG_TABLE_ADDITIONAL:-} ]] || die "Missing additional diag table ${PATHRT}/parm/diag_table/${DIAG_TABLE_ADDITIONAL:-}"
    atparse < "${PATHRT}/parm/diag_table/${DIAG_TABLE_ADDITIONAL:-}" >> diag_table
  fi

  if [[ "${FIELD_TABLE_ADDITIONAL:-}Q" != Q ]]; then
    [[ -f ${PATHRT}/parm/field_table/${FIELD_TABLE_ADDITIONAL:-} ]] || die "Missing additional field table ${PATHRT}/parm/field_table/${FIELD_TABLE_ADDITIONAL:-}"
    atparse < "${PATHRT}/parm/field_table/${FIELD_TABLE_ADDITIONAL:-}" >> field_table
  fi

  if [[ ${CPLCHM} == .true. ]] && [[ ${S2S} = 'false' ]]; then
    [[ -f ${PATHRT}/parm/diag_table/${DIAG_TABLE:-diag_table_template.IN} ]] || die "Missing diag table ${PATHRT}/parm/diag_table/${DIAG_TABLE:-diag_table_template.IN}"
    atparse < "${PATHRT}/parm/diag_table/${DIAG_TABLE:-diag_table_template.IN}" > diag_table
  fi

  if [[ ${DATM_CDEPS} = 'true' ]]; then
    [[ -f ${PATHRT}/parm/${DATM_IN_CONFIGURE:-datm_in.IN} ]] || die "Missing DATM configure ${PATHRT}/parm/${DATM_IN_CONFIGURE:-datm_in.IN}"
    [[ -f ${PATHRT}/parm/${DATM_STREAM_CONFIGURE:-datm.streams.IN} ]] || die "Missing DATM streams ${PATHRT}/parm/${DATM_STREAM_CONFIGURE:-datm.streams.IN}"
    atparse < "${PATHRT}/parm/${DATM_IN_CONFIGURE:-datm_in.IN}" > datm_in
    atparse < "${PATHRT}/parm/${DATM_STREAM_CONFIGURE:-datm.streams.IN}" > datm.streams
  fi

  if [[ ${DOCN_CDEPS} = 'true' ]]; then
    [[ -f ${PATHRT}/parm/${DOCN_IN_CONFIGURE:-docn_in.IN} ]] || die "Missing DOCN configure ${PATHRT}/parm/${DOCN_IN_CONFIGURE:-docn_in.IN}"
    [[ -f ${PATHRT}/parm/${DOCN_STREAM_CONFIGURE:-docn.streams.IN} ]] || die "Missing DOCN streams ${PATHRT}/parm/${DOCN_STREAM_CONFIGURE:-docn.streams.IN}"
    atparse < "${PATHRT}/parm/${DOCN_IN_CONFIGURE:-docn_in.IN}" > docn_in
    atparse < "${PATHRT}/parm/${DOCN_STREAM_CONFIGURE:-docn.streams.IN}" > docn.streams
  fi

  if [[ ${DICE_CDEPS} = 'true' ]]; then
    [[ -f ${PATHRT}/parm/${DICE_IN_CONFIGURE:-dice_in.IN} ]] || die "Missing DICE configure ${PATHRT}/parm/${DICE_IN_CONFIGURE:-dice_in.IN}"
    [[ -f ${PATHRT}/parm/${DICE_STREAM_CONFIGURE:-dice.streams.IN} ]] || die "Missing DICE streams ${PATHRT}/parm/${DICE_STREAM_CONFIGURE:-dice.streams.IN}"
    atparse < "${PATHRT}/parm/${DICE_IN_CONFIGURE:-dice_in.IN}" > dice_in
    atparse < "${PATHRT}/parm/${DICE_STREAM_CONFIGURE:-dice.streams.IN}" > dice.streams
  fi

  if [[ ${CICE_PRESCRIBED} = 'true' ]]; then
    [[ -f ${PATHRT}/parm/ice_in.IN ]] || die "Missing ice template ${PATHRT}/parm/ice_in.IN"
    atparse < "${PATHRT}/parm/ice_in.IN" > ice_in
  fi

  if [[ ${CDEPS_INLINE} = 'true' ]]; then
    [[ -f ${PATHRT}/parm/${CDEPS_INLINE_CONFIGURE:-stream.config.IN} ]] || die "Missing CDEPS inline configure ${PATHRT}/parm/${CDEPS_INLINE_CONFIGURE:-stream.config.IN}"
    atparse < "${PATHRT}/parm/${CDEPS_INLINE_CONFIGURE:-stream.config.IN}" > stream.config
  fi

  if [[ ${FIRE_BEHAVIOR} = 'true' ]]; then
    [[ -f ${PATHRT}/parm/${FIRE_NML:-namelist.fire.IN} ]] || die "Missing fire namelist ${PATHRT}/parm/${FIRE_NML:-namelist.fire.IN}"
    atparse < "${PATHRT}/parm/${FIRE_NML:-namelist.fire.IN}" > namelist.fire
  fi

  if [[ ${atm_model:-} = 'mpasmodel' ]]; then
    [[ -f ${PATHRT}/parm/mpasmodel/namelist.atmosphere.IN ]] || die "Missing MPAS namelist ${PATHRT}/parm/mpasmodel/namelist.atmosphere.IN"
    [[ -f ${PATHRT}/parm/mpasmodel/streams.atmosphere.IN ]] || die "Missing MPAS streams ${PATHRT}/parm/mpasmodel/streams.atmosphere.IN"
    [[ -f ${PATHRT}/parm/mpasmodel/ufs_mpas_streams.IN.yaml ]] || die "Missing MPAS UFS streams ${PATHRT}/parm/mpasmodel/ufs_mpas_streams.IN.yaml"
    atparse < "${PATHRT}/parm/mpasmodel/namelist.atmosphere.IN" > namelist.atmosphere
    atparse < "${PATHRT}/parm/mpasmodel/streams.atmosphere.IN" > streams.atmosphere
    cp "${PATHRT}"/parm/mpasmodel/stream_list.atmosphere.* .
    atparse < "${PATHRT}/parm/mpasmodel/ufs_mpas_streams.IN.yaml" > ufs_mpas_streams.yaml
    cp "${PATHRT}"/parm/mpasmodel/*_vars.list .
  fi

  if [[ -n ${DT_CICE+x} ]]; then
    if [[ ${DT_ATMOS} -ne ${DT_CICE} ]]; then
      die "Atmosphere timestep (DT_ATMOS) should be equal to CICE timestep (DT_CICE)."
    fi
  fi

  if [[ -n ${coupling_interval_slow_sec+x} && -n ${coupling_interval_fast_sec+x} ]]; then
    if [[ $(( coupling_interval_slow_sec % coupling_interval_fast_sec )) -ne 0 ]]; then
      die "The slow coupling timestep must be divisible by the fast coupling timestep."
    fi
  fi
}

render_nested_template() {
  local template_var=$1
  local suffix=$2
  local template_name=${!template_var:-}

  if [[ -n ${template_name} ]]; then
    local inpes_var="INPES_NEST${suffix}"
    local jnpes_var="JNPES_NEST${suffix}"
    local npx_var="NPX_NEST${suffix}"
    local npy_var="NPY_NEST${suffix}"
    local k_split_var="K_SPLIT_NEST${suffix}"
    local n_split_var="N_SPLIT_NEST${suffix}"

    export INPES_NEST=${!inpes_var:-}
    export JNPES_NEST=${!jnpes_var:-}
    export NPX_NEST=${!npx_var:-}
    export NPY_NEST=${!npy_var:-}
    export K_SPLIT_NEST=${!k_split_var:-}
    export N_SPLIT_NEST=${!n_split_var:-}

    [[ -f ${PATHRT}/parm/${template_name} ]] || die "Missing nested template ${PATHRT}/parm/${template_name}"
    atparse < "${PATHRT}/parm/${template_name}" > "input_nest${suffix}.nml"
  else
    sed -i -e "/<output_grid_${suffix}>/,/<\/output_grid_${suffix}>/d" model_configure
  fi
}

TEST_NAME=""
RT_COMPILER="intel"
OUTPUT_DIR=""
WHITELIST_FILE=""
REPORT_UNEXPECTED=false
RTVERBOSE=false

while getopts ":n:c:m:a:o:w:uvh" opt; do
  case ${opt} in
    n)
      TEST_NAME=${OPTARG}
      ;;
    c)
      RT_COMPILER=${OPTARG}
      ;;
    m)
      MACHINE_ID=${OPTARG}
      ;;
    a)
      ACCNR=${OPTARG}
      ;;
    o)
      OUTPUT_DIR=${OPTARG}
      ;;
    w)
      WHITELIST_FILE=${OPTARG}
      ;;
    u)
      REPORT_UNEXPECTED=true
      ;;
    v)
      RTVERBOSE=true
      ;;
    h)
      usage
      exit 0
      ;;
    :)
      usage
      die "Option -${OPTARG} requires an argument."
      ;;
    \?)
      usage
      die "Invalid option: -${OPTARG}"
      ;;
  esac
done

[[ -n ${TEST_NAME} ]] || {
  usage
  die "Option -n <test_name> is required."
}

case ${RT_COMPILER} in
  intel|intelllvm|gnu)
    ;;
  *)
    die "Compiler must be intel, intelllvm, or gnu"
    ;;
esac

if [[ ${RTVERBOSE} == true ]]; then
  set -x
fi

PATHRT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd -P )"
readonly PATHRT
PATHTR="$( cd "${PATHRT}/.." && pwd )"
readonly PATHTR

cd "${PATHRT}"

if [[ -z ${MACHINE_ID:-} ]]; then
  source detect_machine.sh
fi

resolve_machine_settings

[[ -f ${PATHRT}/tests/${TEST_NAME} ]] || die "Test file ${PATHRT}/tests/${TEST_NAME} does not exist"

if [[ -f ${PATHRT}/bl_date.conf ]]; then
  source bl_date.conf
fi

RTPWD=${RTPWD:-${DISKNM}/NEMSfv3gfs/develop-${BL_DATE:-unknown}}
INPUTDATA_ROOT=${INPUTDATA_ROOT:-${DISKNM}/NEMSfv3gfs/input-data-20251015}
INPUTDATA_ROOT_WW3=${INPUTDATA_ROOT_WW3:-${INPUTDATA_ROOT}/WW3_input_data_20250807}
INPUTDATA_LM4=${INPUTDATA_LM4:-${INPUTDATA_ROOT}/LM4_input_data}
INPUTDATA_GFSv17opn=${INPUTDATA_GFSv17opn:-${DISKNM}/NEMSfv3gfs/GFSv17opn_20251014}
INPUTDATA_ROOT_MPAS=${INPUTDATA_ROOT_MPAS:-/glade/derecho/scratch/worthen/MPASMODEL}

TEST_ID=${TEST_NAME}_${RT_COMPILER}
OUTPUT_DIR=${OUTPUT_DIR:-${PATHRT}/logs/envcheck_${TEST_ID}}
WORK_DIR=${OUTPUT_DIR}/work
LOG_DIR=${OUTPUT_DIR}/logs
ENV_FILE=${OUTPUT_DIR}/${TEST_ID}.env
BASELINE_VARS_FILE=${OUTPUT_DIR}/${TEST_ID}.baseline.tsv
FINAL_VARS_FILE=${OUTPUT_DIR}/${TEST_ID}.final.tsv
REPORT_PREFIX=${OUTPUT_DIR}/${TEST_ID}

mkdir -p "${WORK_DIR}" "${LOG_DIR}"

export MACHINE_ID RT_COMPILER PATHRT PATHTR RTPWD INPUTDATA_ROOT INPUTDATA_ROOT_WW3
export INPUTDATA_LM4 INPUTDATA_GFSv17opn INPUTDATA_ROOT_MPAS
export TEST_NAME TEST_ID LOG_DIR RTVERBOSE
export CREATE_BASELINE=false NEW_BASELINE=${OUTPUT_DIR}/new_baseline
export RT_SUFFIX="" BL_SUFFIX="" ROCOTO=false ECFLOW=false DEP_RUN=""
export skip_check_results=false delete_rundir=false KEEP_RUNDIR=true
export REGRESSIONTEST_LOG=${LOG_DIR}/rt_test_env.log

unset MODEL_CONFIGURE
unset UFS_CONFIGURE

source default_vars.sh

write_var_table "${BASELINE_VARS_FILE}"

# shellcheck disable=SC1090
source "${PATHRT}/tests/${TEST_NAME}"

source rt_utils.sh

if [[ ${ESMF_THREADING} == true ]]; then
  compute_petbounds_and_tasks_esmf_threading
else
  compute_petbounds_and_tasks_traditional_threading
fi

TPN=$(( TPN / THRD ))
NODES=$(( TASKS / TPN ))
if (( NODES * TPN < TASKS )); then
  NODES=$((NODES + 1))
fi

PPN=$(( TASKS / NODES ))
if (( TASKS - (PPN * NODES) > 0 )); then
  PPN=$((PPN + 1))
fi

export TPN NODES PPN WLCLK=${WLCLK:-}

write_env_snapshot "${ENV_FILE}"

source atparse.bash

cd "${WORK_DIR}"

render_fv3_run
render_main_templates
render_application_templates

write_var_table "${FINAL_VARS_FILE}"
write_env_snapshot "${ENV_FILE}"

if [[ ${REPORT_UNEXPECTED} == true ]]; then
  generate_var_reports "${BASELINE_VARS_FILE}" "${FINAL_VARS_FILE}" "${REPORT_PREFIX}" "${WHITELIST_FILE}"
fi

cat << EOF
Validated test setup for ${TEST_ID}
Machine: ${MACHINE_ID}
Work directory: ${WORK_DIR}
Environment snapshot: ${ENV_FILE}
Generated files:
  fv3_run
  input.nml (if applicable)
  model_configure
  ufs.configure
  input_nest*.nml (if applicable)
  diag_table / field_table (if applicable)
EOF

if [[ ${REPORT_UNEXPECTED} == true ]]; then
  cat << EOF
Variable reports:
  ${REPORT_PREFIX}.new_vars.txt
  ${REPORT_PREFIX}.changed_vars.txt
  ${REPORT_PREFIX}.unexpected_vars.txt
EOF
fi
