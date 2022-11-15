#!/usr/bin/env python3

import sys, os, shutil
import logging
import argparse, textwrap
import subprocess
import re

logger = logging.getLogger(__name__)

def get_parser():
    """
    Get parser object for this script.
    """
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )

    parser.print_usage = parser.print_help

    parser.add_argument(
        '-v', '--verbose',
        help="ncrease output verbosity",
        action="store_true",
    )
    parser.add_argument(
        "--input_file",
        help = textwrap.dedent('''\
               input file
               [Required]'''),
        action="store",
        dest="input_file",
        required=True,
    )
    return parser

def if_block_detected(line):
    if_block_match = re.match(r'if +([A-Za-z][A-Za-z0-9_]*)', line)
    if if_block_match:
        print (f"WARNING: line is {line}")
    return if_block_match

def ignore_token_type(token):
    if token.strip(" ,") in ["REAL", "INTEGER", "LOGICAL"]:
        ignore = True
    else:
        try:
            tok_int = int(token)
            ignore = True
        except:
            ignore = False
    return ignore

def main ():

    args = get_parser().parse_args()
    input_file = args.input_file
    output_file = input_file + ".mod"

    ifdefs = ["W3_DEBUGSETUGIOBP",
                    "W3_DEBUGSRC",
                    "W3_DEBUGINIT",
                    "W3_DEBUGIO",
                    "W3_DEBUGRUN",
                    "W3_DEBUGCOH",
                    "W3_DEBUGIOBP",
                    "W3_DEBUGDCXDX",
                    "W3_DEBUGIOBC",
                    "W3_DEBUGIOGR",
                    "W3_DEBUGSETIOBP",
                    "W3_DEBUGFLS",
                    "W3_DEBUG",
                    "W3_MPIT",
                    "W3_MPRF",
                    "W3_S",
                    "W3_T",
                    "W3_T0",
                    "W3_T1",
                    "W3_T2",
                    "W3_T3",
                    "W3_T8",
                    "W3_T38",
                    "W3_TS",
                    "W3_TIMINGS"]

    with open(input_file, "r", encoding='utf-8') as inputf:
        input_lines = inputf.readlines()
    print (f"output_file is {output_file}")
    outputf = open(output_file, 'w')

    index_skip = 0
    ending = []
    for index,input_line in enumerate(input_lines):
        # Read input line
        input_line = input_line.rstrip("\n")

        # Determine contents of #ifdef
        ifdef_match = re.match(r'#ifdef +([A-Za-z][A-Za-z0-9_]*)', input_line)
        if ifdef_match:
            # Determine if the match is contained in the ifdefs list
            if ifdef_match:
                next_line_token = input_lines[index+1].strip().split(" ")[0]
                next_line_token_last = input_lines[index+1].strip().split(" ")[-1]
                if next_line_token.lower() == 'if' and next_line_token_last.lower() == 'then':
                    outputf.write(f"WARNING: at line number {index+1}")
                if next_line_token == 'USE':
                    new_line = input_lines[index+1].strip("\n")
                    outputf.write(f"{input_line}")
                    outputf.write(f"{new_line}")
                    ending.append("keep")
                    index_skip = index + 1
                elif ignore_token_type(next_line_token):
                    new_line = input_lines[index+1].strip("\n") + ' ! only for ' + ifdef_match.group(1).strip()
                    index_skip = index + 1
                    outputf.write(f"{new_line}")
                    ending.append("remove")
                elif ifdef_match.group(1) in ifdefs:
                    # Rewrite input_line if needed
                    input_line = 'if (' + ifdef_match.group(1).strip().lower() + '_flag) then'
                    outputf.write (f"{input_line}")
                    ending.append("replace")
                else:
                    if index_skip != index:
                        outputf.write (f"{input_line}")
                    ending.append("keep")
        elif '#if defined' in input_line:
            outputf.write (f"{input_line}")
            ending.append("keep")
        elif '#else' in input_line:
            if ending[-1] == "keep":
                outputf.write (f"{input_line}")
            elif ending[-1] == "replace":
                outputf.write ("else")
            #TODO: add error condition for ending is 'remove'
        elif '#endif' in input_line:
            if ending[-1] == 'replace':
                outputf.write(f"end if")
                ending.pop()
            elif ending[-1] == 'keep':
                outputf.write (f"#endif")
                ending.pop()
        else:
            if index == index_skip and index != 0:
                index_skip = 0
            else:
                outputf.write (f"{input_line}")

    sys.stdout.close()

if __name__ == "__main__":
    main()
