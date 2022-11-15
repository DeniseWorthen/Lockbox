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
        "--input",
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
     #if if_block_match:
     #    print (f"WARNING: line is {line}")
    return if_block_match

def ignore_token_type(token):
    if token.strip(" ,") in ["REAL", "INTEGER", "LOGICAL", "FORMAT"]:
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
    #ifdef_tokens = ["W3_TIMINGS"]

    ifdef_tokens = ["W3_DEBUGSETUGIOBP",
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

    # ifdef_tokens = ["W3_DEBUGSETUGIOBP",
    #                 "W3_DEBUGSRC",
    #                 "W3_DEBUGINIT",
    #                 "W3_DEBUGIO",
    #                 "W3_DEBUGRUN",
    #                 "W3_DEBUGCOH",
    #                 "W3_DEBUGIOBP",
    #                 "W3_DEBUGDCXDX",
    #                 "W3_DEBUGIOBC",
    #                 "W3_DEBUGIOGR",
    #                 "W3_DEBUGSETIOBP",
    #                 "W3_DEBUGFLS",
    #                 "W3_DEBUG",
    #                 "W3_MPIT",
    #                 "W3_MPRF",
    #                 "W3_NL5",
    #                 "W3_S",
    #                 "W3_T",
    #                 "W3_TIDE",
    #                 #
    #                 "W3_REF1",
    #                 "W3_REFRX",
    #                 "W3_RTD",
    #                 "W3_MPI",
    #                 #
    #                 "W3_CESMCOUPLED"]


    with open(input_file, "r", encoding='utf-8') as inputf:
        input_lines = inputf.readlines()
    print (f"output_file is {output_file}")

    index_skip = 0
    ending = []
    with open(output_file, "w+", encoding='utf-8') as outputf:
        for index,input_line in enumerate(input_lines):
            # Read input line
            input_line = input_line.rstrip("\n")
            # Determine contents of #ifdef
            ifdef_match = re.match(r'^#ifdef +([A-Za-z][A-Za-z0-9_]*)', input_line)
            token = ''
            next_line_token = ''
            if ifdef_match:
                token = ifdef_match.group(1).strip()
                next_line_match = re.search(r'([A-Za-z][A-Za-z0-9_]*)',input_lines[index+1])
                #print (f"DEBUG: next_line is {input_lines[index+1]} and match is {next_line_match}")
                next_line_token = re.search(r'([A-Za-z][A-Za-z0-9_]*)',input_lines[index+1]).group(1)
                if token in ifdef_tokens:
                    if ignore_token_type(next_line_token):
                        new_line = input_lines[index+1].strip("\n") + ' ! ' + token
                        index_skip = index + 1
                        outputf.write(f"{new_line}\n")
                        ending.append("remove")
                    elif next_line_token == 'USE':
                        new_line = input_lines[index+1].strip("\n") + ' ! ' + token
                        outputf.write(f"{new_line}\n")
                        ending.append("remove")
                        index_skip = index + 1
                    elif next_line_token == 'CALL' or next_line_token == 'WRITE':
                        # Rewrite input_line if needed
                        new_line = 'if (' + token.lower() + '_flag) then'
                        outputf.write (f"{new_line}\n")
                        ending.append("replace")
                    else:
                        if index_skip != index:
                            outputf.write (f"{input_line}\n")
                        ending.append("keep")
                else:
                    outputf.write (f"{input_line}\n")
                    ending.append("keep")
            elif '#if defined' in input_line:
                #print (f"DEBUG: ",input_line)
                outputf.write (f"{input_line}\n")
                ending.append("keep")
            elif '#else' in input_line:
                if ending[-1] == "keep":
                    outputf.write (f"{input_line}\n")
                elif ending[-1] == "replace":
                    outputf.write ("else\n")
                #TODO: add error condition for ending is 'remove'
            elif '#endif' in input_line:
                if ending:
                    if ending[-1] == 'replace':
                        outputf.write(f"end if\n")
                        ending.pop()
                    elif ending[-1] == 'keep':
                        outputf.write (f"#endif\n")
                        ending.pop()
                else:
                    outputf.write (f"#endif\n")
            else:
                if index == index_skip and index != 0:
                    index_skip = 0
                else:
                    outputf.write (f"{input_line}\n")

    os.rename(output_file, input_file)
    command = "emacs --batch -l .emacsloc " + input_file + " -f mark-whole-buffer -f f90-indent-subprogram -f save-buffer"
    print (f"command is {command}")
    os.system(command)

if __name__ == "__main__":
    main()
