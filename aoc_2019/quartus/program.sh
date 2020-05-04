#!/usr/bin/env bash
set -euo pipefail
WORKDIR=$(dirname $BASH_SOURCE)

(cd $WORKDIR
 quartus_pgm --mode=jtag --operation="p;./output_files/aoc_2019.sof")
