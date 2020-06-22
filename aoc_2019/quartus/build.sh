#!/usr/bin/env bash
set -euo pipefail
WORKDIR=$(dirname $BASH_SOURCE)

(cd $WORKDIR
 quartus_map aoc_2019
 quartus_fit aoc_2019
 quartus_asm aoc_2019
 quartus_sta aoc_2019
 quartus_eda aoc_2019)
