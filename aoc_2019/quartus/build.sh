#!/usr/bin/env bash
set -euo pipefail
WORKDIR=$(dirname $BASH_SOURCE)

(cd $WORKDIR
 qsys-generate ./uart_clk_pll.qsys --synthesis=VHDL --part=5CGXFC5C6F27C7
 quartus_map aoc_2019
 quartus_fit aoc_2019
 quartus_asm aoc_2019
 quartus_sta aoc_2019
 quartus_eda aoc_2019)
