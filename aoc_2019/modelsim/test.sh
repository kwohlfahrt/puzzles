#!/usr/bin/env bash
set -euo pipefail
WORKDIR=$(dirname $BASH_SOURCE)

(cd $WORKDIR
 vsim -c -do ./all.do)
