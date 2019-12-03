#!/usr/bin/env bash

docker run --rm \
	--mount type=bind,source="$(pwd)",target=/workspace \
	--workdir /workspace \
	ghdl/ghdl:buster-mcode ghdl $@
