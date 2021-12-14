vlib uart
vcom -2008 -work uart {../uart/*.vhd}

vlib bcd
vcom -2008 -work bcd {../bcd.vhd}

vlib int_io
vcom -2008 -work int_io {../int_io/*.vhd}

vlib seven_segment
vcom -2008 -work seven_segment {../seven-segment.vhd}

vlib ram
vcom -2008 -work ram {../ram.vhd}

vlib day2
vcom -2008 -work day2 {../day2.vhd}

vlib day2_tb
vcom -2008 -work day2_tb {../tbs/day2.vhd}

vsim -voptargs="+acc" day2_tb.example1 day2_tb.part1

if { !([info exists HEADLESS] && !$HEADLESS) } {
    radix -unsigned
    add wave -group example1 sim:/example1/*
    add wave -group part1 sim:/part1/*
}

run -all

if { !([info exists HEADLESS] && !$HEADLESS) } {
    view structure
    view signals
    wave zoom full
}
