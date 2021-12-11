vlib uart
vcom -2008 -work uart {../uart/*.vhd}

vlib bcd
vcom -2008 -work bcd {../bcd.vhd}

vlib int_io
vcom -2008 -work int_io {../int_io/*.vhd}

vlib seven_segment
vcom -2008 -work seven_segment {../seven-segment.vhd}

vlib day1
vcom -2008 -work day1 {../day1.vhd}

vlib day1_tb
vcom -2008 -work day1_tb {../tbs/day1.vhd}

vsim -voptargs="+acc" day1_tb.example1 day1_tb.part1 day1_tb.example2 day1_tb.part2

if { !([info exists HEADLESS] && !$HEADLESS) } {
    radix -unsigned
    add wave -group part1 sim:/part1/*
    add wave -group example2 sim:/example2/*
    add wave -group part2 sim:/part2/*
}

run -all

if { !([info exists HEADLESS] && !$HEADLESS) } {
    view structure
    view signals
    wave zoom full
}
