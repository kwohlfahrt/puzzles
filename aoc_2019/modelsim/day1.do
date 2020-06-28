vlib day1
vcom -2008 -work day1 {../day1.vhd}

vlib day1_tb
vcom -2008 -work day1_tb {../tbs/day1.vhd}

vsim -voptargs="+acc" day1_tb.example1 day1_tb.part1

if { !([info exists HEADLESS] && !$HEADLESS) } {
    add wave -group part1 sim:/part1/*
}

run -all

if { !([info exists HEADLESS] && !$HEADLESS) } {
    view structure
    view signals
    wave zoom full
}