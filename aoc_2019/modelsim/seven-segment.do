vlib seven_segment
vcom -2008 -work seven_segment {../seven-segment.vhd}

vlib seven_segment_tb
vcom -2008 -work seven_segment_tb {../tbs/seven-segment.vhd}

vsim -voptargs="+acc" seven_segment_tb.tb1 seven_segment_tb.tb2

if { !([info exists HEADLESS] && !$HEADLESS) } {
    add wave -group tb1 sim:/tb1/*
    add wave -group tb2 sim:/tb2/*
}

run -all

if { !([info exists HEADLESS] && !$HEADLESS) } {
    view structure
    view signals
    wave zoom full
}