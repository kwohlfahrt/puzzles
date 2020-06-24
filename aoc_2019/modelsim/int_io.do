vlib int_io
vcom -2008 -work int_io {../int_io/*.vhd}

vlib int_io_tb
vcom -2008 -work int_io_tb {../int_io/tbs/*.vhd}

vsim -voptargs="+acc" int_io_tb.tb1

if { !([info exists HEADLESS] && !$HEADLESS) } {
    add wave -group tb1 sim:/tb1/*
}

run -all

if { !([info exists HEADLESS] && !$HEADLESS) } {
    view structure
    view signals
    wave zoom full
}
