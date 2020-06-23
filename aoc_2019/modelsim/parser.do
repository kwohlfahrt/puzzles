vlib parser_tb
vcom -2008 -work parser_tb {../parser.vhd} {../tbs/parser.vhd}

vsim -L parser_tb -voptargs="+acc" parser_tb.tb1

if { !([info exists HEADLESS] && !$HEADLESS) } {
    add wave -group tb1 sim:/tb1/*
}

run -all

if { !([info exists HEADLESS] && !$HEADLESS) } {
    view structure
    view signals
    wave zoom full
}
