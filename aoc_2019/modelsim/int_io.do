vlib bcd
vcom -2008 -work bcd {../bcd.vhd}

vlib int_io
vcom -2008 -work int_io {../int_io/*.vhd}

vlib int_io_decode_tb
vcom -2008 -work int_io_decode_tb {../int_io/tbs/decode.vhd}

vlib int_io_encode_tb
vcom -2008 -work int_io_encode_tb {../int_io/tbs/encode.vhd}

vsim -voptargs="+acc" int_io_decode_tb.tb1 int_io_encode_tb.tb1 int_io_encode_tb.tb2

if { !([info exists HEADLESS] && !$HEADLESS) } {
    add wave -group decode_tb1 sim:/int_io_decode_tb.tb1/*
    add wave -group encode_tb1 sim:/int_io_encode_tb.tb1/*
    add wave -group encode_tb2 sim:/tb2/*
}

run -all

if { !([info exists HEADLESS] && !$HEADLESS) } {
    view structure
    view signals
    wave zoom full
}
