vlib uart_pll
vcom -2008 -work uart_pll {../quartus/uart_pll_sim/uart_pll.vho}

vlib uart
vcom -2008 -work uart {../uart/*.vhd}

vlib bcd
vcom -2008 -work bcd {../bcd.vhd}

vlib int_io
vcom -2008 -work int_io {../int_io/*.vhd}

vlib seven_segment
vcom -2008 -work seven_segment {../seven-segment.vhd}

vlib aoc_2019_tb
vcom -2008 -work aoc_2019_tb {../reset.vhd} {../day1.vhd} {../day2.vhd} {../aoc_2019.vhd} {../tbs/aoc_2019.vhd}

vsim -L altera_lnsim -L cyclonev -voptargs="+acc" aoc_2019_tb.tb1

if { !([info exists HEADLESS] && !$HEADLESS) } {
    add wave -group tb1 sim:/tb1/*
    add wave -group tb1 sim:/tb1/dut/uart_clk
}

run 5 us

if { !([info exists HEADLESS] && !$HEADLESS) } {
    view structure
    view signals
    wave zoom full
}
