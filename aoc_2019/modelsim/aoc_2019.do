vlib uart_pll
vcom -2008 -work uart_pll {../quartus/uart_pll_sim/uart_pll.vho}

vlib aoc_2019_tb
vcom -2008 -work aoc_2019_tb {../seven-segment.vhd} {../reset.vhd} {../day1.vhd} {../aoc_2019.vhd} {../tbs/aoc_2019.vhd}

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