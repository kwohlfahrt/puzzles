vlib uart_pll
vcom -2008 -work uart_pll {../quartus/uart_pll_sim/uart_pll.vho}

vlib uart
vcom -2008 -work uart {../uart/*.vhd}

vlib int_io
vcom -2008 -work uart {../int_io/*.vhd}

vlib rtl_work
vcom -2008 -work work {../seven-segment.vhd} {../aoc_2019.vhd} {../tbs/aoc_2019.vhd}

vsim -L altera -L lpm -L sgate -L altera_mf -L altera_lnsim -L cyclonev -L rtl_work -L int_io -L work -L uart_clk_pll -L uart -voptargs="+acc" tb1

if { !([info exists HEADLESS] && !$HEADLESS) } {
    add wave -group tb1 sim:/tb1/*
    add wave -group tb1 sim:/tb1/aoc/uart_clk
}

run {300 us}

if { !([info exists HEADLESS] && !$HEADLESS) } {
    view structure
    view signals
    wave zoom full
}