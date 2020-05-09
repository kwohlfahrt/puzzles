vlib uart_clk_pll
vlog -vlog01compat -work uart_clk_pll +incdir+/home/kai/Documents/code/puzzles/aoc_2019/quartus/db/ip/uart_clk_pll/submodules {/home/kai/Documents/code/puzzles/aoc_2019/quartus/db/ip/uart_clk_pll/submodules/uart_clk_pll_pll.v}

vlib uart
vcom -2008 -work uart {../uart/*.vhd}

vlib rtl_work
vcom -2008 -work work {../uart/tbs/rx.vhd}

vsim -quiet -L altera -L lpm -L sgate -L altera_mf -L altera_lnsim -L cyclonev -L rtl_work -L work -L uart_clk_pll -L uart -voptargs="+acc" tb1 tb2 tb3

if { !([info exists HEADLESS] && !$HEADLESS) } {
    add wave -group tb1 sim:/tb1/*
    add wave -group tb2 sim:/tb2/*
    add wave -group tb3 sim:/tb3/*
}

run -all

if { !([info exists HEADLESS] && !$HEADLESS) } {
    view structure
    view signals
    wave zoom full
}
