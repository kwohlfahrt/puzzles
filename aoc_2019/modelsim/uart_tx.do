vlib uart
vcom -2008 -work uart {../uart/*.vhd}

vlib rtl_work
vcom -2008 -work work {../uart/tbs/tx.vhd}

vsim -quiet -L altera -L lpm -L sgate -L altera_mf -L altera_lnsim -L cyclonev -L rtl_work -L work -L uart_clk_pll -L uart -voptargs="+acc" tb1 tb2

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
