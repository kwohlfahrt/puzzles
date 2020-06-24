vlib uart
vcom -2008 -work uart {../uart/*.vhd}

vlib uart_tb_rx
vcom -2008 -work uart_tb_rx {../uart/tbs/rx.vhd}

vlib uart_tb_tx
vcom -2008 -work uart_tb_tx {../uart/tbs/tx.vhd}

vsim -voptargs="+acc" uart_tb_rx.tb1 uart_tb_rx.tb2 uart_tb_rx.tb3 uart_tb_tx.tb1 uart_tb_tx.tb2

if { !([info exists HEADLESS] && !$HEADLESS) } {
    add wave -group rx_tb1 sim:/uart_tb_rx.tb1/*
    add wave -group rx_tb2 sim:/uart_tb_rx.tb2/*
    add wave -group rx_tb3 sim:/tb3/*

    add wave -group tx_tb1 sim:/uart_tb_tx.tb1/*
    add wave -group tx_tb2 sim:/uart_tb_tx.tb2/*
}

run -all

if { !([info exists HEADLESS] && !$HEADLESS) } {
    view structure
    view signals
    wave zoom full
}
