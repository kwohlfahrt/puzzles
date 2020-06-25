vlib bcd
vcom -2008 -work bcd {../bcd.vhd}

vlib bcd_tb
vcom -2008 -work bcd_tb {../tbs/bcd.vhd}

vsim -voptargs="+acc" bcd_tb.tb1

run -all