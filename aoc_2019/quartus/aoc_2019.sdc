create_clock -name oscillator -period "50MHz" [get_ports oscillator]
set_false_path -from * -to [get_ports leds_red[*]]
set_false_path -from * -to [get_ports leds_green[*]]