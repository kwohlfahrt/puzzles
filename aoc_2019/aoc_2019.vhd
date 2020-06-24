library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library int_io;
library uart;
library uart_pll;

entity aoc_2019 is
	port ( switches : in std_logic_vector(9 downto 0);
	       buttons : in std_logic_vector(0 to 3);
	       reset_button : in std_logic;
	       seven_segments : out std_logic_vector(0 to 7 * 4 - 1);
	       leds_red : out std_logic_vector(0 to 9);
	       leds_green : out std_logic_vector(0 to 7);
               oscillator : in std_logic;
               uart_rx : in std_logic;
               uart_tx : out std_logic );
end;

architecture data of aoc_2019 is
	signal reset, uart_clk, uart_valid, uart_ready, value_valid : std_logic;
	signal uart_data : std_logic_vector(7 downto 0);
	signal value : unsigned(13 downto 0);
begin
        reset <= not reset_button;

        gen_switch_led : for i in switches'range generate
          leds_red(i) <= switches(i);
        end generate;
        gen_green_led : for i in buttons'range generate
          leds_green(2 * i) <= buttons(i);
          leds_green(2 * i + 1) <= buttons(i);
        end generate;

        uart_clk_src : entity uart_pll.uart_pll
                port map ( refclk => oscillator, rst => reset, outclk_1 => uart_clk );
        uart_recv : entity uart.rx generic map ( bit_clocks => 15 )
		port map ( rx => uart_rx, clk => uart_clk, output => uart_data, valid => uart_valid, ready => uart_ready );
        parser : entity int_io.decode generic map ( value_size => value'length )
                port map ( clk => uart_clk, byte => uart_data, byte_valid => uart_valid, byte_ready => uart_ready,
                           value => value, value_valid => value_valid, value_ready => '1' );
        uart_trans : entity uart.tx generic map ( bit_clocks => 15 )
		port map ( tx => uart_tx, clk => uart_clk, input => uart_data, valid => '0', ready => open);
	display : entity work.seven_segments_dec generic map ( n => 4 )
		port map ( value => value, output => seven_segments );
end;
