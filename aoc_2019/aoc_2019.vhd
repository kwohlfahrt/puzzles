library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library bcd;
use bcd.bcd.all;

library int_io;
library uart;
library uart_pll;

use work.seven_segments.seven_segments;

entity aoc_2019 is
	port ( switches : in std_logic_vector(9 downto 0);
	       buttons : in std_logic_vector(0 to 3);
	       reset_button : in std_logic;
	       seven_segments : out seven_segments(3 downto 0);
	       leds_red : out std_logic_vector(0 to 9);
	       leds_green : out std_logic_vector(0 to 7);
               oscillator : in std_logic;
               uart_rx : in std_logic;
               uart_tx : out std_logic );
end;

architecture data of aoc_2019 is
	signal reset, uart_reset, uart_clk,
          uart_in_valid, uart_in_ready,
          uart_out_valid, uart_out_ready,
          dec_value_valid, dec_value_ready : std_logic;
	signal uart_in, uart_out : std_logic_vector(7 downto 0);
	signal dec_value : decimal(3 downto 0);
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
        reset_synchronizer : entity work.synchronizer
                port map ( clk => uart_clk, async_reset => reset, sync_reset => uart_reset );

        uart_recv : entity uart.rx generic map ( bit_clocks => 15, stop_slack => 1 )
		port map ( rx => uart_rx, clk => uart_clk, output => uart_in, valid => uart_in_valid, ready => uart_in_ready );
        decoder : entity int_io.decode generic map ( value_size => dec_value'length )
                port map ( clk => uart_clk, reset => uart_reset,
                           byte => uart_in, byte_valid => uart_in_valid, byte_ready => uart_in_ready,
                           value => dec_value, value_valid => dec_value_valid, value_ready => dec_value_ready );
        encoder : entity int_io.encode generic map ( value_size => dec_value'length )
                port map ( clk => uart_clk, reset => uart_reset,
                           byte => uart_out, byte_valid => uart_out_valid, byte_ready => uart_out_ready,
                           value => dec_value, value_valid => dec_value_valid, value_ready => dec_value_ready );
        uart_trans : entity uart.tx generic map ( bit_clocks => 15, stop_slack => 1 )
		port map ( tx => uart_tx, clk => uart_clk, input => uart_out, valid => uart_out_valid, ready => uart_out_ready );
	display : entity work.seven_segments_dec generic map ( n => 4 )
		port map ( value => dec_value, output => seven_segments );
end;
