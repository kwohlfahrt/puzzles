library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library uart_pll;
library uart;
library bcd;
use bcd.bcd.to_decimal;

library seven_segment;
use seven_segment.seven_segments.seven_segments_t;

entity aoc_2019 is
  port ( switches : in std_logic_vector(9 downto 0);
         buttons : in std_logic_vector(0 to 3);
         reset_button : in std_logic;
         seven_segments : out seven_segments_t(3 downto 0);
         leds_red : out std_logic_vector(0 to 9);
         leds_green : out std_logic_vector(0 to 7);
         oscillator : in std_logic;
         uart_rx : in std_logic;
         uart_tx : out std_logic );
end;

architecture data of aoc_2019 is
  signal reset, uart_reset, uart_clk : std_logic;

  signal uart_in, uart_out, day1_out, day2_out : std_logic_vector(7 downto 0);
  signal uart_in_valid, uart_in_ready, uart_out_valid, uart_out_ready,
    day1_in_ready, day1_out_valid,
    day2_in_ready, day2_out_valid : std_logic;
  signal day : positive;
begin
  reset <= not reset_button;
  day <= to_integer(unsigned(switches));

  gen_switch_led : for i in switches'range generate
    leds_red(i) <= switches(i);
  end generate;
  gen_green_led : for i in buttons'range generate
    leds_green(2 * i) <= buttons(i);
    leds_green(2 * i + 1) <= buttons(i);
  end generate;

  display : entity seven_segment.seven_segments_dec generic map ( n => 4 )
    port map ( value => to_decimal(day, seven_segments'length), output => seven_segments );

  uart_clk_src : entity uart_pll.uart_pll
    port map ( refclk => oscillator, rst => reset, outclk_1 => uart_clk );
  reset_synchronizer : entity work.synchronizer
    port map ( clk => uart_clk, async_reset => reset, sync_reset => uart_reset );

  uart_recv : entity uart.rx generic map ( bit_clocks => 15, stop_slack => 1 )
    port map ( rx => uart_rx, clk => uart_clk, output => uart_in, valid => uart_in_valid, ready => uart_in_ready );

  uart_trans : entity uart.tx generic map ( bit_clocks => 15, stop_slack => 1 )
    port map ( clk => uart_clk, tx => uart_tx, input => uart_out, valid => uart_out_valid, ready => uart_out_ready );

  with day select uart_in_ready <=
    day1_in_ready when 1,
    day2_in_ready when 2,
    '-' when others;

  with day select uart_out_valid <=
    day1_out_valid when 1,
    day2_out_valid when 2,
    '-' when others;

  with day select uart_out <=
    day1_out when 1,
    day2_out when 2,
    (others => '-') when others;

  day1 : entity work.day1
    port map ( clk => uart_clk, reset => uart_reset,
               in_ready => day1_in_ready, in_valid => uart_in_valid, in_value => uart_in,
               out_ready => uart_out_ready, out_valid => day1_out_valid, out_value => day1_out );

  day2 : entity work.day2
    port map ( clk => uart_clk, reset => uart_reset,
               in_ready => day2_in_ready, in_valid => uart_in_valid, in_value => uart_in,
               out_ready => uart_out_ready, out_valid => day2_out_valid, out_value => day2_out );
end;
