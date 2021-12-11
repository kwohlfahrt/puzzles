library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library uart_pll;
library seven_segment;
use seven_segment.seven_segments.seven_segments;

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
  signal reset, uart_reset, uart_clk : std_logic;
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

  day1 : entity work.day1
    port map ( clk => uart_clk, reset => uart_reset, uart_rx => uart_rx, uart_tx => uart_tx, seven_segments => seven_segments );
end;
