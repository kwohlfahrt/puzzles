library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity aoc_2019 is
	port ( switches : in std_logic_vector(9 downto 0);
	       buttons : in std_logic_vector(0 to 3);
	       seven_segments : out std_logic_vector(0 to 7 * 4 - 1);
	       leds_red : out std_logic_vector(0 to 9);
	       leds_green : out std_logic_vector(0 to 7);
               oscillator : in std_logic;
               uart_rx : in std_logic;
               uart_tx : out std_logic );
end;

architecture data of aoc_2019 is
	signal full_switches : std_logic_vector(13 downto 0);
	signal uart_clk, uart_clk_reset : std_logic;
begin
	full_switches(full_switches'left downto 10) <= "0000";
	full_switches(9 downto 0) <= switches;
        leds_red(0 to 3) <= buttons;
        leds_red(4 to 8) <= "00000";
        leds_red(9) <= uart_clk;
        uart_pll : entity work.uart_clk_pll
                port map ( refclk_clk => oscillator, reset_reset => uart_clk_reset, outclk_clk => uart_clk );
	display : entity work.seven_segments_dec generic map ( n => 4 )
		port map ( value => unsigned(full_switches), output => seven_segments );
        uart : entity work.uart
		port map ( rx => uart_rx, tx => uart_tx,
                           clk => uart_clk, clk_reset => uart_clk_reset,
                           input => "00000000", output => leds_green(0 to 7) );
end;
