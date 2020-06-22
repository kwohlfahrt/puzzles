library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library uart;
library uart_pll;

entity aoc_2019 is
	port ( switches : in std_logic_vector(9 downto 0);
	       buttons : in std_logic_vector(0 to 3);
	       reset : in std_logic;
	       seven_segments : out std_logic_vector(0 to 7 * 4 - 1);
	       leds_red : out std_logic_vector(0 to 9);
	       leds_green : out std_logic_vector(0 to 7);
               oscillator : in std_logic;
               uart_rx : in std_logic;
               uart_tx : out std_logic );
end;

architecture data of aoc_2019 is
	signal full_switches : std_logic_vector(13 downto 0);
	signal uart_clk, uart_valid, uart_ready : std_logic;
	signal checksum : std_logic_vector(7 downto 0) := "00000000";
	signal uart_data : std_logic_vector(7 downto 0);
	signal value : unsigned(13 downto 0);
begin
	full_switches(full_switches'left downto 10) <= "0000";
	full_switches(9 downto 0) <= switches;
        gen_switch_led : for i in switches'range generate
          leds_red(i) <= switches(i);
        end generate;
        leds_green <= checksum;
        uart_clk_src : entity uart_pll.uart_pll
                port map ( refclk => oscillator, rst => reset, outclk_1 => uart_clk );
        uart_recv : entity uart.rx generic map ( bit_clocks => 15 )
		port map ( rx => uart_rx, clk => uart_clk, output => uart_data, valid => uart_valid, ready => uart_ready );
        uart_trans : entity uart.tx generic map ( bit_clocks => 15 )
		port map ( tx => uart_tx, clk => uart_clk, input => uart_data, valid => uart_valid, ready => uart_ready);
        parser : entity work.int_parser generic map ( value_size => value'length )
                port map ( byte => uart_data, byte_ready => uart_valid, value => value );
	display : entity work.seven_segments_dec generic map ( n => 4 )
		port map ( value => value, output => seven_segments );

        process (uart_valid)
        begin
          if rising_edge(uart_valid) then
            checksum <= checksum xor uart_data;
          end if;
        end process;
end;
