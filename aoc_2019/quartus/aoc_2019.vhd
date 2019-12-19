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
	signal uart_clk, uart_ready : std_logic;
	signal checksum : std_logic_vector(7 downto 0) := "00000000";
	signal uart_data : std_logic_vector(7 downto 0);
begin
	full_switches(full_switches'left downto 10) <= "0000";
	full_switches(9 downto 0) <= switches;
        gen_switch_led : for i in switches'range generate
          leds_red(i) <= switches(i);
        end generate;
        leds_green <= checksum;
        uart_pll : entity work.uart_clk_pll
                port map ( refclk_clk => oscillator, reset_reset => '0', outclk_clk => uart_clk );
	display : entity work.seven_segments_dec generic map ( n => 4 )
		port map ( value => unsigned(full_switches), output => seven_segments );
        uart : entity work.uart generic map ( bit_samples => 15 )
		port map ( rx => uart_rx, clk => uart_clk, output => uart_data, ready => uart_ready );

        process (uart_ready)
        begin
          if rising_edge(uart_ready) then
            checksum <= checksum xor uart_data;
          end if;
        end process;
end;
