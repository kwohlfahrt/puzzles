library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library uart;

entity tb1 is
end tb1;

architecture arch of tb1 is
  signal clk : std_logic := '0';
  signal rx : std_logic := '1';
  signal ready : std_logic;
  signal output : std_logic_vector(7 downto 0);
begin
  uart_rx : entity uart.rx generic map (bit_clocks => 15)
    port map ( rx => rx, clk => clk, output => output, ready => ready );
  process
  begin
    rx <= '1';
    wait for 15 ns;
    rx <= '0';
    wait for 15 ns;
    rx <= '1';
    wait for 150 ns;
  end process;

  process
  begin
    wait for 500 ps;
    clk <= not clk;
  end process;
end arch;
