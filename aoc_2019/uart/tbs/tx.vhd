library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library uart;

entity tb1 is
end tb1;

architecture arch of tb1 is
  signal clk : std_logic := '1';
  signal input : std_logic_vector(7 downto 0);
  signal tx : std_logic;
  signal ready : std_logic;
  signal valid : std_logic := '0';
  constant bit_clocks : positive := 5;
begin
  uart_tx : entity uart.tx generic map (bit_clocks => bit_clocks)
    port map ( tx => tx, clk => clk, input => input, valid => valid, ready => ready );
  process
    type examples_t is array (natural range <>) of std_logic_vector(7 downto 0);
    constant examples : examples_t := ("00001111", "10101011");
  begin
    wait for bit_clocks * 2 ns;

    for i in examples'range loop
      input <= examples(i);
      valid <= '1';
      -- start bit
      wait for bit_clocks * 1 ns;
      for j in examples(i)'range loop
        wait for bit_clocks * 100 ps;
        --assert tx = examples(i)(j);
        wait for bit_clocks * 800 ps;
        --assert tx = examples(i)(j);
        wait for bit_clocks * 100 ps;
      end loop;
      valid <= '0';
      -- stop bit
      wait for bit_clocks * 1 ns;
    end loop;
    report "end of test";
    wait;
  end process;

  process
  begin
    wait for 500 ps;
    clk <= not clk;
  end process;
end arch;
