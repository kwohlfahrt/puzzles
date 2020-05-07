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
    type examples_t is array (natural range <>) of std_logic_vector(7 downto 0);
    constant examples : examples_t := ("00001111", "10101011");
  begin
    rx <= '1';
    wait for 30 ns;

    for i in examples'range loop
      rx <= '0';
      wait for 15 ns;
      for j in examples(i)'range loop
        rx <= examples(i)(j);
        wait for 15 ns;
      end loop;
      rx <= '1';
      wait for 15 ns;

      assert output = examples(i);
      assert ready = '1';
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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library uart;

entity tb2 is
end tb2;

architecture arch of tb2 is
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
    wait for 30 ns;
    -- 'start' glitch
    rx <= '0';
    wait for 5 ns;
    rx <= '1';
    wait for 10 ns;
    rx <= '1';
    wait for 9 * 15 ns;
    assert ready = '0';

    rx <= '1';
    wait for 30 ns;
    rx <= '0';
    wait for 15 ns;
    rx <= '1';
    wait for 8 * 15 ns;
    -- 'stop' glitch
    rx <= '0';
    wait for 15 ns;
    assert ready = '0';

    rx <= '1';
    wait for 30 ns;
    rx <= '0';
    wait for 15 ns;
    rx <= '1';
    wait for 9 * 15 ns;
    assert ready = '1';

    report "end of test";
    wait;
  end process;

  process
  begin
    wait for 500 ps;
    clk <= not clk;
  end process;
end arch;
