library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library uart;

entity tb1 is
end tb1;

architecture arch of tb1 is
  signal clk : std_logic := '1';
  signal rx : std_logic := '1';
  signal valid : std_logic;
  signal output : std_logic_vector(7 downto 0);

  constant bit_clocks : positive := 5;
begin
  uart_rx : entity uart.rx generic map (bit_clocks => bit_clocks)
    port map ( rx => rx, clk => clk, output => output, valid => valid );
  process
    type examples_t is array (natural range <>) of std_logic_vector(7 downto 0);
    constant examples : examples_t := ("00001111", "10101011");
  begin
    rx <= '1';
    wait for bit_clocks * 2 ns;

    for i in examples'range loop
      rx <= '0';
      wait for bit_clocks * 100 ps;
      if i > examples'left then
        assert output = examples(i - 1);
        assert valid = '1';
      end if;
      wait for bit_clocks * 900 ps;
      for j in examples(i)'range loop
        rx <= examples(i)(j);
        wait for bit_clocks * 1 ns;
      end loop;
      rx <= '1';
      wait for bit_clocks * 1 ns;
    end loop;
    wait for bit_clocks * 10 ns;
    assert output = examples(examples'right);
    assert valid = '1';
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
  signal clk : std_logic := '1';
  signal rx : std_logic := '1';
  signal valid : std_logic;
  signal output : std_logic_vector(7 downto 0);

  constant bit_clocks : positive := 5;
begin
  uart_rx : entity uart.rx generic map (bit_clocks => bit_clocks)
    port map ( rx => rx, clk => clk, output => output, valid => valid );
  process
  begin
    rx <= '1';
    wait for bit_clocks * 2 ns;
    -- 'start' glitch
    rx <= '0';
    wait for (bit_clocks / 2 - 1) * 1 ns;
    rx <= '1';
    wait for (bit_clocks - bit_clocks / 2 + 1) * 1 ns;
    rx <= '1';
    wait for bit_clocks * 9 ns;
    assert valid = '0';

    rx <= '1';
    wait for bit_clocks * 2 ns;
    rx <= '0';
    wait for bit_clocks * 1 ns;
    rx <= '1';
    wait for bit_clocks * 8 ns;
    -- 'stop' glitch
    rx <= '0';
    wait for bit_clocks * 1 ns;
    assert valid = '0';

    rx <= '1';
    wait for bit_clocks * 2 ns;
    rx <= '0';
    wait for bit_clocks * 1 ns;
    rx <= '1';
    wait for bit_clocks * 10 ns;
    assert valid = '1';

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

entity tb3 is
end tb3;

architecture arch of tb3 is
  signal clk : std_logic := '1';
  signal input : std_logic := '1';
  signal output : std_logic;
  signal trx : std_logic_vector(7 downto 0);
  signal valid : std_logic;

  type examples_t is array (natural range <>) of std_logic_vector(7 downto 0);
  constant examples : examples_t := ("00001111", "10101011");
  constant bit_clocks : positive := 5;
  constant period : time := 1 ns;
begin
  uart_rx : entity uart.rx generic map (bit_clocks => bit_clocks)
    port map ( rx => input, clk => clk, output => trx, valid => valid );
  uart_tx : entity uart.tx generic map (bit_clocks => bit_clocks)
    port map ( tx => output, clk => clk, input => trx, valid => valid );

  -- stimulus
  process
  begin
    wait for bit_clocks * 2 * period;

    for i in examples'range loop
      input <= '0';
      wait for bit_clocks * period;
      for j in examples(i)'range loop
        input <= examples(i)(j);
        wait for bit_clocks * period;
      end loop;
      input <= '1';
      wait for bit_clocks * period;
    end loop;
    wait;
  end process;

  -- monitor
  process
  begin
    -- warm-up
    wait for bit_clocks * 2 * period;
    -- first cycle
    wait for bit_clocks * 10 * period;

    for i in examples'range loop
      wait for bit_clocks * period / 2;
      assert output = '0';
      for j in examples(i)'range loop
        wait for bit_clocks * period;
        assert output = examples(i)(j);
      end loop;
      wait for bit_clocks * period;
      assert output = '1';
      wait for bit_clocks * period / 2;
    end loop;
    report "end of test";
    wait;
  end process;

  process
  begin
    wait for period / 2;
    clk <= not clk;
  end process;
end arch;