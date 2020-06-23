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
  signal done : boolean := false;

  type examples_t is array (natural range <>) of std_logic_vector(7 downto 0);
  constant examples : examples_t := ("00001111", "10101011");
  constant bit_clocks : positive := 5;
  constant period : time := 1 ns;
begin
  uart_rx : entity uart.rx generic map (bit_clocks => bit_clocks)
    port map ( rx => rx, clk => clk, output => output, valid => valid, ready => '1' );

  -- stimulus
  process
  begin
    rx <= '1';
    wait for bit_clocks * 2 * period;

    for i in examples'range loop
      rx <= '0';
      wait for bit_clocks * period;
      for j in examples(i)'reverse_range loop
        rx <= examples(i)(j);
        wait for bit_clocks * period;
      end loop;
      rx <= '1';
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
    -- offset
    wait for period / 10;

    for i in examples'range loop
      assert output = examples(i);
      assert valid = '1';
      wait for bit_clocks * 10 * period;
    end loop;

    report "end of test";
    done <= true;
    wait;
  end process;

  process
  begin
    while not done loop
      wait for period / 2;
      clk <= not clk;
    end loop;
    wait;
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
  signal done : boolean := false;

  constant bit_clocks : positive := 5;
  constant period : time := 1 ns;
begin
  uart_rx : entity uart.rx generic map (bit_clocks => bit_clocks)
    port map ( rx => rx, clk => clk, output => output, valid => valid, ready => '1' );
  process
  begin
    rx <= '1';
    wait for bit_clocks * 2 * period;
    -- 'start' glitch
    rx <= '0';
    wait for (bit_clocks / 2 - 1) * period;
    rx <= '1';
    wait for (bit_clocks - bit_clocks / 2 + 1) * period;
    rx <= '1';
    wait for bit_clocks * 9 * period;
    assert valid = '0';

    rx <= '1';
    wait for bit_clocks * 2 * period;
    rx <= '0';
    wait for bit_clocks * period;
    rx <= '1';
    wait for bit_clocks * 8 * period;
    -- 'stop' glitch
    rx <= '0';
    wait for bit_clocks * period;
    assert valid = '0';

    rx <= '1';
    wait for bit_clocks * 2 * period;
    rx <= '0';
    wait for bit_clocks * period;
    rx <= '1';
    wait for bit_clocks * 9 * period;
    wait for 1 * period;
    assert valid = '1';

    report "end of test";
    done <= true;
    wait;
  end process;

  process
  begin
    while not done loop
      wait for period / 2;
      clk <= not clk;
    end loop;
    wait;
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
  signal valid, ready : std_logic;
  signal done : boolean := false;

  type examples_t is array (natural range <>) of std_logic_vector(7 downto 0);
  constant examples : examples_t := ("00001111", "10101011");
  constant bit_clocks : positive := 5;
  constant period : time := 1 ns;
begin
  uart_rx : entity uart.rx generic map (bit_clocks => bit_clocks)
    port map ( rx => input, clk => clk, output => trx, valid => valid, ready => ready );
  uart_tx : entity uart.tx generic map (bit_clocks => bit_clocks)
    port map ( tx => output, clk => clk, input => trx, valid => valid, ready => ready );

  -- stimulus
  process
  begin
    wait for bit_clocks * 2 * period;

    for i in examples'range loop
      input <= '0';
      wait for bit_clocks * period;
      for j in examples(i)'reverse_range loop
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
      for j in examples(i)'reverse_range loop
        wait for bit_clocks * period;
        assert output = examples(i)(j);
      end loop;
      wait for bit_clocks * period;
      assert output = '1';
      wait for bit_clocks * period / 2;
    end loop;
    report "end of test";
    done <= true;
    wait;
  end process;

  process
  begin
    while not done loop
      wait for period / 2;
      clk <= not clk;
    end loop;
    wait;
  end process;
end arch;
