library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library uart;
use uart.util."=";

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
    alias state is << signal .tb1.uart_tx.state : uart.util.state_t >>;
    alias phase is << signal .tb1.uart_tx.phase : natural range 1 to bit_clocks >>;
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
    wait for 1 ns;
    -- assert we are ready for next byte
    assert state = state'subtype'left;
    assert phase = phase'subtype'low;
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
  signal input : std_logic_vector(7 downto 0);
  signal output : std_logic_vector(7 downto 0);
  signal trx : std_logic;
  signal tx_valid : std_logic := '0';
  signal rx_valid : std_logic;

  constant bit_clocks : positive := 5;
begin
  uart_tx : entity uart.tx generic map (bit_clocks => bit_clocks)
    port map ( tx => trx, clk => clk, input => input, valid => tx_valid );
  uart_rx : entity uart.rx generic map (bit_clocks => bit_clocks)
    port map ( rx => trx, clk => clk, output => output, valid => rx_valid );
  process
    type examples_t is array (natural range <>) of std_logic_vector(7 downto 0);
    constant examples : examples_t := ("00001111", "10101011");
  begin
    wait for bit_clocks * 2 ns;

    for i in examples'range loop
      input <= examples(i);
      tx_valid <= '1';
      wait for bit_clocks * 1 ns;
      if i > examples'left then
        assert output = examples(i - 1);
      end if;
      wait for bit_clocks * 9 ns;
    end loop;
    tx_valid <= '0';
    wait for 1 ns;
    assert output = examples(examples'right);
    report "end of test";
    wait;
  end process;

  process
  begin
    wait for 500 ps;
    clk <= not clk;
  end process;
end arch;