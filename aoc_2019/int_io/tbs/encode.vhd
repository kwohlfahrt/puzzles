library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library bcd;
use bcd.bcd.all;

library int_io;

entity tb1 is
end;

architecture structure of tb1 is
  signal clk : std_logic := '1';
  signal input : decimal(1 downto 0);
  signal output : std_logic_vector(7 downto 0);

  signal value_ready, byte_valid : std_logic;
  signal value_valid : std_logic := '0';
  signal byte_ready : std_logic := '1';

  -- internal
  signal done : boolean := false;
  constant period : time := 1 ns;
begin
  encoder : entity int_io.encode generic map ( value_size => input'length )
    port map ( clk => clk, value => input, value_valid => value_valid, value_ready => value_ready,
               byte => output, byte_valid => byte_valid, byte_ready => byte_ready );
  process
  begin
    wait for period / 10;
    value_valid <= '1';
    input <= to_decimal(1, input'length);
    wait for period;
    value_valid <= '0';
    wait for period * 2;
    done <= true;
    wait;
  end process;

  process
  begin
    wait for period;
    assert byte_valid = '0';
    wait for period;
    assert byte_valid = '1';
    assert output = "00110001";
    wait for period;
    assert byte_valid = '1';
    assert output = "00101100";
    wait for period;
    assert byte_valid = '0';
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
end;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library bcd;
use bcd.bcd.all;

library int_io;

entity tb2 is
end;

architecture structure of tb2 is
  signal clk : std_logic := '1';
  signal input : decimal(1 downto 0);
  signal output : std_logic_vector(7 downto 0);

  signal value_ready, byte_valid : std_logic;
  signal value_valid : std_logic := '0';
  signal byte_ready : std_logic := '1';

  -- internal
  signal done : boolean := false;
  constant period : time := 1 ns;

  type examples_t is array (natural range <>) of std_logic_vector(7 downto 0);
  constant examples : examples_t := (
    "00110001", "00110010", "00101100",
    "00110100", "00110011", "00101100"
  );
begin
  encoder : entity int_io.encode generic map ( value_size => input'length )
    port map ( clk => clk, value => input, value_valid => value_valid, value_ready => value_ready,
               byte => output, byte_valid => byte_valid, byte_ready => byte_ready );

  process
  begin
    wait for period / 10;
    value_valid <= '1';
    input <= to_decimal(12, input'length);
    wait for period * 3;
    input <= to_decimal(43, input'length);
    wait for period * 1;
    value_valid <= '0';
    wait for period * 3;
    done <= true;
    wait;
  end process;

  process
  begin
    wait for period;
    assert byte_valid = '0';
    for i in examples'range loop
      wait for period;
      assert byte_valid = '1';
      assert output = examples(i) report to_string(output) & " /= " & to_string(examples(i));
    end loop;
    wait for period;
    assert byte_valid = '0';
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
end;
