library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb1 is
end;

architecture structure of tb1 is

  signal clk : std_logic := '1';
  signal input : std_logic_vector(7 downto 0) := "00000000";
  signal output : unsigned(15 downto 0);

  signal byte_ready, value_valid : std_logic;
  signal byte_valid : std_logic := '0';
  signal value_ready : std_logic := '1';

  -- internal
  signal done : boolean := false;
  constant period : time := 1 ns;
begin
  parser : entity work.int_parser generic map ( value_size => output'length )
    port map ( clk => clk, byte => input, byte_valid => byte_valid, byte_ready => byte_ready,
               value => output, value_valid => value_valid, value_ready => value_ready );
  process
  begin
    wait for period / 10;
    byte_valid <= '1';
    input <= "00110010"; -- 2
    wait for period;
    input <= "00110011"; -- 3
    wait for period;
    input <= "00101100"; -- ','
    wait for period;
    byte_valid <= '0';
    wait for period;
    done <= true;
    wait;
  end process;

  process
  begin
    wait for 3 * period;
    assert value_valid = '0';
    wait for period;
    assert output = 23;
    assert value_valid = '1';
    wait for period;
    assert output = 23;
    assert value_valid = '0';
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
