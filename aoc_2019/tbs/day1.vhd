-- Part 1

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library day1;
use day1.util.all;

entity example1 is
end example1;

architecture arch of example1 is
begin
  assert fuel_for(to_unsigned(12, 4)) = 2;
  assert fuel_for(to_unsigned(14, 4)) = 2;
  assert fuel_for(to_unsigned(1969, 11)) = 654;
  assert fuel_for(to_unsigned(100756, 17)) = 33583;
end;

library std;
use std.textio.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library day1;
library uart;
use uart.util.transmit;

entity part1 is
end part1;

architecture arch of part1 is
  signal done : boolean := false;

  signal clk, uart_rx : std_logic := '1';
  signal uart_tx : std_logic;

  constant period : time := (1 sec) / (115200 * 15);
begin
  -- size is 4 * decimal digits
  dut : entity day1.day1 port map ( clk => clk, reset => '0', uart_rx => uart_rx, uart_tx => uart_tx );

  process
    file input : text;
    variable input_line : line;
    variable char : character;
    variable char_bits : std_logic_vector(7 downto 0);
    constant sep : std_logic_vector(7 downto 0) := "00001010";
  begin
    file_open(input, "../day1.txt", read_mode);

    wait for period;
    while not endfile(input) loop
      readline(input, input_line);
      while input_line'length > 0 loop
        read(input_line, char);
        char_bits := std_logic_vector(to_unsigned(character'pos(char), char_bits'length));

        transmit(char_bits, period * 15, uart_rx);
      end loop;
      char_bits := "00001010";
      transmit(char_bits, period * 15, uart_rx);
    end loop;
    wait;
  end process;

  process
    constant expected : string := "3302760";
    variable char_bits : std_logic_vector(7 downto 0);
  begin
    for i in expected'range loop
      wait until uart_tx = '0';
      wait for period;

      char_bits := std_logic_vector(to_unsigned(character'pos(expected(i)), char_bits'length));
      assert uart_tx = '0';
      wait for period * 15;
      for j in char_bits'reverse_range loop
        assert uart_tx = char_bits(j)
          report to_string(uart_tx) & " /= " & to_string(char_bits(j)) & " @ " & to_string(i) & "." & to_string(j);
        wait for period * 15;
      end loop;
      assert uart_tx = '1';
      wait for period;
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
end;

-- Part 2

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library day1;

entity example2 is
end example2;

architecture arch of example2 is
  signal done : boolean := false;

  signal clk : std_logic := '1';
  signal reset : std_logic := '0';
  signal input_valid, output_ready : std_logic := '0';
  signal input_ready, output_valid : std_logic;
  signal input, output : unsigned(20 downto 0);

  constant period : time := 1 ns;
begin
  dut : entity day1.fuel_counter_upper generic map ( size => output'length )
    port map ( clk => clk, reset => reset, part => 2,
               input => input, input_valid => input_valid, input_ready => input_ready,
               output => output, output_valid => output_valid, output_ready => output_ready );

  process
  begin
    wait for period / 4;
    input <= to_unsigned(14, input'length);
    input_valid <= '1';
    output_ready <= '1';
    wait for period;
    input_valid <= '0';

    wait for period;
    reset <= '1';
    wait for period;
    reset <= '0';
    input <= to_unsigned(1969, input'length);
    input_valid <= '1';
    wait for period;
    input_valid <= '0';

    wait for period * 5;
    reset <= '1';
    wait for period;
    reset <= '0';
    input <= to_unsigned(100756, input'length);
    input_valid <= '1';
    wait for period;
    input_valid <= '0';
    wait;
  end process;


  process
  begin
    wait until rising_edge(clk) and output_valid = '1';
    assert output = to_unsigned(2, output'length)
      report to_string(to_integer(output)) & " /= " & to_string(2);
    assert output_valid = '1';

    for i in 0 to 5 loop
      wait until rising_edge(clk) and output_valid = '1';
    end loop;
    assert output = to_unsigned(966, output'length)
      report to_string(to_integer(output)) & " /= " & to_string(966);

    for i in 0 to 9 loop
      wait until rising_edge(clk) and output_valid = '1';
    end loop;
    assert output = to_unsigned(50346, output'length)
      report to_string(to_integer(output)) & " /= " & to_string(50346);
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
end;
