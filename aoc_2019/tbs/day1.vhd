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
library int_io;
use int_io.util.to_ascii;
use int_io.util.from_ascii;

entity part1 is
end part1;

architecture arch of part1 is
  signal done : boolean := false;

  signal clk, out_ready : std_logic := '1';
  signal in_valid : std_logic := '0';
  signal in_ready, out_valid : std_logic;

  signal in_value, out_value : std_logic_vector(7 downto 0);

  constant period : time := 1 ns;
begin
  -- size is 4 * decimal digits
  dut : entity day1.day1
    port map ( clk => clk, reset => '0',
               in_ready => in_ready, in_valid => in_valid, in_value => in_value,
               out_ready => out_ready, out_valid => out_valid, out_value => out_value );

  process
    file input : text;
    variable input_line : line;
    variable char : character;
  begin
    file_open(input, "../day1.txt", read_mode);

    while not endfile(input) loop
      readline(input, input_line);
      while input_line'length > 0 loop
        read(input_line, char);
        in_value <= from_ascii(char);
        in_valid <= '1';

        wait until rising_edge(clk) and in_valid = '1' and in_ready = '1';
        wait for period / 2;
        in_valid <= '0';
        wait for period * 10;
      end loop;
      in_value <= from_ascii(LF);
      in_valid <= '1';
      wait until rising_edge(clk) and in_valid = '1' and in_ready = '1';
      in_valid <= '0';
    end loop;
    wait;
  end process;

  process
    constant expected : string := "3302760";
  begin
    for i in expected'range loop
      wait until rising_edge(clk) and out_valid = '1' and out_ready = '1';

      assert out_value = from_ascii(expected(i))
        report to_ascii(out_value) & " /= " & expected(i) & " @ " & to_string(i);
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
  dut : entity day1.rocket_equation generic map ( size => output'length )
    port map ( clk => clk, reset => reset,
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

    wait until rising_edge(clk) and output_valid = '1';
    assert output = to_unsigned(966, output'length)
      report to_string(to_integer(output)) & " /= " & to_string(966);

    wait until rising_edge(clk) and output_valid = '1';
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

library std;
use std.textio.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library day1;
library uart;
use uart.util.transmit;
library int_io;
use int_io.util.to_ascii;
use int_io.util.from_ascii;

entity part2 is
end part2;

architecture arch of part2 is
  signal done : boolean := false;

  signal clk, out_ready : std_logic := '1';
  signal in_valid : std_logic := '0';
  signal in_ready, out_valid : std_logic;

  signal in_value, out_value : std_logic_vector(7 downto 0);

  constant period : time := 1 ns;
begin
  dut : entity day1.day1
    port map ( clk => clk, reset => '0', part => 2,
               in_ready => in_ready, in_valid => in_valid, in_value => in_value,
               out_ready => out_ready, out_valid => out_valid, out_value => out_value );

  process
    file input : text;
    variable input_line : line;
    variable char : character;
  begin
    file_open(input, "../day1.txt", read_mode);

    while not endfile(input) loop
      readline(input, input_line);
      while input_line'length > 0 loop
        read(input_line, char);
        in_value <= from_ascii(char);
        in_valid <= '1';

        wait until rising_edge(clk) and in_valid = '1' and in_ready = '1';
        wait for period / 2;
        in_valid <= '0';
        -- FIXME: Correctly signal ready
        -- In practice, we have >> 15 clocks delay, for each UART bit
        wait for period * 10;
      end loop;
      in_value <= from_ascii(LF);
      in_valid <= '1';
      wait until rising_edge(clk) and in_valid = '1' and in_ready = '1';
      in_valid <= '0';
    end loop;
    wait;
  end process;

  process
    constant expected : string := "4951265";
  begin
    for i in expected'range loop
      wait until rising_edge(clk) and out_valid = '1' and out_ready = '1';

      assert out_value = from_ascii(expected(i))
        report to_ascii(out_value) & " /= " & expected(i) & " @ " & to_string(i);
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
