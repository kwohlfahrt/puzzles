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

---- Part 2
--
--library std;
--
--entity example2 is
--end example2;
--
--architecture behave of example2 is
--  signal mass, fuel : natural;
--begin
--  counter: entity work.total_fuel_counter(behave)
--    port map (mass => mass, fuel => fuel);
--  process
--    type example_t is record
--      mass, fuel : natural;
--    end record;
--    type examples_t is array (natural range <>) of example_t;
--    constant examples : examples_t :=
--      ((12, 2), (1969, 966), (100756, 50346));
--  begin
--    for i in examples'range loop
--      mass <= examples(i).mass;
--      wait for 1 sec;
--      assert fuel = examples(i).fuel
--        report natural'image(fuel) & " = " & natural'image(examples(i).fuel);
--    end loop;
--    report "end of test";
--    wait;
--  end process;
--end behave;
--
--library std;
--use std.textio.all;
--
--entity part2 is
--end part2;
--
--architecture behave of part2 is
--  file input_data : text;
--  signal mass, fuel, fuel_sum : natural;
--begin
--  counter: entity work.total_fuel_counter(behave)
--    port map (mass => mass, fuel => fuel);
--  upper: entity work.counter_upper(behave)
--    port map (input => fuel, reset => false, output => fuel_sum);
--  process
--    variable input_line, output_line : line;
--    variable input_mass : natural;
--  begin
--    file_open(input_data, "./day1.txt", read_mode);
--    while not endfile(input_data) loop
--      readline(input_data, input_line);
--      read(input_line, input_mass);
--      mass <= input_mass;
--      wait for 1 sec;
--    end loop;
--    write(output_line, natural'image(fuel_sum));
--    writeline(output, output_line);
--    file_close(input_data);
--    wait;
--  end process;
--end behave;
