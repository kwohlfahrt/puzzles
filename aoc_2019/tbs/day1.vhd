-- Part 1

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library day1;
use day1.day1.all;

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

entity part1 is
end part1;

architecture arch of part1 is
  constant size : positive := 22;
  constant period : time := 1 ns;
  file input_data : text;
  signal done : boolean := false;

  signal clk : std_logic := '1';
  signal input_valid : std_logic := '0';
  signal output_valid : std_logic;
  signal input, output : unsigned(size - 1 downto 0);
begin
  counter_upper: entity day1.counter_upper(behave) generic map ( size => size )
    port map ( clk => clk, input => input, input_valid => input_valid,
               output => output, output_valid => output_valid, output_ready => '1' );

  process
    variable input_line : line;
    variable input_mass : natural;
  begin
    wait for period / 4;
    input_valid <= '1';
    file_open(input_data, "../day1.txt", read_mode);
    while not endfile(input_data) loop
      readline(input_data, input_line);
      read(input_line, input_mass);
      input <= to_unsigned(input_mass, input'length);
      wait for period;
    end loop;
    assert to_integer(output) = 3302760 report to_string(to_integer(output)) & " /= 3302760";
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
