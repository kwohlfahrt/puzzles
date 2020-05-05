-- Part 1

library std;

entity example1 is
end example1;

architecture behave of example1 is
  signal mass, fuel : natural;
begin
  counter: entity work.fuel_counter(behave)
    port map (mass => mass, fuel => fuel);
  process
    type example_t is record
      mass, fuel : natural;
    end record;
    type examples_t is array (natural range <>) of example_t;
    constant examples : examples_t :=
      ((12, 2), (14, 2), (1969, 654), (100756, 33583));
  begin
    for i in examples'range loop
      mass <= examples(i).mass;
      wait for 1 sec;
      assert fuel = examples(i).fuel
        report natural'image(fuel) & " = " & natural'image(examples(i).fuel);
    end loop;
    report "end of test";
    wait;
  end process;
end behave;

library std;
use std.textio.all;

entity part1 is
end part1;

architecture behave of part1 is
  file input_data : text;
  signal mass, fuel, fuel_sum : natural;
begin
  counter: entity work.fuel_counter(behave)
    port map (mass => mass, fuel => fuel);
  upper: entity work.counter_upper(behave)
    port map (input => fuel, reset => false, output => fuel_sum);
  process
    variable input_line, output_line : line;
    variable input_mass : natural;
  begin
    file_open(input_data, "./day1.txt", read_mode);
    while not endfile(input_data) loop
      readline(input_data, input_line);
      read(input_line, input_mass);
      mass <= input_mass;
      wait for 1 sec;
    end loop;
    write(output_line, natural'image(fuel_sum));
    writeline(output, output_line);
    file_close(input_data);
    wait;
  end process;
end behave;

-- Part 2

library std;

entity example2 is
end example2;

architecture behave of example2 is
  signal mass, fuel : natural;
begin
  counter: entity work.total_fuel_counter(behave)
    port map (mass => mass, fuel => fuel);
  process
    type example_t is record
      mass, fuel : natural;
    end record;
    type examples_t is array (natural range <>) of example_t;
    constant examples : examples_t :=
      ((12, 2), (1969, 966), (100756, 50346));
  begin
    for i in examples'range loop
      mass <= examples(i).mass;
      wait for 1 sec;
      assert fuel = examples(i).fuel
        report natural'image(fuel) & " = " & natural'image(examples(i).fuel);
    end loop;
    report "end of test";
    wait;
  end process;
end behave;

library std;
use std.textio.all;

entity part2 is
end part2;

architecture behave of part2 is
  file input_data : text;
  signal mass, fuel, fuel_sum : natural;
begin
  counter: entity work.total_fuel_counter(behave)
    port map (mass => mass, fuel => fuel);
  upper: entity work.counter_upper(behave)
    port map (input => fuel, reset => false, output => fuel_sum);
  process
    variable input_line, output_line : line;
    variable input_mass : natural;
  begin
    file_open(input_data, "./day1.txt", read_mode);
    while not endfile(input_data) loop
      readline(input_data, input_line);
      read(input_line, input_mass);
      mass <= input_mass;
      wait for 1 sec;
    end loop;
    write(output_line, natural'image(fuel_sum));
    writeline(output, output_line);
    file_close(input_data);
    wait;
  end process;
end behave;
