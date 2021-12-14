-- Part 1

library std;
use std.textio.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library day2;
library uart;
use uart.util.to_ascii;
use uart.util.from_ascii;

entity example1 is
end example1;

architecture arch of example1 is
  signal done : boolean := false;

  signal clk, out_ready : std_logic := '1';
  signal in_valid : std_logic := '0';
  signal in_ready, out_valid : std_logic;

  signal in_value, out_value : std_logic_vector(7 downto 0);

  constant period : time := 1 ns;
begin
  -- size is 4 * decimal digits
  dut : entity day2.day2
    port map ( clk => clk, reset => '0', part => 0,
               in_ready => in_ready, in_valid => in_valid, in_value => in_value,
               out_ready => out_ready, out_valid => out_valid, out_value => out_value );

  process
    constant input : string := "1,9,10,3,2,3,11,0,99,30,40,50";
  begin
    for i in input'range loop
      in_value <= from_ascii(input(i));
      in_valid <= '1';

      wait until rising_edge(clk) and in_valid = '1' and in_ready = '1';
    end loop;
    in_value <= from_ascii(LF);
    in_valid <= '1';
    wait until rising_edge(clk) and in_valid = '1' and in_ready = '1';
    in_valid <= '0';
    wait;
  end process;

  process
    type expected_t is array (natural range <>) of integer;
    constant expected : expected_t := ( 3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50 );

    type ram_t is array (natural range <>) of unsigned;
    alias ram is << signal .example1.dut.memory.data : ram_t >>;
  begin
    wait until rising_edge(clk) and out_valid = '1';
    for i in expected'range loop
      assert expected(i) = to_integer(ram(i));
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

library std;
use std.textio.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library day2;
library uart;
use uart.util.to_ascii;
use uart.util.from_ascii;

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
  dut : entity day2.day2
    port map ( clk => clk, reset => '0',
               in_ready => in_ready, in_valid => in_valid, in_value => in_value,
               out_ready => out_ready, out_valid => out_valid, out_value => out_value );

  process
    file input : text;
    variable input_line : line;
    variable char : character;
  begin
    file_open(input, "../day2.txt", read_mode);

    while not endfile(input) loop
      readline(input, input_line);
      while input_line'length > 0 loop
        read(input_line, char);
        in_value <= from_ascii(char);
        in_valid <= '1';

        wait until rising_edge(clk) and in_valid = '1' and in_ready = '1';
      end loop;
      in_value <= from_ascii(LF);
      in_valid <= '1';
      wait until rising_edge(clk) and in_valid = '1' and in_ready = '1';
    end loop;
    in_valid <= '0';
    wait;
  end process;

  process
  begin
    wait until rising_edge(clk) and out_valid = '1';
    report to_string(out_value);
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
