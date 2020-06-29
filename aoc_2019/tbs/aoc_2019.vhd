library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library bcd;
use bcd.bcd.all;

use work.seven_segments.seven_segments;

entity tb1 is
end;

architecture structure of tb1 is
  --input
  signal switches : std_logic_vector(9 downto 0) := "0000000000";
  signal buttons : std_logic_vector(0 to 3) := "0000";
  signal oscillator, uart_rx : std_logic := '1';
  signal reset : std_logic := '0';

  -- output
  signal seven_segments : seven_segments(3 downto 0);
  signal leds_red : std_logic_vector(0 to 9);
  signal leds_green : std_logic_vector(0 to 7);
  signal uart_tx : std_logic;

  -- internal
  signal done : boolean := false;
  constant period : time := 20 ns;
  constant uart_period : time := 1 sec / 115200;

  type data_t is array (natural range <>) of std_logic_vector(7 downto 0);
  -- ASCII "23,"
  constant data : data_t := ("00110010", "00110011", "00101100");
  constant output : data_t := ("00110101", "00101100");
begin
  aoc : entity work.aoc_2019
    port map ( switches => switches, buttons => buttons, oscillator => oscillator, reset_button => reset, uart_rx => uart_rx,
               seven_segments => seven_segments, leds_red => leds_red, leds_green => leds_green, uart_tx => uart_tx );

  process
  begin
    wait for 10 ns;
    reset <= '1';
    wait for 2 us;

    for i in data'range loop
      uart_rx <= '0';
      wait for uart_period;
      for j in data(i)'reverse_range loop
        uart_rx <= data(i)(j);
        wait for uart_period;
      end loop;
      uart_rx <= '1';
      wait for uart_period;
    end loop;
    wait;
  end process;

  process
    alias value is << signal .tb1.aoc.value : decimal(5 downto 0) >>;
  begin
    wait for 1 ns;
    assert seven_segments = ( "0000001", "0000001", "0000001", "0000001" );
    wait until value'active;
    assert value = to_decimal(23, value'length);
    wait until done;
    assert seven_segments = ( "0000001", "0000001", "0010010", "0000110" );
    wait;
  end process;

  process
    alias value is << signal .tb1.aoc.value : decimal(5 downto 0) >>;
  begin
    wait for uart_period * 10 * 3;
    wait for uart_period / 2;
    for i in output'range loop
      assert uart_tx = '0';
      wait for uart_period;
      for j in output(i)'reverse_range loop
        assert uart_tx = output(i)(j) report to_string(uart_tx) & " /= " & to_string(output(i)(j));
        wait for uart_period;
      end loop;
      assert uart_tx = '1';
      wait for uart_period;
    end loop;
    done <= true;
    wait;
  end process;

  process
  begin
    while not done loop
      wait for period / 2;
      oscillator <= not oscillator;
    end loop;
    wait;
  end process;
end;
