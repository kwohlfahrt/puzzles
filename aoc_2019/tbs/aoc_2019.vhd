library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library bcd;
use bcd.bcd.all;

library seven_segment;
use seven_segment.seven_segments.seven_segments;

entity tb1 is
end;

architecture structure of tb1 is
  --input
  signal switches : std_logic_vector(9 downto 0) := "0000000001";
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
  constant uart_period : time := 1 sec / (115200 * 15);
begin
  dut : entity work.aoc_2019 port map (
    switches => switches, buttons => buttons, oscillator => oscillator, reset_button => reset, uart_rx => uart_rx,
    seven_segments => seven_segments, leds_red => leds_red, leds_green => leds_green, uart_tx => uart_tx
  );

  process
  begin
    wait for 10 ns;
    reset <= '1';
    wait;
  end process;

  process
    alias uart_clk is << signal .tb1.dut.uart_clk : std_logic >>;
    alias day is << signal .tb1.dut.day : positive >>;
  begin
    wait for 500 ns;
    -- Check we've stopped after reset
    assert uart_clk = '0';
    wait for uart_period / 2;
    assert uart_clk = '0';

    wait until uart_clk /= '0';

    for i in 1 to 5 loop
      assert uart_clk = '1';
      wait for uart_period / 2;
      assert uart_clk = '0';
      wait for uart_period / 2;
    end loop;

    assert day = 1;
    done <= true;
    report "end of test";
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
