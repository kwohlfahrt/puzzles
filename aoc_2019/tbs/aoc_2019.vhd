library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb1 is
end;

architecture structure of tb1 is
  --input
  signal switches : std_logic_vector(9 downto 0) := "0000000000";
  signal buttons : std_logic_vector(0 to 3) := "0000";
  signal oscillator, reset, uart_rx : std_logic := '1';

  -- output
  signal seven_segments : std_logic_vector(0 to 7 * 4 - 1);
  signal leds_red : std_logic_vector(0 to 9);
  signal leds_green : std_logic_vector(0 to 7);
  signal uart_tx : std_logic;

  -- internal
  signal done : boolean := false;
  constant period : time := 20 ns;
begin
  aoc : entity work.aoc_2019
    port map ( switches => switches, buttons => buttons, oscillator => oscillator, reset => reset, uart_rx => uart_rx,
               seven_segments => seven_segments, leds_red => leds_red, leds_green => leds_green, uart_tx => uart_tx );

  process
  begin
    wait for 10 ns;
    reset <= '0';
    wait for 3000 ns;
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
