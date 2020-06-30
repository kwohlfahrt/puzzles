library ieee;
use ieee.numeric_std.all;

package util is
  function fuel_for( mass : unsigned ) return unsigned;
end package;

package body util is
  function fuel_for( mass : unsigned ) return unsigned is
    variable fuel : unsigned(mass'range) := mass / 3;
  begin
    if fuel < 2 then
      return to_unsigned(0, fuel'length);
    else
      return fuel - 2;
    end if;
  end function;
end;

-- Part 1

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.util.all;

entity counter_upper is
  generic ( size : positive );
  port ( clk: in std_logic;
         reset: in std_logic := '0';
         input : in unsigned(size-1 downto 0);
         input_valid : in std_logic; 
         input_ready : out std_logic := '1'; 
         output : out unsigned(size-1 downto 0);
         output_valid : out std_logic := '0';
         output_ready : in std_logic );
end;

architecture arch of counter_upper is
  signal counter : unsigned(size-1 downto 0) := (others => '0');
  signal new_output, still_valid : boolean := false;
begin
  output <= counter;
  output_valid <= '1' when new_output or still_valid else '0';

  process (clk, reset)
  begin
    if reset = '1' then
      counter <= to_unsigned(0, counter'length);
      new_output <= false;
      still_valid <= false;
    elsif rising_edge(clk) then
      if new_output and output_ready /= '1' then
        still_valid <= true;
      elsif output_ready = '1' then
        still_valid <= false;
      end if;

      new_output <= false;
      if input_valid = '1' then
        counter <= counter + fuel_for(input);
        new_output <= true;
      end if;
    end if;
  end process;
end;

-- Part 2

--entity total_fuel_counter is
--  port (mass : in natural;
--        fuel : out natural);
--end total_fuel_counter;
--
--architecture behave of total_fuel_counter is
--  signal item_mass, item_fuel, sub_total : natural;
--  signal reset : boolean;
--begin
--  counter: entity work.fuel_counter(behave)
--    port map (mass => item_mass, fuel => item_fuel);
--  upper: entity work.counter_upper(behave)
--    port map (input => item_fuel, reset => reset, output => sub_total);
--  process
--  begin
--    wait on mass;
--    reset <= false;
--    item_mass <= mass;
--    wait on item_fuel;
--    while item_fuel /= 0 loop
--      item_mass <= item_fuel;
--      wait on item_fuel;
--    end loop;
--    fuel <= sub_total;
--    reset <= true;
--  end process;
--end behave;

library ieee;
use ieee.std_logic_1164.all;

-- No generics in Quartus, so data must be passed around
entity nth is
  generic ( n : positive );
  port ( clk : in std_logic;
         reset : in std_logic := '0';
         input_ready : buffer std_logic;
         input_valid : in std_logic;
         output_ready : in std_logic;
         output_valid : out std_logic );
end;

architecture arch of nth is
  subtype counter_t is integer range 0 to n - 1;
  signal counter : counter_t := 0;
begin
  input_ready <= '1' when counter < counter_t'high else output_ready;
  output_valid <= '0' when counter < counter_t'high else input_valid;

  process (clk, reset)
  begin
    if reset then
      counter <= 0;
    elsif rising_edge(clk) then
      if input_ready and input_valid then
        if counter < counter_t'high then
          counter <= counter + 1;
        else
          counter <= 0;
        end if;
      end if;
    end if;
  end process;
end;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library uart;
library int_io;
library bcd;
use bcd.bcd.all;

entity day1 is
  port ( clk : in std_logic;
         reset : in std_logic := '0';
         uart_rx : in std_logic;
         uart_tx : out std_logic );
end;

architecture arch of day1 is
  signal uart_in_valid, uart_in_ready,
    uart_out_valid, uart_out_ready,
    value_valid, value_ready,
    count_valid, count_ready,
    result_valid, result_ready : std_logic;
  signal uart_in, uart_out : std_logic_vector(7 downto 0);
  signal value : decimal(5 downto 0);
  signal count_dec : decimal(6 downto 0);
  signal count : unsigned(4  * count_dec'length - 1 downto 0);
  signal done : boolean;
begin
  uart_recv : entity uart.rx generic map ( bit_clocks => 15, stop_slack => 1 )
    port map ( rx => uart_rx, clk => clk, output => uart_in, valid => uart_in_valid, ready => uart_in_ready );
  decoder : entity int_io.decode generic map ( value_size => value'length, sep => "00001010" )
    port map ( clk => clk, reset => reset,
               byte => uart_in, byte_valid => uart_in_valid, byte_ready => uart_in_ready,
               value => value, value_valid => value_valid, value_ready => value_ready );
  counter : entity work.counter_upper generic map ( size => count'length )
    port map ( clk => clk, reset => reset,
               input => to_unsigned(value, count'length), input_valid => value_valid, input_ready => value_ready,
               output => count, output_valid => count_valid, output_ready => count_ready );
  count_dec <= to_decimal(count, count_dec'length);

  -- No marker for the last element, so we need to count
  nth : entity work.nth generic map ( n => 100 )
    port map ( clk => clk, reset => reset,
               input_valid => count_valid, input_ready => count_ready,
               output_valid => result_valid, output_ready => result_ready );

  encoder : entity int_io.encode generic map ( value_size => count_dec'length, sep => "00001101"  )
    port map ( clk => clk, reset => reset,
               value => count_dec, value_valid => result_valid, value_ready => result_ready,
               byte => uart_out, byte_valid => uart_out_valid, byte_ready => uart_out_ready );
  uart_trans : entity uart.tx generic map ( bit_clocks => 15, stop_slack => 1 )
    port map ( clk => clk, tx => uart_tx, input => uart_out, valid => uart_out_valid, ready => uart_out_ready );
end;
