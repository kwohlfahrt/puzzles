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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter_upper is
  generic ( size : positive );
  port ( clk : in std_logic;
         reset : in std_logic := '0';
         input : in unsigned(size-1 downto 0);
         input_valid : in std_logic;
         input_ready : buffer std_logic;
         output : buffer unsigned(size-1 downto 0);
         output_valid : buffer std_logic;
         output_ready : in std_logic );
end;

architecture arch of counter_upper is
  signal counter : unsigned(output'range) := to_unsigned(0, output'length);
begin
  output <= counter + input;
  input_ready <= output_ready;
  output_valid <= input_valid;

  process (clk, reset)
  begin
    if reset = '1' then
      counter <= to_unsigned(0, counter'length);
    elsif rising_edge(clk) then
      if input_ready and input_valid then
        counter <= output;
      end if;
    end if;
  end process;
end;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.util.all;

entity rocket_equation is
  generic (size : positive );
  port ( clk : in std_logic;
         reset : in std_logic := '0';
         input : in unsigned(size-1 downto 0);
         input_valid : in std_logic;
         input_ready : buffer std_logic;
         output : buffer unsigned(size-1 downto 0) := to_unsigned(0, size);
         output_valid : buffer std_logic;
         output_ready : in std_logic );
end;

architecture arch of rocket_equation is
  signal acc, mass : unsigned(input'range) := to_unsigned(0, input'length);
  signal have_data : boolean := false;

  signal next_mass : unsigned(input'range);
  signal output_txn, input_txn : boolean;
begin
  input_txn <= input_ready = '1' and input_valid = '1';
  output_txn <= output_ready = '1' and output_valid = '1';

  next_mass <= fuel_for(input) when input_txn else fuel_for(mass);
  output <= next_mass when input_txn else acc + next_mass;

  input_ready <= '1' when fuel_for(fuel_for(mass)) = 0 and (output_ready = '1') else '0';
  output_valid <= '1' when fuel_for(next_mass) = 0 and (have_data or input_valid = '1') else '0';

  process (clk, reset)
  begin
    if reset = '1' then
      acc <= to_unsigned(0, acc'length);
      mass <= to_unsigned(0, mass'length);
    elsif rising_edge(clk) then
      mass <= next_mass;
      acc <= output;

      if input_txn and not output_txn then
        have_data <= true;
      elsif output_txn and not input_txn then
        have_data <= false;
      end if;
    end if;
  end process;
end;

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
  subtype counter_t is integer range 1 to n;
  signal counter : counter_t := counter_t'low;
begin
  input_ready <= output_ready when counter = counter_t'high else '1';
  output_valid <= input_valid when counter = counter_t'high else '0';

  process (clk, reset)
  begin
    if reset then
      counter <= counter_t'low;
    elsif rising_edge(clk) then
      if input_ready and input_valid then
        if counter /= counter_t'high then
          counter <= counter + 1;
        else
          counter <= counter_t'low;
        end if;
      end if;
    end if;
  end process;
end;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.util.all;

entity fuel_counter_upper is
  generic ( size : positive;
            n : positive );
  port ( clk : in std_logic;
         reset : in std_logic := '0';
         part : in positive range 1 to 2 := 1;
         input : in unsigned(size-1 downto 0);
         input_valid : in std_logic;
         input_ready : buffer std_logic;
         output : buffer unsigned(size-1 downto 0);
         output_valid : buffer std_logic;
         output_ready : in std_logic );
end;

architecture arch of fuel_counter_upper is
  signal ready_0, valid_1, ready_1, valid_2, ready_2, valid_3 : std_logic;
  signal value_1, value_2 : unsigned(input'range);
begin
  with part select input_ready <=
    ready_1 when 1,
    ready_0 when 2;

  with part select valid_2 <=
    input_valid when 1,
    valid_1 when 2;

  with part select value_2 <=
    fuel_for(input) when 1,
    value_1 when 2;

  rocket_equation : entity work.rocket_equation generic map ( size => size )
    port map ( clk => clk, reset => reset,
               input => input, input_valid => input_valid, input_ready => ready_0,
               output => value_1, output_valid => valid_1, output_ready => ready_1 );

  counter_upper : entity work.counter_upper generic map ( size => size )
    port map ( clk => clk, reset => reset,
               input => value_2, input_valid => valid_2, input_ready => ready_1,
               output => output, output_valid => valid_3, output_ready => ready_2 );

  -- No marker for the last element, so we need to count
  nth : entity work.nth generic map ( n => n )
    port map ( clk => clk, reset => reset,
               input_valid => valid_3, input_ready => ready_2,
               output_valid => output_valid, output_ready => output_ready );
end;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library int_io;
use int_io.util.from_ascii;
library bcd;
use bcd.bcd.all;
library uart;

entity day1 is
  port ( clk : in std_logic;
         reset : in std_logic := '0';
         part : in natural range 1 to 2 := 1;
         in_value : in std_logic_vector(7 downto 0);
         in_ready : out std_logic;
         in_valid : in std_logic;
         out_value : out std_logic_vector(7 downto 0);
         out_ready : in std_logic;
         out_valid : out std_logic );
end;

architecture arch of day1 is
  signal value_valid, value_ready, count_valid, count_ready : std_logic;
  signal value : decimal(5 downto 0);
  signal count_dec : decimal(6 downto 0);
  signal count : unsigned(4  * count_dec'length - 1 downto 0);
begin
  decoder : entity int_io.decode generic map ( value_size => value'length, seps => (0 => from_ascii(LF)) )
    port map ( clk => clk, reset => reset,
               byte => in_value, byte_valid => in_valid, byte_ready => in_ready,
               value => value, value_valid => value_valid, value_ready => value_ready );

  counter : entity work.fuel_counter_upper generic map ( size => count'length, n => 100 )
    port map ( clk => clk, reset => reset, part => part,
               input => to_unsigned(value, count'length), input_valid => value_valid, input_ready => value_ready,
               output => count, output_valid => count_valid, output_ready => count_ready );
  count_dec <= to_decimal(count, count_dec'length);

  encoder : entity int_io.encode generic map ( value_size => count_dec'length, sep => from_ascii(LF) )
    port map ( clk => clk, reset => reset,
               value => count_dec, value_valid => count_valid, value_ready => count_ready,
               byte => out_value, byte_valid => out_valid, byte_ready => out_ready );
end;
