library ieee;
use ieee.numeric_std.all;

package day1 is
  function fuel_for( mass : unsigned ) return unsigned;
end package;

package body day1 is
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

use work.day1.all;

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
end counter_upper;

architecture behave of counter_upper is
  signal counter : unsigned(size-1 downto 0) := (others => '0');
  signal new_output : boolean := false;
begin
  output <= counter;

  process (clk, reset)
  begin
    if reset = '1' then
      counter <= to_unsigned(0, counter'length);
      output_valid <= '0';
    elsif rising_edge(clk) then
      if new_output then
        output_valid <= '1';
      elsif output_ready = '1' then
        output_valid <= '0';
      end if;

      new_output <= false;
      if input_valid = '1' then
        counter <= counter + fuel_for(input);
        new_output <= true;
      end if;
    end if;
  end process;
end behave;

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
