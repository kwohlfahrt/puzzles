library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library uart;
library int_io;
library ram;

library bcd;
use bcd.bcd.all;

library uart;
use uart.util.from_ascii;

library seven_segment;
use seven_segment.seven_segments.seven_segments;

entity day2 is
  port ( clk : in std_logic;
         reset : in std_logic := '0';
         part : in natural range 1 to 2 := 1;
         in_value : in std_logic_vector(7 downto 0);
         in_ready : out std_logic;
         in_valid : in std_logic;
         out_value : out std_logic_vector(7 downto 0);
         out_ready : in std_logic;
         out_valid : out std_logic;
         seven_segments : out seven_segments(3 downto 0) );
end;

architecture arch of day2 is
  constant addr_size : positive := 8;
  constant data_size : positive := 16;

  signal display_value : decimal(3 downto 0) := to_decimal(0, 4);
  signal input_addr : unsigned(addr_size-1 downto 0) := to_unsigned(0, addr_size);

  signal value_valid, value_ready, count_valid, count_ready : std_logic;
  signal value : decimal(5 downto 0);
  signal count_dec : decimal(6 downto 0);
  signal count : unsigned(4  * count_dec'length - 1 downto 0);
begin
  decoder : entity int_io.decode generic map ( value_size => value'length, sep => unsigned(from_ascii(',')) )
    port map ( clk => clk, reset => reset,
               byte => in_value, byte_valid => in_valid, byte_ready => in_ready,
               value => value, value_valid => value_valid, value_ready => value_ready );

  memory : entity ram.ram generic map ( addr_size => addr_size, data_size => data_size )
    port map ( clk => clk,
               write_enable => value_valid, write_addr => input_addr, data_in => to_unsigned(value, data_size),
               read_addr => to_unsigned(0, addr_size) );

  encoder : entity int_io.encode generic map ( value_size => count_dec'length, sep => unsigned(from_ascii(',')) )
    port map ( clk => clk, reset => reset,
               value => count_dec, value_valid => count_valid, value_ready => count_ready,
               byte => out_value, byte_valid => out_valid, byte_ready => out_ready );

  display : entity seven_segment.seven_segments_dec generic map ( n => 4 )
    port map ( value => display_value, output => seven_segments );

  process (clk, reset)
  begin
    if reset then
      input_addr <= to_unsigned(0, input_addr'length);
    elsif rising_edge(clk) then
      if value_valid = '1' then
        input_addr <= input_addr + 1;
      end if;
    end if;
  end process;
end;
