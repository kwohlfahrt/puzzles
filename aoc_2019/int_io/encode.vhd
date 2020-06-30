library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library bcd;
use bcd.bcd.all;

entity encode is
  generic ( value_size : positive;
            -- ASCII ','
            sep : unsigned(7 downto 0) := "00101100" );
  port ( clk : in std_logic;
         reset : in std_logic := '0';
         value : in decimal(value_size - 1 downto 0);
         value_valid : in std_logic;
         value_ready : buffer std_logic;
         byte : out std_logic_vector(7 downto 0);
         byte_valid : out std_logic := '0';
         byte_ready : in std_logic );
end entity;

architecture structure of encode is
  signal acc : decimal(value'length - 1 downto 0) := (others => "0000");
  signal ndigits : natural range 0 to value_size := 0;
  signal new_byte, still_valid : boolean := false;

  -- ASCII '0'
  constant offset : unsigned(byte'range) := "00110000";
begin
  byte <= std_logic_vector(acc(acc'left) + offset) when ndigits > 0 else std_logic_vector(sep);
  byte_valid <= '1' when new_byte or still_valid else '0';
  value_ready <= '1' when ndigits = 0 else '0';

  process (clk, reset)
  begin
    if reset = '1' then
      still_valid <= false;
      new_byte <= false;
      ndigits <= 0;
      acc <= (others => "0000");
    elsif rising_edge(clk) then
      if new_byte and byte_ready /= '1' then
        still_valid <= true;
      elsif byte_ready = '1' then
        still_valid <= false;
      end if;

      new_byte <= false;
      if not byte_valid or byte_ready then
        if ndigits > 0 then
          acc <= acc sll 1;
          ndigits <= ndigits - 1;
          new_byte <= true;
        elsif value_ready and value_valid then
          acc <= value sll clz(value);
          ndigits <= value_size - clz(value);
          new_byte <= true;
        end if;
      end if;
    end if;
  end process;
end architecture;
