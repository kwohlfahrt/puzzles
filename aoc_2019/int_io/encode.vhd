library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library bcd;
use bcd.bcd.all;

entity encode is
  generic ( value_size : positive );
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
  signal ndigits : natural range 0 to value_size;

  signal valid_toggle : boolean := false;
  signal consumed_toggle : boolean := true;

  -- ASCII '0'
  constant offset : unsigned(byte'range) := "00110000";
  -- ASCII ','
  constant sep : unsigned(byte'range) := "00101100";
begin
  byte <= std_logic_vector(acc(acc'left) + offset) when ndigits > 0 else std_logic_vector(sep);
  byte_valid <= '1' when valid_toggle = consumed_toggle else '0';
  value_ready <= '1' when ndigits = 0 and byte_ready = '1' else '0';

  process (clk, reset)
  begin
    if reset = '1' then
    elsif rising_edge(clk) then
      if byte_valid = '1' and byte_ready = '1' then
        consumed_toggle <= not consumed_toggle;
      end if;

      if byte_ready = '1' then
        if value_ready and value_valid then
          acc <= value sll clz(value);
          ndigits <= value_size - clz(value);
          valid_toggle <= not valid_toggle;
        elsif ndigits > 0 then
          acc <= acc sll 1;
          ndigits <= ndigits - 1;
          valid_toggle <= not valid_toggle;
        end if;
      end if;
    end if;
  end process;
end architecture;
