library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library bcd;
use bcd.bcd.all;

entity decode is
  generic ( value_size : positive;
            -- ASCII ','
            sep : unsigned(7 downto 0) := "00101100" );
  port ( clk : in std_logic;
         reset : in std_logic := '0';
         byte : in std_logic_vector(7 downto 0);
         byte_valid : in std_logic;
         byte_ready : out std_logic := '1';
         value : out decimal(value_size - 1 downto 0);
         value_valid : out std_logic := '0';
         value_ready : in std_logic );
end entity;

architecture structure of decode is
  signal acc : decimal(value'range) := (others => "0000");
  signal byte_value : unsigned(byte'range);
  signal new_value, still_valid : boolean := false;

  -- ASCII '0'
  constant offset : unsigned(byte'range) := "00110000";
begin
  byte_value <= unsigned(byte);
  value_valid <= '1' when new_value or still_valid else '0';

  process (clk, reset)
  begin
    if reset = '1' then
      new_value <= false;
      still_valid <= false;
      acc <= (others => "0000");
      value <= (others => "0000");
    elsif rising_edge(clk) then
      if new_value and value_ready /= '1' then
        still_valid <= true;
      elsif value_ready = '1' then
        still_valid <= false;
      end if;

      new_value <= false;
      if byte_valid = '1' then
        if byte_value = sep then
          value <= acc;
          acc <= (others => "0000");
          new_value <= true;
        else
          acc <= acc(acc'left - 1 downto acc'right) & resize(byte_value, acc(0)'length);
        end if;
      end if;
    end if;
  end process;
end architecture;
