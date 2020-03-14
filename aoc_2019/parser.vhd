library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity int_parser is
  generic ( value_size : positive := 8 );
  port ( byte : in std_logic_vector(7 downto 0);
         byte_ready : in std_logic;
         value : out unsigned(value_size - 1 downto 0);
         value_ready : out std_logic );
end entity;

architecture structure of int_parser is
  signal acc : unsigned(value'range) := (others => '0');

  signal buf : std_logic_vector(byte'range);
  signal byte_value : unsigned(byte'range);

  -- ASCII '0'
  constant offset : std_logic_vector(byte'range) := "00110000";
  -- ASCII ','
  constant sep : std_logic_vector(byte'range) := "00101100";
begin
  byte_value <= unsigned(byte) - unsigned(offset);

  process (byte_ready)
  begin
    if rising_edge(byte_ready) then
      if byte = sep then
        value <= acc;
        value_ready <= '1';
        acc <= (others => '0');
      else
        value_ready <= '0';
        acc <= resize(acc * 10, value'length) + resize(byte_value, value'length);
      end if;
    end if;
  end process;
end architecture;

