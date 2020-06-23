library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity int_parser is
  generic ( value_size : positive := 8 );
  port ( clk : in std_logic;
         byte : in std_logic_vector(7 downto 0);
         byte_valid : in std_logic;
         byte_ready : out std_logic := '1';
         value : out unsigned(value_size - 1 downto 0);
         value_valid : out std_logic := '0';
         value_ready : in std_logic );
end entity;

architecture structure of int_parser is
  signal acc : unsigned(value'range) := (others => '0');
  signal valid_toggle : boolean := false;
  signal consumed_toggle : boolean := true;
  signal byte_value : unsigned(byte'range);

  -- ASCII '0'
  constant offset : unsigned(byte'range) := "00110000";
  -- ASCII ','
  constant sep : unsigned(byte'range) := "00101100";
begin
  byte_value <= unsigned(byte);
  value_valid <= '1' when valid_toggle = consumed_toggle else '0';

  process (clk)
  begin
    if rising_edge(clk) and value_valid = '1' and value_ready = '1' then
      consumed_toggle <= not consumed_toggle;
    end if;

    if rising_edge(clk) and byte_valid = '1' then
      if byte_value = sep then
        value <= acc;
        acc <= (others => '0');
        if not value_valid then
          valid_toggle <= not valid_toggle;
        end if;
      else
        acc <= resize(acc * 10, value'length) + resize(byte_value - offset, value'length);
      end if;
    end if;
  end process;
end architecture;
