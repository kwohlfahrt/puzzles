library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ram is
  generic ( addr_size : positive;
            data_size : positive );
  port ( clk : in std_logic;
         write_enable : in std_logic; 
         write_addr : in unsigned(addr_size-1 downto 0);
         data_in : in unsigned(data_size-1 downto 0); 
         read_addr : in unsigned(addr_size-1 downto 0);
         data_out : out unsigned(data_size-1 downto 0) );
end;

architecture arch of ram is
  type mem is array(2 ** addr_size - 1 downto 0) of unsigned(data_size-1 downto 0);
  signal data : mem;
begin
  process (clk)
  begin
    if rising_edge(clk) then
      if write_enable then
        data(to_integer(write_addr)) <= data_in;
      end if;
      data_out <= data(to_integer(read_addr));
    end if;
  end process;
end;

