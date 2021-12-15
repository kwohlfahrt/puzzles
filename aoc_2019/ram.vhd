library ieee;

package util is
  function log2( x : in natural ) return natural;
end package;

package body util is
  function log2( x : in natural ) return natural is
    variable v : natural := x;
    variable i : natural := 0;
  begin
    while v > 0 loop
      v := v / 2;
      i := i + 1;
    end loop;
    return i;
  end;
end;


library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.util.all;

entity ram is
  generic ( size : natural;
            data_size : positive );
  port ( clk : in std_logic;
         write_enable : in std_logic; 
         write_addr : in unsigned(log2(size)-1 downto 0);
         data_in : in unsigned(data_size-1 downto 0); 
         read_addr : in unsigned(log2(size)-1 downto 0);
         data_out : out unsigned(data_size-1 downto 0) );
end;

architecture arch of ram is
  type mem is array(0 to size-1) of unsigned(data_size-1 downto 0);
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

