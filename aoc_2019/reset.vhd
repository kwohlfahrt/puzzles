library ieee;
use ieee.std_logic_1164.all;

entity synchronizer is
  generic ( stages : positive := 2 );
  port ( clk, async_reset : in std_logic ;
         sync_reset : out std_logic );
end;

architecture data of synchronizer is
  signal buffers : std_logic_vector( 0 to stages - 1 );
begin
  sync_reset <= buffers(stages - 1);

  process (clk, async_reset)
  begin 
    if async_reset = '1' then
      buffers <= ( others => '1' );
    elsif rising_edge(clk) then
      buffers(0) <= '0';
      buffers(1 to stages - 1) <= buffers(0 to stages - 2);
    end if;
  end process;
end;
