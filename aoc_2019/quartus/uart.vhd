library ieee;
use ieee.std_logic_1164.all;

entity uart is
  generic ( stop_bits : natural range 1 to 2 := 1;
            parity : boolean := false );
  port ( clk : in std_logic;
         clk_reset : out std_logic := '1';
         rx : in std_logic;
         tx : out std_logic;
         input : in std_logic_vector(0 to 7);
         output : out std_logic_vector(0 to 7) );
end;

architecture structure of uart is
  signal counter : natural range 0 to 1 := 1;
begin
  tx <= '1';
  process (clk)
  begin
    if rising_edge(clk) then
      if counter = 0 then
        counter <= 1;
      else
        counter <= counter - 1;
      end if;

      if counter = 1 then
        output <= "11111111";
      elsif counter = 0 then
        output <= "00000000";
      end if;
    end if;
  end process;

  process (rx)
  begin
    if falling_edge(rx) then
      clk_reset <= '0';
    end if;
  end process;
end;
