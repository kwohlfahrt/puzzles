library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity seven_segment is
	port (value : in unsigned(3 downto 0);
	      output : out std_logic_vector(0 to 6));
end;

architecture structural of seven_segment is
	type lut is array (natural range 0 to 15) of std_logic_vector(0 to 6);
	-- 7-segment displays are active-low
	constant patterns : lut := ("0000001", "1001111", "0010010", "0000110",
	                            "1001100", "0100100", "0100000", "0001111",
	                            "0000000", "0000100", "0001000", "1100000",
	                            "0110001", "1000010", "0110000", "0111000");
begin
   output <= patterns(to_integer(value));
end;
