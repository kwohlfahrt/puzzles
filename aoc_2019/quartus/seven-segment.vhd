library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity seven_segments_hex is
	generic ( n : positive );
	port (value : in unsigned(n * 4 - 1 downto 0);
	      output : out std_logic_vector(0 to n * 7 - 1));
end;

architecture structural of seven_segments_hex is
begin
	gen_display : for i in 0 to n - 1 generate
		display : entity work.seven_segment_hex
			port map ( value => value(i * 4 + 3 downto i * 4), output => output(i * 7 to i * 7 + 6));
	end generate;
end;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity seven_segment_hex is
	port (value : in unsigned(3 downto 0);
	      output : out std_logic_vector(0 to 6));
end;

architecture structural of seven_segment_hex is
	type lut is array (natural range 0 to 15) of std_logic_vector(0 to 6);
	-- 7-segment displays are active-low
	constant patterns : lut := ("0000001", "1001111", "0010010", "0000110",
	                            "1001100", "0100100", "0100000", "0001111",
	                            "0000000", "0000100", "0001000", "1100000",
	                            "0110001", "1000010", "0110000", "0111000");
begin
	output <= patterns(to_integer(value));
end;

library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.log2;
use ieee.numeric_std.all;

entity seven_segments_dec is
	generic ( n : positive );
	port (value : in unsigned(natural(log2(10.0**n)) downto 0);
	      output : out std_logic_vector(0 to n * 7 - 1));
end;

architecture structural of seven_segments_dec is
	type lut is array (natural range 0 to 9) of std_logic_vector(0 to 6);
	-- 7-segment displays are active-low
	constant patterns : lut := ("0000001", "1001111", "0010010", "0000110",
	                            "1001100", "0100100", "0100000", "0001111",
	                            "0000000", "0000100");
begin
	gen_display : for i in 0 to n - 1 generate
		output(i * 7 to i * 7 + 6) <= patterns(to_integer(value / (10 ** i) mod 10));
	end generate;
end;
