library ieee;
use ieee.std_logic_1164.all;

package seven_segments is
  type seven_segments is array (natural range <>) of std_logic_vector(0 to 6);
end package;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.seven_segments.seven_segments;

entity seven_segments_hex is
	generic ( n : positive );
	port ( value : in unsigned(n * 4 - 1 downto 0);
	       output : out seven_segments(1 to n) );
end;

architecture structural of seven_segments_hex is
	type lut is array (natural range 0 to 15) of std_logic_vector(0 to 6);
	-- 7-segment displays are active-low
	constant patterns : lut := ("0000001", "1001111", "0010010", "0000110",
	                            "1001100", "0100100", "0100000", "0001111",
	                            "0000000", "0000100", "0001000", "1100000",
	                            "0110001", "1000010", "0110000", "0111000");
begin
	gen_display : for i in output'range generate
                output(i) <= patterns(to_integer(value(i * 4 - 1 downto (i - 1) * 4)));
	end generate;
end;

library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.log2;
use ieee.numeric_std.all;

library bcd;
use bcd.bcd.all;

use work.seven_segments.seven_segments;

entity seven_segments_dec is
	generic ( n : positive );
	port ( value : in decimal(n - 1 downto 0);
	       output : out seven_segments(1 to n) );
end;

architecture structural of seven_segments_dec is
	type lut is array (natural range 0 to 9) of std_logic_vector(0 to 6);
	-- 7-segment displays are active-low
	constant patterns : lut := ("0000001", "1001111", "0010010", "0000110",
	                            "1001100", "0100100", "0100000", "0001111",
	                            "0000000", "0000100");
begin
	gen_display : for i in output'range generate
		output(i) <= patterns(to_integer(value(i - 1)));
	end generate;
end;
