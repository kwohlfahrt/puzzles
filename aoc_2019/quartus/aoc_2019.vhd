library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity aoc_2019 is
	port (switches : in std_logic_vector(9 downto 0);
	      seven_segments : out std_logic_vector(0 to (7 * 4 - 1)));
end;

architecture data of aoc_2019 is
	signal full_switches : std_logic_vector(11 downto 0);
begin
	full_switches(11 downto 10) <= "00";
	full_switches(9 downto 0) <= switches;
	gen_display : for i in 0 to 2 generate
		display : entity work.seven_segment
			port map ( value => unsigned(full_switches((i + 1) * 4 - 1 downto i * 4)),
			           output => seven_segments(i * 7 to (i + 1) * 7 - 1) );
	end generate;
	display0 : entity work.seven_segment port map ( value => "0000", output => seven_segments(21 to 27) );
end;
