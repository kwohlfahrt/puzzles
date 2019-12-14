library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity aoc_2019 is
	port (switches : in std_logic_vector(9 downto 0);
	      seven_segments : out std_logic_vector(0 to (7 * 4 - 1)));
end;

architecture data of aoc_2019 is
	signal full_switches : std_logic_vector(13 downto 0);
begin
	full_switches(full_switches'left downto 10) <= "0000";
	full_switches(9 downto 0) <= switches;
	display : entity work.seven_segments_dec generic map ( n => 4 )
		port map ( value => unsigned(full_switches), output => seven_segments );
end;
