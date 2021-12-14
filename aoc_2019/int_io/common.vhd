library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package util is
  type seps_t is array (natural range <>) of std_logic_vector(7 downto 0);

  function to_ascii( c : std_logic_vector(7 downto 0) ) return character;
  function from_ascii( c : character ) return std_logic_vector;
end package;

package body util is
  function to_ascii( c : std_logic_vector(7 downto 0) ) return character is
  begin
    return character'val(to_integer(unsigned(c)));
  end function;

  function from_ascii( c : character ) return std_logic_vector is
  begin
    return std_logic_vector(to_unsigned(character'pos(c), 8));
  end function;
end;
