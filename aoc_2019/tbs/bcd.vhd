library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library bcd;
use bcd.bcd.all;

entity tb1 is
end;

architecture structure of tb1 is
begin
  assert to_string(("0010", "0100", "0011")) = "0010 0100 0011" report to_string(("0010", "0100", "0011"));
  assert to_decimal("11110011", 3) = ("0010", "0100", "0011") report to_string(to_decimal("11110011", 3));
  assert to_unsigned(("0010", "0100", "0011"), 8) = "11110011" report to_string(to_unsigned(("0010", "0100", "0011"), 8));
end;
