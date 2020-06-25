library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library bcd;
use bcd.bcd.all;

entity tb1 is
end;

architecture structure of tb1 is
  constant dec_high_to_low : decimal(2 downto 0) := ("0010", "0100", "0011");
begin
  assert to_string(("0010", "0100", "0011")) = "0010 0100 0011" report to_string(("0010", "0100", "0011"));
  assert to_decimal("11110011", 3) = ("0010", "0100", "0011") report to_string(to_decimal("11110011", 3));
  assert to_decimal(243, 3) = ("0010", "0100", "0011") report to_string(to_decimal(243, 3));
  assert to_unsigned(("0010", "0100", "0011"), 8) = "11110011" report to_string(to_unsigned(("0010", "0100", "0011"), 8));

  assert clz(("0010", "0100", "0011")) = 0 report to_string(clz(("0010", "0100", "0011")));
  assert clz(("0000", "0100", "0011")) = 1 report to_string(clz(("0000", "0100", "0011")));
  assert clz(("0000", "0100", "0000")) = 1 report to_string(clz(("0000", "0100", "0000")));
  assert clz(("0000", "0000", "0000")) = 3 report to_string(clz(("0000", "0000", "0000")));

  assert clz(dec_high_to_low srl 2) = 2 report to_string(clz(dec_high_to_low srl 2));

  assert (decimal'("0010", "0100", "0011") sll 1) = ("0100", "0011", "0000")
    report to_string(decimal'("0010", "0100", "0011") sll 1);
  assert (decimal'("0010", "0100", "0011") sll 2) = ("0011", "0000", "0000")
    report to_string(decimal'("0010", "0100", "0011") sll 2);

  assert (decimal'("0010", "0100", "0011") srl 1) = ("0000", "0010", "0100")
    report to_string(decimal'("0010", "0100", "0011") srl 1);
  assert (decimal'("0010", "0100", "0011") srl 2) = ("0000", "0000", "0010")
    report to_string(decimal'("0010", "0100", "0011") srl 2);
  assert (dec_high_to_low srl 2) = ("0000", "0000", "0010")
    report to_string(dec_high_to_low srl 2);
end;
