library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package bcd is
  subtype digit is unsigned(3 downto 0);
  type decimal is array (natural range <>) of digit;

  -- pragma translate_off
  function to_string( value : decimal ) return string;
  -- pragma translate_on
  function to_decimal( value : unsigned; size : natural ) return decimal;
  function to_unsigned( value : decimal; size : natural ) return unsigned;
end;

package body bcd is
  -- pragma translate_off
  function to_string( value : decimal ) return string is
    variable result : string(1 to value'length * 4 + value'length - 1) := (others => ' ');
  begin
    if value'ascending then
      for i in value'range loop
        result(1 + i * 5 to (i + 1) * 5 - 1) := to_string(value(i));
      end loop;
    else
      for i in value'range loop
        result(1 + i * 5 to (i + 1) * 5 - 1) := to_string(value(value'high - i));
      end loop;
    end if;
    return result;
  end function;
  -- pragma translate_on

  function to_decimal( value : unsigned; size : natural ) return decimal is
    variable acc : unsigned(value'range) := value;
    variable result : decimal(size - 1 downto 0) := (others => "0000");
  begin
    for i in value'range loop
      for j in result'left downto result'right + 1 loop
        if result(j - 1) >= 5 then
          result(j - 1) := result(j - 1) + 3;
        end if;
        result(j) := result(j)(2 downto 0) & result(j - 1)(3);
      end loop;
      result(result'right) := result(result'right)(2 downto 0) & acc(acc'left);
      acc := acc sll 1;
    end loop;
    return result;
  end function;

  function to_unsigned( value : decimal; size : natural ) return unsigned is
    variable acc : decimal(value'length - 1 downto 0) := value;
    variable result : unsigned(size - 1 downto 0) := (others => '0');
  begin
    for i in result'range loop
      result := acc(acc'right)(0) & result(size - 1 downto 1);
      for j in acc'right to acc'left - 1 loop
        acc(j) := acc(j + 1)(0) & acc(j)(3 downto 1);
        if acc(j) >= 8 then
          acc(j) := acc(j) - 3;
        end if;
      end loop;
      acc(acc'left) := acc(acc'left) srl 1;
    end loop;
    return result;
  end function;
end;
