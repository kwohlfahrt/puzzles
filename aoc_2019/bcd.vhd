library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package bcd is
  subtype digit is unsigned(3 downto 0);
  type decimal is array (natural range <>) of digit;

  -- pragma translate_off
  function to_string( value : decimal ) return string;
  -- pragma translate_on
  function clz( value : decimal ) return natural;
  function "sll"( value : decimal; amount: integer ) return decimal;
  function "srl"( value : decimal; amount: integer ) return decimal;

  function to_decimal( value : unsigned; size : natural ) return decimal;
  function to_decimal( value : integer; size : natural ) return decimal;
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

  function clz( value : decimal ) return natural is
    alias x_value : decimal(0 to value'length - 1) is value;
  begin
    for j in x_value'range loop
      if x_value(j) /= 0 then
        return j;
      end if;
    end loop;
    return value'length;
  end function;

  function "sll"( value : decimal; amount: integer ) return decimal is
    variable result : decimal(value'range) := (others => "0000");
    variable temp : unsigned(value'length * 4 - 1 downto 0);

    alias x_value : decimal(value'length - 1 downto 0) is value;
    alias x_result : decimal(result'length - 1 downto 0) is result;
  begin
    for i in x_value'range loop
      temp(i * 4 + 3 downto i * 4) := x_value(i);
    end loop;

    temp := temp sll amount * 4;

    for i in x_result'range loop
      x_result(i) := temp(i * 4 + 3 downto i * 4);
    end loop;
    return result;
  end function;

  function "srl"( value : decimal; amount: integer ) return decimal is
    variable result : decimal(value'range) := (others => "0000");
    variable temp : unsigned(value'length * 4 - 1 downto 0);

    alias x_value : decimal(value'length - 1 downto 0) is value;
    alias x_result : decimal(result'length - 1 downto 0) is result;
  begin
    for i in x_value'range loop
      temp(i * 4 + 3 downto i * 4) := x_value(i);
    end loop;

    temp := temp srl amount * 4;

    for i in x_result'range loop
      x_result(i) := temp(i * 4 + 3 downto i * 4);
    end loop;
    return result;
  end function;

  function to_decimal( value : unsigned; size : natural ) return decimal is
    variable acc : unsigned(value'range) := value;
    variable result : decimal(size - 1 downto 0) := (others => "0000");
  begin
    -- pragma translate_off
    if is_x(value) then
      return result;
    end if;
    -- pragma translate_on

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

  function to_decimal( value : integer; size : natural ) return decimal is
    variable acc : integer := value;
    variable result : decimal(size - 1 downto 0) := (others => "0000");
  begin
    for j in result'right to result'left loop
      result(j) := to_unsigned(acc mod 10, result(j)'length);
      acc := acc / 10;
    end loop;
    return result;
  end function;

  function to_unsigned( value : decimal; size : natural ) return unsigned is
    variable acc : decimal(value'length - 1 downto 0) := value;
    variable result : unsigned(size - 1 downto 0) := (others => '0');
  begin
    -- pragma translate_off
    for i in value'range loop
      if is_x(value(i)) then
        return result;
      end if;
    end loop;
    -- pragma translate_on

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
