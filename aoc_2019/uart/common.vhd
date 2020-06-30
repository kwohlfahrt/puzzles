library ieee;
use ieee.std_logic_1164.all;

package util is
  type parity_t is (odd, even, none);
  type state_t is (start_bit, data, parity, stop_bits);

  -- pragma translate_off
  procedure transmit( byte : in std_logic_vector(7 downto 0); bit_duration : in time; signal tx : out std_logic );
  -- pragma translate_on
  function parity( byte : in std_logic_vector ) return std_logic;
end package;

package body util is
  -- pragma translate_off
  procedure transmit( byte : in std_logic_vector(7 downto 0); bit_duration : in time; signal tx : out std_logic ) is
  begin
    tx <= '0';
    wait for bit_duration;
    for i in byte'reverse_range loop
      tx <= byte(i);
      wait for bit_duration;
    end loop;
    tx <= '1';
    wait for bit_duration;
  end;
  -- pragma translate_on

  function parity( byte : in std_logic_vector ) return std_logic is
    variable result : std_logic := '0';
  begin
    for i in byte'range loop
      result := result xor byte(i);
    end loop;
    return result;
  end;
end;
