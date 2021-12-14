library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library seven_segment;
use seven_segment.seven_segments.all;

entity tb1 is
end;

architecture structural of tb1 is
  signal input : unsigned(4 * 16 - 1 downto 0) := ( others => '0' );
  signal output : seven_segments_t(15 downto 0);
begin
  input <= x"fedcba9876543210";

  display : entity seven_segment.seven_segments_hex generic map ( n => 16 )
    port map ( value => input, output => output );

  process
  begin
    wait for 1 ns;
    for i in 0 to 15 loop
      assert output(i) = hex_digits(i) report to_string(i) & " => " & to_string(output(i));
    end loop;
    wait;
  end process;
end;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library bcd;
use bcd.bcd.all;

library seven_segment;
use seven_segment.seven_segments.all;

entity tb2 is
end;

architecture structural of tb2 is
  signal input : decimal(0 to 9) := ( others => "0000" );
  signal output : seven_segments_t(9 downto 0);
begin
  input_numbers : for i in 0 to 9 generate
    input(i) <= to_unsigned(9 - i, 4);
  end generate;

  display : entity seven_segment.seven_segments_dec generic map ( n => 10 )
    port map ( value => input, output => output );

  process
  begin
    wait for 1 ns;
    for i in 0 to 9 loop
      assert output(i) = hex_digits(i) report to_string(i) & " => " & to_string(output(i));
    end loop;
    wait;
  end process;
end;
