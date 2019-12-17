package uart_util is
  type parity_t is (odd, even, none);
end package;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.uart_util.all;

entity uart is
  generic ( n_stop_bits : natural range 1 to 2 := 1;
            parity_type : parity_t := none );
  port ( clk : in std_logic;
         clk_reset : buffer std_logic := '1';
         rx : in std_logic;
         tx : out std_logic;
         input : in std_logic_vector(0 to 7);
         output : out std_logic_vector(0 to 7) );
end;

architecture structure of uart is
  type rx_state is (start_bit, data, stop_bits);

  signal state : rx_state := stop_bits;
  signal done : std_logic := '1';
  signal phase : natural range 0 to 1 := 0;

  signal buf : std_logic_vector(0 to 7);
  signal idx : natural range buf'range;
begin
  tx <= '1';
  clk_reset <= done and rx;

  process (clk)
    variable samples : std_logic_vector(1 to phase'right);
  begin
    if rising_edge(clk) then
      if phase = phase'right then
        phase <= phase'left;
      else
        phase <= phase + 1;
      end if;

      if phase = phase'left and state = start_bit then
        done <= '0';
      end if;

      if phase /= phase'left then
        samples(phase) := rx;
      end if;

      if phase = phase'right then
        case state is
          when start_bit =>
            if samples(1) = '0' then
              state <= data;
              idx <= 0;
            else
              done <= '1';
              state <= start_bit;
            end if;
          when data =>
            if idx = idx'right then
              state <= stop_bits;
            else
              idx <= idx + 1;
            end if;
          when stop_bits =>
            done <= '1';
            state <= start_bit;
        end case;
      end if;
    end if;
  end process;

  process (state, idx)
  begin
    case state is
      when start_bit => output <= "10000000";
      when data => output <= "01000" & std_logic_vector(to_unsigned(idx, 3));
      when stop_bits => output <= "11000000";
    end case;
  end process;
end;
