package uart_util is
  type parity_t is (odd, even, none);
end package;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.uart_util.all;

entity uart is
  -- FIXME: There is a timing issue - only works with 2 stop bits (+cstopb)
  generic ( bit_samples : positive;
            n_stop_bits : natural range 1 to 2 := 1;
            parity_type : parity_t := none );
  port ( clk : in std_logic;
         rx : in std_logic;
         output : out std_logic_vector(0 to 7);
         ready : out std_logic );
end;

architecture structure of uart is
  type rx_state is (start_bit, data, stop_bits);

  signal state : rx_state := start_bit;
  signal phase : natural range 1 to bit_samples := 1;
  signal done : std_logic := '0';

  signal err : std_logic;
  signal buf : std_logic_vector(output'range);
  signal idx : natural range buf'range;
  signal idle : boolean;
begin
  ready <= done and not err;
  idle <= state = start_bit and rx /= '0';

  process (clk)
    variable sample : std_logic;
  begin
    if rising_edge(clk) then
      if idle then
        phase <= phase'left;
      elsif phase = phase'right then
        phase <= phase'left;
      else
        phase <= phase + 1;
      end if;

      if phase = phase'left + (phase'right - phase'left) / 2 then
        sample := rx;
      end if;

      if phase = phase'right then
        case state is
          when start_bit =>
            if sample = '0' then
              state <= data;
              idx <= 0;
              done <= '0';
            else
              state <= start_bit;
            end if;
          when data =>
            buf(idx) <= sample;

            if idx = idx'right then
              state <= stop_bits;
            else
              idx <= idx + 1;
            end if;
          when stop_bits =>
            done <= '1';
            state <= start_bit;
            if sample = '1' then
              output <= buf;
              err <= '0';
            else
              err <= '1';
            end if;
        end case;
      end if;
    end if;
  end process;
end;
