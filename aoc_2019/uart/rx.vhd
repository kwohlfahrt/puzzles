library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.uart_util.all;

entity rx is
  -- FIXME: There is a timing issue - only works with 2 stop bits (+cstopb)
  generic ( bit_clocks : positive;
            parity_type : parity_t := none;
            n_stop_bits : positive range 1 to 2 := 1 );
  port ( clk : in std_logic;
         rx : in std_logic;
         output : out std_logic_vector(7 downto 0);
         ready : out std_logic );
end;

architecture structure of rx is
  -- Quartus does not support 'subtype annotation
  subtype phase_t is natural range 1 to bit_clocks;
  subtype data_idx_t is natural range output'right to output'left;
  subtype stop_idx_t is natural range 1 to n_stop_bits;

  signal state : state_t := stop_bits;
  signal phase : phase_t := 1;
  signal start_toggle, done_toggle : boolean := false;
  signal err : boolean := true; -- Nothing received

  signal idle : boolean;
  signal data_idx : data_idx_t;
  signal stop_idx : stop_idx_t;
  signal buf : std_logic_vector(output'range);
  signal samples : std_logic_vector(1 to bit_clocks);
begin
  idle <= start_toggle = done_toggle;
  ready <= '1' when idle and not err else '0';

  process (rx, idle)
  begin
    if falling_edge(rx) and idle then
      start_toggle <= not start_toggle;
    end if;
  end process;

  process (clk)
  begin
    if rising_edge(clk) then
      samples(phase) <= rx;

      if idle then
        phase <= phase_t'left;
      elsif phase = phase_t'right then
        phase <= phase_t'left;
      else
        phase <= phase + 1;
      end if;

      if phase = phase_t'right then
        case state is
          when start_bit =>
            if samples(samples'length / 2) /= '0' then
              done_toggle <= not done_toggle;
              err <= true;
            else
              state <= data;
              data_idx <= data_idx_t'left;
              err <= false;
            end if;
          when data =>
            buf(data_idx) <= samples(samples'length / 2);
            if data_idx = data_idx_t'right then
              state <= stop_bits;
              stop_idx <= stop_idx_t'left;
            else
              data_idx <= data_idx_t'rightof(data_idx);
            end if;
          when stop_bits =>
            stop_idx <= stop_idx_t'rightof(stop_idx);
            if samples(samples'length / 2) /= '1' then
              err <= true;
              state <= start_bit;
              done_toggle <= not done_toggle;
            end if;
        end case;
      end if;

      if state = stop_bits and stop_idx = n_stop_bits then
        if phase = samples'length / 2 + 1 then
          if samples(samples'length / 2) /= '1' then
            err <= true;
          else
            output <= buf;
          end if;
        elsif phase > samples'length / 2 + 2 then
          state <= start_bit;
          done_toggle <= not done_toggle;
        end if;
      end if;
    end if;
  end process;
end;
