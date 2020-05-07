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

  signal state : state_t := start_bit;
  signal phase : phase_t := phase_t'low;
  signal start_toggle, done_toggle : boolean := false;
  signal err : boolean := true; -- Nothing received

  signal idle : boolean;
  signal sample : boolean;
  signal data_idx : data_idx_t := data_idx_t'high;
  signal stop_idx : stop_idx_t := stop_idx_t'low;
  signal data_samples : std_logic_vector(output'range);
  signal stop_sample : std_logic;
  signal start_sample : std_logic;
begin
  idle <= start_toggle = done_toggle;
  ready <= '1' when idle and not err else '0';
  sample <= phase = phase_t'high / 2;

  process (rx, idle)
  begin
    if falling_edge(rx) and idle then
      start_toggle <= not start_toggle;
    end if;
  end process;

  process (sample)
  begin
    if rising_edge(sample) then
      if state = start_bit then
        start_sample <= rx;
      elsif state = data then
        data_samples(data_idx) <= rx;
      elsif state = stop_bits then
        stop_sample <= rx;
      end if;
    end if;
  end process;

  process (clk, idle)
  begin
    if rising_edge(clk) and not idle then
      if phase = phase_t'high then
        phase <= phase_t'low;
      else
        phase <= phase_t'succ(phase);
      end if;

      if phase = phase_t'high then
        case state is
          when start_bit =>
            state <= data;
          when data =>
            if data_idx = data_idx_t'low then
              data_idx <= data_idx_t'high;
              state <= stop_bits;
            else
              data_idx <= data_idx_t'pred(data_idx);
            end if;
          when stop_bits =>
            if stop_idx = stop_idx_t'high then
              stop_idx <= stop_idx_t'low;
              state <= start_bit;
              done_toggle <= not done_toggle;
              err <= false;
              output <= data_samples;
            else
              stop_idx <= stop_idx_t'succ(stop_idx);
            end if;
        end case;
      end if;
    end if;
  end process;
end;
