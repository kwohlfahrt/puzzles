library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.util.all;

entity rx is
  -- FIXME: There is a timing issue - only works with 2 stop bits (+cstopb)
  generic ( bit_clocks : positive;
            parity_type : parity_t := none;
            n_stop_bits : positive range 1 to 2 := 1 );
  port ( clk : in std_logic;
         rx : in std_logic;
         ready : in std_logic;
         output : out std_logic_vector(7 downto 0);
         valid : buffer std_logic := '0' );
end;

architecture structure of rx is
  -- Quartus does not support 'subtype annotation
  subtype phase_t is natural range 1 to bit_clocks;
  subtype data_idx_t is natural range output'reverse_range;
  subtype stop_idx_t is natural range 1 to n_stop_bits;

  signal state : state_t := start_bit;
  signal phase : phase_t := phase_t'low;
  signal start_toggle, done_toggle : boolean := false;
  signal err : boolean := true; -- Nothing received

  signal idle : boolean;
  signal data_idx : data_idx_t := data_idx_t'left;
  signal stop_idx : stop_idx_t := stop_idx_t'low;
  signal data_samples : std_logic_vector(output'range);
  signal stop_sample : std_logic;
  signal start_sample : std_logic;
begin
  idle <= start_toggle = done_toggle;

  process (clk, idle)
  begin
    if rising_edge(clk) and idle then
      if not rx then
        start_toggle <= not start_toggle;
      end if;
    end if;
  end process;

  process (clk, idle)
  begin
    if rising_edge(clk) and ready = '1' and valid = '1' then
      valid <= '0';
    end if;

    if rising_edge(clk) and not idle then
      if phase = phase_t'high then
        phase <= phase_t'low;
      else
        phase <= phase_t'succ(phase);
      end if;

      if phase = (phase_t'high + 1) / 2 then
        case state is
          when start_bit => start_sample <= rx;
          when data => data_samples(data_idx) <= rx;
          when stop_bits => stop_sample <= rx;
        end case;
      end if;

      if phase = phase_t'high then
        case state is
          when start_bit =>
            if start_sample /= '0' then
              err <= true;
              state <= start_bit;
              done_toggle <= not done_toggle;
            else
              state <= data;
            end if;
          when data =>
            if data_idx = data_idx_t'right then
              data_idx <= data_idx_t'left;
              state <= stop_bits;
            else
              data_idx <= data_idx_t'rightof(data_idx);
            end if;
          when stop_bits =>
            if stop_sample /= '1' then
              err <= true;
              state <= start_bit;
              done_toggle <= not done_toggle;
            elsif stop_idx = stop_idx_t'high then
              stop_idx <= stop_idx_t'low;
              state <= start_bit;
              if rx then
                done_toggle <= not done_toggle;
              end if;
              err <= false;
              output <= data_samples;
              valid <= '1';
            else
              stop_idx <= stop_idx_t'succ(stop_idx);
            end if;
        end case;
      end if;
    end if;
  end process;
end;
