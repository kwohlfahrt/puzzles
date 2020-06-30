library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.util.all;

entity rx is
  generic ( bit_clocks : positive;
            stop_slack : natural := 0;
            parity_type : parity_t := none;
            n_stop_bits : positive range 1 to 2 := 1 );
  port ( clk : in std_logic;
         reset : in std_logic := '0';
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
  signal nphases : phase_t;
  signal start_toggle, done_toggle : boolean := false;
  signal new_output, still_valid : boolean := false;
  signal err : boolean := true; -- Nothing received

  signal idle : boolean;
  signal data_idx : data_idx_t := data_idx_t'left;
  signal stop_idx : stop_idx_t := stop_idx_t'low;
  signal data_samples : std_logic_vector(output'range);
  signal parity_sample : std_logic;
  signal stop_sample : std_logic;
  signal start_sample : std_logic;
begin
  valid <= '1' when new_output or still_valid else '0';
  idle <= start_toggle = done_toggle;

  with state select nphases <=
    phase_t'high - stop_slack when stop_bits,
    phase_t'high when others;

  process (clk, reset)
  begin
    if reset = '1' then
      state <= start_bit;
      phase <= phase_t'low;
      start_toggle <= false;
      done_toggle <= false;
      new_output <= false;
      still_valid <= false;
    elsif rising_edge(clk) then
      if idle and rx = '0' then
        start_toggle <= not start_toggle;
      end if;

      if new_output and ready /= '1' then
        still_valid <= true;
      elsif ready = '1' then
        still_valid <= false;
      end if;

      new_output <= false;
      if not idle or rx = '0' then
        -- Divider
        if phase = nphases then
          phase <= phase_t'low;
        else
          phase <= phase_t'succ(phase);
        end if;

        -- Sampling
        if phase = (phase_t'high + 1) / 2 then
          case state is
            when start_bit => start_sample <= rx;
            when data => data_samples(data_idx) <= rx; -- TODO: Shift register
            when parity => parity_sample <= rx;
            when stop_bits => stop_sample <= rx;
          end case;
        end if;

        -- Transition
        if phase = nphases then
          case state is
            when start_bit =>
              if start_sample /= '0' then
                err <= true;
                state <= start_bit;
                done_toggle <= not done_toggle;
              else
                state <= data;
                data_idx <= data_idx_t'left;
              end if;
            when data =>
              if data_idx = data_idx_t'right then
                if parity_type = none then
                  state <= stop_bits;
                  stop_idx <= stop_idx_t'low;
                else
                  state <= parity;
                end if;
              else
                data_idx <= data_idx_t'rightof(data_idx);
              end if;
            when parity =>
              -- TODO: actually implement parity
              state <= stop_bits;
              stop_idx <= stop_idx_t'low;
            when stop_bits =>
              if stop_sample /= '1' then
                err <= true;
                state <= start_bit;
                done_toggle <= not done_toggle;
              elsif stop_idx = stop_idx_t'high then
                err <= false;
                state <= start_bit;
                output <= data_samples;
                done_toggle <= not done_toggle;
                new_output <= true;
              else
                stop_idx <= stop_idx_t'succ(stop_idx);
              end if;
          end case;
        end if;
      end if;
    end if;
  end process;
end;
