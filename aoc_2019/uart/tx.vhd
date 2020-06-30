library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.util.all;

entity tx is
  generic ( bit_clocks : positive;
            stop_slack : natural := 0;
            n_stop_bits : natural range 1 to 2 := 1;
            parity_type : parity_t := none );
  port ( clk : in std_logic;
         reset : in std_logic := '0';
         valid : in std_logic;
         input : in std_logic_vector(7 downto 0);
         tx : out std_logic := '1';
         ready : out std_logic );
end;

architecture structure of tx is
  -- Quartus does not support 'subtype annotation
  subtype phase_t is natural range 1 to bit_clocks;
  subtype data_idx_t is natural range input'reverse_range;
  subtype stop_idx_t is natural range 1 to n_stop_bits;

  signal state : state_t := start_bit;
  signal phase : phase_t := phase_t'low;
  signal nphases : phase_t;
  signal start_toggle, done_toggle : boolean := false;

  signal buf : std_logic_vector(input'range);
  signal data_idx : data_idx_t;
  signal stop_idx : stop_idx_t;
  signal idle : boolean;
  signal tx_sample : std_logic;
begin
  ready <= '1' when start_toggle = done_toggle else '0';
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
    elsif rising_edge(clk) then
      if idle and valid = '1' then
        start_toggle <= not start_toggle;
        buf <= input;
      end if;

      if not idle or valid = '1' then
        if phase = nphases then
          phase <= phase_t'low;
        else
          phase <= phase_t'succ(phase);
        end if;

        if phase = phase_t'low then
          -- Quartus does not support select here
          case state is
            when start_bit => tx <= '0';
            when data => tx <= buf(data_idx);
            -- TODO: Implement parity
            when parity => tx <= '1';
            when stop_bits => tx <= '1';
          end case;
        end if;

        if phase = nphases then
          case state is
            when start_bit =>
              state <= data;
              data_idx <= data_idx_t'low;
            when data =>
              if data_idx = data_idx_t'high then
                if parity_type = none then
                  state <= stop_bits;
                  stop_idx <= stop_idx_t'low;
                else
                  state <= parity;
                end if;
              else
                data_idx <= data_idx_t'succ(data_idx);
              end if;
            when parity =>
              state <= stop_bits;
              stop_idx <= stop_idx_t'low;
            when stop_bits =>
              if stop_idx = stop_idx_t'high then
                state <= start_bit;
                done_toggle <= not done_toggle;
              else
                stop_idx <= stop_idx_t'succ(stop_idx);
              end if;
          end case;
        end if;
      end if;
    end if;
  end process;
end;
