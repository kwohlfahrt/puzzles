library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.util.all;

entity tx is
  generic ( bit_clocks : positive;
            -- TODO: Add slack like rx
            n_stop_bits : natural range 1 to 2 := 1;
            parity_type : parity_t := none );
  port ( clk : in std_logic;
         valid : in std_logic;
         input : in std_logic_vector(7 downto 0);
         tx : out std_logic;
         ready : buffer std_logic );
end;

architecture structure of tx is
  -- Quartus does not support 'subtype annotation
  subtype phase_t is natural range 1 to bit_clocks;
  subtype data_idx_t is natural range input'reverse_range;
  subtype stop_idx_t is natural range 1 to n_stop_bits;

  signal state : state_t := start_bit;
  signal phase : phase_t := phase_t'low;
  signal start_toggle, done_toggle : boolean := false;

  signal buf : std_logic_vector(input'range);
  signal data_idx : data_idx_t := data_idx_t'left;
  signal stop_idx : stop_idx_t := stop_idx_t'low;
  signal idle : boolean := true;
  signal tx_sample : std_logic;
begin
  ready <= '1' when start_toggle = done_toggle else '0';
  tx <= tx_sample or ready;

  with state select tx_sample <=
    '0' when start_bit,
    buf(data_idx) when data,
    '1' when stop_bits;

  process (clk)
  begin
    if rising_edge(clk) and ready = '1' then
        if not valid then
          idle <= true;
        else
          start_toggle <= not start_toggle;
          buf <= input;
          idle <= false;
        end if;
    end if;

    if rising_edge(clk) and not idle then
      if phase = phase_t'high then
        phase <= phase_t'low;
      else
        phase <= phase_t'succ(phase);
      end if;

      if phase = phase_t'pred(phase_t'high)
        and state = stop_bits
        and stop_idx = stop_idx_t'high
      then
        done_toggle <= not done_toggle;
      end if;

      if phase = phase_t'high then
        case state is
          when start_bit =>
            state <= data;
          when data =>
            if data_idx = data_idx_t'right then
              data_idx <= data_idx_t'left;
              state <= stop_bits;
            else
              data_idx <= data_idx_t'rightof(data_idx);
            end if;
          when stop_bits =>
            if stop_idx = stop_idx_t'high then
              stop_idx <= stop_idx_t'low;
              state <= start_bit;
            else
              stop_idx <= stop_idx_t'succ(stop_idx);
            end if;
        end case;
      end if;
    end if;
  end process;
end;
