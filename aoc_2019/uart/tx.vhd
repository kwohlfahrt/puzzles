library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.uart_util.all;

entity tx is
  generic ( bit_clocks : positive;
            n_stop_bits : natural range 1 to 2 := 2;
            parity_type : parity_t := none );
  port ( clk : in std_logic;
         tx : out std_logic;
         input : in std_logic_vector(7 downto 0);
         ready : in std_logic );
end;

architecture structure of tx is
  -- Quartus does not support _t annotation
  subtype phase_t is natural range 1 to bit_clocks;
  subtype data_idx_t is natural range input'right to input'left;
  subtype stop_idx_t is natural range 1 to n_stop_bits;

  signal state : state_t := stop_bits;
  signal phase : phase_t := bit_clocks;
  signal start_toggle, done_toggle : std_logic := '0';

  signal buf : std_logic_vector(input'range);
  signal data_idx : data_idx_t;
  signal stop_idx : stop_idx_t;
  signal idle : boolean;
begin
  idle <= start_toggle = done_toggle;

  with state select tx <=
    '0' when start_bit,
    buf(data_idx) when data,
    '1' when stop_bits;

  process (ready, idle)
  begin
    if rising_edge(ready) and idle then
      start_toggle <= not start_toggle;
      buf <= input;
    end if;
  end process;

  process (clk)
  begin
    if rising_edge(clk) then
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
            state <= data;
            data_idx <= data_idx_t'left;
          when data =>
            if data_idx = data_idx_t'right then
              state <= stop_bits;
              stop_idx <= stop_idx_t'left;
            else
              data_idx <= data_idx_t'rightof(data_idx);
            end if;
          when stop_bits =>
            -- FIXME: These constants are weird
            if stop_idx = 4 then
              state <= start_bit;
            elsif stop_idx = 3 then
              done_toggle <= not done_toggle;
              stop_idx <= stop_idx_t'rightof(stop_idx);
            elsif stop_idx < 3 then
              stop_idx <= stop_idx_t'rightof(stop_idx);
            end if;
        end case;
      end if;
    end if;
  end process;
end;
