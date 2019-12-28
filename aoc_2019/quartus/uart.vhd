package uart_util is
  type parity_t is (odd, even, none);
  type state_t is (start_bit, data, stop_bits);
end package;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.uart_util.all;

entity uart_rx is
  -- FIXME: There is a timing issue - only works with 2 stop bits (+cstopb)
  generic ( bit_clocks : positive;
            parity_type : parity_t := none;
            n_stop_bits : positive range 1 to 2 := 1 );
  port ( clk : in std_logic;
         rx : in std_logic;
         output : out std_logic_vector(0 to 7);
         ready : out std_logic );
end;

architecture structure of uart_rx is
  signal state : state_t := stop_bits;
  signal phase : natural range 1 to bit_clocks := 1;
  signal start_toggle, done_toggle : boolean := false;
  signal err : boolean := true; -- Nothing received

  signal idle : boolean;
  signal idx : natural range output'range;
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
        phase <= phase'left;
      elsif phase = phase'right then
        phase <= phase'left;
      else
        phase <= phase + 1;
      end if;

      if phase = phase'right then
        case state is
          when start_bit =>
            if samples(samples'length / 2) /= '0' then
              done_toggle <= not done_toggle;
              err <= true;
            else
              state <= data;
              idx <= 0;
              err <= false;
            end if;
          when data =>
            buf(idx) <= samples(samples'length / 2);
            if idx = idx'right then
              state <= stop_bits;
              idx <= 0;
            else
              idx <= idx + 1;
            end if;
          when stop_bits =>
            idx <= idx + 1;
            if samples(samples'length / 2) /= '1' then
              err <= true;
              state <= start_bit;
              done_toggle <= not done_toggle;
            end if;
        end case;
      end if;

      if state = stop_bits and idx + 1 = n_stop_bits and phase > samples'length / 2 then
        state <= start_bit;
        done_toggle <= not done_toggle;
        if samples(samples'length / 2) /= '1' then
          err <= true;
        else
          output <= buf;
        end if;
      end if;
    end if;
  end process;
end;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.uart_util.all;

entity uart_tx is
  generic ( bit_clocks : positive;
            n_stop_bits : natural range 1 to 2 := 2;
            parity_type : parity_t := none );
  port ( clk : in std_logic;
         tx : out std_logic;
         input : in std_logic_vector(0 to 7);
         ready : in std_logic );
end;

architecture structure of uart_tx is
  signal state : state_t := stop_bits;
  signal phase : natural range 1 to bit_clocks := bit_clocks;
  signal start_toggle, done_toggle : std_logic := '0';

  signal buf : std_logic_vector(input'range);
  signal idx : natural range buf'range;
  signal idle : boolean;
begin
  idle <= start_toggle = done_toggle;

  with state select tx <=
    '0' when start_bit,
    buf(idx) when data,
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
        phase <= phase'left;
      elsif phase = phase'right then
        phase <= phase'left;
      else
        phase <= phase + 1;
      end if;

      if phase = phase'right then
        case state is
          when start_bit =>
            state <= data;
            idx <= 0;
          when data =>
            if idx = idx'right then
              state <= stop_bits;
              idx <= 0;
            else
              idx <= idx + 1;
            end if;
          when stop_bits =>
            if idx = 3 then
              state <= start_bit;
            elsif idx = 2 then
              done_toggle <= not done_toggle;
              idx <= idx + 1;
            elsif idx < 2 then
              idx <= idx + 1;
            end if;
        end case;
      end if;
    end if;
  end process;
end;
