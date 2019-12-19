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
            parity_type : parity_t := none );
  port ( clk : in std_logic;
         rx : in std_logic;
         output : out std_logic_vector(0 to 7);
         ready : out std_logic );
end;

architecture structure of uart_rx is
  signal state : state_t := start_bit;
  signal phase : natural range 1 to bit_clocks := 1;
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
  signal ready_toggle, done_toggle : std_logic := '0';

  signal buf : std_logic_vector(input'range);
  signal idx : natural range buf'range;
  signal idle : boolean;
begin
  idle <= ready_toggle = done_toggle;

  with state select tx <=
    '0' when start_bit,
    buf(idx) when data,
    '1' when stop_bits;

  process (ready)
  begin
    if rising_edge(ready) then
      ready_toggle <= not ready_toggle;
      buf <= input;
    end if;
  end process;

  process (clk)
  begin
    if rising_edge(clk) then
      if idle then
        phase <= phase'left;
      else
        if phase = phase'right then
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
    end if;
  end process;
end;
