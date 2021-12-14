library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library uart;
library int_io;
library ram;
use ram.util.log2;

library bcd;
use bcd.bcd.all;

library uart;
use uart.util.from_ascii;

library seven_segment;
use seven_segment.seven_segments.seven_segments;

entity day2 is
  port ( clk : in std_logic;
         reset : in std_logic := '0';
         part : in natural range 1 to 2 := 1;
         in_value : in std_logic_vector(7 downto 0);
         in_ready : out std_logic;
         in_valid : in std_logic;
         out_value : out std_logic_vector(7 downto 0) := (others => '0');
         out_ready : in std_logic;
         out_valid : out std_logic;
         seven_segments : out seven_segments(3 downto 0) );
end;

architecture arch of day2 is
  type mode is (programming, loading, executing, halt);

  constant ram_size : positive := 192;
  constant addr_size : positive := log2(ram_size);
  constant data_size : positive := 16;

  subtype addr_t is unsigned(addr_size-1 downto 0);
  subtype data_t is unsigned(data_size-1 downto 0);
  -- Quartus doesn't support 'subtype attribute, so define explicitly
  subtype step_t is natural range 0 to 3 ;

  signal display_value : decimal(3 downto 0) := to_decimal(0, 4);
  signal input_addr, pc : addr_t := to_unsigned(0, addr_size);
  signal step : step_t := 0;
  signal current_mode : mode := programming;

  signal value_valid, count_valid, count_ready, write_enable, retire : std_logic;
  signal value : decimal(5 downto 0);
  signal read_addr, output_addr, op_addr : addr_t;
  signal data_in, data_out, output, opcode, op1, op2 : data_t;
  signal mul : unsigned(data_size*2-1 downto 0);
  signal sum : unsigned(data_size-1 downto 0);
begin
  with current_mode select data_in <=
    to_unsigned(value, data_size) when programming,
    output when executing,
    (others => '-') when others;

  mul <= op1 * op2;
  sum <= op1 + op2;

  with opcode select output <=
    sum(data_t'range) when to_unsigned(1, data_t'length),
    mul(data_t'range) when to_unsigned(2, data_t'length),
    (others => '-') when others;

  retire <= '1' when step = 3 else '0';

  with current_mode select write_enable <=
    value_valid when programming,
    retire when executing,
    '0' when others;

  with step select op_addr <=
    op1(op_addr'range) when 1,
    op2(op_addr'range) when 2,
    (others => '-') when others;

  with current_mode select read_addr <=
    pc when loading,
    op_addr when executing,
    (others => '-') when others;

  out_valid <= '1' when current_mode = halt else '0';

  -- FIXME: Support multiple separators, the trailing LF needs to delimit the last value too
  decoder : entity int_io.decode generic map ( value_size => value'length, sep => unsigned(from_ascii(',')) )
    port map ( clk => clk, reset => reset,
               byte => in_value, byte_valid => in_valid, byte_ready => in_ready,
               value => value, value_valid => value_valid, value_ready => '1' );

  memory : entity ram.ram generic map ( size => ram_size, data_size => data_size )
    port map ( clk => clk,
               write_enable => write_enable, write_addr => input_addr, data_in => data_in,
               data_out => data_out, read_addr => read_addr );

  process (clk, reset)
  begin
    if reset then
      input_addr <= to_unsigned(0, input_addr'length);
    elsif rising_edge(clk) then
      case current_mode is
        when programming =>
          if value_valid = '1' then
            input_addr <= input_addr + 1;
          elsif in_valid = '1' and in_value = from_ascii(LF) then
            current_mode <= loading;
          end if;
        when loading =>
          pc <= pc + 1;
          if step = step_t'high then
            step <= 0;
          else
            step <= step_t'succ(step);
          end if;

          case step is
            when 0 => opcode <= data_out;
            when 1 => op1 <= data_out;
            when 2 => op2 <= data_out;
            when 3 =>
              input_addr <= data_out(addr_t'range);
              step <= 0;
              if opcode = 99 then
                current_mode <= halt;
              else
                current_mode <= executing;
                step <= 1;
              end if;
          end case;
        when executing =>
          if step = step_t'high then
            step <= 0;
          else
            step <= step_t'succ(step);
          end if;

          case step is
            when 1 => op1 <= data_out;
            when 2 =>
              op2 <= data_out;
            when 3 =>
              current_mode <= loading;
            when others =>
          end case;
        when halt =>
          -- no-op
      end case;
    end if;
  end process;
end;
