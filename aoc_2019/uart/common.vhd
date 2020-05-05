package uart_util is
  type parity_t is (odd, even, none);
  type state_t is (start_bit, data, stop_bits);
end package;
