package util is
  type parity_t is (odd, even, none);
  type state_t is (start_bit, data, parity, stop_bits);
end package;
