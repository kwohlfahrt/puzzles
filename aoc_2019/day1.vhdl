-- Part 1

entity fuel_counter is
  port (mass : in natural;
        fuel : out natural);
end fuel_counter;

architecture behave of fuel_counter is
begin
  fuel <= 0 when mass / 3 < 2 else mass / 3 - 2;
end behave;

entity counter_upper is
  port (input: in natural;
        reset: in boolean;
        output: out natural);
end counter_upper;

architecture behave of counter_upper is
begin
  process (input, reset)
    variable counter : natural := 0;
  begin
    if reset then
      counter := 0;
    end if;
    counter := counter + input;
    output <= counter;
  end process;
end behave;

-- Part 2

entity total_fuel_counter is
  port (mass : in natural;
        fuel : out natural);
end total_fuel_counter;

architecture behave of total_fuel_counter is
  signal item_mass, item_fuel, sub_total : natural;
  signal reset : boolean;
begin
  counter: entity work.fuel_counter(behave)
    port map (mass => item_mass, fuel => item_fuel);
  upper: entity work.counter_upper(behave)
    port map (input => item_fuel, reset => reset, output => sub_total);
  process
  begin
    wait on mass;
    reset <= false;
    item_mass <= mass;
    wait on item_fuel;
    while item_fuel /= 0 loop
      item_mass <= item_fuel;
      wait on item_fuel;
    end loop;
    fuel <= sub_total;
    reset <= true;
  end process;
end behave;
