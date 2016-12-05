-- Izhar Shaikh
--
-- File: kernel_buffer_tb.vhd
-- Description: testbench for kernel_buffer.vhd
--
-- Entity has only one data_in of data_width size and output of size num_outputs*data_width

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity kernel_buffer_tb is
end kernel_buffer_tb;

architecture kb_test of kernel_buffer_tb is

  constant C_NUM_OUTPUTS : positive := 64;
  constant C_DATA_WIDTH : positive := 16;

  signal clk        : std_logic   := '0';
  signal rst        : std_logic   := '0';
  signal wr_en      : std_logic   := '0';
  signal kb_done    : std_logic   := '0';
  signal full       : std_logic   := '0';
  signal empty      : std_logic   := '1';
  signal data_in    : std_logic_vector(C_DATA_WIDTH-1 downto 0)   := (others => '0');
  signal data_out   : std_logic_vector(C_NUM_OUTPUTS*C_DATA_WIDTH-1 downto 0) := (others=>'0');

  -- extra signals
  signal clkEn : std_logic := '1';

  -- loop count for test data
  constant C_LOOP_COUNT : integer := C_NUM_OUTPUTS;

  -- define reg_array for expected inputs and outputs
  type reg_array is array(integer range<>) of std_logic_vector(C_DATA_WIDTH-1 downto 0);
  signal data_in_expected : reg_array(0 to C_LOOP_COUNT-1);
  -- expected outputs
  signal data_out_expected : std_logic_vector(C_NUM_OUTPUTS*C_DATA_WIDTH-1 downto 0) := (others=>'0');
  signal data_out_expected_array : reg_array(0 to C_NUM_OUTPUTS-1) := (others=>(others=>'0'));

  -- converts from reg_out_array to std_logic_vector
  function vectorize(input        : reg_array;
                     arraySize    : natural;
                     elementWidth : positive) return std_logic_vector is
    variable temp : std_logic_vector(arraySize*elementWidth-1 downto 0);
  begin
    for i in 0 to arraySize-1 loop
      temp((i+1)*elementWidth-1 downto i*elementWidth) := input(input'left+i);
    end loop;

    return temp;
  end function;

  -- returns the expected output window from data_in inputs
  -- it basically returns an array ...
  function getOutputWindow(input        : reg_array;
                           arraySize    : natural;
                           outputSize   : positive) return reg_array is
    variable temp : reg_array(outputSize-1 downto 0) := (others=>(others=>'0'));
  begin

    for i in 0 to outputSize-1 loop
      temp(temp'left-i) := input(input'right-i);
    end loop;

    return temp;
  end function;

begin

  U_KB : entity work.kernel_buffer
  generic map (
    num_outputs => C_NUM_OUTPUTS,
    data_width  => C_DATA_WIDTH
  )
  port map (
    clk        => clk,
    rst        => rst,
    wr_en      => wr_en,
    kb_done    => kb_done,
    full       => full,
    empty      => empty,
    data_in    => data_in,
    data_out   => data_out
  );

  -- Starts here
  clk <= not clk and clkEn after 10 ns;

process
  variable cycle_count : integer := 0;
  begin

  -- reset conditions
  clkEn <= '1';
  rst <= '1';
  wr_en <= '0';
  data_in <= std_logic_vector(to_unsigned(0, C_DATA_WIDTH));

  --wait for some time
  wait for 50 ns;
  rst <= '0';

  -- wait for some time for kernel_buffer to initialize properly
  for i in 0 to 5 loop
    wait until rising_edge(clk);
  end loop;

  -- Start testing a range of inputs
  for i in 1 to C_LOOP_COUNT loop

    -- send data_in and enable writes and wait for a cycle
    data_in <= std_logic_vector(to_unsigned(i, C_DATA_WIDTH));
    wr_en <= '1';
    wait until rising_edge(clk);

    --if it's the last loop; stop after writing the data
    if (i = C_LOOP_COUNT) then
      wr_en <= '0';
    end if;

    -- keep adding data to expected inputs array every cycle
    data_in_expected(i-1) <= std_logic_vector(to_unsigned(i, C_DATA_WIDTH));

    -- update the cycle count
    cycle_count := cycle_count + 1;

    -- check if we get the valid data out
    if (empty = '0') then
      data_out_expected_array <= getOutputWindow(data_in_expected, C_LOOP_COUNT, C_NUM_OUTPUTS);
      data_out_expected <= vectorize(data_out_expected_array, C_NUM_OUTPUTS, C_DATA_WIDTH);
      assert(data_out_expected = data_out)
          report "Output is incorrect. The output is " &
                    integer'image(to_integer(unsigned(data_out_expected))) &
                    " but should be " & integer'image(to_integer(unsigned(data_out)));
    end if;

  end loop;

  wait until rising_edge(clk);
  wait until rising_edge(clk);

  -- wait for some time until we update done
  for i in 0 to 1 loop
    wait until rising_edge(clk);
  end loop;

  -- disable clock
  clkEn <= '0';
  wait;

  end process;
end kb_test;