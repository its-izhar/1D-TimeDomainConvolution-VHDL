-- Izhar Shaikh
--
-- File: smart_buffer_tb.vhd
-- Description: testbench for smart_buffer.vhd
--
-- Entity has only one data_in of data_width size and output of size num_outputs*data_width

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sb_test is
end sb_test;

architecture sb_test of smart_buffer_tb is

  constant C_NUM_OUTPUTS : positive := 128;
  constant C_DATA_WIDTH : positive := 16;

  signal clk        : std_logic   := '0';
  signal rst        : std_logic   := '0';
  signal rd_en      : std_logic   := '0';
  signal wr_en      : std_logic   := '0';
  signal dma_status : std_logic   := '0';
  signal sb_done    : std_logic   := '0';
  signal full       : std_logic   := '0';
  signal empty      : std_logic   := '1';
  signal data_in    : std_logic_vector(C_DATA_WIDTH-1 downto 0) := (others => '0');
  signal data_out   : std_logic_vector(C_NUM_OUTPUTS*C_DATA_WIDTH-1 downto 0) := (others=>'0');

  -- extra signals
  signal clkEn : std_logic := '1';

  -- loop count for test data
  constant C_LOOP_COUNT : positive := 10*C_NUM_OUTPUTS;

  -- define reg_array for expected inputs and outputs
  type reg_array is array(integer range<>) of std_logic_vector(C_DATA_WIDTH-1 downto 0);
  signal data_in_expected : reg_array(0 to C_NUM_OUTPUTS-1) := (others=>(others=>'0'));
  -- expected outputs
  signal data_out_expected : std_logic_vector(C_NUM_OUTPUTS*C_DATA_WIDTH-1 downto 0) := (others=>'0');

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

begin

  U_SB : entity work.smart_buffer
  generic map (
    num_outputs => C_NUM_OUTPUTS,
    data_width  => C_DATA_WIDTH
  )
  port map (
    clk        => clk,
    rst        => rst,
    rd_en      => rd_en,
    wr_en      => wr_en,
    dma_status => dma_status,
    sb_done    => sb_done,
    full       => full,
    empty      => empty,
    data_in    => data_in,
    data_out   => data_out
  );

  -- Starts here
  clk <= not clk and clkEn after 10 ns;

process
  begin

  -- reset conditions
  clkEn <= '1';
  rst <= '1';
  dma_status <= '0';
  rd_en <= '0';
  wr_en <= '0';
  data_in <= std_logic_vector(to_unsigned(0, C_DATA_WIDTH));

  --wait for some time
  wait for 50 ns;
  rst <= '0';

  -- wait for some time for smart_buffer to initialize properly
  for i in 0 to 5 loop
    wait until rising_edge(clk);
  end loop;

  -- enable reads
  rd_en <= '1';

  -- Start testing a range of inputs
  for i in 1 to C_LOOP_COUNT loop

    -- send data_in and enable writes and wait for a cycle
    data_in <= std_logic_vector(to_unsigned(i, C_DATA_WIDTH));
    wr_en <= '1';

    -- keep adding data to expected inputs array every cycle
    data_in_expected(0) <= std_logic_vector(to_unsigned(i, C_DATA_WIDTH));

    -- compute the output
      for j in 0 to C_NUM_OUTPUTS-2 loop
        data_in_expected(j+1) <= data_in_expected(j);
      end loop;

    -- wait for a cycle
    wait until rising_edge(clk);

    --if it's the last loop; stop after writing the data
    if (i = C_LOOP_COUNT) then
      wr_en <= '0';
    end if;

    -- convert the output to std_logic_vector
    data_out_expected <= vectorize(data_in_expected, C_NUM_OUTPUTS, C_DATA_WIDTH);

    -- check if we get the valid data out
    if (empty = '0') then
      -- check for output
      assert(data_out_expected = data_out)
        report "Output is incorrect. The output is " &
        integer'image(to_integer(unsigned(data_out_expected))) &
        " but should be " & integer'image(to_integer(unsigned(data_out)));
    end if;

  end loop;

  -- wait for some time before asserting done
  wait until rising_edge(clk);
  wait until rising_edge(clk);

  -- assert done and check if the buffer reports done
  dma_status <= '1';
  wait until rising_edge(clk);
  wait until rising_edge(clk);
  if(sb_done /= '1') then
    report "Done is not asserted." severity error;
  end if;

  -- wait for some time until we update done
  for i in 0 to 1 loop
    wait until rising_edge(clk);
  end loop;

  -- disable clock
  clkEn <= '0';
  wait;

  end process;
end sb_test;
