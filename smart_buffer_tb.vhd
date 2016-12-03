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

  constant C_NUM_OUTPUTS : positive := 3;
  constant C_DATA_WIDTH : positive := 16;

  signal clk        : std_logic   := '0';
  signal rst        : std_logic   := '0';
  signal rd_en      : std_logic   := '0';
  signal wr_en      : std_logic   := '0';
  signal dma_status : std_logic   := '0';
  signal sb_done    : std_logic   := '0';
  signal full       : std_logic   := '0';
  signal empty      : std_logic   := '1';
  signal data_in    : std_logic_vector(C_DATA_WIDTH-1 downto 0)   := (others => '0');
  signal data_out   : std_logic_vector(C_NUM_OUTPUTS*C_DATA_WIDTH-1 downto 0) := (others=>'0');

  -- extra signals
  signal clkEn : std_logic := '1';

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

  clkEn <= '1';
  -- Starts here
  clk <= not clk and clkEn after 10 ns;

process
  begin

    rst <= '1';
    dma_status <= '1';
    data_in <= (others => '0');

    wait for 200 ns;
    rst <= '0';
    dma_status <= '0';

    for i in 0 to 4 loop
      wait until rising_edge(clk);
    end loop;

    -- enable reads
    rd_en <= '1';

    -- Start testing inputs
    data_in <= std_logic_vector(to_unsigned(1, C_DATA_WIDTH));
    wr_en <= '1';
    wait until rising_edge(clk);

    data_in <= std_logic_vector(to_unsigned(2, C_DATA_WIDTH));
    wr_en <= '1';
    wait until rising_edge(clk);
    wr_en <= '0';
    wait until rising_edge(clk);

    data_in <= std_logic_vector(to_unsigned(3, C_DATA_WIDTH));
    wr_en <= '1';
    wait until rising_edge(clk);
    wr_en <= '0';
    wait until rising_edge(clk);

    data_in <= std_logic_vector(to_unsigned(4, C_DATA_WIDTH));
    wr_en <= '1';
    wait until rising_edge(clk);
    wr_en <= '0';
    wait until rising_edge(clk);

    data_in <= std_logic_vector(to_unsigned(5, C_DATA_WIDTH));
    wr_en <= '1';
    wait until rising_edge(clk);
    wr_en <= '0';
    wait until rising_edge(clk);

    data_in <= std_logic_vector(to_unsigned(6, C_DATA_WIDTH));
    wr_en <= '1';
    wait until rising_edge(clk);
    wr_en <= '0';

    -- disable reads
    rd_en <= '0';

    for i in 0 to 5 loop
      wait until rising_edge(clk);
    end loop;

    -- Just a check
    rd_en <= '1';
    wait until rising_edge(clk);
    rd_en <= '0';

    -- indicate the dma read has been finished
    dma_status <= '1';

    clkEn <= '0';
    wait;

  end process;
end sb_test;
