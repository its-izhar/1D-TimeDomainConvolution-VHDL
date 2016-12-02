-- Izhar Shaikh
--
-- File: smart_buffer.vhd
-- Description: Assembles the data from FIFO into "windows" and feeds the datapath
--
-- Entity has only one data_in of data_width size and output of size num_outputs*data_width

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sb_tb is
end sb_tb;

architecture test_sb_default of sb_tb is

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

    -- Start testing inputs
    data_in <= std_logic_vector(to_unsigned(1, C_DATA_WIDTH));
    wr_en <= '1';
    wait until rising_edge(clk);
    wr_en <= '0';

    for i in 0 to 1 loop
      wait until rising_edge(clk);
    end loop;

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

    clkEn <= '0';
    wait;

  end process;
end test_sb_default;
