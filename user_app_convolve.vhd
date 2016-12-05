-- Izhar Shaikh
--
-- File: user_app_convolve.vhd
-- Description: user_app entity for computing the 1D Time Domain Convolution
--

library ieee;
use ieee.std_logic_1164.all;

use work.config_pkg.all;
use work.user_pkg.all;

entity user_app is
    port (
        clks   : in  std_logic_vector(NUM_CLKS_RANGE);
        rst    : in  std_logic;
        sw_rst : out std_logic;

        -- memory-map interface
        mmap_wr_en   : in  std_logic;
        mmap_wr_addr : in  std_logic_vector(MMAP_ADDR_RANGE);
        mmap_wr_data : in  std_logic_vector(MMAP_DATA_RANGE);
        mmap_rd_en   : in  std_logic;
        mmap_rd_addr : in  std_logic_vector(MMAP_ADDR_RANGE);
        mmap_rd_data : out std_logic_vector(MMAP_DATA_RANGE);

        -- DMA interface for RAM 0
        -- read interface
        ram0_rd_rd_en : out std_logic;
        ram0_rd_clear : out std_logic;
        ram0_rd_go    : out std_logic;
        ram0_rd_valid : in  std_logic;
        ram0_rd_data  : in  std_logic_vector(RAM0_RD_DATA_RANGE);
        ram0_rd_addr  : out std_logic_vector(RAM0_ADDR_RANGE);
        ram0_rd_size  : out std_logic_vector(RAM0_RD_SIZE_RANGE);
        ram0_rd_done  : in  std_logic;
        -- write interface
        ram0_wr_ready : in  std_logic;
        ram0_wr_clear : out std_logic;
        ram0_wr_go    : out std_logic;
        ram0_wr_valid : out std_logic;
        ram0_wr_data  : out std_logic_vector(RAM0_WR_DATA_RANGE);
        ram0_wr_addr  : out std_logic_vector(RAM0_ADDR_RANGE);
        ram0_wr_size  : out std_logic_vector(RAM0_WR_SIZE_RANGE);
        ram0_wr_done  : in  std_logic;

        -- DMA interface for RAM 1
        -- read interface
        ram1_rd_rd_en : out std_logic;
        ram1_rd_clear : out std_logic;
        ram1_rd_go    : out std_logic;
        ram1_rd_valid : in  std_logic;
        ram1_rd_data  : in  std_logic_vector(RAM1_RD_DATA_RANGE);
        ram1_rd_addr  : out std_logic_vector(RAM1_ADDR_RANGE);
        ram1_rd_size  : out std_logic_vector(RAM1_RD_SIZE_RANGE);
        ram1_rd_done  : in  std_logic;
        -- write interface
        ram1_wr_ready : in  std_logic;
        ram1_wr_clear : out std_logic;
        ram1_wr_go    : out std_logic;
        ram1_wr_valid : out std_logic;
        ram1_wr_data  : out std_logic_vector(RAM1_WR_DATA_RANGE);
        ram1_wr_addr  : out std_logic_vector(RAM1_ADDR_RANGE);
        ram1_wr_size  : out std_logic_vector(RAM1_WR_SIZE_RANGE);
        ram1_wr_done  : in  std_logic
        );
end user_app;

architecture default of user_app is

    signal go        : std_logic;
    signal sw_rst_s  : std_logic;
    signal rst_s     : std_logic;

    -- MMAP Signals
    signal ram0_rd_addr : std_logic_vector(RAM0_ADDR_RANGE) := (others=>'0');
    signal ram1_wr_addr : std_logic_vector(RAM1_ADDR_RANGE) := (others=>'0');
    signal signal_size   : std_logic_vector(RAM0_RD_SIZE_RANGE);
    signal kernel_data   : std_logic_vector(KERNEL_WIDTH_RANGE);
    signal kernel_load   : std_logic;
    signal kernel_loaded : std_logic;
    signal done      : std_logic;

    -- Smart Buffer Signals
    signal smart_buffer_rd_en      : std_logic;
    signal smart_buffer_wr_en      : std_logic;
    signal smart_buffer_dma_status : std_logic;
    signal smart_buffer_sb_done    : std_logic;
    signal smart_buffer_full       : std_logic;
    signal smart_buffer_empty      : std_logic;
    signal smart_buffer_data_in    : std_logic_vector(C_SIGNAL_WIDTH-1 downto 0);
    signal smart_buffer_data_out   : std_logic_vector(C_SMART_BUFFER_SIZE*C_SIGNAL_WIDTH-1 downto 0);

    -- Kernel Buffer Signals
    signal kernel_buffer_wr_en    : std_logic;
    signal kernel_buffer_kb_done  : std_logic;
    signal kernel_buffer_full     : std_logic;
    signal kernel_buffer_empty    : std_logic;
    signal kernel_buffer_data_in  : std_logic_vector(C_KERNEL_WIDTH-1 downto 0);
    signal kernel_buffer_data_out : std_logic_vector(C_KERNEL_SIZE*C_KERNEL_WIDTH-1 downto 0);

    -- Datapath Signals
    datapath_en, datapath_valid_in, datapath_valid_out     : std_logic;
    datapath_output : std_logic_vector(C_SIGNAL_WIDTH+C_KERNEL_WIDTH+clog2(C_DATAPATH_INPUT_SIZE)-1 downto 0);


begin

    U_MMAP : entity work.memory_map
        port map (
            clk     => clks(C_CLK_USER),
            rst     => rst,
            wr_en   => mmap_wr_en,
            wr_addr => mmap_wr_addr,
            wr_data => mmap_wr_data,
            rd_en   => mmap_rd_en,
            rd_addr => mmap_rd_addr,
            rd_data => mmap_rd_data,

            -- dma interface for accessing DRAM from software
            ram0_wr_ready => ram0_wr_ready,
            ram0_wr_clear => ram0_wr_clear,
            ram0_wr_go    => ram0_wr_go,
            ram0_wr_valid => ram0_wr_valid,
            ram0_wr_data  => ram0_wr_data,
            ram0_wr_addr  => ram0_wr_addr,
            ram0_wr_size  => ram0_wr_size,
            ram0_wr_done  => ram0_wr_done,

            ram1_rd_rd_en => ram1_rd_rd_en,
            ram1_rd_clear => ram1_rd_clear,
            ram1_rd_go    => ram1_rd_go,
            ram1_rd_valid => ram1_rd_valid,
            ram1_rd_data  => ram1_rd_data,
            ram1_rd_addr  => ram1_rd_addr,
            ram1_rd_size  => ram1_rd_size,
            ram1_rd_done  => ram1_rd_done,

            -- circuit interface from software
            go        => go,
            sw_rst    => sw_rst_s,
            signal_size   => signal_size,
            kernel_data   => kernel_data,
            kernel_load   => kernel_load,
            kernel_loaded => kernel_loaded,
            done      => done
            );

-- Reset Signals
    rst_s  <= rst or sw_rst_s;
    sw_rst <= sw_rst_s;

    U_CTRL : entity work.ctrl
        port map (
            clk           => clks(C_CLK_USER),
            rst           => rst_s,
            go            => go,
            mem_in_go     => ram0_rd_go,
            mem_out_go    => ram1_wr_go,
            mem_in_clear  => ram0_rd_clear,
            mem_out_clear => ram1_wr_clear,
            mem_out_done  => ram1_wr_done,
            done          => done);

-- DMA Interface for RAM0 (Read)
    ram0_rd_addr  <= ram0_rd_addr;    -- Starts at addr 0
    ram0_rd_rd_en <= ram0_rd_valid;
    -- FIXME:: ram0_rd_size  <= signal_size;


    U_SMART_BUFFER : entity work.smart_buffer(bhv_sbuff)
    generic map (
      num_outputs => C_SMART_BUFFER_SIZE,
      data_width  => C_SIGNAL_WIDTH
    )
    port map (
      clk        => clks(C_CLK_USER),
      rst        => rst_s,
      rd_en      => smart_buffer_rd_en,
      wr_en      => smart_buffer_wr_en,
      dma_status => smart_buffer_dma_status,
      sb_done    => smart_buffer_sb_done,
      full       => smart_buffer_full,
      empty      => smart_buffer_empty,
      data_in    => smart_buffer_data_in,
      data_out   => smart_buffer_data_out
    );

    -- NOTE: data read from ram0 will feed the smart_buffer whenever valid
    -- read data is available and the buffer isn't full;
    -- it will stop the feed once the read is complete
    smart_buffer_data_in    <= ram0_rd_data;
    smart_buffer_wr_en      <= ram0_rd_valid and not smart_buffer_full;
    smart_buffer_dma_status <= ram0_rd_done;

    -- read from smart_buffer all the as long as datapath is enabled
    -- (reading empty not partially filled smart buffer won't make a difference
    -- since it will be invalid data)
    smart_buffer_rd_en      <= datapath_en


    U_KERNEL_BUFFER : entity work.kernel_buffer(bhv_kbuff)
    generic map (
      num_outputs => C_KERNEL_SIZE,
      data_width  => C_KERNEL_WIDTH
    )
    port map (
      clk      => clks(C_CLK_USER),
      rst      => rst_s,
      wr_en    => kernel_buffer_wr_en,
      kb_done  => kernel_buffer_kb_done,
      full     => kernel_buffer_full,
      empty    => kernel_buffer_empty,
      data_in  => kernel_buffer_data_in,
      data_out => kernel_buffer_data_out
    );

    -- NOTE: memory_map interface will load the kernel_buffer from software
    -- whenever kernel_load is available and the buffer isn't full;
    -- it will stop the feed once the buffer notifies that it is completely loaded
    kernel_buffer_data_in  <= kernel_data;
    kernel_buffer_wr_en    <= kernel_load and not kernel_buffer_full;
    kernel_buffer_kb_done  <= kernel_loaded;


    -- Signifies valid data that has been processed by the datapath
    -- the data is only valid when both buffers are not empty
    datapath_valid_in <= not smart_buffer_empty and not kernel_buffer_empty;

    -- Creates a delay of the datapath_valid_in signal, which corresponds to the time
    -- when processed valid data is available from the datapath
    U_DATAPATH_VALID : entity work.delay
        generic map (
            cycles => clog2(C_DATAPATH_INPUT_SIZE)+1,
            width  => 1,
            init   => "0")
        port map (
            clk       => clks(C_CLK_USER),
            rst       => rst_s,
            en        => datapath_en,
            input(0)  => datapath_valid_in,
            output(0) => datapath_valid_out);


    -- NOTE: smart_buffer_data_out is connected to datapath input1
    -- NOTE: kernel_buffer_data_out is connected to datapath input2
    U_DATAPATH : entity work.mult_add_tree(unsigned_arch)
    generic map (
      num_inputs   => C_DATAPATH_INPUT_SIZE,
      input1_width => C_SIGNAL_WIDTH,
      input2_width => C_KERNEL_WIDTH
    )
    port map (
      clk    => clks(C_CLK_USER),
      rst    => rst_s,
      en     => datapath_en,
      input1 => smart_buffer_data_out, -- smart_buffer
      input2 => kernel_buffer_data_out, -- kernel_buffer
      output => datapath_output
    );

    -- NOTE: stall the datapath when output memory (fifo) is full (i.e. not ready)
    datapath_en <= ram1_wr_ready;

    -- DMA Interface for RAM1 (Write)
    ram1_wr_addr  <= ram1_wr_addr;    -- Starts at addr 0
    -- FIXME:: ram1_wr_size  <= signal_size;
    ram1_wr_data  <= datapath_output;
    ram1_wr_valid <= datapath_valid_out;

end default;
