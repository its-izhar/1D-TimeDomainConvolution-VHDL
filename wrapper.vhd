-- Greg Stitt
-- University of Florida


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.config_pkg.all;
use work.user_pkg.all;

entity wrapper is
    port(
        clks : in std_logic_vector(NUM_CLKS_RANGE);
        rst  : in std_logic;

        mmap_wr_en   : in  std_logic;
        mmap_wr_addr : in  std_logic_vector(MMAP_ADDR_RANGE);
        mmap_wr_data : in  std_logic_vector(MMAP_DATA_RANGE);
        mmap_rd_en   : in  std_logic;
        mmap_rd_addr : in  std_logic_vector(MMAP_ADDR_RANGE);
        mmap_rd_data : out std_logic_vector(MMAP_DATA_RANGE)
        );
end wrapper;

architecture default of wrapper is

    ----------------------------------------------------------
    -- DMA interface signals
    signal ram0_rd_rd_en : std_logic;
    signal ram0_rd_clear : std_logic;
    signal ram0_rd_go    : std_logic;
    signal ram0_rd_valid : std_logic;
    signal ram0_rd_data  : std_logic_vector(RAM0_RD_DATA_RANGE);
    signal ram0_rd_addr  : std_logic_vector(RAM0_ADDR_RANGE);
    signal ram0_rd_size  : std_logic_vector(RAM0_RD_SIZE_RANGE);
    signal ram0_rd_done  : std_logic;

    signal ram0_wr_ready : std_logic;
    signal ram0_wr_clear : std_logic;
    signal ram0_wr_go    : std_logic;
    signal ram0_wr_valid : std_logic;
    signal ram0_wr_data  : std_logic_vector(RAM0_WR_DATA_RANGE);
    signal ram0_wr_addr  : std_logic_vector(RAM0_ADDR_RANGE);
    signal ram0_wr_size  : std_logic_vector(RAM0_WR_SIZE_RANGE);
    signal ram0_wr_done  : std_logic;

    signal ram1_rd_rd_en : std_logic;
    signal ram1_rd_clear : std_logic;
    signal ram1_rd_go    : std_logic;
    signal ram1_rd_valid : std_logic;
    signal ram1_rd_data  : std_logic_vector(RAM1_RD_DATA_RANGE);
    signal ram1_rd_addr  : std_logic_vector(RAM1_ADDR_RANGE);
    signal ram1_rd_size  : std_logic_vector(RAM1_RD_SIZE_RANGE);
    signal ram1_rd_done  : std_logic;

    signal ram1_wr_ready : std_logic;
    signal ram1_wr_clear : std_logic;
    signal ram1_wr_go    : std_logic;
    signal ram1_wr_valid : std_logic;
    signal ram1_wr_data  : std_logic_vector(RAM1_WR_DATA_RANGE);
    signal ram1_wr_addr  : std_logic_vector(RAM1_ADDR_RANGE);
    signal ram1_wr_size  : std_logic_vector(RAM1_WR_SIZE_RANGE);
    signal ram1_wr_done  : std_logic;

    ----------------------------------------------------------
    -- physical memory signals
    signal dram0_ready      : std_logic;
    signal dram0_wr_en      : std_logic;
    signal dram0_wr_addr    : std_logic_vector(DRAM0_ADDR_RANGE);
    signal dram0_wr_data    : std_logic_vector(DRAM0_DATA_RANGE);
    signal dram0_wr_pending : std_logic;
    signal dram0_rd_en      : std_logic;
    signal dram0_rd_addr    : std_logic_vector(DRAM0_ADDR_RANGE);
    signal dram0_rd_data    : std_logic_vector(DRAM0_DATA_RANGE);
    signal dram0_rd_valid   : std_logic;
    signal dram0_rd_flush   : std_logic;

    signal dram1_ready      : std_logic;
    signal dram1_wr_en      : std_logic;
    signal dram1_wr_addr    : std_logic_vector(DRAM1_ADDR_RANGE);
    signal dram1_wr_data    : std_logic_vector(DRAM1_DATA_RANGE);
    signal dram1_wr_pending : std_logic;
    signal dram1_rd_en      : std_logic;
    signal dram1_rd_addr    : std_logic_vector(DRAM1_ADDR_RANGE);
    signal dram1_rd_data    : std_logic_vector(DRAM1_DATA_RANGE);
    signal dram1_rd_valid   : std_logic;
    signal dram1_rd_flush   : std_logic;

    signal sw_rst, rst_s : std_logic;

    component dram_rd_ram0
        port(dram_clk   : in  std_logic;
             user_clk   : in  std_logic;
             rst        : in  std_logic;
             clear      : in  std_logic;
             go         : in  std_logic;
             rd_en      : in  std_logic;
             stall      : in  std_logic;
             start_addr : in  std_logic_vector(14 downto 0);
             size       : in  std_logic_vector(16 downto 0);
             valid      : out std_logic;
             data       : out std_logic_vector(15 downto 0);
             done       : out std_logic;

             dram_ready    : in  std_logic;
             dram_rd_en    : out std_logic;
             dram_rd_addr  : out std_logic_vector(14 downto 0);
             dram_rd_data  : in  std_logic_vector(31 downto 0);
             dram_rd_valid : in  std_logic;
             dram_rd_flush : out std_logic
             );
    end component;

    component dram_rd_ram1
        port(dram_clk   : in  std_logic;
             user_clk   : in  std_logic;
             rst        : in  std_logic;
             clear      : in  std_logic;
             go         : in  std_logic;
             rd_en      : in  std_logic;
             stall      : in  std_logic;
             start_addr : in  std_logic_vector(14 downto 0);
             size       : in  std_logic_vector(15 downto 0);
             valid      : out std_logic;
             data       : out std_logic_vector(31 downto 0);
             done       : out std_logic;

             dram_ready    : in  std_logic;
             dram_rd_en    : out std_logic;
             dram_rd_addr  : out std_logic_vector(14 downto 0);
             dram_rd_data  : in  std_logic_vector(31 downto 0);
             dram_rd_valid : in  std_logic;
             dram_rd_flush : out std_logic
             );
    end component;

    component dram_wr_ram0
        port(dram_clk   : in  std_logic;
             user_clk   : in  std_logic;
             rst        : in  std_logic;
             clear      : in  std_logic;
             go         : in  std_logic;
             wr_en      : in  std_logic;
             start_addr : in  std_logic_vector(14 downto 0);
             size       : in  std_logic_vector(15 downto 0);
             data       : in  std_logic_vector(31 downto 0);
             done       : out std_logic;
             ready      : out std_logic;

             dram_ready      : in  std_logic;
             dram_wr_en      : out std_logic;
             dram_wr_addr    : out std_logic_vector(14 downto 0);
             dram_wr_data    : out std_logic_vector(31 downto 0);
             dram_wr_pending : in  std_logic
             );
    end component;


    component dram_wr_ram1
        port(dram_clk   : in  std_logic;
             user_clk   : in  std_logic;
             rst        : in  std_logic;
             clear      : in  std_logic;
             go         : in  std_logic;
             wr_en      : in  std_logic;
             start_addr : in  std_logic_vector(14 downto 0);
             size       : in  std_logic_vector(16 downto 0);
             data       : in  std_logic_vector(15 downto 0);
             done       : out std_logic;
             ready      : out std_logic;

             dram_ready      : in  std_logic;
             dram_wr_en      : out std_logic;
             dram_wr_addr    : out std_logic_vector(14 downto 0);
             dram_wr_data    : out std_logic_vector(31 downto 0);
             dram_wr_pending : in  std_logic
             );
    end component;


begin

    rst_s <= rst or sw_rst;

    ----------------------------------------------------------------------
    -- Instantiate the main user application

    U_USER_APP : entity work.user_app_convolve
        port map (
            clks   => clks,
            rst    => rst,
            sw_rst => sw_rst,

            mmap_wr_en   => mmap_wr_en,
            mmap_wr_addr => mmap_wr_addr,
            mmap_wr_data => mmap_wr_data,
            mmap_rd_en   => mmap_rd_en,
            mmap_rd_addr => mmap_rd_addr,
            mmap_rd_data => mmap_rd_data,

            ram0_rd_rd_en => ram0_rd_rd_en,
            ram0_rd_clear => ram0_rd_clear,
            ram0_rd_go    => ram0_rd_go,
            ram0_rd_valid => ram0_rd_valid,
            ram0_rd_data  => ram0_rd_data,
            ram0_rd_addr  => ram0_rd_addr,
            ram0_rd_size  => ram0_rd_size,
            ram0_rd_done  => ram0_rd_done,

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

            ram1_wr_ready => ram1_wr_ready,
            ram1_wr_clear => ram1_wr_clear,
            ram1_wr_go    => ram1_wr_go,
            ram1_wr_valid => ram1_wr_valid,
            ram1_wr_data  => ram1_wr_data,
            ram1_wr_addr  => ram1_wr_addr,
            ram1_wr_size  => ram1_wr_size,
            ram1_wr_done  => ram1_wr_done
            );

    ----------------------------------------------------------------------
    -- Instantiate DMA controllers

    U_DRAM0_RD : dram_rd_ram0
        port map (
            -- user dma control signals
            dram_clk   => clks(C_CLK_DRAM),
            user_clk   => clks(C_CLK_USER),
            rst        => rst_s,
            clear      => ram0_rd_clear,
            go         => ram0_rd_go,
            rd_en      => ram0_rd_rd_en,
            stall      => C_0,
            start_addr => ram0_rd_addr,
            size       => ram0_rd_size,
            valid      => ram0_rd_valid,
            data       => ram0_rd_data,
            done       => ram0_rd_done,

            -- dram control signals
            dram_ready    => dram0_ready,
            dram_rd_en    => dram0_rd_en,
            dram_rd_addr  => dram0_rd_addr,
            dram_rd_data  => dram0_rd_data,
            dram_rd_valid => dram0_rd_valid,
            dram_rd_flush => dram0_rd_flush);


    U_DRAM0_WR : dram_wr_ram0
        port map (
                                        -- user dma control signals
            dram_clk   => clks(C_CLK_DRAM),
            user_clk   => clks(C_CLK_USER),
            rst        => rst_s,
            clear      => ram0_wr_clear,
            go         => ram0_wr_go,
            wr_en      => ram0_wr_valid,
            start_addr => ram0_wr_addr,
            size       => ram0_wr_size,
            data       => ram0_wr_data,
            done       => ram0_wr_done,
            ready      => ram0_wr_ready,

                                        -- dram control signals
            dram_ready      => dram0_ready,
            dram_wr_en      => dram0_wr_en,
            dram_wr_addr    => dram0_wr_addr,
            dram_wr_data    => dram0_wr_data,
            dram_wr_pending => dram0_wr_pending);


    U_DRAM1_RD : dram_rd_ram1
        port map (
                                        -- user dma control signals
            dram_clk   => clks(C_CLK_DRAM),
            user_clk   => clks(C_CLK_USER),
            rst        => rst_s,
            clear      => ram1_rd_clear,
            go         => ram1_rd_go,
            rd_en      => ram1_rd_rd_en,
            stall      => C_0,
            start_addr => ram1_rd_addr,
            size       => ram1_rd_size,
            valid      => ram1_rd_valid,
            data       => ram1_rd_data,
            done       => ram1_rd_done,

                                        -- dram control signals
            dram_ready    => dram1_ready,
            dram_rd_en    => dram1_rd_en,
            dram_rd_addr  => dram1_rd_addr,
            dram_rd_data  => dram1_rd_data,
            dram_rd_valid => dram1_rd_valid,
            dram_rd_flush => dram1_rd_flush);


    U_DRAM1_WR : dram_wr_ram1
        port map (
                                        -- user dma control signals
            dram_clk   => clks(C_CLK_DRAM),
            user_clk   => clks(C_CLK_USER),
            rst        => rst_s,
            clear      => ram1_wr_clear,
            go         => ram1_wr_go,
            wr_en      => ram1_wr_valid,
            start_addr => ram1_wr_addr,
            size       => ram1_wr_size,
            data       => ram1_wr_data,
            done       => ram1_wr_done,
            ready      => ram1_wr_ready,

                                        -- dram control signals
            dram_ready      => dram1_ready,
            dram_wr_en      => dram1_wr_en,
            dram_wr_addr    => dram1_wr_addr,
            dram_wr_data    => dram1_wr_data,
            dram_wr_pending => dram1_wr_pending);


    ----------------------------------------------------------------------
    -- Create the DRAM models
    -- Note that there are no DRAMs inside the FPGA. This code actually uses
    -- SRAMs, but does so in a way that mimics the latency and refresh of a
    -- DRAM.

    U_DRAM0 : entity work.dram_model
        generic map (
            num_words          => 2**C_DRAM0_ADDR_WIDTH,
            word_width         => C_DRAM0_DATA_WIDTH,
            addr_width         => C_DRAM0_ADDR_WIDTH,
            wr_only_when_ready => false)
        port map (
            clk        => clks(C_CLK_DRAM),
            rst        => rst_s,
            ready      => dram0_ready,
            wr_en      => dram0_wr_en,
            wr_addr    => dram0_wr_addr,
            wr_data    => dram0_wr_data,
            wr_pending => dram0_wr_pending,
            rd_en      => dram0_rd_en,
            rd_addr    => dram0_rd_addr,
            rd_data    => dram0_rd_data,
            rd_valid   => dram0_rd_valid,
            rd_flush   => dram0_rd_flush);

    U_DRAM1 : entity work.dram_model
        generic map (
            num_words  => 2**C_DRAM1_ADDR_WIDTH,
            word_width => C_DRAM1_DATA_WIDTH,
            addr_width => C_DRAM1_ADDR_WIDTH,
            rd_latency => 1)
        port map (
            clk        => clks(C_CLK_DRAM),
            rst        => rst_s,
            ready      => dram1_ready,
            wr_en      => dram1_wr_en,
            wr_addr    => dram1_wr_addr,
            wr_data    => dram1_wr_data,
            wr_pending => dram1_wr_pending,
            rd_en      => dram1_rd_en,
            rd_addr    => dram1_rd_addr,
            rd_data    => dram1_rd_data,
            rd_valid   => dram1_rd_valid,
            rd_flush   => dram1_rd_flush);

end default;
