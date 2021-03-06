-- Izhar Shaikh
--
-- File: smart_buffer.vhd
-- Description: Assembles the data from FIFO into "windows" and feeds the datapath
--
-- Entity has only one data_in of data_width size and output of size num_outputs*data_width

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity smart_buffer is
  generic (
    num_outputs  : positive := 128;
    data_width  : positive := 16 );
  port (
    clk         : in  std_logic;
    rst         : in  std_logic;
    rd_en       : in  std_logic;
    wr_en       : in  std_logic;
    dma_status  : in  std_logic;
    sb_done     : out std_logic;
    full        : out std_logic;
    empty       : out std_logic;
    data_in     : in std_logic_vector(data_width-1 downto 0);
    data_out    : out std_logic_vector(num_outputs*data_width-1 downto 0));
end smart_buffer;


architecture bhv_sbuff of smart_buffer is

  constant C_COUNT_REG_WIDTH : positive := 8;

  type reg_array is array(integer range<>) of std_logic_vector(data_width-1 downto 0);
  signal reg_in, reg_out : reg_array(0 to num_outputs-1);

  type STATE_TYPE is (S_WAIT_UNTIL_DMA_READY, S_PROCESS, S_DONE);
  signal state, next_state : STATE_TYPE;
  signal count, next_count : std_logic_vector(C_COUNT_REG_WIDTH-1 downto 0);

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

  -- Generate num_outputs registers and connect them together
  U_GENERATE_REG : for i in 0 to num_outputs-1 generate

    -- REG(0) to REG(num_outputs-1)
    U_REG : entity work.reg
      generic map( width => data_width )
      port map (
        clk     => clk,
        rst     => rst,
        en      => wr_en,
        input   => reg_in(i),
        output  => reg_out(i));

  end generate U_GENERATE_REG;

  U_CONNECT_REG : for i in 0 to num_outputs-2 generate

    -- Connect the registers
    -- Output[Reg(i)] -> Input[Reg(i+1)]; where (i != 0)
      reg_in(i+1) <= reg_out(i);

  end generate U_CONNECT_REG;

  -- Connect the data_in to LSR and data_out to all registers' outputs
  reg_in(0) <= data_in(data_width-1 downto 0);
  data_out <= vectorize(reg_out, num_outputs, data_width);

  -- process to control the write enable/disable logic, full/empty logic
  process(clk, rst)
  begin
      if (rst = '1') then
          state  <= S_WAIT_UNTIL_DMA_READY;
          count <= ( others =>'0' );
      elsif (clk = '1' and clk'event) then
          state  <= next_state;
          count  <= next_count;
      end if;
  end process;

  process(state, count, wr_en, rd_en, dma_status, data_in)
    variable count_reg : integer := 0;
  begin
      -- defaults
      next_state  <= state;
      next_count  <= count;

      full        <= '0';
      empty       <= '1';
      sb_done     <= '0';

      case state is
          when S_WAIT_UNTIL_DMA_READY =>

              -- set defaults for this state
              sb_done     <= '0';
              full        <= '0';
              empty       <= '1';

              if (dma_status = '0') then
                  next_state <= S_PROCESS;
              end if;

          when S_PROCESS =>

              -- get the current count
              count_reg  := to_integer(unsigned(count));

              -- if the buffer is full
              if (count_reg = num_outputs) then
                -- and if no read request
                if (rd_en = '0') then
                  full <= '1';      --indicate we're full to stop writing
                  empty <= '0';     --indicate the output data is valid
                else -- if there is read request
                  full <= '0';      -- indicate we're accepting writes
                  empty <= '0';     -- indicate we're sending valid data out
                  count_reg := count_reg - 1;   -- decrement buffer count to accept writes
                end if;
              end if;

              -- if buffer is empty
              if (count_reg < num_outputs) then
                if (wr_en = '1') then
                  full <= '0';    -- accept writes if requested
                  count_reg := count_reg + 1;  -- keep up the buffer count
                end if;
              end if;

              -- store the variable in next_count to be used in next cycle
              next_count <= std_logic_vector(to_unsigned(count_reg, C_COUNT_REG_WIDTH));

              -- check if the dma has finished reading
              if (wr_en = '0') and (dma_status = '1') then
                  next_state <= S_DONE;
              end if;

          when S_DONE =>

              sb_done <= '1';  -- update done
              if (dma_status = '0') then
                  next_state  <= S_WAIT_UNTIL_DMA_READY;
              end if;

          when others => null;

      end case;
  end process;

end bhv_sbuff;
