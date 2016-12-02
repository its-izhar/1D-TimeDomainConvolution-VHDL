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
    num_outputs  : positive := 3;
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
  signal done, next_done : std_logic;
  signal count, next_count : std_logic_vector(C_COUNT_REG_WIDTH-1 downto 0);

  -- controls the enable/disable of write to the registers
  --signal write_enable, write_enable_out : std_logic;

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
  -- LSR: Least Significant Register [Reg0]
  -- MSR: Most Significant Register
  reg_in(0) <= data_in(data_width-1 downto 0);
  data_out <= vectorize(reg_out, num_outputs, data_width);

  -- connect the write_enable_out to write_enable register
  --write_enable_out <= write_enable;

  -- process to control the write enable/disable logic, full/empty logic
  process(clk, rst)
  begin
      if (rst = '1') then
          state  <= S_WAIT_UNTIL_DMA_READY;
          done <= '0';
          count <= ( others =>'0' );
      elsif (clk = '1' and clk'event) then
          state  <= next_state;
          count  <= next_count;
          done <= next_done;
      end if;
  end process;

  process(state, count, done, wr_en, rd_en, dma_status, data_in)
    variable count_reg : integer := 0;
  begin
      -- defaults
      sb_done <= done;
      next_done <= done;
      next_state  <= state;
      next_count <= count;

      full        <= '0';
      empty       <= '1';
      sb_done     <= '0';

      case state is

          when S_WAIT_UNTIL_DMA_READY =>

              full        <= '0';
              empty       <= '1';
              sb_done     <= '0';
              --write_enable <= '0';

              if (dma_status = '0') then
                  next_state <= S_PROCESS;
              end if;

          when S_PROCESS =>

              -- get the current count
              count_reg  := to_integer(unsigned(count));

              -- write only if the buffer is not full; otherwise stop writing
              -- since read requests are disabled
              if (wr_en = '1') and (rd_en = '0') then
                  if (count_reg < num_outputs) then
                    count_reg := count_reg + 1;   -- update the count until full
                    full  <= '0';         -- keep writing
                    empty <= '1';         -- data_out isn't valid yet
                    --write_enable <= '1';  -- approve write requests
                  elsif (count_reg = num_outputs) then
                      full  <= '1';         -- buffer full; stop writing
                      empty <= '0';         -- buffer full; indicate valid output
                      --write_enable <= '0';   -- buffer full; stop writing
                  end if;

              -- allow write requests; but vacate the last space if buffer is full
              elsif (wr_en = '0') and (rd_en = '1') then
                  --write_enable <= '0';  -- disable write requests
                  full  <= '0';         -- allow writing
                  empty <= '1';         -- data_out isn't valid yet
                  -- updating the register count because the buffer is full
                  -- this will accommodate one more write request in next cycle
                  if (count_reg = num_outputs) then
                      count_reg := count_reg - 1;
                  end if;

              -- buffer read/write every cycle
              elsif (wr_en = '1') and (rd_en = '1') then
                  --write_enable <= '1';   -- write to buffer; every cycle
                  if (count_reg = num_outputs) then
                      full  <= '0';         -- do not stop writing; even when the buffer is full
                                            -- this is due to simultaneous read requests
                      empty <= '0';         -- buffer full; indicate valid output
                  else
                      full  <= '0';         -- keep writing
                      empty <= '1';         -- data_out isn't valid yet
                      count_reg := count_reg + 1;   -- update count_reg until full
                  end if;
              end if;

              -- store the variable in next_count to be used in next cycle
              next_count <= std_logic_vector(to_unsigned(count_reg, C_COUNT_REG_WIDTH));

              -- check if the dma has finished reading
              if (wr_en = '0') and (dma_status = '1') then
                  next_state <= S_DONE;
              end if;

          when S_DONE =>
              sb_done <= '1';
              next_done <= '1';  -- could potentially update sb_done also
                                   -- if we don't want to wait one cycle
              --write_enable <= '0';
              if (dma_status = '0') then
                  next_state  <= S_WAIT_UNTIL_DMA_READY;
              end if;

          when others => null;

      end case;
  end process;

end bhv_sbuff;
