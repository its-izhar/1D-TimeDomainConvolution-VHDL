-- Izhar Shaikh
--
-- File: smart_buffer.vhd
-- Description: Assembles the data from FIFO into "windows" and feeds the datapath
--
-- Entity has only one data_in of data_width size and output of size num_outputs*data_width

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.user_pkg.all;
use work.math_custom.all;

entity smart_buffer is
  generic (
    num_outputs  : positive : 128;
    data_width  : positive : 16 );
  port (
    clk       : in  std_logic;
    rst       : in  std_logic;
    full      : out std_logic;
    empty     : out std_logic;
    rd_en     : in  std_logic;
    wr_en     : in  std_logic;
    data_in   : out std_logic_vector(data_width-1 downto 0);
    data_out  : out std_logic_vector(num_outputs*data_width-1 downto 0));
end smart_buffer;


architecture bhv_sbuff of smart_buffer is

--  type reg_out_array is array(integer range<>) of std_logic_vector(data_width-1 downto 0);
--  signal reg_out : reg_out_array(0 to num_outputs-1);

type reg_array is array(integer range<>) of std_logic_vector(data_width-1 downto 0);
signal reg_in, reg_out : reg_array(0 to num_outputs-1);

  -- converts from reg_out_array to std_logic_vector
  function vectorize(input        : reg_out_array;
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

--  -- REG(first): Left Significant Register (Entry point for data_in)
--  U_LSR : entity work.reg
--    generic map( width => data_width )
--    port map (
--      clk     => clk,
--      rst     => rst,
--      en      => wr_en,
--      input   => data_in(data_width-1 downto 0),
--      output  => reg_out(0));
--
--  U_GENERATE_REG : for i in 1 to num_outputs-2 generate
--
--    -- REG(second) to REG(num_outputs-1)
--    -- output of Reg(i-1) -> input of Reg(i)
--    U_REG : entity work.reg
--      generic map( width => data_width )
--      port map (
--        clk     => clk,
--        rst     => rst,
--        en      => wr_en,
--        input   => reg_out(i-1),
--        output  => reg_out(i));
--
--  end generate U_GENERATE_REG;
--
--  -- REG(LAST) i.e. Reg(num_outputs-1)
--  U_MSR : entity work.reg
--    generic map( width => data_width )
--    port map (
--      clk     => clk,
--      rst     => rst,
--      en      => wr_en,
--      input   => reg_out(num_outputs-2),
--      output  => reg_out(num_outputs-1));
--
--  -- connect the array outputs to data_out
--  data_out <= vectorize(reg_out, num_outputs, data_width)

--------------------------------------------------------------------------------
---------------------Alternate Implementation-----------------------------------
--------------------------------------------------------------------------------

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

    -- Connect the registers
    -- Output[Reg(i)] -> Input[Reg(i+1)]; where (i != 0)
    if (i > 0) then
      reg_in(i) <= reg_out(i-1);
    end if;

  end generate U_GENERATE_REG;

  -- Connect the data_in to LSR and data_out to all registers' outputs
  -- LSR: Least Significant Register [Reg0]
  -- MSR: Most Significant Register
  reg_in(0) <= data_in(data_width-1 downto 0);
  data_out <= vectorize(reg_out, num_outputs, data_width)

end bhv_sbuff;
