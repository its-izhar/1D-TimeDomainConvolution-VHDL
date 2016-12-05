-- Copyright 1986-2015 Xilinx, Inc. All Rights Reserved.
-- --------------------------------------------------------------------------------
-- Tool Version: Vivado v.2015.2 (win64) Build 1266856 Fri Jun 26 16:35:25 MDT 2015
-- Date        : Thu Nov 12 15:26:33 2015
-- Host        : scully running 64-bit Service Pack 1  (build 7601)
-- Command     : write_vhdl -force -mode funcsim
--               c:/Users/gstitt/Desktop/rc_class/project_12/project_12.srcs/sources_1/ip/dram_rd_ram0_0/dram_rd_ram0_0_funcsim.vhdl
-- Design      : dram_rd_ram0_0
-- Purpose     : This VHDL netlist is a functional simulation representation of the design and should not be modified or
--               synthesized. This netlist cannot be used for SDF annotated simulation.
-- Device      : xc7z020clg484-1
-- --------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_DRAM_ADDR_GEN is
  port (
    state : out STD_LOGIC;
    dram_rd_addr : out STD_LOGIC_VECTOR ( 14 downto 0 );
    \gpf1.prog_full_i_reg\ : in STD_LOGIC;
    dram_clk : in STD_LOGIC;
    AR : in STD_LOGIC_VECTOR ( 0 to 0 );
    take_it_tg_ff_reg : in STD_LOGIC;
    Q : in STD_LOGIC_VECTOR ( 14 downto 0 )
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_DRAM_ADDR_GEN : entity is "DRAM_ADDR_GEN";
end dram_rd_ram0_0_DRAM_ADDR_GEN;

architecture STRUCTURE of dram_rd_ram0_0_DRAM_ADDR_GEN is
  signal \addr_current[0]_i_3_n_0\ : STD_LOGIC;
  signal \addr_current[0]_i_4_n_0\ : STD_LOGIC;
  signal \addr_current[0]_i_5_n_0\ : STD_LOGIC;
  signal \addr_current[0]_i_6_n_0\ : STD_LOGIC;
  signal \addr_current[0]_i_7_n_0\ : STD_LOGIC;
  signal \addr_current[12]_i_2_n_0\ : STD_LOGIC;
  signal \addr_current[12]_i_3_n_0\ : STD_LOGIC;
  signal \addr_current[12]_i_4_n_0\ : STD_LOGIC;
  signal \addr_current[4]_i_2_n_0\ : STD_LOGIC;
  signal \addr_current[4]_i_3_n_0\ : STD_LOGIC;
  signal \addr_current[4]_i_4_n_0\ : STD_LOGIC;
  signal \addr_current[4]_i_5_n_0\ : STD_LOGIC;
  signal \addr_current[8]_i_2_n_0\ : STD_LOGIC;
  signal \addr_current[8]_i_3_n_0\ : STD_LOGIC;
  signal \addr_current[8]_i_4_n_0\ : STD_LOGIC;
  signal \addr_current[8]_i_5_n_0\ : STD_LOGIC;
  signal addr_current_reg : STD_LOGIC_VECTOR ( 14 downto 0 );
  signal \addr_current_reg[0]_i_2_n_0\ : STD_LOGIC;
  signal \addr_current_reg[0]_i_2_n_1\ : STD_LOGIC;
  signal \addr_current_reg[0]_i_2_n_2\ : STD_LOGIC;
  signal \addr_current_reg[0]_i_2_n_3\ : STD_LOGIC;
  signal \addr_current_reg[0]_i_2_n_4\ : STD_LOGIC;
  signal \addr_current_reg[0]_i_2_n_5\ : STD_LOGIC;
  signal \addr_current_reg[0]_i_2_n_6\ : STD_LOGIC;
  signal \addr_current_reg[0]_i_2_n_7\ : STD_LOGIC;
  signal \addr_current_reg[12]_i_1_n_2\ : STD_LOGIC;
  signal \addr_current_reg[12]_i_1_n_3\ : STD_LOGIC;
  signal \addr_current_reg[12]_i_1_n_5\ : STD_LOGIC;
  signal \addr_current_reg[12]_i_1_n_6\ : STD_LOGIC;
  signal \addr_current_reg[12]_i_1_n_7\ : STD_LOGIC;
  signal \addr_current_reg[4]_i_1_n_0\ : STD_LOGIC;
  signal \addr_current_reg[4]_i_1_n_1\ : STD_LOGIC;
  signal \addr_current_reg[4]_i_1_n_2\ : STD_LOGIC;
  signal \addr_current_reg[4]_i_1_n_3\ : STD_LOGIC;
  signal \addr_current_reg[4]_i_1_n_4\ : STD_LOGIC;
  signal \addr_current_reg[4]_i_1_n_5\ : STD_LOGIC;
  signal \addr_current_reg[4]_i_1_n_6\ : STD_LOGIC;
  signal \addr_current_reg[4]_i_1_n_7\ : STD_LOGIC;
  signal \addr_current_reg[8]_i_1_n_0\ : STD_LOGIC;
  signal \addr_current_reg[8]_i_1_n_1\ : STD_LOGIC;
  signal \addr_current_reg[8]_i_1_n_2\ : STD_LOGIC;
  signal \addr_current_reg[8]_i_1_n_3\ : STD_LOGIC;
  signal \addr_current_reg[8]_i_1_n_4\ : STD_LOGIC;
  signal \addr_current_reg[8]_i_1_n_5\ : STD_LOGIC;
  signal \addr_current_reg[8]_i_1_n_6\ : STD_LOGIC;
  signal \addr_current_reg[8]_i_1_n_7\ : STD_LOGIC;
  signal \^state\ : STD_LOGIC;
  signal \NLW_addr_current_reg[12]_i_1_CO_UNCONNECTED\ : STD_LOGIC_VECTOR ( 3 downto 2 );
  signal \NLW_addr_current_reg[12]_i_1_O_UNCONNECTED\ : STD_LOGIC_VECTOR ( 3 to 3 );
  attribute SOFT_HLUTNM : string;
  attribute SOFT_HLUTNM of \dram_rd_addr[0]_INST_0\ : label is "soft_lutpair10";
  attribute SOFT_HLUTNM of \dram_rd_addr[10]_INST_0\ : label is "soft_lutpair15";
  attribute SOFT_HLUTNM of \dram_rd_addr[11]_INST_0\ : label is "soft_lutpair15";
  attribute SOFT_HLUTNM of \dram_rd_addr[12]_INST_0\ : label is "soft_lutpair16";
  attribute SOFT_HLUTNM of \dram_rd_addr[13]_INST_0\ : label is "soft_lutpair16";
  attribute SOFT_HLUTNM of \dram_rd_addr[1]_INST_0\ : label is "soft_lutpair10";
  attribute SOFT_HLUTNM of \dram_rd_addr[2]_INST_0\ : label is "soft_lutpair11";
  attribute SOFT_HLUTNM of \dram_rd_addr[3]_INST_0\ : label is "soft_lutpair11";
  attribute SOFT_HLUTNM of \dram_rd_addr[4]_INST_0\ : label is "soft_lutpair12";
  attribute SOFT_HLUTNM of \dram_rd_addr[5]_INST_0\ : label is "soft_lutpair12";
  attribute SOFT_HLUTNM of \dram_rd_addr[6]_INST_0\ : label is "soft_lutpair13";
  attribute SOFT_HLUTNM of \dram_rd_addr[7]_INST_0\ : label is "soft_lutpair13";
  attribute SOFT_HLUTNM of \dram_rd_addr[8]_INST_0\ : label is "soft_lutpair14";
  attribute SOFT_HLUTNM of \dram_rd_addr[9]_INST_0\ : label is "soft_lutpair14";
begin
  state <= \^state\;
\addr_current[0]_i_3\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"B8"
    )
        port map (
      I0 => addr_current_reg(0),
      I1 => \^state\,
      I2 => Q(0),
      O => \addr_current[0]_i_3_n_0\
    );
\addr_current[0]_i_4\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"B8"
    )
        port map (
      I0 => addr_current_reg(3),
      I1 => \^state\,
      I2 => Q(3),
      O => \addr_current[0]_i_4_n_0\
    );
\addr_current[0]_i_5\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"B8"
    )
        port map (
      I0 => addr_current_reg(2),
      I1 => \^state\,
      I2 => Q(2),
      O => \addr_current[0]_i_5_n_0\
    );
\addr_current[0]_i_6\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"B8"
    )
        port map (
      I0 => addr_current_reg(1),
      I1 => \^state\,
      I2 => Q(1),
      O => \addr_current[0]_i_6_n_0\
    );
\addr_current[0]_i_7\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"3A"
    )
        port map (
      I0 => Q(0),
      I1 => addr_current_reg(0),
      I2 => \^state\,
      O => \addr_current[0]_i_7_n_0\
    );
\addr_current[12]_i_2\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"B8"
    )
        port map (
      I0 => addr_current_reg(14),
      I1 => \^state\,
      I2 => Q(14),
      O => \addr_current[12]_i_2_n_0\
    );
\addr_current[12]_i_3\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"B8"
    )
        port map (
      I0 => addr_current_reg(13),
      I1 => \^state\,
      I2 => Q(13),
      O => \addr_current[12]_i_3_n_0\
    );
\addr_current[12]_i_4\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"B8"
    )
        port map (
      I0 => addr_current_reg(12),
      I1 => \^state\,
      I2 => Q(12),
      O => \addr_current[12]_i_4_n_0\
    );
\addr_current[4]_i_2\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"B8"
    )
        port map (
      I0 => addr_current_reg(7),
      I1 => \^state\,
      I2 => Q(7),
      O => \addr_current[4]_i_2_n_0\
    );
\addr_current[4]_i_3\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"B8"
    )
        port map (
      I0 => addr_current_reg(6),
      I1 => \^state\,
      I2 => Q(6),
      O => \addr_current[4]_i_3_n_0\
    );
\addr_current[4]_i_4\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"B8"
    )
        port map (
      I0 => addr_current_reg(5),
      I1 => \^state\,
      I2 => Q(5),
      O => \addr_current[4]_i_4_n_0\
    );
\addr_current[4]_i_5\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"B8"
    )
        port map (
      I0 => addr_current_reg(4),
      I1 => \^state\,
      I2 => Q(4),
      O => \addr_current[4]_i_5_n_0\
    );
\addr_current[8]_i_2\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"B8"
    )
        port map (
      I0 => addr_current_reg(11),
      I1 => \^state\,
      I2 => Q(11),
      O => \addr_current[8]_i_2_n_0\
    );
\addr_current[8]_i_3\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"B8"
    )
        port map (
      I0 => addr_current_reg(10),
      I1 => \^state\,
      I2 => Q(10),
      O => \addr_current[8]_i_3_n_0\
    );
\addr_current[8]_i_4\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"B8"
    )
        port map (
      I0 => addr_current_reg(9),
      I1 => \^state\,
      I2 => Q(9),
      O => \addr_current[8]_i_4_n_0\
    );
\addr_current[8]_i_5\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"B8"
    )
        port map (
      I0 => addr_current_reg(8),
      I1 => \^state\,
      I2 => Q(8),
      O => \addr_current[8]_i_5_n_0\
    );
\addr_current_reg[0]\: unisim.vcomponents.FDCE
     port map (
      C => dram_clk,
      CE => \gpf1.prog_full_i_reg\,
      CLR => AR(0),
      D => \addr_current_reg[0]_i_2_n_7\,
      Q => addr_current_reg(0)
    );
\addr_current_reg[0]_i_2\: unisim.vcomponents.CARRY4
     port map (
      CI => '0',
      CO(3) => \addr_current_reg[0]_i_2_n_0\,
      CO(2) => \addr_current_reg[0]_i_2_n_1\,
      CO(1) => \addr_current_reg[0]_i_2_n_2\,
      CO(0) => \addr_current_reg[0]_i_2_n_3\,
      CYINIT => '0',
      DI(3) => '0',
      DI(2) => '0',
      DI(1) => '0',
      DI(0) => \addr_current[0]_i_3_n_0\,
      O(3) => \addr_current_reg[0]_i_2_n_4\,
      O(2) => \addr_current_reg[0]_i_2_n_5\,
      O(1) => \addr_current_reg[0]_i_2_n_6\,
      O(0) => \addr_current_reg[0]_i_2_n_7\,
      S(3) => \addr_current[0]_i_4_n_0\,
      S(2) => \addr_current[0]_i_5_n_0\,
      S(1) => \addr_current[0]_i_6_n_0\,
      S(0) => \addr_current[0]_i_7_n_0\
    );
\addr_current_reg[10]\: unisim.vcomponents.FDCE
     port map (
      C => dram_clk,
      CE => \gpf1.prog_full_i_reg\,
      CLR => AR(0),
      D => \addr_current_reg[8]_i_1_n_5\,
      Q => addr_current_reg(10)
    );
\addr_current_reg[11]\: unisim.vcomponents.FDCE
     port map (
      C => dram_clk,
      CE => \gpf1.prog_full_i_reg\,
      CLR => AR(0),
      D => \addr_current_reg[8]_i_1_n_4\,
      Q => addr_current_reg(11)
    );
\addr_current_reg[12]\: unisim.vcomponents.FDCE
     port map (
      C => dram_clk,
      CE => \gpf1.prog_full_i_reg\,
      CLR => AR(0),
      D => \addr_current_reg[12]_i_1_n_7\,
      Q => addr_current_reg(12)
    );
\addr_current_reg[12]_i_1\: unisim.vcomponents.CARRY4
     port map (
      CI => \addr_current_reg[8]_i_1_n_0\,
      CO(3 downto 2) => \NLW_addr_current_reg[12]_i_1_CO_UNCONNECTED\(3 downto 2),
      CO(1) => \addr_current_reg[12]_i_1_n_2\,
      CO(0) => \addr_current_reg[12]_i_1_n_3\,
      CYINIT => '0',
      DI(3) => '0',
      DI(2) => '0',
      DI(1) => '0',
      DI(0) => '0',
      O(3) => \NLW_addr_current_reg[12]_i_1_O_UNCONNECTED\(3),
      O(2) => \addr_current_reg[12]_i_1_n_5\,
      O(1) => \addr_current_reg[12]_i_1_n_6\,
      O(0) => \addr_current_reg[12]_i_1_n_7\,
      S(3) => '0',
      S(2) => \addr_current[12]_i_2_n_0\,
      S(1) => \addr_current[12]_i_3_n_0\,
      S(0) => \addr_current[12]_i_4_n_0\
    );
\addr_current_reg[13]\: unisim.vcomponents.FDCE
     port map (
      C => dram_clk,
      CE => \gpf1.prog_full_i_reg\,
      CLR => AR(0),
      D => \addr_current_reg[12]_i_1_n_6\,
      Q => addr_current_reg(13)
    );
\addr_current_reg[14]\: unisim.vcomponents.FDCE
     port map (
      C => dram_clk,
      CE => \gpf1.prog_full_i_reg\,
      CLR => AR(0),
      D => \addr_current_reg[12]_i_1_n_5\,
      Q => addr_current_reg(14)
    );
\addr_current_reg[1]\: unisim.vcomponents.FDCE
     port map (
      C => dram_clk,
      CE => \gpf1.prog_full_i_reg\,
      CLR => AR(0),
      D => \addr_current_reg[0]_i_2_n_6\,
      Q => addr_current_reg(1)
    );
\addr_current_reg[2]\: unisim.vcomponents.FDCE
     port map (
      C => dram_clk,
      CE => \gpf1.prog_full_i_reg\,
      CLR => AR(0),
      D => \addr_current_reg[0]_i_2_n_5\,
      Q => addr_current_reg(2)
    );
\addr_current_reg[3]\: unisim.vcomponents.FDCE
     port map (
      C => dram_clk,
      CE => \gpf1.prog_full_i_reg\,
      CLR => AR(0),
      D => \addr_current_reg[0]_i_2_n_4\,
      Q => addr_current_reg(3)
    );
\addr_current_reg[4]\: unisim.vcomponents.FDCE
     port map (
      C => dram_clk,
      CE => \gpf1.prog_full_i_reg\,
      CLR => AR(0),
      D => \addr_current_reg[4]_i_1_n_7\,
      Q => addr_current_reg(4)
    );
\addr_current_reg[4]_i_1\: unisim.vcomponents.CARRY4
     port map (
      CI => \addr_current_reg[0]_i_2_n_0\,
      CO(3) => \addr_current_reg[4]_i_1_n_0\,
      CO(2) => \addr_current_reg[4]_i_1_n_1\,
      CO(1) => \addr_current_reg[4]_i_1_n_2\,
      CO(0) => \addr_current_reg[4]_i_1_n_3\,
      CYINIT => '0',
      DI(3) => '0',
      DI(2) => '0',
      DI(1) => '0',
      DI(0) => '0',
      O(3) => \addr_current_reg[4]_i_1_n_4\,
      O(2) => \addr_current_reg[4]_i_1_n_5\,
      O(1) => \addr_current_reg[4]_i_1_n_6\,
      O(0) => \addr_current_reg[4]_i_1_n_7\,
      S(3) => \addr_current[4]_i_2_n_0\,
      S(2) => \addr_current[4]_i_3_n_0\,
      S(1) => \addr_current[4]_i_4_n_0\,
      S(0) => \addr_current[4]_i_5_n_0\
    );
\addr_current_reg[5]\: unisim.vcomponents.FDCE
     port map (
      C => dram_clk,
      CE => \gpf1.prog_full_i_reg\,
      CLR => AR(0),
      D => \addr_current_reg[4]_i_1_n_6\,
      Q => addr_current_reg(5)
    );
\addr_current_reg[6]\: unisim.vcomponents.FDCE
     port map (
      C => dram_clk,
      CE => \gpf1.prog_full_i_reg\,
      CLR => AR(0),
      D => \addr_current_reg[4]_i_1_n_5\,
      Q => addr_current_reg(6)
    );
\addr_current_reg[7]\: unisim.vcomponents.FDCE
     port map (
      C => dram_clk,
      CE => \gpf1.prog_full_i_reg\,
      CLR => AR(0),
      D => \addr_current_reg[4]_i_1_n_4\,
      Q => addr_current_reg(7)
    );
\addr_current_reg[8]\: unisim.vcomponents.FDCE
     port map (
      C => dram_clk,
      CE => \gpf1.prog_full_i_reg\,
      CLR => AR(0),
      D => \addr_current_reg[8]_i_1_n_7\,
      Q => addr_current_reg(8)
    );
\addr_current_reg[8]_i_1\: unisim.vcomponents.CARRY4
     port map (
      CI => \addr_current_reg[4]_i_1_n_0\,
      CO(3) => \addr_current_reg[8]_i_1_n_0\,
      CO(2) => \addr_current_reg[8]_i_1_n_1\,
      CO(1) => \addr_current_reg[8]_i_1_n_2\,
      CO(0) => \addr_current_reg[8]_i_1_n_3\,
      CYINIT => '0',
      DI(3) => '0',
      DI(2) => '0',
      DI(1) => '0',
      DI(0) => '0',
      O(3) => \addr_current_reg[8]_i_1_n_4\,
      O(2) => \addr_current_reg[8]_i_1_n_5\,
      O(1) => \addr_current_reg[8]_i_1_n_6\,
      O(0) => \addr_current_reg[8]_i_1_n_7\,
      S(3) => \addr_current[8]_i_2_n_0\,
      S(2) => \addr_current[8]_i_3_n_0\,
      S(1) => \addr_current[8]_i_4_n_0\,
      S(0) => \addr_current[8]_i_5_n_0\
    );
\addr_current_reg[9]\: unisim.vcomponents.FDCE
     port map (
      C => dram_clk,
      CE => \gpf1.prog_full_i_reg\,
      CLR => AR(0),
      D => \addr_current_reg[8]_i_1_n_6\,
      Q => addr_current_reg(9)
    );
\dram_rd_addr[0]_INST_0\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"8"
    )
        port map (
      I0 => \^state\,
      I1 => addr_current_reg(0),
      O => dram_rd_addr(0)
    );
\dram_rd_addr[10]_INST_0\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"8"
    )
        port map (
      I0 => \^state\,
      I1 => addr_current_reg(10),
      O => dram_rd_addr(10)
    );
\dram_rd_addr[11]_INST_0\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"8"
    )
        port map (
      I0 => \^state\,
      I1 => addr_current_reg(11),
      O => dram_rd_addr(11)
    );
\dram_rd_addr[12]_INST_0\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"8"
    )
        port map (
      I0 => \^state\,
      I1 => addr_current_reg(12),
      O => dram_rd_addr(12)
    );
\dram_rd_addr[13]_INST_0\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"8"
    )
        port map (
      I0 => \^state\,
      I1 => addr_current_reg(13),
      O => dram_rd_addr(13)
    );
\dram_rd_addr[14]_INST_0\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"8"
    )
        port map (
      I0 => \^state\,
      I1 => addr_current_reg(14),
      O => dram_rd_addr(14)
    );
\dram_rd_addr[1]_INST_0\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"8"
    )
        port map (
      I0 => \^state\,
      I1 => addr_current_reg(1),
      O => dram_rd_addr(1)
    );
\dram_rd_addr[2]_INST_0\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"8"
    )
        port map (
      I0 => \^state\,
      I1 => addr_current_reg(2),
      O => dram_rd_addr(2)
    );
\dram_rd_addr[3]_INST_0\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"8"
    )
        port map (
      I0 => \^state\,
      I1 => addr_current_reg(3),
      O => dram_rd_addr(3)
    );
\dram_rd_addr[4]_INST_0\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"8"
    )
        port map (
      I0 => \^state\,
      I1 => addr_current_reg(4),
      O => dram_rd_addr(4)
    );
\dram_rd_addr[5]_INST_0\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"8"
    )
        port map (
      I0 => \^state\,
      I1 => addr_current_reg(5),
      O => dram_rd_addr(5)
    );
\dram_rd_addr[6]_INST_0\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"8"
    )
        port map (
      I0 => \^state\,
      I1 => addr_current_reg(6),
      O => dram_rd_addr(6)
    );
\dram_rd_addr[7]_INST_0\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"8"
    )
        port map (
      I0 => \^state\,
      I1 => addr_current_reg(7),
      O => dram_rd_addr(7)
    );
\dram_rd_addr[8]_INST_0\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"8"
    )
        port map (
      I0 => \^state\,
      I1 => addr_current_reg(8),
      O => dram_rd_addr(8)
    );
\dram_rd_addr[9]_INST_0\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"8"
    )
        port map (
      I0 => \^state\,
      I1 => addr_current_reg(9),
      O => dram_rd_addr(9)
    );
state_reg: unisim.vcomponents.FDCE
     port map (
      C => dram_clk,
      CE => '1',
      CLR => AR(0),
      D => take_it_tg_ff_reg,
      Q => \^state\
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_blk_mem_gen_prim_wrapper is
  port (
    D : out STD_LOGIC_VECTOR ( 31 downto 0 );
    rd_clk : in STD_LOGIC;
    wr_clk : in STD_LOGIC;
    tmp_ram_rd_en : in STD_LOGIC;
    E : in STD_LOGIC_VECTOR ( 0 to 0 );
    Q : in STD_LOGIC_VECTOR ( 0 to 0 );
    \gc0.count_d1_reg[5]\ : in STD_LOGIC_VECTOR ( 5 downto 0 );
    \gic0.gc0.count_d2_reg[5]\ : in STD_LOGIC_VECTOR ( 5 downto 0 );
    din : in STD_LOGIC_VECTOR ( 31 downto 0 )
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_blk_mem_gen_prim_wrapper : entity is "blk_mem_gen_prim_wrapper";
end dram_rd_ram0_0_blk_mem_gen_prim_wrapper;

architecture STRUCTURE of dram_rd_ram0_0_blk_mem_gen_prim_wrapper is
  signal \DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram_n_32\ : STD_LOGIC;
  signal \DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram_n_33\ : STD_LOGIC;
  signal \DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram_n_34\ : STD_LOGIC;
  signal \DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram_n_35\ : STD_LOGIC;
  attribute box_type : string;
  attribute box_type of \DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram\ : label is "PRIMITIVE";
begin
\DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram\: unisim.vcomponents.RAMB18E1
    generic map(
      DOA_REG => 0,
      DOB_REG => 0,
      INITP_00 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INITP_01 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INITP_02 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INITP_03 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INITP_04 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INITP_05 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INITP_06 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INITP_07 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_00 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_01 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_02 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_03 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_04 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_05 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_06 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_07 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_08 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_09 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_0A => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_0B => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_0C => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_0D => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_0E => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_0F => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_10 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_11 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_12 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_13 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_14 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_15 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_16 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_17 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_18 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_19 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_1A => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_1B => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_1C => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_1D => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_1E => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_1F => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_20 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_21 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_22 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_23 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_24 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_25 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_26 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_27 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_28 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_29 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_2A => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_2B => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_2C => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_2D => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_2E => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_2F => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_30 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_31 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_32 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_33 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_34 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_35 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_36 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_37 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_38 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_39 => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_3A => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_3B => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_3C => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_3D => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_3E => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_3F => X"0000000000000000000000000000000000000000000000000000000000000000",
      INIT_A => X"00000",
      INIT_B => X"00000",
      INIT_FILE => "NONE",
      IS_CLKARDCLK_INVERTED => '0',
      IS_CLKBWRCLK_INVERTED => '0',
      IS_ENARDEN_INVERTED => '0',
      IS_ENBWREN_INVERTED => '0',
      IS_RSTRAMARSTRAM_INVERTED => '0',
      IS_RSTRAMB_INVERTED => '0',
      IS_RSTREGARSTREG_INVERTED => '0',
      IS_RSTREGB_INVERTED => '0',
      RAM_MODE => "SDP",
      RDADDR_COLLISION_HWCONFIG => "DELAYED_WRITE",
      READ_WIDTH_A => 36,
      READ_WIDTH_B => 0,
      RSTREG_PRIORITY_A => "REGCE",
      RSTREG_PRIORITY_B => "REGCE",
      SIM_COLLISION_CHECK => "ALL",
      SIM_DEVICE => "7SERIES",
      SRVAL_A => X"00000",
      SRVAL_B => X"00000",
      WRITE_MODE_A => "WRITE_FIRST",
      WRITE_MODE_B => "WRITE_FIRST",
      WRITE_WIDTH_A => 0,
      WRITE_WIDTH_B => 36
    )
        port map (
      ADDRARDADDR(13) => '0',
      ADDRARDADDR(12) => '0',
      ADDRARDADDR(11) => '0',
      ADDRARDADDR(10 downto 5) => \gc0.count_d1_reg[5]\(5 downto 0),
      ADDRARDADDR(4) => '0',
      ADDRARDADDR(3) => '0',
      ADDRARDADDR(2) => '0',
      ADDRARDADDR(1) => '0',
      ADDRARDADDR(0) => '0',
      ADDRBWRADDR(13) => '0',
      ADDRBWRADDR(12) => '0',
      ADDRBWRADDR(11) => '0',
      ADDRBWRADDR(10 downto 5) => \gic0.gc0.count_d2_reg[5]\(5 downto 0),
      ADDRBWRADDR(4) => '0',
      ADDRBWRADDR(3) => '0',
      ADDRBWRADDR(2) => '0',
      ADDRBWRADDR(1) => '0',
      ADDRBWRADDR(0) => '0',
      CLKARDCLK => rd_clk,
      CLKBWRCLK => wr_clk,
      DIADI(15 downto 0) => din(15 downto 0),
      DIBDI(15 downto 0) => din(31 downto 16),
      DIPADIP(1) => '0',
      DIPADIP(0) => '0',
      DIPBDIP(1) => '0',
      DIPBDIP(0) => '0',
      DOADO(15 downto 0) => D(15 downto 0),
      DOBDO(15 downto 0) => D(31 downto 16),
      DOPADOP(1) => \DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram_n_32\,
      DOPADOP(0) => \DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram_n_33\,
      DOPBDOP(1) => \DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram_n_34\,
      DOPBDOP(0) => \DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram_n_35\,
      ENARDEN => tmp_ram_rd_en,
      ENBWREN => E(0),
      REGCEAREGCE => '0',
      REGCEB => '0',
      RSTRAMARSTRAM => Q(0),
      RSTRAMB => Q(0),
      RSTREGARSTREG => '0',
      RSTREGB => '0',
      WEA(1) => '0',
      WEA(0) => '0',
      WEBWE(3) => E(0),
      WEBWE(2) => E(0),
      WEBWE(1) => E(0),
      WEBWE(0) => E(0)
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_done_counter is
  port (
    CO : out STD_LOGIC_VECTOR ( 0 to 0 );
    \count_reg[0]_0\ : in STD_LOGIC;
    user_clk : in STD_LOGIC;
    rst : in STD_LOGIC;
    clear : in STD_LOGIC;
    Q : in STD_LOGIC_VECTOR ( 16 downto 0 )
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_done_counter : entity is "done_counter";
end dram_rd_ram0_0_done_counter;

architecture STRUCTURE of dram_rd_ram0_0_done_counter is
  signal \count[0]_i_3_n_0\ : STD_LOGIC;
  signal \count[0]_i_4_n_0\ : STD_LOGIC;
  signal \count[0]_i_5_n_0\ : STD_LOGIC;
  signal \count[0]_i_6_n_0\ : STD_LOGIC;
  signal \count[0]_i_7_n_0\ : STD_LOGIC;
  signal \count[12]_i_2_n_0\ : STD_LOGIC;
  signal \count[12]_i_3_n_0\ : STD_LOGIC;
  signal \count[12]_i_4_n_0\ : STD_LOGIC;
  signal \count[12]_i_5_n_0\ : STD_LOGIC;
  signal \count[16]_i_2_n_0\ : STD_LOGIC;
  signal \count[4]_i_2_n_0\ : STD_LOGIC;
  signal \count[4]_i_3_n_0\ : STD_LOGIC;
  signal \count[4]_i_4_n_0\ : STD_LOGIC;
  signal \count[4]_i_5_n_0\ : STD_LOGIC;
  signal \count[8]_i_2_n_0\ : STD_LOGIC;
  signal \count[8]_i_3_n_0\ : STD_LOGIC;
  signal \count[8]_i_4_n_0\ : STD_LOGIC;
  signal \count[8]_i_5_n_0\ : STD_LOGIC;
  signal count_reg : STD_LOGIC_VECTOR ( 16 downto 0 );
  signal \count_reg[0]_i_2_n_0\ : STD_LOGIC;
  signal \count_reg[0]_i_2_n_1\ : STD_LOGIC;
  signal \count_reg[0]_i_2_n_2\ : STD_LOGIC;
  signal \count_reg[0]_i_2_n_3\ : STD_LOGIC;
  signal \count_reg[0]_i_2_n_4\ : STD_LOGIC;
  signal \count_reg[0]_i_2_n_5\ : STD_LOGIC;
  signal \count_reg[0]_i_2_n_6\ : STD_LOGIC;
  signal \count_reg[0]_i_2_n_7\ : STD_LOGIC;
  signal \count_reg[12]_i_1_n_0\ : STD_LOGIC;
  signal \count_reg[12]_i_1_n_1\ : STD_LOGIC;
  signal \count_reg[12]_i_1_n_2\ : STD_LOGIC;
  signal \count_reg[12]_i_1_n_3\ : STD_LOGIC;
  signal \count_reg[12]_i_1_n_4\ : STD_LOGIC;
  signal \count_reg[12]_i_1_n_5\ : STD_LOGIC;
  signal \count_reg[12]_i_1_n_6\ : STD_LOGIC;
  signal \count_reg[12]_i_1_n_7\ : STD_LOGIC;
  signal \count_reg[16]_i_1_n_7\ : STD_LOGIC;
  signal \count_reg[4]_i_1_n_0\ : STD_LOGIC;
  signal \count_reg[4]_i_1_n_1\ : STD_LOGIC;
  signal \count_reg[4]_i_1_n_2\ : STD_LOGIC;
  signal \count_reg[4]_i_1_n_3\ : STD_LOGIC;
  signal \count_reg[4]_i_1_n_4\ : STD_LOGIC;
  signal \count_reg[4]_i_1_n_5\ : STD_LOGIC;
  signal \count_reg[4]_i_1_n_6\ : STD_LOGIC;
  signal \count_reg[4]_i_1_n_7\ : STD_LOGIC;
  signal \count_reg[8]_i_1_n_0\ : STD_LOGIC;
  signal \count_reg[8]_i_1_n_1\ : STD_LOGIC;
  signal \count_reg[8]_i_1_n_2\ : STD_LOGIC;
  signal \count_reg[8]_i_1_n_3\ : STD_LOGIC;
  signal \count_reg[8]_i_1_n_4\ : STD_LOGIC;
  signal \count_reg[8]_i_1_n_5\ : STD_LOGIC;
  signal \count_reg[8]_i_1_n_6\ : STD_LOGIC;
  signal \count_reg[8]_i_1_n_7\ : STD_LOGIC;
  signal valid_INST_0_i_10_n_0 : STD_LOGIC;
  signal valid_INST_0_i_11_n_0 : STD_LOGIC;
  signal valid_INST_0_i_12_n_0 : STD_LOGIC;
  signal valid_INST_0_i_13_n_0 : STD_LOGIC;
  signal valid_INST_0_i_14_n_0 : STD_LOGIC;
  signal valid_INST_0_i_15_n_0 : STD_LOGIC;
  signal valid_INST_0_i_16_n_0 : STD_LOGIC;
  signal valid_INST_0_i_17_n_0 : STD_LOGIC;
  signal valid_INST_0_i_18_n_0 : STD_LOGIC;
  signal valid_INST_0_i_19_n_0 : STD_LOGIC;
  signal valid_INST_0_i_20_n_0 : STD_LOGIC;
  signal valid_INST_0_i_21_n_0 : STD_LOGIC;
  signal valid_INST_0_i_2_n_0 : STD_LOGIC;
  signal valid_INST_0_i_2_n_1 : STD_LOGIC;
  signal valid_INST_0_i_2_n_2 : STD_LOGIC;
  signal valid_INST_0_i_2_n_3 : STD_LOGIC;
  signal valid_INST_0_i_3_n_0 : STD_LOGIC;
  signal valid_INST_0_i_4_n_0 : STD_LOGIC;
  signal valid_INST_0_i_5_n_0 : STD_LOGIC;
  signal valid_INST_0_i_5_n_1 : STD_LOGIC;
  signal valid_INST_0_i_5_n_2 : STD_LOGIC;
  signal valid_INST_0_i_5_n_3 : STD_LOGIC;
  signal valid_INST_0_i_6_n_0 : STD_LOGIC;
  signal valid_INST_0_i_7_n_0 : STD_LOGIC;
  signal valid_INST_0_i_8_n_0 : STD_LOGIC;
  signal valid_INST_0_i_9_n_0 : STD_LOGIC;
  signal \NLW_count_reg[16]_i_1_CO_UNCONNECTED\ : STD_LOGIC_VECTOR ( 3 downto 0 );
  signal \NLW_count_reg[16]_i_1_O_UNCONNECTED\ : STD_LOGIC_VECTOR ( 3 downto 1 );
  signal NLW_valid_INST_0_i_1_CO_UNCONNECTED : STD_LOGIC_VECTOR ( 3 downto 1 );
  signal NLW_valid_INST_0_i_1_O_UNCONNECTED : STD_LOGIC_VECTOR ( 3 downto 0 );
  signal NLW_valid_INST_0_i_2_O_UNCONNECTED : STD_LOGIC_VECTOR ( 3 downto 0 );
  signal NLW_valid_INST_0_i_5_O_UNCONNECTED : STD_LOGIC_VECTOR ( 3 downto 0 );
begin
\count[0]_i_3\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"2"
    )
        port map (
      I0 => count_reg(0),
      I1 => clear,
      O => \count[0]_i_3_n_0\
    );
\count[0]_i_4\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"2"
    )
        port map (
      I0 => count_reg(3),
      I1 => clear,
      O => \count[0]_i_4_n_0\
    );
\count[0]_i_5\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"2"
    )
        port map (
      I0 => count_reg(2),
      I1 => clear,
      O => \count[0]_i_5_n_0\
    );
\count[0]_i_6\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"2"
    )
        port map (
      I0 => count_reg(1),
      I1 => clear,
      O => \count[0]_i_6_n_0\
    );
\count[0]_i_7\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"1"
    )
        port map (
      I0 => count_reg(0),
      I1 => clear,
      O => \count[0]_i_7_n_0\
    );
\count[12]_i_2\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"2"
    )
        port map (
      I0 => count_reg(15),
      I1 => clear,
      O => \count[12]_i_2_n_0\
    );
\count[12]_i_3\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"2"
    )
        port map (
      I0 => count_reg(14),
      I1 => clear,
      O => \count[12]_i_3_n_0\
    );
\count[12]_i_4\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"2"
    )
        port map (
      I0 => count_reg(13),
      I1 => clear,
      O => \count[12]_i_4_n_0\
    );
\count[12]_i_5\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"2"
    )
        port map (
      I0 => count_reg(12),
      I1 => clear,
      O => \count[12]_i_5_n_0\
    );
\count[16]_i_2\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"2"
    )
        port map (
      I0 => count_reg(16),
      I1 => clear,
      O => \count[16]_i_2_n_0\
    );
\count[4]_i_2\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"2"
    )
        port map (
      I0 => count_reg(7),
      I1 => clear,
      O => \count[4]_i_2_n_0\
    );
\count[4]_i_3\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"2"
    )
        port map (
      I0 => count_reg(6),
      I1 => clear,
      O => \count[4]_i_3_n_0\
    );
\count[4]_i_4\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"2"
    )
        port map (
      I0 => count_reg(5),
      I1 => clear,
      O => \count[4]_i_4_n_0\
    );
\count[4]_i_5\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"2"
    )
        port map (
      I0 => count_reg(4),
      I1 => clear,
      O => \count[4]_i_5_n_0\
    );
\count[8]_i_2\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"2"
    )
        port map (
      I0 => count_reg(11),
      I1 => clear,
      O => \count[8]_i_2_n_0\
    );
\count[8]_i_3\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"2"
    )
        port map (
      I0 => count_reg(10),
      I1 => clear,
      O => \count[8]_i_3_n_0\
    );
\count[8]_i_4\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"2"
    )
        port map (
      I0 => count_reg(9),
      I1 => clear,
      O => \count[8]_i_4_n_0\
    );
\count[8]_i_5\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"2"
    )
        port map (
      I0 => count_reg(8),
      I1 => clear,
      O => \count[8]_i_5_n_0\
    );
\count_reg[0]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \count_reg[0]_0\,
      CLR => rst,
      D => \count_reg[0]_i_2_n_7\,
      Q => count_reg(0)
    );
\count_reg[0]_i_2\: unisim.vcomponents.CARRY4
     port map (
      CI => '0',
      CO(3) => \count_reg[0]_i_2_n_0\,
      CO(2) => \count_reg[0]_i_2_n_1\,
      CO(1) => \count_reg[0]_i_2_n_2\,
      CO(0) => \count_reg[0]_i_2_n_3\,
      CYINIT => '0',
      DI(3) => '0',
      DI(2) => '0',
      DI(1) => '0',
      DI(0) => \count[0]_i_3_n_0\,
      O(3) => \count_reg[0]_i_2_n_4\,
      O(2) => \count_reg[0]_i_2_n_5\,
      O(1) => \count_reg[0]_i_2_n_6\,
      O(0) => \count_reg[0]_i_2_n_7\,
      S(3) => \count[0]_i_4_n_0\,
      S(2) => \count[0]_i_5_n_0\,
      S(1) => \count[0]_i_6_n_0\,
      S(0) => \count[0]_i_7_n_0\
    );
\count_reg[10]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \count_reg[0]_0\,
      CLR => rst,
      D => \count_reg[8]_i_1_n_5\,
      Q => count_reg(10)
    );
\count_reg[11]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \count_reg[0]_0\,
      CLR => rst,
      D => \count_reg[8]_i_1_n_4\,
      Q => count_reg(11)
    );
\count_reg[12]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \count_reg[0]_0\,
      CLR => rst,
      D => \count_reg[12]_i_1_n_7\,
      Q => count_reg(12)
    );
\count_reg[12]_i_1\: unisim.vcomponents.CARRY4
     port map (
      CI => \count_reg[8]_i_1_n_0\,
      CO(3) => \count_reg[12]_i_1_n_0\,
      CO(2) => \count_reg[12]_i_1_n_1\,
      CO(1) => \count_reg[12]_i_1_n_2\,
      CO(0) => \count_reg[12]_i_1_n_3\,
      CYINIT => '0',
      DI(3) => '0',
      DI(2) => '0',
      DI(1) => '0',
      DI(0) => '0',
      O(3) => \count_reg[12]_i_1_n_4\,
      O(2) => \count_reg[12]_i_1_n_5\,
      O(1) => \count_reg[12]_i_1_n_6\,
      O(0) => \count_reg[12]_i_1_n_7\,
      S(3) => \count[12]_i_2_n_0\,
      S(2) => \count[12]_i_3_n_0\,
      S(1) => \count[12]_i_4_n_0\,
      S(0) => \count[12]_i_5_n_0\
    );
\count_reg[13]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \count_reg[0]_0\,
      CLR => rst,
      D => \count_reg[12]_i_1_n_6\,
      Q => count_reg(13)
    );
\count_reg[14]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \count_reg[0]_0\,
      CLR => rst,
      D => \count_reg[12]_i_1_n_5\,
      Q => count_reg(14)
    );
\count_reg[15]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \count_reg[0]_0\,
      CLR => rst,
      D => \count_reg[12]_i_1_n_4\,
      Q => count_reg(15)
    );
\count_reg[16]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \count_reg[0]_0\,
      CLR => rst,
      D => \count_reg[16]_i_1_n_7\,
      Q => count_reg(16)
    );
\count_reg[16]_i_1\: unisim.vcomponents.CARRY4
     port map (
      CI => \count_reg[12]_i_1_n_0\,
      CO(3 downto 0) => \NLW_count_reg[16]_i_1_CO_UNCONNECTED\(3 downto 0),
      CYINIT => '0',
      DI(3) => '0',
      DI(2) => '0',
      DI(1) => '0',
      DI(0) => '0',
      O(3 downto 1) => \NLW_count_reg[16]_i_1_O_UNCONNECTED\(3 downto 1),
      O(0) => \count_reg[16]_i_1_n_7\,
      S(3) => '0',
      S(2) => '0',
      S(1) => '0',
      S(0) => \count[16]_i_2_n_0\
    );
\count_reg[1]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \count_reg[0]_0\,
      CLR => rst,
      D => \count_reg[0]_i_2_n_6\,
      Q => count_reg(1)
    );
\count_reg[2]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \count_reg[0]_0\,
      CLR => rst,
      D => \count_reg[0]_i_2_n_5\,
      Q => count_reg(2)
    );
\count_reg[3]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \count_reg[0]_0\,
      CLR => rst,
      D => \count_reg[0]_i_2_n_4\,
      Q => count_reg(3)
    );
\count_reg[4]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \count_reg[0]_0\,
      CLR => rst,
      D => \count_reg[4]_i_1_n_7\,
      Q => count_reg(4)
    );
\count_reg[4]_i_1\: unisim.vcomponents.CARRY4
     port map (
      CI => \count_reg[0]_i_2_n_0\,
      CO(3) => \count_reg[4]_i_1_n_0\,
      CO(2) => \count_reg[4]_i_1_n_1\,
      CO(1) => \count_reg[4]_i_1_n_2\,
      CO(0) => \count_reg[4]_i_1_n_3\,
      CYINIT => '0',
      DI(3) => '0',
      DI(2) => '0',
      DI(1) => '0',
      DI(0) => '0',
      O(3) => \count_reg[4]_i_1_n_4\,
      O(2) => \count_reg[4]_i_1_n_5\,
      O(1) => \count_reg[4]_i_1_n_6\,
      O(0) => \count_reg[4]_i_1_n_7\,
      S(3) => \count[4]_i_2_n_0\,
      S(2) => \count[4]_i_3_n_0\,
      S(1) => \count[4]_i_4_n_0\,
      S(0) => \count[4]_i_5_n_0\
    );
\count_reg[5]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \count_reg[0]_0\,
      CLR => rst,
      D => \count_reg[4]_i_1_n_6\,
      Q => count_reg(5)
    );
\count_reg[6]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \count_reg[0]_0\,
      CLR => rst,
      D => \count_reg[4]_i_1_n_5\,
      Q => count_reg(6)
    );
\count_reg[7]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \count_reg[0]_0\,
      CLR => rst,
      D => \count_reg[4]_i_1_n_4\,
      Q => count_reg(7)
    );
\count_reg[8]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \count_reg[0]_0\,
      CLR => rst,
      D => \count_reg[8]_i_1_n_7\,
      Q => count_reg(8)
    );
\count_reg[8]_i_1\: unisim.vcomponents.CARRY4
     port map (
      CI => \count_reg[4]_i_1_n_0\,
      CO(3) => \count_reg[8]_i_1_n_0\,
      CO(2) => \count_reg[8]_i_1_n_1\,
      CO(1) => \count_reg[8]_i_1_n_2\,
      CO(0) => \count_reg[8]_i_1_n_3\,
      CYINIT => '0',
      DI(3) => '0',
      DI(2) => '0',
      DI(1) => '0',
      DI(0) => '0',
      O(3) => \count_reg[8]_i_1_n_4\,
      O(2) => \count_reg[8]_i_1_n_5\,
      O(1) => \count_reg[8]_i_1_n_6\,
      O(0) => \count_reg[8]_i_1_n_7\,
      S(3) => \count[8]_i_2_n_0\,
      S(2) => \count[8]_i_3_n_0\,
      S(1) => \count[8]_i_4_n_0\,
      S(0) => \count[8]_i_5_n_0\
    );
\count_reg[9]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \count_reg[0]_0\,
      CLR => rst,
      D => \count_reg[8]_i_1_n_6\,
      Q => count_reg(9)
    );
valid_INST_0_i_1: unisim.vcomponents.CARRY4
     port map (
      CI => valid_INST_0_i_2_n_0,
      CO(3 downto 1) => NLW_valid_INST_0_i_1_CO_UNCONNECTED(3 downto 1),
      CO(0) => CO(0),
      CYINIT => '0',
      DI(3) => '0',
      DI(2) => '0',
      DI(1) => '0',
      DI(0) => valid_INST_0_i_3_n_0,
      O(3 downto 0) => NLW_valid_INST_0_i_1_O_UNCONNECTED(3 downto 0),
      S(3) => '0',
      S(2) => '0',
      S(1) => '0',
      S(0) => valid_INST_0_i_4_n_0
    );
valid_INST_0_i_10: unisim.vcomponents.LUT4
    generic map(
      INIT => X"9009"
    )
        port map (
      I0 => count_reg(14),
      I1 => Q(14),
      I2 => count_reg(15),
      I3 => Q(15),
      O => valid_INST_0_i_10_n_0
    );
valid_INST_0_i_11: unisim.vcomponents.LUT4
    generic map(
      INIT => X"9009"
    )
        port map (
      I0 => count_reg(12),
      I1 => Q(12),
      I2 => count_reg(13),
      I3 => Q(13),
      O => valid_INST_0_i_11_n_0
    );
valid_INST_0_i_12: unisim.vcomponents.LUT4
    generic map(
      INIT => X"9009"
    )
        port map (
      I0 => count_reg(10),
      I1 => Q(10),
      I2 => count_reg(11),
      I3 => Q(11),
      O => valid_INST_0_i_12_n_0
    );
valid_INST_0_i_13: unisim.vcomponents.LUT4
    generic map(
      INIT => X"9009"
    )
        port map (
      I0 => count_reg(8),
      I1 => Q(8),
      I2 => count_reg(9),
      I3 => Q(9),
      O => valid_INST_0_i_13_n_0
    );
valid_INST_0_i_14: unisim.vcomponents.LUT4
    generic map(
      INIT => X"22B2"
    )
        port map (
      I0 => count_reg(7),
      I1 => Q(7),
      I2 => count_reg(6),
      I3 => Q(6),
      O => valid_INST_0_i_14_n_0
    );
valid_INST_0_i_15: unisim.vcomponents.LUT4
    generic map(
      INIT => X"2F02"
    )
        port map (
      I0 => count_reg(4),
      I1 => Q(4),
      I2 => Q(5),
      I3 => count_reg(5),
      O => valid_INST_0_i_15_n_0
    );
valid_INST_0_i_16: unisim.vcomponents.LUT4
    generic map(
      INIT => X"2F02"
    )
        port map (
      I0 => count_reg(2),
      I1 => Q(2),
      I2 => Q(3),
      I3 => count_reg(3),
      O => valid_INST_0_i_16_n_0
    );
valid_INST_0_i_17: unisim.vcomponents.LUT4
    generic map(
      INIT => X"2F02"
    )
        port map (
      I0 => count_reg(0),
      I1 => Q(0),
      I2 => Q(1),
      I3 => count_reg(1),
      O => valid_INST_0_i_17_n_0
    );
valid_INST_0_i_18: unisim.vcomponents.LUT4
    generic map(
      INIT => X"9009"
    )
        port map (
      I0 => count_reg(6),
      I1 => Q(6),
      I2 => count_reg(7),
      I3 => Q(7),
      O => valid_INST_0_i_18_n_0
    );
valid_INST_0_i_19: unisim.vcomponents.LUT4
    generic map(
      INIT => X"9009"
    )
        port map (
      I0 => count_reg(4),
      I1 => Q(4),
      I2 => count_reg(5),
      I3 => Q(5),
      O => valid_INST_0_i_19_n_0
    );
valid_INST_0_i_2: unisim.vcomponents.CARRY4
     port map (
      CI => valid_INST_0_i_5_n_0,
      CO(3) => valid_INST_0_i_2_n_0,
      CO(2) => valid_INST_0_i_2_n_1,
      CO(1) => valid_INST_0_i_2_n_2,
      CO(0) => valid_INST_0_i_2_n_3,
      CYINIT => '0',
      DI(3) => valid_INST_0_i_6_n_0,
      DI(2) => valid_INST_0_i_7_n_0,
      DI(1) => valid_INST_0_i_8_n_0,
      DI(0) => valid_INST_0_i_9_n_0,
      O(3 downto 0) => NLW_valid_INST_0_i_2_O_UNCONNECTED(3 downto 0),
      S(3) => valid_INST_0_i_10_n_0,
      S(2) => valid_INST_0_i_11_n_0,
      S(1) => valid_INST_0_i_12_n_0,
      S(0) => valid_INST_0_i_13_n_0
    );
valid_INST_0_i_20: unisim.vcomponents.LUT4
    generic map(
      INIT => X"9009"
    )
        port map (
      I0 => count_reg(2),
      I1 => Q(2),
      I2 => count_reg(3),
      I3 => Q(3),
      O => valid_INST_0_i_20_n_0
    );
valid_INST_0_i_21: unisim.vcomponents.LUT4
    generic map(
      INIT => X"9009"
    )
        port map (
      I0 => count_reg(0),
      I1 => Q(0),
      I2 => count_reg(1),
      I3 => Q(1),
      O => valid_INST_0_i_21_n_0
    );
valid_INST_0_i_3: unisim.vcomponents.LUT2
    generic map(
      INIT => X"2"
    )
        port map (
      I0 => count_reg(16),
      I1 => Q(16),
      O => valid_INST_0_i_3_n_0
    );
valid_INST_0_i_4: unisim.vcomponents.LUT2
    generic map(
      INIT => X"9"
    )
        port map (
      I0 => Q(16),
      I1 => count_reg(16),
      O => valid_INST_0_i_4_n_0
    );
valid_INST_0_i_5: unisim.vcomponents.CARRY4
     port map (
      CI => '0',
      CO(3) => valid_INST_0_i_5_n_0,
      CO(2) => valid_INST_0_i_5_n_1,
      CO(1) => valid_INST_0_i_5_n_2,
      CO(0) => valid_INST_0_i_5_n_3,
      CYINIT => '1',
      DI(3) => valid_INST_0_i_14_n_0,
      DI(2) => valid_INST_0_i_15_n_0,
      DI(1) => valid_INST_0_i_16_n_0,
      DI(0) => valid_INST_0_i_17_n_0,
      O(3 downto 0) => NLW_valid_INST_0_i_5_O_UNCONNECTED(3 downto 0),
      S(3) => valid_INST_0_i_18_n_0,
      S(2) => valid_INST_0_i_19_n_0,
      S(1) => valid_INST_0_i_20_n_0,
      S(0) => valid_INST_0_i_21_n_0
    );
valid_INST_0_i_6: unisim.vcomponents.LUT4
    generic map(
      INIT => X"2F02"
    )
        port map (
      I0 => count_reg(14),
      I1 => Q(14),
      I2 => Q(15),
      I3 => count_reg(15),
      O => valid_INST_0_i_6_n_0
    );
valid_INST_0_i_7: unisim.vcomponents.LUT4
    generic map(
      INIT => X"2F02"
    )
        port map (
      I0 => count_reg(12),
      I1 => Q(12),
      I2 => Q(13),
      I3 => count_reg(13),
      O => valid_INST_0_i_7_n_0
    );
valid_INST_0_i_8: unisim.vcomponents.LUT4
    generic map(
      INIT => X"2F02"
    )
        port map (
      I0 => count_reg(10),
      I1 => Q(10),
      I2 => Q(11),
      I3 => count_reg(11),
      O => valid_INST_0_i_8_n_0
    );
valid_INST_0_i_9: unisim.vcomponents.LUT4
    generic map(
      INIT => X"2F02"
    )
        port map (
      I0 => count_reg(8),
      I1 => Q(8),
      I2 => Q(9),
      I3 => count_reg(9),
      O => valid_INST_0_i_9_n_0
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_rd_bin_cntr is
  port (
    Q : out STD_LOGIC_VECTOR ( 2 downto 0 );
    ram_empty_fb_i_reg : out STD_LOGIC;
    D : out STD_LOGIC_VECTOR ( 4 downto 0 );
    \DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram\ : out STD_LOGIC_VECTOR ( 5 downto 0 );
    \wr_pntr_bin_reg[3]\ : in STD_LOGIC_VECTOR ( 2 downto 0 );
    \gpregsm1.curr_fwft_state_reg[1]\ : in STD_LOGIC;
    \wr_pntr_bin_reg[2]\ : in STD_LOGIC;
    \gc0.count_d1_reg[4]_0\ : in STD_LOGIC;
    E : in STD_LOGIC_VECTOR ( 0 to 0 );
    rd_clk : in STD_LOGIC;
    \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[2]\ : in STD_LOGIC_VECTOR ( 0 to 0 )
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_rd_bin_cntr : entity is "rd_bin_cntr";
end dram_rd_ram0_0_rd_bin_cntr;

architecture STRUCTURE of dram_rd_ram0_0_rd_bin_cntr is
  signal \^device_7series.no_bmm_info.sdp.wide_prim18.ram\ : STD_LOGIC_VECTOR ( 5 downto 0 );
  signal \^q\ : STD_LOGIC_VECTOR ( 2 downto 0 );
  signal \plusOp__0\ : STD_LOGIC_VECTOR ( 5 downto 0 );
  signal ram_empty_fb_i_i_2_n_0 : STD_LOGIC;
  signal rd_pntr_plus1 : STD_LOGIC_VECTOR ( 3 downto 0 );
  attribute SOFT_HLUTNM : string;
  attribute SOFT_HLUTNM of \gc0.count[1]_i_1\ : label is "soft_lutpair5";
  attribute SOFT_HLUTNM of \gc0.count[3]_i_1\ : label is "soft_lutpair4";
  attribute SOFT_HLUTNM of \gc0.count[4]_i_1\ : label is "soft_lutpair4";
  attribute SOFT_HLUTNM of ram_empty_fb_i_i_1 : label is "soft_lutpair5";
  attribute SOFT_HLUTNM of \rd_pntr_gc[0]_i_1\ : label is "soft_lutpair6";
  attribute SOFT_HLUTNM of \rd_pntr_gc[1]_i_1\ : label is "soft_lutpair6";
  attribute SOFT_HLUTNM of \rd_pntr_gc[3]_i_1\ : label is "soft_lutpair7";
  attribute SOFT_HLUTNM of \rd_pntr_gc[4]_i_1\ : label is "soft_lutpair7";
begin
  \DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram\(5 downto 0) <= \^device_7series.no_bmm_info.sdp.wide_prim18.ram\(5 downto 0);
  Q(2 downto 0) <= \^q\(2 downto 0);
\gc0.count[0]_i_1\: unisim.vcomponents.LUT1
    generic map(
      INIT => X"1"
    )
        port map (
      I0 => rd_pntr_plus1(0),
      O => \plusOp__0\(0)
    );
\gc0.count[1]_i_1\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"6"
    )
        port map (
      I0 => rd_pntr_plus1(0),
      I1 => rd_pntr_plus1(1),
      O => \plusOp__0\(1)
    );
\gc0.count[2]_i_1\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"6A"
    )
        port map (
      I0 => \^q\(0),
      I1 => rd_pntr_plus1(1),
      I2 => rd_pntr_plus1(0),
      O => \plusOp__0\(2)
    );
\gc0.count[3]_i_1\: unisim.vcomponents.LUT4
    generic map(
      INIT => X"6AAA"
    )
        port map (
      I0 => rd_pntr_plus1(3),
      I1 => rd_pntr_plus1(0),
      I2 => rd_pntr_plus1(1),
      I3 => \^q\(0),
      O => \plusOp__0\(3)
    );
\gc0.count[4]_i_1\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"6AAAAAAA"
    )
        port map (
      I0 => \^q\(1),
      I1 => \^q\(0),
      I2 => rd_pntr_plus1(1),
      I3 => rd_pntr_plus1(0),
      I4 => rd_pntr_plus1(3),
      O => \plusOp__0\(4)
    );
\gc0.count[5]_i_1\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"6AAAAAAAAAAAAAAA"
    )
        port map (
      I0 => \^q\(2),
      I1 => rd_pntr_plus1(3),
      I2 => rd_pntr_plus1(0),
      I3 => rd_pntr_plus1(1),
      I4 => \^q\(0),
      I5 => \^q\(1),
      O => \plusOp__0\(5)
    );
\gc0.count_d1_reg[0]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[2]\(0),
      D => rd_pntr_plus1(0),
      Q => \^device_7series.no_bmm_info.sdp.wide_prim18.ram\(0)
    );
\gc0.count_d1_reg[1]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[2]\(0),
      D => rd_pntr_plus1(1),
      Q => \^device_7series.no_bmm_info.sdp.wide_prim18.ram\(1)
    );
\gc0.count_d1_reg[2]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[2]\(0),
      D => \^q\(0),
      Q => \^device_7series.no_bmm_info.sdp.wide_prim18.ram\(2)
    );
\gc0.count_d1_reg[3]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[2]\(0),
      D => rd_pntr_plus1(3),
      Q => \^device_7series.no_bmm_info.sdp.wide_prim18.ram\(3)
    );
\gc0.count_d1_reg[4]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[2]\(0),
      D => \^q\(1),
      Q => \^device_7series.no_bmm_info.sdp.wide_prim18.ram\(4)
    );
\gc0.count_d1_reg[5]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[2]\(0),
      D => \^q\(2),
      Q => \^device_7series.no_bmm_info.sdp.wide_prim18.ram\(5)
    );
\gc0.count_reg[0]\: unisim.vcomponents.FDPE
    generic map(
      INIT => '1'
    )
        port map (
      C => rd_clk,
      CE => E(0),
      D => \plusOp__0\(0),
      PRE => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[2]\(0),
      Q => rd_pntr_plus1(0)
    );
\gc0.count_reg[1]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[2]\(0),
      D => \plusOp__0\(1),
      Q => rd_pntr_plus1(1)
    );
\gc0.count_reg[2]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[2]\(0),
      D => \plusOp__0\(2),
      Q => \^q\(0)
    );
\gc0.count_reg[3]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[2]\(0),
      D => \plusOp__0\(3),
      Q => rd_pntr_plus1(3)
    );
\gc0.count_reg[4]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[2]\(0),
      D => \plusOp__0\(4),
      Q => \^q\(1)
    );
\gc0.count_reg[5]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[2]\(0),
      D => \plusOp__0\(5),
      Q => \^q\(2)
    );
ram_empty_fb_i_i_1: unisim.vcomponents.LUT4
    generic map(
      INIT => X"FF90"
    )
        port map (
      I0 => rd_pntr_plus1(0),
      I1 => \wr_pntr_bin_reg[3]\(0),
      I2 => ram_empty_fb_i_i_2_n_0,
      I3 => \gc0.count_d1_reg[4]_0\,
      O => ram_empty_fb_i_reg
    );
ram_empty_fb_i_i_2: unisim.vcomponents.LUT6
    generic map(
      INIT => X"9009000000000000"
    )
        port map (
      I0 => rd_pntr_plus1(3),
      I1 => \wr_pntr_bin_reg[3]\(2),
      I2 => rd_pntr_plus1(1),
      I3 => \wr_pntr_bin_reg[3]\(1),
      I4 => \gpregsm1.curr_fwft_state_reg[1]\,
      I5 => \wr_pntr_bin_reg[2]\,
      O => ram_empty_fb_i_i_2_n_0
    );
\rd_pntr_gc[0]_i_1\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"6"
    )
        port map (
      I0 => \^device_7series.no_bmm_info.sdp.wide_prim18.ram\(1),
      I1 => \^device_7series.no_bmm_info.sdp.wide_prim18.ram\(0),
      O => D(0)
    );
\rd_pntr_gc[1]_i_1\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"6"
    )
        port map (
      I0 => \^device_7series.no_bmm_info.sdp.wide_prim18.ram\(1),
      I1 => \^device_7series.no_bmm_info.sdp.wide_prim18.ram\(2),
      O => D(1)
    );
\rd_pntr_gc[2]_i_1\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"6"
    )
        port map (
      I0 => \^device_7series.no_bmm_info.sdp.wide_prim18.ram\(2),
      I1 => \^device_7series.no_bmm_info.sdp.wide_prim18.ram\(3),
      O => D(2)
    );
\rd_pntr_gc[3]_i_1\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"6"
    )
        port map (
      I0 => \^device_7series.no_bmm_info.sdp.wide_prim18.ram\(3),
      I1 => \^device_7series.no_bmm_info.sdp.wide_prim18.ram\(4),
      O => D(3)
    );
\rd_pntr_gc[4]_i_1\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"6"
    )
        port map (
      I0 => \^device_7series.no_bmm_info.sdp.wide_prim18.ram\(4),
      I1 => \^device_7series.no_bmm_info.sdp.wide_prim18.ram\(5),
      O => D(4)
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_rd_fwft is
  port (
    empty : out STD_LOGIC;
    ram_empty_fb_i_reg : out STD_LOGIC;
    E : out STD_LOGIC_VECTOR ( 0 to 0 );
    tmp_ram_rd_en : out STD_LOGIC;
    \goreg_bm.dout_i_reg[31]\ : out STD_LOGIC_VECTOR ( 0 to 0 );
    rd_clk : in STD_LOGIC;
    Q : in STD_LOGIC_VECTOR ( 1 downto 0 );
    rd_en : in STD_LOGIC;
    p_18_out : in STD_LOGIC;
    \wr_pntr_bin_reg[5]\ : in STD_LOGIC_VECTOR ( 0 to 0 );
    \gc0.count_reg[5]\ : in STD_LOGIC_VECTOR ( 0 to 0 )
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_rd_fwft : entity is "rd_fwft";
end dram_rd_ram0_0_rd_fwft;

architecture STRUCTURE of dram_rd_ram0_0_rd_fwft is
  signal curr_fwft_state : STD_LOGIC_VECTOR ( 0 to 0 );
  signal empty_fwft_fb : STD_LOGIC;
  signal empty_fwft_i0 : STD_LOGIC;
  signal \gpregsm1.curr_fwft_state[1]_i_1_n_0\ : STD_LOGIC;
  signal \gpregsm1.curr_fwft_state_reg_n_0_[1]\ : STD_LOGIC;
  signal next_fwft_state : STD_LOGIC_VECTOR ( 0 to 0 );
  attribute equivalent_register_removal : string;
  attribute equivalent_register_removal of empty_fwft_fb_reg : label is "no";
  attribute SOFT_HLUTNM : string;
  attribute SOFT_HLUTNM of empty_fwft_i_i_1 : label is "soft_lutpair3";
  attribute equivalent_register_removal of empty_fwft_i_reg : label is "no";
  attribute SOFT_HLUTNM of \gc0.count_d1[5]_i_1\ : label is "soft_lutpair2";
  attribute SOFT_HLUTNM of \goreg_bm.dout_i[31]_i_1\ : label is "soft_lutpair3";
  attribute SOFT_HLUTNM of \gpregsm1.curr_fwft_state[1]_i_1\ : label is "soft_lutpair2";
  attribute equivalent_register_removal of \gpregsm1.curr_fwft_state_reg[0]\ : label is "no";
  attribute equivalent_register_removal of \gpregsm1.curr_fwft_state_reg[1]\ : label is "no";
begin
\DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram_i_1\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"AAAAFFBF"
    )
        port map (
      I0 => Q(0),
      I1 => \gpregsm1.curr_fwft_state_reg_n_0_[1]\,
      I2 => curr_fwft_state(0),
      I3 => rd_en,
      I4 => p_18_out,
      O => tmp_ram_rd_en
    );
empty_fwft_fb_reg: unisim.vcomponents.FDPE
    generic map(
      INIT => '1'
    )
        port map (
      C => rd_clk,
      CE => '1',
      D => empty_fwft_i0,
      PRE => Q(1),
      Q => empty_fwft_fb
    );
empty_fwft_i_i_1: unisim.vcomponents.LUT4
    generic map(
      INIT => X"BA22"
    )
        port map (
      I0 => empty_fwft_fb,
      I1 => \gpregsm1.curr_fwft_state_reg_n_0_[1]\,
      I2 => rd_en,
      I3 => curr_fwft_state(0),
      O => empty_fwft_i0
    );
empty_fwft_i_reg: unisim.vcomponents.FDPE
    generic map(
      INIT => '1'
    )
        port map (
      C => rd_clk,
      CE => '1',
      D => empty_fwft_i0,
      PRE => Q(1),
      Q => empty
    );
\gc0.count_d1[5]_i_1\: unisim.vcomponents.LUT4
    generic map(
      INIT => X"4555"
    )
        port map (
      I0 => p_18_out,
      I1 => rd_en,
      I2 => curr_fwft_state(0),
      I3 => \gpregsm1.curr_fwft_state_reg_n_0_[1]\,
      O => E(0)
    );
\goreg_bm.dout_i[31]_i_1\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"8A"
    )
        port map (
      I0 => \gpregsm1.curr_fwft_state_reg_n_0_[1]\,
      I1 => rd_en,
      I2 => curr_fwft_state(0),
      O => \goreg_bm.dout_i_reg[31]\(0)
    );
\gpregsm1.curr_fwft_state[0]_i_1\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"BA"
    )
        port map (
      I0 => \gpregsm1.curr_fwft_state_reg_n_0_[1]\,
      I1 => rd_en,
      I2 => curr_fwft_state(0),
      O => next_fwft_state(0)
    );
\gpregsm1.curr_fwft_state[1]_i_1\: unisim.vcomponents.LUT4
    generic map(
      INIT => X"40FF"
    )
        port map (
      I0 => rd_en,
      I1 => curr_fwft_state(0),
      I2 => \gpregsm1.curr_fwft_state_reg_n_0_[1]\,
      I3 => p_18_out,
      O => \gpregsm1.curr_fwft_state[1]_i_1_n_0\
    );
\gpregsm1.curr_fwft_state_reg[0]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      CLR => Q(1),
      D => next_fwft_state(0),
      Q => curr_fwft_state(0)
    );
\gpregsm1.curr_fwft_state_reg[1]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      CLR => Q(1),
      D => \gpregsm1.curr_fwft_state[1]_i_1_n_0\,
      Q => \gpregsm1.curr_fwft_state_reg_n_0_[1]\
    );
ram_empty_fb_i_i_4: unisim.vcomponents.LUT6
    generic map(
      INIT => X"00F70000000000F7"
    )
        port map (
      I0 => \gpregsm1.curr_fwft_state_reg_n_0_[1]\,
      I1 => curr_fwft_state(0),
      I2 => rd_en,
      I3 => p_18_out,
      I4 => \wr_pntr_bin_reg[5]\(0),
      I5 => \gc0.count_reg[5]\(0),
      O => ram_empty_fb_i_reg
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_rd_status_flags_as is
  port (
    p_18_out : out STD_LOGIC;
    \gc0.count_reg[0]\ : in STD_LOGIC;
    rd_clk : in STD_LOGIC;
    Q : in STD_LOGIC_VECTOR ( 0 to 0 )
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_rd_status_flags_as : entity is "rd_status_flags_as";
end dram_rd_ram0_0_rd_status_flags_as;

architecture STRUCTURE of dram_rd_ram0_0_rd_status_flags_as is
  attribute equivalent_register_removal : string;
  attribute equivalent_register_removal of ram_empty_fb_i_reg : label is "no";
begin
ram_empty_fb_i_reg: unisim.vcomponents.FDPE
    generic map(
      INIT => '1'
    )
        port map (
      C => rd_clk,
      CE => '1',
      D => \gc0.count_reg[0]\,
      PRE => Q(0),
      Q => p_18_out
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_reset_blk_ramfifo is
  port (
    rst_full_ff_i : out STD_LOGIC;
    rst_full_gen_i : out STD_LOGIC;
    Q : out STD_LOGIC_VECTOR ( 2 downto 0 );
    \gic0.gc0.count_reg[0]\ : out STD_LOGIC_VECTOR ( 1 downto 0 );
    wr_clk : in STD_LOGIC;
    rst : in STD_LOGIC;
    rd_clk : in STD_LOGIC
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_reset_blk_ramfifo : entity is "reset_blk_ramfifo";
end dram_rd_ram0_0_reset_blk_ramfifo;

architecture STRUCTURE of dram_rd_ram0_0_reset_blk_ramfifo is
  signal \ngwrdrst.grst.g7serrst.rd_rst_asreg_i_1_n_0\ : STD_LOGIC;
  signal \ngwrdrst.grst.g7serrst.rd_rst_reg[2]_i_1_n_0\ : STD_LOGIC;
  signal \ngwrdrst.grst.g7serrst.wr_rst_asreg_i_1_n_0\ : STD_LOGIC;
  signal \ngwrdrst.grst.g7serrst.wr_rst_reg[1]_i_1_n_0\ : STD_LOGIC;
  signal rd_rst_asreg : STD_LOGIC;
  signal rd_rst_asreg_d1 : STD_LOGIC;
  signal rd_rst_asreg_d2 : STD_LOGIC;
  signal rst_d1 : STD_LOGIC;
  signal rst_d2 : STD_LOGIC;
  signal rst_d3 : STD_LOGIC;
  signal rst_rd_reg1 : STD_LOGIC;
  signal rst_rd_reg2 : STD_LOGIC;
  signal rst_wr_reg1 : STD_LOGIC;
  signal rst_wr_reg2 : STD_LOGIC;
  signal wr_rst_asreg : STD_LOGIC;
  signal wr_rst_asreg_d1 : STD_LOGIC;
  signal wr_rst_asreg_d2 : STD_LOGIC;
  attribute ASYNC_REG : boolean;
  attribute ASYNC_REG of \grstd1.grst_full.grst_f.rst_d1_reg\ : label is std.standard.true;
  attribute KEEP : string;
  attribute KEEP of \grstd1.grst_full.grst_f.rst_d1_reg\ : label is "yes";
  attribute ASYNC_REG of \grstd1.grst_full.grst_f.rst_d2_reg\ : label is std.standard.true;
  attribute KEEP of \grstd1.grst_full.grst_f.rst_d2_reg\ : label is "yes";
  attribute ASYNC_REG of \grstd1.grst_full.grst_f.rst_d3_reg\ : label is std.standard.true;
  attribute KEEP of \grstd1.grst_full.grst_f.rst_d3_reg\ : label is "yes";
  attribute equivalent_register_removal : string;
  attribute equivalent_register_removal of \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[0]\ : label is "no";
  attribute equivalent_register_removal of \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\ : label is "no";
  attribute equivalent_register_removal of \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[2]\ : label is "no";
  attribute ASYNC_REG of \ngwrdrst.grst.g7serrst.rst_rd_reg1_reg\ : label is std.standard.true;
  attribute KEEP of \ngwrdrst.grst.g7serrst.rst_rd_reg1_reg\ : label is "yes";
  attribute ASYNC_REG of \ngwrdrst.grst.g7serrst.rst_rd_reg2_reg\ : label is std.standard.true;
  attribute KEEP of \ngwrdrst.grst.g7serrst.rst_rd_reg2_reg\ : label is "yes";
  attribute ASYNC_REG of \ngwrdrst.grst.g7serrst.rst_wr_reg1_reg\ : label is std.standard.true;
  attribute KEEP of \ngwrdrst.grst.g7serrst.rst_wr_reg1_reg\ : label is "yes";
  attribute ASYNC_REG of \ngwrdrst.grst.g7serrst.rst_wr_reg2_reg\ : label is std.standard.true;
  attribute KEEP of \ngwrdrst.grst.g7serrst.rst_wr_reg2_reg\ : label is "yes";
  attribute equivalent_register_removal of \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\ : label is "no";
  attribute equivalent_register_removal of \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\ : label is "no";
begin
  rst_full_ff_i <= rst_d2;
\grstd1.grst_full.grst_f.RST_FULL_GEN_reg\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => rst,
      D => rst_d3,
      Q => rst_full_gen_i
    );
\grstd1.grst_full.grst_f.rst_d1_reg\: unisim.vcomponents.FDPE
    generic map(
      INIT => '1'
    )
        port map (
      C => wr_clk,
      CE => '1',
      D => '0',
      PRE => rst,
      Q => rst_d1
    );
\grstd1.grst_full.grst_f.rst_d2_reg\: unisim.vcomponents.FDPE
    generic map(
      INIT => '1'
    )
        port map (
      C => wr_clk,
      CE => '1',
      D => rst_d1,
      PRE => rst,
      Q => rst_d2
    );
\grstd1.grst_full.grst_f.rst_d3_reg\: unisim.vcomponents.FDPE
    generic map(
      INIT => '1'
    )
        port map (
      C => wr_clk,
      CE => '1',
      D => rst_d2,
      PRE => rst,
      Q => rst_d3
    );
\ngwrdrst.grst.g7serrst.rd_rst_asreg_d1_reg\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      D => rd_rst_asreg,
      Q => rd_rst_asreg_d1,
      R => '0'
    );
\ngwrdrst.grst.g7serrst.rd_rst_asreg_d2_reg\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      D => rd_rst_asreg_d1,
      Q => rd_rst_asreg_d2,
      R => '0'
    );
\ngwrdrst.grst.g7serrst.rd_rst_asreg_i_1\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"2"
    )
        port map (
      I0 => rd_rst_asreg,
      I1 => rd_rst_asreg_d1,
      O => \ngwrdrst.grst.g7serrst.rd_rst_asreg_i_1_n_0\
    );
\ngwrdrst.grst.g7serrst.rd_rst_asreg_reg\: unisim.vcomponents.FDPE
    generic map(
      INIT => '1'
    )
        port map (
      C => rd_clk,
      CE => '1',
      D => \ngwrdrst.grst.g7serrst.rd_rst_asreg_i_1_n_0\,
      PRE => rst_rd_reg2,
      Q => rd_rst_asreg
    );
\ngwrdrst.grst.g7serrst.rd_rst_reg[2]_i_1\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"2"
    )
        port map (
      I0 => rd_rst_asreg,
      I1 => rd_rst_asreg_d2,
      O => \ngwrdrst.grst.g7serrst.rd_rst_reg[2]_i_1_n_0\
    );
\ngwrdrst.grst.g7serrst.rd_rst_reg_reg[0]\: unisim.vcomponents.FDPE
    generic map(
      INIT => '1'
    )
        port map (
      C => rd_clk,
      CE => '1',
      D => '0',
      PRE => \ngwrdrst.grst.g7serrst.rd_rst_reg[2]_i_1_n_0\,
      Q => Q(0)
    );
\ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\: unisim.vcomponents.FDPE
    generic map(
      INIT => '1'
    )
        port map (
      C => rd_clk,
      CE => '1',
      D => '0',
      PRE => \ngwrdrst.grst.g7serrst.rd_rst_reg[2]_i_1_n_0\,
      Q => Q(1)
    );
\ngwrdrst.grst.g7serrst.rd_rst_reg_reg[2]\: unisim.vcomponents.FDPE
    generic map(
      INIT => '1'
    )
        port map (
      C => rd_clk,
      CE => '1',
      D => '0',
      PRE => \ngwrdrst.grst.g7serrst.rd_rst_reg[2]_i_1_n_0\,
      Q => Q(2)
    );
\ngwrdrst.grst.g7serrst.rst_rd_reg1_reg\: unisim.vcomponents.FDPE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      D => '0',
      PRE => rst,
      Q => rst_rd_reg1
    );
\ngwrdrst.grst.g7serrst.rst_rd_reg2_reg\: unisim.vcomponents.FDPE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      D => rst_rd_reg1,
      PRE => rst,
      Q => rst_rd_reg2
    );
\ngwrdrst.grst.g7serrst.rst_wr_reg1_reg\: unisim.vcomponents.FDPE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      D => '0',
      PRE => rst,
      Q => rst_wr_reg1
    );
\ngwrdrst.grst.g7serrst.rst_wr_reg2_reg\: unisim.vcomponents.FDPE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      D => rst_wr_reg1,
      PRE => rst,
      Q => rst_wr_reg2
    );
\ngwrdrst.grst.g7serrst.wr_rst_asreg_d1_reg\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      D => wr_rst_asreg,
      Q => wr_rst_asreg_d1,
      R => '0'
    );
\ngwrdrst.grst.g7serrst.wr_rst_asreg_d2_reg\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      D => wr_rst_asreg_d1,
      Q => wr_rst_asreg_d2,
      R => '0'
    );
\ngwrdrst.grst.g7serrst.wr_rst_asreg_i_1\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"2"
    )
        port map (
      I0 => wr_rst_asreg,
      I1 => wr_rst_asreg_d1,
      O => \ngwrdrst.grst.g7serrst.wr_rst_asreg_i_1_n_0\
    );
\ngwrdrst.grst.g7serrst.wr_rst_asreg_reg\: unisim.vcomponents.FDPE
    generic map(
      INIT => '1'
    )
        port map (
      C => wr_clk,
      CE => '1',
      D => \ngwrdrst.grst.g7serrst.wr_rst_asreg_i_1_n_0\,
      PRE => rst_wr_reg2,
      Q => wr_rst_asreg
    );
\ngwrdrst.grst.g7serrst.wr_rst_reg[1]_i_1\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"2"
    )
        port map (
      I0 => wr_rst_asreg,
      I1 => wr_rst_asreg_d2,
      O => \ngwrdrst.grst.g7serrst.wr_rst_reg[1]_i_1_n_0\
    );
\ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\: unisim.vcomponents.FDPE
    generic map(
      INIT => '1'
    )
        port map (
      C => wr_clk,
      CE => '1',
      D => '0',
      PRE => \ngwrdrst.grst.g7serrst.wr_rst_reg[1]_i_1_n_0\,
      Q => \gic0.gc0.count_reg[0]\(0)
    );
\ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\: unisim.vcomponents.FDPE
    generic map(
      INIT => '1'
    )
        port map (
      C => wr_clk,
      CE => '1',
      D => '0',
      PRE => \ngwrdrst.grst.g7serrst.wr_rst_reg[1]_i_1_n_0\,
      Q => \gic0.gc0.count_reg[0]\(1)
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_shrinker_fifo is
  port (
    valid : out STD_LOGIC;
    \count_reg[16]\ : out STD_LOGIC;
    AR : out STD_LOGIC_VECTOR ( 0 to 0 );
    rd_en0_in : out STD_LOGIC;
    data : out STD_LOGIC_VECTOR ( 15 downto 0 );
    CO : in STD_LOGIC_VECTOR ( 0 to 0 );
    clear : in STD_LOGIC;
    rd_en : in STD_LOGIC;
    empty : in STD_LOGIC;
    dout : in STD_LOGIC_VECTOR ( 31 downto 0 );
    rst : in STD_LOGIC;
    user_clk : in STD_LOGIC
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_shrinker_fifo : entity is "shrinker_fifo";
end dram_rd_ram0_0_shrinker_fifo;

architecture STRUCTURE of dram_rd_ram0_0_shrinker_fifo is
  signal \^ar\ : STD_LOGIC_VECTOR ( 0 to 0 );
  signal \count[1]_i_1_n_0\ : STD_LOGIC;
  signal \count_reg__0\ : STD_LOGIC_VECTOR ( 1 downto 0 );
  signal \data[15]_i_1_n_0\ : STD_LOGIC;
  signal \data[16]_i_1_n_0\ : STD_LOGIC;
  signal \data[17]_i_1_n_0\ : STD_LOGIC;
  signal \data[18]_i_1_n_0\ : STD_LOGIC;
  signal \data[19]_i_1_n_0\ : STD_LOGIC;
  signal \data[20]_i_1_n_0\ : STD_LOGIC;
  signal \data[21]_i_1_n_0\ : STD_LOGIC;
  signal \data[22]_i_1_n_0\ : STD_LOGIC;
  signal \data[23]_i_1_n_0\ : STD_LOGIC;
  signal \data[24]_i_1_n_0\ : STD_LOGIC;
  signal \data[25]_i_1_n_0\ : STD_LOGIC;
  signal \data[26]_i_1_n_0\ : STD_LOGIC;
  signal \data[27]_i_1_n_0\ : STD_LOGIC;
  signal \data[28]_i_1_n_0\ : STD_LOGIC;
  signal \data[29]_i_1_n_0\ : STD_LOGIC;
  signal \data[30]_i_1_n_0\ : STD_LOGIC;
  signal \data[31]_i_1_n_0\ : STD_LOGIC;
  signal \data_reg_n_0_[16]\ : STD_LOGIC;
  signal \data_reg_n_0_[17]\ : STD_LOGIC;
  signal \data_reg_n_0_[18]\ : STD_LOGIC;
  signal \data_reg_n_0_[19]\ : STD_LOGIC;
  signal \data_reg_n_0_[20]\ : STD_LOGIC;
  signal \data_reg_n_0_[21]\ : STD_LOGIC;
  signal \data_reg_n_0_[22]\ : STD_LOGIC;
  signal \data_reg_n_0_[23]\ : STD_LOGIC;
  signal \data_reg_n_0_[24]\ : STD_LOGIC;
  signal \data_reg_n_0_[25]\ : STD_LOGIC;
  signal \data_reg_n_0_[26]\ : STD_LOGIC;
  signal \data_reg_n_0_[27]\ : STD_LOGIC;
  signal \data_reg_n_0_[28]\ : STD_LOGIC;
  signal \data_reg_n_0_[29]\ : STD_LOGIC;
  signal \data_reg_n_0_[30]\ : STD_LOGIC;
  signal \data_reg_n_0_[31]\ : STD_LOGIC;
  signal next_count : STD_LOGIC_VECTOR ( 0 to 0 );
  signal next_data : STD_LOGIC_VECTOR ( 15 downto 0 );
  attribute SOFT_HLUTNM : string;
  attribute SOFT_HLUTNM of U_FIFO_i_1 : label is "soft_lutpair19";
  attribute SOFT_HLUTNM of \count[0]_i_1\ : label is "soft_lutpair21";
  attribute SOFT_HLUTNM of \count[0]_i_1__0\ : label is "soft_lutpair20";
  attribute SOFT_HLUTNM of \count[1]_i_1\ : label is "soft_lutpair21";
  attribute SOFT_HLUTNM of \data[18]_i_1\ : label is "soft_lutpair19";
  attribute SOFT_HLUTNM of valid_INST_0 : label is "soft_lutpair20";
begin
  AR(0) <= \^ar\(0);
U_FIFO_i_1: unisim.vcomponents.LUT3
    generic map(
      INIT => X"0D"
    )
        port map (
      I0 => \count_reg__0\(0),
      I1 => rd_en,
      I2 => \count_reg__0\(1),
      O => rd_en0_in
    );
\count[0]_i_1\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"58"
    )
        port map (
      I0 => rd_en,
      I1 => \count_reg__0\(1),
      I2 => \count_reg__0\(0),
      O => next_count(0)
    );
\count[0]_i_1__0\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"DDDCCCCC"
    )
        port map (
      I0 => CO(0),
      I1 => clear,
      I2 => \count_reg__0\(0),
      I3 => \count_reg__0\(1),
      I4 => rd_en,
      O => \count_reg[16]\
    );
\count[1]_i_1\: unisim.vcomponents.LUT4
    generic map(
      INIT => X"D1CD"
    )
        port map (
      I0 => empty,
      I1 => \count_reg__0\(1),
      I2 => \count_reg__0\(0),
      I3 => rd_en,
      O => \count[1]_i_1_n_0\
    );
\count_reg[0]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => '1',
      CLR => \^ar\(0),
      D => next_count(0),
      Q => \count_reg__0\(0)
    );
\count_reg[1]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => '1',
      CLR => \^ar\(0),
      D => \count[1]_i_1_n_0\,
      Q => \count_reg__0\(1)
    );
\data[0]_i_1\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"CCCCCACACCCCCCCA"
    )
        port map (
      I0 => dout(0),
      I1 => \data_reg_n_0_[16]\,
      I2 => empty,
      I3 => \count_reg__0\(0),
      I4 => \count_reg__0\(1),
      I5 => rd_en,
      O => next_data(0)
    );
\data[10]_i_1\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"CCCCCACACCCCCCCA"
    )
        port map (
      I0 => dout(10),
      I1 => \data_reg_n_0_[26]\,
      I2 => empty,
      I3 => \count_reg__0\(0),
      I4 => \count_reg__0\(1),
      I5 => rd_en,
      O => next_data(10)
    );
\data[11]_i_1\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"CCCCCACACCCCCCCA"
    )
        port map (
      I0 => dout(11),
      I1 => \data_reg_n_0_[27]\,
      I2 => empty,
      I3 => \count_reg__0\(0),
      I4 => \count_reg__0\(1),
      I5 => rd_en,
      O => next_data(11)
    );
\data[12]_i_1\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"CCCCCACACCCCCCCA"
    )
        port map (
      I0 => dout(12),
      I1 => \data_reg_n_0_[28]\,
      I2 => empty,
      I3 => \count_reg__0\(0),
      I4 => \count_reg__0\(1),
      I5 => rd_en,
      O => next_data(12)
    );
\data[13]_i_1\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"CCCCCACACCCCCCCA"
    )
        port map (
      I0 => dout(13),
      I1 => \data_reg_n_0_[29]\,
      I2 => empty,
      I3 => \count_reg__0\(0),
      I4 => \count_reg__0\(1),
      I5 => rd_en,
      O => next_data(13)
    );
\data[14]_i_1\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"CCCCCACACCCCCCCA"
    )
        port map (
      I0 => dout(14),
      I1 => \data_reg_n_0_[30]\,
      I2 => empty,
      I3 => \count_reg__0\(0),
      I4 => \count_reg__0\(1),
      I5 => rd_en,
      O => next_data(14)
    );
\data[15]_i_1\: unisim.vcomponents.LUT4
    generic map(
      INIT => X"FD01"
    )
        port map (
      I0 => empty,
      I1 => \count_reg__0\(0),
      I2 => \count_reg__0\(1),
      I3 => rd_en,
      O => \data[15]_i_1_n_0\
    );
\data[15]_i_2\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"CCCCCACACCCCCCCA"
    )
        port map (
      I0 => dout(15),
      I1 => \data_reg_n_0_[31]\,
      I2 => empty,
      I3 => \count_reg__0\(0),
      I4 => \count_reg__0\(1),
      I5 => rd_en,
      O => next_data(15)
    );
\data[15]_i_3\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"E"
    )
        port map (
      I0 => rst,
      I1 => clear,
      O => \^ar\(0)
    );
\data[16]_i_1\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"00230000"
    )
        port map (
      I0 => rd_en,
      I1 => \count_reg__0\(1),
      I2 => \count_reg__0\(0),
      I3 => empty,
      I4 => dout(16),
      O => \data[16]_i_1_n_0\
    );
\data[17]_i_1\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"00230000"
    )
        port map (
      I0 => rd_en,
      I1 => \count_reg__0\(1),
      I2 => \count_reg__0\(0),
      I3 => empty,
      I4 => dout(17),
      O => \data[17]_i_1_n_0\
    );
\data[18]_i_1\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"00230000"
    )
        port map (
      I0 => rd_en,
      I1 => \count_reg__0\(1),
      I2 => \count_reg__0\(0),
      I3 => empty,
      I4 => dout(18),
      O => \data[18]_i_1_n_0\
    );
\data[19]_i_1\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"00230000"
    )
        port map (
      I0 => rd_en,
      I1 => \count_reg__0\(1),
      I2 => \count_reg__0\(0),
      I3 => empty,
      I4 => dout(19),
      O => \data[19]_i_1_n_0\
    );
\data[1]_i_1\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"CCCCCACACCCCCCCA"
    )
        port map (
      I0 => dout(1),
      I1 => \data_reg_n_0_[17]\,
      I2 => empty,
      I3 => \count_reg__0\(0),
      I4 => \count_reg__0\(1),
      I5 => rd_en,
      O => next_data(1)
    );
\data[20]_i_1\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"00230000"
    )
        port map (
      I0 => rd_en,
      I1 => \count_reg__0\(1),
      I2 => \count_reg__0\(0),
      I3 => empty,
      I4 => dout(20),
      O => \data[20]_i_1_n_0\
    );
\data[21]_i_1\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"00230000"
    )
        port map (
      I0 => rd_en,
      I1 => \count_reg__0\(1),
      I2 => \count_reg__0\(0),
      I3 => empty,
      I4 => dout(21),
      O => \data[21]_i_1_n_0\
    );
\data[22]_i_1\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"00230000"
    )
        port map (
      I0 => rd_en,
      I1 => \count_reg__0\(1),
      I2 => \count_reg__0\(0),
      I3 => empty,
      I4 => dout(22),
      O => \data[22]_i_1_n_0\
    );
\data[23]_i_1\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"00230000"
    )
        port map (
      I0 => rd_en,
      I1 => \count_reg__0\(1),
      I2 => \count_reg__0\(0),
      I3 => empty,
      I4 => dout(23),
      O => \data[23]_i_1_n_0\
    );
\data[24]_i_1\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"00230000"
    )
        port map (
      I0 => rd_en,
      I1 => \count_reg__0\(1),
      I2 => \count_reg__0\(0),
      I3 => empty,
      I4 => dout(24),
      O => \data[24]_i_1_n_0\
    );
\data[25]_i_1\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"00230000"
    )
        port map (
      I0 => rd_en,
      I1 => \count_reg__0\(1),
      I2 => \count_reg__0\(0),
      I3 => empty,
      I4 => dout(25),
      O => \data[25]_i_1_n_0\
    );
\data[26]_i_1\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"00230000"
    )
        port map (
      I0 => rd_en,
      I1 => \count_reg__0\(1),
      I2 => \count_reg__0\(0),
      I3 => empty,
      I4 => dout(26),
      O => \data[26]_i_1_n_0\
    );
\data[27]_i_1\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"00230000"
    )
        port map (
      I0 => rd_en,
      I1 => \count_reg__0\(1),
      I2 => \count_reg__0\(0),
      I3 => empty,
      I4 => dout(27),
      O => \data[27]_i_1_n_0\
    );
\data[28]_i_1\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"00230000"
    )
        port map (
      I0 => rd_en,
      I1 => \count_reg__0\(1),
      I2 => \count_reg__0\(0),
      I3 => empty,
      I4 => dout(28),
      O => \data[28]_i_1_n_0\
    );
\data[29]_i_1\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"00230000"
    )
        port map (
      I0 => rd_en,
      I1 => \count_reg__0\(1),
      I2 => \count_reg__0\(0),
      I3 => empty,
      I4 => dout(29),
      O => \data[29]_i_1_n_0\
    );
\data[2]_i_1\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"CCCCCACACCCCCCCA"
    )
        port map (
      I0 => dout(2),
      I1 => \data_reg_n_0_[18]\,
      I2 => empty,
      I3 => \count_reg__0\(0),
      I4 => \count_reg__0\(1),
      I5 => rd_en,
      O => next_data(2)
    );
\data[30]_i_1\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"00230000"
    )
        port map (
      I0 => rd_en,
      I1 => \count_reg__0\(1),
      I2 => \count_reg__0\(0),
      I3 => empty,
      I4 => dout(30),
      O => \data[30]_i_1_n_0\
    );
\data[31]_i_1\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"00230000"
    )
        port map (
      I0 => rd_en,
      I1 => \count_reg__0\(1),
      I2 => \count_reg__0\(0),
      I3 => empty,
      I4 => dout(31),
      O => \data[31]_i_1_n_0\
    );
\data[3]_i_1\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"CCCCCACACCCCCCCA"
    )
        port map (
      I0 => dout(3),
      I1 => \data_reg_n_0_[19]\,
      I2 => empty,
      I3 => \count_reg__0\(0),
      I4 => \count_reg__0\(1),
      I5 => rd_en,
      O => next_data(3)
    );
\data[4]_i_1\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"CCCCCACACCCCCCCA"
    )
        port map (
      I0 => dout(4),
      I1 => \data_reg_n_0_[20]\,
      I2 => empty,
      I3 => \count_reg__0\(0),
      I4 => \count_reg__0\(1),
      I5 => rd_en,
      O => next_data(4)
    );
\data[5]_i_1\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"CCCCCACACCCCCCCA"
    )
        port map (
      I0 => dout(5),
      I1 => \data_reg_n_0_[21]\,
      I2 => empty,
      I3 => \count_reg__0\(0),
      I4 => \count_reg__0\(1),
      I5 => rd_en,
      O => next_data(5)
    );
\data[6]_i_1\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"CCCCCACACCCCCCCA"
    )
        port map (
      I0 => dout(6),
      I1 => \data_reg_n_0_[22]\,
      I2 => empty,
      I3 => \count_reg__0\(0),
      I4 => \count_reg__0\(1),
      I5 => rd_en,
      O => next_data(6)
    );
\data[7]_i_1\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"CCCCCACACCCCCCCA"
    )
        port map (
      I0 => dout(7),
      I1 => \data_reg_n_0_[23]\,
      I2 => empty,
      I3 => \count_reg__0\(0),
      I4 => \count_reg__0\(1),
      I5 => rd_en,
      O => next_data(7)
    );
\data[8]_i_1\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"CCCCCACACCCCCCCA"
    )
        port map (
      I0 => dout(8),
      I1 => \data_reg_n_0_[24]\,
      I2 => empty,
      I3 => \count_reg__0\(0),
      I4 => \count_reg__0\(1),
      I5 => rd_en,
      O => next_data(8)
    );
\data[9]_i_1\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"CCCCCACACCCCCCCA"
    )
        port map (
      I0 => dout(9),
      I1 => \data_reg_n_0_[25]\,
      I2 => empty,
      I3 => \count_reg__0\(0),
      I4 => \count_reg__0\(1),
      I5 => rd_en,
      O => next_data(9)
    );
\data_reg[0]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => next_data(0),
      Q => data(0)
    );
\data_reg[10]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => next_data(10),
      Q => data(10)
    );
\data_reg[11]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => next_data(11),
      Q => data(11)
    );
\data_reg[12]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => next_data(12),
      Q => data(12)
    );
\data_reg[13]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => next_data(13),
      Q => data(13)
    );
\data_reg[14]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => next_data(14),
      Q => data(14)
    );
\data_reg[15]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => next_data(15),
      Q => data(15)
    );
\data_reg[16]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => \data[16]_i_1_n_0\,
      Q => \data_reg_n_0_[16]\
    );
\data_reg[17]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => \data[17]_i_1_n_0\,
      Q => \data_reg_n_0_[17]\
    );
\data_reg[18]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => \data[18]_i_1_n_0\,
      Q => \data_reg_n_0_[18]\
    );
\data_reg[19]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => \data[19]_i_1_n_0\,
      Q => \data_reg_n_0_[19]\
    );
\data_reg[1]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => next_data(1),
      Q => data(1)
    );
\data_reg[20]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => \data[20]_i_1_n_0\,
      Q => \data_reg_n_0_[20]\
    );
\data_reg[21]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => \data[21]_i_1_n_0\,
      Q => \data_reg_n_0_[21]\
    );
\data_reg[22]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => \data[22]_i_1_n_0\,
      Q => \data_reg_n_0_[22]\
    );
\data_reg[23]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => \data[23]_i_1_n_0\,
      Q => \data_reg_n_0_[23]\
    );
\data_reg[24]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => \data[24]_i_1_n_0\,
      Q => \data_reg_n_0_[24]\
    );
\data_reg[25]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => \data[25]_i_1_n_0\,
      Q => \data_reg_n_0_[25]\
    );
\data_reg[26]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => \data[26]_i_1_n_0\,
      Q => \data_reg_n_0_[26]\
    );
\data_reg[27]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => \data[27]_i_1_n_0\,
      Q => \data_reg_n_0_[27]\
    );
\data_reg[28]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => \data[28]_i_1_n_0\,
      Q => \data_reg_n_0_[28]\
    );
\data_reg[29]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => \data[29]_i_1_n_0\,
      Q => \data_reg_n_0_[29]\
    );
\data_reg[2]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => next_data(2),
      Q => data(2)
    );
\data_reg[30]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => \data[30]_i_1_n_0\,
      Q => \data_reg_n_0_[30]\
    );
\data_reg[31]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => \data[31]_i_1_n_0\,
      Q => \data_reg_n_0_[31]\
    );
\data_reg[3]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => next_data(3),
      Q => data(3)
    );
\data_reg[4]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => next_data(4),
      Q => data(4)
    );
\data_reg[5]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => next_data(5),
      Q => data(5)
    );
\data_reg[6]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => next_data(6),
      Q => data(6)
    );
\data_reg[7]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => next_data(7),
      Q => data(7)
    );
\data_reg[8]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => next_data(8),
      Q => data(8)
    );
\data_reg[9]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \data[15]_i_1_n_0\,
      CLR => \^ar\(0),
      D => next_data(9),
      Q => data(9)
    );
valid_INST_0: unisim.vcomponents.LUT3
    generic map(
      INIT => X"54"
    )
        port map (
      I0 => CO(0),
      I1 => \count_reg__0\(1),
      I2 => \count_reg__0\(0),
      O => valid
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_synchronizer_dual_flop is
  port (
    got_it_tg_ff_reg : out STD_LOGIC;
    state_reg : out STD_LOGIC;
    busy_reg : out STD_LOGIC;
    take_it_tg_reg : out STD_LOGIC;
    got_it_tg : in STD_LOGIC;
    dram_clk : in STD_LOGIC;
    AR : in STD_LOGIC_VECTOR ( 0 to 0 );
    user_clk : in STD_LOGIC;
    got_it_tg_ff_reg_0 : in STD_LOGIC;
    state_0 : in STD_LOGIC;
    handshake_go : in STD_LOGIC;
    go : in STD_LOGIC;
    \state_reg[0]\ : in STD_LOGIC;
    \state_reg[1]\ : in STD_LOGIC;
    busy : in STD_LOGIC;
    take_it_tg : in STD_LOGIC
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_synchronizer_dual_flop : entity is "synchronizer_dual_flop";
end dram_rd_ram0_0_synchronizer_dual_flop;

architecture STRUCTURE of dram_rd_ram0_0_synchronizer_dual_flop is
  signal \cdc_reg_n_0_[0]\ : STD_LOGIC;
  signal \^got_it_tg_ff_reg\ : STD_LOGIC;
  signal \sync1_reg_n_0_[0]\ : STD_LOGIC;
  attribute SOFT_HLUTNM : string;
  attribute SOFT_HLUTNM of state_i_1 : label is "soft_lutpair17";
  attribute SOFT_HLUTNM of take_it_tg_i_1 : label is "soft_lutpair17";
begin
  got_it_tg_ff_reg <= \^got_it_tg_ff_reg\;
busy_i_1: unisim.vcomponents.LUT6
    generic map(
      INIT => X"C3FFFFAA000000AA"
    )
        port map (
      I0 => go,
      I1 => \^got_it_tg_ff_reg\,
      I2 => got_it_tg_ff_reg_0,
      I3 => \state_reg[0]\,
      I4 => \state_reg[1]\,
      I5 => busy,
      O => busy_reg
    );
\cdc_reg[0]\: unisim.vcomponents.FDCE
     port map (
      C => dram_clk,
      CE => '1',
      CLR => AR(0),
      D => got_it_tg,
      Q => \cdc_reg_n_0_[0]\
    );
\output_reg[0]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => '1',
      CLR => AR(0),
      D => \sync1_reg_n_0_[0]\,
      Q => \^got_it_tg_ff_reg\
    );
state_i_1: unisim.vcomponents.LUT4
    generic map(
      INIT => X"FF90"
    )
        port map (
      I0 => \^got_it_tg_ff_reg\,
      I1 => got_it_tg_ff_reg_0,
      I2 => state_0,
      I3 => handshake_go,
      O => state_reg
    );
\sync1_reg[0]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => '1',
      CLR => AR(0),
      D => \cdc_reg_n_0_[0]\,
      Q => \sync1_reg_n_0_[0]\
    );
take_it_tg_i_1: unisim.vcomponents.LUT5
    generic map(
      INIT => X"D55D2AA2"
    )
        port map (
      I0 => handshake_go,
      I1 => state_0,
      I2 => got_it_tg_ff_reg_0,
      I3 => \^got_it_tg_ff_reg\,
      I4 => take_it_tg,
      O => take_it_tg_reg
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_synchronizer_dual_flop_0 is
  port (
    take_it_tg_sync : out STD_LOGIC;
    \addr_current_reg[0]\ : out STD_LOGIC;
    dram_rd_en : out STD_LOGIC;
    dram_rd_flush : out STD_LOGIC;
    got_it_tg_reg : out STD_LOGIC;
    state_reg : out STD_LOGIC;
    take_it_tg : in STD_LOGIC;
    user_clk : in STD_LOGIC;
    AR : in STD_LOGIC_VECTOR ( 0 to 0 );
    dram_clk : in STD_LOGIC;
    dram_ready : in STD_LOGIC;
    stall : in STD_LOGIC;
    prog_full : in STD_LOGIC;
    take_it_tg_ff : in STD_LOGIC;
    state : in STD_LOGIC;
    got_it_tg : in STD_LOGIC
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_synchronizer_dual_flop_0 : entity is "synchronizer_dual_flop";
end dram_rd_ram0_0_synchronizer_dual_flop_0;

architecture STRUCTURE of dram_rd_ram0_0_synchronizer_dual_flop_0 is
  signal cdc : STD_LOGIC;
  signal sync1 : STD_LOGIC;
  signal \^take_it_tg_sync\ : STD_LOGIC;
  attribute SOFT_HLUTNM : string;
  attribute SOFT_HLUTNM of dram_rd_flush_INST_0 : label is "soft_lutpair18";
  attribute SOFT_HLUTNM of \state_i_1__0\ : label is "soft_lutpair18";
begin
  take_it_tg_sync <= \^take_it_tg_sync\;
\addr_current[0]_i_1\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"02000002FFFFFFFF"
    )
        port map (
      I0 => dram_ready,
      I1 => stall,
      I2 => prog_full,
      I3 => take_it_tg_ff,
      I4 => \^take_it_tg_sync\,
      I5 => state,
      O => \addr_current_reg[0]\
    );
\cdc_reg[0]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => '1',
      CLR => AR(0),
      D => take_it_tg,
      Q => cdc
    );
dram_rd_en_INST_0: unisim.vcomponents.LUT6
    generic map(
      INIT => X"0008000000000008"
    )
        port map (
      I0 => state,
      I1 => dram_ready,
      I2 => stall,
      I3 => prog_full,
      I4 => take_it_tg_ff,
      I5 => \^take_it_tg_sync\,
      O => dram_rd_en
    );
dram_rd_flush_INST_0: unisim.vcomponents.LUT2
    generic map(
      INIT => X"6"
    )
        port map (
      I0 => take_it_tg_ff,
      I1 => \^take_it_tg_sync\,
      O => dram_rd_flush
    );
got_it_tg_i_1: unisim.vcomponents.LUT3
    generic map(
      INIT => X"96"
    )
        port map (
      I0 => \^take_it_tg_sync\,
      I1 => take_it_tg_ff,
      I2 => got_it_tg,
      O => got_it_tg_reg
    );
\output_reg[0]\: unisim.vcomponents.FDCE
     port map (
      C => dram_clk,
      CE => '1',
      CLR => AR(0),
      D => sync1,
      Q => \^take_it_tg_sync\
    );
\state_i_1__0\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"96"
    )
        port map (
      I0 => take_it_tg_ff,
      I1 => \^take_it_tg_sync\,
      I2 => state,
      O => state_reg
    );
\sync1_reg[0]\: unisim.vcomponents.FDCE
     port map (
      C => dram_clk,
      CE => '1',
      CLR => AR(0),
      D => cdc,
      Q => sync1
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_synchronizer_ff is
  port (
    D : out STD_LOGIC_VECTOR ( 5 downto 0 );
    Q : in STD_LOGIC_VECTOR ( 5 downto 0 );
    rd_clk : in STD_LOGIC;
    \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\ : in STD_LOGIC_VECTOR ( 0 to 0 )
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_synchronizer_ff : entity is "synchronizer_ff";
end dram_rd_ram0_0_synchronizer_ff;

architecture STRUCTURE of dram_rd_ram0_0_synchronizer_ff is
  signal Q_reg : STD_LOGIC_VECTOR ( 5 downto 0 );
  attribute ASYNC_REG : boolean;
  attribute ASYNC_REG of \Q_reg_reg[0]\ : label is std.standard.true;
  attribute KEEP : string;
  attribute KEEP of \Q_reg_reg[0]\ : label is "yes";
  attribute ASYNC_REG of \Q_reg_reg[1]\ : label is std.standard.true;
  attribute KEEP of \Q_reg_reg[1]\ : label is "yes";
  attribute ASYNC_REG of \Q_reg_reg[2]\ : label is std.standard.true;
  attribute KEEP of \Q_reg_reg[2]\ : label is "yes";
  attribute ASYNC_REG of \Q_reg_reg[3]\ : label is std.standard.true;
  attribute KEEP of \Q_reg_reg[3]\ : label is "yes";
  attribute ASYNC_REG of \Q_reg_reg[4]\ : label is std.standard.true;
  attribute KEEP of \Q_reg_reg[4]\ : label is "yes";
  attribute ASYNC_REG of \Q_reg_reg[5]\ : label is std.standard.true;
  attribute KEEP of \Q_reg_reg[5]\ : label is "yes";
begin
  D(5 downto 0) <= Q_reg(5 downto 0);
\Q_reg_reg[0]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0),
      D => Q(0),
      Q => Q_reg(0)
    );
\Q_reg_reg[1]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0),
      D => Q(1),
      Q => Q_reg(1)
    );
\Q_reg_reg[2]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0),
      D => Q(2),
      Q => Q_reg(2)
    );
\Q_reg_reg[3]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0),
      D => Q(3),
      Q => Q_reg(3)
    );
\Q_reg_reg[4]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0),
      D => Q(4),
      Q => Q_reg(4)
    );
\Q_reg_reg[5]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0),
      D => Q(5),
      Q => Q_reg(5)
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_synchronizer_ff_1 is
  port (
    D : out STD_LOGIC_VECTOR ( 5 downto 0 );
    Q : in STD_LOGIC_VECTOR ( 5 downto 0 );
    wr_clk : in STD_LOGIC;
    \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\ : in STD_LOGIC_VECTOR ( 0 to 0 )
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_synchronizer_ff_1 : entity is "synchronizer_ff";
end dram_rd_ram0_0_synchronizer_ff_1;

architecture STRUCTURE of dram_rd_ram0_0_synchronizer_ff_1 is
  signal Q_reg : STD_LOGIC_VECTOR ( 5 downto 0 );
  attribute ASYNC_REG : boolean;
  attribute ASYNC_REG of \Q_reg_reg[0]\ : label is std.standard.true;
  attribute KEEP : string;
  attribute KEEP of \Q_reg_reg[0]\ : label is "yes";
  attribute ASYNC_REG of \Q_reg_reg[1]\ : label is std.standard.true;
  attribute KEEP of \Q_reg_reg[1]\ : label is "yes";
  attribute ASYNC_REG of \Q_reg_reg[2]\ : label is std.standard.true;
  attribute KEEP of \Q_reg_reg[2]\ : label is "yes";
  attribute ASYNC_REG of \Q_reg_reg[3]\ : label is std.standard.true;
  attribute KEEP of \Q_reg_reg[3]\ : label is "yes";
  attribute ASYNC_REG of \Q_reg_reg[4]\ : label is std.standard.true;
  attribute KEEP of \Q_reg_reg[4]\ : label is "yes";
  attribute ASYNC_REG of \Q_reg_reg[5]\ : label is std.standard.true;
  attribute KEEP of \Q_reg_reg[5]\ : label is "yes";
begin
  D(5 downto 0) <= Q_reg(5 downto 0);
\Q_reg_reg[0]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0),
      D => Q(0),
      Q => Q_reg(0)
    );
\Q_reg_reg[1]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0),
      D => Q(1),
      Q => Q_reg(1)
    );
\Q_reg_reg[2]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0),
      D => Q(2),
      Q => Q_reg(2)
    );
\Q_reg_reg[3]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0),
      D => Q(3),
      Q => Q_reg(3)
    );
\Q_reg_reg[4]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0),
      D => Q(4),
      Q => Q_reg(4)
    );
\Q_reg_reg[5]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0),
      D => Q(5),
      Q => Q_reg(5)
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_synchronizer_ff_2 is
  port (
    \out\ : out STD_LOGIC_VECTOR ( 0 to 0 );
    \wr_pntr_bin_reg[4]\ : out STD_LOGIC_VECTOR ( 4 downto 0 );
    D : in STD_LOGIC_VECTOR ( 5 downto 0 );
    rd_clk : in STD_LOGIC;
    \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\ : in STD_LOGIC_VECTOR ( 0 to 0 )
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_synchronizer_ff_2 : entity is "synchronizer_ff";
end dram_rd_ram0_0_synchronizer_ff_2;

architecture STRUCTURE of dram_rd_ram0_0_synchronizer_ff_2 is
  signal Q_reg : STD_LOGIC_VECTOR ( 5 downto 0 );
  attribute ASYNC_REG : boolean;
  attribute ASYNC_REG of \Q_reg_reg[0]\ : label is std.standard.true;
  attribute KEEP : string;
  attribute KEEP of \Q_reg_reg[0]\ : label is "yes";
  attribute ASYNC_REG of \Q_reg_reg[1]\ : label is std.standard.true;
  attribute KEEP of \Q_reg_reg[1]\ : label is "yes";
  attribute ASYNC_REG of \Q_reg_reg[2]\ : label is std.standard.true;
  attribute KEEP of \Q_reg_reg[2]\ : label is "yes";
  attribute ASYNC_REG of \Q_reg_reg[3]\ : label is std.standard.true;
  attribute KEEP of \Q_reg_reg[3]\ : label is "yes";
  attribute ASYNC_REG of \Q_reg_reg[4]\ : label is std.standard.true;
  attribute KEEP of \Q_reg_reg[4]\ : label is "yes";
  attribute ASYNC_REG of \Q_reg_reg[5]\ : label is std.standard.true;
  attribute KEEP of \Q_reg_reg[5]\ : label is "yes";
begin
  \out\(0) <= Q_reg(5);
\Q_reg_reg[0]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0),
      D => D(0),
      Q => Q_reg(0)
    );
\Q_reg_reg[1]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0),
      D => D(1),
      Q => Q_reg(1)
    );
\Q_reg_reg[2]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0),
      D => D(2),
      Q => Q_reg(2)
    );
\Q_reg_reg[3]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0),
      D => D(3),
      Q => Q_reg(3)
    );
\Q_reg_reg[4]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0),
      D => D(4),
      Q => Q_reg(4)
    );
\Q_reg_reg[5]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0),
      D => D(5),
      Q => Q_reg(5)
    );
\wr_pntr_bin[0]_i_1\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"6996966996696996"
    )
        port map (
      I0 => Q_reg(2),
      I1 => Q_reg(0),
      I2 => Q_reg(1),
      I3 => Q_reg(5),
      I4 => Q_reg(3),
      I5 => Q_reg(4),
      O => \wr_pntr_bin_reg[4]\(0)
    );
\wr_pntr_bin[1]_i_1\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"96696996"
    )
        port map (
      I0 => Q_reg(3),
      I1 => Q_reg(1),
      I2 => Q_reg(2),
      I3 => Q_reg(5),
      I4 => Q_reg(4),
      O => \wr_pntr_bin_reg[4]\(1)
    );
\wr_pntr_bin[2]_i_1\: unisim.vcomponents.LUT4
    generic map(
      INIT => X"6996"
    )
        port map (
      I0 => Q_reg(3),
      I1 => Q_reg(2),
      I2 => Q_reg(5),
      I3 => Q_reg(4),
      O => \wr_pntr_bin_reg[4]\(2)
    );
\wr_pntr_bin[3]_i_1\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"96"
    )
        port map (
      I0 => Q_reg(4),
      I1 => Q_reg(3),
      I2 => Q_reg(5),
      O => \wr_pntr_bin_reg[4]\(3)
    );
\wr_pntr_bin[4]_i_1\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"6"
    )
        port map (
      I0 => Q_reg(4),
      I1 => Q_reg(5),
      O => \wr_pntr_bin_reg[4]\(4)
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_synchronizer_ff_3 is
  port (
    \out\ : out STD_LOGIC_VECTOR ( 0 to 0 );
    \rd_pntr_bin_reg[4]\ : out STD_LOGIC_VECTOR ( 4 downto 0 );
    D : in STD_LOGIC_VECTOR ( 5 downto 0 );
    wr_clk : in STD_LOGIC;
    \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\ : in STD_LOGIC_VECTOR ( 0 to 0 )
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_synchronizer_ff_3 : entity is "synchronizer_ff";
end dram_rd_ram0_0_synchronizer_ff_3;

architecture STRUCTURE of dram_rd_ram0_0_synchronizer_ff_3 is
  signal Q_reg : STD_LOGIC_VECTOR ( 5 downto 0 );
  attribute ASYNC_REG : boolean;
  attribute ASYNC_REG of \Q_reg_reg[0]\ : label is std.standard.true;
  attribute KEEP : string;
  attribute KEEP of \Q_reg_reg[0]\ : label is "yes";
  attribute ASYNC_REG of \Q_reg_reg[1]\ : label is std.standard.true;
  attribute KEEP of \Q_reg_reg[1]\ : label is "yes";
  attribute ASYNC_REG of \Q_reg_reg[2]\ : label is std.standard.true;
  attribute KEEP of \Q_reg_reg[2]\ : label is "yes";
  attribute ASYNC_REG of \Q_reg_reg[3]\ : label is std.standard.true;
  attribute KEEP of \Q_reg_reg[3]\ : label is "yes";
  attribute ASYNC_REG of \Q_reg_reg[4]\ : label is std.standard.true;
  attribute KEEP of \Q_reg_reg[4]\ : label is "yes";
  attribute ASYNC_REG of \Q_reg_reg[5]\ : label is std.standard.true;
  attribute KEEP of \Q_reg_reg[5]\ : label is "yes";
begin
  \out\(0) <= Q_reg(5);
\Q_reg_reg[0]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0),
      D => D(0),
      Q => Q_reg(0)
    );
\Q_reg_reg[1]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0),
      D => D(1),
      Q => Q_reg(1)
    );
\Q_reg_reg[2]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0),
      D => D(2),
      Q => Q_reg(2)
    );
\Q_reg_reg[3]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0),
      D => D(3),
      Q => Q_reg(3)
    );
\Q_reg_reg[4]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0),
      D => D(4),
      Q => Q_reg(4)
    );
\Q_reg_reg[5]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0),
      D => D(5),
      Q => Q_reg(5)
    );
\rd_pntr_bin[0]_i_1\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"6996966996696996"
    )
        port map (
      I0 => Q_reg(2),
      I1 => Q_reg(0),
      I2 => Q_reg(1),
      I3 => Q_reg(5),
      I4 => Q_reg(3),
      I5 => Q_reg(4),
      O => \rd_pntr_bin_reg[4]\(0)
    );
\rd_pntr_bin[1]_i_1\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"96696996"
    )
        port map (
      I0 => Q_reg(3),
      I1 => Q_reg(1),
      I2 => Q_reg(2),
      I3 => Q_reg(5),
      I4 => Q_reg(4),
      O => \rd_pntr_bin_reg[4]\(1)
    );
\rd_pntr_bin[2]_i_1\: unisim.vcomponents.LUT4
    generic map(
      INIT => X"6996"
    )
        port map (
      I0 => Q_reg(3),
      I1 => Q_reg(2),
      I2 => Q_reg(5),
      I3 => Q_reg(4),
      O => \rd_pntr_bin_reg[4]\(2)
    );
\rd_pntr_bin[3]_i_1\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"96"
    )
        port map (
      I0 => Q_reg(4),
      I1 => Q_reg(3),
      I2 => Q_reg(5),
      O => \rd_pntr_bin_reg[4]\(3)
    );
\rd_pntr_bin[4]_i_1\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"6"
    )
        port map (
      I0 => Q_reg(4),
      I1 => Q_reg(5),
      O => \rd_pntr_bin_reg[4]\(4)
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_wr_bin_cntr is
  port (
    ram_full_fb_i_reg : out STD_LOGIC;
    \gdiff.diff_pntr_pad_reg[6]\ : out STD_LOGIC_VECTOR ( 2 downto 0 );
    Q : out STD_LOGIC_VECTOR ( 5 downto 0 );
    S : out STD_LOGIC_VECTOR ( 2 downto 0 );
    \DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram\ : out STD_LOGIC_VECTOR ( 5 downto 0 );
    rst_full_gen_i : in STD_LOGIC;
    \rd_pntr_bin_reg[5]\ : in STD_LOGIC_VECTOR ( 5 downto 0 );
    \gic0.gc0.count_d1_reg[4]_0\ : in STD_LOGIC;
    wr_en : in STD_LOGIC;
    p_1_out : in STD_LOGIC;
    E : in STD_LOGIC_VECTOR ( 0 to 0 );
    wr_clk : in STD_LOGIC;
    \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\ : in STD_LOGIC_VECTOR ( 0 to 0 )
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_wr_bin_cntr : entity is "wr_bin_cntr";
end dram_rd_ram0_0_wr_bin_cntr;

architecture STRUCTURE of dram_rd_ram0_0_wr_bin_cntr is
  signal \^q\ : STD_LOGIC_VECTOR ( 5 downto 0 );
  signal \plusOp__1\ : STD_LOGIC_VECTOR ( 5 downto 0 );
  signal ram_full_i_i_2_n_0 : STD_LOGIC;
  signal ram_full_i_i_3_n_0 : STD_LOGIC;
  signal wr_pntr_plus2 : STD_LOGIC_VECTOR ( 5 downto 0 );
  attribute SOFT_HLUTNM : string;
  attribute SOFT_HLUTNM of \gic0.gc0.count[0]_i_1\ : label is "soft_lutpair9";
  attribute SOFT_HLUTNM of \gic0.gc0.count[2]_i_1\ : label is "soft_lutpair9";
  attribute SOFT_HLUTNM of \gic0.gc0.count[3]_i_1\ : label is "soft_lutpair8";
  attribute SOFT_HLUTNM of \gic0.gc0.count[4]_i_1\ : label is "soft_lutpair8";
begin
  Q(5 downto 0) <= \^q\(5 downto 0);
\gdiff.diff_pntr_pad[6]_i_3\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"9"
    )
        port map (
      I0 => \^q\(5),
      I1 => \rd_pntr_bin_reg[5]\(5),
      O => \gdiff.diff_pntr_pad_reg[6]\(2)
    );
\gdiff.diff_pntr_pad[6]_i_4\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"9"
    )
        port map (
      I0 => \^q\(4),
      I1 => \rd_pntr_bin_reg[5]\(4),
      O => \gdiff.diff_pntr_pad_reg[6]\(1)
    );
\gdiff.diff_pntr_pad[6]_i_5\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"9"
    )
        port map (
      I0 => \^q\(3),
      I1 => \rd_pntr_bin_reg[5]\(3),
      O => \gdiff.diff_pntr_pad_reg[6]\(0)
    );
\gdiff.diff_pntr_pad[6]_i_7\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"9"
    )
        port map (
      I0 => \^q\(2),
      I1 => \rd_pntr_bin_reg[5]\(2),
      O => S(2)
    );
\gdiff.diff_pntr_pad[6]_i_8\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"9"
    )
        port map (
      I0 => \^q\(1),
      I1 => \rd_pntr_bin_reg[5]\(1),
      O => S(1)
    );
\gdiff.diff_pntr_pad[6]_i_9\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"9"
    )
        port map (
      I0 => \^q\(0),
      I1 => \rd_pntr_bin_reg[5]\(0),
      O => S(0)
    );
\gic0.gc0.count[0]_i_1\: unisim.vcomponents.LUT1
    generic map(
      INIT => X"1"
    )
        port map (
      I0 => wr_pntr_plus2(0),
      O => \plusOp__1\(0)
    );
\gic0.gc0.count[1]_i_1\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"6"
    )
        port map (
      I0 => wr_pntr_plus2(0),
      I1 => wr_pntr_plus2(1),
      O => \plusOp__1\(1)
    );
\gic0.gc0.count[2]_i_1\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"78"
    )
        port map (
      I0 => wr_pntr_plus2(0),
      I1 => wr_pntr_plus2(1),
      I2 => wr_pntr_plus2(2),
      O => \plusOp__1\(2)
    );
\gic0.gc0.count[3]_i_1\: unisim.vcomponents.LUT4
    generic map(
      INIT => X"6AAA"
    )
        port map (
      I0 => wr_pntr_plus2(3),
      I1 => wr_pntr_plus2(0),
      I2 => wr_pntr_plus2(1),
      I3 => wr_pntr_plus2(2),
      O => \plusOp__1\(3)
    );
\gic0.gc0.count[4]_i_1\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"6AAAAAAA"
    )
        port map (
      I0 => wr_pntr_plus2(4),
      I1 => wr_pntr_plus2(2),
      I2 => wr_pntr_plus2(1),
      I3 => wr_pntr_plus2(0),
      I4 => wr_pntr_plus2(3),
      O => \plusOp__1\(4)
    );
\gic0.gc0.count[5]_i_1\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"6AAAAAAAAAAAAAAA"
    )
        port map (
      I0 => wr_pntr_plus2(5),
      I1 => wr_pntr_plus2(3),
      I2 => wr_pntr_plus2(0),
      I3 => wr_pntr_plus2(1),
      I4 => wr_pntr_plus2(2),
      I5 => wr_pntr_plus2(4),
      O => \plusOp__1\(5)
    );
\gic0.gc0.count_d1_reg[0]\: unisim.vcomponents.FDPE
    generic map(
      INIT => '1'
    )
        port map (
      C => wr_clk,
      CE => E(0),
      D => wr_pntr_plus2(0),
      PRE => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\(0),
      Q => \^q\(0)
    );
\gic0.gc0.count_d1_reg[1]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\(0),
      D => wr_pntr_plus2(1),
      Q => \^q\(1)
    );
\gic0.gc0.count_d1_reg[2]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\(0),
      D => wr_pntr_plus2(2),
      Q => \^q\(2)
    );
\gic0.gc0.count_d1_reg[3]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\(0),
      D => wr_pntr_plus2(3),
      Q => \^q\(3)
    );
\gic0.gc0.count_d1_reg[4]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\(0),
      D => wr_pntr_plus2(4),
      Q => \^q\(4)
    );
\gic0.gc0.count_d1_reg[5]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\(0),
      D => wr_pntr_plus2(5),
      Q => \^q\(5)
    );
\gic0.gc0.count_d2_reg[0]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\(0),
      D => \^q\(0),
      Q => \DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram\(0)
    );
\gic0.gc0.count_d2_reg[1]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\(0),
      D => \^q\(1),
      Q => \DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram\(1)
    );
\gic0.gc0.count_d2_reg[2]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\(0),
      D => \^q\(2),
      Q => \DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram\(2)
    );
\gic0.gc0.count_d2_reg[3]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\(0),
      D => \^q\(3),
      Q => \DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram\(3)
    );
\gic0.gc0.count_d2_reg[4]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\(0),
      D => \^q\(4),
      Q => \DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram\(4)
    );
\gic0.gc0.count_d2_reg[5]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\(0),
      D => \^q\(5),
      Q => \DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram\(5)
    );
\gic0.gc0.count_reg[0]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\(0),
      D => \plusOp__1\(0),
      Q => wr_pntr_plus2(0)
    );
\gic0.gc0.count_reg[1]\: unisim.vcomponents.FDPE
    generic map(
      INIT => '1'
    )
        port map (
      C => wr_clk,
      CE => E(0),
      D => \plusOp__1\(1),
      PRE => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\(0),
      Q => wr_pntr_plus2(1)
    );
\gic0.gc0.count_reg[2]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\(0),
      D => \plusOp__1\(2),
      Q => wr_pntr_plus2(2)
    );
\gic0.gc0.count_reg[3]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\(0),
      D => \plusOp__1\(3),
      Q => wr_pntr_plus2(3)
    );
\gic0.gc0.count_reg[4]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\(0),
      D => \plusOp__1\(4),
      Q => wr_pntr_plus2(4)
    );
\gic0.gc0.count_reg[5]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => E(0),
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\(0),
      D => \plusOp__1\(5),
      Q => wr_pntr_plus2(5)
    );
ram_full_i_i_1: unisim.vcomponents.LUT6
    generic map(
      INIT => X"4000004055555555"
    )
        port map (
      I0 => rst_full_gen_i,
      I1 => ram_full_i_i_2_n_0,
      I2 => ram_full_i_i_3_n_0,
      I3 => \rd_pntr_bin_reg[5]\(0),
      I4 => wr_pntr_plus2(0),
      I5 => \gic0.gc0.count_d1_reg[4]_0\,
      O => ram_full_fb_i_reg
    );
ram_full_i_i_2: unisim.vcomponents.LUT6
    generic map(
      INIT => X"0090000000000090"
    )
        port map (
      I0 => wr_pntr_plus2(5),
      I1 => \rd_pntr_bin_reg[5]\(5),
      I2 => wr_en,
      I3 => p_1_out,
      I4 => \rd_pntr_bin_reg[5]\(4),
      I5 => wr_pntr_plus2(4),
      O => ram_full_i_i_2_n_0
    );
ram_full_i_i_3: unisim.vcomponents.LUT6
    generic map(
      INIT => X"9009000000009009"
    )
        port map (
      I0 => wr_pntr_plus2(2),
      I1 => \rd_pntr_bin_reg[5]\(2),
      I2 => wr_pntr_plus2(3),
      I3 => \rd_pntr_bin_reg[5]\(3),
      I4 => \rd_pntr_bin_reg[5]\(1),
      I5 => wr_pntr_plus2(1),
      O => ram_full_i_i_3_n_0
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_wr_pf_as is
  port (
    prog_full : out STD_LOGIC;
    wr_clk : in STD_LOGIC;
    rst_full_ff_i : in STD_LOGIC;
    rst_full_gen_i : in STD_LOGIC;
    p_1_out : in STD_LOGIC;
    \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\ : in STD_LOGIC_VECTOR ( 0 to 0 );
    wr_pntr_plus1_pad : in STD_LOGIC_VECTOR ( 5 downto 0 );
    S : in STD_LOGIC_VECTOR ( 2 downto 0 );
    \gic0.gc0.count_d1_reg[5]\ : in STD_LOGIC_VECTOR ( 2 downto 0 )
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_wr_pf_as : entity is "wr_pf_as";
end dram_rd_ram0_0_wr_pf_as;

architecture STRUCTURE of dram_rd_ram0_0_wr_pf_as is
  signal diff_pntr : STD_LOGIC_VECTOR ( 5 downto 4 );
  signal \gdiff.diff_pntr_pad_reg[6]_i_1_n_2\ : STD_LOGIC;
  signal \gdiff.diff_pntr_pad_reg[6]_i_1_n_3\ : STD_LOGIC;
  signal \gdiff.diff_pntr_pad_reg[6]_i_2_n_0\ : STD_LOGIC;
  signal \gdiff.diff_pntr_pad_reg[6]_i_2_n_1\ : STD_LOGIC;
  signal \gdiff.diff_pntr_pad_reg[6]_i_2_n_2\ : STD_LOGIC;
  signal \gdiff.diff_pntr_pad_reg[6]_i_2_n_3\ : STD_LOGIC;
  signal \gpf1.prog_full_i_i_1_n_0\ : STD_LOGIC;
  signal plusOp : STD_LOGIC_VECTOR ( 6 downto 0 );
  signal \^prog_full\ : STD_LOGIC;
  signal \NLW_gdiff.diff_pntr_pad_reg[6]_i_1_CO_UNCONNECTED\ : STD_LOGIC_VECTOR ( 3 downto 2 );
  signal \NLW_gdiff.diff_pntr_pad_reg[6]_i_1_O_UNCONNECTED\ : STD_LOGIC_VECTOR ( 3 to 3 );
begin
  prog_full <= \^prog_full\;
\gdiff.diff_pntr_pad_reg[5]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\(0),
      D => plusOp(5),
      Q => diff_pntr(4)
    );
\gdiff.diff_pntr_pad_reg[6]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\(0),
      D => plusOp(6),
      Q => diff_pntr(5)
    );
\gdiff.diff_pntr_pad_reg[6]_i_1\: unisim.vcomponents.CARRY4
     port map (
      CI => \gdiff.diff_pntr_pad_reg[6]_i_2_n_0\,
      CO(3 downto 2) => \NLW_gdiff.diff_pntr_pad_reg[6]_i_1_CO_UNCONNECTED\(3 downto 2),
      CO(1) => \gdiff.diff_pntr_pad_reg[6]_i_1_n_2\,
      CO(0) => \gdiff.diff_pntr_pad_reg[6]_i_1_n_3\,
      CYINIT => '0',
      DI(3) => '0',
      DI(2) => '0',
      DI(1 downto 0) => wr_pntr_plus1_pad(5 downto 4),
      O(3) => \NLW_gdiff.diff_pntr_pad_reg[6]_i_1_O_UNCONNECTED\(3),
      O(2 downto 0) => plusOp(6 downto 4),
      S(3) => '0',
      S(2 downto 0) => \gic0.gc0.count_d1_reg[5]\(2 downto 0)
    );
\gdiff.diff_pntr_pad_reg[6]_i_2\: unisim.vcomponents.CARRY4
     port map (
      CI => '0',
      CO(3) => \gdiff.diff_pntr_pad_reg[6]_i_2_n_0\,
      CO(2) => \gdiff.diff_pntr_pad_reg[6]_i_2_n_1\,
      CO(1) => \gdiff.diff_pntr_pad_reg[6]_i_2_n_2\,
      CO(0) => \gdiff.diff_pntr_pad_reg[6]_i_2_n_3\,
      CYINIT => '0',
      DI(3 downto 0) => wr_pntr_plus1_pad(3 downto 0),
      O(3 downto 0) => plusOp(3 downto 0),
      S(3 downto 1) => S(2 downto 0),
      S(0) => '0'
    );
\gpf1.prog_full_i_i_1\: unisim.vcomponents.LUT5
    generic map(
      INIT => X"0F080008"
    )
        port map (
      I0 => diff_pntr(4),
      I1 => diff_pntr(5),
      I2 => rst_full_gen_i,
      I3 => p_1_out,
      I4 => \^prog_full\,
      O => \gpf1.prog_full_i_i_1_n_0\
    );
\gpf1.prog_full_i_reg\: unisim.vcomponents.FDPE
    generic map(
      INIT => '1'
    )
        port map (
      C => wr_clk,
      CE => '1',
      D => \gpf1.prog_full_i_i_1_n_0\,
      PRE => rst_full_ff_i,
      Q => \^prog_full\
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_wr_status_flags_as is
  port (
    full : out STD_LOGIC;
    p_1_out : out STD_LOGIC;
    E : out STD_LOGIC_VECTOR ( 0 to 0 );
    wr_pntr_plus1_pad : out STD_LOGIC_VECTOR ( 0 to 0 );
    \grstd1.grst_full.grst_f.RST_FULL_GEN_reg\ : in STD_LOGIC;
    wr_clk : in STD_LOGIC;
    rst_full_ff_i : in STD_LOGIC;
    wr_en : in STD_LOGIC
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_wr_status_flags_as : entity is "wr_status_flags_as";
end dram_rd_ram0_0_wr_status_flags_as;

architecture STRUCTURE of dram_rd_ram0_0_wr_status_flags_as is
  signal \^p_1_out\ : STD_LOGIC;
  attribute equivalent_register_removal : string;
  attribute equivalent_register_removal of ram_full_fb_i_reg : label is "no";
  attribute equivalent_register_removal of ram_full_i_reg : label is "no";
begin
  p_1_out <= \^p_1_out\;
\DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram_i_2\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"2"
    )
        port map (
      I0 => wr_en,
      I1 => \^p_1_out\,
      O => E(0)
    );
\gdiff.diff_pntr_pad[6]_i_6\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"2"
    )
        port map (
      I0 => wr_en,
      I1 => \^p_1_out\,
      O => wr_pntr_plus1_pad(0)
    );
ram_full_fb_i_reg: unisim.vcomponents.FDPE
    generic map(
      INIT => '1'
    )
        port map (
      C => wr_clk,
      CE => '1',
      D => \grstd1.grst_full.grst_f.RST_FULL_GEN_reg\,
      PRE => rst_full_ff_i,
      Q => \^p_1_out\
    );
ram_full_i_reg: unisim.vcomponents.FDPE
    generic map(
      INIT => '1'
    )
        port map (
      C => wr_clk,
      CE => '1',
      D => \grstd1.grst_full.grst_f.RST_FULL_GEN_reg\,
      PRE => rst_full_ff_i,
      Q => full
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_blk_mem_gen_prim_width is
  port (
    D : out STD_LOGIC_VECTOR ( 31 downto 0 );
    rd_clk : in STD_LOGIC;
    wr_clk : in STD_LOGIC;
    tmp_ram_rd_en : in STD_LOGIC;
    E : in STD_LOGIC_VECTOR ( 0 to 0 );
    Q : in STD_LOGIC_VECTOR ( 0 to 0 );
    \gc0.count_d1_reg[5]\ : in STD_LOGIC_VECTOR ( 5 downto 0 );
    \gic0.gc0.count_d2_reg[5]\ : in STD_LOGIC_VECTOR ( 5 downto 0 );
    din : in STD_LOGIC_VECTOR ( 31 downto 0 )
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_blk_mem_gen_prim_width : entity is "blk_mem_gen_prim_width";
end dram_rd_ram0_0_blk_mem_gen_prim_width;

architecture STRUCTURE of dram_rd_ram0_0_blk_mem_gen_prim_width is
begin
\prim_noinit.ram\: entity work.dram_rd_ram0_0_blk_mem_gen_prim_wrapper
     port map (
      D(31 downto 0) => D(31 downto 0),
      E(0) => E(0),
      Q(0) => Q(0),
      din(31 downto 0) => din(31 downto 0),
      \gc0.count_d1_reg[5]\(5 downto 0) => \gc0.count_d1_reg[5]\(5 downto 0),
      \gic0.gc0.count_d2_reg[5]\(5 downto 0) => \gic0.gc0.count_d2_reg[5]\(5 downto 0),
      rd_clk => rd_clk,
      tmp_ram_rd_en => tmp_ram_rd_en,
      wr_clk => wr_clk
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_clk_x_pntrs is
  port (
    ram_empty_fb_i_reg : out STD_LOGIC;
    Q : out STD_LOGIC_VECTOR ( 3 downto 0 );
    ram_empty_fb_i_reg_0 : out STD_LOGIC;
    ram_full_fb_i_reg : out STD_LOGIC;
    ram_full_fb_i_reg_0 : out STD_LOGIC_VECTOR ( 5 downto 0 );
    \gc0.count_reg[4]\ : in STD_LOGIC_VECTOR ( 1 downto 0 );
    \gc0.count_d1_reg[5]\ : in STD_LOGIC_VECTOR ( 5 downto 0 );
    \gic0.gc0.count_d1_reg[5]\ : in STD_LOGIC_VECTOR ( 5 downto 0 );
    \gic0.gc0.count_d2_reg[5]\ : in STD_LOGIC_VECTOR ( 5 downto 0 );
    wr_clk : in STD_LOGIC;
    \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\ : in STD_LOGIC_VECTOR ( 0 to 0 );
    rd_clk : in STD_LOGIC;
    \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\ : in STD_LOGIC_VECTOR ( 0 to 0 );
    D : in STD_LOGIC_VECTOR ( 4 downto 0 )
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_clk_x_pntrs : entity is "clk_x_pntrs";
end dram_rd_ram0_0_clk_x_pntrs;

architecture STRUCTURE of dram_rd_ram0_0_clk_x_pntrs is
  signal \^q\ : STD_LOGIC_VECTOR ( 3 downto 0 );
  signal \gsync_stage[2].wr_stg_inst_n_1\ : STD_LOGIC;
  signal \gsync_stage[2].wr_stg_inst_n_2\ : STD_LOGIC;
  signal \gsync_stage[2].wr_stg_inst_n_3\ : STD_LOGIC;
  signal \gsync_stage[2].wr_stg_inst_n_4\ : STD_LOGIC;
  signal \gsync_stage[2].wr_stg_inst_n_5\ : STD_LOGIC;
  signal p_0_in : STD_LOGIC_VECTOR ( 4 downto 0 );
  signal p_0_in4_out : STD_LOGIC_VECTOR ( 4 downto 0 );
  signal p_0_out : STD_LOGIC_VECTOR ( 5 to 5 );
  signal p_1_out : STD_LOGIC_VECTOR ( 4 downto 2 );
  signal p_1_out_0 : STD_LOGIC_VECTOR ( 5 to 5 );
  signal p_2_out : STD_LOGIC_VECTOR ( 5 downto 0 );
  signal p_3_out : STD_LOGIC_VECTOR ( 5 downto 0 );
  signal ram_empty_fb_i_i_6_n_0 : STD_LOGIC;
  signal ram_empty_fb_i_i_7_n_0 : STD_LOGIC;
  signal \^ram_full_fb_i_reg_0\ : STD_LOGIC_VECTOR ( 5 downto 0 );
  signal ram_full_i_i_5_n_0 : STD_LOGIC;
  signal ram_full_i_i_6_n_0 : STD_LOGIC;
  signal rd_pntr_gc : STD_LOGIC_VECTOR ( 5 downto 0 );
  signal wr_pntr_gc : STD_LOGIC_VECTOR ( 5 downto 0 );
  attribute SOFT_HLUTNM : string;
  attribute SOFT_HLUTNM of \wr_pntr_gc[0]_i_1\ : label is "soft_lutpair0";
  attribute SOFT_HLUTNM of \wr_pntr_gc[1]_i_1\ : label is "soft_lutpair0";
  attribute SOFT_HLUTNM of \wr_pntr_gc[2]_i_1\ : label is "soft_lutpair1";
  attribute SOFT_HLUTNM of \wr_pntr_gc[3]_i_1\ : label is "soft_lutpair1";
begin
  Q(3 downto 0) <= \^q\(3 downto 0);
  ram_full_fb_i_reg_0(5 downto 0) <= \^ram_full_fb_i_reg_0\(5 downto 0);
\gsync_stage[1].rd_stg_inst\: entity work.dram_rd_ram0_0_synchronizer_ff
     port map (
      D(5 downto 0) => p_3_out(5 downto 0),
      Q(5 downto 0) => wr_pntr_gc(5 downto 0),
      \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0) => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0),
      rd_clk => rd_clk
    );
\gsync_stage[1].wr_stg_inst\: entity work.dram_rd_ram0_0_synchronizer_ff_1
     port map (
      D(5 downto 0) => p_2_out(5 downto 0),
      Q(5 downto 0) => rd_pntr_gc(5 downto 0),
      \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0) => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0),
      wr_clk => wr_clk
    );
\gsync_stage[2].rd_stg_inst\: entity work.dram_rd_ram0_0_synchronizer_ff_2
     port map (
      D(5 downto 0) => p_3_out(5 downto 0),
      \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0) => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0),
      \out\(0) => p_1_out_0(5),
      rd_clk => rd_clk,
      \wr_pntr_bin_reg[4]\(4 downto 0) => p_0_in(4 downto 0)
    );
\gsync_stage[2].wr_stg_inst\: entity work.dram_rd_ram0_0_synchronizer_ff_3
     port map (
      D(5 downto 0) => p_2_out(5 downto 0),
      \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0) => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0),
      \out\(0) => p_0_out(5),
      \rd_pntr_bin_reg[4]\(4) => \gsync_stage[2].wr_stg_inst_n_1\,
      \rd_pntr_bin_reg[4]\(3) => \gsync_stage[2].wr_stg_inst_n_2\,
      \rd_pntr_bin_reg[4]\(2) => \gsync_stage[2].wr_stg_inst_n_3\,
      \rd_pntr_bin_reg[4]\(1) => \gsync_stage[2].wr_stg_inst_n_4\,
      \rd_pntr_bin_reg[4]\(0) => \gsync_stage[2].wr_stg_inst_n_5\,
      wr_clk => wr_clk
    );
ram_empty_fb_i_i_3: unisim.vcomponents.LUT6
    generic map(
      INIT => X"0000000000009009"
    )
        port map (
      I0 => \gc0.count_d1_reg[5]\(4),
      I1 => p_1_out(4),
      I2 => \gc0.count_d1_reg[5]\(5),
      I3 => \^q\(3),
      I4 => ram_empty_fb_i_i_6_n_0,
      I5 => ram_empty_fb_i_i_7_n_0,
      O => ram_empty_fb_i_reg_0
    );
ram_empty_fb_i_i_5: unisim.vcomponents.LUT4
    generic map(
      INIT => X"9009"
    )
        port map (
      I0 => p_1_out(2),
      I1 => \gc0.count_reg[4]\(0),
      I2 => p_1_out(4),
      I3 => \gc0.count_reg[4]\(1),
      O => ram_empty_fb_i_reg
    );
ram_empty_fb_i_i_6: unisim.vcomponents.LUT4
    generic map(
      INIT => X"6FF6"
    )
        port map (
      I0 => \^q\(1),
      I1 => \gc0.count_d1_reg[5]\(1),
      I2 => \^q\(0),
      I3 => \gc0.count_d1_reg[5]\(0),
      O => ram_empty_fb_i_i_6_n_0
    );
ram_empty_fb_i_i_7: unisim.vcomponents.LUT4
    generic map(
      INIT => X"6FF6"
    )
        port map (
      I0 => \^q\(2),
      I1 => \gc0.count_d1_reg[5]\(3),
      I2 => p_1_out(2),
      I3 => \gc0.count_d1_reg[5]\(2),
      O => ram_empty_fb_i_i_7_n_0
    );
ram_full_i_i_4: unisim.vcomponents.LUT6
    generic map(
      INIT => X"FFFFFFFFFFFF6FF6"
    )
        port map (
      I0 => \gic0.gc0.count_d1_reg[5]\(4),
      I1 => \^ram_full_fb_i_reg_0\(4),
      I2 => \gic0.gc0.count_d1_reg[5]\(5),
      I3 => \^ram_full_fb_i_reg_0\(5),
      I4 => ram_full_i_i_5_n_0,
      I5 => ram_full_i_i_6_n_0,
      O => ram_full_fb_i_reg
    );
ram_full_i_i_5: unisim.vcomponents.LUT4
    generic map(
      INIT => X"6FF6"
    )
        port map (
      I0 => \^ram_full_fb_i_reg_0\(1),
      I1 => \gic0.gc0.count_d1_reg[5]\(1),
      I2 => \^ram_full_fb_i_reg_0\(0),
      I3 => \gic0.gc0.count_d1_reg[5]\(0),
      O => ram_full_i_i_5_n_0
    );
ram_full_i_i_6: unisim.vcomponents.LUT4
    generic map(
      INIT => X"6FF6"
    )
        port map (
      I0 => \^ram_full_fb_i_reg_0\(3),
      I1 => \gic0.gc0.count_d1_reg[5]\(3),
      I2 => \^ram_full_fb_i_reg_0\(2),
      I3 => \gic0.gc0.count_d1_reg[5]\(2),
      O => ram_full_i_i_6_n_0
    );
\rd_pntr_bin_reg[0]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0),
      D => \gsync_stage[2].wr_stg_inst_n_5\,
      Q => \^ram_full_fb_i_reg_0\(0)
    );
\rd_pntr_bin_reg[1]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0),
      D => \gsync_stage[2].wr_stg_inst_n_4\,
      Q => \^ram_full_fb_i_reg_0\(1)
    );
\rd_pntr_bin_reg[2]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0),
      D => \gsync_stage[2].wr_stg_inst_n_3\,
      Q => \^ram_full_fb_i_reg_0\(2)
    );
\rd_pntr_bin_reg[3]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0),
      D => \gsync_stage[2].wr_stg_inst_n_2\,
      Q => \^ram_full_fb_i_reg_0\(3)
    );
\rd_pntr_bin_reg[4]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0),
      D => \gsync_stage[2].wr_stg_inst_n_1\,
      Q => \^ram_full_fb_i_reg_0\(4)
    );
\rd_pntr_bin_reg[5]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0),
      D => p_0_out(5),
      Q => \^ram_full_fb_i_reg_0\(5)
    );
\rd_pntr_gc_reg[0]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0),
      D => D(0),
      Q => rd_pntr_gc(0)
    );
\rd_pntr_gc_reg[1]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0),
      D => D(1),
      Q => rd_pntr_gc(1)
    );
\rd_pntr_gc_reg[2]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0),
      D => D(2),
      Q => rd_pntr_gc(2)
    );
\rd_pntr_gc_reg[3]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0),
      D => D(3),
      Q => rd_pntr_gc(3)
    );
\rd_pntr_gc_reg[4]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0),
      D => D(4),
      Q => rd_pntr_gc(4)
    );
\rd_pntr_gc_reg[5]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0),
      D => \gc0.count_d1_reg[5]\(5),
      Q => rd_pntr_gc(5)
    );
\wr_pntr_bin_reg[0]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0),
      D => p_0_in(0),
      Q => \^q\(0)
    );
\wr_pntr_bin_reg[1]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0),
      D => p_0_in(1),
      Q => \^q\(1)
    );
\wr_pntr_bin_reg[2]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0),
      D => p_0_in(2),
      Q => p_1_out(2)
    );
\wr_pntr_bin_reg[3]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0),
      D => p_0_in(3),
      Q => \^q\(2)
    );
\wr_pntr_bin_reg[4]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0),
      D => p_0_in(4),
      Q => p_1_out(4)
    );
\wr_pntr_bin_reg[5]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0),
      D => p_1_out_0(5),
      Q => \^q\(3)
    );
\wr_pntr_gc[0]_i_1\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"6"
    )
        port map (
      I0 => \gic0.gc0.count_d2_reg[5]\(0),
      I1 => \gic0.gc0.count_d2_reg[5]\(1),
      O => p_0_in4_out(0)
    );
\wr_pntr_gc[1]_i_1\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"6"
    )
        port map (
      I0 => \gic0.gc0.count_d2_reg[5]\(1),
      I1 => \gic0.gc0.count_d2_reg[5]\(2),
      O => p_0_in4_out(1)
    );
\wr_pntr_gc[2]_i_1\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"6"
    )
        port map (
      I0 => \gic0.gc0.count_d2_reg[5]\(2),
      I1 => \gic0.gc0.count_d2_reg[5]\(3),
      O => p_0_in4_out(2)
    );
\wr_pntr_gc[3]_i_1\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"6"
    )
        port map (
      I0 => \gic0.gc0.count_d2_reg[5]\(3),
      I1 => \gic0.gc0.count_d2_reg[5]\(4),
      O => p_0_in4_out(3)
    );
\wr_pntr_gc[4]_i_1\: unisim.vcomponents.LUT2
    generic map(
      INIT => X"6"
    )
        port map (
      I0 => \gic0.gc0.count_d2_reg[5]\(4),
      I1 => \gic0.gc0.count_d2_reg[5]\(5),
      O => p_0_in4_out(4)
    );
\wr_pntr_gc_reg[0]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0),
      D => p_0_in4_out(0),
      Q => wr_pntr_gc(0)
    );
\wr_pntr_gc_reg[1]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0),
      D => p_0_in4_out(1),
      Q => wr_pntr_gc(1)
    );
\wr_pntr_gc_reg[2]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0),
      D => p_0_in4_out(2),
      Q => wr_pntr_gc(2)
    );
\wr_pntr_gc_reg[3]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0),
      D => p_0_in4_out(3),
      Q => wr_pntr_gc(3)
    );
\wr_pntr_gc_reg[4]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0),
      D => p_0_in4_out(4),
      Q => wr_pntr_gc(4)
    );
\wr_pntr_gc_reg[5]\: unisim.vcomponents.FDCE
    generic map(
      INIT => '0'
    )
        port map (
      C => wr_clk,
      CE => '1',
      CLR => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0),
      D => \gic0.gc0.count_d2_reg[5]\(5),
      Q => wr_pntr_gc(5)
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_handshake is
  port (
    got_it_tg_sync : out STD_LOGIC;
    got_it_tg_ff : out STD_LOGIC;
    \addr_current_reg[0]\ : out STD_LOGIC;
    dram_rd_en : out STD_LOGIC;
    dram_rd_flush : out STD_LOGIC;
    busy_reg : out STD_LOGIC;
    state_reg_0 : out STD_LOGIC;
    user_clk : in STD_LOGIC;
    AR : in STD_LOGIC_VECTOR ( 0 to 0 );
    dram_clk : in STD_LOGIC;
    dram_ready : in STD_LOGIC;
    stall : in STD_LOGIC;
    prog_full : in STD_LOGIC;
    state : in STD_LOGIC;
    handshake_go : in STD_LOGIC;
    go : in STD_LOGIC;
    \state_reg[0]\ : in STD_LOGIC;
    \state_reg[1]\ : in STD_LOGIC;
    busy : in STD_LOGIC
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_handshake : entity is "handshake";
end dram_rd_ram0_0_handshake;

architecture STRUCTURE of dram_rd_ram0_0_handshake is
  signal U_GOT_IT_SYNC_n_1 : STD_LOGIC;
  signal U_GOT_IT_SYNC_n_3 : STD_LOGIC;
  signal U_TAKE_IT_SYNC_n_4 : STD_LOGIC;
  signal got_it_tg : STD_LOGIC;
  signal \^got_it_tg_ff\ : STD_LOGIC;
  signal \^got_it_tg_sync\ : STD_LOGIC;
  signal state_0 : STD_LOGIC;
  signal take_it_tg : STD_LOGIC;
  signal take_it_tg_ff : STD_LOGIC;
  signal take_it_tg_sync : STD_LOGIC;
begin
  got_it_tg_ff <= \^got_it_tg_ff\;
  got_it_tg_sync <= \^got_it_tg_sync\;
U_GOT_IT_SYNC: entity work.dram_rd_ram0_0_synchronizer_dual_flop
     port map (
      AR(0) => AR(0),
      busy => busy,
      busy_reg => busy_reg,
      dram_clk => dram_clk,
      go => go,
      got_it_tg => got_it_tg,
      got_it_tg_ff_reg => \^got_it_tg_sync\,
      got_it_tg_ff_reg_0 => \^got_it_tg_ff\,
      handshake_go => handshake_go,
      state_0 => state_0,
      state_reg => U_GOT_IT_SYNC_n_1,
      \state_reg[0]\ => \state_reg[0]\,
      \state_reg[1]\ => \state_reg[1]\,
      take_it_tg => take_it_tg,
      take_it_tg_reg => U_GOT_IT_SYNC_n_3,
      user_clk => user_clk
    );
U_TAKE_IT_SYNC: entity work.dram_rd_ram0_0_synchronizer_dual_flop_0
     port map (
      AR(0) => AR(0),
      \addr_current_reg[0]\ => \addr_current_reg[0]\,
      dram_clk => dram_clk,
      dram_rd_en => dram_rd_en,
      dram_rd_flush => dram_rd_flush,
      dram_ready => dram_ready,
      got_it_tg => got_it_tg,
      got_it_tg_reg => U_TAKE_IT_SYNC_n_4,
      prog_full => prog_full,
      stall => stall,
      state => state,
      state_reg => state_reg_0,
      take_it_tg => take_it_tg,
      take_it_tg_ff => take_it_tg_ff,
      take_it_tg_sync => take_it_tg_sync,
      user_clk => user_clk
    );
got_it_tg_ff_reg: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => '1',
      CLR => AR(0),
      D => \^got_it_tg_sync\,
      Q => \^got_it_tg_ff\
    );
got_it_tg_reg: unisim.vcomponents.FDCE
     port map (
      C => dram_clk,
      CE => '1',
      CLR => AR(0),
      D => U_TAKE_IT_SYNC_n_4,
      Q => got_it_tg
    );
state_reg: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => '1',
      CLR => AR(0),
      D => U_GOT_IT_SYNC_n_1,
      Q => state_0
    );
take_it_tg_ff_reg: unisim.vcomponents.FDCE
     port map (
      C => dram_clk,
      CE => '1',
      CLR => AR(0),
      D => take_it_tg_sync,
      Q => take_it_tg_ff
    );
take_it_tg_reg: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => '1',
      CLR => AR(0),
      D => U_GOT_IT_SYNC_n_3,
      Q => take_it_tg
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_rd_logic is
  port (
    empty : out STD_LOGIC;
    \gc0.count_d1_reg[4]\ : out STD_LOGIC_VECTOR ( 1 downto 0 );
    tmp_ram_rd_en : out STD_LOGIC;
    D : out STD_LOGIC_VECTOR ( 4 downto 0 );
    \DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram\ : out STD_LOGIC_VECTOR ( 5 downto 0 );
    E : out STD_LOGIC_VECTOR ( 0 to 0 );
    rd_clk : in STD_LOGIC;
    Q : in STD_LOGIC_VECTOR ( 1 downto 0 );
    \wr_pntr_bin_reg[5]\ : in STD_LOGIC_VECTOR ( 3 downto 0 );
    \wr_pntr_bin_reg[2]\ : in STD_LOGIC;
    rd_en : in STD_LOGIC;
    \gc0.count_d1_reg[4]_0\ : in STD_LOGIC
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_rd_logic : entity is "rd_logic";
end dram_rd_ram0_0_rd_logic;

architecture STRUCTURE of dram_rd_ram0_0_rd_logic is
  signal \gr1.rfwft_n_1\ : STD_LOGIC;
  signal \gr1.rfwft_n_2\ : STD_LOGIC;
  signal p_18_out : STD_LOGIC;
  signal rd_pntr_plus1 : STD_LOGIC_VECTOR ( 5 to 5 );
  signal rpntr_n_3 : STD_LOGIC;
begin
\gr1.rfwft\: entity work.dram_rd_ram0_0_rd_fwft
     port map (
      E(0) => \gr1.rfwft_n_2\,
      Q(1 downto 0) => Q(1 downto 0),
      empty => empty,
      \gc0.count_reg[5]\(0) => rd_pntr_plus1(5),
      \goreg_bm.dout_i_reg[31]\(0) => E(0),
      p_18_out => p_18_out,
      ram_empty_fb_i_reg => \gr1.rfwft_n_1\,
      rd_clk => rd_clk,
      rd_en => rd_en,
      tmp_ram_rd_en => tmp_ram_rd_en,
      \wr_pntr_bin_reg[5]\(0) => \wr_pntr_bin_reg[5]\(3)
    );
\gras.rsts\: entity work.dram_rd_ram0_0_rd_status_flags_as
     port map (
      Q(0) => Q(1),
      \gc0.count_reg[0]\ => rpntr_n_3,
      p_18_out => p_18_out,
      rd_clk => rd_clk
    );
rpntr: entity work.dram_rd_ram0_0_rd_bin_cntr
     port map (
      D(4 downto 0) => D(4 downto 0),
      \DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram\(5 downto 0) => \DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram\(5 downto 0),
      E(0) => \gr1.rfwft_n_2\,
      Q(2) => rd_pntr_plus1(5),
      Q(1 downto 0) => \gc0.count_d1_reg[4]\(1 downto 0),
      \gc0.count_d1_reg[4]_0\ => \gc0.count_d1_reg[4]_0\,
      \gpregsm1.curr_fwft_state_reg[1]\ => \gr1.rfwft_n_1\,
      \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[2]\(0) => Q(1),
      ram_empty_fb_i_reg => rpntr_n_3,
      rd_clk => rd_clk,
      \wr_pntr_bin_reg[2]\ => \wr_pntr_bin_reg[2]\,
      \wr_pntr_bin_reg[3]\(2 downto 0) => \wr_pntr_bin_reg[5]\(2 downto 0)
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_width_change_fifo is
  port (
    valid : out STD_LOGIC;
    \count_reg[16]\ : out STD_LOGIC;
    AR : out STD_LOGIC_VECTOR ( 0 to 0 );
    rd_en0_in : out STD_LOGIC;
    data : out STD_LOGIC_VECTOR ( 15 downto 0 );
    CO : in STD_LOGIC_VECTOR ( 0 to 0 );
    clear : in STD_LOGIC;
    rd_en : in STD_LOGIC;
    empty : in STD_LOGIC;
    dout : in STD_LOGIC_VECTOR ( 31 downto 0 );
    rst : in STD_LOGIC;
    user_clk : in STD_LOGIC
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_width_change_fifo : entity is "width_change_fifo";
end dram_rd_ram0_0_width_change_fifo;

architecture STRUCTURE of dram_rd_ram0_0_width_change_fifo is
begin
\U_SHRINK.U_SHRINK_FIFO\: entity work.dram_rd_ram0_0_shrinker_fifo
     port map (
      AR(0) => AR(0),
      CO(0) => CO(0),
      clear => clear,
      \count_reg[16]\ => \count_reg[16]\,
      data(15 downto 0) => data(15 downto 0),
      dout(31 downto 0) => dout(31 downto 0),
      empty => empty,
      rd_en => rd_en,
      rd_en0_in => rd_en0_in,
      rst => rst,
      user_clk => user_clk,
      valid => valid
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_wr_logic is
  port (
    full : out STD_LOGIC;
    prog_full : out STD_LOGIC;
    Q : out STD_LOGIC_VECTOR ( 5 downto 0 );
    E : out STD_LOGIC_VECTOR ( 0 to 0 );
    \DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram\ : out STD_LOGIC_VECTOR ( 5 downto 0 );
    wr_clk : in STD_LOGIC;
    rst_full_ff_i : in STD_LOGIC;
    rst_full_gen_i : in STD_LOGIC;
    \rd_pntr_bin_reg[5]\ : in STD_LOGIC_VECTOR ( 5 downto 0 );
    \gic0.gc0.count_d1_reg[4]\ : in STD_LOGIC;
    wr_en : in STD_LOGIC;
    \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\ : in STD_LOGIC_VECTOR ( 0 to 0 )
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_wr_logic : entity is "wr_logic";
end dram_rd_ram0_0_wr_logic;

architecture STRUCTURE of dram_rd_ram0_0_wr_logic is
  signal \^e\ : STD_LOGIC_VECTOR ( 0 to 0 );
  signal \^q\ : STD_LOGIC_VECTOR ( 5 downto 0 );
  signal \gwas.wsts_n_3\ : STD_LOGIC;
  signal p_1_out : STD_LOGIC;
  signal wpntr_n_0 : STD_LOGIC;
  signal wpntr_n_1 : STD_LOGIC;
  signal wpntr_n_10 : STD_LOGIC;
  signal wpntr_n_11 : STD_LOGIC;
  signal wpntr_n_12 : STD_LOGIC;
  signal wpntr_n_2 : STD_LOGIC;
  signal wpntr_n_3 : STD_LOGIC;
begin
  E(0) <= \^e\(0);
  Q(5 downto 0) <= \^q\(5 downto 0);
\gwas.gpf.wrpf\: entity work.dram_rd_ram0_0_wr_pf_as
     port map (
      S(2) => wpntr_n_10,
      S(1) => wpntr_n_11,
      S(0) => wpntr_n_12,
      \gic0.gc0.count_d1_reg[5]\(2) => wpntr_n_1,
      \gic0.gc0.count_d1_reg[5]\(1) => wpntr_n_2,
      \gic0.gc0.count_d1_reg[5]\(0) => wpntr_n_3,
      \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\(0) => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\(0),
      p_1_out => p_1_out,
      prog_full => prog_full,
      rst_full_ff_i => rst_full_ff_i,
      rst_full_gen_i => rst_full_gen_i,
      wr_clk => wr_clk,
      wr_pntr_plus1_pad(5 downto 1) => \^q\(4 downto 0),
      wr_pntr_plus1_pad(0) => \gwas.wsts_n_3\
    );
\gwas.wsts\: entity work.dram_rd_ram0_0_wr_status_flags_as
     port map (
      E(0) => \^e\(0),
      full => full,
      \grstd1.grst_full.grst_f.RST_FULL_GEN_reg\ => wpntr_n_0,
      p_1_out => p_1_out,
      rst_full_ff_i => rst_full_ff_i,
      wr_clk => wr_clk,
      wr_en => wr_en,
      wr_pntr_plus1_pad(0) => \gwas.wsts_n_3\
    );
wpntr: entity work.dram_rd_ram0_0_wr_bin_cntr
     port map (
      \DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram\(5 downto 0) => \DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram\(5 downto 0),
      E(0) => \^e\(0),
      Q(5 downto 0) => \^q\(5 downto 0),
      S(2) => wpntr_n_10,
      S(1) => wpntr_n_11,
      S(0) => wpntr_n_12,
      \gdiff.diff_pntr_pad_reg[6]\(2) => wpntr_n_1,
      \gdiff.diff_pntr_pad_reg[6]\(1) => wpntr_n_2,
      \gdiff.diff_pntr_pad_reg[6]\(0) => wpntr_n_3,
      \gic0.gc0.count_d1_reg[4]_0\ => \gic0.gc0.count_d1_reg[4]\,
      \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\(0) => \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\(0),
      p_1_out => p_1_out,
      ram_full_fb_i_reg => wpntr_n_0,
      \rd_pntr_bin_reg[5]\(5 downto 0) => \rd_pntr_bin_reg[5]\(5 downto 0),
      rst_full_gen_i => rst_full_gen_i,
      wr_clk => wr_clk,
      wr_en => wr_en
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_blk_mem_gen_generic_cstr is
  port (
    D : out STD_LOGIC_VECTOR ( 31 downto 0 );
    rd_clk : in STD_LOGIC;
    wr_clk : in STD_LOGIC;
    tmp_ram_rd_en : in STD_LOGIC;
    E : in STD_LOGIC_VECTOR ( 0 to 0 );
    Q : in STD_LOGIC_VECTOR ( 0 to 0 );
    \gc0.count_d1_reg[5]\ : in STD_LOGIC_VECTOR ( 5 downto 0 );
    \gic0.gc0.count_d2_reg[5]\ : in STD_LOGIC_VECTOR ( 5 downto 0 );
    din : in STD_LOGIC_VECTOR ( 31 downto 0 )
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_blk_mem_gen_generic_cstr : entity is "blk_mem_gen_generic_cstr";
end dram_rd_ram0_0_blk_mem_gen_generic_cstr;

architecture STRUCTURE of dram_rd_ram0_0_blk_mem_gen_generic_cstr is
begin
\ramloop[0].ram.r\: entity work.dram_rd_ram0_0_blk_mem_gen_prim_width
     port map (
      D(31 downto 0) => D(31 downto 0),
      E(0) => E(0),
      Q(0) => Q(0),
      din(31 downto 0) => din(31 downto 0),
      \gc0.count_d1_reg[5]\(5 downto 0) => \gc0.count_d1_reg[5]\(5 downto 0),
      \gic0.gc0.count_d2_reg[5]\(5 downto 0) => \gic0.gc0.count_d2_reg[5]\(5 downto 0),
      rd_clk => rd_clk,
      tmp_ram_rd_en => tmp_ram_rd_en,
      wr_clk => wr_clk
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_blk_mem_gen_top is
  port (
    D : out STD_LOGIC_VECTOR ( 31 downto 0 );
    rd_clk : in STD_LOGIC;
    wr_clk : in STD_LOGIC;
    tmp_ram_rd_en : in STD_LOGIC;
    E : in STD_LOGIC_VECTOR ( 0 to 0 );
    Q : in STD_LOGIC_VECTOR ( 0 to 0 );
    \gc0.count_d1_reg[5]\ : in STD_LOGIC_VECTOR ( 5 downto 0 );
    \gic0.gc0.count_d2_reg[5]\ : in STD_LOGIC_VECTOR ( 5 downto 0 );
    din : in STD_LOGIC_VECTOR ( 31 downto 0 )
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_blk_mem_gen_top : entity is "blk_mem_gen_top";
end dram_rd_ram0_0_blk_mem_gen_top;

architecture STRUCTURE of dram_rd_ram0_0_blk_mem_gen_top is
begin
\valid.cstr\: entity work.dram_rd_ram0_0_blk_mem_gen_generic_cstr
     port map (
      D(31 downto 0) => D(31 downto 0),
      E(0) => E(0),
      Q(0) => Q(0),
      din(31 downto 0) => din(31 downto 0),
      \gc0.count_d1_reg[5]\(5 downto 0) => \gc0.count_d1_reg[5]\(5 downto 0),
      \gic0.gc0.count_d2_reg[5]\(5 downto 0) => \gic0.gc0.count_d2_reg[5]\(5 downto 0),
      rd_clk => rd_clk,
      tmp_ram_rd_en => tmp_ram_rd_en,
      wr_clk => wr_clk
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_blk_mem_gen_v8_2_synth is
  port (
    D : out STD_LOGIC_VECTOR ( 31 downto 0 );
    rd_clk : in STD_LOGIC;
    wr_clk : in STD_LOGIC;
    tmp_ram_rd_en : in STD_LOGIC;
    E : in STD_LOGIC_VECTOR ( 0 to 0 );
    Q : in STD_LOGIC_VECTOR ( 0 to 0 );
    \gc0.count_d1_reg[5]\ : in STD_LOGIC_VECTOR ( 5 downto 0 );
    \gic0.gc0.count_d2_reg[5]\ : in STD_LOGIC_VECTOR ( 5 downto 0 );
    din : in STD_LOGIC_VECTOR ( 31 downto 0 )
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_blk_mem_gen_v8_2_synth : entity is "blk_mem_gen_v8_2_synth";
end dram_rd_ram0_0_blk_mem_gen_v8_2_synth;

architecture STRUCTURE of dram_rd_ram0_0_blk_mem_gen_v8_2_synth is
begin
\gnativebmg.native_blk_mem_gen\: entity work.dram_rd_ram0_0_blk_mem_gen_top
     port map (
      D(31 downto 0) => D(31 downto 0),
      E(0) => E(0),
      Q(0) => Q(0),
      din(31 downto 0) => din(31 downto 0),
      \gc0.count_d1_reg[5]\(5 downto 0) => \gc0.count_d1_reg[5]\(5 downto 0),
      \gic0.gc0.count_d2_reg[5]\(5 downto 0) => \gic0.gc0.count_d2_reg[5]\(5 downto 0),
      rd_clk => rd_clk,
      tmp_ram_rd_en => tmp_ram_rd_en,
      wr_clk => wr_clk
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_blk_mem_gen_v8_2 is
  port (
    D : out STD_LOGIC_VECTOR ( 31 downto 0 );
    rd_clk : in STD_LOGIC;
    wr_clk : in STD_LOGIC;
    tmp_ram_rd_en : in STD_LOGIC;
    E : in STD_LOGIC_VECTOR ( 0 to 0 );
    Q : in STD_LOGIC_VECTOR ( 0 to 0 );
    \gc0.count_d1_reg[5]\ : in STD_LOGIC_VECTOR ( 5 downto 0 );
    \gic0.gc0.count_d2_reg[5]\ : in STD_LOGIC_VECTOR ( 5 downto 0 );
    din : in STD_LOGIC_VECTOR ( 31 downto 0 )
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_blk_mem_gen_v8_2 : entity is "blk_mem_gen_v8_2";
end dram_rd_ram0_0_blk_mem_gen_v8_2;

architecture STRUCTURE of dram_rd_ram0_0_blk_mem_gen_v8_2 is
begin
inst_blk_mem_gen: entity work.dram_rd_ram0_0_blk_mem_gen_v8_2_synth
     port map (
      D(31 downto 0) => D(31 downto 0),
      E(0) => E(0),
      Q(0) => Q(0),
      din(31 downto 0) => din(31 downto 0),
      \gc0.count_d1_reg[5]\(5 downto 0) => \gc0.count_d1_reg[5]\(5 downto 0),
      \gic0.gc0.count_d2_reg[5]\(5 downto 0) => \gic0.gc0.count_d2_reg[5]\(5 downto 0),
      rd_clk => rd_clk,
      tmp_ram_rd_en => tmp_ram_rd_en,
      wr_clk => wr_clk
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_memory is
  port (
    dout : out STD_LOGIC_VECTOR ( 31 downto 0 );
    rd_clk : in STD_LOGIC;
    wr_clk : in STD_LOGIC;
    tmp_ram_rd_en : in STD_LOGIC;
    E : in STD_LOGIC_VECTOR ( 0 to 0 );
    Q : in STD_LOGIC_VECTOR ( 0 to 0 );
    \gc0.count_d1_reg[5]\ : in STD_LOGIC_VECTOR ( 5 downto 0 );
    \gic0.gc0.count_d2_reg[5]\ : in STD_LOGIC_VECTOR ( 5 downto 0 );
    din : in STD_LOGIC_VECTOR ( 31 downto 0 );
    \gpregsm1.curr_fwft_state_reg[1]\ : in STD_LOGIC_VECTOR ( 0 to 0 )
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_memory : entity is "memory";
end dram_rd_ram0_0_memory;

architecture STRUCTURE of dram_rd_ram0_0_memory is
  signal doutb : STD_LOGIC_VECTOR ( 31 downto 0 );
begin
\gbm.gbmg.gbmga.ngecc.bmg\: entity work.dram_rd_ram0_0_blk_mem_gen_v8_2
     port map (
      D(31 downto 0) => doutb(31 downto 0),
      E(0) => E(0),
      Q(0) => Q(0),
      din(31 downto 0) => din(31 downto 0),
      \gc0.count_d1_reg[5]\(5 downto 0) => \gc0.count_d1_reg[5]\(5 downto 0),
      \gic0.gc0.count_d2_reg[5]\(5 downto 0) => \gic0.gc0.count_d2_reg[5]\(5 downto 0),
      rd_clk => rd_clk,
      tmp_ram_rd_en => tmp_ram_rd_en,
      wr_clk => wr_clk
    );
\goreg_bm.dout_i_reg[0]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(0),
      Q => dout(0),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[10]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(10),
      Q => dout(10),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[11]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(11),
      Q => dout(11),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[12]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(12),
      Q => dout(12),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[13]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(13),
      Q => dout(13),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[14]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(14),
      Q => dout(14),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[15]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(15),
      Q => dout(15),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[16]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(16),
      Q => dout(16),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[17]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(17),
      Q => dout(17),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[18]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(18),
      Q => dout(18),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[19]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(19),
      Q => dout(19),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[1]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(1),
      Q => dout(1),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[20]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(20),
      Q => dout(20),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[21]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(21),
      Q => dout(21),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[22]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(22),
      Q => dout(22),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[23]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(23),
      Q => dout(23),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[24]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(24),
      Q => dout(24),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[25]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(25),
      Q => dout(25),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[26]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(26),
      Q => dout(26),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[27]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(27),
      Q => dout(27),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[28]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(28),
      Q => dout(28),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[29]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(29),
      Q => dout(29),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[2]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(2),
      Q => dout(2),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[30]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(30),
      Q => dout(30),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[31]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(31),
      Q => dout(31),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[3]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(3),
      Q => dout(3),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[4]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(4),
      Q => dout(4),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[5]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(5),
      Q => dout(5),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[6]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(6),
      Q => dout(6),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[7]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(7),
      Q => dout(7),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[8]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(8),
      Q => dout(8),
      R => Q(0)
    );
\goreg_bm.dout_i_reg[9]\: unisim.vcomponents.FDRE
    generic map(
      INIT => '0'
    )
        port map (
      C => rd_clk,
      CE => \gpregsm1.curr_fwft_state_reg[1]\(0),
      D => doutb(9),
      Q => dout(9),
      R => Q(0)
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_fifo_generator_ramfifo is
  port (
    dout : out STD_LOGIC_VECTOR ( 31 downto 0 );
    empty : out STD_LOGIC;
    full : out STD_LOGIC;
    prog_full : out STD_LOGIC;
    rd_en : in STD_LOGIC;
    wr_en : in STD_LOGIC;
    rd_clk : in STD_LOGIC;
    wr_clk : in STD_LOGIC;
    din : in STD_LOGIC_VECTOR ( 31 downto 0 );
    rst : in STD_LOGIC
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_fifo_generator_ramfifo : entity is "fifo_generator_ramfifo";
end dram_rd_ram0_0_fifo_generator_ramfifo;

architecture STRUCTURE of dram_rd_ram0_0_fifo_generator_ramfifo is
  signal RD_RST : STD_LOGIC;
  signal \^rst\ : STD_LOGIC;
  signal WR_RST : STD_LOGIC;
  signal \gntv_or_sync_fifo.gcx.clkx_n_0\ : STD_LOGIC;
  signal \gntv_or_sync_fifo.gcx.clkx_n_5\ : STD_LOGIC;
  signal \gntv_or_sync_fifo.gcx.clkx_n_6\ : STD_LOGIC;
  signal \gntv_or_sync_fifo.gl0.rd_n_4\ : STD_LOGIC;
  signal \gntv_or_sync_fifo.gl0.rd_n_5\ : STD_LOGIC;
  signal \gntv_or_sync_fifo.gl0.rd_n_6\ : STD_LOGIC;
  signal \gntv_or_sync_fifo.gl0.rd_n_7\ : STD_LOGIC;
  signal \gntv_or_sync_fifo.gl0.rd_n_8\ : STD_LOGIC;
  signal p_0_out : STD_LOGIC_VECTOR ( 5 downto 0 );
  signal p_15_out : STD_LOGIC;
  signal p_1_out : STD_LOGIC_VECTOR ( 5 downto 0 );
  signal p_20_out : STD_LOGIC_VECTOR ( 5 downto 0 );
  signal p_3_out : STD_LOGIC;
  signal p_8_out : STD_LOGIC_VECTOR ( 5 downto 0 );
  signal p_9_out : STD_LOGIC_VECTOR ( 5 downto 0 );
  signal rd_pntr_plus1 : STD_LOGIC_VECTOR ( 4 downto 2 );
  signal rd_rst_i : STD_LOGIC_VECTOR ( 0 to 0 );
  signal rst_full_ff_i : STD_LOGIC;
  signal rst_full_gen_i : STD_LOGIC;
  signal rstblk_n_2 : STD_LOGIC;
  signal tmp_ram_rd_en : STD_LOGIC;
begin
\gntv_or_sync_fifo.gcx.clkx\: entity work.dram_rd_ram0_0_clk_x_pntrs
     port map (
      D(4) => \gntv_or_sync_fifo.gl0.rd_n_4\,
      D(3) => \gntv_or_sync_fifo.gl0.rd_n_5\,
      D(2) => \gntv_or_sync_fifo.gl0.rd_n_6\,
      D(1) => \gntv_or_sync_fifo.gl0.rd_n_7\,
      D(0) => \gntv_or_sync_fifo.gl0.rd_n_8\,
      Q(3) => p_1_out(5),
      Q(2) => p_1_out(3),
      Q(1 downto 0) => p_1_out(1 downto 0),
      \gc0.count_d1_reg[5]\(5 downto 0) => p_20_out(5 downto 0),
      \gc0.count_reg[4]\(1) => rd_pntr_plus1(4),
      \gc0.count_reg[4]\(0) => rd_pntr_plus1(2),
      \gic0.gc0.count_d1_reg[5]\(5 downto 0) => p_8_out(5 downto 0),
      \gic0.gc0.count_d2_reg[5]\(5 downto 0) => p_9_out(5 downto 0),
      \ngwrdrst.grst.g7serrst.rd_rst_reg_reg[1]\(0) => RD_RST,
      \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[0]\(0) => \^rst\,
      ram_empty_fb_i_reg => \gntv_or_sync_fifo.gcx.clkx_n_0\,
      ram_empty_fb_i_reg_0 => \gntv_or_sync_fifo.gcx.clkx_n_5\,
      ram_full_fb_i_reg => \gntv_or_sync_fifo.gcx.clkx_n_6\,
      ram_full_fb_i_reg_0(5 downto 0) => p_0_out(5 downto 0),
      rd_clk => rd_clk,
      wr_clk => wr_clk
    );
\gntv_or_sync_fifo.gl0.rd\: entity work.dram_rd_ram0_0_rd_logic
     port map (
      D(4) => \gntv_or_sync_fifo.gl0.rd_n_4\,
      D(3) => \gntv_or_sync_fifo.gl0.rd_n_5\,
      D(2) => \gntv_or_sync_fifo.gl0.rd_n_6\,
      D(1) => \gntv_or_sync_fifo.gl0.rd_n_7\,
      D(0) => \gntv_or_sync_fifo.gl0.rd_n_8\,
      \DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram\(5 downto 0) => p_20_out(5 downto 0),
      E(0) => p_15_out,
      Q(1) => rstblk_n_2,
      Q(0) => rd_rst_i(0),
      empty => empty,
      \gc0.count_d1_reg[4]\(1) => rd_pntr_plus1(4),
      \gc0.count_d1_reg[4]\(0) => rd_pntr_plus1(2),
      \gc0.count_d1_reg[4]_0\ => \gntv_or_sync_fifo.gcx.clkx_n_5\,
      rd_clk => rd_clk,
      rd_en => rd_en,
      tmp_ram_rd_en => tmp_ram_rd_en,
      \wr_pntr_bin_reg[2]\ => \gntv_or_sync_fifo.gcx.clkx_n_0\,
      \wr_pntr_bin_reg[5]\(3) => p_1_out(5),
      \wr_pntr_bin_reg[5]\(2) => p_1_out(3),
      \wr_pntr_bin_reg[5]\(1 downto 0) => p_1_out(1 downto 0)
    );
\gntv_or_sync_fifo.gl0.wr\: entity work.dram_rd_ram0_0_wr_logic
     port map (
      \DEVICE_7SERIES.NO_BMM_INFO.SDP.WIDE_PRIM18.ram\(5 downto 0) => p_9_out(5 downto 0),
      E(0) => p_3_out,
      Q(5 downto 0) => p_8_out(5 downto 0),
      full => full,
      \gic0.gc0.count_d1_reg[4]\ => \gntv_or_sync_fifo.gcx.clkx_n_6\,
      \ngwrdrst.grst.g7serrst.wr_rst_reg_reg[1]\(0) => WR_RST,
      prog_full => prog_full,
      \rd_pntr_bin_reg[5]\(5 downto 0) => p_0_out(5 downto 0),
      rst_full_ff_i => rst_full_ff_i,
      rst_full_gen_i => rst_full_gen_i,
      wr_clk => wr_clk,
      wr_en => wr_en
    );
\gntv_or_sync_fifo.mem\: entity work.dram_rd_ram0_0_memory
     port map (
      E(0) => p_3_out,
      Q(0) => rd_rst_i(0),
      din(31 downto 0) => din(31 downto 0),
      dout(31 downto 0) => dout(31 downto 0),
      \gc0.count_d1_reg[5]\(5 downto 0) => p_20_out(5 downto 0),
      \gic0.gc0.count_d2_reg[5]\(5 downto 0) => p_9_out(5 downto 0),
      \gpregsm1.curr_fwft_state_reg[1]\(0) => p_15_out,
      rd_clk => rd_clk,
      tmp_ram_rd_en => tmp_ram_rd_en,
      wr_clk => wr_clk
    );
rstblk: entity work.dram_rd_ram0_0_reset_blk_ramfifo
     port map (
      Q(2) => rstblk_n_2,
      Q(1) => RD_RST,
      Q(0) => rd_rst_i(0),
      \gic0.gc0.count_reg[0]\(1) => WR_RST,
      \gic0.gc0.count_reg[0]\(0) => \^rst\,
      rd_clk => rd_clk,
      rst => rst,
      rst_full_ff_i => rst_full_ff_i,
      rst_full_gen_i => rst_full_gen_i,
      wr_clk => wr_clk
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_fifo_generator_top is
  port (
    dout : out STD_LOGIC_VECTOR ( 31 downto 0 );
    empty : out STD_LOGIC;
    full : out STD_LOGIC;
    prog_full : out STD_LOGIC;
    rd_en : in STD_LOGIC;
    wr_en : in STD_LOGIC;
    rd_clk : in STD_LOGIC;
    wr_clk : in STD_LOGIC;
    din : in STD_LOGIC_VECTOR ( 31 downto 0 );
    rst : in STD_LOGIC
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_fifo_generator_top : entity is "fifo_generator_top";
end dram_rd_ram0_0_fifo_generator_top;

architecture STRUCTURE of dram_rd_ram0_0_fifo_generator_top is
begin
\grf.rf\: entity work.dram_rd_ram0_0_fifo_generator_ramfifo
     port map (
      din(31 downto 0) => din(31 downto 0),
      dout(31 downto 0) => dout(31 downto 0),
      empty => empty,
      full => full,
      prog_full => prog_full,
      rd_clk => rd_clk,
      rd_en => rd_en,
      rst => rst,
      wr_clk => wr_clk,
      wr_en => wr_en
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_fifo_generator_v12_0_synth is
  port (
    dout : out STD_LOGIC_VECTOR ( 31 downto 0 );
    empty : out STD_LOGIC;
    full : out STD_LOGIC;
    prog_full : out STD_LOGIC;
    rd_en : in STD_LOGIC;
    wr_en : in STD_LOGIC;
    rd_clk : in STD_LOGIC;
    wr_clk : in STD_LOGIC;
    din : in STD_LOGIC_VECTOR ( 31 downto 0 );
    rst : in STD_LOGIC
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_fifo_generator_v12_0_synth : entity is "fifo_generator_v12_0_synth";
end dram_rd_ram0_0_fifo_generator_v12_0_synth;

architecture STRUCTURE of dram_rd_ram0_0_fifo_generator_v12_0_synth is
begin
\gconvfifo.rf\: entity work.dram_rd_ram0_0_fifo_generator_top
     port map (
      din(31 downto 0) => din(31 downto 0),
      dout(31 downto 0) => dout(31 downto 0),
      empty => empty,
      full => full,
      prog_full => prog_full,
      rd_clk => rd_clk,
      rd_en => rd_en,
      rst => rst,
      wr_clk => wr_clk,
      wr_en => wr_en
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_fifo_generator_v12_0 is
  port (
    backup : in STD_LOGIC;
    backup_marker : in STD_LOGIC;
    clk : in STD_LOGIC;
    rst : in STD_LOGIC;
    srst : in STD_LOGIC;
    wr_clk : in STD_LOGIC;
    wr_rst : in STD_LOGIC;
    rd_clk : in STD_LOGIC;
    rd_rst : in STD_LOGIC;
    din : in STD_LOGIC_VECTOR ( 31 downto 0 );
    wr_en : in STD_LOGIC;
    rd_en : in STD_LOGIC;
    prog_empty_thresh : in STD_LOGIC_VECTOR ( 5 downto 0 );
    prog_empty_thresh_assert : in STD_LOGIC_VECTOR ( 5 downto 0 );
    prog_empty_thresh_negate : in STD_LOGIC_VECTOR ( 5 downto 0 );
    prog_full_thresh : in STD_LOGIC_VECTOR ( 5 downto 0 );
    prog_full_thresh_assert : in STD_LOGIC_VECTOR ( 5 downto 0 );
    prog_full_thresh_negate : in STD_LOGIC_VECTOR ( 5 downto 0 );
    int_clk : in STD_LOGIC;
    injectdbiterr : in STD_LOGIC;
    injectsbiterr : in STD_LOGIC;
    sleep : in STD_LOGIC;
    dout : out STD_LOGIC_VECTOR ( 31 downto 0 );
    full : out STD_LOGIC;
    almost_full : out STD_LOGIC;
    wr_ack : out STD_LOGIC;
    overflow : out STD_LOGIC;
    empty : out STD_LOGIC;
    almost_empty : out STD_LOGIC;
    valid : out STD_LOGIC;
    underflow : out STD_LOGIC;
    data_count : out STD_LOGIC_VECTOR ( 5 downto 0 );
    rd_data_count : out STD_LOGIC_VECTOR ( 5 downto 0 );
    wr_data_count : out STD_LOGIC_VECTOR ( 5 downto 0 );
    prog_full : out STD_LOGIC;
    prog_empty : out STD_LOGIC;
    sbiterr : out STD_LOGIC;
    dbiterr : out STD_LOGIC;
    wr_rst_busy : out STD_LOGIC;
    rd_rst_busy : out STD_LOGIC;
    m_aclk : in STD_LOGIC;
    s_aclk : in STD_LOGIC;
    s_aresetn : in STD_LOGIC;
    m_aclk_en : in STD_LOGIC;
    s_aclk_en : in STD_LOGIC;
    s_axi_awid : in STD_LOGIC_VECTOR ( 0 to 0 );
    s_axi_awaddr : in STD_LOGIC_VECTOR ( 31 downto 0 );
    s_axi_awlen : in STD_LOGIC_VECTOR ( 7 downto 0 );
    s_axi_awsize : in STD_LOGIC_VECTOR ( 2 downto 0 );
    s_axi_awburst : in STD_LOGIC_VECTOR ( 1 downto 0 );
    s_axi_awlock : in STD_LOGIC_VECTOR ( 0 to 0 );
    s_axi_awcache : in STD_LOGIC_VECTOR ( 3 downto 0 );
    s_axi_awprot : in STD_LOGIC_VECTOR ( 2 downto 0 );
    s_axi_awqos : in STD_LOGIC_VECTOR ( 3 downto 0 );
    s_axi_awregion : in STD_LOGIC_VECTOR ( 3 downto 0 );
    s_axi_awuser : in STD_LOGIC_VECTOR ( 0 to 0 );
    s_axi_awvalid : in STD_LOGIC;
    s_axi_awready : out STD_LOGIC;
    s_axi_wid : in STD_LOGIC_VECTOR ( 0 to 0 );
    s_axi_wdata : in STD_LOGIC_VECTOR ( 63 downto 0 );
    s_axi_wstrb : in STD_LOGIC_VECTOR ( 7 downto 0 );
    s_axi_wlast : in STD_LOGIC;
    s_axi_wuser : in STD_LOGIC_VECTOR ( 0 to 0 );
    s_axi_wvalid : in STD_LOGIC;
    s_axi_wready : out STD_LOGIC;
    s_axi_bid : out STD_LOGIC_VECTOR ( 0 to 0 );
    s_axi_bresp : out STD_LOGIC_VECTOR ( 1 downto 0 );
    s_axi_buser : out STD_LOGIC_VECTOR ( 0 to 0 );
    s_axi_bvalid : out STD_LOGIC;
    s_axi_bready : in STD_LOGIC;
    m_axi_awid : out STD_LOGIC_VECTOR ( 0 to 0 );
    m_axi_awaddr : out STD_LOGIC_VECTOR ( 31 downto 0 );
    m_axi_awlen : out STD_LOGIC_VECTOR ( 7 downto 0 );
    m_axi_awsize : out STD_LOGIC_VECTOR ( 2 downto 0 );
    m_axi_awburst : out STD_LOGIC_VECTOR ( 1 downto 0 );
    m_axi_awlock : out STD_LOGIC_VECTOR ( 0 to 0 );
    m_axi_awcache : out STD_LOGIC_VECTOR ( 3 downto 0 );
    m_axi_awprot : out STD_LOGIC_VECTOR ( 2 downto 0 );
    m_axi_awqos : out STD_LOGIC_VECTOR ( 3 downto 0 );
    m_axi_awregion : out STD_LOGIC_VECTOR ( 3 downto 0 );
    m_axi_awuser : out STD_LOGIC_VECTOR ( 0 to 0 );
    m_axi_awvalid : out STD_LOGIC;
    m_axi_awready : in STD_LOGIC;
    m_axi_wid : out STD_LOGIC_VECTOR ( 0 to 0 );
    m_axi_wdata : out STD_LOGIC_VECTOR ( 63 downto 0 );
    m_axi_wstrb : out STD_LOGIC_VECTOR ( 7 downto 0 );
    m_axi_wlast : out STD_LOGIC;
    m_axi_wuser : out STD_LOGIC_VECTOR ( 0 to 0 );
    m_axi_wvalid : out STD_LOGIC;
    m_axi_wready : in STD_LOGIC;
    m_axi_bid : in STD_LOGIC_VECTOR ( 0 to 0 );
    m_axi_bresp : in STD_LOGIC_VECTOR ( 1 downto 0 );
    m_axi_buser : in STD_LOGIC_VECTOR ( 0 to 0 );
    m_axi_bvalid : in STD_LOGIC;
    m_axi_bready : out STD_LOGIC;
    s_axi_arid : in STD_LOGIC_VECTOR ( 0 to 0 );
    s_axi_araddr : in STD_LOGIC_VECTOR ( 31 downto 0 );
    s_axi_arlen : in STD_LOGIC_VECTOR ( 7 downto 0 );
    s_axi_arsize : in STD_LOGIC_VECTOR ( 2 downto 0 );
    s_axi_arburst : in STD_LOGIC_VECTOR ( 1 downto 0 );
    s_axi_arlock : in STD_LOGIC_VECTOR ( 0 to 0 );
    s_axi_arcache : in STD_LOGIC_VECTOR ( 3 downto 0 );
    s_axi_arprot : in STD_LOGIC_VECTOR ( 2 downto 0 );
    s_axi_arqos : in STD_LOGIC_VECTOR ( 3 downto 0 );
    s_axi_arregion : in STD_LOGIC_VECTOR ( 3 downto 0 );
    s_axi_aruser : in STD_LOGIC_VECTOR ( 0 to 0 );
    s_axi_arvalid : in STD_LOGIC;
    s_axi_arready : out STD_LOGIC;
    s_axi_rid : out STD_LOGIC_VECTOR ( 0 to 0 );
    s_axi_rdata : out STD_LOGIC_VECTOR ( 63 downto 0 );
    s_axi_rresp : out STD_LOGIC_VECTOR ( 1 downto 0 );
    s_axi_rlast : out STD_LOGIC;
    s_axi_ruser : out STD_LOGIC_VECTOR ( 0 to 0 );
    s_axi_rvalid : out STD_LOGIC;
    s_axi_rready : in STD_LOGIC;
    m_axi_arid : out STD_LOGIC_VECTOR ( 0 to 0 );
    m_axi_araddr : out STD_LOGIC_VECTOR ( 31 downto 0 );
    m_axi_arlen : out STD_LOGIC_VECTOR ( 7 downto 0 );
    m_axi_arsize : out STD_LOGIC_VECTOR ( 2 downto 0 );
    m_axi_arburst : out STD_LOGIC_VECTOR ( 1 downto 0 );
    m_axi_arlock : out STD_LOGIC_VECTOR ( 0 to 0 );
    m_axi_arcache : out STD_LOGIC_VECTOR ( 3 downto 0 );
    m_axi_arprot : out STD_LOGIC_VECTOR ( 2 downto 0 );
    m_axi_arqos : out STD_LOGIC_VECTOR ( 3 downto 0 );
    m_axi_arregion : out STD_LOGIC_VECTOR ( 3 downto 0 );
    m_axi_aruser : out STD_LOGIC_VECTOR ( 0 to 0 );
    m_axi_arvalid : out STD_LOGIC;
    m_axi_arready : in STD_LOGIC;
    m_axi_rid : in STD_LOGIC_VECTOR ( 0 to 0 );
    m_axi_rdata : in STD_LOGIC_VECTOR ( 63 downto 0 );
    m_axi_rresp : in STD_LOGIC_VECTOR ( 1 downto 0 );
    m_axi_rlast : in STD_LOGIC;
    m_axi_ruser : in STD_LOGIC_VECTOR ( 0 to 0 );
    m_axi_rvalid : in STD_LOGIC;
    m_axi_rready : out STD_LOGIC;
    s_axis_tvalid : in STD_LOGIC;
    s_axis_tready : out STD_LOGIC;
    s_axis_tdata : in STD_LOGIC_VECTOR ( 7 downto 0 );
    s_axis_tstrb : in STD_LOGIC_VECTOR ( 0 to 0 );
    s_axis_tkeep : in STD_LOGIC_VECTOR ( 0 to 0 );
    s_axis_tlast : in STD_LOGIC;
    s_axis_tid : in STD_LOGIC_VECTOR ( 0 to 0 );
    s_axis_tdest : in STD_LOGIC_VECTOR ( 0 to 0 );
    s_axis_tuser : in STD_LOGIC_VECTOR ( 3 downto 0 );
    m_axis_tvalid : out STD_LOGIC;
    m_axis_tready : in STD_LOGIC;
    m_axis_tdata : out STD_LOGIC_VECTOR ( 7 downto 0 );
    m_axis_tstrb : out STD_LOGIC_VECTOR ( 0 to 0 );
    m_axis_tkeep : out STD_LOGIC_VECTOR ( 0 to 0 );
    m_axis_tlast : out STD_LOGIC;
    m_axis_tid : out STD_LOGIC_VECTOR ( 0 to 0 );
    m_axis_tdest : out STD_LOGIC_VECTOR ( 0 to 0 );
    m_axis_tuser : out STD_LOGIC_VECTOR ( 3 downto 0 );
    axi_aw_injectsbiterr : in STD_LOGIC;
    axi_aw_injectdbiterr : in STD_LOGIC;
    axi_aw_prog_full_thresh : in STD_LOGIC_VECTOR ( 3 downto 0 );
    axi_aw_prog_empty_thresh : in STD_LOGIC_VECTOR ( 3 downto 0 );
    axi_aw_data_count : out STD_LOGIC_VECTOR ( 4 downto 0 );
    axi_aw_wr_data_count : out STD_LOGIC_VECTOR ( 4 downto 0 );
    axi_aw_rd_data_count : out STD_LOGIC_VECTOR ( 4 downto 0 );
    axi_aw_sbiterr : out STD_LOGIC;
    axi_aw_dbiterr : out STD_LOGIC;
    axi_aw_overflow : out STD_LOGIC;
    axi_aw_underflow : out STD_LOGIC;
    axi_aw_prog_full : out STD_LOGIC;
    axi_aw_prog_empty : out STD_LOGIC;
    axi_w_injectsbiterr : in STD_LOGIC;
    axi_w_injectdbiterr : in STD_LOGIC;
    axi_w_prog_full_thresh : in STD_LOGIC_VECTOR ( 9 downto 0 );
    axi_w_prog_empty_thresh : in STD_LOGIC_VECTOR ( 9 downto 0 );
    axi_w_data_count : out STD_LOGIC_VECTOR ( 10 downto 0 );
    axi_w_wr_data_count : out STD_LOGIC_VECTOR ( 10 downto 0 );
    axi_w_rd_data_count : out STD_LOGIC_VECTOR ( 10 downto 0 );
    axi_w_sbiterr : out STD_LOGIC;
    axi_w_dbiterr : out STD_LOGIC;
    axi_w_overflow : out STD_LOGIC;
    axi_w_underflow : out STD_LOGIC;
    axi_w_prog_full : out STD_LOGIC;
    axi_w_prog_empty : out STD_LOGIC;
    axi_b_injectsbiterr : in STD_LOGIC;
    axi_b_injectdbiterr : in STD_LOGIC;
    axi_b_prog_full_thresh : in STD_LOGIC_VECTOR ( 3 downto 0 );
    axi_b_prog_empty_thresh : in STD_LOGIC_VECTOR ( 3 downto 0 );
    axi_b_data_count : out STD_LOGIC_VECTOR ( 4 downto 0 );
    axi_b_wr_data_count : out STD_LOGIC_VECTOR ( 4 downto 0 );
    axi_b_rd_data_count : out STD_LOGIC_VECTOR ( 4 downto 0 );
    axi_b_sbiterr : out STD_LOGIC;
    axi_b_dbiterr : out STD_LOGIC;
    axi_b_overflow : out STD_LOGIC;
    axi_b_underflow : out STD_LOGIC;
    axi_b_prog_full : out STD_LOGIC;
    axi_b_prog_empty : out STD_LOGIC;
    axi_ar_injectsbiterr : in STD_LOGIC;
    axi_ar_injectdbiterr : in STD_LOGIC;
    axi_ar_prog_full_thresh : in STD_LOGIC_VECTOR ( 3 downto 0 );
    axi_ar_prog_empty_thresh : in STD_LOGIC_VECTOR ( 3 downto 0 );
    axi_ar_data_count : out STD_LOGIC_VECTOR ( 4 downto 0 );
    axi_ar_wr_data_count : out STD_LOGIC_VECTOR ( 4 downto 0 );
    axi_ar_rd_data_count : out STD_LOGIC_VECTOR ( 4 downto 0 );
    axi_ar_sbiterr : out STD_LOGIC;
    axi_ar_dbiterr : out STD_LOGIC;
    axi_ar_overflow : out STD_LOGIC;
    axi_ar_underflow : out STD_LOGIC;
    axi_ar_prog_full : out STD_LOGIC;
    axi_ar_prog_empty : out STD_LOGIC;
    axi_r_injectsbiterr : in STD_LOGIC;
    axi_r_injectdbiterr : in STD_LOGIC;
    axi_r_prog_full_thresh : in STD_LOGIC_VECTOR ( 9 downto 0 );
    axi_r_prog_empty_thresh : in STD_LOGIC_VECTOR ( 9 downto 0 );
    axi_r_data_count : out STD_LOGIC_VECTOR ( 10 downto 0 );
    axi_r_wr_data_count : out STD_LOGIC_VECTOR ( 10 downto 0 );
    axi_r_rd_data_count : out STD_LOGIC_VECTOR ( 10 downto 0 );
    axi_r_sbiterr : out STD_LOGIC;
    axi_r_dbiterr : out STD_LOGIC;
    axi_r_overflow : out STD_LOGIC;
    axi_r_underflow : out STD_LOGIC;
    axi_r_prog_full : out STD_LOGIC;
    axi_r_prog_empty : out STD_LOGIC;
    axis_injectsbiterr : in STD_LOGIC;
    axis_injectdbiterr : in STD_LOGIC;
    axis_prog_full_thresh : in STD_LOGIC_VECTOR ( 9 downto 0 );
    axis_prog_empty_thresh : in STD_LOGIC_VECTOR ( 9 downto 0 );
    axis_data_count : out STD_LOGIC_VECTOR ( 10 downto 0 );
    axis_wr_data_count : out STD_LOGIC_VECTOR ( 10 downto 0 );
    axis_rd_data_count : out STD_LOGIC_VECTOR ( 10 downto 0 );
    axis_sbiterr : out STD_LOGIC;
    axis_dbiterr : out STD_LOGIC;
    axis_overflow : out STD_LOGIC;
    axis_underflow : out STD_LOGIC;
    axis_prog_full : out STD_LOGIC;
    axis_prog_empty : out STD_LOGIC
  );
  attribute C_ADD_NGC_CONSTRAINT : integer;
  attribute C_ADD_NGC_CONSTRAINT of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_APPLICATION_TYPE_AXIS : integer;
  attribute C_APPLICATION_TYPE_AXIS of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_APPLICATION_TYPE_RACH : integer;
  attribute C_APPLICATION_TYPE_RACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_APPLICATION_TYPE_RDCH : integer;
  attribute C_APPLICATION_TYPE_RDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_APPLICATION_TYPE_WACH : integer;
  attribute C_APPLICATION_TYPE_WACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_APPLICATION_TYPE_WDCH : integer;
  attribute C_APPLICATION_TYPE_WDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_APPLICATION_TYPE_WRCH : integer;
  attribute C_APPLICATION_TYPE_WRCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_AXIS_TDATA_WIDTH : integer;
  attribute C_AXIS_TDATA_WIDTH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 8;
  attribute C_AXIS_TDEST_WIDTH : integer;
  attribute C_AXIS_TDEST_WIDTH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_AXIS_TID_WIDTH : integer;
  attribute C_AXIS_TID_WIDTH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_AXIS_TKEEP_WIDTH : integer;
  attribute C_AXIS_TKEEP_WIDTH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_AXIS_TSTRB_WIDTH : integer;
  attribute C_AXIS_TSTRB_WIDTH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_AXIS_TUSER_WIDTH : integer;
  attribute C_AXIS_TUSER_WIDTH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 4;
  attribute C_AXIS_TYPE : integer;
  attribute C_AXIS_TYPE of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_AXI_ADDR_WIDTH : integer;
  attribute C_AXI_ADDR_WIDTH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 32;
  attribute C_AXI_ARUSER_WIDTH : integer;
  attribute C_AXI_ARUSER_WIDTH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_AXI_AWUSER_WIDTH : integer;
  attribute C_AXI_AWUSER_WIDTH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_AXI_BUSER_WIDTH : integer;
  attribute C_AXI_BUSER_WIDTH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_AXI_DATA_WIDTH : integer;
  attribute C_AXI_DATA_WIDTH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 64;
  attribute C_AXI_ID_WIDTH : integer;
  attribute C_AXI_ID_WIDTH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_AXI_LEN_WIDTH : integer;
  attribute C_AXI_LEN_WIDTH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 8;
  attribute C_AXI_LOCK_WIDTH : integer;
  attribute C_AXI_LOCK_WIDTH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_AXI_RUSER_WIDTH : integer;
  attribute C_AXI_RUSER_WIDTH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_AXI_TYPE : integer;
  attribute C_AXI_TYPE of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_AXI_WUSER_WIDTH : integer;
  attribute C_AXI_WUSER_WIDTH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_COMMON_CLOCK : integer;
  attribute C_COMMON_CLOCK of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_COUNT_TYPE : integer;
  attribute C_COUNT_TYPE of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_DATA_COUNT_WIDTH : integer;
  attribute C_DATA_COUNT_WIDTH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 6;
  attribute C_DEFAULT_VALUE : string;
  attribute C_DEFAULT_VALUE of dram_rd_ram0_0_fifo_generator_v12_0 : entity is "BlankString";
  attribute C_DIN_WIDTH : integer;
  attribute C_DIN_WIDTH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 32;
  attribute C_DIN_WIDTH_AXIS : integer;
  attribute C_DIN_WIDTH_AXIS of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_DIN_WIDTH_RACH : integer;
  attribute C_DIN_WIDTH_RACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 32;
  attribute C_DIN_WIDTH_RDCH : integer;
  attribute C_DIN_WIDTH_RDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 64;
  attribute C_DIN_WIDTH_WACH : integer;
  attribute C_DIN_WIDTH_WACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 32;
  attribute C_DIN_WIDTH_WDCH : integer;
  attribute C_DIN_WIDTH_WDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 64;
  attribute C_DIN_WIDTH_WRCH : integer;
  attribute C_DIN_WIDTH_WRCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 2;
  attribute C_DOUT_RST_VAL : string;
  attribute C_DOUT_RST_VAL of dram_rd_ram0_0_fifo_generator_v12_0 : entity is "0";
  attribute C_DOUT_WIDTH : integer;
  attribute C_DOUT_WIDTH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 32;
  attribute C_ENABLE_RLOCS : integer;
  attribute C_ENABLE_RLOCS of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_ENABLE_RST_SYNC : integer;
  attribute C_ENABLE_RST_SYNC of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_ERROR_INJECTION_TYPE : integer;
  attribute C_ERROR_INJECTION_TYPE of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_ERROR_INJECTION_TYPE_AXIS : integer;
  attribute C_ERROR_INJECTION_TYPE_AXIS of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_ERROR_INJECTION_TYPE_RACH : integer;
  attribute C_ERROR_INJECTION_TYPE_RACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_ERROR_INJECTION_TYPE_RDCH : integer;
  attribute C_ERROR_INJECTION_TYPE_RDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_ERROR_INJECTION_TYPE_WACH : integer;
  attribute C_ERROR_INJECTION_TYPE_WACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_ERROR_INJECTION_TYPE_WDCH : integer;
  attribute C_ERROR_INJECTION_TYPE_WDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_ERROR_INJECTION_TYPE_WRCH : integer;
  attribute C_ERROR_INJECTION_TYPE_WRCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_FAMILY : string;
  attribute C_FAMILY of dram_rd_ram0_0_fifo_generator_v12_0 : entity is "zynq";
  attribute C_FULL_FLAGS_RST_VAL : integer;
  attribute C_FULL_FLAGS_RST_VAL of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_HAS_ALMOST_EMPTY : integer;
  attribute C_HAS_ALMOST_EMPTY of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_ALMOST_FULL : integer;
  attribute C_HAS_ALMOST_FULL of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_AXIS_TDATA : integer;
  attribute C_HAS_AXIS_TDATA of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_HAS_AXIS_TDEST : integer;
  attribute C_HAS_AXIS_TDEST of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_AXIS_TID : integer;
  attribute C_HAS_AXIS_TID of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_AXIS_TKEEP : integer;
  attribute C_HAS_AXIS_TKEEP of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_AXIS_TLAST : integer;
  attribute C_HAS_AXIS_TLAST of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_AXIS_TREADY : integer;
  attribute C_HAS_AXIS_TREADY of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_HAS_AXIS_TSTRB : integer;
  attribute C_HAS_AXIS_TSTRB of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_AXIS_TUSER : integer;
  attribute C_HAS_AXIS_TUSER of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_HAS_AXI_ARUSER : integer;
  attribute C_HAS_AXI_ARUSER of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_AXI_AWUSER : integer;
  attribute C_HAS_AXI_AWUSER of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_AXI_BUSER : integer;
  attribute C_HAS_AXI_BUSER of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_AXI_ID : integer;
  attribute C_HAS_AXI_ID of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_AXI_RD_CHANNEL : integer;
  attribute C_HAS_AXI_RD_CHANNEL of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_HAS_AXI_RUSER : integer;
  attribute C_HAS_AXI_RUSER of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_AXI_WR_CHANNEL : integer;
  attribute C_HAS_AXI_WR_CHANNEL of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_HAS_AXI_WUSER : integer;
  attribute C_HAS_AXI_WUSER of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_BACKUP : integer;
  attribute C_HAS_BACKUP of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_DATA_COUNT : integer;
  attribute C_HAS_DATA_COUNT of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_DATA_COUNTS_AXIS : integer;
  attribute C_HAS_DATA_COUNTS_AXIS of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_DATA_COUNTS_RACH : integer;
  attribute C_HAS_DATA_COUNTS_RACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_DATA_COUNTS_RDCH : integer;
  attribute C_HAS_DATA_COUNTS_RDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_DATA_COUNTS_WACH : integer;
  attribute C_HAS_DATA_COUNTS_WACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_DATA_COUNTS_WDCH : integer;
  attribute C_HAS_DATA_COUNTS_WDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_DATA_COUNTS_WRCH : integer;
  attribute C_HAS_DATA_COUNTS_WRCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_INT_CLK : integer;
  attribute C_HAS_INT_CLK of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_MASTER_CE : integer;
  attribute C_HAS_MASTER_CE of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_MEMINIT_FILE : integer;
  attribute C_HAS_MEMINIT_FILE of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_OVERFLOW : integer;
  attribute C_HAS_OVERFLOW of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_PROG_FLAGS_AXIS : integer;
  attribute C_HAS_PROG_FLAGS_AXIS of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_PROG_FLAGS_RACH : integer;
  attribute C_HAS_PROG_FLAGS_RACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_PROG_FLAGS_RDCH : integer;
  attribute C_HAS_PROG_FLAGS_RDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_PROG_FLAGS_WACH : integer;
  attribute C_HAS_PROG_FLAGS_WACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_PROG_FLAGS_WDCH : integer;
  attribute C_HAS_PROG_FLAGS_WDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_PROG_FLAGS_WRCH : integer;
  attribute C_HAS_PROG_FLAGS_WRCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_RD_DATA_COUNT : integer;
  attribute C_HAS_RD_DATA_COUNT of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_RD_RST : integer;
  attribute C_HAS_RD_RST of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_RST : integer;
  attribute C_HAS_RST of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_HAS_SLAVE_CE : integer;
  attribute C_HAS_SLAVE_CE of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_SRST : integer;
  attribute C_HAS_SRST of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_UNDERFLOW : integer;
  attribute C_HAS_UNDERFLOW of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_VALID : integer;
  attribute C_HAS_VALID of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_WR_ACK : integer;
  attribute C_HAS_WR_ACK of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_WR_DATA_COUNT : integer;
  attribute C_HAS_WR_DATA_COUNT of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_HAS_WR_RST : integer;
  attribute C_HAS_WR_RST of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_IMPLEMENTATION_TYPE : integer;
  attribute C_IMPLEMENTATION_TYPE of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 2;
  attribute C_IMPLEMENTATION_TYPE_AXIS : integer;
  attribute C_IMPLEMENTATION_TYPE_AXIS of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_IMPLEMENTATION_TYPE_RACH : integer;
  attribute C_IMPLEMENTATION_TYPE_RACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_IMPLEMENTATION_TYPE_RDCH : integer;
  attribute C_IMPLEMENTATION_TYPE_RDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_IMPLEMENTATION_TYPE_WACH : integer;
  attribute C_IMPLEMENTATION_TYPE_WACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_IMPLEMENTATION_TYPE_WDCH : integer;
  attribute C_IMPLEMENTATION_TYPE_WDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_IMPLEMENTATION_TYPE_WRCH : integer;
  attribute C_IMPLEMENTATION_TYPE_WRCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_INIT_WR_PNTR_VAL : integer;
  attribute C_INIT_WR_PNTR_VAL of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_INTERFACE_TYPE : integer;
  attribute C_INTERFACE_TYPE of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_MEMORY_TYPE : integer;
  attribute C_MEMORY_TYPE of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_MIF_FILE_NAME : string;
  attribute C_MIF_FILE_NAME of dram_rd_ram0_0_fifo_generator_v12_0 : entity is "BlankString";
  attribute C_MSGON_VAL : integer;
  attribute C_MSGON_VAL of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_OPTIMIZATION_MODE : integer;
  attribute C_OPTIMIZATION_MODE of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_OVERFLOW_LOW : integer;
  attribute C_OVERFLOW_LOW of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_POWER_SAVING_MODE : integer;
  attribute C_POWER_SAVING_MODE of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_PRELOAD_LATENCY : integer;
  attribute C_PRELOAD_LATENCY of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_PRELOAD_REGS : integer;
  attribute C_PRELOAD_REGS of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_PRIM_FIFO_TYPE : string;
  attribute C_PRIM_FIFO_TYPE of dram_rd_ram0_0_fifo_generator_v12_0 : entity is "512x36";
  attribute C_PRIM_FIFO_TYPE_AXIS : string;
  attribute C_PRIM_FIFO_TYPE_AXIS of dram_rd_ram0_0_fifo_generator_v12_0 : entity is "1kx18";
  attribute C_PRIM_FIFO_TYPE_RACH : string;
  attribute C_PRIM_FIFO_TYPE_RACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is "512x36";
  attribute C_PRIM_FIFO_TYPE_RDCH : string;
  attribute C_PRIM_FIFO_TYPE_RDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is "1kx36";
  attribute C_PRIM_FIFO_TYPE_WACH : string;
  attribute C_PRIM_FIFO_TYPE_WACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is "512x36";
  attribute C_PRIM_FIFO_TYPE_WDCH : string;
  attribute C_PRIM_FIFO_TYPE_WDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is "1kx36";
  attribute C_PRIM_FIFO_TYPE_WRCH : string;
  attribute C_PRIM_FIFO_TYPE_WRCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is "512x36";
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL : integer;
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 4;
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL_AXIS : integer;
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL_AXIS of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1022;
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL_RACH : integer;
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL_RACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1022;
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL_RDCH : integer;
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL_RDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1022;
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL_WACH : integer;
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL_WACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1022;
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL_WDCH : integer;
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL_WDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1022;
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL_WRCH : integer;
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL_WRCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1022;
  attribute C_PROG_EMPTY_THRESH_NEGATE_VAL : integer;
  attribute C_PROG_EMPTY_THRESH_NEGATE_VAL of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 5;
  attribute C_PROG_EMPTY_TYPE : integer;
  attribute C_PROG_EMPTY_TYPE of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_PROG_EMPTY_TYPE_AXIS : integer;
  attribute C_PROG_EMPTY_TYPE_AXIS of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_PROG_EMPTY_TYPE_RACH : integer;
  attribute C_PROG_EMPTY_TYPE_RACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_PROG_EMPTY_TYPE_RDCH : integer;
  attribute C_PROG_EMPTY_TYPE_RDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_PROG_EMPTY_TYPE_WACH : integer;
  attribute C_PROG_EMPTY_TYPE_WACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_PROG_EMPTY_TYPE_WDCH : integer;
  attribute C_PROG_EMPTY_TYPE_WDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_PROG_EMPTY_TYPE_WRCH : integer;
  attribute C_PROG_EMPTY_TYPE_WRCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL : integer;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 50;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL_AXIS : integer;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL_AXIS of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1023;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL_RACH : integer;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL_RACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1023;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL_RDCH : integer;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL_RDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1023;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL_WACH : integer;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL_WACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1023;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL_WDCH : integer;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL_WDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1023;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL_WRCH : integer;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL_WRCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1023;
  attribute C_PROG_FULL_THRESH_NEGATE_VAL : integer;
  attribute C_PROG_FULL_THRESH_NEGATE_VAL of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 49;
  attribute C_PROG_FULL_TYPE : integer;
  attribute C_PROG_FULL_TYPE of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_PROG_FULL_TYPE_AXIS : integer;
  attribute C_PROG_FULL_TYPE_AXIS of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_PROG_FULL_TYPE_RACH : integer;
  attribute C_PROG_FULL_TYPE_RACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_PROG_FULL_TYPE_RDCH : integer;
  attribute C_PROG_FULL_TYPE_RDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_PROG_FULL_TYPE_WACH : integer;
  attribute C_PROG_FULL_TYPE_WACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_PROG_FULL_TYPE_WDCH : integer;
  attribute C_PROG_FULL_TYPE_WDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_PROG_FULL_TYPE_WRCH : integer;
  attribute C_PROG_FULL_TYPE_WRCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_RACH_TYPE : integer;
  attribute C_RACH_TYPE of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_RDCH_TYPE : integer;
  attribute C_RDCH_TYPE of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_RD_DATA_COUNT_WIDTH : integer;
  attribute C_RD_DATA_COUNT_WIDTH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 6;
  attribute C_RD_DEPTH : integer;
  attribute C_RD_DEPTH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 64;
  attribute C_RD_FREQ : integer;
  attribute C_RD_FREQ of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_RD_PNTR_WIDTH : integer;
  attribute C_RD_PNTR_WIDTH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 6;
  attribute C_REG_SLICE_MODE_AXIS : integer;
  attribute C_REG_SLICE_MODE_AXIS of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_REG_SLICE_MODE_RACH : integer;
  attribute C_REG_SLICE_MODE_RACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_REG_SLICE_MODE_RDCH : integer;
  attribute C_REG_SLICE_MODE_RDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_REG_SLICE_MODE_WACH : integer;
  attribute C_REG_SLICE_MODE_WACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_REG_SLICE_MODE_WDCH : integer;
  attribute C_REG_SLICE_MODE_WDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_REG_SLICE_MODE_WRCH : integer;
  attribute C_REG_SLICE_MODE_WRCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_SYNCHRONIZER_STAGE : integer;
  attribute C_SYNCHRONIZER_STAGE of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 2;
  attribute C_UNDERFLOW_LOW : integer;
  attribute C_UNDERFLOW_LOW of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_USE_COMMON_OVERFLOW : integer;
  attribute C_USE_COMMON_OVERFLOW of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_USE_COMMON_UNDERFLOW : integer;
  attribute C_USE_COMMON_UNDERFLOW of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_USE_DEFAULT_SETTINGS : integer;
  attribute C_USE_DEFAULT_SETTINGS of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_USE_DOUT_RST : integer;
  attribute C_USE_DOUT_RST of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_USE_ECC : integer;
  attribute C_USE_ECC of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_USE_ECC_AXIS : integer;
  attribute C_USE_ECC_AXIS of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_USE_ECC_RACH : integer;
  attribute C_USE_ECC_RACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_USE_ECC_RDCH : integer;
  attribute C_USE_ECC_RDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_USE_ECC_WACH : integer;
  attribute C_USE_ECC_WACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_USE_ECC_WDCH : integer;
  attribute C_USE_ECC_WDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_USE_ECC_WRCH : integer;
  attribute C_USE_ECC_WRCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_USE_EMBEDDED_REG : integer;
  attribute C_USE_EMBEDDED_REG of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_USE_FIFO16_FLAGS : integer;
  attribute C_USE_FIFO16_FLAGS of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_USE_FWFT_DATA_COUNT : integer;
  attribute C_USE_FWFT_DATA_COUNT of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_USE_PIPELINE_REG : integer;
  attribute C_USE_PIPELINE_REG of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_VALID_LOW : integer;
  attribute C_VALID_LOW of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_WACH_TYPE : integer;
  attribute C_WACH_TYPE of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_WDCH_TYPE : integer;
  attribute C_WDCH_TYPE of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_WRCH_TYPE : integer;
  attribute C_WRCH_TYPE of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_WR_ACK_LOW : integer;
  attribute C_WR_ACK_LOW of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 0;
  attribute C_WR_DATA_COUNT_WIDTH : integer;
  attribute C_WR_DATA_COUNT_WIDTH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 6;
  attribute C_WR_DEPTH : integer;
  attribute C_WR_DEPTH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 64;
  attribute C_WR_DEPTH_AXIS : integer;
  attribute C_WR_DEPTH_AXIS of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1024;
  attribute C_WR_DEPTH_RACH : integer;
  attribute C_WR_DEPTH_RACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 16;
  attribute C_WR_DEPTH_RDCH : integer;
  attribute C_WR_DEPTH_RDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1024;
  attribute C_WR_DEPTH_WACH : integer;
  attribute C_WR_DEPTH_WACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 16;
  attribute C_WR_DEPTH_WDCH : integer;
  attribute C_WR_DEPTH_WDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1024;
  attribute C_WR_DEPTH_WRCH : integer;
  attribute C_WR_DEPTH_WRCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 16;
  attribute C_WR_FREQ : integer;
  attribute C_WR_FREQ of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute C_WR_PNTR_WIDTH : integer;
  attribute C_WR_PNTR_WIDTH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 6;
  attribute C_WR_PNTR_WIDTH_AXIS : integer;
  attribute C_WR_PNTR_WIDTH_AXIS of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 10;
  attribute C_WR_PNTR_WIDTH_RACH : integer;
  attribute C_WR_PNTR_WIDTH_RACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 4;
  attribute C_WR_PNTR_WIDTH_RDCH : integer;
  attribute C_WR_PNTR_WIDTH_RDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 10;
  attribute C_WR_PNTR_WIDTH_WACH : integer;
  attribute C_WR_PNTR_WIDTH_WACH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 4;
  attribute C_WR_PNTR_WIDTH_WDCH : integer;
  attribute C_WR_PNTR_WIDTH_WDCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 10;
  attribute C_WR_PNTR_WIDTH_WRCH : integer;
  attribute C_WR_PNTR_WIDTH_WRCH of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 4;
  attribute C_WR_RESPONSE_LATENCY : integer;
  attribute C_WR_RESPONSE_LATENCY of dram_rd_ram0_0_fifo_generator_v12_0 : entity is 1;
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_fifo_generator_v12_0 : entity is "fifo_generator_v12_0";
end dram_rd_ram0_0_fifo_generator_v12_0;

architecture STRUCTURE of dram_rd_ram0_0_fifo_generator_v12_0 is
  signal \<const0>\ : STD_LOGIC;
  signal \<const1>\ : STD_LOGIC;
begin
  almost_empty <= \<const0>\;
  almost_full <= \<const0>\;
  axi_ar_data_count(4) <= \<const0>\;
  axi_ar_data_count(3) <= \<const0>\;
  axi_ar_data_count(2) <= \<const0>\;
  axi_ar_data_count(1) <= \<const0>\;
  axi_ar_data_count(0) <= \<const0>\;
  axi_ar_dbiterr <= \<const0>\;
  axi_ar_overflow <= \<const0>\;
  axi_ar_prog_empty <= \<const1>\;
  axi_ar_prog_full <= \<const0>\;
  axi_ar_rd_data_count(4) <= \<const0>\;
  axi_ar_rd_data_count(3) <= \<const0>\;
  axi_ar_rd_data_count(2) <= \<const0>\;
  axi_ar_rd_data_count(1) <= \<const0>\;
  axi_ar_rd_data_count(0) <= \<const0>\;
  axi_ar_sbiterr <= \<const0>\;
  axi_ar_underflow <= \<const0>\;
  axi_ar_wr_data_count(4) <= \<const0>\;
  axi_ar_wr_data_count(3) <= \<const0>\;
  axi_ar_wr_data_count(2) <= \<const0>\;
  axi_ar_wr_data_count(1) <= \<const0>\;
  axi_ar_wr_data_count(0) <= \<const0>\;
  axi_aw_data_count(4) <= \<const0>\;
  axi_aw_data_count(3) <= \<const0>\;
  axi_aw_data_count(2) <= \<const0>\;
  axi_aw_data_count(1) <= \<const0>\;
  axi_aw_data_count(0) <= \<const0>\;
  axi_aw_dbiterr <= \<const0>\;
  axi_aw_overflow <= \<const0>\;
  axi_aw_prog_empty <= \<const1>\;
  axi_aw_prog_full <= \<const0>\;
  axi_aw_rd_data_count(4) <= \<const0>\;
  axi_aw_rd_data_count(3) <= \<const0>\;
  axi_aw_rd_data_count(2) <= \<const0>\;
  axi_aw_rd_data_count(1) <= \<const0>\;
  axi_aw_rd_data_count(0) <= \<const0>\;
  axi_aw_sbiterr <= \<const0>\;
  axi_aw_underflow <= \<const0>\;
  axi_aw_wr_data_count(4) <= \<const0>\;
  axi_aw_wr_data_count(3) <= \<const0>\;
  axi_aw_wr_data_count(2) <= \<const0>\;
  axi_aw_wr_data_count(1) <= \<const0>\;
  axi_aw_wr_data_count(0) <= \<const0>\;
  axi_b_data_count(4) <= \<const0>\;
  axi_b_data_count(3) <= \<const0>\;
  axi_b_data_count(2) <= \<const0>\;
  axi_b_data_count(1) <= \<const0>\;
  axi_b_data_count(0) <= \<const0>\;
  axi_b_dbiterr <= \<const0>\;
  axi_b_overflow <= \<const0>\;
  axi_b_prog_empty <= \<const1>\;
  axi_b_prog_full <= \<const0>\;
  axi_b_rd_data_count(4) <= \<const0>\;
  axi_b_rd_data_count(3) <= \<const0>\;
  axi_b_rd_data_count(2) <= \<const0>\;
  axi_b_rd_data_count(1) <= \<const0>\;
  axi_b_rd_data_count(0) <= \<const0>\;
  axi_b_sbiterr <= \<const0>\;
  axi_b_underflow <= \<const0>\;
  axi_b_wr_data_count(4) <= \<const0>\;
  axi_b_wr_data_count(3) <= \<const0>\;
  axi_b_wr_data_count(2) <= \<const0>\;
  axi_b_wr_data_count(1) <= \<const0>\;
  axi_b_wr_data_count(0) <= \<const0>\;
  axi_r_data_count(10) <= \<const0>\;
  axi_r_data_count(9) <= \<const0>\;
  axi_r_data_count(8) <= \<const0>\;
  axi_r_data_count(7) <= \<const0>\;
  axi_r_data_count(6) <= \<const0>\;
  axi_r_data_count(5) <= \<const0>\;
  axi_r_data_count(4) <= \<const0>\;
  axi_r_data_count(3) <= \<const0>\;
  axi_r_data_count(2) <= \<const0>\;
  axi_r_data_count(1) <= \<const0>\;
  axi_r_data_count(0) <= \<const0>\;
  axi_r_dbiterr <= \<const0>\;
  axi_r_overflow <= \<const0>\;
  axi_r_prog_empty <= \<const1>\;
  axi_r_prog_full <= \<const0>\;
  axi_r_rd_data_count(10) <= \<const0>\;
  axi_r_rd_data_count(9) <= \<const0>\;
  axi_r_rd_data_count(8) <= \<const0>\;
  axi_r_rd_data_count(7) <= \<const0>\;
  axi_r_rd_data_count(6) <= \<const0>\;
  axi_r_rd_data_count(5) <= \<const0>\;
  axi_r_rd_data_count(4) <= \<const0>\;
  axi_r_rd_data_count(3) <= \<const0>\;
  axi_r_rd_data_count(2) <= \<const0>\;
  axi_r_rd_data_count(1) <= \<const0>\;
  axi_r_rd_data_count(0) <= \<const0>\;
  axi_r_sbiterr <= \<const0>\;
  axi_r_underflow <= \<const0>\;
  axi_r_wr_data_count(10) <= \<const0>\;
  axi_r_wr_data_count(9) <= \<const0>\;
  axi_r_wr_data_count(8) <= \<const0>\;
  axi_r_wr_data_count(7) <= \<const0>\;
  axi_r_wr_data_count(6) <= \<const0>\;
  axi_r_wr_data_count(5) <= \<const0>\;
  axi_r_wr_data_count(4) <= \<const0>\;
  axi_r_wr_data_count(3) <= \<const0>\;
  axi_r_wr_data_count(2) <= \<const0>\;
  axi_r_wr_data_count(1) <= \<const0>\;
  axi_r_wr_data_count(0) <= \<const0>\;
  axi_w_data_count(10) <= \<const0>\;
  axi_w_data_count(9) <= \<const0>\;
  axi_w_data_count(8) <= \<const0>\;
  axi_w_data_count(7) <= \<const0>\;
  axi_w_data_count(6) <= \<const0>\;
  axi_w_data_count(5) <= \<const0>\;
  axi_w_data_count(4) <= \<const0>\;
  axi_w_data_count(3) <= \<const0>\;
  axi_w_data_count(2) <= \<const0>\;
  axi_w_data_count(1) <= \<const0>\;
  axi_w_data_count(0) <= \<const0>\;
  axi_w_dbiterr <= \<const0>\;
  axi_w_overflow <= \<const0>\;
  axi_w_prog_empty <= \<const1>\;
  axi_w_prog_full <= \<const0>\;
  axi_w_rd_data_count(10) <= \<const0>\;
  axi_w_rd_data_count(9) <= \<const0>\;
  axi_w_rd_data_count(8) <= \<const0>\;
  axi_w_rd_data_count(7) <= \<const0>\;
  axi_w_rd_data_count(6) <= \<const0>\;
  axi_w_rd_data_count(5) <= \<const0>\;
  axi_w_rd_data_count(4) <= \<const0>\;
  axi_w_rd_data_count(3) <= \<const0>\;
  axi_w_rd_data_count(2) <= \<const0>\;
  axi_w_rd_data_count(1) <= \<const0>\;
  axi_w_rd_data_count(0) <= \<const0>\;
  axi_w_sbiterr <= \<const0>\;
  axi_w_underflow <= \<const0>\;
  axi_w_wr_data_count(10) <= \<const0>\;
  axi_w_wr_data_count(9) <= \<const0>\;
  axi_w_wr_data_count(8) <= \<const0>\;
  axi_w_wr_data_count(7) <= \<const0>\;
  axi_w_wr_data_count(6) <= \<const0>\;
  axi_w_wr_data_count(5) <= \<const0>\;
  axi_w_wr_data_count(4) <= \<const0>\;
  axi_w_wr_data_count(3) <= \<const0>\;
  axi_w_wr_data_count(2) <= \<const0>\;
  axi_w_wr_data_count(1) <= \<const0>\;
  axi_w_wr_data_count(0) <= \<const0>\;
  axis_data_count(10) <= \<const0>\;
  axis_data_count(9) <= \<const0>\;
  axis_data_count(8) <= \<const0>\;
  axis_data_count(7) <= \<const0>\;
  axis_data_count(6) <= \<const0>\;
  axis_data_count(5) <= \<const0>\;
  axis_data_count(4) <= \<const0>\;
  axis_data_count(3) <= \<const0>\;
  axis_data_count(2) <= \<const0>\;
  axis_data_count(1) <= \<const0>\;
  axis_data_count(0) <= \<const0>\;
  axis_dbiterr <= \<const0>\;
  axis_overflow <= \<const0>\;
  axis_prog_empty <= \<const1>\;
  axis_prog_full <= \<const0>\;
  axis_rd_data_count(10) <= \<const0>\;
  axis_rd_data_count(9) <= \<const0>\;
  axis_rd_data_count(8) <= \<const0>\;
  axis_rd_data_count(7) <= \<const0>\;
  axis_rd_data_count(6) <= \<const0>\;
  axis_rd_data_count(5) <= \<const0>\;
  axis_rd_data_count(4) <= \<const0>\;
  axis_rd_data_count(3) <= \<const0>\;
  axis_rd_data_count(2) <= \<const0>\;
  axis_rd_data_count(1) <= \<const0>\;
  axis_rd_data_count(0) <= \<const0>\;
  axis_sbiterr <= \<const0>\;
  axis_underflow <= \<const0>\;
  axis_wr_data_count(10) <= \<const0>\;
  axis_wr_data_count(9) <= \<const0>\;
  axis_wr_data_count(8) <= \<const0>\;
  axis_wr_data_count(7) <= \<const0>\;
  axis_wr_data_count(6) <= \<const0>\;
  axis_wr_data_count(5) <= \<const0>\;
  axis_wr_data_count(4) <= \<const0>\;
  axis_wr_data_count(3) <= \<const0>\;
  axis_wr_data_count(2) <= \<const0>\;
  axis_wr_data_count(1) <= \<const0>\;
  axis_wr_data_count(0) <= \<const0>\;
  data_count(5) <= \<const0>\;
  data_count(4) <= \<const0>\;
  data_count(3) <= \<const0>\;
  data_count(2) <= \<const0>\;
  data_count(1) <= \<const0>\;
  data_count(0) <= \<const0>\;
  dbiterr <= \<const0>\;
  m_axi_araddr(31) <= \<const0>\;
  m_axi_araddr(30) <= \<const0>\;
  m_axi_araddr(29) <= \<const0>\;
  m_axi_araddr(28) <= \<const0>\;
  m_axi_araddr(27) <= \<const0>\;
  m_axi_araddr(26) <= \<const0>\;
  m_axi_araddr(25) <= \<const0>\;
  m_axi_araddr(24) <= \<const0>\;
  m_axi_araddr(23) <= \<const0>\;
  m_axi_araddr(22) <= \<const0>\;
  m_axi_araddr(21) <= \<const0>\;
  m_axi_araddr(20) <= \<const0>\;
  m_axi_araddr(19) <= \<const0>\;
  m_axi_araddr(18) <= \<const0>\;
  m_axi_araddr(17) <= \<const0>\;
  m_axi_araddr(16) <= \<const0>\;
  m_axi_araddr(15) <= \<const0>\;
  m_axi_araddr(14) <= \<const0>\;
  m_axi_araddr(13) <= \<const0>\;
  m_axi_araddr(12) <= \<const0>\;
  m_axi_araddr(11) <= \<const0>\;
  m_axi_araddr(10) <= \<const0>\;
  m_axi_araddr(9) <= \<const0>\;
  m_axi_araddr(8) <= \<const0>\;
  m_axi_araddr(7) <= \<const0>\;
  m_axi_araddr(6) <= \<const0>\;
  m_axi_araddr(5) <= \<const0>\;
  m_axi_araddr(4) <= \<const0>\;
  m_axi_araddr(3) <= \<const0>\;
  m_axi_araddr(2) <= \<const0>\;
  m_axi_araddr(1) <= \<const0>\;
  m_axi_araddr(0) <= \<const0>\;
  m_axi_arburst(1) <= \<const0>\;
  m_axi_arburst(0) <= \<const0>\;
  m_axi_arcache(3) <= \<const0>\;
  m_axi_arcache(2) <= \<const0>\;
  m_axi_arcache(1) <= \<const0>\;
  m_axi_arcache(0) <= \<const0>\;
  m_axi_arid(0) <= \<const0>\;
  m_axi_arlen(7) <= \<const0>\;
  m_axi_arlen(6) <= \<const0>\;
  m_axi_arlen(5) <= \<const0>\;
  m_axi_arlen(4) <= \<const0>\;
  m_axi_arlen(3) <= \<const0>\;
  m_axi_arlen(2) <= \<const0>\;
  m_axi_arlen(1) <= \<const0>\;
  m_axi_arlen(0) <= \<const0>\;
  m_axi_arlock(0) <= \<const0>\;
  m_axi_arprot(2) <= \<const0>\;
  m_axi_arprot(1) <= \<const0>\;
  m_axi_arprot(0) <= \<const0>\;
  m_axi_arqos(3) <= \<const0>\;
  m_axi_arqos(2) <= \<const0>\;
  m_axi_arqos(1) <= \<const0>\;
  m_axi_arqos(0) <= \<const0>\;
  m_axi_arregion(3) <= \<const0>\;
  m_axi_arregion(2) <= \<const0>\;
  m_axi_arregion(1) <= \<const0>\;
  m_axi_arregion(0) <= \<const0>\;
  m_axi_arsize(2) <= \<const0>\;
  m_axi_arsize(1) <= \<const0>\;
  m_axi_arsize(0) <= \<const0>\;
  m_axi_aruser(0) <= \<const0>\;
  m_axi_arvalid <= \<const0>\;
  m_axi_awaddr(31) <= \<const0>\;
  m_axi_awaddr(30) <= \<const0>\;
  m_axi_awaddr(29) <= \<const0>\;
  m_axi_awaddr(28) <= \<const0>\;
  m_axi_awaddr(27) <= \<const0>\;
  m_axi_awaddr(26) <= \<const0>\;
  m_axi_awaddr(25) <= \<const0>\;
  m_axi_awaddr(24) <= \<const0>\;
  m_axi_awaddr(23) <= \<const0>\;
  m_axi_awaddr(22) <= \<const0>\;
  m_axi_awaddr(21) <= \<const0>\;
  m_axi_awaddr(20) <= \<const0>\;
  m_axi_awaddr(19) <= \<const0>\;
  m_axi_awaddr(18) <= \<const0>\;
  m_axi_awaddr(17) <= \<const0>\;
  m_axi_awaddr(16) <= \<const0>\;
  m_axi_awaddr(15) <= \<const0>\;
  m_axi_awaddr(14) <= \<const0>\;
  m_axi_awaddr(13) <= \<const0>\;
  m_axi_awaddr(12) <= \<const0>\;
  m_axi_awaddr(11) <= \<const0>\;
  m_axi_awaddr(10) <= \<const0>\;
  m_axi_awaddr(9) <= \<const0>\;
  m_axi_awaddr(8) <= \<const0>\;
  m_axi_awaddr(7) <= \<const0>\;
  m_axi_awaddr(6) <= \<const0>\;
  m_axi_awaddr(5) <= \<const0>\;
  m_axi_awaddr(4) <= \<const0>\;
  m_axi_awaddr(3) <= \<const0>\;
  m_axi_awaddr(2) <= \<const0>\;
  m_axi_awaddr(1) <= \<const0>\;
  m_axi_awaddr(0) <= \<const0>\;
  m_axi_awburst(1) <= \<const0>\;
  m_axi_awburst(0) <= \<const0>\;
  m_axi_awcache(3) <= \<const0>\;
  m_axi_awcache(2) <= \<const0>\;
  m_axi_awcache(1) <= \<const0>\;
  m_axi_awcache(0) <= \<const0>\;
  m_axi_awid(0) <= \<const0>\;
  m_axi_awlen(7) <= \<const0>\;
  m_axi_awlen(6) <= \<const0>\;
  m_axi_awlen(5) <= \<const0>\;
  m_axi_awlen(4) <= \<const0>\;
  m_axi_awlen(3) <= \<const0>\;
  m_axi_awlen(2) <= \<const0>\;
  m_axi_awlen(1) <= \<const0>\;
  m_axi_awlen(0) <= \<const0>\;
  m_axi_awlock(0) <= \<const0>\;
  m_axi_awprot(2) <= \<const0>\;
  m_axi_awprot(1) <= \<const0>\;
  m_axi_awprot(0) <= \<const0>\;
  m_axi_awqos(3) <= \<const0>\;
  m_axi_awqos(2) <= \<const0>\;
  m_axi_awqos(1) <= \<const0>\;
  m_axi_awqos(0) <= \<const0>\;
  m_axi_awregion(3) <= \<const0>\;
  m_axi_awregion(2) <= \<const0>\;
  m_axi_awregion(1) <= \<const0>\;
  m_axi_awregion(0) <= \<const0>\;
  m_axi_awsize(2) <= \<const0>\;
  m_axi_awsize(1) <= \<const0>\;
  m_axi_awsize(0) <= \<const0>\;
  m_axi_awuser(0) <= \<const0>\;
  m_axi_awvalid <= \<const0>\;
  m_axi_bready <= \<const0>\;
  m_axi_rready <= \<const0>\;
  m_axi_wdata(63) <= \<const0>\;
  m_axi_wdata(62) <= \<const0>\;
  m_axi_wdata(61) <= \<const0>\;
  m_axi_wdata(60) <= \<const0>\;
  m_axi_wdata(59) <= \<const0>\;
  m_axi_wdata(58) <= \<const0>\;
  m_axi_wdata(57) <= \<const0>\;
  m_axi_wdata(56) <= \<const0>\;
  m_axi_wdata(55) <= \<const0>\;
  m_axi_wdata(54) <= \<const0>\;
  m_axi_wdata(53) <= \<const0>\;
  m_axi_wdata(52) <= \<const0>\;
  m_axi_wdata(51) <= \<const0>\;
  m_axi_wdata(50) <= \<const0>\;
  m_axi_wdata(49) <= \<const0>\;
  m_axi_wdata(48) <= \<const0>\;
  m_axi_wdata(47) <= \<const0>\;
  m_axi_wdata(46) <= \<const0>\;
  m_axi_wdata(45) <= \<const0>\;
  m_axi_wdata(44) <= \<const0>\;
  m_axi_wdata(43) <= \<const0>\;
  m_axi_wdata(42) <= \<const0>\;
  m_axi_wdata(41) <= \<const0>\;
  m_axi_wdata(40) <= \<const0>\;
  m_axi_wdata(39) <= \<const0>\;
  m_axi_wdata(38) <= \<const0>\;
  m_axi_wdata(37) <= \<const0>\;
  m_axi_wdata(36) <= \<const0>\;
  m_axi_wdata(35) <= \<const0>\;
  m_axi_wdata(34) <= \<const0>\;
  m_axi_wdata(33) <= \<const0>\;
  m_axi_wdata(32) <= \<const0>\;
  m_axi_wdata(31) <= \<const0>\;
  m_axi_wdata(30) <= \<const0>\;
  m_axi_wdata(29) <= \<const0>\;
  m_axi_wdata(28) <= \<const0>\;
  m_axi_wdata(27) <= \<const0>\;
  m_axi_wdata(26) <= \<const0>\;
  m_axi_wdata(25) <= \<const0>\;
  m_axi_wdata(24) <= \<const0>\;
  m_axi_wdata(23) <= \<const0>\;
  m_axi_wdata(22) <= \<const0>\;
  m_axi_wdata(21) <= \<const0>\;
  m_axi_wdata(20) <= \<const0>\;
  m_axi_wdata(19) <= \<const0>\;
  m_axi_wdata(18) <= \<const0>\;
  m_axi_wdata(17) <= \<const0>\;
  m_axi_wdata(16) <= \<const0>\;
  m_axi_wdata(15) <= \<const0>\;
  m_axi_wdata(14) <= \<const0>\;
  m_axi_wdata(13) <= \<const0>\;
  m_axi_wdata(12) <= \<const0>\;
  m_axi_wdata(11) <= \<const0>\;
  m_axi_wdata(10) <= \<const0>\;
  m_axi_wdata(9) <= \<const0>\;
  m_axi_wdata(8) <= \<const0>\;
  m_axi_wdata(7) <= \<const0>\;
  m_axi_wdata(6) <= \<const0>\;
  m_axi_wdata(5) <= \<const0>\;
  m_axi_wdata(4) <= \<const0>\;
  m_axi_wdata(3) <= \<const0>\;
  m_axi_wdata(2) <= \<const0>\;
  m_axi_wdata(1) <= \<const0>\;
  m_axi_wdata(0) <= \<const0>\;
  m_axi_wid(0) <= \<const0>\;
  m_axi_wlast <= \<const0>\;
  m_axi_wstrb(7) <= \<const0>\;
  m_axi_wstrb(6) <= \<const0>\;
  m_axi_wstrb(5) <= \<const0>\;
  m_axi_wstrb(4) <= \<const0>\;
  m_axi_wstrb(3) <= \<const0>\;
  m_axi_wstrb(2) <= \<const0>\;
  m_axi_wstrb(1) <= \<const0>\;
  m_axi_wstrb(0) <= \<const0>\;
  m_axi_wuser(0) <= \<const0>\;
  m_axi_wvalid <= \<const0>\;
  m_axis_tdata(7) <= \<const0>\;
  m_axis_tdata(6) <= \<const0>\;
  m_axis_tdata(5) <= \<const0>\;
  m_axis_tdata(4) <= \<const0>\;
  m_axis_tdata(3) <= \<const0>\;
  m_axis_tdata(2) <= \<const0>\;
  m_axis_tdata(1) <= \<const0>\;
  m_axis_tdata(0) <= \<const0>\;
  m_axis_tdest(0) <= \<const0>\;
  m_axis_tid(0) <= \<const0>\;
  m_axis_tkeep(0) <= \<const0>\;
  m_axis_tlast <= \<const0>\;
  m_axis_tstrb(0) <= \<const0>\;
  m_axis_tuser(3) <= \<const0>\;
  m_axis_tuser(2) <= \<const0>\;
  m_axis_tuser(1) <= \<const0>\;
  m_axis_tuser(0) <= \<const0>\;
  m_axis_tvalid <= \<const0>\;
  overflow <= \<const0>\;
  prog_empty <= \<const0>\;
  rd_data_count(5) <= \<const0>\;
  rd_data_count(4) <= \<const0>\;
  rd_data_count(3) <= \<const0>\;
  rd_data_count(2) <= \<const0>\;
  rd_data_count(1) <= \<const0>\;
  rd_data_count(0) <= \<const0>\;
  rd_rst_busy <= \<const0>\;
  s_axi_arready <= \<const0>\;
  s_axi_awready <= \<const0>\;
  s_axi_bid(0) <= \<const0>\;
  s_axi_bresp(1) <= \<const0>\;
  s_axi_bresp(0) <= \<const0>\;
  s_axi_buser(0) <= \<const0>\;
  s_axi_bvalid <= \<const0>\;
  s_axi_rdata(63) <= \<const0>\;
  s_axi_rdata(62) <= \<const0>\;
  s_axi_rdata(61) <= \<const0>\;
  s_axi_rdata(60) <= \<const0>\;
  s_axi_rdata(59) <= \<const0>\;
  s_axi_rdata(58) <= \<const0>\;
  s_axi_rdata(57) <= \<const0>\;
  s_axi_rdata(56) <= \<const0>\;
  s_axi_rdata(55) <= \<const0>\;
  s_axi_rdata(54) <= \<const0>\;
  s_axi_rdata(53) <= \<const0>\;
  s_axi_rdata(52) <= \<const0>\;
  s_axi_rdata(51) <= \<const0>\;
  s_axi_rdata(50) <= \<const0>\;
  s_axi_rdata(49) <= \<const0>\;
  s_axi_rdata(48) <= \<const0>\;
  s_axi_rdata(47) <= \<const0>\;
  s_axi_rdata(46) <= \<const0>\;
  s_axi_rdata(45) <= \<const0>\;
  s_axi_rdata(44) <= \<const0>\;
  s_axi_rdata(43) <= \<const0>\;
  s_axi_rdata(42) <= \<const0>\;
  s_axi_rdata(41) <= \<const0>\;
  s_axi_rdata(40) <= \<const0>\;
  s_axi_rdata(39) <= \<const0>\;
  s_axi_rdata(38) <= \<const0>\;
  s_axi_rdata(37) <= \<const0>\;
  s_axi_rdata(36) <= \<const0>\;
  s_axi_rdata(35) <= \<const0>\;
  s_axi_rdata(34) <= \<const0>\;
  s_axi_rdata(33) <= \<const0>\;
  s_axi_rdata(32) <= \<const0>\;
  s_axi_rdata(31) <= \<const0>\;
  s_axi_rdata(30) <= \<const0>\;
  s_axi_rdata(29) <= \<const0>\;
  s_axi_rdata(28) <= \<const0>\;
  s_axi_rdata(27) <= \<const0>\;
  s_axi_rdata(26) <= \<const0>\;
  s_axi_rdata(25) <= \<const0>\;
  s_axi_rdata(24) <= \<const0>\;
  s_axi_rdata(23) <= \<const0>\;
  s_axi_rdata(22) <= \<const0>\;
  s_axi_rdata(21) <= \<const0>\;
  s_axi_rdata(20) <= \<const0>\;
  s_axi_rdata(19) <= \<const0>\;
  s_axi_rdata(18) <= \<const0>\;
  s_axi_rdata(17) <= \<const0>\;
  s_axi_rdata(16) <= \<const0>\;
  s_axi_rdata(15) <= \<const0>\;
  s_axi_rdata(14) <= \<const0>\;
  s_axi_rdata(13) <= \<const0>\;
  s_axi_rdata(12) <= \<const0>\;
  s_axi_rdata(11) <= \<const0>\;
  s_axi_rdata(10) <= \<const0>\;
  s_axi_rdata(9) <= \<const0>\;
  s_axi_rdata(8) <= \<const0>\;
  s_axi_rdata(7) <= \<const0>\;
  s_axi_rdata(6) <= \<const0>\;
  s_axi_rdata(5) <= \<const0>\;
  s_axi_rdata(4) <= \<const0>\;
  s_axi_rdata(3) <= \<const0>\;
  s_axi_rdata(2) <= \<const0>\;
  s_axi_rdata(1) <= \<const0>\;
  s_axi_rdata(0) <= \<const0>\;
  s_axi_rid(0) <= \<const0>\;
  s_axi_rlast <= \<const0>\;
  s_axi_rresp(1) <= \<const0>\;
  s_axi_rresp(0) <= \<const0>\;
  s_axi_ruser(0) <= \<const0>\;
  s_axi_rvalid <= \<const0>\;
  s_axi_wready <= \<const0>\;
  s_axis_tready <= \<const0>\;
  sbiterr <= \<const0>\;
  underflow <= \<const0>\;
  valid <= \<const0>\;
  wr_ack <= \<const0>\;
  wr_data_count(5) <= \<const0>\;
  wr_data_count(4) <= \<const0>\;
  wr_data_count(3) <= \<const0>\;
  wr_data_count(2) <= \<const0>\;
  wr_data_count(1) <= \<const0>\;
  wr_data_count(0) <= \<const0>\;
  wr_rst_busy <= \<const0>\;
GND: unisim.vcomponents.GND
     port map (
      G => \<const0>\
    );
VCC: unisim.vcomponents.VCC
     port map (
      P => \<const1>\
    );
inst_fifo_gen: entity work.dram_rd_ram0_0_fifo_generator_v12_0_synth
     port map (
      din(31 downto 0) => din(31 downto 0),
      dout(31 downto 0) => dout(31 downto 0),
      empty => empty,
      full => full,
      prog_full => prog_full,
      rd_clk => rd_clk,
      rd_en => rd_en,
      rst => rst,
      wr_clk => wr_clk,
      wr_en => wr_en
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_fifo_32_prog_full is
  port (
    rst : in STD_LOGIC;
    wr_clk : in STD_LOGIC;
    rd_clk : in STD_LOGIC;
    din : in STD_LOGIC_VECTOR ( 31 downto 0 );
    wr_en : in STD_LOGIC;
    rd_en : in STD_LOGIC;
    dout : out STD_LOGIC_VECTOR ( 31 downto 0 );
    full : out STD_LOGIC;
    empty : out STD_LOGIC;
    prog_full : out STD_LOGIC
  );
  attribute CHECK_LICENSE_TYPE : string;
  attribute CHECK_LICENSE_TYPE of dram_rd_ram0_0_fifo_32_prog_full : entity is "fifo_32_prog_full,fifo_generator_v12_0,{}";
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_fifo_32_prog_full : entity is "fifo_32_prog_full";
  attribute core_generation_info : string;
  attribute core_generation_info of dram_rd_ram0_0_fifo_32_prog_full : entity is "fifo_32_prog_full,fifo_generator_v12_0,{x_ipProduct=Vivado 2015.2,x_ipVendor=xilinx.com,x_ipLibrary=ip,x_ipName=fifo_generator,x_ipVersion=12.0,x_ipCoreRevision=4,x_ipLanguage=VERILOG,x_ipSimLanguage=MIXED,C_COMMON_CLOCK=0,C_COUNT_TYPE=0,C_DATA_COUNT_WIDTH=6,C_DEFAULT_VALUE=BlankString,C_DIN_WIDTH=32,C_DOUT_RST_VAL=0,C_DOUT_WIDTH=32,C_ENABLE_RLOCS=0,C_FAMILY=zynq,C_FULL_FLAGS_RST_VAL=1,C_HAS_ALMOST_EMPTY=0,C_HAS_ALMOST_FULL=0,C_HAS_BACKUP=0,C_HAS_DATA_COUNT=0,C_HAS_INT_CLK=0,C_HAS_MEMINIT_FILE=0,C_HAS_OVERFLOW=0,C_HAS_RD_DATA_COUNT=0,C_HAS_RD_RST=0,C_HAS_RST=1,C_HAS_SRST=0,C_HAS_UNDERFLOW=0,C_HAS_VALID=0,C_HAS_WR_ACK=0,C_HAS_WR_DATA_COUNT=0,C_HAS_WR_RST=0,C_IMPLEMENTATION_TYPE=2,C_INIT_WR_PNTR_VAL=0,C_MEMORY_TYPE=1,C_MIF_FILE_NAME=BlankString,C_OPTIMIZATION_MODE=0,C_OVERFLOW_LOW=0,C_PRELOAD_LATENCY=0,C_PRELOAD_REGS=1,C_PRIM_FIFO_TYPE=512x36,C_PROG_EMPTY_THRESH_ASSERT_VAL=4,C_PROG_EMPTY_THRESH_NEGATE_VAL=5,C_PROG_EMPTY_TYPE=0,C_PROG_FULL_THRESH_ASSERT_VAL=50,C_PROG_FULL_THRESH_NEGATE_VAL=49,C_PROG_FULL_TYPE=1,C_RD_DATA_COUNT_WIDTH=6,C_RD_DEPTH=64,C_RD_FREQ=1,C_RD_PNTR_WIDTH=6,C_UNDERFLOW_LOW=0,C_USE_DOUT_RST=1,C_USE_ECC=0,C_USE_EMBEDDED_REG=0,C_USE_PIPELINE_REG=0,C_POWER_SAVING_MODE=0,C_USE_FIFO16_FLAGS=0,C_USE_FWFT_DATA_COUNT=0,C_VALID_LOW=0,C_WR_ACK_LOW=0,C_WR_DATA_COUNT_WIDTH=6,C_WR_DEPTH=64,C_WR_FREQ=1,C_WR_PNTR_WIDTH=6,C_WR_RESPONSE_LATENCY=1,C_MSGON_VAL=1,C_ENABLE_RST_SYNC=1,C_ERROR_INJECTION_TYPE=0,C_SYNCHRONIZER_STAGE=2,C_INTERFACE_TYPE=0,C_AXI_TYPE=1,C_HAS_AXI_WR_CHANNEL=1,C_HAS_AXI_RD_CHANNEL=1,C_HAS_SLAVE_CE=0,C_HAS_MASTER_CE=0,C_ADD_NGC_CONSTRAINT=0,C_USE_COMMON_OVERFLOW=0,C_USE_COMMON_UNDERFLOW=0,C_USE_DEFAULT_SETTINGS=0,C_AXI_ID_WIDTH=1,C_AXI_ADDR_WIDTH=32,C_AXI_DATA_WIDTH=64,C_AXI_LEN_WIDTH=8,C_AXI_LOCK_WIDTH=1,C_HAS_AXI_ID=0,C_HAS_AXI_AWUSER=0,C_HAS_AXI_WUSER=0,C_HAS_AXI_BUSER=0,C_HAS_AXI_ARUSER=0,C_HAS_AXI_RUSER=0,C_AXI_ARUSER_WIDTH=1,C_AXI_AWUSER_WIDTH=1,C_AXI_WUSER_WIDTH=1,C_AXI_BUSER_WIDTH=1,C_AXI_RUSER_WIDTH=1,C_HAS_AXIS_TDATA=1,C_HAS_AXIS_TID=0,C_HAS_AXIS_TDEST=0,C_HAS_AXIS_TUSER=1,C_HAS_AXIS_TREADY=1,C_HAS_AXIS_TLAST=0,C_HAS_AXIS_TSTRB=0,C_HAS_AXIS_TKEEP=0,C_AXIS_TDATA_WIDTH=8,C_AXIS_TID_WIDTH=1,C_AXIS_TDEST_WIDTH=1,C_AXIS_TUSER_WIDTH=4,C_AXIS_TSTRB_WIDTH=1,C_AXIS_TKEEP_WIDTH=1,C_WACH_TYPE=0,C_WDCH_TYPE=0,C_WRCH_TYPE=0,C_RACH_TYPE=0,C_RDCH_TYPE=0,C_AXIS_TYPE=0,C_IMPLEMENTATION_TYPE_WACH=1,C_IMPLEMENTATION_TYPE_WDCH=1,C_IMPLEMENTATION_TYPE_WRCH=1,C_IMPLEMENTATION_TYPE_RACH=1,C_IMPLEMENTATION_TYPE_RDCH=1,C_IMPLEMENTATION_TYPE_AXIS=1,C_APPLICATION_TYPE_WACH=0,C_APPLICATION_TYPE_WDCH=0,C_APPLICATION_TYPE_WRCH=0,C_APPLICATION_TYPE_RACH=0,C_APPLICATION_TYPE_RDCH=0,C_APPLICATION_TYPE_AXIS=0,C_PRIM_FIFO_TYPE_WACH=512x36,C_PRIM_FIFO_TYPE_WDCH=1kx36,C_PRIM_FIFO_TYPE_WRCH=512x36,C_PRIM_FIFO_TYPE_RACH=512x36,C_PRIM_FIFO_TYPE_RDCH=1kx36,C_PRIM_FIFO_TYPE_AXIS=1kx18,C_USE_ECC_WACH=0,C_USE_ECC_WDCH=0,C_USE_ECC_WRCH=0,C_USE_ECC_RACH=0,C_USE_ECC_RDCH=0,C_USE_ECC_AXIS=0,C_ERROR_INJECTION_TYPE_WACH=0,C_ERROR_INJECTION_TYPE_WDCH=0,C_ERROR_INJECTION_TYPE_WRCH=0,C_ERROR_INJECTION_TYPE_RACH=0,C_ERROR_INJECTION_TYPE_RDCH=0,C_ERROR_INJECTION_TYPE_AXIS=0,C_DIN_WIDTH_WACH=32,C_DIN_WIDTH_WDCH=64,C_DIN_WIDTH_WRCH=2,C_DIN_WIDTH_RACH=32,C_DIN_WIDTH_RDCH=64,C_DIN_WIDTH_AXIS=1,C_WR_DEPTH_WACH=16,C_WR_DEPTH_WDCH=1024,C_WR_DEPTH_WRCH=16,C_WR_DEPTH_RACH=16,C_WR_DEPTH_RDCH=1024,C_WR_DEPTH_AXIS=1024,C_WR_PNTR_WIDTH_WACH=4,C_WR_PNTR_WIDTH_WDCH=10,C_WR_PNTR_WIDTH_WRCH=4,C_WR_PNTR_WIDTH_RACH=4,C_WR_PNTR_WIDTH_RDCH=10,C_WR_PNTR_WIDTH_AXIS=10,C_HAS_DATA_COUNTS_WACH=0,C_HAS_DATA_COUNTS_WDCH=0,C_HAS_DATA_COUNTS_WRCH=0,C_HAS_DATA_COUNTS_RACH=0,C_HAS_DATA_COUNTS_RDCH=0,C_HAS_DATA_COUNTS_AXIS=0,C_HAS_PROG_FLAGS_WACH=0,C_HAS_PROG_FLAGS_WDCH=0,C_HAS_PROG_FLAGS_WRCH=0,C_HAS_PROG_FLAGS_RACH=0,C_HAS_PROG_FLAGS_RDCH=0,C_HAS_PROG_FLAGS_AXIS=0,C_PROG_FULL_TYPE_WACH=0,C_PROG_FULL_TYPE_WDCH=0,C_PROG_FULL_TYPE_WRCH=0,C_PROG_FULL_TYPE_RACH=0,C_PROG_FULL_TYPE_RDCH=0,C_PROG_FULL_TYPE_AXIS=0,C_PROG_FULL_THRESH_ASSERT_VAL_WACH=1023,C_PROG_FULL_THRESH_ASSERT_VAL_WDCH=1023,C_PROG_FULL_THRESH_ASSERT_VAL_WRCH=1023,C_PROG_FULL_THRESH_ASSERT_VAL_RACH=1023,C_PROG_FULL_THRESH_ASSERT_VAL_RDCH=1023,C_PROG_FULL_THRESH_ASSERT_VAL_AXIS=1023,C_PROG_EMPTY_TYPE_WACH=0,C_PROG_EMPTY_TYPE_WDCH=0,C_PROG_EMPTY_TYPE_WRCH=0,C_PROG_EMPTY_TYPE_RACH=0,C_PROG_EMPTY_TYPE_RDCH=0,C_PROG_EMPTY_TYPE_AXIS=0,C_PROG_EMPTY_THRESH_ASSERT_VAL_WACH=1022,C_PROG_EMPTY_THRESH_ASSERT_VAL_WDCH=1022,C_PROG_EMPTY_THRESH_ASSERT_VAL_WRCH=1022,C_PROG_EMPTY_THRESH_ASSERT_VAL_RACH=1022,C_PROG_EMPTY_THRESH_ASSERT_VAL_RDCH=1022,C_PROG_EMPTY_THRESH_ASSERT_VAL_AXIS=1022,C_REG_SLICE_MODE_WACH=0,C_REG_SLICE_MODE_WDCH=0,C_REG_SLICE_MODE_WRCH=0,C_REG_SLICE_MODE_RACH=0,C_REG_SLICE_MODE_RDCH=0,C_REG_SLICE_MODE_AXIS=0}";
  attribute downgradeipidentifiedwarnings : string;
  attribute downgradeipidentifiedwarnings of dram_rd_ram0_0_fifo_32_prog_full : entity is "yes";
  attribute x_core_info : string;
  attribute x_core_info of dram_rd_ram0_0_fifo_32_prog_full : entity is "fifo_generator_v12_0,Vivado 2015.2";
end dram_rd_ram0_0_fifo_32_prog_full;

architecture STRUCTURE of dram_rd_ram0_0_fifo_32_prog_full is
  signal NLW_U0_almost_empty_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_almost_full_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_ar_dbiterr_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_ar_overflow_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_ar_prog_empty_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_ar_prog_full_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_ar_sbiterr_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_ar_underflow_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_aw_dbiterr_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_aw_overflow_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_aw_prog_empty_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_aw_prog_full_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_aw_sbiterr_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_aw_underflow_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_b_dbiterr_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_b_overflow_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_b_prog_empty_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_b_prog_full_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_b_sbiterr_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_b_underflow_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_r_dbiterr_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_r_overflow_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_r_prog_empty_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_r_prog_full_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_r_sbiterr_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_r_underflow_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_w_dbiterr_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_w_overflow_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_w_prog_empty_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_w_prog_full_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_w_sbiterr_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_w_underflow_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axis_dbiterr_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axis_overflow_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axis_prog_empty_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axis_prog_full_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axis_sbiterr_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axis_underflow_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_dbiterr_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_m_axi_arvalid_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_m_axi_awvalid_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_m_axi_bready_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_m_axi_rready_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_m_axi_wlast_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_m_axi_wvalid_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_m_axis_tlast_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_m_axis_tvalid_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_overflow_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_prog_empty_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_rd_rst_busy_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_s_axi_arready_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_s_axi_awready_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_s_axi_bvalid_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_s_axi_rlast_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_s_axi_rvalid_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_s_axi_wready_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_s_axis_tready_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_sbiterr_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_underflow_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_valid_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_wr_ack_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_wr_rst_busy_UNCONNECTED : STD_LOGIC;
  signal NLW_U0_axi_ar_data_count_UNCONNECTED : STD_LOGIC_VECTOR ( 4 downto 0 );
  signal NLW_U0_axi_ar_rd_data_count_UNCONNECTED : STD_LOGIC_VECTOR ( 4 downto 0 );
  signal NLW_U0_axi_ar_wr_data_count_UNCONNECTED : STD_LOGIC_VECTOR ( 4 downto 0 );
  signal NLW_U0_axi_aw_data_count_UNCONNECTED : STD_LOGIC_VECTOR ( 4 downto 0 );
  signal NLW_U0_axi_aw_rd_data_count_UNCONNECTED : STD_LOGIC_VECTOR ( 4 downto 0 );
  signal NLW_U0_axi_aw_wr_data_count_UNCONNECTED : STD_LOGIC_VECTOR ( 4 downto 0 );
  signal NLW_U0_axi_b_data_count_UNCONNECTED : STD_LOGIC_VECTOR ( 4 downto 0 );
  signal NLW_U0_axi_b_rd_data_count_UNCONNECTED : STD_LOGIC_VECTOR ( 4 downto 0 );
  signal NLW_U0_axi_b_wr_data_count_UNCONNECTED : STD_LOGIC_VECTOR ( 4 downto 0 );
  signal NLW_U0_axi_r_data_count_UNCONNECTED : STD_LOGIC_VECTOR ( 10 downto 0 );
  signal NLW_U0_axi_r_rd_data_count_UNCONNECTED : STD_LOGIC_VECTOR ( 10 downto 0 );
  signal NLW_U0_axi_r_wr_data_count_UNCONNECTED : STD_LOGIC_VECTOR ( 10 downto 0 );
  signal NLW_U0_axi_w_data_count_UNCONNECTED : STD_LOGIC_VECTOR ( 10 downto 0 );
  signal NLW_U0_axi_w_rd_data_count_UNCONNECTED : STD_LOGIC_VECTOR ( 10 downto 0 );
  signal NLW_U0_axi_w_wr_data_count_UNCONNECTED : STD_LOGIC_VECTOR ( 10 downto 0 );
  signal NLW_U0_axis_data_count_UNCONNECTED : STD_LOGIC_VECTOR ( 10 downto 0 );
  signal NLW_U0_axis_rd_data_count_UNCONNECTED : STD_LOGIC_VECTOR ( 10 downto 0 );
  signal NLW_U0_axis_wr_data_count_UNCONNECTED : STD_LOGIC_VECTOR ( 10 downto 0 );
  signal NLW_U0_data_count_UNCONNECTED : STD_LOGIC_VECTOR ( 5 downto 0 );
  signal NLW_U0_m_axi_araddr_UNCONNECTED : STD_LOGIC_VECTOR ( 31 downto 0 );
  signal NLW_U0_m_axi_arburst_UNCONNECTED : STD_LOGIC_VECTOR ( 1 downto 0 );
  signal NLW_U0_m_axi_arcache_UNCONNECTED : STD_LOGIC_VECTOR ( 3 downto 0 );
  signal NLW_U0_m_axi_arid_UNCONNECTED : STD_LOGIC_VECTOR ( 0 to 0 );
  signal NLW_U0_m_axi_arlen_UNCONNECTED : STD_LOGIC_VECTOR ( 7 downto 0 );
  signal NLW_U0_m_axi_arlock_UNCONNECTED : STD_LOGIC_VECTOR ( 0 to 0 );
  signal NLW_U0_m_axi_arprot_UNCONNECTED : STD_LOGIC_VECTOR ( 2 downto 0 );
  signal NLW_U0_m_axi_arqos_UNCONNECTED : STD_LOGIC_VECTOR ( 3 downto 0 );
  signal NLW_U0_m_axi_arregion_UNCONNECTED : STD_LOGIC_VECTOR ( 3 downto 0 );
  signal NLW_U0_m_axi_arsize_UNCONNECTED : STD_LOGIC_VECTOR ( 2 downto 0 );
  signal NLW_U0_m_axi_aruser_UNCONNECTED : STD_LOGIC_VECTOR ( 0 to 0 );
  signal NLW_U0_m_axi_awaddr_UNCONNECTED : STD_LOGIC_VECTOR ( 31 downto 0 );
  signal NLW_U0_m_axi_awburst_UNCONNECTED : STD_LOGIC_VECTOR ( 1 downto 0 );
  signal NLW_U0_m_axi_awcache_UNCONNECTED : STD_LOGIC_VECTOR ( 3 downto 0 );
  signal NLW_U0_m_axi_awid_UNCONNECTED : STD_LOGIC_VECTOR ( 0 to 0 );
  signal NLW_U0_m_axi_awlen_UNCONNECTED : STD_LOGIC_VECTOR ( 7 downto 0 );
  signal NLW_U0_m_axi_awlock_UNCONNECTED : STD_LOGIC_VECTOR ( 0 to 0 );
  signal NLW_U0_m_axi_awprot_UNCONNECTED : STD_LOGIC_VECTOR ( 2 downto 0 );
  signal NLW_U0_m_axi_awqos_UNCONNECTED : STD_LOGIC_VECTOR ( 3 downto 0 );
  signal NLW_U0_m_axi_awregion_UNCONNECTED : STD_LOGIC_VECTOR ( 3 downto 0 );
  signal NLW_U0_m_axi_awsize_UNCONNECTED : STD_LOGIC_VECTOR ( 2 downto 0 );
  signal NLW_U0_m_axi_awuser_UNCONNECTED : STD_LOGIC_VECTOR ( 0 to 0 );
  signal NLW_U0_m_axi_wdata_UNCONNECTED : STD_LOGIC_VECTOR ( 63 downto 0 );
  signal NLW_U0_m_axi_wid_UNCONNECTED : STD_LOGIC_VECTOR ( 0 to 0 );
  signal NLW_U0_m_axi_wstrb_UNCONNECTED : STD_LOGIC_VECTOR ( 7 downto 0 );
  signal NLW_U0_m_axi_wuser_UNCONNECTED : STD_LOGIC_VECTOR ( 0 to 0 );
  signal NLW_U0_m_axis_tdata_UNCONNECTED : STD_LOGIC_VECTOR ( 7 downto 0 );
  signal NLW_U0_m_axis_tdest_UNCONNECTED : STD_LOGIC_VECTOR ( 0 to 0 );
  signal NLW_U0_m_axis_tid_UNCONNECTED : STD_LOGIC_VECTOR ( 0 to 0 );
  signal NLW_U0_m_axis_tkeep_UNCONNECTED : STD_LOGIC_VECTOR ( 0 to 0 );
  signal NLW_U0_m_axis_tstrb_UNCONNECTED : STD_LOGIC_VECTOR ( 0 to 0 );
  signal NLW_U0_m_axis_tuser_UNCONNECTED : STD_LOGIC_VECTOR ( 3 downto 0 );
  signal NLW_U0_rd_data_count_UNCONNECTED : STD_LOGIC_VECTOR ( 5 downto 0 );
  signal NLW_U0_s_axi_bid_UNCONNECTED : STD_LOGIC_VECTOR ( 0 to 0 );
  signal NLW_U0_s_axi_bresp_UNCONNECTED : STD_LOGIC_VECTOR ( 1 downto 0 );
  signal NLW_U0_s_axi_buser_UNCONNECTED : STD_LOGIC_VECTOR ( 0 to 0 );
  signal NLW_U0_s_axi_rdata_UNCONNECTED : STD_LOGIC_VECTOR ( 63 downto 0 );
  signal NLW_U0_s_axi_rid_UNCONNECTED : STD_LOGIC_VECTOR ( 0 to 0 );
  signal NLW_U0_s_axi_rresp_UNCONNECTED : STD_LOGIC_VECTOR ( 1 downto 0 );
  signal NLW_U0_s_axi_ruser_UNCONNECTED : STD_LOGIC_VECTOR ( 0 to 0 );
  signal NLW_U0_wr_data_count_UNCONNECTED : STD_LOGIC_VECTOR ( 5 downto 0 );
  attribute C_ADD_NGC_CONSTRAINT : integer;
  attribute C_ADD_NGC_CONSTRAINT of U0 : label is 0;
  attribute C_APPLICATION_TYPE_AXIS : integer;
  attribute C_APPLICATION_TYPE_AXIS of U0 : label is 0;
  attribute C_APPLICATION_TYPE_RACH : integer;
  attribute C_APPLICATION_TYPE_RACH of U0 : label is 0;
  attribute C_APPLICATION_TYPE_RDCH : integer;
  attribute C_APPLICATION_TYPE_RDCH of U0 : label is 0;
  attribute C_APPLICATION_TYPE_WACH : integer;
  attribute C_APPLICATION_TYPE_WACH of U0 : label is 0;
  attribute C_APPLICATION_TYPE_WDCH : integer;
  attribute C_APPLICATION_TYPE_WDCH of U0 : label is 0;
  attribute C_APPLICATION_TYPE_WRCH : integer;
  attribute C_APPLICATION_TYPE_WRCH of U0 : label is 0;
  attribute C_AXIS_TDATA_WIDTH : integer;
  attribute C_AXIS_TDATA_WIDTH of U0 : label is 8;
  attribute C_AXIS_TDEST_WIDTH : integer;
  attribute C_AXIS_TDEST_WIDTH of U0 : label is 1;
  attribute C_AXIS_TID_WIDTH : integer;
  attribute C_AXIS_TID_WIDTH of U0 : label is 1;
  attribute C_AXIS_TKEEP_WIDTH : integer;
  attribute C_AXIS_TKEEP_WIDTH of U0 : label is 1;
  attribute C_AXIS_TSTRB_WIDTH : integer;
  attribute C_AXIS_TSTRB_WIDTH of U0 : label is 1;
  attribute C_AXIS_TUSER_WIDTH : integer;
  attribute C_AXIS_TUSER_WIDTH of U0 : label is 4;
  attribute C_AXIS_TYPE : integer;
  attribute C_AXIS_TYPE of U0 : label is 0;
  attribute C_AXI_ADDR_WIDTH : integer;
  attribute C_AXI_ADDR_WIDTH of U0 : label is 32;
  attribute C_AXI_ARUSER_WIDTH : integer;
  attribute C_AXI_ARUSER_WIDTH of U0 : label is 1;
  attribute C_AXI_AWUSER_WIDTH : integer;
  attribute C_AXI_AWUSER_WIDTH of U0 : label is 1;
  attribute C_AXI_BUSER_WIDTH : integer;
  attribute C_AXI_BUSER_WIDTH of U0 : label is 1;
  attribute C_AXI_DATA_WIDTH : integer;
  attribute C_AXI_DATA_WIDTH of U0 : label is 64;
  attribute C_AXI_ID_WIDTH : integer;
  attribute C_AXI_ID_WIDTH of U0 : label is 1;
  attribute C_AXI_LEN_WIDTH : integer;
  attribute C_AXI_LEN_WIDTH of U0 : label is 8;
  attribute C_AXI_LOCK_WIDTH : integer;
  attribute C_AXI_LOCK_WIDTH of U0 : label is 1;
  attribute C_AXI_RUSER_WIDTH : integer;
  attribute C_AXI_RUSER_WIDTH of U0 : label is 1;
  attribute C_AXI_TYPE : integer;
  attribute C_AXI_TYPE of U0 : label is 1;
  attribute C_AXI_WUSER_WIDTH : integer;
  attribute C_AXI_WUSER_WIDTH of U0 : label is 1;
  attribute C_COMMON_CLOCK : integer;
  attribute C_COMMON_CLOCK of U0 : label is 0;
  attribute C_COUNT_TYPE : integer;
  attribute C_COUNT_TYPE of U0 : label is 0;
  attribute C_DATA_COUNT_WIDTH : integer;
  attribute C_DATA_COUNT_WIDTH of U0 : label is 6;
  attribute C_DEFAULT_VALUE : string;
  attribute C_DEFAULT_VALUE of U0 : label is "BlankString";
  attribute C_DIN_WIDTH : integer;
  attribute C_DIN_WIDTH of U0 : label is 32;
  attribute C_DIN_WIDTH_AXIS : integer;
  attribute C_DIN_WIDTH_AXIS of U0 : label is 1;
  attribute C_DIN_WIDTH_RACH : integer;
  attribute C_DIN_WIDTH_RACH of U0 : label is 32;
  attribute C_DIN_WIDTH_RDCH : integer;
  attribute C_DIN_WIDTH_RDCH of U0 : label is 64;
  attribute C_DIN_WIDTH_WACH : integer;
  attribute C_DIN_WIDTH_WACH of U0 : label is 32;
  attribute C_DIN_WIDTH_WDCH : integer;
  attribute C_DIN_WIDTH_WDCH of U0 : label is 64;
  attribute C_DIN_WIDTH_WRCH : integer;
  attribute C_DIN_WIDTH_WRCH of U0 : label is 2;
  attribute C_DOUT_RST_VAL : string;
  attribute C_DOUT_RST_VAL of U0 : label is "0";
  attribute C_DOUT_WIDTH : integer;
  attribute C_DOUT_WIDTH of U0 : label is 32;
  attribute C_ENABLE_RLOCS : integer;
  attribute C_ENABLE_RLOCS of U0 : label is 0;
  attribute C_ENABLE_RST_SYNC : integer;
  attribute C_ENABLE_RST_SYNC of U0 : label is 1;
  attribute C_ERROR_INJECTION_TYPE : integer;
  attribute C_ERROR_INJECTION_TYPE of U0 : label is 0;
  attribute C_ERROR_INJECTION_TYPE_AXIS : integer;
  attribute C_ERROR_INJECTION_TYPE_AXIS of U0 : label is 0;
  attribute C_ERROR_INJECTION_TYPE_RACH : integer;
  attribute C_ERROR_INJECTION_TYPE_RACH of U0 : label is 0;
  attribute C_ERROR_INJECTION_TYPE_RDCH : integer;
  attribute C_ERROR_INJECTION_TYPE_RDCH of U0 : label is 0;
  attribute C_ERROR_INJECTION_TYPE_WACH : integer;
  attribute C_ERROR_INJECTION_TYPE_WACH of U0 : label is 0;
  attribute C_ERROR_INJECTION_TYPE_WDCH : integer;
  attribute C_ERROR_INJECTION_TYPE_WDCH of U0 : label is 0;
  attribute C_ERROR_INJECTION_TYPE_WRCH : integer;
  attribute C_ERROR_INJECTION_TYPE_WRCH of U0 : label is 0;
  attribute C_FAMILY : string;
  attribute C_FAMILY of U0 : label is "zynq";
  attribute C_FULL_FLAGS_RST_VAL : integer;
  attribute C_FULL_FLAGS_RST_VAL of U0 : label is 1;
  attribute C_HAS_ALMOST_EMPTY : integer;
  attribute C_HAS_ALMOST_EMPTY of U0 : label is 0;
  attribute C_HAS_ALMOST_FULL : integer;
  attribute C_HAS_ALMOST_FULL of U0 : label is 0;
  attribute C_HAS_AXIS_TDATA : integer;
  attribute C_HAS_AXIS_TDATA of U0 : label is 1;
  attribute C_HAS_AXIS_TDEST : integer;
  attribute C_HAS_AXIS_TDEST of U0 : label is 0;
  attribute C_HAS_AXIS_TID : integer;
  attribute C_HAS_AXIS_TID of U0 : label is 0;
  attribute C_HAS_AXIS_TKEEP : integer;
  attribute C_HAS_AXIS_TKEEP of U0 : label is 0;
  attribute C_HAS_AXIS_TLAST : integer;
  attribute C_HAS_AXIS_TLAST of U0 : label is 0;
  attribute C_HAS_AXIS_TREADY : integer;
  attribute C_HAS_AXIS_TREADY of U0 : label is 1;
  attribute C_HAS_AXIS_TSTRB : integer;
  attribute C_HAS_AXIS_TSTRB of U0 : label is 0;
  attribute C_HAS_AXIS_TUSER : integer;
  attribute C_HAS_AXIS_TUSER of U0 : label is 1;
  attribute C_HAS_AXI_ARUSER : integer;
  attribute C_HAS_AXI_ARUSER of U0 : label is 0;
  attribute C_HAS_AXI_AWUSER : integer;
  attribute C_HAS_AXI_AWUSER of U0 : label is 0;
  attribute C_HAS_AXI_BUSER : integer;
  attribute C_HAS_AXI_BUSER of U0 : label is 0;
  attribute C_HAS_AXI_ID : integer;
  attribute C_HAS_AXI_ID of U0 : label is 0;
  attribute C_HAS_AXI_RD_CHANNEL : integer;
  attribute C_HAS_AXI_RD_CHANNEL of U0 : label is 1;
  attribute C_HAS_AXI_RUSER : integer;
  attribute C_HAS_AXI_RUSER of U0 : label is 0;
  attribute C_HAS_AXI_WR_CHANNEL : integer;
  attribute C_HAS_AXI_WR_CHANNEL of U0 : label is 1;
  attribute C_HAS_AXI_WUSER : integer;
  attribute C_HAS_AXI_WUSER of U0 : label is 0;
  attribute C_HAS_BACKUP : integer;
  attribute C_HAS_BACKUP of U0 : label is 0;
  attribute C_HAS_DATA_COUNT : integer;
  attribute C_HAS_DATA_COUNT of U0 : label is 0;
  attribute C_HAS_DATA_COUNTS_AXIS : integer;
  attribute C_HAS_DATA_COUNTS_AXIS of U0 : label is 0;
  attribute C_HAS_DATA_COUNTS_RACH : integer;
  attribute C_HAS_DATA_COUNTS_RACH of U0 : label is 0;
  attribute C_HAS_DATA_COUNTS_RDCH : integer;
  attribute C_HAS_DATA_COUNTS_RDCH of U0 : label is 0;
  attribute C_HAS_DATA_COUNTS_WACH : integer;
  attribute C_HAS_DATA_COUNTS_WACH of U0 : label is 0;
  attribute C_HAS_DATA_COUNTS_WDCH : integer;
  attribute C_HAS_DATA_COUNTS_WDCH of U0 : label is 0;
  attribute C_HAS_DATA_COUNTS_WRCH : integer;
  attribute C_HAS_DATA_COUNTS_WRCH of U0 : label is 0;
  attribute C_HAS_INT_CLK : integer;
  attribute C_HAS_INT_CLK of U0 : label is 0;
  attribute C_HAS_MASTER_CE : integer;
  attribute C_HAS_MASTER_CE of U0 : label is 0;
  attribute C_HAS_MEMINIT_FILE : integer;
  attribute C_HAS_MEMINIT_FILE of U0 : label is 0;
  attribute C_HAS_OVERFLOW : integer;
  attribute C_HAS_OVERFLOW of U0 : label is 0;
  attribute C_HAS_PROG_FLAGS_AXIS : integer;
  attribute C_HAS_PROG_FLAGS_AXIS of U0 : label is 0;
  attribute C_HAS_PROG_FLAGS_RACH : integer;
  attribute C_HAS_PROG_FLAGS_RACH of U0 : label is 0;
  attribute C_HAS_PROG_FLAGS_RDCH : integer;
  attribute C_HAS_PROG_FLAGS_RDCH of U0 : label is 0;
  attribute C_HAS_PROG_FLAGS_WACH : integer;
  attribute C_HAS_PROG_FLAGS_WACH of U0 : label is 0;
  attribute C_HAS_PROG_FLAGS_WDCH : integer;
  attribute C_HAS_PROG_FLAGS_WDCH of U0 : label is 0;
  attribute C_HAS_PROG_FLAGS_WRCH : integer;
  attribute C_HAS_PROG_FLAGS_WRCH of U0 : label is 0;
  attribute C_HAS_RD_DATA_COUNT : integer;
  attribute C_HAS_RD_DATA_COUNT of U0 : label is 0;
  attribute C_HAS_RD_RST : integer;
  attribute C_HAS_RD_RST of U0 : label is 0;
  attribute C_HAS_RST : integer;
  attribute C_HAS_RST of U0 : label is 1;
  attribute C_HAS_SLAVE_CE : integer;
  attribute C_HAS_SLAVE_CE of U0 : label is 0;
  attribute C_HAS_SRST : integer;
  attribute C_HAS_SRST of U0 : label is 0;
  attribute C_HAS_UNDERFLOW : integer;
  attribute C_HAS_UNDERFLOW of U0 : label is 0;
  attribute C_HAS_VALID : integer;
  attribute C_HAS_VALID of U0 : label is 0;
  attribute C_HAS_WR_ACK : integer;
  attribute C_HAS_WR_ACK of U0 : label is 0;
  attribute C_HAS_WR_DATA_COUNT : integer;
  attribute C_HAS_WR_DATA_COUNT of U0 : label is 0;
  attribute C_HAS_WR_RST : integer;
  attribute C_HAS_WR_RST of U0 : label is 0;
  attribute C_IMPLEMENTATION_TYPE : integer;
  attribute C_IMPLEMENTATION_TYPE of U0 : label is 2;
  attribute C_IMPLEMENTATION_TYPE_AXIS : integer;
  attribute C_IMPLEMENTATION_TYPE_AXIS of U0 : label is 1;
  attribute C_IMPLEMENTATION_TYPE_RACH : integer;
  attribute C_IMPLEMENTATION_TYPE_RACH of U0 : label is 1;
  attribute C_IMPLEMENTATION_TYPE_RDCH : integer;
  attribute C_IMPLEMENTATION_TYPE_RDCH of U0 : label is 1;
  attribute C_IMPLEMENTATION_TYPE_WACH : integer;
  attribute C_IMPLEMENTATION_TYPE_WACH of U0 : label is 1;
  attribute C_IMPLEMENTATION_TYPE_WDCH : integer;
  attribute C_IMPLEMENTATION_TYPE_WDCH of U0 : label is 1;
  attribute C_IMPLEMENTATION_TYPE_WRCH : integer;
  attribute C_IMPLEMENTATION_TYPE_WRCH of U0 : label is 1;
  attribute C_INIT_WR_PNTR_VAL : integer;
  attribute C_INIT_WR_PNTR_VAL of U0 : label is 0;
  attribute C_INTERFACE_TYPE : integer;
  attribute C_INTERFACE_TYPE of U0 : label is 0;
  attribute C_MEMORY_TYPE : integer;
  attribute C_MEMORY_TYPE of U0 : label is 1;
  attribute C_MIF_FILE_NAME : string;
  attribute C_MIF_FILE_NAME of U0 : label is "BlankString";
  attribute C_MSGON_VAL : integer;
  attribute C_MSGON_VAL of U0 : label is 1;
  attribute C_OPTIMIZATION_MODE : integer;
  attribute C_OPTIMIZATION_MODE of U0 : label is 0;
  attribute C_OVERFLOW_LOW : integer;
  attribute C_OVERFLOW_LOW of U0 : label is 0;
  attribute C_POWER_SAVING_MODE : integer;
  attribute C_POWER_SAVING_MODE of U0 : label is 0;
  attribute C_PRELOAD_LATENCY : integer;
  attribute C_PRELOAD_LATENCY of U0 : label is 0;
  attribute C_PRELOAD_REGS : integer;
  attribute C_PRELOAD_REGS of U0 : label is 1;
  attribute C_PRIM_FIFO_TYPE : string;
  attribute C_PRIM_FIFO_TYPE of U0 : label is "512x36";
  attribute C_PRIM_FIFO_TYPE_AXIS : string;
  attribute C_PRIM_FIFO_TYPE_AXIS of U0 : label is "1kx18";
  attribute C_PRIM_FIFO_TYPE_RACH : string;
  attribute C_PRIM_FIFO_TYPE_RACH of U0 : label is "512x36";
  attribute C_PRIM_FIFO_TYPE_RDCH : string;
  attribute C_PRIM_FIFO_TYPE_RDCH of U0 : label is "1kx36";
  attribute C_PRIM_FIFO_TYPE_WACH : string;
  attribute C_PRIM_FIFO_TYPE_WACH of U0 : label is "512x36";
  attribute C_PRIM_FIFO_TYPE_WDCH : string;
  attribute C_PRIM_FIFO_TYPE_WDCH of U0 : label is "1kx36";
  attribute C_PRIM_FIFO_TYPE_WRCH : string;
  attribute C_PRIM_FIFO_TYPE_WRCH of U0 : label is "512x36";
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL : integer;
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL of U0 : label is 4;
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL_AXIS : integer;
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL_AXIS of U0 : label is 1022;
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL_RACH : integer;
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL_RACH of U0 : label is 1022;
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL_RDCH : integer;
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL_RDCH of U0 : label is 1022;
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL_WACH : integer;
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL_WACH of U0 : label is 1022;
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL_WDCH : integer;
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL_WDCH of U0 : label is 1022;
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL_WRCH : integer;
  attribute C_PROG_EMPTY_THRESH_ASSERT_VAL_WRCH of U0 : label is 1022;
  attribute C_PROG_EMPTY_THRESH_NEGATE_VAL : integer;
  attribute C_PROG_EMPTY_THRESH_NEGATE_VAL of U0 : label is 5;
  attribute C_PROG_EMPTY_TYPE : integer;
  attribute C_PROG_EMPTY_TYPE of U0 : label is 0;
  attribute C_PROG_EMPTY_TYPE_AXIS : integer;
  attribute C_PROG_EMPTY_TYPE_AXIS of U0 : label is 0;
  attribute C_PROG_EMPTY_TYPE_RACH : integer;
  attribute C_PROG_EMPTY_TYPE_RACH of U0 : label is 0;
  attribute C_PROG_EMPTY_TYPE_RDCH : integer;
  attribute C_PROG_EMPTY_TYPE_RDCH of U0 : label is 0;
  attribute C_PROG_EMPTY_TYPE_WACH : integer;
  attribute C_PROG_EMPTY_TYPE_WACH of U0 : label is 0;
  attribute C_PROG_EMPTY_TYPE_WDCH : integer;
  attribute C_PROG_EMPTY_TYPE_WDCH of U0 : label is 0;
  attribute C_PROG_EMPTY_TYPE_WRCH : integer;
  attribute C_PROG_EMPTY_TYPE_WRCH of U0 : label is 0;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL : integer;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL of U0 : label is 50;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL_AXIS : integer;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL_AXIS of U0 : label is 1023;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL_RACH : integer;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL_RACH of U0 : label is 1023;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL_RDCH : integer;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL_RDCH of U0 : label is 1023;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL_WACH : integer;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL_WACH of U0 : label is 1023;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL_WDCH : integer;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL_WDCH of U0 : label is 1023;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL_WRCH : integer;
  attribute C_PROG_FULL_THRESH_ASSERT_VAL_WRCH of U0 : label is 1023;
  attribute C_PROG_FULL_THRESH_NEGATE_VAL : integer;
  attribute C_PROG_FULL_THRESH_NEGATE_VAL of U0 : label is 49;
  attribute C_PROG_FULL_TYPE : integer;
  attribute C_PROG_FULL_TYPE of U0 : label is 1;
  attribute C_PROG_FULL_TYPE_AXIS : integer;
  attribute C_PROG_FULL_TYPE_AXIS of U0 : label is 0;
  attribute C_PROG_FULL_TYPE_RACH : integer;
  attribute C_PROG_FULL_TYPE_RACH of U0 : label is 0;
  attribute C_PROG_FULL_TYPE_RDCH : integer;
  attribute C_PROG_FULL_TYPE_RDCH of U0 : label is 0;
  attribute C_PROG_FULL_TYPE_WACH : integer;
  attribute C_PROG_FULL_TYPE_WACH of U0 : label is 0;
  attribute C_PROG_FULL_TYPE_WDCH : integer;
  attribute C_PROG_FULL_TYPE_WDCH of U0 : label is 0;
  attribute C_PROG_FULL_TYPE_WRCH : integer;
  attribute C_PROG_FULL_TYPE_WRCH of U0 : label is 0;
  attribute C_RACH_TYPE : integer;
  attribute C_RACH_TYPE of U0 : label is 0;
  attribute C_RDCH_TYPE : integer;
  attribute C_RDCH_TYPE of U0 : label is 0;
  attribute C_RD_DATA_COUNT_WIDTH : integer;
  attribute C_RD_DATA_COUNT_WIDTH of U0 : label is 6;
  attribute C_RD_DEPTH : integer;
  attribute C_RD_DEPTH of U0 : label is 64;
  attribute C_RD_FREQ : integer;
  attribute C_RD_FREQ of U0 : label is 1;
  attribute C_RD_PNTR_WIDTH : integer;
  attribute C_RD_PNTR_WIDTH of U0 : label is 6;
  attribute C_REG_SLICE_MODE_AXIS : integer;
  attribute C_REG_SLICE_MODE_AXIS of U0 : label is 0;
  attribute C_REG_SLICE_MODE_RACH : integer;
  attribute C_REG_SLICE_MODE_RACH of U0 : label is 0;
  attribute C_REG_SLICE_MODE_RDCH : integer;
  attribute C_REG_SLICE_MODE_RDCH of U0 : label is 0;
  attribute C_REG_SLICE_MODE_WACH : integer;
  attribute C_REG_SLICE_MODE_WACH of U0 : label is 0;
  attribute C_REG_SLICE_MODE_WDCH : integer;
  attribute C_REG_SLICE_MODE_WDCH of U0 : label is 0;
  attribute C_REG_SLICE_MODE_WRCH : integer;
  attribute C_REG_SLICE_MODE_WRCH of U0 : label is 0;
  attribute C_SYNCHRONIZER_STAGE : integer;
  attribute C_SYNCHRONIZER_STAGE of U0 : label is 2;
  attribute C_UNDERFLOW_LOW : integer;
  attribute C_UNDERFLOW_LOW of U0 : label is 0;
  attribute C_USE_COMMON_OVERFLOW : integer;
  attribute C_USE_COMMON_OVERFLOW of U0 : label is 0;
  attribute C_USE_COMMON_UNDERFLOW : integer;
  attribute C_USE_COMMON_UNDERFLOW of U0 : label is 0;
  attribute C_USE_DEFAULT_SETTINGS : integer;
  attribute C_USE_DEFAULT_SETTINGS of U0 : label is 0;
  attribute C_USE_DOUT_RST : integer;
  attribute C_USE_DOUT_RST of U0 : label is 1;
  attribute C_USE_ECC : integer;
  attribute C_USE_ECC of U0 : label is 0;
  attribute C_USE_ECC_AXIS : integer;
  attribute C_USE_ECC_AXIS of U0 : label is 0;
  attribute C_USE_ECC_RACH : integer;
  attribute C_USE_ECC_RACH of U0 : label is 0;
  attribute C_USE_ECC_RDCH : integer;
  attribute C_USE_ECC_RDCH of U0 : label is 0;
  attribute C_USE_ECC_WACH : integer;
  attribute C_USE_ECC_WACH of U0 : label is 0;
  attribute C_USE_ECC_WDCH : integer;
  attribute C_USE_ECC_WDCH of U0 : label is 0;
  attribute C_USE_ECC_WRCH : integer;
  attribute C_USE_ECC_WRCH of U0 : label is 0;
  attribute C_USE_EMBEDDED_REG : integer;
  attribute C_USE_EMBEDDED_REG of U0 : label is 0;
  attribute C_USE_FIFO16_FLAGS : integer;
  attribute C_USE_FIFO16_FLAGS of U0 : label is 0;
  attribute C_USE_FWFT_DATA_COUNT : integer;
  attribute C_USE_FWFT_DATA_COUNT of U0 : label is 0;
  attribute C_USE_PIPELINE_REG : integer;
  attribute C_USE_PIPELINE_REG of U0 : label is 0;
  attribute C_VALID_LOW : integer;
  attribute C_VALID_LOW of U0 : label is 0;
  attribute C_WACH_TYPE : integer;
  attribute C_WACH_TYPE of U0 : label is 0;
  attribute C_WDCH_TYPE : integer;
  attribute C_WDCH_TYPE of U0 : label is 0;
  attribute C_WRCH_TYPE : integer;
  attribute C_WRCH_TYPE of U0 : label is 0;
  attribute C_WR_ACK_LOW : integer;
  attribute C_WR_ACK_LOW of U0 : label is 0;
  attribute C_WR_DATA_COUNT_WIDTH : integer;
  attribute C_WR_DATA_COUNT_WIDTH of U0 : label is 6;
  attribute C_WR_DEPTH : integer;
  attribute C_WR_DEPTH of U0 : label is 64;
  attribute C_WR_DEPTH_AXIS : integer;
  attribute C_WR_DEPTH_AXIS of U0 : label is 1024;
  attribute C_WR_DEPTH_RACH : integer;
  attribute C_WR_DEPTH_RACH of U0 : label is 16;
  attribute C_WR_DEPTH_RDCH : integer;
  attribute C_WR_DEPTH_RDCH of U0 : label is 1024;
  attribute C_WR_DEPTH_WACH : integer;
  attribute C_WR_DEPTH_WACH of U0 : label is 16;
  attribute C_WR_DEPTH_WDCH : integer;
  attribute C_WR_DEPTH_WDCH of U0 : label is 1024;
  attribute C_WR_DEPTH_WRCH : integer;
  attribute C_WR_DEPTH_WRCH of U0 : label is 16;
  attribute C_WR_FREQ : integer;
  attribute C_WR_FREQ of U0 : label is 1;
  attribute C_WR_PNTR_WIDTH : integer;
  attribute C_WR_PNTR_WIDTH of U0 : label is 6;
  attribute C_WR_PNTR_WIDTH_AXIS : integer;
  attribute C_WR_PNTR_WIDTH_AXIS of U0 : label is 10;
  attribute C_WR_PNTR_WIDTH_RACH : integer;
  attribute C_WR_PNTR_WIDTH_RACH of U0 : label is 4;
  attribute C_WR_PNTR_WIDTH_RDCH : integer;
  attribute C_WR_PNTR_WIDTH_RDCH of U0 : label is 10;
  attribute C_WR_PNTR_WIDTH_WACH : integer;
  attribute C_WR_PNTR_WIDTH_WACH of U0 : label is 4;
  attribute C_WR_PNTR_WIDTH_WDCH : integer;
  attribute C_WR_PNTR_WIDTH_WDCH of U0 : label is 10;
  attribute C_WR_PNTR_WIDTH_WRCH : integer;
  attribute C_WR_PNTR_WIDTH_WRCH of U0 : label is 4;
  attribute C_WR_RESPONSE_LATENCY : integer;
  attribute C_WR_RESPONSE_LATENCY of U0 : label is 1;
begin
U0: entity work.dram_rd_ram0_0_fifo_generator_v12_0
     port map (
      almost_empty => NLW_U0_almost_empty_UNCONNECTED,
      almost_full => NLW_U0_almost_full_UNCONNECTED,
      axi_ar_data_count(4 downto 0) => NLW_U0_axi_ar_data_count_UNCONNECTED(4 downto 0),
      axi_ar_dbiterr => NLW_U0_axi_ar_dbiterr_UNCONNECTED,
      axi_ar_injectdbiterr => '0',
      axi_ar_injectsbiterr => '0',
      axi_ar_overflow => NLW_U0_axi_ar_overflow_UNCONNECTED,
      axi_ar_prog_empty => NLW_U0_axi_ar_prog_empty_UNCONNECTED,
      axi_ar_prog_empty_thresh(3) => '0',
      axi_ar_prog_empty_thresh(2) => '0',
      axi_ar_prog_empty_thresh(1) => '0',
      axi_ar_prog_empty_thresh(0) => '0',
      axi_ar_prog_full => NLW_U0_axi_ar_prog_full_UNCONNECTED,
      axi_ar_prog_full_thresh(3) => '0',
      axi_ar_prog_full_thresh(2) => '0',
      axi_ar_prog_full_thresh(1) => '0',
      axi_ar_prog_full_thresh(0) => '0',
      axi_ar_rd_data_count(4 downto 0) => NLW_U0_axi_ar_rd_data_count_UNCONNECTED(4 downto 0),
      axi_ar_sbiterr => NLW_U0_axi_ar_sbiterr_UNCONNECTED,
      axi_ar_underflow => NLW_U0_axi_ar_underflow_UNCONNECTED,
      axi_ar_wr_data_count(4 downto 0) => NLW_U0_axi_ar_wr_data_count_UNCONNECTED(4 downto 0),
      axi_aw_data_count(4 downto 0) => NLW_U0_axi_aw_data_count_UNCONNECTED(4 downto 0),
      axi_aw_dbiterr => NLW_U0_axi_aw_dbiterr_UNCONNECTED,
      axi_aw_injectdbiterr => '0',
      axi_aw_injectsbiterr => '0',
      axi_aw_overflow => NLW_U0_axi_aw_overflow_UNCONNECTED,
      axi_aw_prog_empty => NLW_U0_axi_aw_prog_empty_UNCONNECTED,
      axi_aw_prog_empty_thresh(3) => '0',
      axi_aw_prog_empty_thresh(2) => '0',
      axi_aw_prog_empty_thresh(1) => '0',
      axi_aw_prog_empty_thresh(0) => '0',
      axi_aw_prog_full => NLW_U0_axi_aw_prog_full_UNCONNECTED,
      axi_aw_prog_full_thresh(3) => '0',
      axi_aw_prog_full_thresh(2) => '0',
      axi_aw_prog_full_thresh(1) => '0',
      axi_aw_prog_full_thresh(0) => '0',
      axi_aw_rd_data_count(4 downto 0) => NLW_U0_axi_aw_rd_data_count_UNCONNECTED(4 downto 0),
      axi_aw_sbiterr => NLW_U0_axi_aw_sbiterr_UNCONNECTED,
      axi_aw_underflow => NLW_U0_axi_aw_underflow_UNCONNECTED,
      axi_aw_wr_data_count(4 downto 0) => NLW_U0_axi_aw_wr_data_count_UNCONNECTED(4 downto 0),
      axi_b_data_count(4 downto 0) => NLW_U0_axi_b_data_count_UNCONNECTED(4 downto 0),
      axi_b_dbiterr => NLW_U0_axi_b_dbiterr_UNCONNECTED,
      axi_b_injectdbiterr => '0',
      axi_b_injectsbiterr => '0',
      axi_b_overflow => NLW_U0_axi_b_overflow_UNCONNECTED,
      axi_b_prog_empty => NLW_U0_axi_b_prog_empty_UNCONNECTED,
      axi_b_prog_empty_thresh(3) => '0',
      axi_b_prog_empty_thresh(2) => '0',
      axi_b_prog_empty_thresh(1) => '0',
      axi_b_prog_empty_thresh(0) => '0',
      axi_b_prog_full => NLW_U0_axi_b_prog_full_UNCONNECTED,
      axi_b_prog_full_thresh(3) => '0',
      axi_b_prog_full_thresh(2) => '0',
      axi_b_prog_full_thresh(1) => '0',
      axi_b_prog_full_thresh(0) => '0',
      axi_b_rd_data_count(4 downto 0) => NLW_U0_axi_b_rd_data_count_UNCONNECTED(4 downto 0),
      axi_b_sbiterr => NLW_U0_axi_b_sbiterr_UNCONNECTED,
      axi_b_underflow => NLW_U0_axi_b_underflow_UNCONNECTED,
      axi_b_wr_data_count(4 downto 0) => NLW_U0_axi_b_wr_data_count_UNCONNECTED(4 downto 0),
      axi_r_data_count(10 downto 0) => NLW_U0_axi_r_data_count_UNCONNECTED(10 downto 0),
      axi_r_dbiterr => NLW_U0_axi_r_dbiterr_UNCONNECTED,
      axi_r_injectdbiterr => '0',
      axi_r_injectsbiterr => '0',
      axi_r_overflow => NLW_U0_axi_r_overflow_UNCONNECTED,
      axi_r_prog_empty => NLW_U0_axi_r_prog_empty_UNCONNECTED,
      axi_r_prog_empty_thresh(9) => '0',
      axi_r_prog_empty_thresh(8) => '0',
      axi_r_prog_empty_thresh(7) => '0',
      axi_r_prog_empty_thresh(6) => '0',
      axi_r_prog_empty_thresh(5) => '0',
      axi_r_prog_empty_thresh(4) => '0',
      axi_r_prog_empty_thresh(3) => '0',
      axi_r_prog_empty_thresh(2) => '0',
      axi_r_prog_empty_thresh(1) => '0',
      axi_r_prog_empty_thresh(0) => '0',
      axi_r_prog_full => NLW_U0_axi_r_prog_full_UNCONNECTED,
      axi_r_prog_full_thresh(9) => '0',
      axi_r_prog_full_thresh(8) => '0',
      axi_r_prog_full_thresh(7) => '0',
      axi_r_prog_full_thresh(6) => '0',
      axi_r_prog_full_thresh(5) => '0',
      axi_r_prog_full_thresh(4) => '0',
      axi_r_prog_full_thresh(3) => '0',
      axi_r_prog_full_thresh(2) => '0',
      axi_r_prog_full_thresh(1) => '0',
      axi_r_prog_full_thresh(0) => '0',
      axi_r_rd_data_count(10 downto 0) => NLW_U0_axi_r_rd_data_count_UNCONNECTED(10 downto 0),
      axi_r_sbiterr => NLW_U0_axi_r_sbiterr_UNCONNECTED,
      axi_r_underflow => NLW_U0_axi_r_underflow_UNCONNECTED,
      axi_r_wr_data_count(10 downto 0) => NLW_U0_axi_r_wr_data_count_UNCONNECTED(10 downto 0),
      axi_w_data_count(10 downto 0) => NLW_U0_axi_w_data_count_UNCONNECTED(10 downto 0),
      axi_w_dbiterr => NLW_U0_axi_w_dbiterr_UNCONNECTED,
      axi_w_injectdbiterr => '0',
      axi_w_injectsbiterr => '0',
      axi_w_overflow => NLW_U0_axi_w_overflow_UNCONNECTED,
      axi_w_prog_empty => NLW_U0_axi_w_prog_empty_UNCONNECTED,
      axi_w_prog_empty_thresh(9) => '0',
      axi_w_prog_empty_thresh(8) => '0',
      axi_w_prog_empty_thresh(7) => '0',
      axi_w_prog_empty_thresh(6) => '0',
      axi_w_prog_empty_thresh(5) => '0',
      axi_w_prog_empty_thresh(4) => '0',
      axi_w_prog_empty_thresh(3) => '0',
      axi_w_prog_empty_thresh(2) => '0',
      axi_w_prog_empty_thresh(1) => '0',
      axi_w_prog_empty_thresh(0) => '0',
      axi_w_prog_full => NLW_U0_axi_w_prog_full_UNCONNECTED,
      axi_w_prog_full_thresh(9) => '0',
      axi_w_prog_full_thresh(8) => '0',
      axi_w_prog_full_thresh(7) => '0',
      axi_w_prog_full_thresh(6) => '0',
      axi_w_prog_full_thresh(5) => '0',
      axi_w_prog_full_thresh(4) => '0',
      axi_w_prog_full_thresh(3) => '0',
      axi_w_prog_full_thresh(2) => '0',
      axi_w_prog_full_thresh(1) => '0',
      axi_w_prog_full_thresh(0) => '0',
      axi_w_rd_data_count(10 downto 0) => NLW_U0_axi_w_rd_data_count_UNCONNECTED(10 downto 0),
      axi_w_sbiterr => NLW_U0_axi_w_sbiterr_UNCONNECTED,
      axi_w_underflow => NLW_U0_axi_w_underflow_UNCONNECTED,
      axi_w_wr_data_count(10 downto 0) => NLW_U0_axi_w_wr_data_count_UNCONNECTED(10 downto 0),
      axis_data_count(10 downto 0) => NLW_U0_axis_data_count_UNCONNECTED(10 downto 0),
      axis_dbiterr => NLW_U0_axis_dbiterr_UNCONNECTED,
      axis_injectdbiterr => '0',
      axis_injectsbiterr => '0',
      axis_overflow => NLW_U0_axis_overflow_UNCONNECTED,
      axis_prog_empty => NLW_U0_axis_prog_empty_UNCONNECTED,
      axis_prog_empty_thresh(9) => '0',
      axis_prog_empty_thresh(8) => '0',
      axis_prog_empty_thresh(7) => '0',
      axis_prog_empty_thresh(6) => '0',
      axis_prog_empty_thresh(5) => '0',
      axis_prog_empty_thresh(4) => '0',
      axis_prog_empty_thresh(3) => '0',
      axis_prog_empty_thresh(2) => '0',
      axis_prog_empty_thresh(1) => '0',
      axis_prog_empty_thresh(0) => '0',
      axis_prog_full => NLW_U0_axis_prog_full_UNCONNECTED,
      axis_prog_full_thresh(9) => '0',
      axis_prog_full_thresh(8) => '0',
      axis_prog_full_thresh(7) => '0',
      axis_prog_full_thresh(6) => '0',
      axis_prog_full_thresh(5) => '0',
      axis_prog_full_thresh(4) => '0',
      axis_prog_full_thresh(3) => '0',
      axis_prog_full_thresh(2) => '0',
      axis_prog_full_thresh(1) => '0',
      axis_prog_full_thresh(0) => '0',
      axis_rd_data_count(10 downto 0) => NLW_U0_axis_rd_data_count_UNCONNECTED(10 downto 0),
      axis_sbiterr => NLW_U0_axis_sbiterr_UNCONNECTED,
      axis_underflow => NLW_U0_axis_underflow_UNCONNECTED,
      axis_wr_data_count(10 downto 0) => NLW_U0_axis_wr_data_count_UNCONNECTED(10 downto 0),
      backup => '0',
      backup_marker => '0',
      clk => '0',
      data_count(5 downto 0) => NLW_U0_data_count_UNCONNECTED(5 downto 0),
      dbiterr => NLW_U0_dbiterr_UNCONNECTED,
      din(31 downto 0) => din(31 downto 0),
      dout(31 downto 0) => dout(31 downto 0),
      empty => empty,
      full => full,
      injectdbiterr => '0',
      injectsbiterr => '0',
      int_clk => '0',
      m_aclk => '0',
      m_aclk_en => '0',
      m_axi_araddr(31 downto 0) => NLW_U0_m_axi_araddr_UNCONNECTED(31 downto 0),
      m_axi_arburst(1 downto 0) => NLW_U0_m_axi_arburst_UNCONNECTED(1 downto 0),
      m_axi_arcache(3 downto 0) => NLW_U0_m_axi_arcache_UNCONNECTED(3 downto 0),
      m_axi_arid(0) => NLW_U0_m_axi_arid_UNCONNECTED(0),
      m_axi_arlen(7 downto 0) => NLW_U0_m_axi_arlen_UNCONNECTED(7 downto 0),
      m_axi_arlock(0) => NLW_U0_m_axi_arlock_UNCONNECTED(0),
      m_axi_arprot(2 downto 0) => NLW_U0_m_axi_arprot_UNCONNECTED(2 downto 0),
      m_axi_arqos(3 downto 0) => NLW_U0_m_axi_arqos_UNCONNECTED(3 downto 0),
      m_axi_arready => '0',
      m_axi_arregion(3 downto 0) => NLW_U0_m_axi_arregion_UNCONNECTED(3 downto 0),
      m_axi_arsize(2 downto 0) => NLW_U0_m_axi_arsize_UNCONNECTED(2 downto 0),
      m_axi_aruser(0) => NLW_U0_m_axi_aruser_UNCONNECTED(0),
      m_axi_arvalid => NLW_U0_m_axi_arvalid_UNCONNECTED,
      m_axi_awaddr(31 downto 0) => NLW_U0_m_axi_awaddr_UNCONNECTED(31 downto 0),
      m_axi_awburst(1 downto 0) => NLW_U0_m_axi_awburst_UNCONNECTED(1 downto 0),
      m_axi_awcache(3 downto 0) => NLW_U0_m_axi_awcache_UNCONNECTED(3 downto 0),
      m_axi_awid(0) => NLW_U0_m_axi_awid_UNCONNECTED(0),
      m_axi_awlen(7 downto 0) => NLW_U0_m_axi_awlen_UNCONNECTED(7 downto 0),
      m_axi_awlock(0) => NLW_U0_m_axi_awlock_UNCONNECTED(0),
      m_axi_awprot(2 downto 0) => NLW_U0_m_axi_awprot_UNCONNECTED(2 downto 0),
      m_axi_awqos(3 downto 0) => NLW_U0_m_axi_awqos_UNCONNECTED(3 downto 0),
      m_axi_awready => '0',
      m_axi_awregion(3 downto 0) => NLW_U0_m_axi_awregion_UNCONNECTED(3 downto 0),
      m_axi_awsize(2 downto 0) => NLW_U0_m_axi_awsize_UNCONNECTED(2 downto 0),
      m_axi_awuser(0) => NLW_U0_m_axi_awuser_UNCONNECTED(0),
      m_axi_awvalid => NLW_U0_m_axi_awvalid_UNCONNECTED,
      m_axi_bid(0) => '0',
      m_axi_bready => NLW_U0_m_axi_bready_UNCONNECTED,
      m_axi_bresp(1) => '0',
      m_axi_bresp(0) => '0',
      m_axi_buser(0) => '0',
      m_axi_bvalid => '0',
      m_axi_rdata(63) => '0',
      m_axi_rdata(62) => '0',
      m_axi_rdata(61) => '0',
      m_axi_rdata(60) => '0',
      m_axi_rdata(59) => '0',
      m_axi_rdata(58) => '0',
      m_axi_rdata(57) => '0',
      m_axi_rdata(56) => '0',
      m_axi_rdata(55) => '0',
      m_axi_rdata(54) => '0',
      m_axi_rdata(53) => '0',
      m_axi_rdata(52) => '0',
      m_axi_rdata(51) => '0',
      m_axi_rdata(50) => '0',
      m_axi_rdata(49) => '0',
      m_axi_rdata(48) => '0',
      m_axi_rdata(47) => '0',
      m_axi_rdata(46) => '0',
      m_axi_rdata(45) => '0',
      m_axi_rdata(44) => '0',
      m_axi_rdata(43) => '0',
      m_axi_rdata(42) => '0',
      m_axi_rdata(41) => '0',
      m_axi_rdata(40) => '0',
      m_axi_rdata(39) => '0',
      m_axi_rdata(38) => '0',
      m_axi_rdata(37) => '0',
      m_axi_rdata(36) => '0',
      m_axi_rdata(35) => '0',
      m_axi_rdata(34) => '0',
      m_axi_rdata(33) => '0',
      m_axi_rdata(32) => '0',
      m_axi_rdata(31) => '0',
      m_axi_rdata(30) => '0',
      m_axi_rdata(29) => '0',
      m_axi_rdata(28) => '0',
      m_axi_rdata(27) => '0',
      m_axi_rdata(26) => '0',
      m_axi_rdata(25) => '0',
      m_axi_rdata(24) => '0',
      m_axi_rdata(23) => '0',
      m_axi_rdata(22) => '0',
      m_axi_rdata(21) => '0',
      m_axi_rdata(20) => '0',
      m_axi_rdata(19) => '0',
      m_axi_rdata(18) => '0',
      m_axi_rdata(17) => '0',
      m_axi_rdata(16) => '0',
      m_axi_rdata(15) => '0',
      m_axi_rdata(14) => '0',
      m_axi_rdata(13) => '0',
      m_axi_rdata(12) => '0',
      m_axi_rdata(11) => '0',
      m_axi_rdata(10) => '0',
      m_axi_rdata(9) => '0',
      m_axi_rdata(8) => '0',
      m_axi_rdata(7) => '0',
      m_axi_rdata(6) => '0',
      m_axi_rdata(5) => '0',
      m_axi_rdata(4) => '0',
      m_axi_rdata(3) => '0',
      m_axi_rdata(2) => '0',
      m_axi_rdata(1) => '0',
      m_axi_rdata(0) => '0',
      m_axi_rid(0) => '0',
      m_axi_rlast => '0',
      m_axi_rready => NLW_U0_m_axi_rready_UNCONNECTED,
      m_axi_rresp(1) => '0',
      m_axi_rresp(0) => '0',
      m_axi_ruser(0) => '0',
      m_axi_rvalid => '0',
      m_axi_wdata(63 downto 0) => NLW_U0_m_axi_wdata_UNCONNECTED(63 downto 0),
      m_axi_wid(0) => NLW_U0_m_axi_wid_UNCONNECTED(0),
      m_axi_wlast => NLW_U0_m_axi_wlast_UNCONNECTED,
      m_axi_wready => '0',
      m_axi_wstrb(7 downto 0) => NLW_U0_m_axi_wstrb_UNCONNECTED(7 downto 0),
      m_axi_wuser(0) => NLW_U0_m_axi_wuser_UNCONNECTED(0),
      m_axi_wvalid => NLW_U0_m_axi_wvalid_UNCONNECTED,
      m_axis_tdata(7 downto 0) => NLW_U0_m_axis_tdata_UNCONNECTED(7 downto 0),
      m_axis_tdest(0) => NLW_U0_m_axis_tdest_UNCONNECTED(0),
      m_axis_tid(0) => NLW_U0_m_axis_tid_UNCONNECTED(0),
      m_axis_tkeep(0) => NLW_U0_m_axis_tkeep_UNCONNECTED(0),
      m_axis_tlast => NLW_U0_m_axis_tlast_UNCONNECTED,
      m_axis_tready => '0',
      m_axis_tstrb(0) => NLW_U0_m_axis_tstrb_UNCONNECTED(0),
      m_axis_tuser(3 downto 0) => NLW_U0_m_axis_tuser_UNCONNECTED(3 downto 0),
      m_axis_tvalid => NLW_U0_m_axis_tvalid_UNCONNECTED,
      overflow => NLW_U0_overflow_UNCONNECTED,
      prog_empty => NLW_U0_prog_empty_UNCONNECTED,
      prog_empty_thresh(5) => '0',
      prog_empty_thresh(4) => '0',
      prog_empty_thresh(3) => '0',
      prog_empty_thresh(2) => '0',
      prog_empty_thresh(1) => '0',
      prog_empty_thresh(0) => '0',
      prog_empty_thresh_assert(5) => '0',
      prog_empty_thresh_assert(4) => '0',
      prog_empty_thresh_assert(3) => '0',
      prog_empty_thresh_assert(2) => '0',
      prog_empty_thresh_assert(1) => '0',
      prog_empty_thresh_assert(0) => '0',
      prog_empty_thresh_negate(5) => '0',
      prog_empty_thresh_negate(4) => '0',
      prog_empty_thresh_negate(3) => '0',
      prog_empty_thresh_negate(2) => '0',
      prog_empty_thresh_negate(1) => '0',
      prog_empty_thresh_negate(0) => '0',
      prog_full => prog_full,
      prog_full_thresh(5) => '0',
      prog_full_thresh(4) => '0',
      prog_full_thresh(3) => '0',
      prog_full_thresh(2) => '0',
      prog_full_thresh(1) => '0',
      prog_full_thresh(0) => '0',
      prog_full_thresh_assert(5) => '0',
      prog_full_thresh_assert(4) => '0',
      prog_full_thresh_assert(3) => '0',
      prog_full_thresh_assert(2) => '0',
      prog_full_thresh_assert(1) => '0',
      prog_full_thresh_assert(0) => '0',
      prog_full_thresh_negate(5) => '0',
      prog_full_thresh_negate(4) => '0',
      prog_full_thresh_negate(3) => '0',
      prog_full_thresh_negate(2) => '0',
      prog_full_thresh_negate(1) => '0',
      prog_full_thresh_negate(0) => '0',
      rd_clk => rd_clk,
      rd_data_count(5 downto 0) => NLW_U0_rd_data_count_UNCONNECTED(5 downto 0),
      rd_en => rd_en,
      rd_rst => '0',
      rd_rst_busy => NLW_U0_rd_rst_busy_UNCONNECTED,
      rst => rst,
      s_aclk => '0',
      s_aclk_en => '0',
      s_aresetn => '0',
      s_axi_araddr(31) => '0',
      s_axi_araddr(30) => '0',
      s_axi_araddr(29) => '0',
      s_axi_araddr(28) => '0',
      s_axi_araddr(27) => '0',
      s_axi_araddr(26) => '0',
      s_axi_araddr(25) => '0',
      s_axi_araddr(24) => '0',
      s_axi_araddr(23) => '0',
      s_axi_araddr(22) => '0',
      s_axi_araddr(21) => '0',
      s_axi_araddr(20) => '0',
      s_axi_araddr(19) => '0',
      s_axi_araddr(18) => '0',
      s_axi_araddr(17) => '0',
      s_axi_araddr(16) => '0',
      s_axi_araddr(15) => '0',
      s_axi_araddr(14) => '0',
      s_axi_araddr(13) => '0',
      s_axi_araddr(12) => '0',
      s_axi_araddr(11) => '0',
      s_axi_araddr(10) => '0',
      s_axi_araddr(9) => '0',
      s_axi_araddr(8) => '0',
      s_axi_araddr(7) => '0',
      s_axi_araddr(6) => '0',
      s_axi_araddr(5) => '0',
      s_axi_araddr(4) => '0',
      s_axi_araddr(3) => '0',
      s_axi_araddr(2) => '0',
      s_axi_araddr(1) => '0',
      s_axi_araddr(0) => '0',
      s_axi_arburst(1) => '0',
      s_axi_arburst(0) => '0',
      s_axi_arcache(3) => '0',
      s_axi_arcache(2) => '0',
      s_axi_arcache(1) => '0',
      s_axi_arcache(0) => '0',
      s_axi_arid(0) => '0',
      s_axi_arlen(7) => '0',
      s_axi_arlen(6) => '0',
      s_axi_arlen(5) => '0',
      s_axi_arlen(4) => '0',
      s_axi_arlen(3) => '0',
      s_axi_arlen(2) => '0',
      s_axi_arlen(1) => '0',
      s_axi_arlen(0) => '0',
      s_axi_arlock(0) => '0',
      s_axi_arprot(2) => '0',
      s_axi_arprot(1) => '0',
      s_axi_arprot(0) => '0',
      s_axi_arqos(3) => '0',
      s_axi_arqos(2) => '0',
      s_axi_arqos(1) => '0',
      s_axi_arqos(0) => '0',
      s_axi_arready => NLW_U0_s_axi_arready_UNCONNECTED,
      s_axi_arregion(3) => '0',
      s_axi_arregion(2) => '0',
      s_axi_arregion(1) => '0',
      s_axi_arregion(0) => '0',
      s_axi_arsize(2) => '0',
      s_axi_arsize(1) => '0',
      s_axi_arsize(0) => '0',
      s_axi_aruser(0) => '0',
      s_axi_arvalid => '0',
      s_axi_awaddr(31) => '0',
      s_axi_awaddr(30) => '0',
      s_axi_awaddr(29) => '0',
      s_axi_awaddr(28) => '0',
      s_axi_awaddr(27) => '0',
      s_axi_awaddr(26) => '0',
      s_axi_awaddr(25) => '0',
      s_axi_awaddr(24) => '0',
      s_axi_awaddr(23) => '0',
      s_axi_awaddr(22) => '0',
      s_axi_awaddr(21) => '0',
      s_axi_awaddr(20) => '0',
      s_axi_awaddr(19) => '0',
      s_axi_awaddr(18) => '0',
      s_axi_awaddr(17) => '0',
      s_axi_awaddr(16) => '0',
      s_axi_awaddr(15) => '0',
      s_axi_awaddr(14) => '0',
      s_axi_awaddr(13) => '0',
      s_axi_awaddr(12) => '0',
      s_axi_awaddr(11) => '0',
      s_axi_awaddr(10) => '0',
      s_axi_awaddr(9) => '0',
      s_axi_awaddr(8) => '0',
      s_axi_awaddr(7) => '0',
      s_axi_awaddr(6) => '0',
      s_axi_awaddr(5) => '0',
      s_axi_awaddr(4) => '0',
      s_axi_awaddr(3) => '0',
      s_axi_awaddr(2) => '0',
      s_axi_awaddr(1) => '0',
      s_axi_awaddr(0) => '0',
      s_axi_awburst(1) => '0',
      s_axi_awburst(0) => '0',
      s_axi_awcache(3) => '0',
      s_axi_awcache(2) => '0',
      s_axi_awcache(1) => '0',
      s_axi_awcache(0) => '0',
      s_axi_awid(0) => '0',
      s_axi_awlen(7) => '0',
      s_axi_awlen(6) => '0',
      s_axi_awlen(5) => '0',
      s_axi_awlen(4) => '0',
      s_axi_awlen(3) => '0',
      s_axi_awlen(2) => '0',
      s_axi_awlen(1) => '0',
      s_axi_awlen(0) => '0',
      s_axi_awlock(0) => '0',
      s_axi_awprot(2) => '0',
      s_axi_awprot(1) => '0',
      s_axi_awprot(0) => '0',
      s_axi_awqos(3) => '0',
      s_axi_awqos(2) => '0',
      s_axi_awqos(1) => '0',
      s_axi_awqos(0) => '0',
      s_axi_awready => NLW_U0_s_axi_awready_UNCONNECTED,
      s_axi_awregion(3) => '0',
      s_axi_awregion(2) => '0',
      s_axi_awregion(1) => '0',
      s_axi_awregion(0) => '0',
      s_axi_awsize(2) => '0',
      s_axi_awsize(1) => '0',
      s_axi_awsize(0) => '0',
      s_axi_awuser(0) => '0',
      s_axi_awvalid => '0',
      s_axi_bid(0) => NLW_U0_s_axi_bid_UNCONNECTED(0),
      s_axi_bready => '0',
      s_axi_bresp(1 downto 0) => NLW_U0_s_axi_bresp_UNCONNECTED(1 downto 0),
      s_axi_buser(0) => NLW_U0_s_axi_buser_UNCONNECTED(0),
      s_axi_bvalid => NLW_U0_s_axi_bvalid_UNCONNECTED,
      s_axi_rdata(63 downto 0) => NLW_U0_s_axi_rdata_UNCONNECTED(63 downto 0),
      s_axi_rid(0) => NLW_U0_s_axi_rid_UNCONNECTED(0),
      s_axi_rlast => NLW_U0_s_axi_rlast_UNCONNECTED,
      s_axi_rready => '0',
      s_axi_rresp(1 downto 0) => NLW_U0_s_axi_rresp_UNCONNECTED(1 downto 0),
      s_axi_ruser(0) => NLW_U0_s_axi_ruser_UNCONNECTED(0),
      s_axi_rvalid => NLW_U0_s_axi_rvalid_UNCONNECTED,
      s_axi_wdata(63) => '0',
      s_axi_wdata(62) => '0',
      s_axi_wdata(61) => '0',
      s_axi_wdata(60) => '0',
      s_axi_wdata(59) => '0',
      s_axi_wdata(58) => '0',
      s_axi_wdata(57) => '0',
      s_axi_wdata(56) => '0',
      s_axi_wdata(55) => '0',
      s_axi_wdata(54) => '0',
      s_axi_wdata(53) => '0',
      s_axi_wdata(52) => '0',
      s_axi_wdata(51) => '0',
      s_axi_wdata(50) => '0',
      s_axi_wdata(49) => '0',
      s_axi_wdata(48) => '0',
      s_axi_wdata(47) => '0',
      s_axi_wdata(46) => '0',
      s_axi_wdata(45) => '0',
      s_axi_wdata(44) => '0',
      s_axi_wdata(43) => '0',
      s_axi_wdata(42) => '0',
      s_axi_wdata(41) => '0',
      s_axi_wdata(40) => '0',
      s_axi_wdata(39) => '0',
      s_axi_wdata(38) => '0',
      s_axi_wdata(37) => '0',
      s_axi_wdata(36) => '0',
      s_axi_wdata(35) => '0',
      s_axi_wdata(34) => '0',
      s_axi_wdata(33) => '0',
      s_axi_wdata(32) => '0',
      s_axi_wdata(31) => '0',
      s_axi_wdata(30) => '0',
      s_axi_wdata(29) => '0',
      s_axi_wdata(28) => '0',
      s_axi_wdata(27) => '0',
      s_axi_wdata(26) => '0',
      s_axi_wdata(25) => '0',
      s_axi_wdata(24) => '0',
      s_axi_wdata(23) => '0',
      s_axi_wdata(22) => '0',
      s_axi_wdata(21) => '0',
      s_axi_wdata(20) => '0',
      s_axi_wdata(19) => '0',
      s_axi_wdata(18) => '0',
      s_axi_wdata(17) => '0',
      s_axi_wdata(16) => '0',
      s_axi_wdata(15) => '0',
      s_axi_wdata(14) => '0',
      s_axi_wdata(13) => '0',
      s_axi_wdata(12) => '0',
      s_axi_wdata(11) => '0',
      s_axi_wdata(10) => '0',
      s_axi_wdata(9) => '0',
      s_axi_wdata(8) => '0',
      s_axi_wdata(7) => '0',
      s_axi_wdata(6) => '0',
      s_axi_wdata(5) => '0',
      s_axi_wdata(4) => '0',
      s_axi_wdata(3) => '0',
      s_axi_wdata(2) => '0',
      s_axi_wdata(1) => '0',
      s_axi_wdata(0) => '0',
      s_axi_wid(0) => '0',
      s_axi_wlast => '0',
      s_axi_wready => NLW_U0_s_axi_wready_UNCONNECTED,
      s_axi_wstrb(7) => '0',
      s_axi_wstrb(6) => '0',
      s_axi_wstrb(5) => '0',
      s_axi_wstrb(4) => '0',
      s_axi_wstrb(3) => '0',
      s_axi_wstrb(2) => '0',
      s_axi_wstrb(1) => '0',
      s_axi_wstrb(0) => '0',
      s_axi_wuser(0) => '0',
      s_axi_wvalid => '0',
      s_axis_tdata(7) => '0',
      s_axis_tdata(6) => '0',
      s_axis_tdata(5) => '0',
      s_axis_tdata(4) => '0',
      s_axis_tdata(3) => '0',
      s_axis_tdata(2) => '0',
      s_axis_tdata(1) => '0',
      s_axis_tdata(0) => '0',
      s_axis_tdest(0) => '0',
      s_axis_tid(0) => '0',
      s_axis_tkeep(0) => '0',
      s_axis_tlast => '0',
      s_axis_tready => NLW_U0_s_axis_tready_UNCONNECTED,
      s_axis_tstrb(0) => '0',
      s_axis_tuser(3) => '0',
      s_axis_tuser(2) => '0',
      s_axis_tuser(1) => '0',
      s_axis_tuser(0) => '0',
      s_axis_tvalid => '0',
      sbiterr => NLW_U0_sbiterr_UNCONNECTED,
      sleep => '0',
      srst => '0',
      underflow => NLW_U0_underflow_UNCONNECTED,
      valid => NLW_U0_valid_UNCONNECTED,
      wr_ack => NLW_U0_wr_ack_UNCONNECTED,
      wr_clk => wr_clk,
      wr_data_count(5 downto 0) => NLW_U0_wr_data_count_UNCONNECTED(5 downto 0),
      wr_en => wr_en,
      wr_rst => '0',
      wr_rst_busy => NLW_U0_wr_rst_busy_UNCONNECTED
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_dram_rd is
  port (
    valid : out STD_LOGIC;
    dram_rd_addr : out STD_LOGIC_VECTOR ( 14 downto 0 );
    data : out STD_LOGIC_VECTOR ( 15 downto 0 );
    done : out STD_LOGIC;
    dram_rd_en : out STD_LOGIC;
    dram_rd_flush : out STD_LOGIC;
    dram_ready : in STD_LOGIC;
    stall : in STD_LOGIC;
    clear : in STD_LOGIC;
    rd_en : in STD_LOGIC;
    go : in STD_LOGIC;
    dram_clk : in STD_LOGIC;
    user_clk : in STD_LOGIC;
    dram_rd_data : in STD_LOGIC_VECTOR ( 31 downto 0 );
    dram_rd_valid : in STD_LOGIC;
    start_addr : in STD_LOGIC_VECTOR ( 14 downto 0 );
    rst : in STD_LOGIC;
    size : in STD_LOGIC_VECTOR ( 16 downto 0 )
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_dram_rd : entity is "dram_rd";
end dram_rd_ram0_0_dram_rd;

architecture STRUCTURE of dram_rd_ram0_0_dram_rd is
  signal U_HANDSHAKE_n_2 : STD_LOGIC;
  signal U_HANDSHAKE_n_5 : STD_LOGIC;
  signal U_HANDSHAKE_n_6 : STD_LOGIC;
  signal U_WIDTH_FIFO_n_1 : STD_LOGIC;
  signal U_WIDTH_FIFO_n_2 : STD_LOGIC;
  signal busy : STD_LOGIC;
  signal done_s : STD_LOGIC;
  signal dout : STD_LOGIC_VECTOR ( 31 downto 0 );
  signal empty : STD_LOGIC;
  signal got_it_tg_ff : STD_LOGIC;
  signal got_it_tg_sync : STD_LOGIC;
  signal handshake_go : STD_LOGIC;
  signal handshake_go_i_1_n_0 : STD_LOGIC;
  signal prog_full : STD_LOGIC;
  signal rd_en0_in : STD_LOGIC;
  signal size_s : STD_LOGIC_VECTOR ( 16 downto 0 );
  signal start_addr_s : STD_LOGIC_VECTOR ( 14 downto 0 );
  signal \start_addr_s[14]_i_1_n_0\ : STD_LOGIC;
  signal state : STD_LOGIC;
  signal \state[0]_i_1_n_0\ : STD_LOGIC;
  signal \state[1]_i_1_n_0\ : STD_LOGIC;
  signal \state_reg_n_0_[0]\ : STD_LOGIC;
  signal \state_reg_n_0_[1]\ : STD_LOGIC;
  signal NLW_U_FIFO_full_UNCONNECTED : STD_LOGIC;
  attribute CHECK_LICENSE_TYPE : string;
  attribute CHECK_LICENSE_TYPE of U_FIFO : label is "fifo_32_prog_full,fifo_generator_v12_0,{}";
  attribute core_generation_info : string;
  attribute core_generation_info of U_FIFO : label is "fifo_32_prog_full,fifo_generator_v12_0,{x_ipProduct=Vivado 2015.2,x_ipVendor=xilinx.com,x_ipLibrary=ip,x_ipName=fifo_generator,x_ipVersion=12.0,x_ipCoreRevision=4,x_ipLanguage=VERILOG,x_ipSimLanguage=MIXED,C_COMMON_CLOCK=0,C_COUNT_TYPE=0,C_DATA_COUNT_WIDTH=6,C_DEFAULT_VALUE=BlankString,C_DIN_WIDTH=32,C_DOUT_RST_VAL=0,C_DOUT_WIDTH=32,C_ENABLE_RLOCS=0,C_FAMILY=zynq,C_FULL_FLAGS_RST_VAL=1,C_HAS_ALMOST_EMPTY=0,C_HAS_ALMOST_FULL=0,C_HAS_BACKUP=0,C_HAS_DATA_COUNT=0,C_HAS_INT_CLK=0,C_HAS_MEMINIT_FILE=0,C_HAS_OVERFLOW=0,C_HAS_RD_DATA_COUNT=0,C_HAS_RD_RST=0,C_HAS_RST=1,C_HAS_SRST=0,C_HAS_UNDERFLOW=0,C_HAS_VALID=0,C_HAS_WR_ACK=0,C_HAS_WR_DATA_COUNT=0,C_HAS_WR_RST=0,C_IMPLEMENTATION_TYPE=2,C_INIT_WR_PNTR_VAL=0,C_MEMORY_TYPE=1,C_MIF_FILE_NAME=BlankString,C_OPTIMIZATION_MODE=0,C_OVERFLOW_LOW=0,C_PRELOAD_LATENCY=0,C_PRELOAD_REGS=1,C_PRIM_FIFO_TYPE=512x36,C_PROG_EMPTY_THRESH_ASSERT_VAL=4,C_PROG_EMPTY_THRESH_NEGATE_VAL=5,C_PROG_EMPTY_TYPE=0,C_PROG_FULL_THRESH_ASSERT_VAL=50,C_PROG_FULL_THRESH_NEGATE_VAL=49,C_PROG_FULL_TYPE=1,C_RD_DATA_COUNT_WIDTH=6,C_RD_DEPTH=64,C_RD_FREQ=1,C_RD_PNTR_WIDTH=6,C_UNDERFLOW_LOW=0,C_USE_DOUT_RST=1,C_USE_ECC=0,C_USE_EMBEDDED_REG=0,C_USE_PIPELINE_REG=0,C_POWER_SAVING_MODE=0,C_USE_FIFO16_FLAGS=0,C_USE_FWFT_DATA_COUNT=0,C_VALID_LOW=0,C_WR_ACK_LOW=0,C_WR_DATA_COUNT_WIDTH=6,C_WR_DEPTH=64,C_WR_FREQ=1,C_WR_PNTR_WIDTH=6,C_WR_RESPONSE_LATENCY=1,C_MSGON_VAL=1,C_ENABLE_RST_SYNC=1,C_ERROR_INJECTION_TYPE=0,C_SYNCHRONIZER_STAGE=2,C_INTERFACE_TYPE=0,C_AXI_TYPE=1,C_HAS_AXI_WR_CHANNEL=1,C_HAS_AXI_RD_CHANNEL=1,C_HAS_SLAVE_CE=0,C_HAS_MASTER_CE=0,C_ADD_NGC_CONSTRAINT=0,C_USE_COMMON_OVERFLOW=0,C_USE_COMMON_UNDERFLOW=0,C_USE_DEFAULT_SETTINGS=0,C_AXI_ID_WIDTH=1,C_AXI_ADDR_WIDTH=32,C_AXI_DATA_WIDTH=64,C_AXI_LEN_WIDTH=8,C_AXI_LOCK_WIDTH=1,C_HAS_AXI_ID=0,C_HAS_AXI_AWUSER=0,C_HAS_AXI_WUSER=0,C_HAS_AXI_BUSER=0,C_HAS_AXI_ARUSER=0,C_HAS_AXI_RUSER=0,C_AXI_ARUSER_WIDTH=1,C_AXI_AWUSER_WIDTH=1,C_AXI_WUSER_WIDTH=1,C_AXI_BUSER_WIDTH=1,C_AXI_RUSER_WIDTH=1,C_HAS_AXIS_TDATA=1,C_HAS_AXIS_TID=0,C_HAS_AXIS_TDEST=0,C_HAS_AXIS_TUSER=1,C_HAS_AXIS_TREADY=1,C_HAS_AXIS_TLAST=0,C_HAS_AXIS_TSTRB=0,C_HAS_AXIS_TKEEP=0,C_AXIS_TDATA_WIDTH=8,C_AXIS_TID_WIDTH=1,C_AXIS_TDEST_WIDTH=1,C_AXIS_TUSER_WIDTH=4,C_AXIS_TSTRB_WIDTH=1,C_AXIS_TKEEP_WIDTH=1,C_WACH_TYPE=0,C_WDCH_TYPE=0,C_WRCH_TYPE=0,C_RACH_TYPE=0,C_RDCH_TYPE=0,C_AXIS_TYPE=0,C_IMPLEMENTATION_TYPE_WACH=1,C_IMPLEMENTATION_TYPE_WDCH=1,C_IMPLEMENTATION_TYPE_WRCH=1,C_IMPLEMENTATION_TYPE_RACH=1,C_IMPLEMENTATION_TYPE_RDCH=1,C_IMPLEMENTATION_TYPE_AXIS=1,C_APPLICATION_TYPE_WACH=0,C_APPLICATION_TYPE_WDCH=0,C_APPLICATION_TYPE_WRCH=0,C_APPLICATION_TYPE_RACH=0,C_APPLICATION_TYPE_RDCH=0,C_APPLICATION_TYPE_AXIS=0,C_PRIM_FIFO_TYPE_WACH=512x36,C_PRIM_FIFO_TYPE_WDCH=1kx36,C_PRIM_FIFO_TYPE_WRCH=512x36,C_PRIM_FIFO_TYPE_RACH=512x36,C_PRIM_FIFO_TYPE_RDCH=1kx36,C_PRIM_FIFO_TYPE_AXIS=1kx18,C_USE_ECC_WACH=0,C_USE_ECC_WDCH=0,C_USE_ECC_WRCH=0,C_USE_ECC_RACH=0,C_USE_ECC_RDCH=0,C_USE_ECC_AXIS=0,C_ERROR_INJECTION_TYPE_WACH=0,C_ERROR_INJECTION_TYPE_WDCH=0,C_ERROR_INJECTION_TYPE_WRCH=0,C_ERROR_INJECTION_TYPE_RACH=0,C_ERROR_INJECTION_TYPE_RDCH=0,C_ERROR_INJECTION_TYPE_AXIS=0,C_DIN_WIDTH_WACH=32,C_DIN_WIDTH_WDCH=64,C_DIN_WIDTH_WRCH=2,C_DIN_WIDTH_RACH=32,C_DIN_WIDTH_RDCH=64,C_DIN_WIDTH_AXIS=1,C_WR_DEPTH_WACH=16,C_WR_DEPTH_WDCH=1024,C_WR_DEPTH_WRCH=16,C_WR_DEPTH_RACH=16,C_WR_DEPTH_RDCH=1024,C_WR_DEPTH_AXIS=1024,C_WR_PNTR_WIDTH_WACH=4,C_WR_PNTR_WIDTH_WDCH=10,C_WR_PNTR_WIDTH_WRCH=4,C_WR_PNTR_WIDTH_RACH=4,C_WR_PNTR_WIDTH_RDCH=10,C_WR_PNTR_WIDTH_AXIS=10,C_HAS_DATA_COUNTS_WACH=0,C_HAS_DATA_COUNTS_WDCH=0,C_HAS_DATA_COUNTS_WRCH=0,C_HAS_DATA_COUNTS_RACH=0,C_HAS_DATA_COUNTS_RDCH=0,C_HAS_DATA_COUNTS_AXIS=0,C_HAS_PROG_FLAGS_WACH=0,C_HAS_PROG_FLAGS_WDCH=0,C_HAS_PROG_FLAGS_WRCH=0,C_HAS_PROG_FLAGS_RACH=0,C_HAS_PROG_FLAGS_RDCH=0,C_HAS_PROG_FLAGS_AXIS=0,C_PROG_FULL_TYPE_WACH=0,C_PROG_FULL_TYPE_WDCH=0,C_PROG_FULL_TYPE_WRCH=0,C_PROG_FULL_TYPE_RACH=0,C_PROG_FULL_TYPE_RDCH=0,C_PROG_FULL_TYPE_AXIS=0,C_PROG_FULL_THRESH_ASSERT_VAL_WACH=1023,C_PROG_FULL_THRESH_ASSERT_VAL_WDCH=1023,C_PROG_FULL_THRESH_ASSERT_VAL_WRCH=1023,C_PROG_FULL_THRESH_ASSERT_VAL_RACH=1023,C_PROG_FULL_THRESH_ASSERT_VAL_RDCH=1023,C_PROG_FULL_THRESH_ASSERT_VAL_AXIS=1023,C_PROG_EMPTY_TYPE_WACH=0,C_PROG_EMPTY_TYPE_WDCH=0,C_PROG_EMPTY_TYPE_WRCH=0,C_PROG_EMPTY_TYPE_RACH=0,C_PROG_EMPTY_TYPE_RDCH=0,C_PROG_EMPTY_TYPE_AXIS=0,C_PROG_EMPTY_THRESH_ASSERT_VAL_WACH=1022,C_PROG_EMPTY_THRESH_ASSERT_VAL_WDCH=1022,C_PROG_EMPTY_THRESH_ASSERT_VAL_WRCH=1022,C_PROG_EMPTY_THRESH_ASSERT_VAL_RACH=1022,C_PROG_EMPTY_THRESH_ASSERT_VAL_RDCH=1022,C_PROG_EMPTY_THRESH_ASSERT_VAL_AXIS=1022,C_REG_SLICE_MODE_WACH=0,C_REG_SLICE_MODE_WDCH=0,C_REG_SLICE_MODE_WRCH=0,C_REG_SLICE_MODE_RACH=0,C_REG_SLICE_MODE_RDCH=0,C_REG_SLICE_MODE_AXIS=0}";
  attribute downgradeipidentifiedwarnings : string;
  attribute downgradeipidentifiedwarnings of U_FIFO : label is "yes";
  attribute x_core_info : string;
  attribute x_core_info of U_FIFO : label is "fifo_generator_v12_0,Vivado 2015.2";
  attribute SOFT_HLUTNM : string;
  attribute SOFT_HLUTNM of done_INST_0 : label is "soft_lutpair22";
  attribute SOFT_HLUTNM of handshake_go_i_1 : label is "soft_lutpair22";
begin
U_AG_SI: entity work.dram_rd_ram0_0_DRAM_ADDR_GEN
     port map (
      AR(0) => U_WIDTH_FIFO_n_2,
      Q(14 downto 0) => start_addr_s(14 downto 0),
      dram_clk => dram_clk,
      dram_rd_addr(14 downto 0) => dram_rd_addr(14 downto 0),
      \gpf1.prog_full_i_reg\ => U_HANDSHAKE_n_2,
      state => state,
      take_it_tg_ff_reg => U_HANDSHAKE_n_6
    );
U_DONE_COUNT: entity work.dram_rd_ram0_0_done_counter
     port map (
      CO(0) => done_s,
      Q(16 downto 0) => size_s(16 downto 0),
      clear => clear,
      \count_reg[0]_0\ => U_WIDTH_FIFO_n_1,
      rst => rst,
      user_clk => user_clk
    );
U_FIFO: entity work.dram_rd_ram0_0_fifo_32_prog_full
     port map (
      din(31 downto 0) => dram_rd_data(31 downto 0),
      dout(31 downto 0) => dout(31 downto 0),
      empty => empty,
      full => NLW_U_FIFO_full_UNCONNECTED,
      prog_full => prog_full,
      rd_clk => user_clk,
      rd_en => rd_en0_in,
      rst => U_WIDTH_FIFO_n_2,
      wr_clk => dram_clk,
      wr_en => dram_rd_valid
    );
U_HANDSHAKE: entity work.dram_rd_ram0_0_handshake
     port map (
      AR(0) => U_WIDTH_FIFO_n_2,
      \addr_current_reg[0]\ => U_HANDSHAKE_n_2,
      busy => busy,
      busy_reg => U_HANDSHAKE_n_5,
      dram_clk => dram_clk,
      dram_rd_en => dram_rd_en,
      dram_rd_flush => dram_rd_flush,
      dram_ready => dram_ready,
      go => go,
      got_it_tg_ff => got_it_tg_ff,
      got_it_tg_sync => got_it_tg_sync,
      handshake_go => handshake_go,
      prog_full => prog_full,
      stall => stall,
      state => state,
      \state_reg[0]\ => \state_reg_n_0_[0]\,
      \state_reg[1]\ => \state_reg_n_0_[1]\,
      state_reg_0 => U_HANDSHAKE_n_6,
      user_clk => user_clk
    );
U_WIDTH_FIFO: entity work.dram_rd_ram0_0_width_change_fifo
     port map (
      AR(0) => U_WIDTH_FIFO_n_2,
      CO(0) => done_s,
      clear => clear,
      \count_reg[16]\ => U_WIDTH_FIFO_n_1,
      data(15 downto 0) => data(15 downto 0),
      dout(31 downto 0) => dout(31 downto 0),
      empty => empty,
      rd_en => rd_en,
      rd_en0_in => rd_en0_in,
      rst => rst,
      user_clk => user_clk,
      valid => valid
    );
busy_reg: unisim.vcomponents.FDPE
     port map (
      C => user_clk,
      CE => '1',
      D => U_HANDSHAKE_n_5,
      PRE => rst,
      Q => busy
    );
done_INST_0: unisim.vcomponents.LUT3
    generic map(
      INIT => X"02"
    )
        port map (
      I0 => done_s,
      I1 => busy,
      I2 => go,
      O => done
    );
handshake_go_i_1: unisim.vcomponents.LUT4
    generic map(
      INIT => X"00E2"
    )
        port map (
      I0 => go,
      I1 => \state_reg_n_0_[1]\,
      I2 => done_s,
      I3 => \state_reg_n_0_[0]\,
      O => handshake_go_i_1_n_0
    );
handshake_go_reg: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => '1',
      CLR => rst,
      D => handshake_go_i_1_n_0,
      Q => handshake_go
    );
\size_s_reg[0]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => size(0),
      Q => size_s(0)
    );
\size_s_reg[10]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => size(10),
      Q => size_s(10)
    );
\size_s_reg[11]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => size(11),
      Q => size_s(11)
    );
\size_s_reg[12]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => size(12),
      Q => size_s(12)
    );
\size_s_reg[13]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => size(13),
      Q => size_s(13)
    );
\size_s_reg[14]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => size(14),
      Q => size_s(14)
    );
\size_s_reg[15]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => size(15),
      Q => size_s(15)
    );
\size_s_reg[16]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => size(16),
      Q => size_s(16)
    );
\size_s_reg[1]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => size(1),
      Q => size_s(1)
    );
\size_s_reg[2]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => size(2),
      Q => size_s(2)
    );
\size_s_reg[3]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => size(3),
      Q => size_s(3)
    );
\size_s_reg[4]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => size(4),
      Q => size_s(4)
    );
\size_s_reg[5]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => size(5),
      Q => size_s(5)
    );
\size_s_reg[6]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => size(6),
      Q => size_s(6)
    );
\size_s_reg[7]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => size(7),
      Q => size_s(7)
    );
\size_s_reg[8]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => size(8),
      Q => size_s(8)
    );
\size_s_reg[9]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => size(9),
      Q => size_s(9)
    );
\start_addr_s[14]_i_1\: unisim.vcomponents.LUT3
    generic map(
      INIT => X"02"
    )
        port map (
      I0 => go,
      I1 => \state_reg_n_0_[0]\,
      I2 => \state_reg_n_0_[1]\,
      O => \start_addr_s[14]_i_1_n_0\
    );
\start_addr_s_reg[0]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => start_addr(0),
      Q => start_addr_s(0)
    );
\start_addr_s_reg[10]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => start_addr(10),
      Q => start_addr_s(10)
    );
\start_addr_s_reg[11]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => start_addr(11),
      Q => start_addr_s(11)
    );
\start_addr_s_reg[12]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => start_addr(12),
      Q => start_addr_s(12)
    );
\start_addr_s_reg[13]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => start_addr(13),
      Q => start_addr_s(13)
    );
\start_addr_s_reg[14]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => start_addr(14),
      Q => start_addr_s(14)
    );
\start_addr_s_reg[1]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => start_addr(1),
      Q => start_addr_s(1)
    );
\start_addr_s_reg[2]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => start_addr(2),
      Q => start_addr_s(2)
    );
\start_addr_s_reg[3]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => start_addr(3),
      Q => start_addr_s(3)
    );
\start_addr_s_reg[4]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => start_addr(4),
      Q => start_addr_s(4)
    );
\start_addr_s_reg[5]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => start_addr(5),
      Q => start_addr_s(5)
    );
\start_addr_s_reg[6]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => start_addr(6),
      Q => start_addr_s(6)
    );
\start_addr_s_reg[7]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => start_addr(7),
      Q => start_addr_s(7)
    );
\start_addr_s_reg[8]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => start_addr(8),
      Q => start_addr_s(8)
    );
\start_addr_s_reg[9]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => \start_addr_s[14]_i_1_n_0\,
      CLR => rst,
      D => start_addr(9),
      Q => start_addr_s(9)
    );
\state[0]_i_1\: unisim.vcomponents.LUT6
    generic map(
      INIT => X"C3AAC3AAC3FFC300"
    )
        port map (
      I0 => done_s,
      I1 => got_it_tg_ff,
      I2 => got_it_tg_sync,
      I3 => \state_reg_n_0_[0]\,
      I4 => go,
      I5 => \state_reg_n_0_[1]\,
      O => \state[0]_i_1_n_0\
    );
\state[1]_i_1\: unisim.vcomponents.LUT4
    generic map(
      INIT => X"9F60"
    )
        port map (
      I0 => got_it_tg_ff,
      I1 => got_it_tg_sync,
      I2 => \state_reg_n_0_[0]\,
      I3 => \state_reg_n_0_[1]\,
      O => \state[1]_i_1_n_0\
    );
\state_reg[0]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => '1',
      CLR => rst,
      D => \state[0]_i_1_n_0\,
      Q => \state_reg_n_0_[0]\
    );
\state_reg[1]\: unisim.vcomponents.FDCE
     port map (
      C => user_clk,
      CE => '1',
      CLR => rst,
      D => \state[1]_i_1_n_0\,
      Q => \state_reg_n_0_[1]\
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0_dram_rd_ram0 is
  port (
    valid : out STD_LOGIC;
    dram_rd_addr : out STD_LOGIC_VECTOR ( 14 downto 0 );
    data : out STD_LOGIC_VECTOR ( 15 downto 0 );
    done : out STD_LOGIC;
    dram_rd_en : out STD_LOGIC;
    dram_rd_flush : out STD_LOGIC;
    dram_ready : in STD_LOGIC;
    stall : in STD_LOGIC;
    clear : in STD_LOGIC;
    rd_en : in STD_LOGIC;
    go : in STD_LOGIC;
    dram_clk : in STD_LOGIC;
    user_clk : in STD_LOGIC;
    dram_rd_data : in STD_LOGIC_VECTOR ( 31 downto 0 );
    dram_rd_valid : in STD_LOGIC;
    start_addr : in STD_LOGIC_VECTOR ( 14 downto 0 );
    rst : in STD_LOGIC;
    size : in STD_LOGIC_VECTOR ( 16 downto 0 )
  );
  attribute ORIG_REF_NAME : string;
  attribute ORIG_REF_NAME of dram_rd_ram0_0_dram_rd_ram0 : entity is "dram_rd_ram0";
end dram_rd_ram0_0_dram_rd_ram0;

architecture STRUCTURE of dram_rd_ram0_0_dram_rd_ram0 is
begin
U_DRAM0_RD: entity work.dram_rd_ram0_0_dram_rd
     port map (
      clear => clear,
      data(15 downto 0) => data(15 downto 0),
      done => done,
      dram_clk => dram_clk,
      dram_rd_addr(14 downto 0) => dram_rd_addr(14 downto 0),
      dram_rd_data(31 downto 0) => dram_rd_data(31 downto 0),
      dram_rd_en => dram_rd_en,
      dram_rd_flush => dram_rd_flush,
      dram_rd_valid => dram_rd_valid,
      dram_ready => dram_ready,
      go => go,
      rd_en => rd_en,
      rst => rst,
      size(16 downto 0) => size(16 downto 0),
      stall => stall,
      start_addr(14 downto 0) => start_addr(14 downto 0),
      user_clk => user_clk,
      valid => valid
    );
end STRUCTURE;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity dram_rd_ram0_0 is
  port (
    dram_clk : in STD_LOGIC;
    user_clk : in STD_LOGIC;
    rst : in STD_LOGIC;
    clear : in STD_LOGIC;
    go : in STD_LOGIC;
    rd_en : in STD_LOGIC;
    stall : in STD_LOGIC;
    start_addr : in STD_LOGIC_VECTOR ( 14 downto 0 );
    size : in STD_LOGIC_VECTOR ( 16 downto 0 );
    valid : out STD_LOGIC;
    data : out STD_LOGIC_VECTOR ( 15 downto 0 );
    done : out STD_LOGIC;
    dram_ready : in STD_LOGIC;
    dram_rd_en : out STD_LOGIC;
    dram_rd_addr : out STD_LOGIC_VECTOR ( 14 downto 0 );
    dram_rd_data : in STD_LOGIC_VECTOR ( 31 downto 0 );
    dram_rd_valid : in STD_LOGIC;
    dram_rd_flush : out STD_LOGIC
  );
  attribute NotValidForBitStream : boolean;
  attribute NotValidForBitStream of dram_rd_ram0_0 : entity is true;
  attribute CHECK_LICENSE_TYPE : string;
  attribute CHECK_LICENSE_TYPE of dram_rd_ram0_0 : entity is "dram_rd_ram0_0,dram_rd_ram0,{}";
  attribute downgradeipidentifiedwarnings : string;
  attribute downgradeipidentifiedwarnings of dram_rd_ram0_0 : entity is "yes";
  attribute x_core_info : string;
  attribute x_core_info of dram_rd_ram0_0 : entity is "dram_rd_ram0,Vivado 2015.2";
end dram_rd_ram0_0;

architecture STRUCTURE of dram_rd_ram0_0 is
begin
U0: entity work.dram_rd_ram0_0_dram_rd_ram0
     port map (
      clear => clear,
      data(15 downto 0) => data(15 downto 0),
      done => done,
      dram_clk => dram_clk,
      dram_rd_addr(14 downto 0) => dram_rd_addr(14 downto 0),
      dram_rd_data(31 downto 0) => dram_rd_data(31 downto 0),
      dram_rd_en => dram_rd_en,
      dram_rd_flush => dram_rd_flush,
      dram_rd_valid => dram_rd_valid,
      dram_ready => dram_ready,
      go => go,
      rd_en => rd_en,
      rst => rst,
      size(16 downto 0) => size(16 downto 0),
      stall => stall,
      start_addr(14 downto 0) => start_addr(14 downto 0),
      user_clk => user_clk,
      valid => valid
    );
end STRUCTURE;
