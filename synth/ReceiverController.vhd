----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 11/15/2024 10:25:08 AM
-- Design Name: 
-- Module Name: ReceiverController - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity ReceiverController is
    generic (
        BUFFER_SIZE: integer := 200000
    );
    Port ( clock : in STD_LOGIC;
           reset : in STD_LOGIC;
           vpeRxLine : in STD_LOGIC;
           sendToUartEn : in STD_LOGIC;
           data : in STD_LOGIC_VECTOR (7 downto 0);
           address : out integer range 0 to BUFFER_SIZE-1;
           uartTx : out STD_LOGIC;
           writeEn : out STD_LOGIC;
           sevenSegmentHex : out STD_LOGIC_VECTOR (15 downto 0);
           writeData: out STD_LOGIC_VECTOR(7 downto 0));
end ReceiverController;

architecture Behavioral of ReceiverController is

begin


end Behavioral;
