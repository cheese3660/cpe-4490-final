----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 11/15/2024 10:19:28 AM
-- Design Name: 
-- Module Name: TransceiverController - Structural
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

entity TransceiverController is
    generic (
        BUFFER_SIZE: integer := 200000;
        SEPARATE_BUFFERS: boolean := false;
        CLOCK_FREQUENCY: integer := 100_000_000
    );
    Port ( clock : in STD_LOGIC;
           reset : in STD_LOGIC;
           uartRx : in STD_LOGIC;
           vpeRx : in STD_LOGIC;
           sendToVpeEn : in STD_LOGIC;
           sendToUartEn : in STD_LOGIC;
           uartTx : out STD_LOGIC;
           vpeTx : out STD_LOGIC;
           sevenSegmentHexTx : out STD_LOGIC_VECTOR (15 downto 0);
           sevenSegmentHexRx : out STD_LOGIC_VECTOR (15 downto 0));
end TransceiverController;

architecture Structural of TransceiverController is
    component Memory
        generic (
            BUFFER_SIZE: integer := 200000;
            DATA_SIZE: integer := 8;
            SEPARATE_BUFFERS: boolean := false
        );
        port(
            reset: in std_logic;
            clock: in std_logic;
            txAddress: in integer range 0 to BUFFER_SIZE-1;
            rxAddress: in integer range 0 to BUFFER_SIZE-1;
            txWriteData: in std_logic_vector(DATA_SIZE-1 downto 0);
            rxWriteData: in std_logic_vector(DATA_SIZE-1 downto 0);
            txWriteEn: in std_logic;
            rxWriteEn: in std_logic;
            outTx: out std_logic_vector(DATA_SIZE-1 downto 0);
            outRx: out std_logic_vector(DATA_SIZE-1 downto 0)
        );
    end component;
    
    
    component ReceiverController
        generic (
            BUFFER_SIZE: integer := 200000;
            CLOCK_FREQUENCY: integer := 100_000_000
        );
        Port ( clock : in STD_LOGIC;
               reset : in STD_LOGIC;
               vpeRxLine : in STD_LOGIC;
               sendToUartEn : in STD_LOGIC;
               data : in STD_LOGIC_VECTOR (7 downto 0);
               address : out integer range 0 to BUFFER_SIZE-1;
               uartTxLine : out STD_LOGIC;
               writeEn : out STD_LOGIC;
               sevenSegmentHex : out STD_LOGIC_VECTOR (15 downto 0);
               writeData: out STD_LOGIC_VECTOR(7 downto 0));
    end component;
    
    component TransmitterController
        generic (
            BUFFER_SIZE: integer := 200000;
            CLOCK_FREQUENCY: integer := 100_000_000
        );
        Port ( clock : in STD_LOGIC;
               reset : in STD_LOGIC;
               uartRxLine : in STD_LOGIC;
               sendToVpeEn : in STD_LOGIC;
               data : in STD_LOGIC_VECTOR (7 downto 0);
               address : out integer range 0 to BUFFER_SIZE-1;
               vpeTxLine : out STD_LOGIC;
               writeEn : out STD_LOGIC;
               writeData: out STD_LOGIC_VECTOR(7 downto 0);
               -- debug
               sevenSegmentHex : out STD_LOGIC_VECTOR (15 downto 0));
    end component;
    
    signal tx_data: std_logic_vector(7 downto 0);
    signal rx_data: std_logic_vector(7 downto 0);
    signal tx_write_data: std_logic_vector(7 downto 0);
    signal rx_write_data: std_logic_vector(7 downto 0);
    signal tx_write_en: std_logic;
    signal rx_write_en: std_logic;
    signal tx_addr: integer range 0 to BUFFER_SIZE-1;
    signal rx_addr: integer range 0 to BUFFER_SIZE-1;
begin
    MEM: Memory generic map (
        BUFFER_SIZE => BUFFER_SIZE,
        DATA_SIZE => 8,
        SEPARATE_BUFFERS => SEPARATE_BUFFERS
    ) port map(
        reset => reset,
        clock => clock,
        txAddress => tx_addr,
        rxAddress => rx_addr,
        txWriteData => tx_write_data,
        rxWriteData => rx_write_data,
        txWriteEn => tx_write_en,
        rxWriteEn => rx_write_en,
        outTx => tx_data,
        outRx => rx_data
    );
    
    RX: ReceiverController generic map (
        BUFFER_SIZE => BUFFER_SIZE,
        CLOCK_FREQUENCY => CLOCK_FREQUENCY
    ) port map(
        reset => reset,
        clock => clock,
        vpeRxLine => vpeRx,
        sendToUartEn => sendToUartEn,
        data => rx_data,
        address => rx_addr,
        uartTxLine => uartTx,
        writeEn => rx_write_en,
        sevenSegmentHex => sevenSegmentHexRx,
        writeData => rx_write_data
    );
    
    TX: TransmitterController generic map (
        BUFFER_SIZE => BUFFER_SIZE,
        CLOCK_FREQUENCY => CLOCK_FREQUENCY
    ) port map(
        reset => reset,
        clock => clock,
        uartRxLine => uartRx,
        sendToVpeEn => sendToVpeEn,
        data => tx_data,
        address => tx_addr,
        vpeTxLine => vpeTx,
        writeEn => tx_write_en,
        sevenSegmentHex => sevenSegmentHexTx,
        writeData => tx_write_data
    );
end Structural;
