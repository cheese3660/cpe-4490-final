--------------------------------------------------------------------------------
-- Author: Lexi Allen
--
-- Create Date: 11/15/2024 10:19:28 AM
-- Design Name: VPE Serial Interface
-- Module Name: TransceiverController - Structural
-- Description:
--  Combines the UART -> VPE, VPE -> UART and memory sections of the transceiver
--  into one block for ease of wrapping
--------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity TransceiverController is
    generic (
        -- How much memory should be allocated for the TX and RX portions
        BUFFER_SIZE: integer := 200000;

        -- Should the memory be allocated in separate buffers, each getting the
        -- full amount, or should they be combined into one buffer?
        SEPARATE_BUFFERS: boolean := false;

        -- The clock frequency of the device that this is being implemented on
        CLOCK_FREQUENCY: integer := 100_000_000
    );
    Port ( clock : in STD_LOGIC;
           reset : in STD_LOGIC;

           -- The incoming UART signal
           uartRx : in STD_LOGIC;

           -- The incoming VPE signal
           vpeRx : in STD_LOGIC;

           -- Trigger a send of the UART data over VPE
           sendToVpeEn : in STD_LOGIC;

           -- Trigger a send of the VPE data over UART
           sendToUartEn : in STD_LOGIC;

           -- The outgoing UART signal
           uartTx : out STD_LOGIC;

           -- The outgoing VPE signal
           vpeTx : out STD_LOGIC;
           
           -- The state + address value for the UART -> VPE portion
           sevenSegmentHexTx : out STD_LOGIC_VECTOR (15 downto 0);
           -- The state + address value for the VPE -> UART portion
           sevenSegmentHexRx : out STD_LOGIC_VECTOR (15 downto 0));
end TransceiverController;

architecture Structural of TransceiverController is
    -- COMPONENTS

    -- The memory buffer being used for the transceiver
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
    
    -- The VPE -> UART portion of the transceiver
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
    
    -- The UART -> VPE portion of the transceiver
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
    
    -- The memory control signals for the transmitter and receiver halves
    signal tx_data: std_logic_vector(7 downto 0);
    signal rx_data: std_logic_vector(7 downto 0);
    signal tx_write_data: std_logic_vector(7 downto 0);
    signal rx_write_data: std_logic_vector(7 downto 0);
    signal tx_write_en: std_logic;
    signal rx_write_en: std_logic;
    signal tx_addr: integer range 0 to BUFFER_SIZE-1;
    signal rx_addr: integer range 0 to BUFFER_SIZE-1;
begin

    -- [MEM]
    -- Map the memory to both halves of the transceiver
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
    
    -- [RX]
    -- The VPE -> UART portion of the transceiver
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
    
    -- [TX]
    -- The UART -> VPE portion of the transceiver
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
