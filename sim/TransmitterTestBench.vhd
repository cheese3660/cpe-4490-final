----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 11/16/2024 07:04:29 AM
-- Design Name: 
-- Module Name: TransmitterTestBench - TestBench
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
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity TransmitterTestBench is
--  Port ( );
end TransmitterTestBench;

architecture TestBench of TransmitterTestBench is

    
    component TransceiverController is
        generic (
            BUFFER_SIZE: integer := 200000;
            SEPARATE_BUFFERS: boolean := false
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
    end component;
    
    component VpeRx is
        Generic (
            -- The width of data you wish to receive
            DATA_WIDTH: integer range 4 to integer'high := 32
        );
        Port ( 
               -- The serial input port
               vpeSerial : in STD_LOGIC;
               clock : in STD_LOGIC;
               reset : in STD_LOGIC;
               -- The last word received
               data : out STD_LOGIC_VECTOR(DATA_WIDTH-1 downto 0);
               -- Signals that a new word has been received
               newDataEn : out STD_LOGIC;
               -- Signals that the frame has been ended
               endFrameEn : out STD_LOGIC);
    end component;
    
    component UartTx is  
        generic(
            BAUD_RATE: positive  := 115200;
            CLOCK_FREQ: positive := 100_000_000
            );
        port(
            clock:       in   std_logic;
            reset:       in   std_logic;
            txEn:        in   std_logic;
            dataIn:      in   std_logic_vector(7 downto 0);
            txComplete:  out  std_logic;
            dataOut:     out  std_logic
            );
    end component;

    -- Constants
    constant ACTIVE: std_logic := '1';

    -- Signals
    signal clock: std_logic;
    signal reset: std_logic;
    signal uartRx: std_logic;
    signal sendToVpeEn: std_logic;
    signal vpeTx: std_logic;
    signal dataIn: std_logic_vector(7 downto 0);
    signal txEn: std_logic;
    signal txComplete: std_logic;
    signal dataOut: std_logic_vector(7 downto 0);
    signal newDataEn: std_logic;
    signal frameEndEn: std_logic;
    
    -- Functions
    function nibble_to_text(nibble: std_logic_vector(3 downto 0)) return character is
    begin
        case nibble is  	 
        	when X"0" =>
            	return '0';  	 
        	when X"1" =>
            	return '1';
        	when X"2" =>
            	return '2';
        	when X"3" =>
            	return '3';
        	when X"4" =>
            	return '4';
        	when X"5" =>
            	return '5';
        	when X"6" =>
            	return '6';
        	when X"7" =>
            	return '7';
        	when X"8" =>
            	return '8';
        	when X"9" =>
            	return '9';
        	when X"A" =>
            	return 'A';
        	when X"B" =>
            	return 'B';
        	when X"C" =>
            	return 'C';
        	when X"D" =>
            	return 'D';
        	when X"E" =>
            	return 'E';
        	when X"F" =>
            	return 'F';
        	when others =>
            	return '?';
    	end case;
    end function;
    
    
    -- Converts a word to a string for reporting
    function get_word(word: std_logic_vector(7 downto 0)) return string is
        variable word_string: string(1 to 2);
    begin
        for i in 1 downto 0 loop
            word_string((1-i)+1) := nibble_to_text(word(i*4+3 downto i*4));
        end loop;
        return word_string;
    end function;
    
    
    -- This will just send up to count hex values that represent 0 through count-1
    procedure send_data
    (
        constant timebase: in integer;
        constant count: in integer range 1 to 255;
        
        signal clock: in std_logic;
        signal reset: in std_logic;
        
        signal tx_complete: in std_logic;
        signal frame_end: in std_logic;
        
        signal tx_data: out std_logic_vector(7 downto 0);
        signal send_to_vpe: out std_logic;
        signal tx_en: out std_logic
    )
    is
        variable timebase_vector: std_logic_vector(31 downto 0);
        variable count_vector: std_logic_vector(7 downto 0);
        variable i_vector: std_logic_vector(7 downto 0);
    begin
        -- First let's send the header
        timebase_vector := std_logic_vector(to_unsigned(timebase,32));
        count_vector := std_logic_vector(to_unsigned(count,8));
        
        -- Send a count of 4 for the timebase length
        
        tx_data <= X"04";
        if tx_complete /= ACTIVE then
            wait until tx_complete = ACTIVE;
        end if;
        report "Sending header time base of: " & integer'image(timebase);
        wait until rising_edge(clock);
        tx_en <= ACTIVE;
        wait until rising_edge(clock);
        tx_en <= not ACTIVE;
        
        -- Send the time base length
        wait until tx_complete = ACTIVE;
        wait until rising_edge(clock);
        tx_data <= timebase_vector(31 downto 24);
        tx_en <= ACTIVE;
        wait until rising_edge(clock);
        tx_en <= not ACTIVE;
        
        wait until tx_complete = ACTIVE;
        wait until rising_edge(clock);
        tx_data <= timebase_vector(23 downto 16);
        tx_en <= ACTIVE;
        wait until rising_edge(clock);
        tx_en <= not ACTIVE;
        
        wait until tx_complete = ACTIVE;
        wait until rising_edge(clock);
        tx_data <= timebase_vector(15 downto 8);
        tx_en <= ACTIVE;
        wait until rising_edge(clock);
        tx_en <= not ACTIVE;
        
        wait until tx_complete = ACTIVE;
        wait until rising_edge(clock);
        tx_data <= timebase_vector(7 downto 0);
        tx_en <= ACTIVE;
        wait until rising_edge(clock);
        tx_en <= not ACTIVE;
        
        
        -- Send a count of 1 for the data length
        wait until tx_complete = ACTIVE;
        report "Sending header count of: " & integer'image(count);
        wait until rising_edge(clock);
        tx_data <= X"01";
        tx_en <= ACTIVE;
        wait until rising_edge(clock);
        tx_en <= not ACTIVE;
        
        -- Send the data length
        wait until tx_complete = ACTIVE;
        wait until rising_edge(clock);
        tx_data <= count_vector;
        tx_en <= ACTIVE;
        wait until rising_edge(clock);
        tx_en <= not ACTIVE;
        
        report "Sending header data";
        -- Send the data
        for i in 0 to count-1 loop
            i_vector := std_logic_vector(to_unsigned(i,8));
            wait until tx_complete = ACTIVE;
            wait until rising_edge(clock);
            tx_data <= i_vector;
            tx_en <= ACTIVE;
            wait until rising_edge(clock);
            tx_en <= not ACTIVE;
        end loop;
        wait until tx_complete = ACTIVE;
        wait until rising_edge(clock);
        wait for 10000 ns;
        
        report "Sending over VPE";
        wait until rising_edge(clock);
        send_to_vpe <= ACTIVE;
        wait until rising_edge(clock);
        send_to_vpe <= not ACTIVE;
        
        wait until rising_edge(frame_end);
        wait until rising_edge(frame_end);
        
    end procedure;
begin
    CLOCK_RESET: process
    begin
        reset <= ACTIVE;
        clock <= not ACTIVE;
        wait for 5 ns;
        reset <= not ACTIVE;
        loop
            wait for 5 ns;
            clock <= not clock;
        end loop;
    end process;
    
    TRANSCEIVER: TransceiverController generic map(
        BUFFER_SIZE => 16384,
        SEPARATE_BUFFERS => true
    ) port map(
        clock => clock,
        reset => reset,
        uartRx => uartRx,
        vpeRx => '0',
        sendToVpeEn => sendToVpeEn,
        sendToUartEn => '0',
        vpeTx => vpeTx
        -- sevenSegmentHex => hexDebug
    );
    
    TRANSMITTER: UartTx port map(
        clock => clock,
        reset => reset,
        txEn => txEn,
        dataIn => dataIn,
        txComplete => txComplete,
        dataOut => uartRx
    );
    
    RECEIVER: VpeRx generic map(
        DATA_WIDTH => 8
    ) port map(
        vpeSerial => vpeTx,
        clock => clock,
        reset => reset,
        data => dataOut,
        newDataEn => newDataEn,
        endFrameEn => frameEndEn
    );
    
    DRIVE_TRANSMITTER: process
        constant MICROSECOND: integer := 1000;
    begin
        sendToVpeEn <= not ACTIVE;
        send_data(100 * MICROSECOND, 10, clock, reset, txComplete, frameEndEn, dataIn, sendToVpeEn, txEn);
        wait for 1000 ns;
        send_data(50 * MICROSECOND, 5, clock, reset, txComplete, frameEndEn, dataIn, sendToVpeEn, txEn);
        wait for 1000 ns;
        send_data(10 * MICROSECOND, 127, clock, reset, txComplete, frameEndEn, dataIn, sendToVpeEn, txEn);
        loop
            wait for 1sec;
        end loop;
    end process;
    
    REPORT_DATA: process
        variable hasFrameBegun: boolean := false;
    begin
        wait until rising_edge(clock);
        if (newDataEn = '1') then
            if not hasFrameBegun then
                report "/* BEGIN FRAME */ {";
                hasFrameBegun := true;
            end if;
            report "    WORD: " & get_word(dataOut);
        end if;
        if (frameEndEn = '1') then
            report "} /* END FRAME */";
            hasFrameBegun := false;
        end if;
    end process;

end TestBench;
