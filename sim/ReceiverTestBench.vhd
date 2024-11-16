----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 11/16/2024 02:57:29 PM
-- Design Name: 
-- Module Name: ReceiverTestBench - Receiver
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

entity ReceiverTestBench is
--  Port ( );
end ReceiverTestBench;

architecture Receiver of ReceiverTestBench is

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
    
    component TimeBasedVpeTx is
    
        Generic (
           NIBBLES: integer := 8
        );
        Port (
            t0Time : in time;                       	-- Defines the time base of the VPE signal
            data : in std_logic_vector(
               NIBBLES*4-1 downto 0
            );	                                        -- This is the current word being sent
            txMode : in std_logic;                  	-- When this gets flipped high, a transmission
                                                        -- starts being sent
            vpeSerial : out std_logic;              	-- The signal that the transmission is being
                                                        -- sent along
            wordEndEn : out std_logic               	-- This turns high at the beginning of the
                                                        -- ultimate pulse of a word being set, when it
                                                        -- goes high,
                                                        -- you can deassert the txmode until it goes
                                                        -- low to stop the frame, or keep it asserted
                                                        -- to
                                                        -- continue the frame, if you deassert txmode,
                                                        -- you can reassert it right after wordEndEn
                                                        -- goes low
                                                        -- to immediately send a frame after the
                                                        -- current one
        );
    end component;
    
    
    component UartRx is
        generic(
            BAUD_RATE: positive  := 115200;
            CLOCK_FREQ: positive := 100_000_000
            );
        port(
            clock:     in   std_logic;
            reset:     in   std_logic;
            rxData:    in   std_logic;
            dataReady: out  std_logic;
            dataOut:   out  std_logic_vector(7 downto 0)
            );
    end component;
    
    -- Constants
    constant ACTIVE: std_logic := '1';
    
    -- Signals
    signal clock: std_logic;
    signal reset: std_logic;
    signal uartTx: std_logic;
    signal sendToUartEn: std_logic;
    signal vpeRx: std_logic;
    signal dataIn: std_logic_vector(7 downto 0);
    signal txMode: std_logic;
    signal txTimebase: time;
    signal wordEndEn: std_logic;
    signal dataOut: std_logic_vector(7 downto 0);
    signal dataReady: std_logic;
    
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
    
    -- procedures
    
    procedure send_data(
        constant timebase: in time;
        constant count: in integer range 1 to 255;
        
        signal clock: in std_logic;
        
        signal word_end: in std_logic;
        
        signal vpe_data: in std_logic;
        
        signal send_to_uart: out std_logic;
        signal tx_timebase: out time;
        signal tx_data: out std_logic_vector(7 downto 0);
        signal tx_mode: out std_logic
    ) 
    is
        variable timebase_vector: std_logic_vector(31 downto 0);
        variable count_vector: std_logic_vector(7 downto 0);
        variable i_vector: std_logic_vector(7 downto 0);
    begin
        timebase_vector := std_logic_vector(to_unsigned(timebase/1ns,32));
        count_vector := std_logic_vector(to_unsigned(count,8));
        
        send_to_uart <= not ACTIVE;
        
        -- Send 
        tx_timebase <= 100us;
        tx_data <= X"04";
        
        report "Sending header time base of: " & integer'image(timebase/1ns);
        tx_mode <= ACTIVE;
        wait until word_end = '1';
        tx_data <= timebase_vector(31 downto 24);
        wait until word_end = '0';
        wait until word_end = '1';
        tx_data <= timebase_vector(23 downto 16);
        wait until word_end = '0';
        wait until word_end = '1';
        tx_data <= timebase_vector(15 downto 8);
        wait until word_end = '0';
        wait until word_end = '1';
        tx_data <= timebase_vector(7 downto 0);
        wait until word_end = '0';
        
        wait until word_end = '1';
        tx_data <= X"01";
        wait until word_end = '0';
        
        report "Sending header count of: " & integer'image(count);
        wait until word_end = '1';
        tx_data <= count_vector(7 downto 0);
        wait until word_end = '0';
        
        wait until word_end = '1';
        tx_mode <= not ACTIVE;
        wait until word_end = '0';
        
        wait until falling_edge(vpe_data);
        
        -- Change the timebase
        tx_timebase <= timebase;
        report "Sending data";
        
        tx_data <= X"00";
        tx_mode <= ACTIVE;
        
        
        if count > 1 then
            for i in 1 to count-1 loop
                wait until word_end = '1';
                i_vector := std_logic_vector(to_unsigned(i,8));
                tx_data <= i_vector;
                wait until word_end = '0';
            end loop;
        end if;
        
        wait until word_end = '1';
        tx_mode <= not ACTIVE;
        wait until word_end = '0';
        
        wait until falling_edge(vpe_data);
        
        wait for 1000us;
        
        report "Sending over UART";
        
        wait until rising_edge(clock);
        send_to_uart <= ACTIVE;
        wait until rising_edge(clock);
        send_to_uart <= not ACTIVE;
        
        wait for 5000us;
        
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
        uartRx => '0',
        vpeRx => vpeRx,
        sendToVpeEn => '0',
        sendToUartEn => sendToUartEn,
        uartTx => uartTx
    );
    
    TRANSMITTER: TimeBasedVpeTx generic map (
        NIBBLES => 2
    ) port map (
        t0Time => txTimebase,
        data => dataIn,
        txMode => txMode,
        vpeSerial => vpeRx,
        wordEndEn => wordEndEn
    );
    
    RECEIVER: UartRx port map(
        clock => clock,
        reset => reset,
        rxData => uartTx,
        dataReady => dataReady,
        dataOut => dataOut
    );

    DRIVE_RECEIVER: process
    begin
        send_data(100us, 10, clock, wordEndEn, vpeRx, sendToUartEn, txTimebase, dataIn, txMode);
        send_data(50us, 5, clock, wordEndEn, vpeRx, sendToUartEn, txTimebase, dataIn, txMode);
        send_data(10us, 127, clock, wordEndEn, vpeRx, sendToUartEn, txTimebase, dataIn, txMode);
        loop
            wait for 1sec;
        end loop;
    end process;

    REPORT_DATA: process
    begin
        wait until rising_edge(clock);
        if (dataReady = ACTIVE) then
            report "WORD: " & get_word(dataOut);
        end if;
    end process;
end Receiver;
