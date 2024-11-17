--------------------------------------------------------------------------------
-- Author: Lexi Allen
-- Create Date: 11/16/2024 05:02:06 PM
-- Design Name: VPE Serial Interface
-- Module Name: RoundTripTestBench - TestBench
-- Description:
--  Tests the UART -> VPE -> UART capabilities of the UART <-> VPE transceiver
--------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use IEEE.NUMERIC_STD.ALL;

entity RoundTripTestBench is
end RoundTripTestBench;

architecture TestBench of RoundTripTestBench is

    -- COMPONENTS

    -- The UART <-> VPE transceiver we are testing
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

    -- A UART receiver to read the UART output from the Transceiver
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

    -- A UART transmitter to feed data into the transceiver
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
    signal uartInData: std_logic;
    signal sendToVpeEn: std_logic;
    signal sendToUartEn: std_logic;
    signal vpeSerial: std_logic;
    signal dataIn: std_logic_vector(7 downto 0);
    signal txEn: std_logic;
    signal txComplete: std_logic;
    signal uartOutData: std_logic;
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

    procedure send_data
    (
        constant timebase: in integer;
        constant count: in integer range 1 to 255;
        
        signal clock: in std_logic;
        
        signal tx_complete: in std_logic;
        
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
        
        report "Sending data";
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
        
        wait for (timebase * count) * 100ns;
    end procedure;
begin
    
    -- [CLOCK RESET]
    -- Generates a 5 ns reset pulse, then a 10 ns period clock
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

    -- [TRANSCEIVER]
    -- This is the transceiver that we are testing
    TRANSCEIVER: TransceiverController generic map(
        BUFFER_SIZE => 16384,
        SEPARATE_BUFFERS => true
    ) port map(
        clock => clock,
        reset => reset,
        uartRx => uartInData,
        vpeRx => vpeSerial,
        sendToVpeEn => sendToVpeEn,
        sendToUartEn => sendToUartEn,
        vpeTx => vpeSerial,
        uartTx => uartOutData
    );
    
    -- [TRANSMITTER]
    -- This is the UART transmitter that will generate the UART signals for
    -- tests
    TRANSMITTER: UartTx port map(
        clock => clock,
        reset => reset,
        txEn => txEn,
        dataIn => dataIn,
        txComplete => txComplete,
        dataOut => uartInData
    );
    
    -- [RECEIVER]
    -- This is the UART receiver that will be used to log the UART sent by the
    -- transceiver
    RECEIVER: UartRx port map(
        clock => clock,
        reset => reset,
        rxData => uartOutData,
        dataReady => dataReady,
        dataOut => dataOut
    );
    -- [DRIVE TRANSCEIVER]
    -- This sends a UART signal to test the transceiver, tells it to send the
    -- value over VPE, then send the received VPE signal over UART again
    DRIVE_TRANSCEIVER: process
        constant MICROSECOND: integer := 1000;
    begin
        sendToVpeEn <= not ACTIVE;
        sendToUartEn <= not ACTIVE;
        -- Send a signal with 255 values and a timebase of 1us
        send_data(
            MICROSECOND, 
            255, 
            clock, 
            txComplete, 
            dataIn,
            sendToVpeEn, 
            txEn
        );
        wait for 1000 ns;
        wait until rising_edge(clock);
        sendToUartEn <= ACTIVE;
        wait until rising_edge(clock);
        sendToUartEn <= not ACTIVE;
        loop
            wait for 1sec;
        end loop;
    end process;

    -- [REPORT DATA]
    -- This reports the data being received over UART
    REPORT_DATA: process
    begin
        wait until rising_edge(clock);
        if (dataReady = ACTIVE) then
            report "WORD: " & get_word(dataOut);
        end if;
    end process;

end TestBench;
