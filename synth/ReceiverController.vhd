--------------------------------------------------------------------------------
-- Author: Lexi Allen
--
-- Create Date: 11/15/2024 10:25:08 AM
-- Design Name: VPE Serial Interface
-- Module Name: ReceiverController - Procedural
-- Description: Implements the VPE -> UART half of the transceiver
--------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use IEEE.NUMERIC_STD.ALL;

entity ReceiverController is
    generic (
        -- The size of the buffer this controller is interfacing
        BUFFER_SIZE: integer := 200000;
        -- The clock frequency of the device this is on
        CLOCK_FREQUENCY: integer := 100_000_000
    );
    Port ( clock : in STD_LOGIC;
           reset : in STD_LOGIC;

           -- The VPE data coming in
           vpeRxLine : in STD_LOGIC;

           -- Pulse high to send the data over UART
           sendToUartEn : in STD_LOGIC;

           -- The data coming in from the memory buffers
           data : in STD_LOGIC_VECTOR (7 downto 0);

           -- The address to read from/write to
           address : out integer range 0 to BUFFER_SIZE-1;

           -- The UART data being sent out
           uartTxLine : out STD_LOGIC;

           -- Trigger a write to memory
           writeEn : out STD_LOGIC;

           -- The state + address display
           sevenSegmentHex : out STD_LOGIC_VECTOR (15 downto 0);

           -- The data to write to memory
           writeData: out STD_LOGIC_VECTOR(7 downto 0));
end ReceiverController;

architecture Procedural of ReceiverController is
    -- COMPONENTS

    -- A VPE receiver to use to read data from
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
    
    -- A UART transmitter to send the UART data out
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
    
    -- TYPES

    -- The state machine states
    type control_t is (
        IDLE,
        LOAD_TIMEBASE,
        LOAD_COUNT,
        SETUP_RX_A,
        SETUP_RX_B,
        READ_DATA,
        INCREMENT_ADDRESS,
        DELAY_TX_START,
        HEADER_SEND,
        READ_FIRST_BYTE,
        BODY_SEND
    );
    
    -- CONSTANTS
    constant ACTIVE: std_logic := '1';

    -- Used in the dataSelect line to select the header data for being
    -- transmitted over UART
    constant DATA_SELECT_HEADER: std_logic := not ACTIVE;

    -- Selects the latched read data instead for being transmitted over UART
    constant DATA_SELECT_READ: std_logic := ACTIVE;
    
    -- FUNCTIONS

    -- Calculates the ratio between nanoseconds and clocks
    function nanosecondToClockRatio return integer is
    begin
        if CLOCK_FREQUENCY < 1_000_000_000 then
            return 1_000_000_000/CLOCK_FREQUENCY;
        else
            return CLOCK_FREQUENCY/1_000_000_000;
        end if;
    end function;
    
    -- Converts a number in clocks to a number in nanoseconds
    function clocksToNanoseconds(clocks: unsigned) return unsigned is
        constant ratio: integer := nanosecondToClockRatio;
    begin
        if CLOCK_FREQUENCY < 1_000_000_000 then
            return clocks * ratio;
        else
            return clocks / ratio;
        end if;
    end function;
    
    -- SIGNALS
    
    -- [RX]

    -- The raw data coming out of the VPE receiver
    signal vpeDataUnlatched: std_logic_vector(7 downto 0);

    -- Has new data come in on the receiver
    signal newDataEn: std_logic;

    -- Has the incoming frame ended?
    signal frameEndEn: std_logic;
    
    -- [DATA LATCH]

    -- The latched data from the VPE receiver
    signal vpeData: std_logic_vector(7 downto 0);

    -- [CONTROL]
    
    -- The current state
    signal state: control_t := IDLE;
    
    -- [TIMEBASE REGISTER]
    
    -- Shift in the next byte of the timebase from vpeData
    signal shiftInTimebaseEn: std_logic;

    -- Load the length of the time base from vpeData
    signal loadTimebaseLengthEn: std_logic;

    -- The timebase in nanoseconds
    signal timebaseNs: integer;
    
    -- Has the timebase been fully loaded?
    signal timebaseLoadedMode: std_logic;
    
    -- [COUNT REGISTER]

    -- Shift in the next byte of the data count from vpeData
    signal shiftInCountEn: std_logic;

    -- Load the length of the data count from vpeData
    signal loadCountLengthEn: std_logic;

    -- The data count in bytes
    signal count: integer;
    
    -- Has the data count been fully loaded?
    signal countLoadedMode: std_logic;
    
    -- [ADDRESS INCREMENTER]

    -- Reset the address incrementer
    signal addressClearEn: std_logic;

    -- Increment the current address by one
    signal addressIncrementEn: std_logic;

    -- Has the address reached the data count or end of buffer?
    signal addressTerminalMode: std_logic;
    
    -- [TIMER]

    -- Start the receive timer
    signal timerStartEn: std_logic;

    -- Stop the receive timer
    signal timerEndEn: std_logic;

    -- The length of time it took to receive the transmission in cycles
    -- 48 bits should be enough for up to 3 days
    signal timerCycles: unsigned(47 downto 0) := (others => '0');
    
    -- [HEADER GENERATOR]

    -- Load the header data into a register
    signal headerLoadEn: std_logic;

    -- Read the next byte from the header
    signal headerNextEn: std_logic;

    -- Has the header been fully read?
    signal headerCompleteMode: std_logic;

    -- The current byte of the header
    signal headerData: std_logic_vector(7 downto 0);
    
    -- [READ LATCH]

    -- Read a byte from memory into the latch
    signal readEn: std_logic;

    -- The latched data
    signal readData: std_logic_vector(7 downto 0) := (others => '0');
    
    -- [UART TRANSMITTER]

    -- Select between header data/read data
    signal dataSelect: std_logic;

    -- Send a byte over UART
    signal txEn: std_logic;
    
    -- The data to send over UART
    signal txData: std_logic_vector(7 downto 0);

    -- Has the last byte finished sending?
    signal txComplete: std_logic;
    
begin
    -- [[ RECEIVING VPE SIGNALS ]]

    -- [RX]
    -- Receives the incoming VPE signal
    RX: VpeRx generic map (
        DATA_WIDTH => 8
    ) port map (
        clock => clock,
        reset => reset,
        vpeSerial => vpeRxLine,
        data => vpeDataUnlatched,
        newDataEn => newDataEn,
        endFrameEn => frameEndEn
    );
    
    -- [DATA LATCH]
    -- Latches the last VPE signal into a register on newDataEn
    DATA_LATCH: process(clock, reset, vpeData) is
        variable latched: std_logic_vector(7 downto 0) := (others => '0');
    begin
        if (reset = ACTIVE) then
            latched := (others => '0');
        elsif (rising_edge(clock)) then
            if newDataEn = ACTIVE then
                latched := vpeDataUnlatched;
            end if;
            vpeData <= latched;
        end if;
    end process;

    -- [[ PARSING TRANSMISSION HEADER ]]

    -- [TIMEBASE REGISTER]
    -- Reads the timebase portion of the header, starting with the length, then
    -- that many bytes
    --
    -- Signals timebaseLoadedMode when fully loaded
    TIMEBASE_REGISTER: process(clock, reset)
        variable timebase: unsigned(31 downto 0) := (others => '0');
        variable timebase_length: unsigned(7 downto 0) := (others => '0');
    begin
        if (reset = ACTIVE) then
            timebase_length := (others => '0');
            timebase := (others => '0');
        elsif (rising_edge(clock)) then
            if loadTimebaseLengthEn = ACTIVE then
                timebase_length := unsigned(vpeData);
                timebase := (others => '0');
            elsif shiftInTimebaseEn = ACTIVE and timebase_length > 0 then
                timebase(31 downto 8) := timebase(23 downto 0);
                timebase(7 downto 0) := unsigned(vpeData);
                timebase_length := timebase_length - 1;
            end if;
            if timebase_length = 0 then
                timebaseLoadedMode <= ACTIVE;
            else
                timebaseLoadedMode <= not ACTIVE;
            end if;
            timebaseNs <= to_integer(timebase);
        end if;
    end process;
    

    -- [COUNT REGISTER]
    -- Reads the data count portion of the header, starting with the length,
    -- then that many bytes
    --
    -- Signals countLoadedMode when fully loaded
    COUNT_REGISTER: process(clock, reset)
        variable count_var: unsigned(31 downto 0) := (others => '0');
        variable count_length: unsigned(7 downto 0) := (others => '0');
    begin
        if (reset = ACTIVE) then
            count_length := (others => '0');
            count_var := (others => '0');
        elsif (rising_edge(clock)) then
            if loadCountLengthEn = ACTIVE then
                count_length := unsigned(vpeData);
                count_var := (others => '0');
            elsif shiftInCountEn = ACTIVE and count_length > 0 then
                count_var(31 downto 8) := count_var(23 downto 0);
                count_var(7 downto 0) := unsigned(vpeData);
                count_length := count_length - 1;
            end if;
            if count_length = 0 then
                countLoadedMode <= ACTIVE;
            else
                countLoadedMode <= not ACTIVE;
            end if;
            count <= to_integer(count_var);
        end if;
    end process;
    
    -- [[ TIMING TRANSMISSION ]]

    -- [TIMER]
    -- Starts counting clock cycles when timerStartEn is pulsed and stops
    -- counting them when timerEndEn is pulsed
    TIMER: process(clock, reset)
        variable timerEnabled: boolean := false;
    begin
        if (reset = ACTIVE) then
            timerEnabled := false;
        elsif (rising_edge(clock)) then
            if (timerStartEn = ACTIVE) then
                timerEnabled := true;
                timerCycles <= (others => '0');
            elsif (timerEndEn = ACTIVE) then
                timerEnabled := false;
            elsif (timerEnabled) then
                timerCycles <= timerCycles+1;
            end if;
        end if;
    end process;
    
    -- [[ READING/WRITING DATA ]]

    -- [ADDRESS INCREMENT]
    -- Holds the current read/write address, is set to 0 when addressClearEn is
    -- is pulsed, and increments up to the data count or buffer size when
    -- addressIncrementEn is pulsed
    --
    -- Signals addressTerminalMode when the data count or buffer size has been
    -- reached by the current address
    ADDRESS_INCREMENTER: process(clock, reset)
        variable current_address: integer range 0 to BUFFER_SIZE-1 := 0;
    begin
        if (reset = ACTIVE) then
            current_address := 0;
            addressTerminalMode <= not ACTIVE;
        elsif (rising_edge(clock)) then
            if (addressClearEn = ACTIVE) then
                current_address := 0;
                addressTerminalMode <= not ACTIVE;
            end if;
            
            if (addressIncrementEn = ACTIVE) then
                if current_address < BUFFER_SIZE-1 then 
                    current_address := current_address+1;
                    if current_address = count then
                        addressTerminalMode <= ACTIVE;
                    end if;
                else
                    addressTerminalMode <= ACTIVE;
                end if;
            end if;
            
            address <= current_address;
            sevenSegmentHex(11 downto 0) <= std_logic_vector(to_unsigned(current_address,12));
        end if;
    end process;
    
    -- [READ LATCH]
    -- Reads a byte of data from memory when readEn has been selected
    READ_LATCH: process(clock, reset)
    begin
        if (reset = ACTIVE) then
            readData <= (others => '0');
        elsif (rising_edge(clock)) then
            if readEn = ACTIVE then
                readData <= data;
            end if;
        end if;
    end process;

    -- MAP VPE DATA TO MEMORY
    writeData <= vpeData;

    -- [[ GENERATING UART DATA ]]

    -- [HEADER GENERATOR]
    -- Generates and sends out the UART data header
    --
    -- Loads the full header into a register when headerLoadEn is pulsed,
    -- converting the timer from clock cycles to nanoseconds at that time
    --
    -- Then sends out bytes individually from the header each time headerNextEn
    -- is pulsed
    --
    -- Signals headerCompleteMode when the last byte has been sent out
    HEADER_GENERATOR: process(clock, reset)
        variable index: integer range 0 to 11 := 0;
        variable header: std_logic_vector((12*8)-1 downto 0);
    begin
        if (reset = ACTIVE) then
            index := 0;
        elsif rising_edge(clock) then
            headerCompleteMode <= not ACTIVE;
            if headerLoadEn = ACTIVE then
                index := 0;
                header(31 downto 0) := std_logic_vector(to_unsigned(count,32));
                header(39 downto 32) := X"04";
                header(87 downto 40) := std_logic_vector(
                                            clocksToNanoseconds(timerCycles)
                                                (47 downto 0)
                                        );
                header(95 downto 88) := X"06";
            elsif headerNextEn = ACTIVE and index < 11 then
                index := index + 1;
            end if;
            headerData <= header(((12 - index)*8)-1 downto (11 - index)*8);
            if index = 11 then
                headerCompleteMode <= active;
            end if;
        end if;
    end process;

    -- [[ TRANSMITTING UART DATA ]]
    
    -- Selects between the header data and read data based on the dataSelect
    -- signal
    with dataSelect select
        txData <= headerData when DATA_SELECT_HEADER,
                  readData when DATA_SELECT_READ,
                  headerData when others;

    -- [UART TRANSMITTER]
    -- Used to transmit the UART data
    UART_TRANSMITTER: UartTx generic map (
        CLOCK_FREQ => CLOCK_FREQUENCY
    ) port map (
        clock => clock,
        reset => reset,
        txEn => txEn,
        dataIn => txData,
        dataOut => uartTxLine,
        txComplete => txComplete
    );

    -- [[ CONTROL ]]

    -- [CONTROL]
    -- Controls this receiver
    --
    -- Responds to new data on the VPE line by parsing the header into registers
    -- and loading the data into memory, timing how long it takes
    --
    -- Responds to sendToUartEn by sending a generated header and data over the
    -- UART line
    CONTROL: process(clock, reset) is
        -- Did we just send data over UART, used to stop double transmissions
        variable justSent: boolean := false;
    begin
        if reset = ACTIVE then
            state <= IDLE;
            justSent := false;
        elsif rising_edge(clock) then
            loadTimebaseLengthEn <= not ACTIVE;
            loadCountLengthEn <= not ACTIVE;
            shiftInTimebaseEn <= not ACTIVE;
            shiftInCountEn <= not ACTIVE;
            timerStartEn <= not ACTIVE;
            addressClearEn <= not ACTIVE;
            writeEn <= not ACTIVE;
            timerEndEn <= not ACTIVE;
            addressIncrementEn <= not ACTIVE;
            headerLoadEn <= not ACTIVE;
            headerNextEn <= not ACTIVE;
            txEn <= not ACTIVE;
            readEn <= not ACTIVE;
            dataSelect <= DATA_SELECT_HEADER;
            
            sevenSegmentHex(15 downto 12) <= X"F";
            
            case state is
                
                ----------------------------------------------------------------
                --                          IDLE                              --
                --                                                            --
                -- Waits for data to come in over the VPE line or for a send  --
                -- to UART to be requested                                    --
                --                                                            --
                -- Begins parsing the header frame when data comes over VPE   --
                --                                                            --
                -- Starts setting up for a transmission when a send is        --
                -- signalled                                                  --
                ----------------------------------------------------------------
                when IDLE =>
                    if newDataEn = ACTIVE then
                        loadTimebaseLengthEn <= ACTIVE;
                        state <= LOAD_TIMEBASE;
                    elsif sendToUartEn = ACTIVE then
                        headerLoadEn <= ACTIVE;
                        addressClearEn <= ACTIVE;
                        state <= DELAY_TX_START;
                    end if;
                    
                    sevenSegmentHex(15 downto 12) <= X"0";
                
                ----------------------------------------------------------------
                --                      LOAD TIMEBASE                         --
                --                                                            --
                -- Reads the timebase from the header frame, and once that is --
                -- done begins parsing the data count                         --
                ----------------------------------------------------------------
                when LOAD_TIMEBASE =>
                    if newDataEn = ACTIVE then
                        if timebaseLoadedMode = ACTIVE then
                            loadCountLengthEn <= ACTIVE;
                            state <= LOAD_COUNT;
                        else
                            shiftInTimebaseEn <= ACTIVE;
                        end if;
                    end if;
                    
                    sevenSegmentHex(15 downto 12) <= X"1";
               
                ----------------------------------------------------------------
                --                      LOAD COUNT                            --
                --                                                            --
                -- Reads the data count from the header frame, and once that  --
                -- is done, starts setting up to read the data into memory    --
                ----------------------------------------------------------------
                when LOAD_COUNT =>
                    if newDataEn = ACTIVE then
                        shiftInCountEn <= ACTIVE;
                    end if;
                    if frameEndEn = ACTIVE then
                        state <= SETUP_RX_A;
                    end if;
                    
                    sevenSegmentHex(15 downto 12) <= X"2";
            
                ----------------------------------------------------------------
                --                      SETUP RECEIVE A                       --
                --                                                            --
                -- Starts the receive timer, then continues setting up to     --
                -- read                                                       --
                ----------------------------------------------------------------
                when SETUP_RX_A =>
                    timerStartEn <= ACTIVE;
                    state <= SETUP_RX_B;
                    
                    sevenSegmentHex(15 downto 12) <= X"3";
                
                ----------------------------------------------------------------
                --                      SETUP RECEIVE B                       --
                --                                                            --
                -- Clears the memory address, then starts reading data into   --
                -- memory                                                     --
                ----------------------------------------------------------------
                when SETUP_RX_B =>
                    addressClearEn <= ACTIVE;
                    state <= READ_DATA;
                    
                    sevenSegmentHex(15 downto 12) <= X"4";
                
                ----------------------------------------------------------------
                --                          READ DATA                         --
                --                                                            --
                -- Reads bytes into memory, returning to idle when done       --
                --                                                            --
                -- Increments the address after each byte                     --
                ----------------------------------------------------------------
                when READ_DATA =>
                    if newDataEn = ACTIVE then
                        writeEn <= ACTIVE;
                    end if;
                    if frameEndEn = ACTIVE then
                        timerEndEn <= ACTIVE;
                        state <= IDLE;
                    elsif newDataEn = ACTIVE then
                        state <= INCREMENT_ADDRESS;
                    end if;
                    
                    sevenSegmentHex(15 downto 12) <= X"5";
                
                ----------------------------------------------------------------
                --                      INCREMENT ADDRESS                     --
                --                                                            --
                -- Increments the address for reading data into, then         --
                -- continues reading data                                     --
                ----------------------------------------------------------------
                when INCREMENT_ADDRESS =>
                    addressIncrementEn <= ACTIVE;
                    state <= READ_DATA;
                    
                    sevenSegmentHex(15 downto 12) <= X"6";
                
                ----------------------------------------------------------------
                --                  DELAY TRANSMISSION START                  --
                --                                                            --
                -- Wait one clock cycle to begin transmitting the header      --
                ----------------------------------------------------------------
                when DELAY_TX_START =>
                    state <= HEADER_SEND;
                    
                    sevenSegmentHex(15 downto 12) <= X"7";
                
                ----------------------------------------------------------------
                --                        SEND HEADER                         --
                --                                                            --
                -- Transmit the header over UART, then start transmitting the --
                -- data                                                       --
                ----------------------------------------------------------------
                when HEADER_SEND =>
                    if txComplete = ACTIVE and not justSent then
                        if headerCompleteMode = ACTIVE then
                            state <= READ_FIRST_BYTE;
                        else
                            justSent := true;
                            headerNextEn <= ACTIVE;
                        end if;
                        txEn <= ACTIVE;
                    else
                        justSent := false;
                    end if;
                    
                    sevenSegmentHex(15 downto 12) <= X"8";
            
                ----------------------------------------------------------------
                --                      READ FIRST BYTE                       --
                --                                                            --
                -- Read the first byte of the data from memory, then begin    --
                -- transmission of data over UART                             --
                ----------------------------------------------------------------
                when READ_FIRST_BYTE =>
                    readEn <= ACTIVE;
                    addressIncrementEn <= ACTIVE;
                    dataSelect <= DATA_SELECT_READ;
                    state <= BODY_SEND;
                    justSent := false;
                    
                    sevenSegmentHex(15 downto 12) <= X"9";
                
                ----------------------------------------------------------------
                --                         SEND BODY                          --
                --                                                            --
                -- Sends the data over UART then returns to idle              --
                ----------------------------------------------------------------
                when BODY_SEND =>
                    dataSelect <= DATA_SELECT_READ;
                    if txComplete = ACTIVE and not justSent then
                        if addressTerminalMode = ACTIVE then
                            justSent := false;
                            state <= IDLE;
                        else
                            justSent := true;
                            readEn <= ACTIVE;
                            addressIncrementEn <= ACTIVE;
                        end if;
                        txEn <= ACTIVE;
                    else
                        justSent := false;
                    end if;
                    
                    sevenSegmentHex(15 downto 12) <= X"A";
                    
            end case;
        end if;
    end process;

end Procedural;
