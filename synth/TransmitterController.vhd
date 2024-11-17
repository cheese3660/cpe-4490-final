--------------------------------------------------------------------------------
-- Author: Lexi Allen
-- 
-- Create Date: 11/15/2024 10:25:08 AM
-- Design Name: VPE Serial Interface
-- Module Name: TransmitterController - Behavioral
-- Description: Implements the UART -> VPE half of the transceiver
--------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use IEEE.NUMERIC_STD.ALL;

entity TransmitterController is
    generic (
        -- The size of the buffer this controller is interfacing
        BUFFER_SIZE: integer := 200000;
        -- The clock frequency of the device this is on
        CLOCK_FREQUENCY: integer := 100_000_000
    );
    Port ( clock : in STD_LOGIC;
           reset : in STD_LOGIC;

           -- The UART data coming in
           uartRxLine : in STD_LOGIC;
           
           -- Pulse high to send the data over UART
           sendToVpeEn : in STD_LOGIC;
           
           -- The data coming in from the memory buffers
           data : in STD_LOGIC_VECTOR (7 downto 0);
           
           -- The address to read from/write to
           address : out integer range 0 to BUFFER_SIZE-1;
           
           -- The VPE data being sent out
           vpeTxLine : out STD_LOGIC;
           
           -- Trigger a write to memory
           writeEn : out STD_LOGIC;

           -- The data to write to memory
           writeData: out STD_LOGIC_VECTOR(7 downto 0);
           
           -- The state + address display
           sevenSegmentHex : out STD_LOGIC_VECTOR (15 downto 0)
           );
end TransmitterController;

architecture Procedural of TransmitterController is
    -- COMPONENTS

    -- A UART receiver to use to read data from
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
    
    -- A VPE transmitter to send the UART data out
    component VpeTransmitter is
    Generic (
        NIBBLE_COUNT: integer := 8
    );
    Port ( 
            -- The next word to be transmitted
            data : in STD_LOGIC_VECTOR ((NIBBLE_COUNT*4)-1 downto 0);
            -- Pulse when one t0 for the transmission
            -- has passed since the last pulse
            t0En : in STD_LOGIC;
            -- Keep this active while transmitting
            -- Set it to inactive to signal the end
            -- of a frame
            txMode : in STD_LOGIC;
            clock : in STD_LOGIC;
            reset : in STD_LOGIC;
            -- Output of the serial transmission
            vpeSerial : out STD_LOGIC;
            -- Signals the end of a word
            -- used to determine when txMode and
            -- data should be updated
            wordEndEn : out STD_LOGIC);
    end component;
    
    -- TYPES
    
    -- The state machine states
    type control_t is (
        IDLE,
        LOAD_TIMEBASE,
        LOAD_COUNT_DELAY,
        LOAD_COUNT,
        READ_DATA_DELAY,
        READ_DATA,
        INCREMENT_ADDRESS,
        DELAY_TX_START,
        TRANSMIT_HEADER,
        WAIT_HEADER_END_A,
        WAIT_HEADER_END_B,
        READ_FIRST_BYTE,
        TRANSMIT_BODY
    );
    
    -- CONSTANTS
    constant ACTIVE: std_logic := '1';

    -- The length of the header timebase in clock cycles (100 microseconds)
    constant HEADER_T0_CLOCKS: integer := CLOCK_FREQUENCY / 10000;

    -- Used in the dataSelect line to select the header data for being
    -- transmitted over VPE
    constant DATA_SELECT_HEADER: std_logic := not ACTIVE;

    -- Selects the latched read data instead for being transmitted over VPE
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
    
    -- Converts a number in nanoseconds to a number in clocks
    function nanosecondsToClocks(nanoseconds: integer) return integer is
        constant ratio: integer := nanosecondToClockRatio;
    begin
        if CLOCK_FREQUENCY < 1_000_000_000 then
            return nanoseconds / ratio;
        else
            return nanoseconds * ratio;
        end if;
    end function;
    
    -- SIGNALS
    
    -- [UART]

    -- Has new data come in on the receiver
    signal dataReady: std_logic;

    -- The data that has been received
    signal uartData: std_logic_vector(7 downto 0);
    
    -- [CONTROL]
    -- The current state
    signal state: control_t := IDLE;
    
    -- [TIMEBASE REGISTER]
    
    -- Shift in the next byte of the timebase from uartData
    signal shiftInTimebaseEn: std_logic;

    -- Load the length of the time base from uartData
    signal loadTimebaseLengthEn: std_logic;

    -- The timebase in nanoseconds
    signal timebaseNs: integer;
    
    -- Has the timebase been fully loaded?
    signal timebaseLoadedMode: std_logic;
    
    -- [COUNT REGISTER]

    -- Shift in the next byte of the data count from uartData
    signal shiftInCountEn: std_logic;

    -- Load the length of the data count from uartData
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
    
    -- [T0 GENERATOR]

    -- Load the timebase that is to be used for the data frame
    signal loadDataTimebaseEn: std_logic;

    -- Load the timebase that is to be used for the header frame
    signal loadHeaderTimebaseEn: std_logic;

    -- Pulses for each timebase
    signal t0En: std_logic;
    
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
    
    -- [TX]

    -- Select between header data/read data
    signal dataSelect: std_logic;

    -- The data to send over VPE
    signal txData: std_logic_vector(7 downto 0);

    -- Signals that we want to transmit
    signal txMode: std_logic;

    -- The current VPE line
    signal vpeSerial: std_logic;

    -- Has the last byte finished sending?
    signal wordEndEn: std_logic;
    
    -- [PULSE END DETECTOR]
    
    -- Has the VPE pulse fully ended?
    signal pulseEndedEn: std_logic;
    
begin
    -- [[ RECEIVING UART SIGNALS ]]

    -- [UART RECEIVER]
    -- Receives the incoming UART signal
    UART_RECEIVER: UartRx 
    generic map(
        CLOCK_FREQ => CLOCK_FREQUENCY
    ) port map(
        clock => clock,
        reset => reset,
        rxData => uartRxLine,
        dataReady => dataReady,
        dataOut => uartData
    );

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
                timebase_length := unsigned(uartData);
                timebase := (others => '0');
            elsif shiftInTimebaseEn = ACTIVE and timebase_length > 0 then
                timebase(31 downto 8) := timebase(23 downto 0);
                timebase(7 downto 0) := unsigned(uartData);
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
                count_length := unsigned(uartData);
                count_var := (others => '0');
            elsif shiftInCountEn = ACTIVE and count_length > 0 then
                count_var(31 downto 8) := count_var(23 downto 0);
                count_var(7 downto 0) := unsigned(uartData);
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

    -- MAP UART DATA TO WRITE
    writeData <= uartData;
    
    -- [[ GENERATING VPE DATA ]]

    -- [TIMEBASE GENERATOR]
    -- Generates pulses at the needed timebase for the current data frame
    --
    -- Loads the header timebase when loadHeaderTimeBaseEn becomes active
    --
    -- Otherwise loads the data timebase, converting from ns to clocks when
    -- loadDataTimeBaseEn becomes active
    T0_GENERATOR: process(clock, reset)
        variable currentCount: integer := 0;
        variable terminalCount: integer := HEADER_T0_CLOCKS-1;
    begin
        if (reset = ACTIVE) then
            currentCount := 0;
            terminalCount := HEADER_T0_CLOCKS-1;
        elsif (rising_edge(clock)) then
            t0En <= not ACTIVE;
            if loadDataTimebaseEn = ACTIVE then
                currentCount := 0;
                terminalCount := nanosecondsToClocks(timebaseNs)-1;
            elsif loadHeaderTimebaseEn = ACTIVE then
                currentCount := 0;
                terminalCount := HEADER_T0_CLOCKS-1;
            elsif currentCount = terminalCount then
                currentCount := 0;
                t0En <= ACTIVE;
            else
                currentCount := currentCount + 1;
            end if;
        end if;
    end process;
    
    -- [HEADER GENERATOR]
    -- Generates and sends out the VPE header frame
    --
    -- Loads the full header into a register when headerLoadEn is pulsed
    --
    -- Then sends out bytes individually from the header each time headerNextEn
    -- is pulsed
    --
    -- Signals headerCompleteMode when the last byte has been sent out
    HEADER_GENERATOR: process(clock, reset)
        variable index: integer range 0 to 9 := 0;
        variable header: std_logic_vector(79 downto 0);
    begin
        if (reset = ACTIVE) then
            index := 0;
        elsif (rising_edge(clock)) then
            headerCompleteMode <= not ACTIVE;
            if headerLoadEn = ACTIVE then
                index := 0;
                header(31 downto 0) := std_logic_vector(to_unsigned(count,32));
                header(39 downto 32) := X"04";
                header(71 downto 40) := std_logic_vector(to_unsigned(
                                                            timebaseNs,
                                                            32));
                header(79 downto 72) := X"04";
            elsif headerNextEn = ACTIVE and index < 9  then
                index := index + 1;
            end if;
            headerData <= header(((10 - index)*8)-1 downto (9 - index)*8);
            if index = 9 then
                headerCompleteMode <= active;
            end if;
        end if;
    end process;
    
    -- [[ TRANSMITTING VPE DATA ]]

    -- Selects between the header data and read data based on the dataSelect
    -- signal
    with dataSelect select
        txData <= headerData when DATA_SELECT_HEADER,
                  readData when DATA_SELECT_READ,
                  headerData when others;
    
    
    -- [TX]
    -- Used to transmit the VPE data
    TX: VpeTransmitter
        generic map(
            NIBBLE_COUNT => 2
        )
        port map(
            t0En => t0En,
            data => txData,
            txMode => txMode,
            clock => clock,
            reset => reset,
            vpeSerial => vpeSerial,
            wordEndEn => wordEndEn
        );
    
    -- MAP SERIAL TO TX
    vpeTxLine <= vpeSerial;
    
    -- [[ CONTROL ]]

    -- [PULSE END DETECTOR]
    -- Detects when a VPE pulse has ended, sending a pulse when that is the case
    PULSE_END_DETECTOR: process(clock, reset)
        variable lastSerial: std_logic := not ACTIVE;
    begin
        if (reset = ACTIVE) then
            lastSerial := '0';
            pulseEndedEn <= not ACTIVE;
        elsif (rising_edge(clock)) then
            pulseEndedEn <= not ACTIVE;
            if vpeSerial = not ACTIVE and lastSerial = ACTIVE then
                pulseEndedEn <= ACTIVE;
            end if;
            lastSerial := vpeSerial;
        end if;
    end process;

    -- [CONTROL]
    -- Controls this transmitter
    --
    -- Responds to new data on the UART line by parsing the header into
    -- registers and loading the data into memory
    --
    -- Responds to sendToVpeEn by sending a generated header and data over the
    -- VPE line
    CONTROL: process(clock, reset)
    begin
        if (reset = ACTIVE) then
            state <= IDLE;
        elsif (rising_edge(clock)) then
            shiftInTimebaseEn <= not ACTIVE;
            loadTimeBaseLengthEn <= not ACTIVE;
            shiftInCountEn <= not ACTIVE;
            loadCountLengthEn <= not ACTIVE;
            addressClearEn <= not ACTIVE;
            writeEn <= not ACTIVE;
            addressIncrementEn <= not ACTIVE;
            loadHeaderTimebaseEn <= not ACTIVE;
            headerLoadEn <= not ACTIVE;
            txMode <= not ACTIVE;
            headerNextEn <= not ACTIVE;
            loadDataTimebaseEn <= not ACTIVE;
            readEn <= not ACTIVE;
            dataSelect <= DATA_SELECT_HEADER;
            
            sevenSegmentHex(15 downto 12) <= X"F";
            
            case state is
                ----------------------------------------------------------------
                --                          IDLE                              --
                --                                                            --
                -- Waits for data to come in over the UART line or for a send --
                -- to VPE to be requested                                     --
                --                                                            --
                -- Begins parsing the header when data comes in over UART     --
                --                                                            --
                -- Starts setting up for a transmission when a send is        --
                -- signalled                                                  --
                ----------------------------------------------------------------
                when IDLE =>
                    if dataReady = ACTIVE then
                        loadTimeBaseLengthEn <= ACTIVE;
                        state <= LOAD_TIMEBASE;
                    elsif sendToVpeEn = ACTIVE then
                        addressClearEn <= ACTIVE;
                        loadHeaderTimebaseEn <= ACTIVE;
                        headerLoadEn <= ACTIVE;
                        state <= DELAY_TX_START;
                    end if;

                    sevenSegmentHex(15 downto 12) <= X"0";
                
                ----------------------------------------------------------------
                --                      LOAD TIMEBASE                         --
                --                                                            --
                -- Reads the timebase from the header, and once that is       --
                -- done begins parsing the data count                         --
                ----------------------------------------------------------------
                when LOAD_TIMEBASE =>
                    if dataReady = ACTIVE then
                        if timebaseLoadedMode = ACTIVE then
                            loadCountLengthEn <= ACTIVE;
                            state <= LOAD_COUNT_DELAY;
                        else
                            shiftInTimeBaseEn <= ACTIVE;
                        end if;
                    end if;

                    sevenSegmentHex(15 downto 12) <= X"1";
                
                ----------------------------------------------------------------
                --                      LOAD COUNT DELAY                      --
                --                                                            --
                -- Waits one cycle to begin loading the count from UART       --
                ----------------------------------------------------------------
                when LOAD_COUNT_DELAY =>
                    state <= LOAD_COUNT;

                    sevenSegmentHex(15 downto 12) <= X"2";
                
                ----------------------------------------------------------------
                --                      LOAD COUNT                            --
                --                                                            --
                -- Reads the data count from the header, and once that        --
                -- is done, starts setting up to read the data into memory    --
                ----------------------------------------------------------------
                when LOAD_COUNT =>
                    if countLoadedMode = ACTIVE then
                        addressClearEn <= ACTIVE;
                        state <= READ_DATA_DELAY;
                    elsif dataReady = ACTIVE then
                        shiftInCountEn <= ACTIVE;
                    end if;

                    sevenSegmentHex(15 downto 12) <= X"3";

                ----------------------------------------------------------------
                --                      READ DATA DELAY                       --
                --                                                            --
                -- Waits one cycle to begin reading the data from UART        --
                ----------------------------------------------------------------
                when READ_DATA_DELAY =>
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
                    if addressTerminalMode = ACTIVE then
                        state <= IDLE;
                    elsif dataReady = ACTIVE then
                        writeEn <= ACTIVE;
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
                -- frame                                                      --
                ----------------------------------------------------------------
                when DELAY_TX_START =>
                    state <= TRANSMIT_HEADER;

                    sevenSegmentHex(15 downto 12) <= X"7";
                    
                ----------------------------------------------------------------
                --                      TRANSMIT HEADER                       --
                --                                                            --
                -- Transmit the header frame over VPE, then wait for the VPE  --
                -- frame to stop                                              --
                ----------------------------------------------------------------
                when TRANSMIT_HEADER =>
                    txMode <= ACTIVE;
                    if wordEndEn = ACTIVE then
                        if headerCompleteMode = ACTIVE then
                            txMode <= not ACTIVE;
                            state <= WAIT_HEADER_END_A;
                        else
                            headerNextEn <= ACTIVE;
                        end if;
                    end if;

                    sevenSegmentHex(15 downto 12) <= X"8";
            
                ----------------------------------------------------------------
                --                     WAIT HEADER END A                      --
                --                                                            --
                -- Wait for the first pulse end from the VPE frame, then wait --
                -- for the second                                             --
                ----------------------------------------------------------------
                when WAIT_HEADER_END_A =>
                    if pulseEndedEn = ACTIVE then
                        state <= WAIT_HEADER_END_B;
                    end if;

                    sevenSegmentHex(15 downto 12) <= X"9";

                ----------------------------------------------------------------
                --                     WAIT HEADER END B                      --
                --                                                            --
                -- Wait for the second pulse end from the VPE frame, then     --
                -- setup the timebase for transmitting the body frame, and    --
                -- start doing that                                           --
                ----------------------------------------------------------------                
                when WAIT_HEADER_END_B =>
                    if pulseEndedEn = ACTIVE then
                        loadDataTimebaseEn <= ACTIVE;
                        state <= READ_FIRST_BYTE;
                    end if;

                    sevenSegmentHex(15 downto 12) <= X"A";
                
                ----------------------------------------------------------------
                --                      READ FIRST BYTE                       --
                --                                                            --
                -- Read the first byte of the data from memory, then begin    --
                -- transmission of the body frame over VPE                    --
                ----------------------------------------------------------------
                when READ_FIRST_BYTE =>
                    readEn <= ACTIVE;
                    addressIncrementEn <= ACTIVE;
                    state <= TRANSMIT_BODY;

                    sevenSegmentHex(15 downto 12) <= X"B";

                ----------------------------------------------------------------
                --                        TRANSMIT BODY                       --
                --                                                            --
                -- Sends the body frame over VPE then returns to idle         --
                ----------------------------------------------------------------
                when TRANSMIT_BODY =>
                    dataSelect <= DATA_SELECT_READ;
                    txMode <= ACTIVE;
                    if wordEndEn = ACTIVE then
                        if addressTerminalMode = ACTIVE then
                            txMode <= not ACTIVE;
                            state <= IDLE;
                        else
                            readEn <= ACTIVE;
                            addressIncrementEn <= ACTIVE;
                        end if;
                    end if;

                    sevenSegmentHex(15 downto 12) <= X"C";
            end case;
        end if;
    end process;

end Procedural;
