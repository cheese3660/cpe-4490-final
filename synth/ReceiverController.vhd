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
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity ReceiverController is
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
end ReceiverController;

architecture Behavioral of ReceiverController is
    -- COMPONENTS
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
    
    -- TYPES
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
        SEND_FIRST_BYTE,
        BODY_SEND
    );
    
    -- CONSTANTS
    constant ACTIVE: std_logic := '0';
    constant DATA_SELECT_HEADER: std_logic := not ACTIVE;
    constant DATA_SELECT_READ: std_logic := ACTIVE;
    
    -- FUNCTIONS
    function nanosecondToClockRatio return integer is
    begin
        if CLOCK_FREQUENCY < 1_000_000_000 then
            return 1_000_000_000/CLOCK_FREQUENCY;
        else
            return CLOCK_FREQUENCY/1_000_000_000;
        end if;
    end function;
    
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
    signal vpeDataUnlatched: std_logic_vector(7 downto 0);
    signal newDataEn: std_logic;
    signal frameEndEn: std_logic;
    
    -- [DATA LATCH]
    signal vpeData: std_logic_vector(7 downto 0);

    -- [CONTROL]
    signal state: control_t := IDLE;
    
    -- [TIMEBASE REGISTER]
    signal shiftInTimebaseEn: std_logic;
    signal loadTimebaseLengthEn: std_logic;
    signal timebaseNs: integer;
    signal timebaseLoadedMode: std_logic;
    
    -- [COUNT REGISTER]
    signal shiftInCountEn: std_logic;
    signal loadCountLengthEn: std_logic;
    signal count: integer;
    signal countLoadedMode: std_logic;
    
    -- [ADDRESS INCREMENTER]
    signal addressClearEn: std_logic;
    signal addressIncrementEn: std_logic;
    signal addressTerminalMode: std_logic;
    
    -- [TIMER]
    signal timerStartEn: std_logic;
    signal timerEndEn: std_logic;
    signal timerCycles: unsigned(47 downto 0) := (others => '0');
    
    -- [HEADER GENERATOR]
    signal headerLoadEn: std_logic;
    signal headerNextEn: std_logic;
    signal headerCompleteMode: std_logic;
    signal headerData: std_logic_vector(7 downto 0);
    
    -- [READ LATCH]
    signal readEn: std_logic;
    signal readData: std_logic_vector(7 downto 0) := (others => '0');
    
    -- [UART TRANSMITTER]
    signal dataSelect: std_logic;
    signal txEn: std_logic;
    signal txData: std_logic_vector(7 downto 0);
    signal txComplete: std_logic;
    
begin
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
    
    DATA_LATCH: process(clock, reset) is
        variable latched: std_logic_vector(7 downto 0) := (others => '0');
    begin
        if (reset = ACTIVE) then
            latched := (others => '0');
        elsif (rising_edge(clock)) then
            if newDataEn = ACTIVE then
                latched := vpeDataUnlatched;
            end if;
        end if;
        
        if newDataEn = ACTIVE then
            vpeData <= vpeDataUnlatched;
        else
            vpeData <= latched;
        end if;
    end process;

    -- TIME BASE REGISTER
    TIMEBASE_REGISTER: process(clock, reset)
        variable timebase: unsigned(31 downto 0) := (others => '0');
        variable timebase_length: unsigned(7 downto 0) := (others => '0');
    begin
        if (reset = ACTIVE) then
            timebase := (others => '0');
            timebase_length := (others => '0');
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
    
    -- COUNT REGISTER
    COUNT_REGISTER: process(clock, reset)
        variable count_var: unsigned(31 downto 0) := (others => '0');
        variable count_length: unsigned(7 downto 0) := (others => '0');
    begin
        if (reset = ACTIVE) then
            count_var := (others => '0');
            count_length := (others => '0');
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
            
            
            sevenSegmentHex(11 downto 0) <= std_logic_vector(to_unsigned(count,12));
        end if;
    end process;
    
    -- TIMER
    TIMER: process(clock, reset)
    begin
        if (reset = ACTIVE) then
            timerCycles <= (others => '0');
        elsif (rising_edge(clock)) then
            timerCycles <= timerCycles+1;
        end if;
    end process;
    
    -- ADDRESS INCREMENTER
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
            --sevenSegmentHex(11 downto 0) <= std_logic_vector(to_unsigned(current_address,12));
        end if;
    end process;
    
    -- HEADER GENERATOR
    HEADER_GENERATOR: process(clock, reset)
        variable index: integer range 0 to 11 := 0;
        variable header: std_logic_vector((12*8)-1 downto 0);
    begin
        if reset = ACTIVE then
            index := 0;
        elsif rising_edge(clock) then
            headerCompleteMode <= not ACTIVE;
            if headerLoadEn = ACTIVE then
                index := 0;
                header(31 downto 0) := std_logic_vector(to_unsigned(count,32));
                header(39 downto 32) := X"04";
                header(87 downto 40) := std_logic_vector(clocksToNanoseconds(timerCycles)(47 downto 0));
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
    
    -- READ LATCH
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
    
    -- TX DATA SELECT
    with dataSelect select
        txData <= headerData when DATA_SELECT_HEADER,
                  readData when DATA_SELECT_READ,
                  headerData when others;

    -- UART TRANSMITTER
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

    -- STATE MACHINE
    CONTROL: process(clock, reset) is
    begin
        if reset = ACTIVE then
            state <= IDLE;
        else
            loadTimebaseLengthEn <= not ACTIVE;
            loadCountLengthEn <= not ACTIVE;
            shiftInTimebaseEn <= not ACTIVE;
            shiftInCountEn <= not ACTIVE;
            
            sevenSegmentHex(15 downto 12) <= X"F";
            
            case state is
                when IDLE =>
                    if newDataEn = ACTIVE then
                        loadTimebaseLengthEn <= ACTIVE;
                        state <= LOAD_TIMEBASE;
                    end if;
                    
                    sevenSegmentHex(15 downto 12) <= X"0";
                
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
               
                when LOAD_COUNT =>
                    if frameEndEn = ACTIVE then
                        if newDataEn = ACTIVE then
                            shiftInCountEn <= ACTIVE;
                        end if;
                        state <= SETUP_RX_A;
                    elsif newDataEn = ACTIVE then
                        shiftInCountEn <= ACTIVE;
                    end if;
                    
                    sevenSegmentHex(15 downto 12) <= X"2";
            
                when SETUP_RX_A =>
                    
                    sevenSegmentHex(15 downto 12) <= X"3";
                
                when others =>
                    sevenSegmentHex(15 downto 12) <= X"E";
            end case;
        end if;
    end process;

end Behavioral;
