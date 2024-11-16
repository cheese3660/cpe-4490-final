----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 11/15/2024 10:25:08 AM
-- Design Name: 
-- Module Name: TransmitterController - Behavioral
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

entity TransmitterController is
    generic (
        BUFFER_SIZE: integer := 200000
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
           sevenSegmentHex : out STD_LOGIC_VECTOR (15 downto 0)
           );
end TransmitterController;

architecture Behavioral of TransmitterController is
    -- COMPONENTS
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
    
    type control_t is (
        IDLE,
        LOAD_TIMEBASE,
        LOAD_COUNT_DELAY,
        LOAD_COUNT,
        READ_DATA_DELAY,
        READ_DATA,
        INCREMENT_ADDRESS
    );
    
    -- CONSTANTS
    constant ACTIVE: std_logic := '1';
    
    -- SIGNALS
    
    -- [UART]
    signal dataReady: std_logic;
    signal uartData: std_logic_vector(7 downto 0);
    
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
    
begin
    UART_RECEIVER: UartRx port map(
        clock => clock,
        reset => reset,
        rxData => uartRxLine,
        dataReady => dataReady,
        dataOut => uartData
    );
    
    -- TIME BASE REGISTER
    TIMEBASE_REGISTER: process(clock, reset)
        variable timebase: unsigned(31 downto 0);
        variable timebase_length: unsigned(7 downto 0);
    begin
        if (reset = ACTIVE) then
            timebase := (others => '0');
            timebase_length := (others => '0');
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
            --sevenSegmentHex(11 downto 8) <= std_logic_vector(timebase_length(3 downto 0));
            --sevenSegmentHex(3 downto 2) <= '0' & timeBaseLoadedMode;
            --sevenSegmentHex(15) <= timebaseLoadedMode;
        end if;
    end process;
    
    -- COUNT REGISTER
    COUNT_REGISTER: process(clock, reset)
        variable count_var: unsigned(31 downto 0);
        variable count_length: unsigned(7 downto 0);
    begin
        if (reset = ACTIVE) then
            count_var := (others => '0');
            count_length := (others => '0');
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
            --sevenSegmentHex(7 downto 4) <= std_logic_vector(count_length(3 downto 0));
            --sevenSegmentHex(1 downto 0) <= '0' & countLoadedMode;
            --sevenSegmentHex(11 downto 0) <= std_logic_vector(count_var(11 downto 0));
            --sevenSegmentHex(6 downto 0) <= std_logic_vector(count_length(6 downto 0));
            --sevenSegmentHex(7) <= countLoadedMode;
        end if;
    end process;
    
    
    -- ADDRESS INCREMENTER
    ADDRESS_INCREMENTER: process(clock, reset)
        variable current_address: integer range 0 to BUFFER_SIZE-1;
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
    
    -- TO GENERATOR
    
    T0_GENERATOR: process(clock, reset)
        variable currentCount: integer;
        variable terminalCount: integer;
    begin
        if (reset = ACTIVE) then
            currentCount := 0;
            
        elsif (rising_edge(clock)) then
        end if;
    end process;
    
    -- CONTROL STATE MACHINE
    CONTROL: process(clock, reset)
    begin
        if (reset = ACTIVE) then
            state <= IDLE;
            shiftInTimebaseEn <= not ACTIVE;
            loadTimeBaseLengthEn <= not ACTIVE;
            shiftInCountEn <= not ACTIVE;
            loadCountLengthEn <= not ACTIVE;
            addressClearEn <= not ACTIVE;
            writeEn <= not ACTIVE;
            addressIncrementEn <= not ACTIVE;
        elsif (rising_edge(clock)) then
            shiftInTimebaseEn <= not ACTIVE;
            loadTimeBaseLengthEn <= not ACTIVE;
            shiftInCountEn <= not ACTIVE;
            loadCountLengthEn <= not ACTIVE;
            addressClearEn <= not ACTIVE;
            writeEn <= not ACTIVE;
            addressIncrementEn <= not ACTIVE;
            case state is
                when IDLE =>
                    if dataReady = ACTIVE then
                        loadTimeBaseLengthEn <= ACTIVE;
                        state <= LOAD_TIMEBASE;
                    end if;
                    sevenSegmentHex(15 downto 12) <= X"0";
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
                when LOAD_COUNT_DELAY =>
                    state <= LOAD_COUNT;
                    sevenSegmentHex(15 downto 12) <= X"2";
                when LOAD_COUNT =>
                    if countLoadedMode = ACTIVE then
                        addressClearEn <= ACTIVE;
                        state <= READ_DATA_DELAY;
                    elsif dataReady = ACTIVE then
                        shiftInCountEn <= ACTIVE;
                    end if;
                    sevenSegmentHex(15 downto 12) <= X"3";
                when READ_DATA_DELAY =>
                    state <= READ_DATA;
                    sevenSegmentHex(15 downto 12) <= X"4";
                when READ_DATA =>
                    if addressTerminalMode = ACTIVE then
                        state <= IDLE;
                    elsif dataReady = ACTIVE then
                        writeEn <= ACTIVE;
                        state <= INCREMENT_ADDRESS;
                    end if;
                    sevenSegmentHex(15 downto 12) <= X"5";
                when INCREMENT_ADDRESS =>
                    addressIncrementEn <= ACTIVE;
                    state <= READ_DATA;
                    sevenSegmentHex(15 downto 12) <= X"6";
            end case;
        end if;
    end process;

end Behavioral;
