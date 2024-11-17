--------------------------------------------------------------------------------
-- Author: Lexi Allen 
-- 
-- Create Date: 11/16/2024
-- Design Name: VPE Serial Interface
-- Module Name: Basys3Wrapper - Wrapper
-- Description: Implements the UART <-> VPE transceiver for the basys 3
--
-- Receives UART from the host computer via serial
-- Receives VPE over JB1
--
-- Sends UART to the host computer via serial when data is received and up is
-- pressed 
--
-- Sends VPE over JA1 when data has been uploaded and the left button is pressed
--
-- Switches display between the state + low 12 bits of the address for the
-- transmitter and receiver on the down button
--
-- Resets on the center
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Basys3Wrapper is
    port (
        clk: in STD_LOGIC;
        btnC: in STD_LOGIC; -- reset
        btnU: in STD_LOGIC; -- send to UART
        btnL: in STD_LOGIC; -- send to VPE
        btnD: in STD_LOGIC; -- switch which display is being shown
        RsRx: in STD_LOGIC; -- UART received
        JB_0: in STD_LOGIC; -- VPE received
        seg: out STD_LOGIC_VECTOR(6 downto 0);
        an: out STD_LOGIC_VECTOR(3 downto 0);
        led: out STD_LOGIC_VECTOR(15 downto 0);
        RsTx: out STD_LOGIC; -- UART sent
        JA_0: out STD_LOGIC  -- VPE sent
    );
end Basys3Wrapper;


architecture Wrapper of Basys3Wrapper is
    -- COMPONENTS

    -- The UART <-> VPE transceiver that this wraps
    component TransceiverController is
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
    end component;

    -- A seven segment display driver for driving the seven segs
    component SevenSegmentDriver is
        port(
            reset: in std_logic;
            clock: in std_logic;
    
            digit3: in std_logic_vector(3 downto 0);    --leftmost digit
            digit2: in std_logic_vector(3 downto 0);    --2nd from left digit
            digit1: in std_logic_vector(3 downto 0);    --3rd from left digit
            digit0: in std_logic_vector(3 downto 0);    --rightmost digit
    
            blank3: in std_logic;    --leftmost digit
            blank2: in std_logic;    --2nd from left digit
            blank1: in std_logic;    --3rd from left digit
            blank0: in std_logic;    --rightmost digit
    
            sevenSegs: out std_logic_vector(6 downto 0);    --MSB=g, LSB=a
            anodes:    out std_logic_vector(3 downto 0)    --MSB=leftmost digit
        );
    end component;

    -- A debouncer to debounce button inputs
    component Debouncer is
        Generic (
            QUEUE_LENGTH: integer := 16;
            QUEUE_FILL_AMOUNT: integer := 12
        );
        Port ( clock : in STD_LOGIC;
               reset : in STD_LOGIC;
               sampleEn : in STD_LOGIC;
               noisySignal : in STD_LOGIC;
               debouncedSignal : out STD_LOGIC);
    end component;

    -- A level detector to detect the levels on button inputs
    component Leveldetector is
        port (
            reset:     in  std_logic;
            clock:     in  std_logic;
            trigger:   in  std_logic;
            pulseOut:  out std_logic
        );
    end component;
    
    -- CONSTANTS
    constant ACTIVE: std_logic := '1';
    constant MICROSECOND: integer := 100;
    constant MILLISECOND: integer := 1000 * MICROSECOND;
    
    -- SIGNALS

    -- [CLOCK/RESET]
    signal clock: std_logic;
    signal reset: std_logic;
    
    -- [SEVEN SEGMENT DATA]
    signal sevenSegmentHexTx: std_logic_vector(15 downto 0);
    signal sevenSegmentHexRx: std_logic_vector(15 downto 0);
    signal sevenSegmentHex: std_logic_vector(15 downto 0);
    signal currentDisplay: std_logic := '0';
    
    -- [DEBOUNCE]
    signal debounceSampleEn: std_logic;
    
    -- [SEND TO VPE]
    signal sendToVpeMode: std_logic;
    signal sendToVpeEn: std_logic;
    
    -- [SEND TO UART]
    signal sendToUartMode: std_logic;
    signal sendToUartEn: std_logic;
    
    -- [SWITCH DISPLAYS]
    signal switchDisplayMode: std_logic;
    signal switchDisplayEn: std_logic;
    
begin
    -- rename clock/reset
    clock <= clk;
    reset <= btnC;

    -- [GENERATE DEBOUNCE SAMPLE]
    -- Generates a signal to drive the debouncers at a period of 1 millisecond
    -- per sample
    GENERATE_DEBOUNCE_SAMPLE: process(clock,reset) is
        constant PERIOD: integer := MILLISECOND;
        variable count: integer range 0 to PERIOD-1 := 0;
    begin
        if (reset = ACTIVE) then
            -- Ignore resets
        elsif (rising_edge(clock)) then
            debounceSampleEn <= not ACTIVE;
            if count < PERIOD-1 then
                count := count + 1;
            else
                count := 0;
                debounceSampleEn <= ACTIVE;
            end if;
        end if;
    end process;

    -- [[ SEND TO VPE ]]

    -- [DEBOUNCE SEND TO VPE]
    -- Debounces the left button
    DEBOUNCE_SEND_TO_VPE: Debouncer generic map(
        QUEUE_LENGTH => 16,
        QUEUE_FILL_AMOUNT => 12
    ) port map (
        clock => clock,
        reset => reset,
        sampleEn => debounceSampleEn,
        noisySignal => btnL,
        debouncedSignal => sendToVpeMode
    );
    
    -- [LEVEL DETECT SEND TO VPE]
    -- Level detects the left button to use it to signal a send to VPE
    LEVEL_DETECT_SEND_TO_VPE: LevelDetector port map(
        clock => clock,
        reset => reset,
        trigger => sendToVpeMode,
        pulseOut => sendToVpeEn
    );

    -- [[ SEND TO UART ]]
    
    -- [DEBOUNCE SEND TO UART]
    -- Debounces the up button
    DEBOUNCE_SEND_TO_UART: Debouncer generic map(
        QUEUE_LENGTH => 16,
        QUEUE_FILL_AMOUNT => 12
    ) port map (
        clock => clock,
        reset => reset,
        sampleEn => debounceSampleEn,
        noisySignal => btnU,
        debouncedSignal => sendToUartMode
    );
    
    -- [LEVEL DETECT SEND TO UART]
    -- Level detects the up button to use it to signal a send to UART
    LEVEL_DETECT_SEND_TO_UART: LevelDetector port map(
        clock => clock,
        reset => reset,
        trigger => sendToUartMode,
        pulseOut => sendToUartEn
    );
    
    -- [[ DISPLAYS ]]
    
    -- [DEBOUNCE SWITCH DISPLAY]
    -- Debounces the down button
    DEBOUNCE_SWITCH_DISPLAY: Debouncer generic map(
        QUEUE_LENGTH => 16,
        QUEUE_FILL_AMOUNT => 12
    ) port map (
        clock => clock,
        reset => reset,
        sampleEn => debounceSampleEn,
        noisySignal => btnD,
        debouncedSignal => switchDisplayMode
    );

    -- [LEVEL DETECT SWITCH DISPLAY]
    -- Level detects the down button to use it to signal a display switch    
    LEVEL_DETECT_SWITCH_DISPLAY: LevelDetector port map(
        clock => clock,
        reset => reset,
        trigger => switchDisplayMode,
        pulseOut => switchDisplayEn
    );
    
    -- [SWITCH DISPLAY]
    -- Switches between the TX display and the RX display on the down button
    SWITCH_DISPLAY: process(clock, reset) is
    begin
        if reset = ACTIVE then
            currentDisplay <= '0';
        elsif rising_edge(clock) then
            if switchDisplayEn = ACTIVE then
                currentDisplay <= not currentDisplay;
            end if;
        end if;
    end process;

    -- Map the current display signal
    with currentDisplay select
        sevenSegmentHex <= sevenSegmentHexRx when '1',
                           sevenSegmentHexTx when others;
    -- And put the leds on the other one
    with currentDisplay select
        led <= sevenSegmentHexTx when '1',
               sevenSegmentHexRx when others;
    
    -- [DISPLAY]
    -- Shows the selected display on the seven segment LCDs
    DISPLAY: SevenSegmentDriver port map(
        clock => clock,
        reset => reset,
        digit3 => sevenSegmentHex(15 downto 12),
        digit2 => sevenSegmentHex(11 downto 8),
        digit1 => sevenSegmentHex(7 downto 4),
        digit0 => sevenSegmentHex(3 downto 0),
        blank3 => '0',
        blank2 => '0',
        blank1 => '0',
        blank0 => '0',
        sevenSegs => seg,
        anodes => an
    );

    -- [RX/TX]
    -- The transceiver that we are wrapping
    RX_TX: TransceiverController generic map (
        -- 180KB is around the maximum buffer size available on the basys 3
        BUFFER_SIZE => 180_000,
        -- Combine the buffers because of the size
        SEPARATE_BUFFERS => false
    ) port map (
        clock => clock,
        reset => reset,
        uartRx => RsRx,
        vpeRx => JB_0,
        sendToVpeEn => sendToVpeEn,
        sendToUartEn => sendToUartEn,
        vpeTx => JA_0,
        uartTx => RsTx,
        sevenSegmentHexTx => sevenSegmentHexTx,
        sevenSegmentHexRx => sevenSegmentHexRx
    );

end Wrapper;