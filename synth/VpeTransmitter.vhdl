------------------------------------------------------------------------
-- Author: Lexi Allen
-- Create Date: 09/25/2024 07:19:47 AM
-- Design Name: Vpe Serial Transmitter
-- Module Name: VpeTransmitter - Procedural
-- Description: Implements a transmitter for the VPE serial protocol
-- To perform a transmission:
-- * Set the data input to the first word to transmit
-- * Set txMode Active
-- * Wait until wordEndEn gets set active
--   (This signals the end of the word)
--   * To end the transmission, set txMode not active
--   * To continue the transmission, keep txMode active
--     and set data to the next word to transmit
--     then repeat the third step
-- To control the frequency of the transmission
-- * Send active pulses to t0En
--   at the frequency of the transmission
-- * Note: Do not change the frequency mid-frame
------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;


entity VpeTransmitter is
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
end VpeTransmitter;

architecture Procedural of VpeTransmitter is
    
    -------------------------
    -- Types               --
    -------------------------
    
    -- State machine states
    type TX_CONTROL_T is (
        IDLE, 
        WAIT_FOR_PULSE, 
        CHECK_SHOULD_CONTINUE, 
        TX_WORD_START
    );
    
    -- Type of the nibble encoding array
    type NIBBLES_ARRAY_T is
        array (0 to 15) of 
        std_logic_vector(6 downto 0);

    -------------------------
    -- General Constants   --
    -------------------------
    
    -- Circuit is active high
    constant ACTIVE: std_logic := '1';
    
    -------------------------
    -- Vpe Constants       --
    -------------------------
    
    -- All Vpe constants are encoded in a 7 bit string
    -- representing the target waveform, with the first
    -- t0 being the highest bit and continuing sequentally from there
    -- with the pulse ending once all following bits are 0
    
    -- Vpe word start encoding, 1 low, 1 high
    constant WORD_START: std_logic_vector(6 downto 0) := "0100000";
    -- Vpe frame start encoding, 6 low, 1 high
    constant FRAME_START: std_logic_vector(6 downto 0) := "0000001";
    
    -- All nibble encodings in a array starting sequentially from 0
    constant NIBBLES: NIBBLES_ARRAY_T := (
        -- "0000"
        "0110000",
        -- "0001"
        "0010000",
        -- "0010"
        "0111000",
        -- "0011"
        "0011000",
        -- "0100"
        "0001000",
        -- "0101"
        "0111100",
        -- "0110"
        "0011100",
        -- "0111"
        "0001100",
        -- "1000"
        "0000100",
        -- "1001"
        "0111110",
        -- "1010"
        "0011110",
        -- "1011"
        "0001110",
        -- "1100"
        "0000110",
        -- "1101"
        "0000010",
        -- "1110"
        "0111111",
        -- "1111"
        "0011111"
    );
    
    -------------------------
    -- Control             --
    -------------------------
    
    -- Used to load the data shift register
    signal dataLoadEn: std_logic;
    
    -- Used to shift the data shift register
    signal dataShiftEn: std_logic;
    
    -- Makes the pulse encoder output a frame start
    signal frameStart: std_logic;
    
    -- Makes the pulse encoder output a word start
    signal wordStart: std_logic;
    
    -- Load the next pulse into the pulse transmitter
    signal pulseLoadEn: std_logic;
    
    -------------------------
    -- Circuit State       --
    -------------------------
    
    -- Is the data shift register on its last nibble?
    signal lastNibbleMode: std_logic;
    -- Has the last pulse being transmitted completed?
    signal pulseCompleteMode: std_logic;
    
    -------------------------
    -- State Machine       --
    -------------------------
    
    -- State of the state machine
    signal txState: TX_CONTROL_T := IDLE;
    
    -------------------------
    -- Data                --
    -------------------------
    
    -- The current nibble of the data shift register
    signal currentNibble: std_logic_vector (3 downto 0);
    
    -- The wave form encoding of the current nibble
    -- or frame/word start
    signal currentWaveForm: std_logic_vector (6 downto 0);
begin
    --------------------------------------------------------------------
    --                       DATA SHIFT REGISTER                      --
    -- Stores the current word to transmit and shifts it out nibble   --
    -- by nibble, ignoring the leading zeroes                         --
    --                                                                --
    -- Also signals when it is done shifting out the word             --
    --------------------------------------------------------------------
    DATA_SHIFT: process(clock,reset)
        -- The current word being transmitted
        variable storedWord: std_logic_vector((NIBBLE_COUNT*4)-1 downto 0);
        
        -- The current index of the nibble being output
        variable nibbleIndex: integer range 0 to NIBBLE_COUNT-1;
    begin
        if (reset = ACTIVE) then
            nibbleIndex := 0;
            storedWord := (others => '0');
            currentNibble <= (others => '0');
            lastNibbleMode <= not ACTIVE;
        elsif (rising_edge(clock)) then
            if (dataLoadEn = ACTIVE) then
                storedWord := data;
                nibbleIndex := 0;
                -- Find the index of the most significant non-zero
                -- nibble
                for i in NIBBLE_COUNT-1 downto 0 loop
                    if (nibbleIndex = 0 and 
                        storedWord(i * 4 + 3 downto i*4) /= "0000") then
                        nibbleIndex := i;
                    end if;
                end loop;
            elsif (dataShiftEn = ACTIVE) then
                -- Shift the data by changing the nibble index
                if (nibbleIndex > 0) then
                    nibbleIndex := nibbleIndex - 1;
                end if;
            end if;
        end if;
        
        currentNibble <= storedWord((nibbleIndex) * 4 + 3 downto
                                    (nibbleIndex) * 4);
        -- The last nibble is being output when nibbleIndex is 0
        if (nibbleIndex = 0) then
            lastNibbleMode <= ACTIVE;
        else
            lastNibbleMode <= not ACTIVE;
        end if;
    end process;
    
    --------------------------------------------------------------------
    --                          PULSE ENCODER                         --
    -- Converts the current nibble into a waveform encoding           --
    --                                                                --
    -- Can also output the frame start/word start encodings with the  --
    -- appropriate signals                                            --
    --------------------------------------------------------------------
    PULSE_ECNODER: process(currentNibble, frameStart, wordStart)
    begin
        if (frameStart = ACTIVE) then
            currentWaveForm <= FRAME_START;
        elsif (wordStart = ACTIVE) then
            currentWaveForm <= WORD_START;
        else
            -- Index into the nibbles array with the currentNibble as
            -- an integer index
            currentWaveForm <= nibbles(
                                       to_integer(
                                                  unsigned(
                                                           currentNibble
                                                           )
                                                  )
                                      );
        end if;
    end process;
    
    --------------------------------------------------------------------
    --                      PULSE SHIFT REGISTER                      --
    -- Outputs a vpe waveform over the vpe serial port, at the        --
    -- frequency determined by t0En                                   --
    --                                                                --
    -- Also signals when the pulse ends                               --
    --------------------------------------------------------------------
    PULSE_SHIFT: process(clock, reset)
        -- The current pulse to output
        variable currentPulse: std_logic_vector(6 downto 0) := (others => '0');
    begin
        if (reset = ACTIVE) then
            currentPulse := (others => '0');
            vpeSerial <= not ACTIVE;
            pulseCompleteMode <= not ACTIVE;
        elsif (rising_edge(clock)) then
            if (pulseLoadEn = ACTIVE) then
                currentPulse := currentWaveForm;
            end if;
            -- On every t0, even if a load just happened
            if (t0En = ACTIVE) then
                -- Set the vpe serial output to the high bit of the
                -- pulse, and shift
                vpeSerial <= currentPulse(6);
                currentPulse(6 downto 1) := currentPulse(5 downto 0);
                currentPulse(0) := '0';
            end if;
        end if;
        -- A pulse is complete when every single bit has been shifted
        -- out
        if (currentPulse = "0000000") then
            pulseCompleteMode <= ACTIVE;
        else
            pulseCompleteMode <= not ACTIVE;
        end if;
    end process;
    
    --------------------------------------------------------------------
    --                       TRANSMITTER CONTROL                      --
    -- Controls the other processes and the word end port via a state --
    -- machine                                                        --
    --------------------------------------------------------------------
    TX_CONTROL: process(clock, reset)
        -- Used to delay processing of certain states by one clock cycle
        -- for timing reasons
        variable delayed: std_logic := '0';
    begin
        if (reset = ACTIVE) then
            txState <= IDLE;
            dataLoadEn <= not ACTIVE;
            dataShiftEn <= not ACTIVE;
            frameStart <= not ACTIVE;
            wordStart <= not ACTIVE;
            pulseLoadEn <= not ACTIVE;
            wordEndEn <= not ACTIVE;
            delayed := not ACTIVE;
        elsif (rising_edge(clock)) then
            dataLoadEn <= not ACTIVE;
            dataShiftEn <= not ACTIVE;
            frameStart <= not ACTIVE;
            wordStart <= not ACTIVE;
            pulseLoadEn <= not ACTIVE;
            wordEndEn <= not ACTIVE;
            case txState is
                --------------------------------------------------------
                --                      IDLE STATE                    --
                -- Waits until the next transmission to be started,   --
                -- signaled by txMode being high and the last one     --
                -- being finished                                     --
                --                                                    --
                -- Then, signals for the data to be latched in and to --
                -- transmit a frame start, going to the next state in --
                -- the state machine (the WAIT FOR PULSE state)       --
                --------------------------------------------------------
                when IDLE =>
                    if (txMode = ACTIVE and pulseCompleteMode = ACTIVE) 
                    then
                        frameStart <= ACTIVE;
                        pulseLoadEn <= ACTIVE;
                        dataLoadEn <= ACTIVE;
                        txState <= WAIT_FOR_PULSE;
                    end if;
                    
                --------------------------------------------------------
                --                 WAIT FOR PULSE STATE               --
                -- Waits for the current pulse to be completed,       --
                -- signaled by the pulseCompleteMode signal           --
                --                                                    --
                -- Then, signals for the next nibble to be sent, and  --
                -- if lastNibbleMode is active, goes to CHECK SHOULD  --
                -- CONTINUE state and pulses wordEndEn high, else it  --
                -- transisitions to this state again                  --
                --
                -- This state does not do its checks until one clock  --
                -- cycle has passed due to timing constraints         --
                --------------------------------------------------------
                when WAIT_FOR_PULSE =>
                    -- Use the delayed variable to delay processing by 
                    -- one clock cycle
                    if (delayed = ACTIVE and pulseCompleteMode = ACTIVE) 
                    then
                        -- Reset the delayed variable upon transition
                        delayed := not ACTIVE;
                        pulseLoadEn <= ACTIVE;
                        dataShiftEn <= ACTIVE;
                        if (lastNibbleMode = ACTIVE) then
                            wordEndEn <= ACTIVE;
                            txState <= CHECK_SHOULD_CONTINUE;
                        end if;
                    elsif (delayed = not ACTIVE) then
                        -- Set the delay signal when it is not active
                        -- delaying the processing by a clock cycle
                        delayed := ACTIVE;
                    end if;
                    
                
                --------------------------------------------------------
                --              CHECK SHOULD CONTINUE STATE           --
                -- Waits for one clock cycle then checks the value of --
                -- the txMode signal                                  --
                --                                                    --
                -- if it is high, it goes to the TX WORD START state  --
                -- otherwise, it goes to the IDLE state               --
                --------------------------------------------------------
                when CHECK_SHOULD_CONTINUE =>
                    -- Delay processing by 1 clock cycle
                    if (delayed = ACTIVE) then
                        -- Reset delayed on transition
                        delayed := not ACTIVE;
                        if (txMode = ACTIVE) then
                            txState <= TX_WORD_START;
                        else
                            txState <= IDLE; 
                        end if;
                    else
                        -- Set the delay signal when it is not active
                        -- delaying the processing by a clock cycle
                        delayed := ACTIVE;
                    end if;
                    
                --------------------------------------------------------
                --                TX WORD START STATE                 --
                -- Waits for the last pulse to be done being sent,    --
                -- then signals for a word start to be sent, going to --
                -- the WAIT FOR PULSE state                           --
                --------------------------------------------------------
                when TX_WORD_START =>
                    if (pulseCompleteMode = ACTIVE) then
                        wordStart <= ACTIVE;
                        pulseLoadEn <= ACTIVE;
                        dataLoadEn <= ACTIVE;
                        txState <= WAIT_FOR_PULSE;
                    end if;
            end case;
        end if;
    end process;
end Procedural;