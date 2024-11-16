--------------------------------------------------------------------------------
-- Author: Lexi Allen
-- Create Date: 10/26/2024 10:21:58 AM
-- Design Name: Vpe Receiver
-- Module Name: VpeRx - Procedural
-- Description: Implements a generic vpe receiver component, that can be
-- specialized for a given data width you wish to receive.
--
-- Reads incoming serial from the vpeSerial port, and signals newDataEn when
-- a new word has been read in, and endFrameEn when it detects a frame has been
-- finished.
--
-- The maximum frequency it supports is (FPGA CLOCK RATE)/~40
--------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity VpeRx is
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
end VpeRx;

architecture Procedural of VpeRx is
    --- TYPES ---
    
    -- The state for the frame end latch
    type latch_state_t is (
        -- This latch is waiting for the frame to start
        -- So any frameStartEn signal will not send an endFrameEn
        WAITING_FOR_FRAME_START, 
        -- This latch is waiting for the frame to end
        -- So a frameStartEn signal will send an endFrameEn
        WAITING_FOR_FRAME_END
    );
    
    --- CONSTANTS ---
    constant ACTIVE: STD_LOGIC := '1';
    
    --- SIGNAL_DENOISER SIGNALS ---
    
    -- the vpeSerial port put through a simple denoiser
    signal denoisedVpe: STD_LOGIC;
    
    --- FRAME_START DETECTOR SIGNALS ---
    
    -- signals that a frame start has been detected
    signal frameStartEn: STD_LOGIC;
    signal fallingEdgeDetectedEn: STD_LOGIC;
    
    -- the size of the frame start in cycles
    signal t0Cycles: unsigned(31 downto 0);
    
    -- half of the prior
    signal halfT0Cycles: unsigned(31 downto 0);
    
    --- SAMPLER SIGNALS ---
    
    -- How many low counts were sampled on the current pulse
    signal lowCount: integer range 0 to 6 := 0;
    -- How many high counts were sampled on the current pulse
    signal highCount: integer range 0 to 6 := 0;
    -- Has the current pulse been completed?
    signal pulseEndEn: STD_LOGIC;
    
    --- PULSE_DECODER_SIGNALS ---
    
    -- Is the current pulse a word end? (1L1H)
    signal pulseIsWordEnd: STD_LOGIC;
    
    -- What nibble does it represent?
    signal nibble: STD_LOGIC_VECTOR(3 downto 0);
    
    --- FRAME_END_DETECTOR SIGNALS ---
    -- has a frame end caused by idling been detected?
    signal frameEndEn: STD_LOGIC;
    
    --- NEW_DATA_DETECTOR SIGNALS ---
    -- has new data been detected?
    signal newDataDetected: STD_LOGIC;
    
    --- FRAME_STATE_LATCH SIGNALS ---
    
    -- did the previous frame just end?
    signal prevFrameEndedEn: STD_LOGIC;
    
    -- the current state of the frame state latch
    signal latchState: latch_state_t := WAITING_FOR_FRAME_START;
begin

    -- SIGNAL DENOISER --
    -- This applies a very simple denoising algorithm on the vpe input
    -- It runs it through a queue of 16 bits, and once flip_count bits
    -- are in one state or another, it will switch the output to that
    -- state
    SIGNAL_DENOISER: process (clock, reset)
        -- The amount of bits in one state required to cause the output to flip
        constant flip_count: integer range 0 to 16 := 12;
        variable bits: std_logic_vector(15 downto 0) := X"0000";
        variable inactive_count: integer range 0 to 16;
        variable active_count: integer range 0 to 16;
    begin
        if (reset = ACTIVE) then
            bits := (others => '0');
            denoisedVpe <= not ACTIVE;
            inactive_count := 0;
            active_count := 0;
        elsif (rising_edge(clock)) then
            bits(15 downto 1) := bits(14 downto 0);
            bits(0) := vpeSerial;
            inactive_count := 0;
            active_count := 0;
            for index in 0 to 15 loop
                if (bits(index) = ACTIVE) then
                    active_count := active_count + 1;
                else
                    inactive_count := inactive_count + 1;
                end if;
            end loop;
            if (active_count >= flip_count) then
                denoisedVpe <= ACTIVE;
            elsif (inactive_count >= flip_count) then
                denoisedVpe <= not ACTIVE;
            end if;
        end if;
    end process;
    
    -- FRAME START DETECTOR --
    -- this counts the pulse length of the incoming pulse in raw pulse counts
    -- and uses that to detect when a frame start happens when the low count
    -- is 5.5x greater in length than the high count
    FRAME_START_DETECTOR: process(clock, reset)
        variable low_count: unsigned(31 downto 0) := TO_UNSIGNED(0,32);
        variable high_count: unsigned(31 downto 0) := TO_UNSIGNED(0,32);
        variable half_high_count: unsigned(31 downto 0);
        variable multiplied_high_count: unsigned(31 downto 0);
    begin
        if (reset = ACTIVE) then
            low_count := TO_UNSIGNED(0,32);
            high_count := TO_UNSIGNED(0,32);
        elsif (rising_edge(clock)) then
            frameStartEn <= not ACTIVE;
            t0Cycles <= TO_UNSIGNED(0,32);
            halfT0Cycles <= TO_UNSIGNED(0,32);
            half_high_count := TO_UNSIGNED(0,32);
            multiplied_high_count := TO_UNSIGNED(0,32);
            fallingEdgeDetectedEn <= not ACTIVE;
            if (denoisedVpe = ACTIVE) then
                high_count := high_count + 1;
            else
                if (high_count = 0) then
                    low_count := low_count + 1;
                else
                    -- Let's do a 5.5x multiplication to see if we've reached a
                    -- frame end
                    
                    -- multiply by doing (x << 2) + x + (x >> 1)
                    half_high_count(30 downto 0) := high_count(31 downto 1);
                    multiplied_high_count(31 downto 2) := 
                        high_count(29 downto 0);
                    multiplied_high_count := multiplied_high_count 
                                             + high_count;
                    multiplied_high_count := multiplied_high_count 
                                             + half_high_count;
                    -- We have detected a frame start, signal it
                    if (multiplied_high_count <= low_count) then
                        frameStartEn <= ACTIVE;
                        t0Cycles <= high_count;
                        halfT0Cycles <= half_high_count;
                    else
                        fallingEdgeDetectedEn <= ACTIVE;
                    end if;              
                    high_count := TO_UNSIGNED(0,32);
                    low_count := TO_UNSIGNED(1,32);
                end if;
            end if;
        end if;
    end process;
    
    -- SAMPLER --
    -- Samples the vpe signal at pulse midpoints at a specified rate determined
    -- by the frame start detector
    --
    -- Pulses the pulseEndEn signal when an entire pulse has been sampled
    SAMPLER: process(clock, reset)
        -- Used for timing the pulse sampling
        variable cyclesRemaining: integer := 0;
        variable fullCycleSize: integer := 0;
        
        -- Used to determine what should happen when the next clock pulse
        -- happens, determines if the count is reset, and what low is reset to
        -- Necessary for timing resets of values one clock cycle after the
        -- pulseEndEn signal is set high
        type next_clock_action_t is (NONE, SEND_PULSE, RESET_COUNTS);
        variable next_clock_action: next_clock_action_t := NONE;
    begin
        if (reset = ACTIVE) then
            cyclesRemaining := 0;
            fullCycleSize := 0;
            lowCount <= 0;
            highCount <= 0;
            next_clock_action := NONE;
        elsif (rising_edge(clock)) then
            -- Perform the action described by next_clock_action
            pulseEndEn <= not ACTIVE;
            if (next_clock_action = SEND_PULSE) then
                pulseEndEn <= ACTIVE;
                next_clock_action := RESET_COUNTS;
            elsif (next_clock_action = RESET_COUNTS) then
                highCount <= 0;
                lowCount <= 0;
                next_clock_action := NONE;
            end if;
            
            
            -- If a new frame start has been detected, signal that a pulse has
            -- ended if necessary, then setup the pulse timing signals for the
            -- frames t0 time, then queue a reset of the pulse counts, with
            -- low being reset to 0 as this wasn't triggered by a pulse edge
            if (frameStartEn = ACTIVE) then
                if (highCount > 0) then
                    pulseEndEn <= ACTIVE;
                end if;
                cyclesRemaining := TO_INTEGER(halfT0Cycles) - 1;
                fullCycleSize := TO_INTEGER(t0Cycles);
                next_clock_action := RESET_COUNTS;
            elsif (fallingEdgeDetectedEn = ACTIVE) then
                if (highCount > 0) then
                    next_clock_action := SEND_PULSE;
                else
                    next_clock_action := RESET_COUNTS;
                end if;
                cyclesRemaining := (fullCycleSize/2) - 1;
            elsif (cyclesRemaining = 0) then
                if (fullCycleSize > 0) then
                    -- When it is time to sample, and we are sampling
                    if (denoisedVpe = ACTIVE) then
                        -- If the pulse is high, increment the high count
                        if (highCount < 6) then
                            highCount <= highCount + 1;
                        end if;
                    else
                        lowCount <= lowCount + 1;
                    end if;
                    cyclesRemaining := fullCycleSize-1;
                end if;
            else
                cyclesRemaining := cyclesRemaining - 1;
            end if;
            
        end if;
    end process;
    
    -- PULSE DECODER --
    -- converts a pulse into a nibble, and detects if its a word end
    PULSE_DECODER: process(lowCount, highCount)
    begin
        pulseIsWordEnd <= not ACTIVE;
        nibble <= X"0";
        if (lowCount = 1 and highCount = 1)  then
            pulseIsWordEnd <= ACTIVE;
        elsif (lowCount = 1 and highCount = 2) then
            nibble <= X"0";
        elsif (lowCount = 2 and highCount = 1) then
            nibble <= X"1";
        elsif (lowCount = 1 and highCount = 3) then
            nibble <= X"2";
        elsif (lowCount = 2 and highCount = 2) then
            nibble <= X"3";
        elsif (lowCount = 3 and highCount = 1) then
            nibble <= X"4";
        elsif (lowCount = 1 and highCount = 4) then
            nibble <= X"5";
        elsif (lowCount = 2 and highCount = 3) then
            nibble <= X"6";
        elsif (lowCount = 3 and highCount = 2) then
            nibble <= X"7";
        elsif (lowCount = 4 and highCount = 1) then
            nibble <= X"8";
        elsif (lowCount = 1 and highCount = 5) then
            nibble <= X"9";
        elsif (lowCount = 2 and highCount = 4) then
            nibble <= X"A";
        elsif (lowCount = 3 and highCount = 3) then
            nibble <= X"B";
        elsif (lowCount = 4 and highCount = 2) then
            nibble <= X"C";
        elsif (lowCount = 5 and highCount = 1) then
            nibble <= X"D";
        elsif (lowCount = 1 and highCount = 6) then
            nibble <= X"E";
        elsif (lowCount = 2 and highCount = 5) then
            nibble <= X"F";
        end if;
    end process;
    
    
    -- DATA REGISTER --
    -- Latches each nibble on a pulseEnd, storing them in a shift register to
    -- store a full word of data
    --
    -- clears out the shift register on a pulseEndEn&pulseIsWordEnd 
    -- or prevFrameEndedEn
    DATA_REGISTER: process(clock, reset)
        variable latchedData: std_logic_vector(DATA_WIDTH-1 downto 0)
                              := (others => '0');
    begin
        if (reset = ACTIVE) then
            latchedData := (others => '0');
        elsif (rising_edge(CLOCK)) then
            if (pulseEndEn = ACTIVE) then
                if (pulseIsWordEnd = ACTIVE) then
                    latchedData := (others => '0');
                else
                    if (DATA_WIDTH > 4) then
                        latchedData(DATA_WIDTH-1 downto 4) := 
                            latchedData(DATA_WIDTH-5 downto 0); 
                    end if; 
                    latchedData(3 downto 0) := nibble;
                end if;
            elsif (prevFrameEndedEn = ACTIVE) then
                latchedData := (others => '0');
            end if;
            
            data <= latchedData;
        end if;
    end process;
    
    
    -- FRAME END DETECTOR --
    -- Detects a frame end due to idling, by pulsing frameEndEn when
    -- the low pulse count gets changed to 6
    FRAME_END_DETECTOR: process(clock, reset)
        variable last_count: integer range 0 to 6;
    begin
        if (reset = ACTIVE) then
            last_count := 0;
            frameEndEn <= not ACTIVE;
        elsif (rising_edge(clock)) then
            if (lowCount = 6) and (last_count /= 6) then
                frameEndEn <= ACTIVE;
            else
                frameEndEn <= not ACTIVE;
            end if;
            last_count := lowCount;
        end if;
    end process;
    
    -- FRAME STATE LATCH --
    -- A simple state machine that determines when to signal a frame end
    -- Needs to be a state machine to prevent duplicate frame ends
    FRAME_STATE_LATCH: process(clock, reset)
    begin
        if (reset = ACTIVE) then
            latchState <= WAITING_FOR_FRAME_START;
        elsif (rising_edge(clock)) then
            prevFrameEndedEn <= not ACTIVE;
            case latchState is
            
                ----------------------------------------------------------------
                --                WAITING FOR FRAME START STATE               --
                --                                                            --
                -- Waits for a frame start before it determines that it       --
                -- should treat any frameStartEn signal as an implicit frame  --
                -- end                                                        --
                ----------------------------------------------------------------
                when WAITING_FOR_FRAME_START =>
                    if frameStartEn = ACTIVE then
                        latchState <= WAITING_FOR_FRAME_END;
                    end if;
                ----------------------------------------------------------------
                --                WAITING FOR FRAME END STATE                 --
                --                                                            --
                -- Waits for either a frame start or frame end due to timeout --
                -- and signals a frame end signal when that happens           --
                --                                                            --
                -- If it was caused by timeout, switches back to the previous --
                -- state, otherwise stays in this state                       --
                ----------------------------------------------------------------
                when WAITING_FOR_FRAME_END =>
                    if frameStartEn = ACTIVE or frameEndEn = ACTIVE then
                        prevFrameEndedEn <= ACTIVE;
                        if frameEndEn = ACTIVE then
                            latchState <= WAITING_FOR_FRAME_START;
                        end if;
                    end if;
            end case;
        end if;
    end process;
    
    -- Map prevFrameEndedEn to the endFrameEn port
    endFrameEn <= prevFrameEndedEn;
    
    -- NEW DATA DETECTOR --
    -- Detects when a word has been finished and should be signaled over the
    -- newDataEn port
    --
    -- Happens either when a word end has been received, or a frame end has
    -- been detected
    NEW_DATA_DETECTOR: process(prevFrameEndedEn, pulseEndEn, pulseIsWordEnd)
    begin
        newDataEn <= prevFrameEndedEn or (pulseEndEn and pulseIsWordEnd);
    end process;
end Procedural;
