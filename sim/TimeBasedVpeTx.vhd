--------------------------------------------------------------------------------
-- Author: Lexi Allen
-- Create Date: 09/08/2024 10:05:56 AM
-- Design Name: Time Input Based VPE Transmitter
-- Module Name: TimeBasedVpeTx - Simulation
-- Description: Implements a simulation-only based VPE transmitter
--------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;


entity TimeBasedVpeTx is
	Generic (
	   NIBBLES: integer := 8
	);
	Port (
    	t0Time : in time;           -- Defines the time base of the VPE signal
    	data : in std_logic_vector( -- This is the current word being sent
    	   NIBBLES*4-1 downto 0
    	);	                        
    	txMode : in std_logic;      -- When this gets flipped high, a 
                                    -- transmission starts being sent
    	vpeSerial : out std_logic;  -- The signal that the transmission is being
                                    -- sent along
    	wordEndEn : out std_logic   -- This turns high at the beginning of the
                                    -- ultimate pulse of a word being set, when
                                    -- it goes high, you can deassert the txmode
                                    -- until it goes low to stop the frame, or
                                    -- keep it asserted to continue the frame,
                                    -- if you deassert txmode, you can reassert
                                    -- it right after wordEndEn goes low to
                                    -- immediately send a frame after the
                                    -- current one
	);
end TimeBasedVpeTx;

architecture Simulation of TimeBasedVpeTx is

	-- A type that defines how a pulse in its low to high t0 counts
	type pulse_type is record
    	low: integer range 0 to 7;
    	high: integer range 0 to 7;
	end record pulse_type;
    
	-- Loads a new pulse into the pulse transmitter
	signal loadNewPulse : std_logic;
	-- The pulse to load
	signal pulseToLoad : pulse_type;
	-- Asserted when the pulse transmitter has finished sending its last pulse
	signal pulseEnd : std_logic;
    
	-- All the pulse constants
	constant FRAME_START :  	pulse_type := (
    	low => 6,
    	high => 1
	);
	constant WORD_START :   	pulse_type := (
    	low => 1,
    	high => 1
	);
	constant PULSE_0 :      	pulse_type := (
    	low => 1,
    	high => 2
	);
	constant PULSE_1 :      	pulse_type := (
    	low => 2,
    	high => 1
	);
	constant PULSE_2 :      	pulse_type := (
    	low => 1,
    	high => 3
	);
	constant PULSE_3 :      	pulse_type := (
    	low => 2,
    	high => 2
	);
	constant PULSE_4 :      	pulse_type := (
    	low => 3,
    	high => 1
	);
	constant PULSE_5 :      	pulse_type := (
    	low => 1,
    	high => 4
	);
	constant PULSE_6 :      	pulse_type := (
    	low => 2,
    	high => 3
	);
	constant PULSE_7 :      	pulse_type := (
    	low => 3,
    	high => 2
	);
	constant PULSE_8 :      	pulse_type := (
    	low => 4,
    	high => 1
	);
	constant PULSE_9 :      	pulse_type := (
    	low => 1,
    	high => 5
	);
	constant PULSE_A :      	pulse_type := (
    	low => 2,
    	high => 4
	);
	constant PULSE_B :      	pulse_type := (
    	low => 3,
    	high => 3
	);
	constant PULSE_C :      	pulse_type := (
    	low => 4,
    	high => 2
	);
	constant PULSE_D :      	pulse_type := (
    	low => 5,
    	high => 1
	);
	constant PULSE_E :      	pulse_type := (
    	low => 1,
    	high => 6
	);
	constant PULSE_F :      	pulse_type := (
    	low => 2,
    	high => 5
	);
	constant PULSE_UNKNOWN :	pulse_type := (
    	low => 0,
    	high => 0
	);
   
	-- Active constant
	constant ACTIVE : std_logic := '1';
    
	-- Converts a nibble to a VPE pulse
	function nibbleToPulse(
    	nibble : STD_LOGIC_VECTOR(3 downto 0)
	) return pulse_type is
	begin
    	case nibble is  	 
        	when X"0" =>
            	return PULSE_0;  	 
        	when X"1" =>
            	return PULSE_1;
        	when X"2" =>
            	return PULSE_2;
        	when X"3" =>
            	return PULSE_3;
        	when X"4" =>
            	return PULSE_4;
        	when X"5" =>
            	return PULSE_5;
        	when X"6" =>
            	return PULSE_6;
        	when X"7" =>
            	return PULSE_7;
        	when X"8" =>
            	return PULSE_8;
        	when X"9" =>
            	return PULSE_9;
        	when X"A" =>
            	return PULSE_A;
        	when X"B" =>
            	return PULSE_B;
        	when X"C" =>
            	return PULSE_C;
        	when X"D" =>
            	return PULSE_D;
        	when X"E" =>
            	return PULSE_E;
        	when X"F" =>
            	return PULSE_F;
        	when others =>
            	return PULSE_UNKNOWN;
    	end case;
	end function;
    
	-- A helper function for getting a nibble from a word
	function getNibbleAtIndex(
		word: std_logic_vector(NIBBLES*4 - 1 downto 0);
		index: integer range 0 to NIBBLES-1
	) return std_logic_vector is
	begin
    	return word(index*4 + 3 downto index*4);
	end function;
    
    
	-- Gets where we want to start the word if we want to avoid leading zeros
	function getWordBeginPointer(word: std_logic_vector(NIBBLES*4 - 1 downto 0)) 
	return integer is
	begin
    	for index in NIBBLES - 1 downto 0 loop
        	if (getNibbleAtIndex(word,index) /= "0000") then
            	return index + 1;
        	end if;
    	end loop;
    	return 1;
	end function;
    
begin

	-- Sends a pulse over the vpeSerial port
	SEND_PULSE: process
    	variable pulse : pulse_type;
	begin
    	-- Waiting for pulse state --
    	vpeSerial <= not ACTIVE;
    	pulseEnd <= ACTIVE;
    	wait until loadNewPulse = ACTIVE;
    	pulseEnd <= not ACTIVE;
    	-- Sending pulse state --
    	pulse := pulseToLoad;
    	wait for pulse.low * t0Time;
    	vpeSerial <= ACTIVE;
    	wait for pulse.high * t0Time;
	end process;

	-- Controls the sequence of pulses to be sent
	CONTROL_PULSES: process
    	variable currentData : std_logic_vector(NIBBLES*4-1 downto 0) := 
			(others => '0');
    	variable dataPointer : integer range 0 to NIBBLES := 0;
	begin
    	-- Let's make sure wordEndEn is inactive before we do anything
    	wordEndEn <= not ACTIVE;
    	-- to start off with, we want to wait until txMode activates as we are
		-- idling at this point
    	wait until txMode = ACTIVE;
    	-- After thats done, we load the start frame of the frame we are sending
    	currentData := data;
    	dataPointer := getWordBeginPointer(data);
    	-- then we want to make sure the last pulse has already ended before
		-- starting the frame off
    	if (pulseEnd /= ACTIVE) then
        	wait until pulseEnd = ACTIVE;
    	end if;
   	 
    	-- Then we start the frame off with a FRAME_START
    	pulseToLoad <= FRAME_START;
    	loadNewPulse <= ACTIVE;
    	wait for t0Time;
    	loadNewPulse <= not ACTIVE;
    	loop
        	-- Then we constantly wait until the pulse we last sent finiishes
        	wait until pulseEnd = ACTIVE;
        	-- If we just finished the penultimate pulse
        	if (dataPointer = 1) then
            	-- We assert word end
            	wordEndEn <= ACTIVE;
            	-- We load the ultimate post
            	pulseToLoad <= nibbleToPulse(getNibbleAtIndex(data,0));
            	loadNewPulse <= ACTIVE;
            	wait for t0Time;
            	loadNewPulse <= not ACTIVE;
            	-- Then after the load is completed t0Time has elapsed, and we
            	-- check if txMode is still active
            	if (txMode = not ACTIVE) then
                	-- if not, we finish the frame
                	wordEndEn <= not ACTIVE;
                	exit;
            	else
                	-- otherwise we continue the frame as normal
                	wordEndEn <= not ACTIVE;
                	dataPointer := 0;
            	end if;
        	-- if instead we finished the ultimate pulse, and are continuing the
			-- frame
        	elsif (dataPointer = 0) then
            	-- we load the next word
            	currentData := data;
            	dataPointer := getWordBeginPointer(data);
            	-- then send a word start
            	pulseToLoad <= WORD_START;
            	loadNewPulse <= ACTIVE;
            	wait for t0Time;
            	loadNewPulse <= not ACTIVE;
        	else
            	-- In all other cases, we send the next nibble
            	dataPointer := dataPointer - 1;
            	pulseToLoad <= nibbleToPulse(
					getNibbleAtIndex(data,dataPointer)
				);
            	loadNewPulse <= ACTIVE;
            	wait for t0Time;
            	loadNewPulse <= not ACTIVE;
        	end if;
    	end loop;
	end process;
end Simulation;