--------------------------------------------------------------------------------
-- Author: Lexi Allen
--
-- Create Date: 11/16/2024 10:18:00 AM
-- Design Name: VPE Serial Interface
-- Module Name: Debouncer - Procedural
-- Description: A simple generic signal debouncer
--------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity Debouncer is
    Generic (
        QUEUE_LENGTH: integer := 16; -- How many bits should be stored
        QUEUE_FILL_AMOUNT: integer := 12 -- How many of the stored bits need to
                                         -- be a certain value to flip
    );
    Port ( clock : in STD_LOGIC;
           reset : in STD_LOGIC;
           sampleEn : in STD_LOGIC; -- Pulse this to trigger a sample
           noisySignal : in STD_LOGIC; -- This is the signal to debounce
           debouncedSignal : out STD_LOGIC -- This is the filtered signal
        );
end Debouncer;

architecture Procedural of Debouncer is
    constant ACTIVE: std_logic := '1';
begin
    -- [DEBOUNCE]
    -- Reads a bit from noisySignal every time sampleEn is pulsed and checks
    -- if QUEUE_FILL_AMOUNT bits stored are in one state and then switches the
    -- output to that state if so
    DEBOUNCE: process(clock, reset) is
        variable queue: std_logic_vector(QUEUE_LENGTH-1 downto 0) := (others => '0');
        variable inactive_count: integer range 0 to QUEUE_LENGTH;
        variable active_count: integer range 0 to QUEUE_LENGTH;
    begin
        if (reset = ACTIVE) then
            debouncedSignal <= not ACTIVE;
            queue := (others => '0');
        elsif (rising_edge(clock)) then
            inactive_count := 0;
            active_count := 0;
            if sampleEn = ACTIVE then
                queue(QUEUE_LENGTH-1 downto 1) := queue(QUEUE_LENGTH-2 downto 0);
                queue(0) := noisySignal;
                for index in 0 to QUEUE_LENGTH-1 loop
                    if (queue(index) = ACTIVE) then
                        active_count := active_count + 1;
                    else
                        inactive_count := inactive_count + 1;
                    end if;
                end loop;
                
                if (active_count >= QUEUE_FILL_AMOUNT) then
                    debouncedSignal <= ACTIVE;
                elsif (inactive_count >= QUEUE_FILL_AMOUNT) then
                    debouncedSignal <= not ACTIVE;
                end if;
            end if;
        end if;
    end process;
end Procedural;
