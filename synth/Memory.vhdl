--------------------------------------------------------------------------------
-- Author: Lexi Allen 
-- 
-- Create Date: 11/16/2024
-- Design Name: VPE Serial Interface
-- Module Name: Memory - Procedural
-- Description:
--  A generic dual port memory buffer, can have the ports go to separate buffers
--  or the same buffer
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity Memory is
    generic (
        -- The size of the RX/TX buffers
        BUFFER_SIZE: integer := 200000;
        -- The data size stored in the buffers
        DATA_SIZE: integer := 8;
        -- Should the RX/TX buffers be separate buffers rather than occupy the
        -- same memory space
        SEPARATE_BUFFERS: boolean := false
    );
    port(
        reset: in std_logic;
        clock: in std_logic;
        txAddress: in integer range 0 to BUFFER_SIZE-1;
        rxAddress: in integer range 0 to BUFFER_SIZE-1;
        txWriteData: in std_logic_vector(DATA_SIZE-1 downto 0);
        rxWriteData: in std_logic_vector(DATA_SIZE-1 downto 0);
        txWriteEn: in std_logic;
        rxWriteEn: in std_logic;
        outTx: out std_logic_vector(DATA_SIZE-1 downto 0);
        outRx: out std_logic_vector(DATA_SIZE-1 downto 0)
    );
end Memory;

architecture Procedural of Memory is
    type buffer_t is array(BUFFER_SIZE-1 downto 0) of std_logic_vector(DATA_SIZE-1 downto 0);
    constant ACTIVE: std_logic := '1';
begin
    -- [MEMORY BLOCKS SEPARATE]
    -- Controls 2 single port buffers with the control signals
    MEMORY_BLOCKS_SEPARATE: if SEPARATE_BUFFERS generate
        CONTROL_MEMORY: process(clock, reset)
            variable data_buffer_rx: buffer_t;
            variable data_buffer_tx: buffer_t;
        begin
            if (reset = ACTIVE) then
            elsif (rising_edge(clock)) then
                if (txWriteEn = ACTIVE) then
                    data_buffer_tx(txAddress) := txWriteData;
                end if;
                outTx <= data_buffer_tx(txAddress);
                if (rxWriteEn = ACTIVE) then
                    data_buffer_rx(rxAddress) := rxWriteData;
                end if;
                outRx <= data_buffer_rx(rxAddress);
            end if;
        end process;
    end generate;

    -- [MEMORY BLOCKS COMBINED]
    -- Controls 1 dual port buffer with the control signals
    MEMORY_BLOCKS_COMBINED: if not SEPARATE_BUFFERS generate
        signal data_buffer: buffer_t;
    begin
        CONTROL_MEMORY_TX: process(clock, reset)
        begin
            if (reset = ACTIVE) then
            elsif (rising_edge(clock)) then
                if (txWriteEn = ACTIVE) then
                    data_buffer(txAddress) <= txWriteData;
                end if;
                outTx <= data_buffer(txAddress);
            end if;
        end process;
        CONTROL_MEMORY_RX: process(clock, reset)
        begin
            if (reset = ACTIVE) then
            elsif (rising_edge(clock)) then
                if (rxWriteEn = ACTIVE) then
                    data_buffer(rxAddress) <= rxWriteData;
                end if;
                outRx <= data_buffer(rxAddress);
            end if;
        end process;
    end generate;
end Procedural;