library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Basys3Wrapper is
    port (
        clk: in STD_LOGIC;
        btnC: in STD_LOGIC; -- reset
        btnU: in STD_LOGIC; -- send to UART
        btnL: in STD_LOGIC; -- send to VPE
        RsRx: in STD_LOGIC;
        JB_0: in STD_LOGIC;
        seg: out STD_LOGIC_VECTOR(6 downto 0);
        an: out STD_LOGIC_VECTOR(3 downto 0);
        RsTx: out STD_LOGIC;
        JA_0: out STD_LOGIC
    );
end Basys3Wrapper;


architecture Wrapper of Basys3Wrapper is
    component TransceiverController is
        generic (
            BUFFER_SIZE: integer := 200000;
            SEPARATE_BUFFERS: boolean := false
        );
        Port ( clock : in STD_LOGIC;
               reset : in STD_LOGIC;
               uartRx : in STD_LOGIC;
               vpeRx : in STD_LOGIC;
               sendToVpeEn : in STD_LOGIC;
               sendToUartEn : in STD_LOGIC;
               uartTx : out STD_LOGIC;
               vpeTx : out STD_LOGIC;
               sevenSegmentHex : out STD_LOGIC_VECTOR (15 downto 0));
    end component;
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
    -- Now we implement the parts piecemeal
    signal clock: std_logic;
    signal reset: std_logic;
    signal sevenSegmentHex: std_logic_vector(15 downto 0);
begin
    -- rename clock/reset
    clock <= clk;
    reset <= btnC;

    RX_TX: TransceiverController generic map (
        BUFFER_SIZE => 8192,
        SEPARATE_BUFFERS => true
    ) port map (
        clock => clock,
        reset => reset,
        uartRx => RsRx,
        vpeRx => '0',
        sendToVpeEn => '0',
        sendToUartEn => '0',
        sevenSegmentHex => sevenSegmentHex
    );

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

end Wrapper;