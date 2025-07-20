library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use work.common_pack.all;
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity cmdProc is
port (
    clk:		in std_logic;
    reset:        in std_logic;
    rxnow:        in std_logic; --when it is high, tansfer the data 
    rxData:            in std_logic_vector (7 downto 0);
    txData:            out std_logic_vector (7 downto 0);
    rxdone:        out std_logic;
    ovErr:        in std_logic;
    framErr:    in std_logic;
    txnow:        out std_logic;
    txdone:        in std_logic;
    start: out std_logic;
    numWords_bcd: out BCD_ARRAY_TYPE(2 downto 0);
    dataReady: in std_logic;
    byte: in std_logic_vector(7 downto 0);
    maxIndex: in BCD_ARRAY_TYPE(2 downto 0);
    dataResults: in CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1);
    seqDone: in std_logic
    );
end cmdProc;

architecture Behavioral of cmdProc is
    type state_type is (IDLE, CHECK_A_WAIT,CHECK_A, CHECK_DIGIT1_WAIT,CHECK_DIGIT1, CHECK_DIGIT2_WAIT,CHECK_DIGIT2,S0,S1,S2,S3,S4,S5,S6,S7,L0,L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,P0,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P10_space,P11,P12,P12_space,P13,P14);
    signal current_state: state_type:= IDLE; --default to IDLE at the beginning
    signal next_state: state_type;  --default to IDLE at the beginning
    signal reg: BCD_ARRAY_TYPE(2 downto 0);
    signal reg_1, reg_2, reg_3: std_logic_vector(3 downto 0); -- value holders for finite state machine (the 3 register stored values)
    signal R1_en, R2_en, R3_en : std_logic; -- enable bit for the registers
    signal fsm_valid: std_logic; --this is the bit which is default to 0 but set to 1 once the fsm has reached final state, when this is 1 the numWords BCD is updated with the values from the 3 registers
    signal test: std_logic_vector(3 downto 0); --This is the 8bit asc11 data converted into 4 bit binary representation
    signal byte_Done_msd: STD_LOGIC_VECTOR(7 downto 0);
    signal byte_Done_lsd: STD_LOGIC_VECTOR(7 downto 0);
    signal reg_dataresults: CHAR_ARRAY_TYPE(0 to 6);  -- This initializes all elements to '0'
    signal reg_maxindex: BCD_ARRAY_TYPE(2 downto 0);
    signal x : STD_LOGIC_VECTOR(7 downto 0);
    signal output_valuemsd : STD_LOGIC_VECTOR(7 downto 0) := "00000000";
    signal output_valuelsd : STD_LOGIC_VECTOR(7 downto 0) := "00000000";
    signal index : integer:= 0; --initialise at 0
    signal maxindex_convert:BCD_ARRAY_TYPE(2 downto 0) ;
    signal maxindex_done: STD_LOGIC_VECTOR(7 downto 0);
    signal maxindex_0: STD_LOGIC_VECTOR(7 downto 0);
    signal maxindex_1: STD_LOGIC_VECTOR(7 downto 0);
    signal maxindex_2: STD_LOGIC_VECTOR(7 downto 0);
    
BEGIN

   
        
    convert_4bit: PROCESS(rxData) --process converts the 8bit ascII data into 4 bit binary representation, A is set to "1111" as it's unused and other invalid inputs are set to "1110" which is also an unused combination
        VARIABLE rx_Data4b_conv: std_logic_vector(3 downto 0);
    BEGIN
        CASE rxData IS
            WHEN "00110000" => rx_Data4b_conv := "0000";  -- ASCII '0'
            WHEN "00110001" => rx_Data4b_conv := "0001";  -- ASCII '1'
            WHEN "00110010" => rx_Data4b_conv := "0010";  -- ASCII '2'
            WHEN "00110011" => rx_Data4b_conv := "0011";  -- ASCII '3'
            WHEN "00110100" => rx_Data4b_conv := "0100";  -- ASCII '4'
            WHEN "00110101" => rx_Data4b_conv := "0101";  -- ASCII '5'
            WHEN "00110110" => rx_Data4b_conv := "0110";  -- ASCII '6'
            WHEN "00110111" => rx_Data4b_conv := "0111";  -- ASCII '7'
            WHEN "00111000" => rx_Data4b_conv := "1000";  -- ASCII '8'
            WHEN "00111001" => rx_Data4b_conv := "1001";  -- ASCII '9'
            WHEN "01000001" => rx_Data4b_conv := "1111";  -- We set ASCII 'A' to be binary "1111" as this is not used by any of our other cases
            WHEN "01100001" => rx_Data4b_conv := "1111";  --ASCII 'a' is also valid starting argument, set to "1111" which is the same as A
            WHEN "01001100" => rx_Data4b_conv := "1010";  -- ASCII'L'
            WHEN "01101100" => rx_Data4b_conv := "1010";  --ASCII'l'
            WHEN "01110000" => rx_Data4b_conv := "1011";  -- ASCII'P'
            WHEN "01010000" => rx_Data4b_conv := "1011";  -- ASCII'P
            WHEN OTHERS => rx_Data4b_conv := "1110";      -- This is an unused bit combination which is beyond the range we use for the fsm input checks

        END CASE;
        test <= rx_Data4b_conv;
    END PROCESS;
    
    convert_8bit: PROCESS(byte) --process converts the 8bit ascII data into 4 bit binary representation, A is set to "1111" as it's unused and other invalid inputs are set to "1110" which is also an unused combination
        VARIABLE byte_extracted_msd: STD_LOGIC_VECTOR(3 downto 0);
        VARIABLE byte_extracted_lsd: STD_LOGIC_VECTOR(3 downto 0);
    BEGIN
        --extract the digits from byte
        byte_extracted_msd := byte(7 downto 4);
        byte_extracted_lsd := byte(3 downto 0);
                            
            CASE byte_extracted_msd IS
                WHEN "0000" => byte_Done_msd <= "00110000";  -- ASCII '0'
                WHEN "0001" => byte_Done_msd <= "00110001";  -- ASCII '1'
                WHEN "0010" => byte_Done_msd <= "00110010";  -- ASCII '2'
                WHEN "0011" => byte_Done_msd <= "00110011";  -- ASCII '3'
                WHEN "0100" => byte_Done_msd <= "00110100";  -- ASCII '4'
                WHEN "0101" => byte_Done_msd <= "00110101";  -- ASCII '5'
                WHEN "0110" => byte_Done_msd <= "00110110";  -- ASCII '6'
                WHEN "0111" => byte_Done_msd <= "00110111";  -- ASCII '7'
                WHEN "1000" => byte_Done_msd <= "00111000";  -- ASCII '8'
                WHEN "1001" => byte_Done_msd <= "00111001";  -- ASCII '9'
                WHEN "1010" => byte_Done_msd <= "01000001";  -- ASCII 'A'
                WHEN "1011" => byte_Done_msd <= "01000010";  -- ASCII 'B'   
                WHEN "1100" => byte_Done_msd <= "01000011";  -- ASCII 'C'   
                WHEN "1101" => byte_Done_msd <= "01000100";  -- ASCII 'D'
                WHEN "1110" => byte_Done_msd <= "01000101";  -- ASCII 'E'   
                WHEN "1111" => byte_Done_msd <= "01000110";  -- ASCII 'F'
                WHEN others => byte_Done_lsd <= "00110000"; 
                
            END CASE;
            
            Case byte_extracted_lsd IS
                WHEN "0000" => byte_Done_lsd <= "00110000";  -- ASCII '0'
                WHEN "0001" => byte_Done_lsd <= "00110001";  -- ASCII '1'
                WHEN "0010" => byte_Done_lsd <= "00110010";  -- ASCII '2'
                WHEN "0011" => byte_Done_lsd <= "00110011";  -- ASCII '3'
                WHEN "0100" => byte_Done_lsd <= "00110100";  -- ASCII '4'
                WHEN "0101" => byte_Done_lsd <= "00110101";  -- ASCII '5'
                WHEN "0110" => byte_Done_lsd <= "00110110";  -- ASCII '6'
                WHEN "0111" => byte_Done_lsd <= "00110111";  -- ASCII '7'
                WHEN "1000" => byte_Done_lsd <= "00111000";  -- ASCII '8'
                WHEN "1001" => byte_Done_lsd <= "00111001";  -- ASCII '9'
                WHEN "1010" => byte_Done_lsd <= "01000001";  -- ASCII 'A'
                WHEN "1011" => byte_Done_lsd <= "01000010";  -- ASCII 'B'   
                WHEN "1100" => byte_Done_lsd <= "01000011";  -- ASCII 'C'   
                WHEN "1101" => byte_Done_lsd <= "01000100";  -- ASCII 'D'
                WHEN "1110" => byte_Done_lsd <= "01000101";  -- ASCII 'E'   
                WHEN "1111" => byte_Done_lsd <= "01000110";  -- ASCII 'F'
                WHEN others => byte_Done_lsd <= "00110000";
            END CASE;
    END PROCESS;
    
    convert_list: PROCESS(x)
        VARIABLE input_bcd_msd: STD_LOGIC_VECTOR(3 downto 0);
        VARIABLE input_bcd_lsd: STD_LOGIC_VECTOR(3 downto 0);
        
    BEGIN
         input_bcd_msd := x(7 downto 4);
         input_bcd_lsd := x(3 downto 0);  
          
            CASE input_BCD_msd IS
                WHEN "0000" => output_valuemsd <= "00110000";  -- ASCII '0'
                WHEN "0001" => output_valuemsd <= "00110001";  -- ASCII '1'
                WHEN "0010" => output_valuemsd <= "00110010";  -- ASCII '2'
                WHEN "0011" => output_valuemsd <= "00110011";  -- ASCII '3'
                WHEN "0100" => output_valuemsd <= "00110100";  -- ASCII '4'
                WHEN "0101" => output_valuemsd <= "00110101";  -- ASCII '5'
                WHEN "0110" => output_valuemsd <= "00110110";  -- ASCII '6'
                WHEN "0111" => output_valuemsd <= "00110111";  -- ASCII '7'
                WHEN "1000" => output_valuemsd <= "00111000";  -- ASCII '8'
                WHEN "1001" => output_valuemsd <= "00111001";  -- ASCII '9'               
                WHEN "1010" => output_valuemsd <= "01000001";  -- ASCII 'A'
                WHEN "1011" => output_valuemsd <= "01000010";  -- ASCII 'B'   
                WHEN "1100" => output_valuemsd <= "01000011";  -- ASCII 'C'   
                WHEN "1101" => output_valuemsd <= "01000100";  -- ASCII 'D'
                WHEN "1110" => output_valuemsd <= "01000101";  -- ASCII 'E'   
                WHEN "1111" => output_valuemsd <= "01000110";  -- ASCII 'F'
                WHEN others => output_valuemsd <= "00110000";
                
            END CASE;
            
            CASE input_BCD_lsd IS
                WHEN "0000" => output_valuelsd <= "00110000";  -- ASCII '0'
                WHEN "0001" => output_valuelsd <= "00110001";  -- ASCII '1'
                WHEN "0010" => output_valuelsd <= "00110010";  -- ASCII '2'
                WHEN "0011" => output_valuelsd <= "00110011";  -- ASCII '3'
                WHEN "0100" => output_valuelsd <= "00110100";  -- ASCII '4'
                WHEN "0101" => output_valuelsd <= "00110101";  -- ASCII '5'
                WHEN "0110" => output_valuelsd <= "00110110";  -- ASCII '6'
                WHEN "0111" => output_valuelsd <= "00110111";  -- ASCII '7'
                WHEN "1000" => output_valuelsd <= "00111000";  -- ASCII '8'
                WHEN "1001" => output_valuelsd <= "00111001";  -- ASCII '9'
                WHEN "1010" => output_valuelsd <= "01000001";  -- ASCII 'A'
                WHEN "1011" => output_valuelsd <= "01000010";  -- ASCII 'B'   
                WHEN "1100" => output_valuelsd <= "01000011";  -- ASCII 'C'   
                WHEN "1101" => output_valuelsd <= "01000100";  -- ASCII 'D'
                WHEN "1110" => output_valuelsd <= "01000101";  -- ASCII 'E'   
                WHEN "1111" => output_valuelsd <= "01000110";  -- ASCII 'F'
                WHEN others => output_valuelsd <= "00110000";
                
            END CASE;
        END PROCESS;
            
    
        
    convert_maxindex: PROCESS(maxindex_convert)
        VARIABLE index0: STD_LOGIC_VECTOR(3 downto 0);
        VARIABLE index1: STD_LOGIC_VECTOR(3 downto 0);
        VARIABLE index2: STD_LOGIC_VECTOR(3 downto 0);
    BEGIN
          index0 := maxindex_convert(0); 
          index1 := maxindex_convert(1);
          index2 := maxindex_convert(2);
            
            CASE index0 IS
                WHEN "0000" => maxindex_0 <= "00110000";  -- ASCII '0'
                WHEN "0001" => maxindex_0 <= "00110001";  -- ASCII '1'
                WHEN "0010" => maxindex_0 <= "00110010";  -- ASCII '2'
                WHEN "0011" => maxindex_0 <= "00110011";  -- ASCII '3'
                WHEN "0100" => maxindex_0 <= "00110100";  -- ASCII '4'
                WHEN "0101" => maxindex_0 <= "00110101";  -- ASCII '5'
                WHEN "0110" => maxindex_0 <= "00110110";  -- ASCII '6'
                WHEN "0111" => maxindex_0 <= "00110111";  -- ASCII '7'
                WHEN "1000" => maxindex_0 <= "00111000";  -- ASCII '8'
                WHEN "1001" => maxindex_0 <= "00111001";  -- ASCII '9'
                WHEN others => maxindex_0 <= "00110000";
            END CASE;
            
            CASE index1 IS
                WHEN "0000" => maxindex_1 <= "00110000";  -- ASCII '0'
                WHEN "0001" => maxindex_1 <= "00110001";  -- ASCII '1'
                WHEN "0010" => maxindex_1 <= "00110010";  -- ASCII '2'
                WHEN "0011" => maxindex_1 <= "00110011";  -- ASCII '3'
                WHEN "0100" => maxindex_1 <= "00110100";  -- ASCII '4'
                WHEN "0101" => maxindex_1 <= "00110101";  -- ASCII '5'
                WHEN "0110" => maxindex_1 <= "00110110";  -- ASCII '6'
                WHEN "0111" => maxindex_1 <= "00110111";  -- ASCII '7'
                WHEN "1000" => maxindex_1 <= "00111000";  -- ASCII '8'
                WHEN "1001" => maxindex_1 <= "00111001";  -- ASCII '9'
                WHEN others => maxindex_1 <= "00110000";
            END CASE;
            
            CASE index2 IS
                WHEN "0000" => maxindex_2 <= "00110000";  -- ASCII '0'
                WHEN "0001" => maxindex_2 <= "00110001";  -- ASCII '1'
                WHEN "0010" => maxindex_2 <= "00110010";  -- ASCII '2'
                WHEN "0011" => maxindex_2 <= "00110011";  -- ASCII '3'
                WHEN "0100" => maxindex_2 <= "00110100";  -- ASCII '4'
                WHEN "0101" => maxindex_2 <= "00110101";  -- ASCII '5'
                WHEN "0110" => maxindex_2 <= "00110110";  -- ASCII '6'
                WHEN "0111" => maxindex_2 <= "00110111";  -- ASCII '7'
                WHEN "1000" => maxindex_2 <= "00111000";  -- ASCII '8'
                WHEN "1001" => maxindex_2 <= "00111001";  -- ASCII '9'
                WHEN others => maxindex_2 <= "00110000";
            END CASE;
            
            
        END PROCESS;
        
--Convert rxData into 4 bit representation
    Register_1: PROCESS(clk, reset)
    BEGIN
        IF reset = '1' THEN
            reg_1 <= "0000"; -- reset back to 0 if reset high

        ELSIF clk'EVENT AND clk = '1' THEN --Take value of the 4 bit representationwhen rising edge clk
          IF R1_en = '1' THEN  --enable bit has to be true for this register to take the value of rxdata in 4 bit form
            reg_1 <= test;
          
          END IF;

        END IF;
    END PROCESS;

    Register_2: PROCESS(clk, reset)
    BEGIN
        IF reset = '1'  THEN
            reg_2 <= "0000";

        ELSIF clk'EVENT AND clk = '1' THEN
          IF R2_en = '1' THEN
            reg_2 <= test;

          END IF;

        END IF;
    END PROCESS;

    Register_3: PROCESS(clk, reset)
    BEGIN
        IF reset = '1' THEN
            reg_3 <= "0000";

        ELSIF clk'EVENT AND clk = '1' THEN
          IF R3_en = '1' THEN
            reg_3 <= test;
          
          END IF;

        END IF;
    END PROCESS;

    Regiser_dataresults: PROCESS(clk, reset)
    BEGIN
    
        IF reset = '1' THEN 
            reg_dataresults <= ("00000000","00000000","00000000","00000000","00000000","00000000","00000000");
            
        ELSIF clk'event and clk = '1' THEN 
            IF seqDone = '1' THEN 
                reg_dataresults <= dataResults; --store dataresults signal for L and P commands
                
            END IF;
        END IF;
        
    END PROCESS;

    Register_maxindex: PROCESS(clk, reset) --store maxindex signal for P command
    BEGIN 
        IF reset = '1' THEN 
        -- Reset all elements of reg_maxindex to "00000000"
            reg_maxindex <= (others => "0000");
        ELSIF clk'event and clk = '1' THEN 
            IF seqDone = '1' THEN 
            -- Update reg_maxindex with the value of maxIndex
                reg_maxindex <= maxIndex;
            END IF;
            END IF;
    END PROCESS;

    Bit_fsm: process(current_state, test, rxnow, seqDone, dataready,txdone)
    begin
        R1_en <= '0'; --assign default values for register enable bits, start, txnow and txdone
        R2_en <= '0';
        R3_en <= '0';
        start <= '0';
        txnow <= '0';
        rxdone <= '0';
        
        CASE current_state IS
            WHEN IDLE =>
                index <= 0;
                
                if rxnow'event and rxnow = '1' then
                    txData <= rxData; --print A on txdata line
                    fsm_valid <= '0';
                    txnow <= '1';
                    rxdone <= '1';
                    IF test = "1111" THEN 
                        next_state <= CHECK_A; -- Move to next state if rxdata is sending A or a
                    
                        
                    
                    elsif test = "1010"  then  
                        next_state <= L0; --If we receive L, move to L command state in FSM
                    elsif test = "1011"  then
                        next_state <= P0; --If we receive an invalid input fsm stays in IDLE
                        
                    else
                        next_state <= IDLE;
                    END IF;
                end if;
            When CHECK_A_WAIT =>
                if txdone = '1' then
                    txnow <= '0';
                    rxdone <= '0';
                    next_state <= CHECK_A;
                end if;
            WHEN CHECK_A =>
                
                if rxnow'event and rxnow = '1' then
                    fsm_valid <= '0';
                    txData <= rxData;
                    txnow <= '1';
                    rxdone <= '1';
                    IF test >= "0000" AND test <= "1001" and txdone = '1' THEN --check to see if rxdata value is valid
                        next_state <= CHECK_DIGIT1; -- Move to next state and store value
                        R1_en <= '1'; -- Enable register 1 to store valid input
                                                      
                    ELSE
                        IF test = "1111" THEN
                            next_state <= CHECK_A;
                        ELSE 
                            next_state <= IDLE;
                        END IF;
                    END IF;
                 END IF;
            When CHECK_DIGIT1_WAIT =>
                if txdone = '1' then
                    txnow <= '0';
                    rxdone <= '0';
                    next_state <= CHECK_DIGIT1;
                end if; 
            WHEN CHECK_DIGIT1 =>
                
                if rxnow'event and rxnow = '1' then
                    fsm_valid <= '0';
                    txData <= rxData;
                    txnow <= '1';
                    rxdone <= '1';
                    IF test >= "0000" AND test <= "1001" and txdone = '1' THEN 
                        next_state <= CHECK_DIGIT2; 
                        R2_en <= '1'; 
                    ELSE
                        IF test = "1111" THEN --if we receive A/a, move to CHECK_A state
                            next_state <= CHECK_A;
                        ELSE 
                            next_state <= IDLE;
                        END IF;
                    END IF;
                 END IF;
            When CHECK_DIGIT2_WAIT =>
                if txdone = '1' then
                    txnow <= '0';
                    rxdone <= '0';
                    next_state <= CHECK_DIGIT1;
                end if; 
            WHEN CHECK_DIGIT2 =>
                
                if rxnow'event and rxnow = '1' then
                    fsm_valid <= '0';
                    txData <= rxData;
                    
                    rxdone <= '1';
                    IF test >= "0000" AND test <= "1001" and txdone = '1' THEN
                        next_state <= S0; --when all valid inputs have been inputted to the system, start processing and print out bytes processed by data processor
                        R3_en <= '1';
                    ELSE
                        IF test = "1111" THEN
                            next_state <= CHECK_A;
                        ELSE 
                            next_state <= IDLE;
                        END IF;
                    END IF;
                END IF;

            
                
            When S0 =>
                fsm_valid <= '1';
                start <= '1';
                rxdone <= '0';
                txnow <= '0';
                next_state <= S1;
                
                if rxnow'event and rxnow = '1' then
                    IF test >= "0000" AND test <= "1001" THEN
                        next_state <= CHECK_A;
                    ELSE
                        next_state <= IDLE;
                    END IF;
                END IF;
                
                
            When S1 =>
                
                if dataReady'event and dataReady = '1' then --if data has byte has been processed by data processor and sent to cmd module then move to next state
                    next_state <= S2; 
                    txnow <= '1';
                    
                end if;
            When S2 =>
                if rxnow = '0' then
                    txnow <= '0';
                    next_state <= S3;
                    txData <= byte_Done_msd; --send out most significant digit of byte signal converted into 8 bit ASCII data on txdata line
                end if;
            when S3 =>
                
                if txdone = '1' then --when data has been successfully transmitted on txdata line, move to next state
                    txnow <= '1';
                    txData <= byte_Done_lsd; --send out least significant digit of byte signal converted into 8 bit ASCII data on txdata line
                    next_state <= S4;
                end if;
                
            When S4 =>
                if rxnow = '0' then
                    txnow <= '0';
                    next_state <= S5; 
                end if;
                
            When S5 =>
                
                if txdone = '1' then
                    txnow <= '1';
                    next_state <= S6;
                    txData <= "00100000";
                end if;
                
            When S6 =>
                if rxnow = '0' then
                    txnow <= '0';
                    next_state <= S7;    
                end if;
                
            when S7 =>
                txnow <= '0';
                if txdone = '1' then
                    txnow <= '1';
                    next_state <= S0; 
                    txData <= "00100000";
                end if; 
                        
             
                When L0 =>
                txnow <= '1';
               
                txData <= rxData; --output L on txdata line
                
                next_state <= L1;
                
             When L1 =>
                txNow <= '0';
                txData <= rxData;
                If txDone'event and txdone = '1' then --only move to next state when data is processed
                        next_state <= L2;
                END if;
                
             When L2 =>
                
                
                x <= reg_dataResults(index); --extract register element containing 8 bit string
                 --extract first 4 bits of this 8 bit string to be converted to ASCII and output
                next_state <= L3; -- state is for 1 clk cycle
                
             When L3 =>
                txnow <= '1'; --assert txnow high for one clk cycle 
                
                     
                txData <= output_valuemsd; --output msd of x converted into ASCII representation
                next_state <= L4;
                
            WHEN L4 =>
                txnow <= '0';
                
                txData <= output_valuemsd; 
                
                if txDone'event and txdone = '1' then
                    next_state <= L5;
                    
                end if;
                
            WHEN L5 =>
                txnow <= '1'; --assert txnow high for one clk cycle
                
                txdata <= output_valuelsd; --output lsd of x converted into ASCII representation
                
                next_state <= L6;
                
            WHEN L6 =>
                txnow <= '0';
                txdata <= output_valuelsd;
                
                if txDone'event and txdone = '1' then
                    next_state <= L7;
                    
                end if;
                
            WHEN L7 =>
            
                txnow <= '1';
                txdata <= "00100000"; --space
                
                next_state <= L8;
                
            WHEN L8 =>
            
                txnow <= '0';
                txdata <= "00100000"; --space
                
                if txDone'event and txdone = '1' then
                    next_state <= L9;
                    
                end if;
                
            WHEN L9 =>
                index <= index + 1; --increment index by 1 to extract next byte from dataresults register
                next_state <= L10; 
                
            WHEN L10 =>
                
                if index <= 6 then --check if we have printed out all bytes in register
                    next_state <= L2;
                    
                else
                    next_state <= IDLE;
                end if;
                                    
                
             When P0 =>
                txnow <= '1';
                txData <= rxdata; --print out P on txdata line
                next_state <= P1;
                
             When P1 =>
                txNow <= '0';
                txData <= rxdata; 
                
                IF txdone'event and txdone = '1' then --wait for byte to finish transferring out
                    next_state <= P2;  
                end if;
                
             When P2 =>
                
                txnow <= '0';
                x <= reg_dataresults(3); -- start conversion process of  peak byte in dataresults register
                maxindex_convert <= reg_maxindex; --start conversion process for index of peak byte
                
                  
                next_state <= P3;    
                
             When P3 =>
                txnow <= '1'; --set txnow high for one clk cycle
                
                txdata <= output_valuemsd; --output most significant digit
                
                
                            
                next_state <= P4;
                
             When P4 =>
                txNow <= '0';
                txdata <= output_valuemsd;
                
                IF txdone'event and txdone = '1' then --wait for byte to finish being sent to transmitter on txdata line
                
                    next_state <= P5;
                END IF;
                
             WHEN P5 =>
                txnow <= '1';
                txdata <= output_valuelsd; --transmit least significant digit on txdata line
                
                next_state <= P6;
                
             WHEN P6 =>
                txnow <= '0';
                txdata <= output_valuelsd; 
                
                IF txdone'event and txdone = '1' then 
                    next_state <= P7;
                END IF;
                
             WHEN P7 =>
                txnow <= '1';
                txdata <= "00100000"; --space
                
                next_state <= P8;
                
             WHEN P8 => 
                txnow <= '0';
                txdata <= "00100000"; --space
                
                IF txdone'event and txdone = '1' then 
                    next_state <= P9;
                END IF;
                
            WHEN P9 =>
                txnow <= '1';
                txdata <= maxindex_2; --send out first peak byte index digit on txdata line
                
                next_state <= P10;
                
            WHEN P10 =>
                txnow <= '0';
                txdata <= maxindex_2; --space
                
                IF txdone'event and txdone = '1' then 
                    next_state <= P10_space;
                end if;
                
            WHEN P10_space =>
               txdata <= "00100000"; --space;
               next_state <= P11;
               
            WHEN P11 =>
            
                txnow <= '1';
                txdata <= maxindex_1;
                
                next_state <= P12;
                
            WHEN P12 =>
                txnow <= '0';
                txdata <= maxindex_1;
                
                IF txdone'event and txdone = '1' then 
                    next_state <= P12_space;
                end if;
                
            WHEN P12_space =>
                txdata <= "00100000"; --space;
                next_state <= P13;   
                   
            WHEN P13 =>
                txnow <= '1';
                txdata <= maxindex_0;
                next_state <= P14;
           
           WHEN P14 =>
                txnow <= '0';
                txdata <= maxindex_0;     
                
                IF txdone'event and txdone = '1' then 
                    next_state <= IDLE;
                    txdata <= "00100000"; 
                END IF;
                    
            END CASE;
            IF seqDone'event and seqDone = '1' THEN --return to IDLE state if seqDone is set high
                next_state <= IDLE;
              
            
            END IF;
        end process;


-- Sequential process with reset
    seq_state: PROCESS(clk, reset)
    BEGIN 
        IF reset = '1' THEN
            current_state <= IDLE; -- when reset is high, move fsm back to IDLE state
            
        ELSIF clk'event and clk = '1' THEN
            --current_state <= next_state;
            --data ready sent by rx module (means new data is coming in)
                current_state <= next_state;
                
            END IF;
        
    END PROCESS;

    
    process(clk,reset) --process to store the 4 bit register 1,2,3 values into BCD number on numwords line
    begin
 
        IF reset = '1'  THEN --reset numwords signal
            numWords_bcd(0) <= "0000";
            numWords_bcd(1) <= "0000";
            numWords_bcd(2) <= "0000";

        ELSIF clk'event and clk = '1' THEN -- assign Numwords signal with the stored values from the registers
          IF fsm_valid = '1' THEN -- if the fsm has completed successfully then numwords_BCD is updated with input values stored in R1,R2 and R3
             numWords_bcd(0) <= reg_3;
             numWords_bcd(1) <= reg_2;
             numWords_bcd(2) <= reg_1;


             end if;

        end if;
        
    end process;
        

    
   
end Behavioral;