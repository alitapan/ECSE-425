-- ECSE425 - Assignment 1
-- Ali Tapan - 260556540 

LIBRARY ieee;
USE ieee.STD_LOGIC_1164.all;

ENTITY fsm_tb IS
END fsm_tb;

ARCHITECTURE behaviour OF fsm_tb IS

COMPONENT comments_fsm IS
PORT (clk : in std_logic;
      reset : in std_logic;
      input : in std_logic_vector(7 downto 0);
      output : out std_logic
  );
END COMPONENT;

--The input signals with their initial values
SIGNAL clk, s_reset, s_output: STD_LOGIC := '0';
SIGNAL s_input: std_logic_vector(7 downto 0) := (others => '0');

CONSTANT clk_period : time := 1 ns;
CONSTANT SLASH_CHARACTER : std_logic_vector(7 downto 0) := "00101111";
CONSTANT STAR_CHARACTER : std_logic_vector(7 downto 0) := "00101010";
CONSTANT NEW_LINE_CHARACTER : std_logic_vector(7 downto 0) := "00001010";

BEGIN
dut: comments_fsm
PORT MAP(clk, s_reset, s_input, s_output);

 --clock process
clk_process : PROCESS
BEGIN
	clk <= '0';
	WAIT FOR clk_period/2;
	clk <= '1';
	WAIT FOR clk_period/2;
END PROCESS;

--TODO: Thoroughly test your FSM
stim_process: PROCESS
BEGIN
    	REPORT "Example case, reading a meaningless character";
	s_input <= "01011000";
	WAIT FOR 1 * clk_period;
	ASSERT (s_output = '0') REPORT "When reading a meaningless character, the output should be '0'" SEVERITY ERROR;
	REPORT "_______________________";

   	 -- Reset the state machine
	s_reset <= '1';
	WAIT FOR 1 * clk_period;
	s_reset <= '0';
	WAIT FOR 1 * clk_period;

    	-- Test case 1: Double slash + comment + new line character + code
    	REPORT "Test 1: // test \n test";
    	s_input <= SLASH_CHARACTER;
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading a slash character for the first time, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= SLASH_CHARACTER;
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading a slash character for a second time consecutively, the output should still be '0'" SEVERITY ERROR;
	
    	s_input <= "01110100";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '1') REPORT "When reading characters after double slashes, the output should be '1'" SEVERITY ERROR;
	
    	s_input <= "01100101";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '1') REPORT "When reading characters after double slashes, the output should be '1'" SEVERITY ERROR;
	
    	s_input <= "01110011";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '1') REPORT "When reading characters after double slashes, the output should be '1'" SEVERITY ERROR;
	
    	s_input <= "01110100";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '1') REPORT "When reading characters after double slashes, the output should be '1'" SEVERITY ERROR;
	
    	s_input <= NEW_LINE_CHARACTER;
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '1') REPORT "When reading a new line character after double slashes, the output should be '1'" SEVERITY ERROR;
	
    	s_input <= "01110100";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters after new line character, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01100101";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters after new line character, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01110011";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters after new line character, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01110100";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters after new line character, the output should be '0'" SEVERITY ERROR;
	
    	REPORT "__________________";
	
   	 -- Reset the state machine
	s_reset <= '1';
	WAIT FOR 1 * clk_period;
	s_reset <= '0';
	WAIT FOR 1 * clk_period;
	
    	-- Test case 2: Single slash + star + comment + star + slash + code
    	REPORT "Test 1: /* test */ test";
    	s_input <= SLASH_CHARACTER;
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading a slash character for the first time, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= STAR_CHARACTER;
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading a star character after a slash character, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01110100";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '1') REPORT "When reading characters after a slash and a star, the output should be '1'" SEVERITY ERROR;
	
    	s_input <= "01100101";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '1') REPORT "When reading characters after a slash and a star, the output should be '1'" SEVERITY ERROR;
	
    	s_input <= "01110011";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '1') REPORT "When reading characters after a slash and a star, the output should be '1'" SEVERITY ERROR;
	
    	s_input <= "01110100";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '1') REPORT "When reading characters after a slash and a star, the output should be '1'" SEVERITY ERROR;
	
    	s_input <= STAR_CHARACTER;
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '1') REPORT "When reading a star character after a slash and a star, the output should be '1'" SEVERITY ERROR;
	
    	s_input <= SLASH_CHARACTER;
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '1') REPORT "When reading a slash character after a closing star, the output should be '1'" SEVERITY ERROR;
	
    	s_input <= "01110100";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters after closing star and slash, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01100101";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters after closing star and slash, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01110011";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters after closing star and slash, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01110100";
   	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters after closing star and slash, the output should be '0'" SEVERITY ERROR;
	
    	REPORT "__________________";
	
   	 -- Reset the state machine
	s_reset <= '1';
	WAIT FOR 1 * clk_period;
	s_reset <= '0';
	WAIT FOR 1 * clk_period;
	
    	 -- Test case 3: slash + code
    	REPORT "Test 3: /test";
    	s_input <= SLASH_CHARACTER;
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading a slash character for the first time, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01110100";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters while comment characters are not specified, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01100101";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters while comment characters are not specified, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01110011";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters while comment characters are not specified, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01110100";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters while comment characters are not specified, the output should be '0'" SEVERITY ERROR;
	
    	REPORT "__________________";
	
   	 -- Reset the state machine
	s_reset <= '1';
	WAIT FOR 1 * clk_period;
	s_reset <= '0';
	WAIT FOR 1 * clk_period;

    	-- Test case 4: code + star + slash + code
    	REPORT "Test 4: test */ test";
    	s_input <= "01110100";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters while comment characters are not specified, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01100101";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters while comment characters are not specified, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01110011";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters while comment characters are not specified, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01110100";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters while comment characters are not specified, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= STAR_CHARACTER;
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading a star character while comment characters are not specified, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= SLASH_CHARACTER;
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading a slash character for the first time, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01110100";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters while comment characters are not specified, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01100101";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters while comment characters are not specified, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01110011";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters while comment characters are not specified, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01110100";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters while comment characters are not specified, the output should be '0'" SEVERITY ERROR;
	
    	REPORT "__________________";
	
   	 -- Reset the state machine
	s_reset <= '1';
	WAIT FOR 1 * clk_period;
	s_reset <= '0';
	WAIT FOR 1 * clk_period;
	
    	 -- Test case 5: slash + star + star + slash + code
    	REPORT "Test 5: /**/ test";
    	s_input <= SLASH_CHARACTER;
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading a slash character for the first time, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= STAR_CHARACTER;
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading a star character after a slash character, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= STAR_CHARACTER;
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '1') REPORT "When reading a star character after a slash and a star, the output should be '1'" SEVERITY ERROR;
	
    	s_input <= SLASH_CHARACTER;
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '1') REPORT "When reading a slash character after a closing star, the output should be '1'" SEVERITY ERROR;
	
    	s_input <= "01110100";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters after closing star and slash, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01100101";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters after closing star and slash, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01110011";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters after closing star and slash, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01110100";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters after closing star and slash, the output should be '0'" SEVERITY ERROR;
	
    	REPORT "__________________";
	
   	 -- Reset the state machine
	s_reset <= '1';
	WAIT FOR 1 * clk_period;
	s_reset <= '0';
	WAIT FOR 1 * clk_period;

    	-- Test case 6: slash + slash + new line character + code
    	REPORT "Test 6: //\n test";
    	s_input <= SLASH_CHARACTER;
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading a slash character for the first time, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= SLASH_CHARACTER;
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading a slash character for a second time consecutively, the output should still be '0'" SEVERITY ERROR;
	
    	s_input <= NEW_LINE_CHARACTER;
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '1') REPORT "When reading a new line character after double slashes, the output should be '1'" SEVERITY ERROR;
	
    	s_input <= "01110100";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters after new line character, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01100101";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters after new line character, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01110011";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters after new line character, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01110100";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters after new line character, the output should be '0'" SEVERITY ERROR;
	
   	 -- Reset the state machine
	s_reset <= '1';
	WAIT FOR 1 * clk_period;
	s_reset <= '0';
	WAIT FOR 1 * clk_period;

    	REPORT "__________________";
	
    	-- Test case 7: code + new line character + code
    	REPORT "Test 7: test \n test";
    	s_input <= "01110100";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters while comment characters are not specified, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01100101";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters while comment characters are not specified, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01110011";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters while comment characters are not specified, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01110100";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters while comment characters are not specified, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= NEW_LINE_CHARACTER;
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading a new line character while comment characters are not specified, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01110100";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters while comment characters are not specified, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01100101";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters while comment characters are not specified, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01110011";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters while comment characters are not specified, the output should be '0'" SEVERITY ERROR;
	
    	s_input <= "01110100";
    	WAIT FOR 1 * clk_period;
    	ASSERT (s_output = '0') REPORT "When reading characters while comment characters are not specified, the output should be '0'" SEVERITY ERROR;
	
    	REPORT "__________________";

	WAIT;
END PROCESS stim_process;
END;
