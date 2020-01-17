-- ECSE 425 - Assignment 2
-- Ali Tapan - 260556540

LIBRARY ieee;
USE ieee.STD_LOGIC_1164.all;
use ieee.numeric_std.all;

ENTITY pipeline_tb IS
END pipeline_tb;

ARCHITECTURE behaviour OF pipeline_tb IS

COMPONENT pipeline IS
port (clk : in std_logic;
      a, b, c, d, e : in integer;
      op1, op2, op3, op4, op5, final_output : out integer
  );
END COMPONENT;

--The input signals with their initial values
SIGNAL clk: STD_LOGIC := '0';
SIGNAL s_a, s_b, s_c, s_d, s_e : INTEGER := 0;
SIGNAL s_op1, s_op2, s_op3, s_op4, s_op5, s_final_output : INTEGER := 0;

CONSTANT clk_period : time := 1 ns;

BEGIN
dut: pipeline
PORT MAP(clk, s_a, s_b, s_c, s_d, s_e, s_op1, s_op2, s_op3, s_op4, s_op5, s_final_output);

 --clock process
clk_process : PROCESS
BEGIN
	clk <= '0';
	WAIT FOR clk_period/2;
	clk <= '1';
	WAIT FOR clk_period/2;
END PROCESS;
 

stim_process: PROCESS
BEGIN   
	-- Simulate the inputs for the pipelined equation ((a + b) * 42) - (c * d * (a - e)) and assert the results
	-- s_op1 = a + b
	-- s_op2 = s_op1 * 42
	-- s_op3 = c * d
	-- s_op4 = a - e
	-- s_op5 = s_op4 * s_op3
	-- s_final_output = s_op2 - s_op5

	-- **************  TEST 1 ************** --
	REPORT "Test Case 1: Zero Case with a = 0, b = 0, c = 0, d = 0, e = 0  ----> expected output is 0";
	s_a <= 0;
	s_b <= 0;
	s_c <= 0;
	s_d <= 0;
	s_e <= 0;

	WAIT FOR 3 * clk_period;

	ASSERT (s_op1 = 0) REPORT "op1 should be equal to '0', test result = " & integer'image(s_op1) SEVERITY ERROR;
	ASSERT (s_op2 = 0) REPORT "op2 should be equal to '0', test result = " & integer'image(s_op2) SEVERITY ERROR;
	ASSERT (s_op3 = 0) REPORT "op3 should be equal to '0', test result = " & integer'image(s_op3) SEVERITY ERROR;
	ASSERT (s_op4 = 0) REPORT "op4 should be equal to '0', test result = " & integer'image(s_op4) SEVERITY ERROR;
	ASSERT (s_op5 = 0) REPORT "op5 should be equal to '0', test result = " & integer'image(s_op5) SEVERITY ERROR;
	ASSERT (s_final_output = 0) REPORT "final_output should be equal to '0', test result = " & integer'image(s_final_output) SEVERITY ERROR;
	
	-- Let the pipeline clear up

	WAIT FOR 3 * clk_period;
	REPORT "_________________";

	-- **************  TEST 2 ************** --
	REPORT "Test Case 2: Full Case with a = 5, b = 4, c = 2, d = 7, e = 10 ----> expected output is 448";
	s_a <= 5;
	s_b <= 4;
	s_c <= 2;
	s_d <= 7;
	s_e <= 10;

	WAIT FOR 3 * clk_period;

	ASSERT (s_op1 = 9) REPORT "op1 should be equal to '9', test result = " & integer'image(s_op1) SEVERITY ERROR;
	ASSERT (s_op2 = 378) REPORT "op2 should be equal to '378', test result = " & integer'image(s_op2) SEVERITY ERROR;
	ASSERT (s_op3 = 14) REPORT "op3 should be equal to '14', test result = " & integer'image(s_op3) SEVERITY ERROR;
	ASSERT (s_op4 = -5) REPORT "op4 should be equal to '0', test result = " & integer'image(s_op4) SEVERITY ERROR;
	ASSERT (s_op5 = -70) REPORT "op5 should be equal to '0', test result = " & integer'image(s_op5) SEVERITY ERROR;
	ASSERT (s_final_output = 448) REPORT "final_output should be equal to '448', test result = " & integer'image(s_final_output) SEVERITY ERROR;
	
	REPORT "_________________";

	-- **************  TEST 3 ************** --
	REPORT "Test Case 3: 1 Clock Cycle with a = 2, b = 5, c = 7, d = 3, e = 4";
	-- Empty the pipeline first
	s_a <= 0;
	s_b <= 0;
	s_c <= 0;
	s_d <= 0;
	s_e <= 0;

	WAIT FOR 3 * clk_period;
	
	-- Then set the inputs
	s_a <= 2;
	s_b <= 5;
	s_c <= 7;
	s_d <= 3;
	s_e <= 4;

	WAIT FOR 1 * clk_period;
	
	-- Since only 1 clock cycle has elapsed s_op2, s_op5 and s_final_output should be zero
	ASSERT (s_op1 = 7) REPORT "op1 should be equal to '7', test result = " & integer'image(s_op1) SEVERITY ERROR;
	ASSERT (s_op2 = 0) REPORT "op2 should be equal to '0', test result = " & integer'image(s_op2) SEVERITY ERROR;
	ASSERT (s_op3 = 21) REPORT "op3 should be equal to '21', test result = " & integer'image(s_op3) SEVERITY ERROR;
	ASSERT (s_op4 = -2) REPORT "op4 should be equal to '-2', test result = " & integer'image(s_op4) SEVERITY ERROR;
	ASSERT (s_op5 = 0) REPORT "op5 should be equal to '0', test result = " & integer'image(s_op5) SEVERITY ERROR;
	ASSERT (s_final_output = 0) REPORT "final_output should be equal to '0', test result = " & integer'image(s_final_output) SEVERITY ERROR;

	WAIT FOR 3 * clk_period;
	REPORT "_________________";
	
	-- **************  TEST 4 ************** --
	REPORT "Test Case 4: 2 Clock Cycles with a = 2, b = 5, c = 7, d = 3, e = 4";
	-- Empty the pipeline first
	s_a <= 0;
	s_b <= 0;
	s_c <= 0;
	s_d <= 0;
	s_e <= 0;

	WAIT FOR 3 * clk_period;
	
	-- Then set the inputs
	s_a <= 2;
	s_b <= 5;
	s_c <= 7;
	s_d <= 3;
	s_e <= 4;

	WAIT FOR 2 * clk_period;
	
	ASSERT (s_op1 = 7) REPORT "op1 should be equal to '7', test result = " & integer'image(s_op1) SEVERITY ERROR;
	ASSERT (s_op2 = 294) REPORT "op2 should be equal to '294', test result = " & integer'image(s_op2) SEVERITY ERROR;
	ASSERT (s_op3 = 21) REPORT "op3 should be equal to '21', test result = " & integer'image(s_op3) SEVERITY ERROR;
	ASSERT (s_op4 = -2) REPORT "op4 should be equal to '-2', test result = " & integer'image(s_op4) SEVERITY ERROR;
	ASSERT (s_op5 = -42) REPORT "op5 should be equal to '0', test result = " & integer'image(s_op5) SEVERITY ERROR;
	ASSERT (s_final_output = 336) REPORT "final_output should be equal to '336', test result = " & integer'image(s_final_output) SEVERITY ERROR;

	WAIT FOR 3 * clk_period;
	REPORT "_________________";

	-- **************  TEST 5 ************** --
	REPORT "Test Case 5: 1 Clock Cycle with a = 1, b = 4, c = 2, d = 10, e = 10 and then switch the inputs to a = 2, b = 4, c = 9, d = 2, e = 100 the following clock cycle.";
	-- Empty the pipeline first
	s_a <= 0;
	s_b <= 0;
	s_c <= 0;
	s_d <= 0;
	s_e <= 0;

	WAIT FOR 3 * clk_period;

	-- Then set the inputs
	s_a <= 1;
	s_b <= 4;
	s_c <= 2;
	s_d <= 10;
	s_e <= 10;
	
	WAIT FOR 1 * clk_period;

	-- Since only 1 clock cycle has elapsed s_op2, s_op5 and s_final_output should be zero
	ASSERT (s_op1 = 5) REPORT "op1 should be equal to '5', test result = " & integer'image(s_op1) SEVERITY ERROR;
	ASSERT (s_op2 = 0) REPORT "op2 should be equal to '0', test result = " & integer'image(s_op2) SEVERITY ERROR;
	ASSERT (s_op3 = 20) REPORT "op3 should be equal to '20', test result = " & integer'image(s_op3) SEVERITY ERROR;
	ASSERT (s_op4 = -9) REPORT "op4 should be equal to '-9', test result = " & integer'image(s_op4) SEVERITY ERROR;
	ASSERT (s_op5 = 0) REPORT "op5 should be equal to '0', test result = " & integer'image(s_op5) SEVERITY ERROR;
	ASSERT (s_final_output = 0) REPORT "final_output should be equal to '0', test result = " & integer'image(s_final_output) SEVERITY ERROR;

	-- Reset the inputs to the new ones
	s_a <= 2;
	s_b <= 4;
	s_c <= 9;
	s_d <= 2;
	s_e <= 100;
	
	-- Re-Assert the outputs just to check
	ASSERT (s_op1 = 5) REPORT "op1 should be equal to '5', test result = " & integer'image(s_op1) SEVERITY ERROR;
	ASSERT (s_op2 = 0) REPORT "op2 should be equal to x, test result = " & integer'image(s_op2) SEVERITY ERROR;
	ASSERT (s_op3 = 20) REPORT "op3 should be equal to '20', test result = " & integer'image(s_op3) SEVERITY ERROR;
	ASSERT (s_op4 = -9) REPORT "op4 should be equal to x, test result = " & integer'image(s_op4) SEVERITY ERROR;
	ASSERT (s_op5 = 0) REPORT "op5 should be equal to x, test result = " & integer'image(s_op5) SEVERITY ERROR;
	ASSERT (s_final_output = 0) REPORT "final_output should be equal to '0', test result = " & integer'image(s_final_output) SEVERITY ERROR;

	WAIT FOR 1 * clk_period;

	-- Now the outputs s_op2, s_op5 and s_final_output should be equal to 210, -180 and 390 respectively which is from the previous inputs.
	-- Outputs s_op1, s_op3, s_op4 should change with respect to the new inputs we provided.
	ASSERT (s_op1 = 6) REPORT "op1 should be equal to '5', test result = " & integer'image(s_op1) SEVERITY ERROR;
	ASSERT (s_op2 = 210) REPORT "op2 should be equal to '210', test result = " & integer'image(s_op2) SEVERITY ERROR;
	ASSERT (s_op3 = 18) REPORT "op3 should be equal to '18', test result = " & integer'image(s_op3) SEVERITY ERROR;
	ASSERT (s_op4 = -98) REPORT "op4 should be equal to '-98', test result = " & integer'image(s_op4) SEVERITY ERROR;
	ASSERT (s_op5 = -180) REPORT "op5 should be equal to '-180', test result = " & integer'image(s_op5) SEVERITY ERROR;
	ASSERT (s_final_output = 390) REPORT "final_output should be equal to '336', test result = " & integer'image(s_final_output) SEVERITY ERROR;
	
	WAIT FOR 1 * clk_period;

	-- Now the outputs should reflect only the new inputs we provided.
	ASSERT (s_op1 = 6) REPORT "op1 should be equal to '5', test result = " & integer'image(s_op1) SEVERITY ERROR;
	ASSERT (s_op2 = 252) REPORT "op2 should be equal to '252', test result = " & integer'image(s_op2) SEVERITY ERROR;
	ASSERT (s_op3 = 18) REPORT "op3 should be equal to '18', test result = " & integer'image(s_op3) SEVERITY ERROR;
	ASSERT (s_op4 = -98) REPORT "op4 should be equal to '-98', test result = " & integer'image(s_op4) SEVERITY ERROR;
	ASSERT (s_op5 = -1764) REPORT "op5 should be equal to '-1764', test result = " & integer'image(s_op5) SEVERITY ERROR;
	ASSERT (s_final_output = 2016) REPORT "final_output should be equal to '2016', test result = " & integer'image(s_final_output) SEVERITY ERROR;

	REPORT "_________________";

	WAIT;
END PROCESS stim_process;
END;
