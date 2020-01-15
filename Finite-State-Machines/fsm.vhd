-- ECSE425 - Assignment 1
-- Ali Tapan - 260556540 

library ieee;
use IEEE.std_logic_1164.all;
use ieee.numeric_std.all;

-- Do not modify the port map of this structure
entity comments_fsm is
port (clk : in std_logic;
      reset : in std_logic;
      input : in std_logic_vector(7 downto 0);
      output : out std_logic
  );
end comments_fsm;

architecture behavioral of comments_fsm is

-- The ASCII value for the '/', '*' and end-of-line characters
constant SLASH_CHARACTER : std_logic_vector(7 downto 0) := "00101111";
constant STAR_CHARACTER : std_logic_vector(7 downto 0) := "00101010";
constant NEW_LINE_CHARACTER : std_logic_vector(7 downto 0) := "00001010";

-- Initial decleration of the states
TYPE State_Type is (state_0, state_1, state_2, state_3, state_4, state_5, state_6, state_7);
signal state : State_Type;

begin

process (clk, reset)
begin
	-- Resets the state machine
	if ( reset = '1') then
		state <= state_0;

	elsif rising_edge(clk) then
		case state is
			-- If we have a '/' then we have a potential comment line incoming - move on to state 1 
			when state_0 =>
				if input = SLASH_CHARACTER then
					state <= state_1; 
				else
					state <= state_0;
				end if;

			-- If we have a '/' followed by '/' or '*' then it is a beginning of a comment - move on to the appropriate state
			when state_1 =>
				if input = SLASH_CHARACTER then
					state <= state_2;
				elsif input = STAR_CHARACTER then
					state <= state_3;
				else
					state <= state_0;
				end if;

			-- If we have a '/n' followed by '//' the comment is over - move on to the appropriate state
			-- If we don't have a '/n' followed by '//' there are comment characters to be read - move on to the appropriate state
			when state_2 =>
				if input = NEW_LINE_CHARACTER then
					state <= state_7;
				else
					state <= state_4;
				end if;

			-- If we have a '*' followed by '/*' the comment MAY be over - move on to the appropriate state
			-- If we don't have '*' character followed by '/*' there are comment characters to be read - move on to the appropriate state
			when state_3 =>
				if input = STAR_CHARACTER then
					state <= state_6;
				else
					state <= state_5;
				end if;

			-- Read the comment characters until '/n' appears - move on to state 7 when '/n' appears
			when state_4 =>
				if input = NEW_LINE_CHARACTER then
					state <= state_7;
				else
					state <= state_4;
				end if;

			-- Read the comment characters until '*' appears - move on to state 6 when '*' appears
			when state_5 =>
				if input = STAR_CHARACTER then 
					state <= state_6;
				else
					state <= state_5;
				end if;

			-- If we have a '/' after a '*' then the comment is over - move on to state 7
			-- If not go back to state 5 and keep reading the comment characters
			when state_6 =>
				if input = SLASH_CHARACTER then
					state <= state_7;
				else
					state <= state_5;
				end if;

			-- End of comment - move back to begginig state, state 0
			when state_7 =>
				state <= state_0;
		end case;
	end if;			
end process;

process(state)
	begin
		-- Output assignments for each state is as follows - Moore Machine!
		case state is
			when state_0 => output <= '0';
			when state_1 => output <= '0';
			when state_2 => output <= '0';
			when state_3 => output <= '0';
			when state_4 => output <= '1';
			when state_5 => output <= '1';
			when state_6 => output <= '1';
			when state_7 => output <= '1';
		end case;
	end process;

end behavioral;