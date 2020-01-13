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

TYPE State_Type is (state_0, state_1, state_2, state_3, state_4, state_5, state_6, state_7);
signal state : State_Type;

begin

-- Insert your processes here
process (clk, reset)
begin
	-- Resets the state machine
	if ( reset = '1') then
		state <= state_0;

	elsif rising_edge(clk) then
		case state is
			-- If we have a '/' then we have a potential comment line incoming - move on to state 1 
			when state_0 =>
				output <= '0';
				if input = SLASH_CHARACTER then
					state <= state_1; 
				else
					state <= state_0;
				end if;

			-- If we have a '/' followed by '/' or '*' then it is a beginning of a comment - move on to the appropriate state
			when state_1 =>
				output <= '0';
				if input = SLASH_CHARACTER then
					state <= state_2;
				elsif input = STAR_CHARACTER then
					state <= state_3;
				else
					state <= state_0;
				end if;

			when state_2 =>
				output <= '0';
				if input = NEW_LINE_CHARACTER then
					state <= state_7;
				else
					state <= state_4;
				end if;
	
			when state_3 =>
				output <= '0';
				if input = STAR_CHARACTER then
					state <= state_6;
				else
					state <= state_5;
				end if;
	
			when state_4 =>
				output <= '1';
				if input = NEW_LINE_CHARACTER then
					state <= state_7;
				else
					state <= state_4;
				end if;
	
			when state_5 =>
				output <= '1';
				if input = STAR_CHARACTER then 
					state <= state_6;
				else
					state <= state_5;
				end if;
	
			when state_6 =>
				output <= '1';
				if input = SLASH_CHARACTER then
					state <= state_7;
				else
					state <= state_5;
				end if;
	
			when state_7 =>
				output <= '1';
				state <= state_0;
		end case;
	end if;			
end process;

end behavioral;