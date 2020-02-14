library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cache is
generic(
	ram_size : INTEGER := 32768;
);
port(
	clock : in std_logic;
	reset : in std_logic;
	
	-- Avalon interface --
	s_addr : in std_logic_vector (31 downto 0);
	s_read : in std_logic;
	s_readdata : out std_logic_vector (31 downto 0);
	s_write : in std_logic;
	s_writedata : in std_logic_vector (31 downto 0);
	s_waitrequest : out std_logic; 
    
	m_addr : out integer range 0 to ram_size-1;
	m_read : out std_logic;
	m_readdata : in std_logic_vector (7 downto 0);
	m_write : out std_logic;
	m_writedata : out std_logic_vector (7 downto 0);
	m_waitrequest : in std_logic
);
end cache;

architecture arch of cache is

-- Address Portions: 
-- 4 words per block (128 / 32)  -> 2 bits for offset
-- 32 blocks in the memory (4096 / 128) -> 5 bits for index
-- 32 bit addresses -> 32 - 5 - 2 = 25 bits for tag

-- Write-back policy only updates external memory when a line 
-- in the cache is cleaned or replaced with a new line.
-- Define the possible states for a write-back cache:

type state_type is (initial, _read, _write, _memread, _memwrite, _memwait, _writeback);
signal state : state_type;
signal _next : state_type;
type cache_def is array (0 to 31) of std_logic_vector (154 downto 0);
signal cache: state_type;

begin
-- Set up the clock for transition between states
process (clock, rst)
begin
	if rst = '1' then
		state <= start;
	elsif (clock'event and clock = '1') then
		state <= next_state;
	end if;

end process;

process(s_read, s_write, m_waitrequest, state)
	variable word_offset: INTEGER;
	variable block_index: INTEGER;
	variable delta: INTEGER:= word_offset
	variable count: INTEGER := 0;
	variable address: std_logic_vector (14 downto 0);
begin
	word_offset := to_integer(unsigned(s_addr(1 downto 0));
	block_index:= to_integer(unsigned(s_addr(6 downto 2));
	delta := word_offset - 1;

	-- Defining a Moore state machine for the Cache Operations
	case state is
		when initial =>

		when _read =>
		
		when _write =>

		when _memread =>

		when _memwrite =>

		when _memwait =>

		when _writeback =>
	end case;
end process;


end arch;