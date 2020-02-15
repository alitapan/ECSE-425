library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cache is
generic(
	ram_size : INTEGER := 32768
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
-- Defining the possible states for a write-back cache:

type state_type is (initial, _write, _read, memory_write, memory_read, memory_wait, writeback);
signal state : state_type;
signal _next : state_type;
type cache_def is array (0 to 31) of std_logic_vector (154 downto 0);
signal _cache: state_type;

begin
-- Set up the clock for transition between states
process (clock, rst)
begin
	if rst = '1' then
		-- Reset
		state <= initial;
	elsif (clock'event and clock = '1') then
		-- Normal flow
		state <= _next;
	end if;

end process;

process(s_read, s_write, m_waitrequest, state)

	variable c: INTEGER := 0; -- Word counter within the block
	variable word_offset: INTEGER;
	variable block_index: INTEGER;

	-- Cache should simply use the lower 15 bits of the address and ignore the rest since
	-- the main memory here has only 2^15 bytes (32768 bytes).
	variable address: std_logic_vector (14 downto 0);

	variable delta: INTEGER:= word_offset - 1;


begin
	-- 2 bits for offset
	word_offset := to_integer(unsigned(s_addr(1 downto 0));
	delta := word_offset - 1;
	-- 5 bits for index
	block_index:= to_integer(unsigned(s_addr(6 downto 2));
	
	-- Defining a Moore machine for the Cache Operations
	case state is

		-- The initial state of the state machine, check what kind of operation is requested (read or write)
		when initial =>
			s_waitrequest <= '1';
			-- Check if there is a write operation
			if s_write = '1' then 
				_next <= _write;
			-- Check if there is a read operation
			elsif s_read = '1' then
				_next <= _read;
			-- If no operation is specificed, stay in the initial state
			else
				_next <= initial;
			end if;

		-- The operation requested is write
		when _write=>
			-- Check for dirty and valid bits before writing, if it is then move to the writeback state
			if _cache(block_index)(153) = '1'  and _next /= initial 
			and (_cache(block_index)(154) or _cache(block_index)(152 downto 128) /= s_addr (31 downto 7)) then
				_next <= writeback;

			-- If there is no present dirty bit and valid bit present, set it to 1 since we are doing a write operation
			else
				-- Set dirt and valid bits to 1
				_cache(block_index)(153) <= '1';
				_cache(block_index)(154) <= '1';
				-- Write
				_cache(block_index)(127 downto 0)((word_offset*32)) - 1 downto 32*delta) <= s_writedata;
				_cache(index)(152 downto 128) <= s_addr(31 downto 7);
				s_waitrequest <= '0';
				-- Once write operation is done move back to the initial state to wait for the next operation
				_next <= initial;
			end if;

		-- The operation requested is read
		when _read =>
			-- Check if the tags are matching and the valid bit is 1, if its then its a hit in the cache
			if _cache(block_index)(152 downto 128) = s_addr(31 downto 7) and _cache(block_index)(154) = '1' then
				-- Read the requested data by indexing though the block and the word
				s_readdate <= _cache(block_index)(127 downto 0)((word_offset*32) - 1 downto 32*delta);
				s_waitrequest <= '0';
				-- Once read operation is done move back to the initial state to wait for the next operation
				_next <= initial;
			-- If the requested data is dirty and does not exist in the cache (miss) we have to writeback
			elsif _cache(block_index)(153) = '1' then
				_next <= memory_write;
			-- If the requested data is not dirty but does not exist in the cache (miss) we have to bring
			-- it in from the memory
			elsif _cache(block_index)(153) = '0' or _cache(block_index)(153) = 'U' then
				_next <= memory_read;
			-- Reading is not done, keep reading
			else
				_next <= _read;
			end if;

		-- Reading from the main memory
		when memory_read =>
			if m_waitrequest = '1' then 
				-- Since we're only reading the lower 15 bits
				m_addr <= to_integer(unsigned(s_addr(14 downto 0))) + c;
				m_read <= '1';
				m_write <= '0';
				-- Wait until the read is processed, since main memory is slower than cache
				_next <= memory_wait;
			else
				_next <= memory_read;
			end if;

		-- Writing to the main memory
		when memory_write =>
			-- Evict the current habitant at the requested address
			if c < 4 and m_waitrequest = '1' and _next /= memory_read then
				address := _cache(block_index)(135 downto 128) & s_addr (6 downto 0);
				m_addr <= to_integer(unsigned (address)) + c;
				m_write <= '1';
				m_read <= '0';
				m_writedata <= _cache(block_index)(127 downto 0)((c * 8) + 7 + 32*delta downto (c*8) + 32*delta);
				c := c + 1;
				_next <= memomry_write;
			-- Read from memory 
			elsif count = 4 then
				count:= 0;
				_next <= memory_read;
			else
				m_write = '0';
				_next <= memory_write;
			end if;

		-- Wait
		when memory_wait => 
			if c < 3 and m_waitrequest = '0' then
				_cache(block_index)(127 downto 0)((c * 8) + 7 + 32*delta downto (c*8) + 32*delta) <= m_readdate;
				c := c + 1;
				m_read <= '0';
				_next <= memory_read;
			elsif c = 3 and m_waitrequest = '0' then
				_cache(block_index)(127 downto 0)((c*8) + 7 + 32*delta downto (c*8) + 32*delta) <= m_readdate;
				c := c + 1;
				m_read <= '0';
				_next <= memory_wait;
			elsif c = 4 then
				s_readdate <= _cache(block_index)(127 downto 0)((word_offset * 32) - 1 downto 32*delta);
				-- Set Tag
				_cache(block_index)(152 downto 128) <= s_addr (31 downto 7);
				-- Set dirt bit to 0 and valid bit to 1
				_cache(block_index)(153) <= '0';
				_cache(block_index)(154) <= '1';
				m_read <= '0';
				m_write <= '0';
				s_waitrequest <= '0';
				-- Reset the word counter
				count := 0;
				-- Move back to the initial state to wait for the next operation
				_next <= initial;
			else
				_next <= memory_wait;
			end if;
	
		-- Write back operation for the cache
		when writeback =>
			-- Evict the current habitant at the requested address
			if c < 4 and m_waitrequest = '1' then
				address := _cache(block_index)(135 downto 128) & s_addr (6 downto 0);
				m_addr <= to_integer(unsigned (address)) + c;
				m_write <= '1';
				m_read <= '0';
				m_writedata <= _cache(block_index)(127 downto 0)((c * 8) + 7 + 32*delta downto (c*8) + 32*delta);
				c := c + 1;
				_next <= writeback;
			-- Write to the cache
			elsif c = 4 then 
				-- Write
				_cache(block_index)(127 downto 0)((block_offset * 32) - 1 downto 32*delta) <= s_writedata (31 downto 0);
				-- Set Tag
				_cache(block_index)(152 downto 128) <= s_addr (31 downto 7);
				-- Set dirt and valid bits to 1
				_cache(block_index)(153) <= '1';
				_cache(block_index)(154) <= '1';
				-- Reset the word counter
				count := 0;

				s_waitrequest <= '0';
				m_write <= '0';
				-- Once write operation is done move back to the initial state to wait for the next operation
				_next <= initial;
			else
				m_write <= '0';
				_next <= write_back;
			end if;
	end case;
end process;

end arch;
