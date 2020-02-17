library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cache_tb is
end cache_tb;

architecture behavior of cache_tb is

COMPONENT cache is
	GENERIC(
  	  ram_size : INTEGER := 32768
	);
	PORT(

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
	end component;

component memory is 
GENERIC(
    ram_size : INTEGER := 32768;
    mem_delay : time := 10 ns;
    clock_period : time := 1 ns
);
PORT (
    clock: IN STD_LOGIC;
    writedata: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
    address: IN INTEGER RANGE 0 TO ram_size-1;
    memwrite: IN STD_LOGIC;
    memread: IN STD_LOGIC;
    readdata: OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
    waitrequest: OUT STD_LOGIC
);
end component;
	
-- test signals 
signal reset : std_logic := '0';
signal clk : std_logic := '0';
constant clk_period : time := 1 ns;

signal s_addr : std_logic_vector (31 downto 0);
signal s_read : std_logic;
signal s_readdata : std_logic_vector (31 downto 0);
signal s_write : std_logic;
signal s_writedata : std_logic_vector (31 downto 0);
signal s_waitrequest : std_logic;

signal m_addr : integer range 0 to 2147483647;
signal m_read : std_logic;
signal m_readdata : std_logic_vector (7 downto 0);
signal m_write : std_logic;
signal m_writedata : std_logic_vector (7 downto 0);
signal m_waitrequest : std_logic; 

begin

-- Connect the components which we instantiated above to their
-- respective signals.
dut: cache 
port map(
    clock => clk,
    reset => reset,

    s_addr => s_addr,
    s_read => s_read,
    s_readdata => s_readdata,
    s_write => s_write,
    s_writedata => s_writedata,
    s_waitrequest => s_waitrequest,

    m_addr => m_addr,
    m_read => m_read,
    m_readdata => m_readdata,
    m_write => m_write,
    m_writedata => m_writedata,
    m_waitrequest => m_waitrequest
);

MEM : memory
port map (
    clock => clk,
    writedata => m_writedata,
    address => m_addr,
    memwrite => m_write,
    memread => m_read,
    readdata => m_readdata,
    waitrequest => m_waitrequest
);
				

clk_process : process
begin
  clk <= '0';
  wait for clk_period/2;
  clk <= '1';
  wait for clk_period/2;
end process;

test_process : process
begin

	-- Table of Contents for Test Cases:
	--------------------------------------------
	--    1.  Read  -  Clean,  Hit,   Valid
	--    2.  Read  -  Clean,  Hit,   Invalid (Impossible Case)
	--    3.  Read  -  Clean,  Miss,  Valid 
	--    4.  Read  -  Clean,  Miss,  Invalid
	--    5.  Read  -  Dirty,  Hit,   Valid
	--    6.  Read  -  Dirty,  Hit,   Invalid (Impossible Case)
	--    7.  Read  -  Dirty,  Miss,  Valid
	--    8.  Read  -  Dirty,  Miss,  Invalid (Impossible Case)
	--    9.  Write -  Clean,  Hit,   Valid
	--    10. Write -  Clean,  Hit,   Invalid (Impossible Case)
	--    11. Write -  Clean,  Miss,  Valid
	--    12. Write -  Clean,  Miss,  Invalid
	--    13. Write -  Dirty,  Hit,   Valid
	--    14. Write -  Dirty,  Hit,   Invalid (Impossible Case)
	--    15. Write -  Dirty,  Miss,  Valid
	--    16. Write -  Dirty,  Miss,  Invalid (Impossible Case)
	--------------------------------------------

	-- 1.  Read  -  Clean,  Hit,   Valid
	s_addr <= "11111111111111111111111111111111";                        
	s_write <= '1';                                                      
	s_writedata <= x"000F000A";                                          
	wait until rising_edge(s_waitrequest);                               
	s_read <= '1';                                                       
	s_write <= '0';                                                      
	wait until rising_edge(s_waitrequest);                               
	assert s_readdata = x"000F000A" report "Test 1 Not Passed!" severity error;
	s_read <= '0';                                                       
	s_write <= '0'; 
	
	wait for clk_period;


	-- 3.  Read  -  Clean,  Miss,  Valid
	s_addr <= "00000000000000000000000000000000";	
	s_read <= '1';                                                       
	s_write <= '0';                                                      
	wait until rising_edge(s_waitrequest);                               
	s_addr <= "00000000000000000000000010000000";	
	s_read <= '1';                                                       
	s_write <= '0';                                                      
	wait until rising_edge(s_waitrequest);                               
	s_read <= '0';                                                       
	s_write <= '0';

	wait for clk_period;

	-- 4.  Read  -  Clean,  Miss,  Invalid
	s_addr <= "11111111101111011111111110111111";                        
	s_read <= '1';                                                       
	s_write <= '0';                                                      
	wait until rising_edge(s_waitrequest);                               
	s_read <= '0';                                                       
	s_write <= '0'; 
   
	wait for clk_period;

	-- 5.  Read  -  Dirty,  Hit,   Valid
	s_addr <= "11111111111111111111111111111111";                        
	s_write <= '1';                                                      
	s_writedata <= x"000F000A";                                          
	wait until rising_edge(s_waitrequest);                               
	s_read <= '1';                                                       
	s_write <= '0';                                                      
	wait until rising_edge(s_waitrequest);                               
	assert s_readdata = x"000F000A" report "Test 5 Not Passed!" severity error;
	s_read <= '0';                                                       
	s_write <= '0'; 

	wait for clk_period;


	-- 7.  Read  -  Dirty,  Miss,  Valid
	WAIT FOR clk_period;
	s_addr <= "11111110000000000000000000000000";
	s_write <= '1';
	s_writedata <= x"04030201";
	wait until rising_edge(s_waitrequest);
	s_addr <= "00000000000000000000100000000000";
	s_write <= '0';
	s_read <= '1';
	wait until rising_edge(s_waitrequest);
	assert s_readdata = x"03020100" report "Test 7 Not Passed!" severity error;
	s_read <= '0';
	s_write <= '0';

	wait for clk_period;

	-- 9.  Write -  Clean,  Hit,   Valid
	s_addr <= "10000000000000000000000000000000";	
	s_read <= '1';                                                       
	s_write <= '0';                                                      
	wait until rising_edge(s_waitrequest);                               
	s_write <= '1';
	s_read <= '0';
	s_writedata <= x"0000000B";
	wait until rising_edge(s_waitrequest);                               
	s_write <= '0';
	s_read <= '0';

	wait for clk_period;

	-- 11. Write -  Clean,  Miss,  Valid
	s_addr <= "11100000000000000000000000000000";	
	s_read <= '1';                                                       
	s_write <= '0';                                                      
	wait until rising_edge(s_waitrequest);                               
	s_addr <= "11100000000000000000001000000000";	
	s_write <= '1';
	s_read <= '0';
	s_writedata <= x"0000000D";
	wait until rising_edge(s_waitrequest);                               
	s_write <= '0';
	s_read <= '0';

	wait for clk_period;

	-- 12. Write -  Clean,  Miss,  Invalid
	s_addr <= "11111111111111111111111111111111";                        
	s_write <= '1';                                                      
	s_writedata <= x"000F000A";                                          
	wait until rising_edge(s_waitrequest);                               
	s_read <= '1';                                                       
	s_write <= '0';                                                      
	wait until rising_edge(s_waitrequest);                               
	assert s_readdata = x"000F000A" report "Test 12 Not Passed!" severity error;
	s_read <= '0';                                                       
	s_write <= '0'; 

	wait for clk_period;

	-- 13. Write -  Dirty,  Hit,   Valid
	s_addr <= "11000000000000000000000000000000";	
	s_write <= '1';
	s_read <= '0';
	s_writedata <= x"0000000B";
	wait until rising_edge(s_waitrequest);                               
	s_addr <= "11000000000000000000000000000000";	
	s_write <='0';
	wait for clk_period;
	s_write <= '1';
	s_read <= '0';
	s_writedata <= x"0000000C";
	wait until rising_edge(s_waitrequest);                               
	s_write <= '0';
	s_read <= '0';

	wait for clk_period;

	-- 15. Write -  Dirty,  Miss,  Valid
	s_addr <= "11111100000000000000000000000000";
	s_write <= '1';
	s_writedata <= x"04030201";
	wait until rising_edge(s_waitrequest);
	s_addr <= "00000000000000000000000100000000";
	s_write <= '1';
	s_writedata <= x"000000BA"; 	
	wait until rising_edge(s_waitrequest);
	s_read <= '1';
	s_write <= '0';
	wait until rising_edge(s_waitrequest);
	assert s_readdata = x"000000BA" report "Test 15 Not Passed!" severity error;
	s_read <= '0';
	s_write <= '0';

	wait for clk_period;

	wait;
	
end process;
	
end;
