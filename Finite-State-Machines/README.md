# Finite-State Machines

The goal of this deliverable was to build and test a finite-state machine to identify the commented characters in C code.
The finite machine has the following ports:

- clk: in std_logic;
- reset: in std_logic;
- input: std_logic_vector (7 downto 0);
- output: out std_logic;

For every clock cycle one ASCII character was fed to the FSM. The output of the FSM was programmed to give '1' if the character
was part of a comment, and '0' otherwise.

The exit sequence of a comment was considered to be a part of the comment while the opening sequence was not.

To compile, open ModelSim and change the directory (File, Change Directory) to the one containing those three files. In the 
Transcript section (ModelSim console), run the following command:

<b> source fsm_tb.tcl </b>
