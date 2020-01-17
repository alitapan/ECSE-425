# Pipelining

The goal of this deliverable was to build a pipeline for the following equation:

                Output = ((a + b) * 42) - (c * d * (a - e))

The VHDL code has the following inputs and outputs:

- clk : in std_logic;
- a, b, c, d, e : in integer;
- op1, op2, op3, op4, op5, final_output : out integer

where

- op1 is the intermediate result of a + b
- op2 is the intermediate result of op1 * 42
- op3 is the intermediate result of c * d
- op4 is the intermediate result of a – e
- op5 is the intermediate result of op3 * op4
- final_output is the result of op2 – op5

To compile, open ModelSim and change the directory (File, Change Directory) to the one containing those three files. In the Transcript section (ModelSim console), run the following command:

<b> source pipeline_tb.tcl </b>
