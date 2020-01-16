proc AddWaves {} {
	;#Add waves we're interested in to the Wave window
    add wave -position end sim:/pipeline_tb/clk
    add wave -position end sim:/pipeline_tb/s_a
    add wave -position end sim:/pipeline_tb/s_b
    add wave -position end sim:/pipeline_tb/s_c
    add wave -position end sim:/pipeline_tb/s_d
    add wave -position end sim:/pipeline_tb/s_e
    add wave -position end sim:/pipeline_tb/s_op1
    add wave -position end sim:/pipeline_tb/s_op2
    add wave -position end sim:/pipeline_tb/s_op3
    add wave -position end sim:/pipeline_tb/s_op4
    add wave -position end sim:/pipeline_tb/s_op5
    add wave -position end sim:/pipeline_tb/s_final_output
}

vlib work

;# Compile components if any
vcom pipeline.vhd
vcom pipeline_tb.vhd

;# Start simulation
vsim pipeline_tb

;# Generate a clock with 1ns period
force -deposit clk 0 0 ns, 1 0.5 ns -repeat 1 ns

;# Add the waves
AddWaves

;# Run for 50 ns
run 50ns
