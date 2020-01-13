proc AddWaves {} {
	;#Add waves we're interested in to the Wave window
    add wave -position end sim:/fsm_tb/clk
    add wave -position end sim:/fsm_tb/s_reset
    add wave -position end sim:/fsm_tb/s_input
    add wave -position end sim:/fsm_tb/s_output
}

vlib work

;# Compile components if any
vcom fsm.vhd
vcom fsm_tb.vhd

;# Start simulation
vsim fsm_tb

;# Generate a clock with 1ns period
force -deposit clk 0 0 ns, 1 0.5 ns -repeat 1 ns

;# Add the waves
AddWaves

;# Run for 50 ns
run 50ns
