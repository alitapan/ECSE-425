proc AddWaves {} {
    ;#Add waves we're interested in to the Wave window
    add wave -position end sim:/cache_tb/clk
    add wave -position end sim:/cache_tb/s_addr
    add wave -position end sim:/cache_tb/s_read 
    add wave -position end sim:/cache_tb/s_readdata 
    add wave -position end sim:/cache_tb/s_write
    add wave -position end sim:/cache_tb/s_writedata
    add wave -position end sim:/cache_tb/s_waitrequest
    add wave -position end sim:/cache_tb/m_addr 
    add wave -position end sim:/cache_tb/m_read 
    add wave -position end sim:/cache_tb/m_readdata 
    add wave -position end sim:/cache_tb/m_write 
    add wave -position end sim:/cache_tb/m_writedata
    add wave -position end sim:/cache_tb/m_waitrequest

}

vlib work

;# Compile components if any
vcom cache.vhd
vcom memory.vhd
vcom cache_tb.vhd
vcom memory_tb.vhd


;# Start simulation
vsim cache_tb

;# Generate a clock with 1ns period
force -deposit clk 0 0 ns, 1 0.5 ns -repeat 1 ns

;# Add the waves
AddWaves

;# Run for 50 ns
run 250ns
