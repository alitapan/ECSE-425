# Cache

The goal of this deliverable was to build a cache circuit. The cache has the following paramters:

- Write-back policy
- Direct-mapped
- 32-bit words
- 128-bit blocks
- 4096-bits of data storage 
- 32-bit addresses

In addition to these parameters, storage for data, tags and flags (valid and dirty bits) were implemented as VHDL arrays.

To compile, open ModelSim and change the directory (File, Change Directory) to the one containing the project files. In the 
Transcript section (ModelSim console), run the following command:

<b> source cache_tb.tcl </b>
