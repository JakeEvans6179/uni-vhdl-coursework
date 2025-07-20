# uni-vhdl-coursework
VHDL simulation for peak detection system including peak detector, data source and communication links.
The following files are for simulating the command processor block shown in the block_diagram.png file. Simulation was succesfull, however implementation onto an fpga board was not successfully carried out.
The file me and my team worked on and created is the cmdproc.vhd while the others were provided to help simulate.
To run, open a new project in vivado and add all .vhd files provided, no constraints needed. Choose the component XC7A35TCPG236-1 by filtering for family Artix-7, package cpg236 and speed grade -1.

Processing of a data sequence is intiated by the start command "aNNN" or "ANNN" where "NNN" are 3 decimal digits ranging from "000" to "999" (eg. "a500" means process 500 bytes). Termination of character string by hitting <ENTER> is not required. 

If the letter "A" or "a" is followed by 3 decimal digits in sequence, regardless of what keystrokes preceded "A" or "a", the command is valid. Examples of a valid start string are "A500", "a067" and "hjhgjga945".

Once processing of a data sequence is completed, two commands are valid:
"P" or "p" to list peak byte in hexadecimal format followed by the index of that byte
"L" or "l" to list the 3 bytes before peak byte, the peak byte and the 3 bytes after the peak byte all in hexadecimal format.

The commands will not result in any action if no data sequence has been processed prior to recieving them.
