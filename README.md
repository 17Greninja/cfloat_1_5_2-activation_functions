test bench source: https://gitlab.com/shaktiproject/cores/fbox/-/blob/master/fpu_pipelined/bsv_testbench/Tb_fp_add.bsv?ref_type=heads


cfloat_1_5_2_activation_functions
tanh(x), sigmoid(x), ReLu(x) and SeLu(x) - cfloat_1_5_2 datatype

I have done LUT implementation of inputs indexing the LUT with outputs.

The test input data and output data is generated using the generatTestData.cpp file and the data is placed in the txt files, input data: filename.txt output data: {function_name}.txt

The main module is mainFile.bsv which contaons 3 rules, pre-process, mainRule and postProcess rules. preProcess: sets the flags mainRule: does the computation postProcess: displays the output

The implementation is done a lot using LUT tables, which are also placed in the src directory. The LUT tables have been generated using the lutTableGenerator.cpp file.

The testing is performed by generating the outputs and matching them to the {function_name}.txt files.
