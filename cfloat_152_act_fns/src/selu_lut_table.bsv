package selu_lut_table;
  import Vector :: *;
  import FIFOF :: *;
  import GetPut :: *;
  import extraFunctions :: *;

  typedef struct {
    Bit#(1) sign; // 1 - Negative, 0 - Positive
    Bit#(5) exp;
    Bit#(2) mantissa;
  } Cfloat_1_5_2 deriving(Bits, Eq, FShow);

  typedef struct {
    Bool invalid;
    Bool denormal;
    Bool overflow;
    Bool underflow;
  } Flags deriving(Bits, Eq, FShow);

  typedef enum {Tanh, Sigmoid, LeakyReLu, SeLu} Operation deriving(Bits, Eq, FShow);

  typedef struct {
    Cfloat_1_5_2 inp;
    Int#(6) bias;
    Operation op;
  } PreprocessStageMeta deriving(Bits, Eq, FShow);

  typedef struct {
    Bit#(1) sign;
    Int#(8) act_exp;
    Bit#(3) act_mantissa;
    Int#(6) bias;
    Operation op;
    Flags flags;
  } ComputeStageMeta deriving(Bits, Eq, FShow);

  typedef struct {
    Bit#(1) final_sign;
    Int#(8) exponent_final_output;
    Bit#(4) mantissa_final_output;
    Int#(6) bias;
    Flags flags;
  } PostprocessStageMeta deriving(Bits, Eq, FShow);

  typedef struct {
    Cfloat_1_5_2 out;
    Flags flags;
  } OutputStageMeta deriving(Bits, Eq, FShow);

  interface interface_selu_lut_table_obs_1;
    method Tuple2#(Int#(7), Bit#(2)) mv_selu_output(Bit#(6) exp, Bit#(2) man);
  endinterface

  module mkselu_lut_table_obs_1(interface_selu_lut_table_obs_1);
    // range: 42 - 314
    Reg#(Bit#(2)) rg_man_output[220];
    rg_man_output[0] = readOnlyReg(0);
    rg_man_output[1] = readOnlyReg(3);
    rg_man_output[2] = readOnlyReg(3);
    rg_man_output[3] = readOnlyReg(3);
    rg_man_output[4] = readOnlyReg(3);
    rg_man_output[5] = readOnlyReg(3);
    rg_man_output[6] = readOnlyReg(3);
    rg_man_output[7] = readOnlyReg(3);
    rg_man_output[8] = readOnlyReg(3);
    rg_man_output[9] = readOnlyReg(3);
    rg_man_output[10] = readOnlyReg(1);
    rg_man_output[11] = readOnlyReg(1);
    rg_man_output[12] = readOnlyReg(3);
    rg_man_output[13] = readOnlyReg(0);
    rg_man_output[14] = readOnlyReg(1);
    rg_man_output[15] = readOnlyReg(2);
    rg_man_output[16] = readOnlyReg(3);
    rg_man_output[17] = readOnlyReg(0);
    rg_man_output[18] = readOnlyReg(1);
    rg_man_output[19] = readOnlyReg(2);
    rg_man_output[20] = readOnlyReg(3);
    rg_man_output[21] = readOnlyReg(0);
    rg_man_output[22] = readOnlyReg(1);
    rg_man_output[23] = readOnlyReg(2);
    rg_man_output[24] = readOnlyReg(3);
    rg_man_output[25] = readOnlyReg(0);
    rg_man_output[26] = readOnlyReg(1);
    rg_man_output[27] = readOnlyReg(2);
    rg_man_output[28] = readOnlyReg(3);
    rg_man_output[29] = readOnlyReg(0);
    rg_man_output[30] = readOnlyReg(1);
    rg_man_output[31] = readOnlyReg(2);
    rg_man_output[32] = readOnlyReg(3);
    rg_man_output[33] = readOnlyReg(0);
    rg_man_output[34] = readOnlyReg(1);
    rg_man_output[35] = readOnlyReg(2);
    rg_man_output[36] = readOnlyReg(3);
    rg_man_output[37] = readOnlyReg(0);
    rg_man_output[38] = readOnlyReg(1);
    rg_man_output[39] = readOnlyReg(2);
    rg_man_output[40] = readOnlyReg(3);
    rg_man_output[41] = readOnlyReg(0);
    rg_man_output[42] = readOnlyReg(1);
    rg_man_output[43] = readOnlyReg(2);
    rg_man_output[44] = readOnlyReg(3);
    rg_man_output[45] = readOnlyReg(0);
    rg_man_output[46] = readOnlyReg(1);
    rg_man_output[47] = readOnlyReg(2);
    rg_man_output[48] = readOnlyReg(3);
    rg_man_output[49] = readOnlyReg(0);
    rg_man_output[50] = readOnlyReg(1);
    rg_man_output[51] = readOnlyReg(2);
    rg_man_output[52] = readOnlyReg(3);
    rg_man_output[53] = readOnlyReg(0);
    rg_man_output[54] = readOnlyReg(1);
    rg_man_output[55] = readOnlyReg(2);
    rg_man_output[56] = readOnlyReg(3);
    rg_man_output[57] = readOnlyReg(0);
    rg_man_output[58] = readOnlyReg(1);
    rg_man_output[59] = readOnlyReg(2);
    rg_man_output[60] = readOnlyReg(3);
    rg_man_output[61] = readOnlyReg(0);
    rg_man_output[62] = readOnlyReg(1);
    rg_man_output[63] = readOnlyReg(2);
    rg_man_output[64] = readOnlyReg(3);
    rg_man_output[65] = readOnlyReg(0);
    rg_man_output[66] = readOnlyReg(1);
    rg_man_output[67] = readOnlyReg(2);
    rg_man_output[68] = readOnlyReg(3);
    rg_man_output[69] = readOnlyReg(0);
    rg_man_output[70] = readOnlyReg(1);
    rg_man_output[71] = readOnlyReg(2);
    rg_man_output[72] = readOnlyReg(3);
    rg_man_output[73] = readOnlyReg(0);
    rg_man_output[74] = readOnlyReg(1);
    rg_man_output[75] = readOnlyReg(2);
    rg_man_output[76] = readOnlyReg(3);
    rg_man_output[77] = readOnlyReg(0);
    rg_man_output[78] = readOnlyReg(1);
    rg_man_output[79] = readOnlyReg(2);
    rg_man_output[80] = readOnlyReg(3);
    rg_man_output[81] = readOnlyReg(0);
    rg_man_output[82] = readOnlyReg(1);
    rg_man_output[83] = readOnlyReg(2);
    rg_man_output[84] = readOnlyReg(3);
    rg_man_output[85] = readOnlyReg(0);
    rg_man_output[86] = readOnlyReg(1);
    rg_man_output[87] = readOnlyReg(2);
    rg_man_output[88] = readOnlyReg(3);
    rg_man_output[89] = readOnlyReg(0);
    rg_man_output[90] = readOnlyReg(1);
    rg_man_output[91] = readOnlyReg(2);
    rg_man_output[92] = readOnlyReg(3);
    rg_man_output[93] = readOnlyReg(0);
    rg_man_output[94] = readOnlyReg(1);
    rg_man_output[95] = readOnlyReg(2);
    rg_man_output[96] = readOnlyReg(3);
    rg_man_output[97] = readOnlyReg(0);
    rg_man_output[98] = readOnlyReg(1);
    rg_man_output[99] = readOnlyReg(2);
    rg_man_output[100] = readOnlyReg(3);
    rg_man_output[101] = readOnlyReg(0);
    rg_man_output[102] = readOnlyReg(1);
    rg_man_output[103] = readOnlyReg(2);
    rg_man_output[104] = readOnlyReg(3);
    rg_man_output[105] = readOnlyReg(0);
    rg_man_output[106] = readOnlyReg(1);
    rg_man_output[107] = readOnlyReg(2);
    rg_man_output[108] = readOnlyReg(3);
    rg_man_output[109] = readOnlyReg(0);
    rg_man_output[110] = readOnlyReg(1);
    rg_man_output[111] = readOnlyReg(2);
    rg_man_output[112] = readOnlyReg(3);
    rg_man_output[113] = readOnlyReg(0);
    rg_man_output[114] = readOnlyReg(1);
    rg_man_output[115] = readOnlyReg(2);
    rg_man_output[116] = readOnlyReg(3);
    rg_man_output[117] = readOnlyReg(0);
    rg_man_output[118] = readOnlyReg(1);
    rg_man_output[119] = readOnlyReg(2);
    rg_man_output[120] = readOnlyReg(3);
    rg_man_output[121] = readOnlyReg(0);
    rg_man_output[122] = readOnlyReg(1);
    rg_man_output[123] = readOnlyReg(2);
    rg_man_output[124] = readOnlyReg(3);
    rg_man_output[125] = readOnlyReg(0);
    rg_man_output[126] = readOnlyReg(1);
    rg_man_output[127] = readOnlyReg(2);
    rg_man_output[128] = readOnlyReg(3);
    rg_man_output[129] = readOnlyReg(0);
    rg_man_output[130] = readOnlyReg(1);
    rg_man_output[131] = readOnlyReg(2);
    rg_man_output[132] = readOnlyReg(3);
    rg_man_output[133] = readOnlyReg(0);
    rg_man_output[134] = readOnlyReg(1);
    rg_man_output[135] = readOnlyReg(2);
    rg_man_output[136] = readOnlyReg(3);
    rg_man_output[137] = readOnlyReg(0);
    rg_man_output[138] = readOnlyReg(1);
    rg_man_output[139] = readOnlyReg(2);
    rg_man_output[140] = readOnlyReg(3);
    rg_man_output[141] = readOnlyReg(0);
    rg_man_output[142] = readOnlyReg(1);
    rg_man_output[143] = readOnlyReg(2);
    rg_man_output[144] = readOnlyReg(3);
    rg_man_output[145] = readOnlyReg(0);
    rg_man_output[146] = readOnlyReg(1);
    rg_man_output[147] = readOnlyReg(2);
    rg_man_output[148] = readOnlyReg(3);
    rg_man_output[149] = readOnlyReg(0);
    rg_man_output[150] = readOnlyReg(1);
    rg_man_output[151] = readOnlyReg(2);
    rg_man_output[152] = readOnlyReg(3);
    rg_man_output[153] = readOnlyReg(0);
    rg_man_output[154] = readOnlyReg(1);
    rg_man_output[155] = readOnlyReg(2);
    rg_man_output[156] = readOnlyReg(3);
    rg_man_output[157] = readOnlyReg(0);
    rg_man_output[158] = readOnlyReg(1);
    rg_man_output[159] = readOnlyReg(2);
    rg_man_output[160] = readOnlyReg(3);
    rg_man_output[161] = readOnlyReg(0);
    rg_man_output[162] = readOnlyReg(1);
    rg_man_output[163] = readOnlyReg(2);
    rg_man_output[164] = readOnlyReg(3);
    rg_man_output[165] = readOnlyReg(0);
    rg_man_output[166] = readOnlyReg(1);
    rg_man_output[167] = readOnlyReg(2);
    rg_man_output[168] = readOnlyReg(3);
    rg_man_output[169] = readOnlyReg(0);
    rg_man_output[170] = readOnlyReg(1);
    rg_man_output[171] = readOnlyReg(2);
    rg_man_output[172] = readOnlyReg(3);
    rg_man_output[173] = readOnlyReg(0);
    rg_man_output[174] = readOnlyReg(1);
    rg_man_output[175] = readOnlyReg(2);
    rg_man_output[176] = readOnlyReg(3);
    rg_man_output[177] = readOnlyReg(0);
    rg_man_output[178] = readOnlyReg(1);
    rg_man_output[179] = readOnlyReg(2);
    rg_man_output[180] = readOnlyReg(3);
    rg_man_output[181] = readOnlyReg(0);
    rg_man_output[182] = readOnlyReg(1);
    rg_man_output[183] = readOnlyReg(2);
    rg_man_output[184] = readOnlyReg(3);
    rg_man_output[185] = readOnlyReg(0);
    rg_man_output[186] = readOnlyReg(1);
    rg_man_output[187] = readOnlyReg(2);
    rg_man_output[188] = readOnlyReg(3);
    rg_man_output[189] = readOnlyReg(0);
    rg_man_output[190] = readOnlyReg(1);
    rg_man_output[191] = readOnlyReg(2);
    rg_man_output[192] = readOnlyReg(3);
    rg_man_output[193] = readOnlyReg(0);
    rg_man_output[194] = readOnlyReg(1);
    rg_man_output[195] = readOnlyReg(2);
    rg_man_output[196] = readOnlyReg(3);
    rg_man_output[197] = readOnlyReg(0);
    rg_man_output[198] = readOnlyReg(1);
    rg_man_output[199] = readOnlyReg(2);
    rg_man_output[200] = readOnlyReg(3);
    rg_man_output[201] = readOnlyReg(0);
    rg_man_output[202] = readOnlyReg(1);
    rg_man_output[203] = readOnlyReg(2);
    rg_man_output[204] = readOnlyReg(3);
    rg_man_output[205] = readOnlyReg(0);
    rg_man_output[206] = readOnlyReg(1);
    rg_man_output[207] = readOnlyReg(2);
    rg_man_output[208] = readOnlyReg(2);
    rg_man_output[209] = readOnlyReg(0);
    rg_man_output[210] = readOnlyReg(0);
    rg_man_output[211] = readOnlyReg(1);
    rg_man_output[212] = readOnlyReg(2);
    rg_man_output[213] = readOnlyReg(3);
    rg_man_output[214] = readOnlyReg(3);
    rg_man_output[215] = readOnlyReg(3);
    rg_man_output[216] = readOnlyReg(0);
    rg_man_output[217] = readOnlyReg(2);
    rg_man_output[218] = readOnlyReg(2);
    rg_man_output[219] = readOnlyReg(3);
    

    Reg#(Int#(7)) rg_exp_output[220];
    rg_exp_output[0] = readOnlyReg(0);
    rg_exp_output[1] = readOnlyReg(-53);
    rg_exp_output[2] = readOnlyReg(-53);
    rg_exp_output[3] = readOnlyReg(-53);
    rg_exp_output[4] = readOnlyReg(-53);
    rg_exp_output[5] = readOnlyReg(-53);
    rg_exp_output[6] = readOnlyReg(-53);
    rg_exp_output[7] = readOnlyReg(-52);
    rg_exp_output[8] = readOnlyReg(-52);
    rg_exp_output[9] = readOnlyReg(-52);
    rg_exp_output[10] = readOnlyReg(-51);
    rg_exp_output[11] = readOnlyReg(-51);
    rg_exp_output[12] = readOnlyReg(-51);
    rg_exp_output[13] = readOnlyReg(-50);
    rg_exp_output[14] = readOnlyReg(-50);
    rg_exp_output[15] = readOnlyReg(-50);
    rg_exp_output[16] = readOnlyReg(-50);
    rg_exp_output[17] = readOnlyReg(-49);
    rg_exp_output[18] = readOnlyReg(-49);
    rg_exp_output[19] = readOnlyReg(-49);
    rg_exp_output[20] = readOnlyReg(-49);
    rg_exp_output[21] = readOnlyReg(-48);
    rg_exp_output[22] = readOnlyReg(-48);
    rg_exp_output[23] = readOnlyReg(-48);
    rg_exp_output[24] = readOnlyReg(-48);
    rg_exp_output[25] = readOnlyReg(-47);
    rg_exp_output[26] = readOnlyReg(-47);
    rg_exp_output[27] = readOnlyReg(-47);
    rg_exp_output[28] = readOnlyReg(-47);
    rg_exp_output[29] = readOnlyReg(-46);
    rg_exp_output[30] = readOnlyReg(-46);
    rg_exp_output[31] = readOnlyReg(-46);
    rg_exp_output[32] = readOnlyReg(-46);
    rg_exp_output[33] = readOnlyReg(-45);
    rg_exp_output[34] = readOnlyReg(-45);
    rg_exp_output[35] = readOnlyReg(-45);
    rg_exp_output[36] = readOnlyReg(-45);
    rg_exp_output[37] = readOnlyReg(-44);
    rg_exp_output[38] = readOnlyReg(-44);
    rg_exp_output[39] = readOnlyReg(-44);
    rg_exp_output[40] = readOnlyReg(-44);
    rg_exp_output[41] = readOnlyReg(-43);
    rg_exp_output[42] = readOnlyReg(-43);
    rg_exp_output[43] = readOnlyReg(-43);
    rg_exp_output[44] = readOnlyReg(-43);
    rg_exp_output[45] = readOnlyReg(-42);
    rg_exp_output[46] = readOnlyReg(-42);
    rg_exp_output[47] = readOnlyReg(-42);
    rg_exp_output[48] = readOnlyReg(-42);
    rg_exp_output[49] = readOnlyReg(-41);
    rg_exp_output[50] = readOnlyReg(-41);
    rg_exp_output[51] = readOnlyReg(-41);
    rg_exp_output[52] = readOnlyReg(-41);
    rg_exp_output[53] = readOnlyReg(-40);
    rg_exp_output[54] = readOnlyReg(-40);
    rg_exp_output[55] = readOnlyReg(-40);
    rg_exp_output[56] = readOnlyReg(-40);
    rg_exp_output[57] = readOnlyReg(-39);
    rg_exp_output[58] = readOnlyReg(-39);
    rg_exp_output[59] = readOnlyReg(-39);
    rg_exp_output[60] = readOnlyReg(-39);
    rg_exp_output[61] = readOnlyReg(-38);
    rg_exp_output[62] = readOnlyReg(-38);
    rg_exp_output[63] = readOnlyReg(-38);
    rg_exp_output[64] = readOnlyReg(-38);
    rg_exp_output[65] = readOnlyReg(-37);
    rg_exp_output[66] = readOnlyReg(-37);
    rg_exp_output[67] = readOnlyReg(-37);
    rg_exp_output[68] = readOnlyReg(-37);
    rg_exp_output[69] = readOnlyReg(-36);
    rg_exp_output[70] = readOnlyReg(-36);
    rg_exp_output[71] = readOnlyReg(-36);
    rg_exp_output[72] = readOnlyReg(-36);
    rg_exp_output[73] = readOnlyReg(-35);
    rg_exp_output[74] = readOnlyReg(-35);
    rg_exp_output[75] = readOnlyReg(-35);
    rg_exp_output[76] = readOnlyReg(-35);
    rg_exp_output[77] = readOnlyReg(-34);
    rg_exp_output[78] = readOnlyReg(-34);
    rg_exp_output[79] = readOnlyReg(-34);
    rg_exp_output[80] = readOnlyReg(-34);
    rg_exp_output[81] = readOnlyReg(-33);
    rg_exp_output[82] = readOnlyReg(-33);
    rg_exp_output[83] = readOnlyReg(-33);
    rg_exp_output[84] = readOnlyReg(-33);
    rg_exp_output[85] = readOnlyReg(-32);
    rg_exp_output[86] = readOnlyReg(-32);
    rg_exp_output[87] = readOnlyReg(-32);
    rg_exp_output[88] = readOnlyReg(-32);
    rg_exp_output[89] = readOnlyReg(-31);
    rg_exp_output[90] = readOnlyReg(-31);
    rg_exp_output[91] = readOnlyReg(-31);
    rg_exp_output[92] = readOnlyReg(-31);
    rg_exp_output[93] = readOnlyReg(-30);
    rg_exp_output[94] = readOnlyReg(-30);
    rg_exp_output[95] = readOnlyReg(-30);
    rg_exp_output[96] = readOnlyReg(-30);
    rg_exp_output[97] = readOnlyReg(-29);
    rg_exp_output[98] = readOnlyReg(-29);
    rg_exp_output[99] = readOnlyReg(-29);
    rg_exp_output[100] = readOnlyReg(-29);
    rg_exp_output[101] = readOnlyReg(-28);
    rg_exp_output[102] = readOnlyReg(-28);
    rg_exp_output[103] = readOnlyReg(-28);
    rg_exp_output[104] = readOnlyReg(-28);
    rg_exp_output[105] = readOnlyReg(-27);
    rg_exp_output[106] = readOnlyReg(-27);
    rg_exp_output[107] = readOnlyReg(-27);
    rg_exp_output[108] = readOnlyReg(-27);
    rg_exp_output[109] = readOnlyReg(-26);
    rg_exp_output[110] = readOnlyReg(-26);
    rg_exp_output[111] = readOnlyReg(-26);
    rg_exp_output[112] = readOnlyReg(-26);
    rg_exp_output[113] = readOnlyReg(-25);
    rg_exp_output[114] = readOnlyReg(-25);
    rg_exp_output[115] = readOnlyReg(-25);
    rg_exp_output[116] = readOnlyReg(-25);
    rg_exp_output[117] = readOnlyReg(-24);
    rg_exp_output[118] = readOnlyReg(-24);
    rg_exp_output[119] = readOnlyReg(-24);
    rg_exp_output[120] = readOnlyReg(-24);
    rg_exp_output[121] = readOnlyReg(-23);
    rg_exp_output[122] = readOnlyReg(-23);
    rg_exp_output[123] = readOnlyReg(-23);
    rg_exp_output[124] = readOnlyReg(-23);
    rg_exp_output[125] = readOnlyReg(-22);
    rg_exp_output[126] = readOnlyReg(-22);
    rg_exp_output[127] = readOnlyReg(-22);
    rg_exp_output[128] = readOnlyReg(-22);
    rg_exp_output[129] = readOnlyReg(-21);
    rg_exp_output[130] = readOnlyReg(-21);
    rg_exp_output[131] = readOnlyReg(-21);
    rg_exp_output[132] = readOnlyReg(-21);
    rg_exp_output[133] = readOnlyReg(-20);
    rg_exp_output[134] = readOnlyReg(-20);
    rg_exp_output[135] = readOnlyReg(-20);
    rg_exp_output[136] = readOnlyReg(-20);
    rg_exp_output[137] = readOnlyReg(-19);
    rg_exp_output[138] = readOnlyReg(-19);
    rg_exp_output[139] = readOnlyReg(-19);
    rg_exp_output[140] = readOnlyReg(-19);
    rg_exp_output[141] = readOnlyReg(-18);
    rg_exp_output[142] = readOnlyReg(-18);
    rg_exp_output[143] = readOnlyReg(-18);
    rg_exp_output[144] = readOnlyReg(-18);
    rg_exp_output[145] = readOnlyReg(-17);
    rg_exp_output[146] = readOnlyReg(-17);
    rg_exp_output[147] = readOnlyReg(-17);
    rg_exp_output[148] = readOnlyReg(-17);
    rg_exp_output[149] = readOnlyReg(-16);
    rg_exp_output[150] = readOnlyReg(-16);
    rg_exp_output[151] = readOnlyReg(-16);
    rg_exp_output[152] = readOnlyReg(-16);
    rg_exp_output[153] = readOnlyReg(-15);
    rg_exp_output[154] = readOnlyReg(-15);
    rg_exp_output[155] = readOnlyReg(-15);
    rg_exp_output[156] = readOnlyReg(-15);
    rg_exp_output[157] = readOnlyReg(-14);
    rg_exp_output[158] = readOnlyReg(-14);
    rg_exp_output[159] = readOnlyReg(-14);
    rg_exp_output[160] = readOnlyReg(-14);
    rg_exp_output[161] = readOnlyReg(-13);
    rg_exp_output[162] = readOnlyReg(-13);
    rg_exp_output[163] = readOnlyReg(-13);
    rg_exp_output[164] = readOnlyReg(-13);
    rg_exp_output[165] = readOnlyReg(-12);
    rg_exp_output[166] = readOnlyReg(-12);
    rg_exp_output[167] = readOnlyReg(-12);
    rg_exp_output[168] = readOnlyReg(-12);
    rg_exp_output[169] = readOnlyReg(-11);
    rg_exp_output[170] = readOnlyReg(-11);
    rg_exp_output[171] = readOnlyReg(-11);
    rg_exp_output[172] = readOnlyReg(-11);
    rg_exp_output[173] = readOnlyReg(-10);
    rg_exp_output[174] = readOnlyReg(-10);
    rg_exp_output[175] = readOnlyReg(-10);
    rg_exp_output[176] = readOnlyReg(-10);
    rg_exp_output[177] = readOnlyReg(-9);
    rg_exp_output[178] = readOnlyReg(-9);
    rg_exp_output[179] = readOnlyReg(-9);
    rg_exp_output[180] = readOnlyReg(-9);
    rg_exp_output[181] = readOnlyReg(-8);
    rg_exp_output[182] = readOnlyReg(-8);
    rg_exp_output[183] = readOnlyReg(-8);
    rg_exp_output[184] = readOnlyReg(-8);
    rg_exp_output[185] = readOnlyReg(-7);
    rg_exp_output[186] = readOnlyReg(-7);
    rg_exp_output[187] = readOnlyReg(-7);
    rg_exp_output[188] = readOnlyReg(-7);
    rg_exp_output[189] = readOnlyReg(-6);
    rg_exp_output[190] = readOnlyReg(-6);
    rg_exp_output[191] = readOnlyReg(-6);
    rg_exp_output[192] = readOnlyReg(-6);
    rg_exp_output[193] = readOnlyReg(-5);
    rg_exp_output[194] = readOnlyReg(-5);
    rg_exp_output[195] = readOnlyReg(-5);
    rg_exp_output[196] = readOnlyReg(-5);
    rg_exp_output[197] = readOnlyReg(-4);
    rg_exp_output[198] = readOnlyReg(-4);
    rg_exp_output[199] = readOnlyReg(-4);
    rg_exp_output[200] = readOnlyReg(-4);
    rg_exp_output[201] = readOnlyReg(-3);
    rg_exp_output[202] = readOnlyReg(-3);
    rg_exp_output[203] = readOnlyReg(-3);
    rg_exp_output[204] = readOnlyReg(-3);
    rg_exp_output[205] = readOnlyReg(-2);
    rg_exp_output[206] = readOnlyReg(-2);
    rg_exp_output[207] = readOnlyReg(-2);
    rg_exp_output[208] = readOnlyReg(-2);
    rg_exp_output[209] = readOnlyReg(-1);
    rg_exp_output[210] = readOnlyReg(-1);
    rg_exp_output[211] = readOnlyReg(-1);
    rg_exp_output[212] = readOnlyReg(-1);
    rg_exp_output[213] = readOnlyReg(-1);
    rg_exp_output[214] = readOnlyReg(-1);
    rg_exp_output[215] = readOnlyReg(0);
    rg_exp_output[216] = readOnlyReg(0);
    rg_exp_output[217] = readOnlyReg(-2);
    rg_exp_output[218] = readOnlyReg(-1);
    rg_exp_output[219] = readOnlyReg(-1);

    method Tuple2#(Int#(7), Bit#(2)) mv_selu_output(Bit#(6) exp, Bit#(2) man);
      Bit#(8) index = {exp, man};
      return tuple2(rg_exp_output[index], rg_man_output[index]);
    endmethod
  endmodule
endpackage
