/*
main module
*/

package mainFile;
  import FIFOF :: *;
  import extraFunctions :: *;
  import SpecialFIFOs :: *;
  import sigmoid_lut_table :: *;
  import selu_lut_table :: *;
  import Vector :: *;
  import GetPut :: *;
  import FIFO :: *;

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

  interface interface_activation_function_compute;
    method Action ma_input(Cfloat_1_5_2 inp, Int#(6) bias, Operation op);
    method ActionValue#(Maybe#(OutputStageMeta)) mav_output;
  endinterface: interface_activation_function_compute

  module mkmainFile(interface_activation_function_compute);
    /* FIFO to store the inputs*/
    FIFOF#(PreprocessStageMeta) input_fifof <- mkFIFOF();

    /* FIFO to store preprocessed outputs */
    FIFOF#(ComputeStageMeta) compute_fifof <- mkFIFOF();

    /* post processed outputs */
    FIFOF#(PostprocessStageMeta) postProcess_fifof <- mkFIFOF();

    /* FIFO to store the final outputs*/
    FIFOF#(OutputStageMeta) output_fifof <- mkFIFOF();

    interface_sigmoid_lut_table_obs_1 sigmoid_obs_1_table <- mksigmoid_lut_table_obs_1;
    interface_sigmoid_lut_table_obs_2 sigmoid_obs_2_table <- mksigmoid_lut_table_obs_2;

    interface_selu_lut_table_obs_1 selu_obs_1_table <- mkselu_lut_table_obs_1;

    function Tuple2#(Bit#(4), Int#(8)) fn_compute_sigmoid(ComputeStageMeta data);
      Bit#(4) mantissa_output_final;
      Int#(8) exponent_output_final;
      if (data.sign == 1) begin
        if (data.act_exp >= -63 && data.act_exp <= -4) begin
          mantissa_output_final = 4'b1000;
          exponent_output_final = -1;
        end
        else if (data.act_exp >= -3 && data.act_exp <= 5) begin
          Bit#(4) exp_index = truncate(pack(data.act_exp + 3));
          let lut_output = sigmoid_obs_1_table.mv_sig_output(exp_index, data.act_mantissa[1:0]);
          mantissa_output_final = {1'b1, tpl_2(lut_output), 1'b0};
          exponent_output_final = signExtend(tpl_1(lut_output));
        end
        else begin // exp >= 6
          mantissa_output_final = 0;
          exponent_output_final = 0;
        end
      end
      else begin
        if (data.act_exp >= -63 && data.act_exp <= -3) begin
          mantissa_output_final = 0;
          exponent_output_final = -1;
        end
        else if (data.act_exp >= -2 && data.act_exp <= 1) begin
          Bit#(2) exp_index = truncate(pack(data.act_exp + 2));
          let lut_output = sigmoid_obs_2_table.mv_sig_output(exp_index, data.act_mantissa[1:0]);
          mantissa_output_final = {1'b1, tpl_2(lut_output), 1'b0};
          exponent_output_final = signExtend(tpl_1(lut_output));
        end
        else begin
          mantissa_output_final = {1'b1, 2'b11, 1'b0};
          exponent_output_final = 0;
        end
      end

      return tuple2(mantissa_output_final, exponent_output_final);
    endfunction

    rule rule_preprocessing(input_fifof.notEmpty);
      input_fifof.deq;
      let lv_input= input_fifof.first.inp;
      let lv_bias = input_fifof.first.bias;
      Flags lv_flags = unpack(0);

      if(isDenormal(lv_input.exp, lv_input.mantissa))
        lv_flags.denormal = True;

      Int#(8) actual_inp_exp = unpack(zeroExtend(lv_input.exp)) - zeroExtend(lv_bias);
      Bit#(3) actual_mantissa = {hiddenBit(lv_input.exp), lv_input.mantissa};

      compute_fifof.enq(ComputeStageMeta { sign: input_fifof.first.inp.sign,
                                        act_exp: actual_inp_exp,
                                        act_mantissa: actual_mantissa,
                                        bias: lv_bias,
                                        op: input_fifof.first.op,
                                        flags: lv_flags
                                      });
    endrule: rule_preprocessing

    rule mainRule(compute_fifof.notEmpty);
      if(compute_fifof.first.op == Sigmoid)
        compute_fifof.deq;
        let data = compute_fifof.first;

        PostprocessStageMeta lv_output;
        let bias = data.bias;
        lv_output.bias = data.bias;
        lv_output.flags = data.flags;
        lv_output.final_sign = 0;

        let tmp_output = fn_compute_sigmoid(data);
        lv_output.mantissa_output_final = tpl_1(tmp_output);
        lv_output.exponent_output_final = tpl_2(tmp_output);

        postProcess_fifof.enq(lv_output);
      end
      else if(compute_fifof.first.op == Tanh && compute_fifof.notEmpty)
        compute_fifof.deq;
        let data = compute_fifof.first;

        PostprocessStageMeta lv_output;
        let bias = data.bias;
        lv_output.bias = data.bias;
        lv_output.flags = data.flags;
        lv_output.final_sign = data.sign;

        data.act_exp = data.act_exp + 1;
        let tmp_output = fn_compute_sigmoid(data);
        
        lv_output.mantissa_output_final = tpl_1(tmp_output) - 1;
        lv_output.exponent_output_final = tpl_2(tmp_output) + 1;
        
        postProcess_fifof.enq(lv_output);
      end
      else if(compute_fifof.first.op == SeLu)
        compute_fifof.deq;
        let data = compute_fifof.first;

        PostprocessStageMeta lv_output;
        let bias = data.bias;
        lv_output.bias = data.bias;
        lv_output.flags = data.flags;
        lv_output.final_sign = data.sign;

        if (data.sign == 1) begin
          if (data.act_exp >= -63 && data.act_exp <= -55) begin
            lv_output.mantissa_output_final = 0;
            lv_output.exponent_output_final = 0;
          end
          else if (data.act_exp >= -54 && data.act_exp <= 0) begin
            Bit#(6) exp_index = truncate(pack(data.act_exp + 54));
            let lut_output = selu_obs_1_table.mv_selu_output(exp_index, data.act_mantissa[1:0]);
            lv_output.mantissa_output_final = {1'b1, tpl_2(lut_output), 1'b0};
            lv_output.exponent_output_final = signExtend(tpl_1(lut_output));
          end
          else begin
            lv_output.mantissa_output_final = {1'b0, 2'b11, 1'b0};
            lv_output.exponent_output_final = 0;
          end
        end
        else begin
          lv_output.mantissa_output_final = {data.act_mantissa, 1'b0};
          lv_output.exponent_output_final = signExtend(data.act_exp);
        end

        postProcess_fifof.enq(lv_output);
      end
      else if(compute_fifof.first.op == LeakyReLu)
        compute_fifof.deq;
        let data = compute_fifof.first;

        PostprocessStageMeta lv_output;
        let bias = data.bias;
        lv_output.bias = data.bias;
        lv_output.flags = data.flags;
        lv_output.final_sign = data.sign;

        if (data.sign == 0) begin
          lv_output.exponent_output_final = data.act_exp;
          lv_output.mantissa_output_final = {data.act_mantissa, 1'b0};
        end
        else begin
          if (data.act_mantissa[2] == 0) begin
            lv_output.exponent_output_final = signExtend(-bias);
            lv_output.mantissa_output_final = 0;
            lv_output.flags.underflow = True;
          end
          else begin
            case(data.act_mantissa[1:0])
              2'b00: begin
                lv_output.exponent_output_final = data.act_exp - 7;
                lv_output.mantissa_output_final = 4'b1010;
              end
              2'b01: begin
                lv_output.exponent_output_final = data.act_exp - 7;
                lv_output.mantissa_output_final = 4'b1100;
                lv_output.flags.overflow = True;
              end
              2'b10: begin
                lv_output.exponent_output_final = data.act_exp - 7;
                lv_output.mantissa_output_final = 4'b1111; //Round to nearest
                lv_output.flags.overflow = True;
              end
              2'b11: begin
                //$display("Hello");
                lv_output.exponent_output_final = data.act_exp - 6;
                lv_output.mantissa_output_final = 4'b1000;
                lv_output.flags.overflow = True;
              end
              default: begin
                lv_output.exponent_output_final = 0;
                lv_output.mantissa_output_final = 4'b0000;
              end
            endcase
          end
        end

        postProcess_fifof.enq(lv_output);
      end   
    endrule: mainRule

    rule rule_postprocessing(postProcess_fifof.notEmpty);
      postProcess_fifof.deq;
      let output_computed = postProcess_fifof.first;
      Cfloat_1_5_2 output_final;
      let bias = output_computed.bias;

      if (output_computed.exponent_output_final < signExtend(-bias-3)) begin
        output_computed.flags.underflow = True;
        output_final = Cfloat_1_5_2 { sign : output_computed.final_sign,
                                      exp: 0,
                                      mantissa: 2'b00
                                    };
      end
      else if (output_computed.exponent_output_final >= signExtend(-bias-3)
              && output_computed.exponent_output_final < signExtend(-bias)) begin
        //Integer number_of_shift = unpack(pack(-bias)) + unpack(pack(-compute_output.exponent_output_final));
        $display(output_computed.mantissa_output_final);
        $display(signExtend(-bias) - output_computed.exponent_output_final);
        Bit#(4) calc_mantissa_output_final = output_computed.mantissa_output_final >> (signExtend(-bias) - output_computed.exponent_output_final);
        $display(calc_mantissa_output_final);
        if (calc_mantissa_output_final[0] == 1)
          calc_mantissa_output_final = calc_mantissa_output_final + 1;
        output_final = Cfloat_1_5_2 { sign : output_computed.final_sign,
                                      exp: 0,
                                      mantissa: calc_mantissa_output_final[2:1]
                                    };
      end
      else if (output_computed.exponent_output_final > signExtend(-bias + 31)) begin
        output_final = Cfloat_1_5_2 { sign : output_computed.final_sign,
                                      exp: 31, 
                                      mantissa: 2'b11
                                    };
      end
      else begin
        $display("before: %d", output_computed.mantissa_output_final);
        if (output_computed.mantissa_output_final[0] == 1)
          output_computed.mantissa_output_final = output_computed.mantissa_output_final + 1;
        if (output_computed.mantissa_output_final == 0) begin
          output_computed.mantissa_output_final = 4'b1000;
          output_computed.exponent_output_final = output_computed.exponent_output_final + 1;
        end
        $display("After: %d", output_computed.mantissa_output_final);
        output_final = Cfloat_1_5_2 { sign : output_computed.final_sign,
                                      exp: pack(truncate(signExtend(bias) + output_computed.exponent_output_final)), 
                                      mantissa: output_computed.mantissa_output_final[2:1]
                                    };
      end

      output_fifof.enq(OutputStageMeta { out : output_final,
                                      flags : output_computed.flags
                                    });
    endrule rule_postprocessing

    method Action ma_input(Cfloat_1_5_2 inp, Int#(6) bias,Operation op);
      input_fifof.enq(PreprocessStageMeta { inp: inp,
                                         bias: bias,
                                         op: op
                                       });
    endmethod

    method ActionValue#(Maybe#(OutputStageMeta)) mav_output;
      output_fifof.deq;
      return tagged Valid output_fifof.first;
    endmethod

  endmodule
endpackage
