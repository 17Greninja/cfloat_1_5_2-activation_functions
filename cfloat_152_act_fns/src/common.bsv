package common;

  /* returns the hiddenBit */
  function Bit#(1) hiddenBit(Bit#(5) exponent);
    if (exponent == 0)
      return 0;
    else
      return 1;
  endfunction : hiddenBit

  function Reg#(t) readOnlyReg(t r);
   return (interface Reg;
      method t _read = r;
      method Action _write(t x) = noAction;
  endinterface);

endfunction

endpackage
