package mkISAEX;
//import lib files
import ISA_EX_LIB::* ;
import ISA_INSTR_DEF::* ;

//interface defined
interface Isa_ex_itf;
   method Action alu_set(Alu_in alu_in);
   method Alu_out_to_reg alu_out_to_reg;
   method Alu_out_to_mem alu_out_to_mem;
 //method Alu_out_to_mem get_mem;
 //method Alu_out_to_csrreg get_csrreg;
 //method Alu_out_to_div get_div;
 //method Alu_out_to_pipectrl get_pipectrl;
endinterface 

//module function
(* synthesize *)
module mkISAEX(Isa_ex_itf);
   Wire#(Alu_in) alu_inx <- mkWire();
   Reg#(Alu_out_to_reg) alu_out_to_regx <- mkReg(unpack(0));
   Reg#(Alu_out_to_mem) alu_out_to_memx <- mkReg(unpack(0));
  
   rule rl_instr_ex; 
       let instr_ex  = instr_EX(alu_inx);
       alu_out_to_regx <= instr_ex.alu_out_to_reg;
       alu_out_to_memx <= instr_ex.alu_out_to_mem;

       Bit#(32) b_bit = pack(alu_out_to_regx)[31:0];
       $display("%d", b_bit);
   endrule


   method Alu_out_to_reg alu_out_to_reg;
      return alu_out_to_regx;
   endmethod

   method Alu_out_to_mem alu_out_to_mem;
      return alu_out_to_memx;
   endmethod

   method Action alu_set(Alu_in alu_in);
      alu_inx <= alu_in;
   endmethod
   
endmodule


endpackage
