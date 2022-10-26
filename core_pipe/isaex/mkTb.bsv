package mkTb;
import ISA_EX_LIB::* ;
import ISA_INSTR_DEF::* ;

import mkISAEX::* ;
import mkRegfile::* ;

(* synthesize *)
module mkTb();
   Reg#(UInt#(10)) counter <- mkReg('b0);
   
   Isa_ex_itf isa_ex_itf <- mkISAEX;
   Reg_itf    reg_itf    <- mkRegfile;
   rule cnt_incr;
      if(counter < 'd1023) begin
         counter <= counter + 1;
      end 
      else begin
         $finish();
      end 
   endrule
   
   rule drv_in_ISAEX;
      $display ("%d , ISAEX immd operation input",counter);
      Alu_in alu_in = unpack(0);
      alu_in.instr = {2'b0,pack(counter),5'b0,pack(counter)[2:0],5'b0,inst_TYPE_I};
      alu_in.reg_wr = 1;
      alu_in.reg_waddr = 1;
      alu_in.rs1_data = signExtend(unpack(pack(counter)));
      isa_ex_itf.alu_set(alu_in);
   endrule

   rule write_to_reg ;
      $display ("%d , IMMD operation output",isa_ex_itf.alu_out_to_reg().reg_wdata);
      $display ("%d %d , IMMD operation output",isa_ex_itf.alu_out_to_mem().mem_rd,isa_ex_itf.alu_out_to_mem().mem_raddr);
      //TODO, reg input tie 0
      let reg_to_id = reg_itf.reg_rd_out(unpack(0));
      reg_itf.reg_wr_back(isa_ex_itf.alu_out_to_reg());
   endrule
   
   
endmodule


endpackage
