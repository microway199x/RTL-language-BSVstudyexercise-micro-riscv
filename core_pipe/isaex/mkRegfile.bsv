package mkRegfile;

import Vector::* ;
import ISA_EX_LIB::* ;
import ISA_INSTR_DEF::* ;

interface Reg_itf;
   method Action reg_wr_back(Alu_out_to_reg alu_out_to_reg);
   method Reg_to_id reg_rd_out(Id_to_reg id_to_reg);
endinterface

function Int#(32) reg_rd_op(Bit#(1) reg_rd, UInt#(5) reg_raddr,
                            Bit#(1) reg_wr, UInt#(5) reg_waddr,
                            Int#(32) reg_wdata,
                            Vector#(32,Reg#(Int#(32))) regs);
   Int#(32) reg_rdata;
   if(reg_rd == 'b1) begin
      if((reg_raddr == reg_waddr) && (reg_wr == 'b1)) begin
         reg_rdata = reg_wdata;
      end 
      else begin
         reg_rdata = regs[reg_raddr];
      end 
   end 
   else begin
      reg_rdata = 0;
   end  
   return reg_rdata;
endfunction

(* synthesize *)
module mkRegfile(Reg_itf );
   Vector#(32,Reg#(Int#(32))) regs <- replicateM(mkReg(0));
   Wire#(UInt#(5)) reg_waddr <- mkWire();
   Wire#(Bit#(1))  reg_wr    <- mkWire();
   Wire#(Int#(32)) reg_wdata <- mkWire();
   
   ///{{{ interface related  ////////////////////
   method Action reg_wr_back(Alu_out_to_reg alu_out_to_reg);
       if(alu_out_to_reg.reg_wr == 'b1) begin
          regs[alu_out_to_reg.reg_waddr] <=  alu_out_to_reg.reg_wdata;
       end 
      reg_wr    <= alu_out_to_reg.reg_wr;
      reg_waddr <= alu_out_to_reg.reg_waddr;
      reg_wdata <= alu_out_to_reg.reg_wdata;
       
   endmethod
   
   method Reg_to_id reg_rd_out(Id_to_reg id_to_reg);
       let rs1_data = reg_rd_op(id_to_reg.rs1_rd,
                                id_to_reg.rs1_raddr,
                                reg_wr,
                                reg_waddr,
                                reg_wdata,
                                regs);

       let rs2_data = reg_rd_op( id_to_reg.rs2_rd,
                                 id_to_reg.rs2_raddr,
                                 reg_wr,
                                 reg_waddr,
                                 reg_wdata,
                                 regs);
       return Reg_to_id {rs1_data: rs1_data,
                         rs2_data: rs2_data};

   endmethod
      
   ///}}}
   
   
endmodule

endpackage
