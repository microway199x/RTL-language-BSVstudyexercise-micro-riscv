
package mkPC;
//import lib files
import ISA_EX_LIB::* ;
import ISA_INSTR_DEF::* ;
import CPU_GLOBAL_DEF::* ;
interface Pc_itf;
   method Action pipe_ctrl(Alu_out_to_pipectrl alu_out_to_pipectrl,Bit#(1) pc_reset);
   method Bit#(32) get_pc_addr;
endinterface 


module mkPC(Pc_itf);
   Reg#(Bit#(32)) pc_addr <- mkReg(cpu_reset_addr);
   method Action pipe_ctrl(Alu_out_to_pipectrl alu_out_to_pipectrl,Bit#(1) pc_reset );
      if(pc_reset == 1) begin
         pc_addr <= cpu_reset_addr;
      end
      else if(alu_out_to_pipectrl.jump_flag == 1) begin
         pc_addr <= pack(alu_out_to_pipectrl.jump_addr);
      end
      else begin
         pc_addr <= pc_addr + 4;
      end 
   endmethod
   
   method Bit#(32) get_pc_addr;
      return pc_addr;
   endmethod
   
endmodule


endpackage
