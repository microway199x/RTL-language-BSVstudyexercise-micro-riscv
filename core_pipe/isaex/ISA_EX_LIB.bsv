package ISA_EX_LIB;  
//cation: package name shall be same with filename without file extension 
import ISA_INSTR_DEF::* ;

///{{{ 1. variable define, for input and output
/// 1.1. EX related
typedef struct {
   Bit#(32)    instr      ;
   UInt#(32)   instr_addr ;
   Int#(32)    rs1_data   ;
   Int#(32)    rs2_data   ;
   Bit#(1)     reg_wr     ;     //reg write back? 0: not 1: yes
   Bit#(5)     reg_waddr  ;     //
   Bit#(1)     csr_reg_wr ;     //csr reg write back? 0: not 1: yes
   Bit#(5)     csr_waddr  ;     //
   Int#(32)    csr_rdata  ;     //
   Bit#(1)     intr_in    ;     // interrupt input
   UInt#(32)   intr_addr  ;     // interrupt addr
   UInt#(32)   mem_rdata  ;     // load from MEM
   } Alu_in deriving(Bits,FShow);

typedef struct {
   UInt#(32)  mem_raddr;
   Int#(32)   mem_wdata;
   UInt#(32)  mem_waddr;
   Bit#(1)    mem_wr;
   Bit#(1)    mem_rd;
   } Alu_out_to_mem deriving(Bits,FShow);


typedef struct {
   Int#(32)   reg_wdata;
   Bit#(1)    reg_wr;
   UInt#(5)   reg_waddr;
   } Alu_out_to_reg deriving(Bits,FShow);

typedef struct {
   Int#(32)   csrreg_wdata;
   Bit#(1)    csrreg_wr;
   UInt#(5)   csrreg_waddr;
   } Alu_out_to_csrreg deriving(Bits,FShow);

typedef struct {
   Bit#(1)    div_start_o;
   Int#(32)   div_dividend;
   Int#(32)   div_divisor;
   Bit#(3)    div_op;
   UInt#(5)   div_reg_waddr;
   } Alu_out_to_div deriving(Bits,FShow);

typedef struct {
   Bit#(1)    hold_flag;
   Bit#(1)    jump_flag;
   UInt#(32)  jump_addr;
   } Alu_out_to_pipectrl deriving(Bits,FShow);

/// 1.2. regs related
typedef struct {
   Bit#(1)    rs1_rd;
   Bit#(1)    rs2_rd;
   UInt#(5)   rs1_raddr;
   UInt#(5)   rs2_raddr;
   } Id_to_reg deriving(Bits,FShow);

typedef struct {
   Int#(32)   rs1_data;
   Int#(32)   rs2_data;
   } Reg_to_id deriving(Bits,FShow);

///}}}

///{{{ 2. operation and instraction get
function Int#(32) instr_IMMD (Bit#(32) instr);
   let immd = instr[31:20];
   return signExtend(unpack(immd));
endfunction

function Bit#(3) instr_FUNC3 (Bit#(32) instr);
   return instr[14:12];
endfunction

function Bit#(7) instr_FUNC7 (Bit#(32) instr);
   return instr[31:25];
endfunction

function UInt#(5) instr_TYPE_I_RD (Bit#(32) instr);
   return unpack(instr[11:7]);
endfunction

function Bit#(7) instr_OPCODE (Bit#(32) instr);
   return unpack(instr[6:0]);
endfunction

///}}}

///{{{ 3. instruction implement
///{{{ 3.1. I type instraction
function Alu_out_to_reg instr_TYPE_I_EX(Alu_in alu_in);
   Int#(32) reg_wdata;

   Alu_out_to_reg       alu_out_to_reg;
   alu_out_to_reg.reg_wr     = alu_in.reg_wr;
   alu_out_to_reg.reg_waddr  = instr_TYPE_I_RD(alu_in.instr);

   let immd       = instr_IMMD(alu_in.instr);
   let rs1_data   = alu_in.rs1_data;
   case(instr_FUNC3(alu_in.instr))
      inst_ADDI: begin
                    reg_wdata = rs1_data + immd; 
                 end 
      inst_SLTI: begin
                    reg_wdata = unpack({31'b0,pack(rs1_data < immd)}); 
                 end 
      inst_SLTIU: begin
                     UInt#(32) immd_u     = unpack(pack(immd));
                     UInt#(32) rs1_data_u = unpack(pack(rs1_data));
                     reg_wdata = unpack({31'b0,pack(rs1_data_u < immd_u)}); 
                 end 
      inst_XORI: begin
                    reg_wdata = rs1_data ^ immd;
                 end 
      inst_ORI: begin
                    reg_wdata = rs1_data | immd;
                 end 
      inst_ANDI: begin
                    reg_wdata = rs1_data & immd;
                 end 

      inst_SLLI: begin
                    reg_wdata = rs1_data << immd;
                 end 
      inst_SRI: begin
                    reg_wdata = rs1_data >> immd;
                end 
      default: begin 
                  reg_wdata = 0;
               end 
   
   endcase 

   alu_out_to_reg.reg_wdata  = reg_wdata;

   return alu_out_to_reg;
endfunction

///}}}end 3.1

///{{{ 3.2. R type instraction

///3.2.1 mult operation
function Int#(32) instr_TYPE_M(Alu_in alu_in);
   Int#(32) reg_wdata;
   Int#(64) rs1_data          = extend(alu_in.rs1_data);  ///value binding
   Int#(64) rs1_data_minus    = extend(-alu_in.rs1_data); ///value binding
   Int#(64) rs2_data          = extend(alu_in.rs2_data);
   UInt#(64) rs1_data_u  = unpack(pack(rs1_data));  ///value binding
   UInt#(64) rs2_data_u  = unpack(pack(rs2_data));
   let func7             = instr_FUNC7(alu_in.instr); ///function return, combinal logic
   if(func7 == 'b1) begin
       case(instr_FUNC3(alu_in.instr))
           inst_MUL: begin 
               reg_wdata = unpack(pack(rs1_data * rs2_data)[31:0]);
           end 
           inst_MULH: begin 
               reg_wdata = unpack(pack(rs1_data * rs2_data)[63:32]);
           end 
           inst_MULHU: begin 
               reg_wdata = unpack(pack(rs1_data_u * rs2_data_u)[63:32]);
           end 
           inst_MULHSU: begin 
               if(pack(rs1_data)[31] == 'b1) begin
                    reg_wdata = unpack(pack(rs1_data_minus * rs2_data)[63:32]);
               end 
               else begin 
                    reg_wdata = unpack(pack(rs1_data * rs2_data)[63:32]);
               end 
           end
           default: begin
               reg_wdata = 0;
           end
       endcase
   end 
   else begin 
       reg_wdata = 0;
   end 

   return reg_wdata;
endfunction

///3.2.2 all R instraction 
function Alu_out_to_reg instr_TYPE_RM_EX(Alu_in alu_in);
   Int#(32) reg_wdata;

   Alu_out_to_reg       alu_out_to_reg;
   alu_out_to_reg.reg_wr     = alu_in.reg_wr;
   alu_out_to_reg.reg_waddr  = instr_TYPE_I_RD(alu_in.instr);

   let immd       = instr_IMMD(alu_in.instr);
   let rs1_data   = alu_in.rs1_data;  ///value binding
   let rs2_data   = alu_in.rs2_data;
   UInt#(32) rs1_data_u  = unpack(pack(rs1_data));  ///value binding
   UInt#(32) rs2_data_u  = unpack(pack(rs2_data));
   Bit#(32) rs2_data_b   = pack(rs2_data);
   let func7      = instr_FUNC7(alu_in.instr); ///function return, combinal logic
   if(func7 == 'b1) begin
      reg_wdata = instr_TYPE_M(alu_in);
   end 
   else begin
        case(instr_FUNC3(alu_in.instr))
            inst_ADD_SUB: begin 
                                if(func7 == 'b0) begin
                                    reg_wdata = rs1_data + rs2_data; 
                                end
                                else if(func7 == 'b010_0000) begin
                                    reg_wdata = rs1_data - rs2_data; 
                                end 
                                else begin
                                    reg_wdata = 0; 
                                end
                            end 
            inst_SLL: begin //logic shift
                        reg_wdata = unpack(pack(rs1_data_u << rs2_data_b[4:0]));
                        end 
            inst_SLT: begin //less than
                            reg_wdata = unpack({31'b0,pack(rs1_data < rs2_data)}); 
                        end 
            inst_SLTU: begin
                            reg_wdata = unpack({31'b0,pack(rs1_data_u < rs2_data_u)}); 
                        end 
            inst_XOR: begin
                        reg_wdata = rs1_data ^ rs2_data;
                        end 
            inst_SR:  begin
                            reg_wdata = rs1_data >> rs2_data_b[4:0];
                        end 
            inst_OR:  begin
                            reg_wdata = rs1_data | rs2_data;
                        end 
            inst_AND: begin
                            reg_wdata = rs1_data & rs2_data;
                        end 
            default: begin 
                        reg_wdata = 'b0;
                    end 
            endcase 
      end 

   alu_out_to_reg.reg_wdata  = reg_wdata;

   return alu_out_to_reg;
endfunction

///}}}end 3.2

///{{{ 3.3 Load instraction
typedef  struct {
    Alu_out_to_reg alu_out_to_reg;
    Alu_out_to_mem alu_out_to_mem;
    } Instr_L_out deriving(Bits);

function Instr_L_out instr_TYPE_L(Alu_in alu_in);
   Int#(32) reg_wdata;
   Bit#(1)  mem_rd;

   let alu_out_to_mem = Alu_out_to_mem{
                           mem_raddr:0,
                           mem_wdata:0,
                           mem_waddr:0,
                           mem_wr: 'b0,
                           mem_rd: 'b0 
                           };
   let alu_out_to_reg = Alu_out_to_reg{ reg_wdata: 0,
                            reg_wr   : 'b0,
                            reg_waddr: 0 
                           };
   alu_out_to_reg.reg_wr     = alu_in.reg_wr;
   alu_out_to_reg.reg_waddr  = instr_TYPE_I_RD(alu_in.instr);

   let mem_raddr = alu_in.rs1_data + instr_IMMD(alu_in.instr); 
   let mem_rdata = pack(alu_in.mem_rdata);
   Bit#(2) mem_raddr_byte_index = pack(mem_raddr)[1:0]; 
   
   case(instr_FUNC3(alu_in.instr))
      inst_LB: begin  //Load byte
                  mem_rd = 'b1;
                  case(mem_raddr_byte_index)
                     'd0: reg_wdata     = signExtend(unpack(mem_rdata[ 7: 0]));
                     'd1: reg_wdata     = signExtend(unpack(mem_rdata[15: 8]));
                     'd2: reg_wdata     = signExtend(unpack(mem_rdata[23:16]));
                     default: reg_wdata = signExtend(unpack(mem_rdata[31:24]));
                  endcase 
      end 
      inst_LH: begin //Load half
                  mem_rd = 'b1;
                  if(mem_raddr_byte_index == 'b0) begin
                     reg_wdata = signExtend(unpack(mem_rdata[15:0]));
                  end 
                  else begin
                     reg_wdata = signExtend(unpack(mem_rdata[31:16]));
                  end 
      end
      inst_LW: begin //Load Word
                  mem_rd = 'b1;
                  reg_wdata = unpack(mem_rdata);
      end 
      inst_LBU: begin  //Load byte
                  mem_rd = 'b1;
                   case(mem_raddr_byte_index)
                     'd0: reg_wdata     = unpack({24'b0,mem_rdata[ 7: 0]});
                     'd1: reg_wdata     = unpack({24'b0,mem_rdata[15: 8]});
                     'd2: reg_wdata     = unpack({24'b0,mem_rdata[23:16]});
                     default: reg_wdata = unpack({24'b0,mem_rdata[31:24]});
                   endcase 
      end 
      inst_LHU: begin //Load half
                  mem_rd = 'b1;
                  if(mem_raddr_byte_index == 'b0) begin
                     reg_wdata = unpack({16'b0,mem_rdata[15:0]});
                  end 
                  else begin
                     reg_wdata = unpack({16'b0,mem_rdata[31:16]});
                  end 
      end
      default: begin
                  mem_rd = 'b1;
                  reg_wdata = 0;
      end 
  endcase

  alu_out_to_mem.mem_raddr     = unpack(pack(mem_raddr)); 
  alu_out_to_mem.mem_rd        = mem_rd;

  return Instr_L_out{alu_out_to_reg:alu_out_to_reg,
                     alu_out_to_mem:alu_out_to_mem};
   
   
endfunction

///}}}

///}}} end 3


// function: all EX commands

typedef  struct {
    Alu_out_to_reg alu_out_to_reg;
    Alu_out_to_mem alu_out_to_mem;
    } Instr_EX_out deriving(Bits);

function Instr_EX_out instr_EX(Alu_in alu_in);
    Alu_out_to_reg       alu_out_to_reg     ;
    Alu_out_to_mem       alu_out_to_mem     ;
  //Alu_out_to_div       alu_out_to_div     ;
  //Alu_out_to_pipectrl  alu_out_to_pipectrl;
  //Alu_out_to_csrreg    alu_out_to_csrreg  ; 

    case(instr_OPCODE(alu_in.instr))
        inst_TYPE_I: begin
                         alu_out_to_reg      = instr_TYPE_I_EX(alu_in);
                          alu_out_to_mem = Alu_out_to_mem{
                                                 mem_raddr:0,
                                                 mem_wdata:0,
                                                 mem_waddr:0,
                                                 mem_wr: 'b0,
                                                 mem_rd: 'b0 
                                                 };
                       //alu_out_to_div      = unpack(0);
                       //alu_out_to_pipectrl = unpack(0);
                       //alu_out_to_csrreg   = unpack(0); 
                     end 
        inst_TYPE_R_M: begin
                           alu_out_to_reg      = instr_TYPE_RM_EX(alu_in);
                           alu_out_to_mem = Alu_out_to_mem{
                                                  mem_raddr:0,
                                                  mem_wdata:0,
                                                  mem_waddr:0,
                                                  mem_wr: 'b0,
                                                  mem_rd: 'b0 
                                                  };
                       end 
        inst_TYPE_L: begin
                           let inst_load      = instr_TYPE_L(alu_in);
                           alu_out_to_reg     = inst_load.alu_out_to_reg;
                           alu_out_to_mem     = inst_load.alu_out_to_mem;
                     end 
        default: begin
                     alu_out_to_mem = Alu_out_to_mem{
                                            mem_raddr:0,
                                            mem_wdata:0,
                                            mem_waddr:0,
                                            mem_wr: 'b0,
                                            mem_rd: 'b0 
                                            };
                     alu_out_to_reg = Alu_out_to_reg{ reg_wdata: 0,
                                             reg_wr   : 'b0,
                                             reg_waddr: 0 
                                            };
                  //alu_out_to_div      = unpack(0);
                  //alu_out_to_pipectrl = unpack(0);
                  //alu_out_to_csrreg   = unpack(0); 
                 end 
    endcase

    return Instr_EX_out{alu_out_to_reg:alu_out_to_reg,
                        alu_out_to_mem:alu_out_to_mem};
endfunction

endpackage
