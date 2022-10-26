package ISA_INSTR_DEF;

//{{{ instraction define
// I type inst
Bit#(7) inst_TYPE_I = 7'b0010011 ; //`define INST_TYPE_I 7'b0010011
Bit#(3) inst_ADDI   = 3'b000     ; //`define INST_ADDI   3'b000
Bit#(3) inst_SLTI   = 3'b010     ; //`define INST_SLTI   3'b010
Bit#(3) inst_SLTIU  = 3'b011     ; //`define INST_SLTIU  3'b011
Bit#(3) inst_XORI   = 3'b100     ; //`define INST_XORI   3'b100
Bit#(3) inst_ORI    = 3'b110     ; //`define INST_ORI    3'b110
Bit#(3) inst_ANDI   = 3'b111     ; //`define INST_ANDI   3'b111
Bit#(3) inst_SLLI   = 3'b001     ; //`define INST_SLLI   3'b001
Bit#(3) inst_SRI    = 3'b101     ; //`define INST_SRI    3'b101

// L type inst
Bit#(7) inst_TYPE_L = 7'b0000011 ; //`define INST_TYPE_L 7'b0000011
Bit#(3) inst_LB     = 3'b000     ; //`define INST_LB     3'b000
Bit#(3) inst_LH     = 3'b001     ; //`define INST_LH     3'b001
Bit#(3) inst_LW     = 3'b010     ; //`define INST_LW     3'b010
Bit#(3) inst_LBU    = 3'b100     ; //`define INST_LBU    3'b100
Bit#(3) inst_LHU    = 3'b101     ; //`define INST_LHU    3'b101

// S type inst
Bit#(7) inst_TYPE_S = 7'b0100011 ;//`define INST_TYPE_S 7'b0100011
Bit#(3) inst_SB     = 3'b000     ;//`define INST_SB     3'b000
Bit#(3) inst_SH     = 3'b001     ;//`define INST_SH     3'b001
Bit#(3) inst_SW     = 3'b010     ;//`define INST_SW     3'b010

// R and M type inst
Bit#(7) inst_TYPE_R_M = 7'b0110011; //`define INST_TYPE_R_M 7'b0110011
// R type inst
Bit#(3) inst_ADD_SUB  = 3'b000  ; //`define INST_ADD_SUB 3'b000
Bit#(3) inst_SLL      = 3'b001  ; //`define INST_SLL    3'b001
Bit#(3) inst_SLT      = 3'b010  ; //`define INST_SLT    3'b010
Bit#(3) inst_SLTU     = 3'b011  ; //`define INST_SLTU   3'b011
Bit#(3) inst_XOR      = 3'b100  ; //`define INST_XOR    3'b100
Bit#(3) inst_SR       = 3'b101  ; //`define INST_SR     3'b101
Bit#(3) inst_OR       = 3'b110  ; //`define INST_OR     3'b110
Bit#(3) inst_AND      = 3'b111  ; //`define INST_AND    3'b111

// M type inst
Bit#(3) inst_MUL      = 3'b000  ; //`define INST_MUL    3'b000
Bit#(3) inst_MULH     = 3'b001  ; //`define INST_MULH   3'b001
Bit#(3) inst_MULHSU   = 3'b010  ; //`define INST_MULHSU 3'b010
Bit#(3) inst_MULHU    = 3'b011  ; //`define INST_MULHU  3'b011
Bit#(3) inst_DIV      = 3'b100  ; //`define INST_DIV    3'b100
Bit#(3) inst_DIVU     = 3'b101  ; //`define INST_DIVU   3'b101
Bit#(3) inst_REM      = 3'b110  ; //`define INST_REM    3'b110
Bit#(3) inst_REMU     = 3'b111  ; //`define INST_REMU   3'b111

// J type inst
Bit#(7)  inst_JAL    = 7'b1101111  ;//`define INST_JAL    7'b1101111
Bit#(7)  inst_JALR   = 7'b1100111  ;//`define INST_JALR   7'b1100111

Bit#(7)  inst_LUI    = 7'b0110111  ;//`define INST_LUI    7'b0110111
Bit#(7)  inst_AUIPC  = 7'b0010111  ;//`define INST_AUIPC  7'b0010111
Bit#(32) inst_NOP    = 32'h00000001;//`define INST_NOP    32'h00000001
Bit#(7)  inst_NOP_OP = 7'b0000001  ;//`define INST_NOP_OP 7'b0000001
Bit#(32) inst_MRET   = 32'h30200073;//`define INST_MRET   32'h30200073
Bit#(32) inst_RET    = 32'h00008067;//`define INST_RET    32'h00008067

Bit#(7)  inst_FENCE  = 7'b0001111  ;//`define INST_FENCE  7'b0001111
Bit#(32) inst_ECALL  = 32'h73      ;//`define INST_ECALL  32'h73
Bit#(32) inst_EBREAK = 32'h00100073;//`define INST_EBREAK 32'h00100073

// J type inst
Bit#(7) inst_TYPE_B  = 7'b1100011  ; //`define INST_TYPE_B 7'b1100011
Bit#(3) inst_BEQ     = 3'b000      ; //`define INST_BEQ    3'b000
Bit#(3) inst_BNE     = 3'b001      ; //`define INST_BNE    3'b001
Bit#(3) inst_BLT     = 3'b100      ; //`define INST_BLT    3'b100
Bit#(3) inst_BGE     = 3'b101      ; //`define INST_BGE    3'b101
Bit#(3) inst_BLTU    = 3'b110      ; //`define INST_BLTU   3'b110
Bit#(3) inst_BGEU    = 3'b111      ; //`define INST_BGEU   3'b111

// CSR inst
Bit#(7) inst_CSR     = 7'b1110011  ; //`define INST_CSR    7'b1110011
Bit#(3) inst_CSRRW   = 3'b001      ; //`define INST_CSRRW  3'b001
Bit#(3) inst_CSRRS   = 3'b010      ; //`define INST_CSRRS  3'b010
Bit#(3) inst_CSRRC   = 3'b011      ; //`define INST_CSRRC  3'b011
Bit#(3) inst_CSRRWI  = 3'b101      ; //`define INST_CSRRWI 3'b101
Bit#(3) inst_CSRRSI  = 3'b110      ; //`define INST_CSRRSI 3'b110
Bit#(3) inst_CSRRCI  = 3'b111      ; //`define INST_CSRRCI 3'b111

// CSR reg addr
Bit#(12) csr_CYCLE     = 12'hc00   ; //`define CSR_CYCLE   12'hc00
Bit#(12) csr_CYCLEH    = 12'hc80   ; //`define CSR_CYCLEH  12'hc80
Bit#(12) csr_MTVEC     = 12'h305   ; //`define CSR_MTVEC   12'h305
Bit#(12) csr_MCAUSE    = 12'h342   ; //`define CSR_MCAUSE  12'h342
Bit#(12) csr_MEPC      = 12'h341   ; //`define CSR_MEPC    12'h341
Bit#(12) csr_MIE       = 12'h304   ; //`define CSR_MIE     12'h304
Bit#(12) csr_MSTATUS   = 12'h300   ; //`define CSR_MSTATUS 12'h300
Bit#(12) csr_MSCRATCH  = 12'h340   ; //`define CSR_MSCRATCH 12'h340
//}}

endpackage
