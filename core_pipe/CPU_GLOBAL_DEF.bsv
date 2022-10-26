package CPU_GLOBAL_DEF;

//{{{ Global define
//`define CpuResetAddr 32'h0
//`define RstEnable 1'b0
//`define RstDisable 1'b1
//`define ZeroWord 32'h0
//`define ZeroReg 5'h0
//`define WriteEnable 1'b1
//`define WriteDisable 1'b0
//`define ReadEnable 1'b1
//`define ReadDisable 1'b0
//`define True 1'b1
//`define False 1'b0
//`define ChipEnable 1'b1
//`define ChipDisable 1'b0
//`define JumpEnable 1'b1
//`define JumpDisable 1'b0
//`define DivResultNotReady 1'b0
//`define DivResultReady 1'b1
//`define DivStart 1'b1
//`define DivStop 1'b0
//`define HoldEnable 1'b1
//`define HoldDisable 1'b0
//`define Stop 1'b1
//`define NoStop 1'b0
//`define RIB_ACK 1'b1
//`define RIB_NACK 1'b0
//`define RIB_REQ 1'b1
//`define RIB_NREQ 1'b0
//`define INT_ASSERT 1'b1
//`define INT_DEASSERT 1'b0

//`define INT_BUS 7:0
//`define INT_NONE 8'h0
//`define INT_RET 8'hff
//`define INT_TIMER0 8'b00000001
//`define INT_TIMER0_ENTRY_ADDR 32'h4

//`define Hold_Flag_Bus   2:0
//`define Hold_None 3'b000
//`define Hold_Pc   3'b001
//`define Hold_If   3'b010
//`define Hold_Id   3'b011

//`define RomNum 4096  // rom depth(how many words)

//`define MemNum 4096  // memory depth(how many words)
//`define MemBus 31:0
//`define MemAddrBus 31:0

//`define InstBus 31:0
//`define InstAddrBus 31:0

//// common regs
//`define RegAddrBus 4:0
//`define RegBus 31:0
//`define DoubleRegBus 63:0
//`define RegWidth 32
//`define RegNum 32        // reg num
//`define RegNumLog2 5
//}}

endpackage
