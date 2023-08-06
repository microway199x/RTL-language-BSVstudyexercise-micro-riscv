//
// Generated by Bluespec Compiler, version 2022.01-29-gc526ff54 (build c526ff54)
//
// On Thu Oct 27 07:01:14 CST 2022
//
//
// Ports:
// Name                         I/O  size props
// RDY_pipe_ctrl                  O     1 const
// get_pc_addr                    O    32 reg
// RDY_get_pc_addr                O     1 const
// CLK                            I     1 clock
// RST_N                          I     1 reset
// pipe_ctrl_alu_out_to_pipectrl  I    34
// pipe_ctrl_pc_reset             I     1
// EN_pipe_ctrl                   I     1
//
// No combinational paths from inputs to outputs
//
//

`ifdef BSV_ASSIGNMENT_DELAY
`else
  `define BSV_ASSIGNMENT_DELAY
`endif

`ifdef BSV_POSITIVE_RESET
  `define BSV_RESET_VALUE 1'b1
  `define BSV_RESET_EDGE posedge
`else
  `define BSV_RESET_VALUE 1'b0
  `define BSV_RESET_EDGE negedge
`endif

module mkPC(CLK,
	    RST_N,

	    pipe_ctrl_alu_out_to_pipectrl,
	    pipe_ctrl_pc_reset,
	    EN_pipe_ctrl,
	    RDY_pipe_ctrl,

	    get_pc_addr,
	    RDY_get_pc_addr);
  input  CLK;
  input  RST_N;

  // action method pipe_ctrl
  input  [33 : 0] pipe_ctrl_alu_out_to_pipectrl;
  input  pipe_ctrl_pc_reset;
  input  EN_pipe_ctrl;
  output RDY_pipe_ctrl;

  // value method get_pc_addr
  output [31 : 0] get_pc_addr;
  output RDY_get_pc_addr;

  // signals for module outputs
  wire [31 : 0] get_pc_addr;
  wire RDY_get_pc_addr, RDY_pipe_ctrl;

  // register pc_addr
  reg [31 : 0] pc_addr;
  wire [31 : 0] pc_addr$D_IN;
  wire pc_addr$EN;

  // remaining internal signals
  wire [31 : 0] x__h97;

  // action method pipe_ctrl
  assign RDY_pipe_ctrl = 1'd1 ;

  // value method get_pc_addr
  assign get_pc_addr = pc_addr ;
  assign RDY_get_pc_addr = 1'd1 ;

  // register pc_addr
  assign pc_addr$D_IN =
	     pipe_ctrl_pc_reset ?
	       32'h0 :
	       (pipe_ctrl_alu_out_to_pipectrl[32] ?
		  pipe_ctrl_alu_out_to_pipectrl[31:0] :
		  x__h97) ;
  assign pc_addr$EN = EN_pipe_ctrl ;

  // remaining internal signals
  assign x__h97 = pc_addr + 32'd4 ;

  // handling of inlined registers

  always@(posedge CLK)
  begin
    if (RST_N == `BSV_RESET_VALUE)
      begin
        pc_addr <= `BSV_ASSIGNMENT_DELAY 32'h0;
      end
    else
      begin
        if (pc_addr$EN) pc_addr <= `BSV_ASSIGNMENT_DELAY pc_addr$D_IN;
      end
  end

  // synopsys translate_off
  `ifdef BSV_NO_INITIAL_BLOCKS
  `else // not BSV_NO_INITIAL_BLOCKS
  initial
  begin
    pc_addr = 32'hAAAAAAAA;
  end
  `endif // BSV_NO_INITIAL_BLOCKS
  // synopsys translate_on
endmodule  // mkPC

