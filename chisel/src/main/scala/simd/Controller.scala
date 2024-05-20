package simd

import chisel3._
import chisel3.util._

class Controller extends Module {
  val io = IO(new Bundle {
    val cmd_payload = Flipped(Decoupled(new CfuInPayload()))
    val rsp_payload = Decoupled(new CfuOutPayload())

    val rsMatch     = Input(Bool())
    val addSubOpSel = Output(AddSubActivationOp())
    val mulOpSel    = Output(MulOp())
    val wenRs       = Output(Bool())
    val wenRd       = Output(Bool())
    val outputSel   = Output(OutputSel())

    val rs1   = Input(UInt(32.W))
    val rs2   = Input(UInt(32.W))
    val scale_factor = Output(SInt(8.W))
    val zero_point = Output(SInt(8.W))
  })

  val funct = io.cmd_payload.bits.funct7 ## io.cmd_payload.bits.funct3
  when(io.cmd_payload.valid & io.rsp_payload.ready) {
    // AddSubActivationUnit controll
    io.addSubOpSel := MuxLookup(
      funct,
      AddSubActivationOp.NONE,
      Seq(
        "b0000000_000".U -> AddSubActivationOp.ADDI8I8S_VV,
        "b0000000_001".U -> AddSubActivationOp.ADDI16I16S_VV,
        "b0000001_000".U -> AddSubActivationOp.SUBI8I8S_VV,
        "b0000001_001".U -> AddSubActivationOp.SUBI16I16S_VV,
        // popo
        "b1000000_000".U -> AddSubActivationOp.ADDI8I8S_VX,
        "b1000000_001".U -> AddSubActivationOp.ADDI16I16S_VX,
        "b1000001_000".U -> AddSubActivationOp.SUBI8I8S_VX,
        "b1000001_001".U -> AddSubActivationOp.SUBI16I16S_VX
      )
    )

    // AddSubActivationUnit MulUnit controll
    io.mulOpSel := MuxLookup(
      funct,
      MulOp.NONE,
      Seq(
        "b0000010_000".U -> MulOp.AMULI8I8S_VV,
        "b0000010_100".U -> MulOp.PMULI8I16S_VV_L,
        "b0000010_101".U -> Mux(
          io.rsMatch,
          MulOp.NONE,
          MulOp.PMULI8I16S_VV_H
        ),
        "b1000010_100".U -> MulOp.PMULI8I16S_VX_L,
        "b1000010_101".U -> Mux(
          io.rsMatch,
          MulOp.NONE,
          MulOp.PMULI8I16S_VX_H
        ),
        "b1000010_000".U -> MulOp.AMULI8I8S_VX_NQ,
        "b1000010_001".U -> MulOp.AMULI8I8S_VX_AQ,
        
        "b0000010_001".U -> MulOp.AMULI8I8S_VV_AQ,

        "b0000111_001".U -> MulOp.QNTI16I8S_VV_NQ,
        "b0000111_010".U -> MulOp.QNTI16I8S_VV_AQ,
        
      )
    )
    // Register controll
    io.wenRs := MuxLookup(
      funct,
      false.B,
      Seq(
        "b0000010_100".U -> true.B
      )
    )
    io.wenRd := MuxLookup(
      funct,
      false.B,
      Seq(
        "b0000010_100".U -> true.B
      )
    )
  }.otherwise {
    io.addSubOpSel := AddSubActivationOp.NONE
    io.mulOpSel    := MulOp.NONE
    io.wenRs       := false.B
    io.wenRd       := false.B
  }

  // Output controll
  io.cmd_payload.ready   := true.B
  io.rsp_payload.valid   := io.cmd_payload.valid & io.rsp_payload.ready
  io.rsp_payload.bits.rd := DontCare

  when(io.addSubOpSel =/= AddSubActivationOp.NONE) {
    io.outputSel := OutputSel.ADDSUB
  }.elsewhen(io.mulOpSel =/= MulOp.NONE) {
    io.outputSel := OutputSel.MUL
  }.otherwise {
    io.outputSel := OutputSel.REG
  }

  val rem_scale_factor = RegInit(0.S(8.W))
  val rem_zero_point = RegInit(0.S(8.W))

  when(funct === "b0000111_000".U){
    // printf("wtffff seee?? rs1: %x, rs2: %x\n", (io.rs1(7,0)).asSInt, (io.rs2(7,0)).asSInt)
    rem_scale_factor := io.rs1(7,0).asSInt
    rem_zero_point := io.rs2(7,0).asSInt
    // printf("for wire rs1: %x, rs2: %x \n", (io.rs1(7,0)).asSInt, (io.rs2(7,0)).asSInt)
    // printf("for reg rs1: %x, rs2: %x \n", rem_scale_factor, rem_zero_point)
  }

  io.scale_factor := rem_scale_factor
  io.zero_point := rem_zero_point
}
