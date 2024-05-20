package simd

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

object MulOp extends ChiselEnum {
  val NONE                                           = Value
  val AMULI8I8S_VV, PMULI8I16S_VV_L, PMULI8I16S_VV_H = Value
  // popo
  val PMULI8I16S_VX_L, PMULI8I16S_VX_H = Value
  val AMULI8I8S_VX_NQ, AMULI8I8S_VX_AQ = Value
  val AMULI8I8S_VV_AQ = Value
  // val SET_QNT_CONENT = Value
  val QNTI16I8S_VV_NQ, QNTI16I8S_VV_AQ = Value
}

class MulUnit extends Module {
  val io = IO(new Bundle {
    val opSel = Input(MulOp())
    val rs1   = Input(UInt(32.W))
    val rs2   = Input(UInt(32.W))
    val rdMsb = Output(UInt(32.W))
    val rd    = Output(UInt(32.W))

    val scale_factor = Input(SInt(8.W))
    val zero_point = Input(SInt(8.W))
  })

  val rs1ByteArray    = Wire(Vec(4, UInt(8.W)))
  val rs2ByteArray    = Wire(Vec(4, UInt(8.W)))
  val rdHalfArray     = Wire(Vec(4, UInt(16.W)))
  val rdMsbByteConcat = Wire(UInt(32.W))
  val rdLsbHalfConcat = Wire(UInt(32.W))
  val rdMsbHalfConcat = Wire(UInt(32.W))

  // popo
  val rdQbyteConcat = Wire(UInt(32.W))
  val rdQHalfArray    = Wire(Vec(4, UInt(16.W)))
  val abs_scale_factor = io.scale_factor.abs.asUInt
  val rdLsbByteConcat = Wire(UInt(32.W))
  
  // 8-bit wire assignment
  for (i <- 0 until 4) {
    rs1ByteArray(i) := io.rs1(8 * i + 7, 8 * i)
    rs2ByteArray(i) := io.rs2(8 * i + 7, 8 * i)

    when(io.opSel.isOneOf(MulOp.AMULI8I8S_VV, MulOp.PMULI8I16S_VV_L, MulOp.PMULI8I16S_VV_H)) {
      rdHalfArray(i) := (rs1ByteArray(i).asSInt * rs2ByteArray(i).asSInt).asUInt
    }.elsewhen(io.opSel.isOneOf(MulOp.PMULI8I16S_VX_L, MulOp.PMULI8I16S_VX_H)){ // popo
      rdHalfArray(i) := (rs1ByteArray(i).asSInt * rs2ByteArray(0).asSInt).asUInt
    }.elsewhen(io.opSel === MulOp.AMULI8I8S_VX_NQ){
      rdHalfArray(i) := (rs1ByteArray(i).asSInt * rs2ByteArray(0).asSInt).asUInt
    }.elsewhen(io.opSel === MulOp.AMULI8I8S_VX_AQ){
      when(io.scale_factor.asSInt >=0.S){
        rdHalfArray(i) := (((rs1ByteArray(i).asSInt * rs2ByteArray(0).asSInt).asSInt >> abs_scale_factor).asSInt + io.zero_point.asSInt).asUInt
      }.otherwise{
        rdHalfArray(i) := (((rs1ByteArray(i).asSInt * rs2ByteArray(0).asSInt).asSInt << abs_scale_factor).asSInt + io.zero_point.asSInt).asUInt
      }
    }.elsewhen(io.opSel === MulOp.AMULI8I8S_VV_AQ){
      when(io.scale_factor.asSInt >=0.S){
        rdHalfArray(i) := (((rs1ByteArray(i).asSInt * rs2ByteArray(i).asSInt).asSInt >> abs_scale_factor).asSInt + io.zero_point.asSInt).asUInt
      }.otherwise{
        rdHalfArray(i) := (((rs1ByteArray(i).asSInt * rs2ByteArray(i).asSInt).asSInt << abs_scale_factor).asSInt + io.zero_point.asSInt).asUInt
      }
    }.otherwise {
      rdHalfArray(i) := DontCare
    }
  }

  rdMsbByteConcat := Seq.range(3, -1, -1).map { i => rdHalfArray(i)(15, 8) }.reduce(_ ## _)
  rdLsbHalfConcat := Seq.range(1, -1, -1).map { i => rdHalfArray(i) }.reduce(_ ## _)
  rdMsbHalfConcat := Seq.range(3, 1, -1).map { i => rdHalfArray(i)}.reduce(_ ## _)
  rdLsbByteConcat := Seq.range(3, -1, -1).map { i => rdHalfArray(i)(7, 0) }.reduce(_ ## _)

  // when(io.opSel === MulOp.QNTI16I8S_VV_AQ){
  //   printf("here scale: %d, zeropoint:%d\n", io.scale_factor.asSInt, io.zero_point.asSInt)
  // }
  for (i <- 0 until 2) {
    when(io.opSel === MulOp.QNTI16I8S_VV_NQ){
      rdQHalfArray(i) := (io.rs1(16 * (i+1) - 1, 16 * i + 8).asSInt).asUInt
      rdQHalfArray(i+2) := (io.rs2(16 * (i+1) - 1, 16 * i + 8).asSInt).asUInt
    }.elsewhen(io.opSel === MulOp.QNTI16I8S_VV_AQ){
      when(io.scale_factor.asSInt >=0.S){
        rdQHalfArray(i) := ((io.rs1(16 * (i+1) - 1, 16 * i).asSInt >> abs_scale_factor).asSInt + io.zero_point.asSInt).asUInt
        rdQHalfArray(i+2) := ((io.rs2(16 * (i+1) - 1, 16 * i).asSInt >> abs_scale_factor).asSInt + io.zero_point.asSInt).asUInt
      }.otherwise{
        rdQHalfArray(i) := ((io.rs1(16 * (i+1) - 1, 16 * i).asSInt << abs_scale_factor).asSInt + io.zero_point.asSInt).asUInt
        rdQHalfArray(i+2) := ((io.rs2(16 * (i+1) - 1, 16 * i).asSInt << abs_scale_factor).asSInt + io.zero_point.asSInt).asUInt
      }
    }.otherwise{
      rdQHalfArray(i) := 0.U
      rdQHalfArray(i+2) := 0.U
    }
  }
  rdQbyteConcat := Cat(rdQHalfArray(3)(7,0), rdQHalfArray(2)(7,0), rdQHalfArray(1)(7,0), rdQHalfArray(0)(7,0)).asUInt

  // output assignment
  io.rdMsb := rdMsbHalfConcat
  io.rd    := MuxLookup(
    io.opSel.asUInt,
    DontCare,
    Seq(
      MulOp.AMULI8I8S_VV.asUInt    -> rdMsbByteConcat,
      MulOp.PMULI8I16S_VV_L.asUInt -> rdLsbHalfConcat,
      MulOp.PMULI8I16S_VV_H.asUInt -> rdMsbHalfConcat,
      // popo
      MulOp.AMULI8I8S_VV_AQ.asUInt -> rdLsbByteConcat,
      MulOp.PMULI8I16S_VX_L.asUInt -> rdLsbHalfConcat,
      MulOp.PMULI8I16S_VX_H.asUInt -> rdMsbHalfConcat,
      MulOp.AMULI8I8S_VX_NQ.asUInt -> rdMsbByteConcat,
      MulOp.QNTI16I8S_VV_NQ.asUInt -> rdQbyteConcat,
      MulOp.QNTI16I8S_VV_AQ.asUInt -> rdQbyteConcat,
      MulOp.AMULI8I8S_VX_AQ.asUInt -> rdLsbByteConcat,
    )
  )
}
