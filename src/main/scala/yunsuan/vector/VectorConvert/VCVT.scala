package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._

class CVTIO(width: Int) extends Bundle {
  val fire = Input(Bool())
  val src = Input(UInt(width.W))
  val opType = Input(UInt(9.W))
  val rm = Input(UInt(3.W))
  val inSew1H = Input(UInt(4.W))
  val outSew1H = Input(UInt(4.W))
  val result = Output(UInt(width.W))
  val fflags = Output(UInt(5.W))
}

abstract class CVT(width: Int) extends Module{
  val io = IO(new CVTIO(width))
}

class VCVT(width: Int) extends Module{
  val io = IO(new CVTIO(width))
  val vcvtImpl = width match {
    case 16 => Module(new CVT16(16))
    case 32 => Module(new CVT32(32))
    case 64 => Module(new CVT64(64, isVectorCvt=true))
  }
  io <> vcvtImpl.io
}
object VCVT {
  def apply(
             width: Int
           )(fire:    Bool,
             input:   UInt,
             opType:  UInt,
             rm:      UInt,
             inSew1H:      UInt,
             outSew1H:      UInt
           ): (UInt, UInt) = {
    val vcvtWraper = Module(new VCVT(width))
    vcvtWraper.io.fire := fire
    vcvtWraper.io.src := input
    vcvtWraper.io.opType := opType
    vcvtWraper.io.rm := rm
    vcvtWraper.io.inSew1H := inSew1H
    vcvtWraper.io.outSew1H := outSew1H
    (vcvtWraper.io.result, vcvtWraper.io.fflags)
  }
}