package yunsuan.vector.VectorALU

import chisel3._
import chisel3.util._
import yunsuan.encoding.Opcode.FixedPointRoundingMode._
import yunsuan.encoding.Opcode.VIAluOpcode
import yunsuan.vector.{SewOH, UIntSplit}

import math.pow

class VIAluMiscInput(xlen: Int) extends Bundle {
  val opcode = new VIAluOpcode
  val vsew = UInt(2.W)
  val vs2 = UInt(xlen.W)
  val vs1 = UInt(xlen.W)
  val isSigned = Bool()
  val isExt = ValidIO(new ExtInfo)
  val isNarrow = Bool()
  val vxrm = UInt(2.W)
}

class VIAluMiscOutput(xlen: Int) extends Bundle {
  val vd = UInt(xlen.W)
  val narrowVd = UInt((xlen/2).W)
}

class VIAluMisc(xlen: Int = 64) extends Module {
  val io = IO(new Bundle {
    val in = Input(new VIAluMiscInput(xlen))
    val out = Output(new VIAluMiscOutput(xlen))
  })

  private val opcode = io.in.opcode
  private val vsew = io.in.vsew
  private val vs2 = io.in.vs2
  private val vs1 = io.in.vs1
  private val isSigned = io.in.isSigned
  private val isExt = io.in.isExt.valid
  private val isVf2 = io.in.isExt.bits.isVf2
  private val isVf4 = io.in.isExt.bits.isVf4
  private val isVf8 = io.in.isExt.bits.isVf8
  private val isNarrow = io.in.isNarrow
  private val rm = io.in.vxrm

  private val sel8 = SewOH(vsew).is8
  private val sel16 = SewOH(vsew).is16
  private val sel32 = SewOH(vsew).is32
  private val sel64 = SewOH(vsew).is64

  private val isVand  = opcode.isVand
  private val isVnand = opcode.isVnand
  private val isVandn = opcode.isVandn
  private val isVor  = opcode.isVor
  private val isVxor = opcode.isVxor
  private val isVnor = opcode.isVnor
  private val isVorn = opcode.isVorn
  private val isVxnor = opcode.isVxnor
  private val isShift = opcode.isShiftLogic
  private val isLeftShiftLogic = opcode.isLeftShiftLogic
  private val isScalShiftLogic = opcode.isScalShiftLogic

  private val ext = Wire(Vec(8, UInt(8.W)))
  ext(0) := Mux1H(Seq(
    isVf2 -> Mux1H(Seq(
      sel16 -> vs2(7, 0),
      sel32 -> vs2(7, 0),
      sel64 -> vs2(7, 0),
    )),
    isVf4 -> Mux1H(Seq(
      sel32 -> vs2(7, 0),
      sel64 -> vs2(7, 0),
    )),
    isVf8 -> vs2(7, 0),
  ))
  ext(1) := Mux1H(Seq(
    isVf2 -> Mux1H(Seq(
      sel16 -> Fill(8, isSigned & vs2(7)),
      sel32 -> vs2(15, 8),
      sel64 -> vs2(15, 8),
    )),
    isVf4 -> Mux1H(Seq(
      sel32 -> Fill(8, isSigned & vs2(7)),
      sel64 -> vs2(15, 8),
    )),
    isVf8 -> Fill(8, isSigned & vs2(7)),
  ))
  ext(2) := Mux1H(Seq(
    isVf2 -> Mux1H(Seq(
      sel16 -> vs2(15, 8),
      sel32 -> Fill(8, isSigned & vs2(15)),
      sel64 -> vs2(23, 16),
    )),
    isVf4 -> Mux1H(Seq(
      sel32 -> Fill(8, isSigned & vs2(7)),
      sel64 -> Fill(8, isSigned & vs2(15)),
    )),
    isVf8 -> Fill(8, isSigned & vs2(7)),
  ))
  ext(3) := Mux1H(Seq(
    isVf2 -> Mux1H(Seq(
      sel16 -> Fill(8, isSigned & vs2(15)),
      sel32 -> Fill(8, isSigned & vs2(15)),
      sel64 -> vs2(31, 24),
    )),
    isVf4 -> Mux1H(Seq(
      sel32 -> Fill(8, isSigned & vs2(7)),
      sel64 -> Fill(8, isSigned & vs2(15)),
    )),
    isVf8 -> Fill(8, isSigned & vs2(7)),
  ))
  ext(4) := Mux1H(Seq(
    isVf2 -> Mux1H(Seq(
      sel16 -> vs2(23, 16),
      sel32 -> vs2(23, 16),
      sel64 -> Fill(8, isSigned & vs2(31)),
    )),
    isVf4 -> Mux1H(Seq(
      sel32 -> vs2(15, 8),
      sel64 -> Fill(8, isSigned & vs2(15)),
    )),
    isVf8 -> Fill(8, isSigned & vs2(7)),
  ))
  ext(5) := Mux1H(Seq(
    isVf2 -> Mux1H(Seq(
      sel16 -> Fill(8, isSigned & vs2(23)),
      sel32 -> vs2(31, 24),
      sel64 -> Fill(8, isSigned & vs2(31)),
    )),
    isVf4 -> Mux1H(Seq(
      sel32 -> Fill(8, isSigned & vs2(15)),
      sel64 -> Fill(8, isSigned & vs2(15)),
    )),
    isVf8 -> Fill(8, isSigned & vs2(7)),
  ))
  ext(6) := Mux1H(Seq(
    isVf2 -> Mux1H(Seq(
      sel16 -> vs2(31, 24),
      sel32 -> Fill(8, isSigned & vs2(31)),
      sel64 -> Fill(8, isSigned & vs2(31)),
    )),
    isVf4 -> Mux1H(Seq(
      sel32 -> Fill(8, isSigned & vs2(15)),
      sel64 -> Fill(8, isSigned & vs2(15)),
    )),
    isVf8 -> Fill(8, isSigned & vs2(7)),
  ))
  ext(7) := Mux1H(Seq(
    isVf2 -> Mux1H(Seq(
      sel16 -> Fill(8, isSigned & vs2(31)),
      sel32 -> Fill(8, isSigned & vs2(31)),
      sel64 -> Fill(8, isSigned & vs2(31)),
    )),
    isVf4 -> Mux1H(Seq(
      sel32 -> Fill(8, isSigned & vs2(15)),
      sel64 -> Fill(8, isSigned & vs2(15)),
    )),
    isVf8 -> Fill(8, isSigned & vs2(7)),
  ))

  private val extResult = Wire(UInt(xlen.W))
  extResult := ext.asUInt

  private val andResult = vs2 & vs1
  private val orResult  = vs2 | vs1
  private val xorResult = vs2 ^ vs1

  private val bitLogicResult = Wire(UInt(xlen.W))
  bitLogicResult := Mux1H(Seq(
    isVand -> andResult,
    isVor  -> orResult,
    isVxor -> xorResult,
    isVnand -> ~andResult,
    isVandn -> (vs2 & (~vs1).asUInt),
    isVnor -> ~orResult,
    isVorn -> (vs2 | (~vs1).asUInt),
    isVxnor -> ~xorResult
  ))

  private val vs2Reverse = Cat(vs2.asBools)
  private val vs2Shift = Mux(isLeftShiftLogic, vs2Reverse, vs2)
  private val vs1Shift = MuxCase(vs1, Seq(
    (isNarrow & sel8)  -> Cat(0.U(8.W), vs1(31, 24), 0.U(8.W), vs1(23, 16), 0.U(8.W), vs1(15, 8), 0.U(8.W), vs1(7, 0)),
    (isNarrow & sel16) -> Cat(0.U(16.W), vs1(31, 16), 0.U(16.W), vs1(15, 0)),
    (isLeftShiftLogic & sel8)  -> Cat(UIntSplit(vs1,  8)),
    (isLeftShiftLogic & sel16) -> Cat(UIntSplit(vs1, 16)),
    (isLeftShiftLogic & sel32) -> Cat(UIntSplit(vs1, 32)),
  ))

  private val vs2Adjust = Wire(UInt(xlen.W))
  private val vs1Adjust = Wire(UInt(xlen.W))
  vs2Adjust := vs2Shift
  vs1Adjust := vs1Shift


  def dynamicShift(data: UInt, shift: UInt, shiftRound: UInt): (UInt, UInt) = {
    val length = shift.getWidth
    val amount = pow(2, length - 1).intValue
    val tmp = data.getWidth - amount
    val shifted = Mux(shift.head(1).asBool, Cat(Fill(amount, isSigned & data.head(1).asBool), data.head(tmp)), data)
    val shiftData = Mux(shift.head(1).asBool, Cat(data.tail(tmp), shiftRound.head(tmp)), shiftRound)
    if (length == 1) (shifted, shiftData) else dynamicShift(shifted, shift.tail(1), shiftData)
  }

  // shift sew data
  def shiftOneElement(data: UInt, shift: UInt, sew: Int): (UInt, UInt) = {
    val shiftData = WireInit(0.U(data.getWidth.W))
    sew match {
      case 8  => dynamicShift(data, shift(2, 0), shiftData)
      case 16 => dynamicShift(data, shift(3, 0), shiftData)
      case 32 => dynamicShift(data, shift(4, 0), shiftData)
      case 64 => dynamicShift(data, shift(5, 0), shiftData)
    }
  }

  def shift(sew: Int): (UInt, UInt) = {
    val shift = UIntSplit(vs2Adjust, sew).zip(UIntSplit(vs1Adjust, sew)).map { case (vs2, vs1) =>
      shiftOneElement(vs2, vs1, sew)
    }
    val shifted = Mux(isLeftShiftLogic, Cat(Cat(shift.map(_._1).reverse).asBools), Cat(shift.map(_._1).reverse))
    val shiftData = Cat(shift.map(_._2).reverse)
    (shifted, shiftData)
  }

  private val (shift64, shiftData64) = shift(64)
  private val (shift32, shiftData32) = shift(32)
  private val (shift16, shiftData16) = shift(16)
  private val (shift8 , shiftData8)  = shift(8)

  def genRound(in: UInt, rIn: UInt): UInt = {
    val (g, r, s) = (in(0), rIn.head(1).asBool, rIn.tail(1))
    val roundUp = MuxLookup(
      rm,
      false.B
    )(Seq(
      RNU -> r,
      RNE -> (r & (s.orR | g)),
      RDN -> false.B,
      ROD -> (!g & rIn.orR),
    ))
    val incOne = in + 1.U
    Mux(roundUp, incOne, in)
  }

  private val scalResultSel8 = Wire(Vec(8, UInt(8.W)))
  private val scalResultSel16 = Wire(Vec(4, UInt(16.W)))
  private val scalResultSel32 = Wire(Vec(2, UInt(32.W)))
  private val scalResultSel64 = Wire(UInt(xlen.W))

  for (i <- 0 until 8) {
    scalResultSel8(i) := genRound(shift8(i * 8 + 7, i * 8), shiftData8(i * 8 + 7, i * 8))
  }
  for (i <- 0 until 4) {
    scalResultSel16(i) := genRound(shift16(i * 16 + 15, i * 16), shiftData16(i * 16 + 15, i * 16))
  }
  for (i <- 0 until 2) {
    scalResultSel32(i) := genRound(shift32(i * 32 + 31, i * 32), shiftData32(i * 32 + 31, i * 32))
  }
  scalResultSel64 := genRound(shift64, shiftData64)

  private val shiftResult = Wire(UInt(xlen.W))
  shiftResult := Mux1H(Seq(
    sel8  -> shift8,
    sel16 -> shift16,
    sel32 -> shift32,
    sel64 -> shift64,
  ))

  private val narrowResult = Wire(UInt((xlen / 2).W))
  narrowResult := Mux1H(Seq(
    sel8  -> Cat(shift16(55, 48), shift16(39, 32), shift16(23, 16), shift16(7, 0)),
    sel16 -> Cat(shift32(47, 32), shift32(15, 0)),
    sel32 -> shift64,
  ))

  private val scalResult = Wire(UInt(xlen.W))
  scalResult := Mux1H(Seq(
    sel8  -> scalResultSel8.asUInt,
    sel16 -> scalResultSel16.asUInt,
    sel32 -> scalResultSel32.asUInt,
    sel64 -> scalResultSel64,
  ))


  io.out.vd := MuxCase(bitLogicResult, Seq(
    isExt -> extResult,
    isShift -> Mux(isScalShiftLogic, scalResult, shiftResult),
  ))
  io.out.narrowVd := narrowResult

  dontTouch(isScalShiftLogic)
  dontTouch(shiftData64)
  dontTouch(shiftData32)
  dontTouch(shiftData16)
  dontTouch(shiftData8)
  dontTouch(scalResultSel8)
  dontTouch(scalResultSel16)
  dontTouch(scalResultSel32)
  dontTouch(scalResultSel64)
  dontTouch(vs1Shift)
  dontTouch(shiftResult)
  dontTouch(shift64)
  dontTouch(shift32)
  dontTouch(shift16)
  dontTouch(shift8)
}
