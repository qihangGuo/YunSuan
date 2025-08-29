package yunsuan.vector.VectorALU

import chisel3._
import chisel3.util._
import yunsuan.encoding.Opcode.FixedPointConst._
import yunsuan.encoding.Opcode.FixedPointRoundingMode._
import yunsuan.encoding.Opcode.VIAluOpcode
import yunsuan.vector.{SewOH, UIntSplit}
import yunsuan.util._

import math.pow

class VIAluMiscInput(xlen: Int) extends Bundle {
  val opcode = new VIAluOpcode
  val vsew = UInt(2.W)
  val vs2 = UInt(xlen.W)
  val vs1 = UInt(xlen.W)
  val widen = Bool()
  val isSigned = Bool()
  val isExt = ValidIO(new ExtInfo)
  val isNarrow = Bool()
  val vxrm = UInt(2.W)
}

class VIAluMiscOutput(xlen: Int) extends Bundle {
  val vd = UInt(xlen.W)
  val narrowVd = UInt((xlen/2).W)
  val vxsat = UInt(8.W)
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
  private val widen = io.in.widen
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
  private val isScalVro = opcode.isScalVro
  private val isNotVro = opcode.isNotVro
  private val isScal = isShift & isScalVro & isNotVro
  private val isNClip = isNarrow & isScal
  private val isZvbbOthers = opcode.isZvbbOthers
  private val isVcpop = opcode.isVcpop
  private val isVbrev = opcode.isVbrev
  private val isVbrev8 = opcode.isVbrev8
  private val isVrev8 = opcode.isVrev8
  private val isCountZero = opcode.isCountZero
  private val isCtz = opcode.isCtz

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
    val incOne = Wire(UInt((in.getWidth + 1).W))
    incOne := in + 1.U
    val out = Mux(roundUp, incOne, in)
    out.tail(1)
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

  private val nClipResultSel8  = Wire(Vec(4, UInt(8.W)))
  private val nClipResultSel16 = Wire(Vec(2, UInt(16.W)))
  private val nClipResultSel32 = Wire(UInt((xlen / 2).W))
  private val nClipSatSel8  = Wire(Vec(4, Bool()))
  private val nClipSatSel16 = Wire(Vec(2, Bool()))
  private val nClipSatSel32 = Wire(Bool())
  for (i <- 0 until 4) {
    val upOverflowUnSign = scalResultSel16(i).head(8).orR
    val signBit = scalResultSel16(i).head(1).asBool
    val upOverflowSign = scalResultSel16(i).tail(1).head(8).orR
    val downOverflowSign = !scalResultSel16(i).tail(1).head(8).andR
    val overflowSign = Mux(signBit, downOverflowSign, upOverflowSign)

    nClipSatSel8(i) := Mux(isSigned, overflowSign, upOverflowUnSign)
    nClipResultSel8(i) := Mux(isSigned,
      Mux(overflowSign,
        Mux(signBit, signedMin, signedMax),
        scalResultSel16(i).tail(8)),
      Mux(upOverflowUnSign, unsignedMax, scalResultSel16(i).tail(8)))
  }
  for (i <- 0 until 2) {
    val upOverflowUnSign = scalResultSel32(i).head(16).orR
    val signBit = scalResultSel32(i).head(1).asBool
    val upOverflowSign = scalResultSel32(i).tail(1).head(16).orR
    val downOverflowSign = !scalResultSel32(i).tail(1).head(16).andR
    val overflowSign = Mux(signBit, downOverflowSign, upOverflowSign)

    nClipSatSel16(i) := Mux(isSigned, overflowSign, upOverflowUnSign)
    nClipResultSel16(i) := Mux(isSigned,
      Mux(overflowSign,
        Mux(signBit, Cat(signedMin, unsignedMin), Cat(signedMax, unsignedMax)),
        scalResultSel32(i).tail(16)),
      Mux(upOverflowUnSign, Fill(2, unsignedMax), scalResultSel32(i).tail(16)))
  }
  val upOverflowUnSign = scalResultSel64.head(32).orR
  val signBit = scalResultSel64.head(1).asBool
  val upOverflowSign = scalResultSel64.tail(1).head(32).orR
  val downOverflowSign = !scalResultSel64.tail(1).head(32).andR
  val overflowSign = Mux(signBit, downOverflowSign, upOverflowSign)

  nClipSatSel32 := Mux(isSigned, overflowSign, upOverflowUnSign)
  nClipResultSel32 := Mux(isSigned,
    Mux(overflowSign,
      Mux(signBit, Cat(signedMin, Fill(3, unsignedMin)), Cat(signedMax, Fill(3, unsignedMax))),
      scalResultSel64.tail(32)),
    Mux(upOverflowUnSign, Fill(4, unsignedMax), scalResultSel64.tail(32)))

  private val nClipResult = Wire(UInt((xlen / 2).W))
  nClipResult := Mux1H(Seq(
    sel8  -> nClipResultSel8.asUInt,
    sel16 -> nClipResultSel16.asUInt,
    sel32 -> nClipResultSel32,
  ))
  private val nClipSat = Wire(UInt(4.W))
  nClipSat := Mux1H(Seq(
    sel8  -> nClipSatSel8.asUInt,
    sel16 -> nClipSatSel16.asUInt,
    sel32 -> nClipSatSel32,
  ))

  def shiftRotateLeftOneElement(data: UInt, shift: UInt, sew: Int): UInt = {
    sew match {
      case 8   => doShiftRotateLeft(data, shift(2, 0)).asUInt
      case 16  => doShiftRotateLeft(data, shift(3, 0)).asUInt
      case 32  => doShiftRotateLeft(data, shift(4, 0)).asUInt
      case 64  => doShiftRotateLeft(data, shift(5, 0)).asUInt
    }
  }

  def shiftRotateRightOneElement(data: UInt, shift: UInt, sew: Int): UInt = {
    sew match {
      case 8  => doShiftRotateRight(data, shift(2, 0)).asUInt
      case 16 => doShiftRotateRight(data, shift(3, 0)).asUInt
      case 32 => doShiftRotateRight(data, shift(4, 0)).asUInt
      case 64 => doShiftRotateRight(data, shift(5, 0)).asUInt
    }
  }

  def shiftRotateLeft(data: UInt, shift: UInt, sew: Int): UInt = {
    Cat(UIntSplit(vs2, sew).zip(UIntSplit(vs1, sew)).map { case (vs2, vs1) =>
      shiftRotateLeftOneElement(vs2, vs1, sew)}.reverse)
  }

  def shiftRotateRight(data: UInt, shift: UInt, sew: Int): UInt = {
    Cat(UIntSplit(vs2, sew).zip(UIntSplit(vs1, sew)).map { case (vs2, vs1) =>
      shiftRotateRightOneElement(vs2, vs1, sew)}.reverse)
  }

  private val vroShiftRotateLeft64 = shiftRotateLeft(vs2, vs1, 64)
  private val vroShiftRotateLeft32 = shiftRotateLeft(vs2, vs1, 32)
  private val vroShiftRotateLeft16 = shiftRotateLeft(vs2, vs1, 16)
  private val vroShiftRotateLeft8  = shiftRotateLeft(vs2, vs1, 8)
  private val vroShiftRotateRight64 = shiftRotateRight(vs2, vs1, 64)
  private val vroShiftRotateRight32 = shiftRotateRight(vs2, vs1, 32)
  private val vroShiftRotateRight16 = shiftRotateRight(vs2, vs1, 16)
  private val vroShiftRotateRight8  = shiftRotateRight(vs2, vs1, 8)


  private val vroShift64 = Mux(isLeftShiftLogic, vroShiftRotateLeft64, vroShiftRotateRight64)
  private val vroShift32 = Mux(isLeftShiftLogic, vroShiftRotateLeft32, vroShiftRotateRight32)
  private val vroShift16 = Mux(isLeftShiftLogic, vroShiftRotateLeft16, vroShiftRotateRight16)
  private val vroShift8  = Mux(isLeftShiftLogic, vroShiftRotateLeft8,  vroShiftRotateRight8)

  private val vroShiftResult = Wire(UInt(xlen.W))
  vroShiftResult := Mux1H(Seq(
    sel8  -> vroShift8,
    sel16 -> vroShift16,
    sel32 -> vroShift32,
    sel64 -> vroShift64,
  ))

  def CSA3to1(a: UInt, b: UInt, cin: UInt): (UInt, UInt) = {
    val S = a ^ b ^ cin
    val C = a & b | a & cin | b & cin
    (S, C)
  }

  def PopCount8(in: UInt): UInt = {
    val (l1S1, l1C1) = CSA3to1(in(0), in(1), in(2))
    val (l1S2, l1C2) = CSA3to1(in(3), in(4), in(5))
    val l1S3 = in(6) ^ in(7)
    val l1C3 = in(6) & in(7)

    val (l2S1, l2C1) = CSA3to1(l1S1, l1S2, l1S3)
    val (l2S2, l2C2) = CSA3to1(l1C1, l1C2, l1C3)

    val count = Wire(Vec(4, Bool()))
    count(0) := l2S1
    count(1) := l2C1 ^ l2S2
    count(2) := !l2S2 & l2C2 | !l2C1 & l2C2 | l2C1 & l2S2 & !l2C2
    count(3) := l2C1 & l2S2 & l2C2
    count.asUInt
  }

  private val pop8  = Wire(Vec(8, UInt(8.W)))
  private val pop16 = Wire(Vec(4, UInt(16.W)))
  private val pop32 = Wire(Vec(2, UInt(32.W)))
  private val pop64 = Wire(UInt(xlen.W))
  for (i <- 0 until 8) {
    pop8(i) := PopCount8(vs2(i * 8 + 7, i * 8))
  }
  for (i <- 0 until 4) {
    pop16(i) := pop8(i * 2 + 1)(3, 0) +& pop8(i * 2)(3, 0)
  }
  for (i <- 0 until 2) {
    pop32(i) := pop16(i * 2 + 1)(4, 0) +& pop16(i * 2)(4, 0)
  }
  pop64 := pop32(1)(5, 0) +& pop32(0)(5, 0)

  private val popResult = Wire(UInt(xlen.W))
  popResult := Mux1H(Seq(
    sel8  -> pop8.asUInt,
    sel16 -> pop16.asUInt,
    sel32 -> pop32.asUInt,
    sel64 -> pop64
  ))

  private val brevResultSel8  = Wire(Vec(8, UInt(8.W)))
  private val brevResultSel16 = Wire(Vec(4, UInt(16.W)))
  private val brevResultSel32 = Wire(Vec(2, UInt(32.W)))
  private val brevResultSel64 = Wire(UInt(64.W))

  for (i <- 0 until 8) {
    brevResultSel8(i) := Cat(vs2(i * 8 + 7, i * 8).asBools)
  }
  for (i <- 0 until 4) {
    brevResultSel16(i) := Cat(vs2(i * 16 + 15, i * 16).asBools)
  }
  for (i <- 0 until 2) {
    brevResultSel32(i) := Cat(vs2(i * 32 + 31, i * 32).asBools)
  }
  brevResultSel64 := vs2Reverse

  private val brevResult = Wire(UInt(xlen.W))
  brevResult := Mux1H(Seq(
    sel8  -> brevResultSel8.asUInt,
    sel16 -> brevResultSel16.asUInt,
    sel32 -> brevResultSel32.asUInt,
    sel64 -> brevResultSel64.asUInt,
  ))

  private val brev8Result = Wire(UInt(xlen.W))
  brev8Result := brevResultSel8.asUInt

  private val rev8ResultSel16 = Wire(Vec(4, Vec(2, UInt(8.W))))
  private val rev8ResultSel32 = Wire(Vec(2, Vec(4, UInt(8.W))))
  private val rev8ResultSel64 = Wire(Vec(8, UInt(8.W)))
  private val rev8ResultSel16Tmp = Wire(Vec(4, Vec(2, UInt(8.W))))
  private val rev8ResultSel32Tmp = Wire(Vec(2, Vec(4, UInt(8.W))))
  private val rev8ResultSel64Tmp = Wire(Vec(8, UInt(8.W)))
  rev8ResultSel16Tmp := vs2.asTypeOf(rev8ResultSel16Tmp)
  rev8ResultSel32Tmp := vs2.asTypeOf(rev8ResultSel32Tmp)
  rev8ResultSel64Tmp := vs2.asTypeOf(rev8ResultSel64Tmp)
  for (i <- 0 until 4) {
    for (j <- 0 until 2) {
      rev8ResultSel16(i)(1 - j) := rev8ResultSel16Tmp(i)(j)
    }
  }
  for (i <- 0 until 2) {
    for (j <- 0 until 4) {
      rev8ResultSel32(i)(3 - j) := rev8ResultSel32Tmp(i)(j)
    }
  }
  for (i <- 0 until 8) {
    rev8ResultSel64(7 - i) := rev8ResultSel64Tmp(i)
  }

  private val rev8Result = Wire(UInt(xlen.W))
  rev8Result := Mux1H(Seq(
    sel8  -> vs2,
    sel16 -> rev8ResultSel16.asUInt,
    sel32 -> rev8ResultSel32.asUInt,
    sel64 -> rev8ResultSel64.asUInt,
  ))

  private val revResult = Wire(UInt(xlen.W))
  revResult := Mux1H(Seq(
    isVcpop  -> popResult,
    isVbrev  -> brevResult,
    isVbrev8 -> brev8Result,
    isVrev8  -> rev8Result,
  ))

  private val leadZeroIn = Mux(isCtz, brevResultSel8.asUInt, vs2)
  private val lzc = Lzc(leadZeroIn, 1)

  private val clzResultSel8  = Wire(Vec(8, UInt(8.W)))
  private val clzResultSel16 = Wire(Vec(4, UInt(16.W)))
  private val clzResultSel32 = Wire(Vec(2, UInt(32.W)))
  private val clzResultSel64 = Wire(UInt(64.W))

  for (i <- 0 until 8) {
    clzResultSel8(i) := lzc.io.out8.get(i)
  }
  for (i <- 0 until 4) {
    clzResultSel16(i) := lzc.io.out16.get(i)
  }
  for (i <- 0 until 2) {
    clzResultSel32(i) := lzc.io.out32.get(i)
  }
  clzResultSel64 := lzc.io.out64.get

  private val leadZeroResult = Wire(UInt(xlen.W))
  leadZeroResult := Mux1H(Seq(
    sel8  -> clzResultSel8.asUInt,
    sel16 -> clzResultSel16.asUInt,
    sel32 -> clzResultSel32.asUInt,
    sel64 -> clzResultSel64.asUInt,
  ))

  private val vs2WidenVec = Wire(Vec(8, UInt(8.W)))
  private val vs1WidenVec = Wire(Vec(4, UInt(8.W)))
  private val vs2WidenWire = Wire(UInt(xlen.W))

  vs2WidenVec(0) := vs2(7, 0)
  vs2WidenVec(1) := Mux(sel8, 0.U, vs2(15, 8))
  vs2WidenVec(2) := Mux1H(Seq(
    sel8  -> vs2(15, 8),
    sel16 -> 0.U,
    sel32 -> vs2(23, 16),
  ))
  vs2WidenVec(3) := Mux(sel32, vs2(31, 24), 0.U)
  vs2WidenVec(4) := Mux(sel32, 0.U, vs2(23, 16))
  vs2WidenVec(5) := Mux(sel16, vs2(31, 24), 0.U)
  vs2WidenVec(6) := Mux(sel8,  vs2(31, 24), 0.U)
  vs2WidenVec(7) := 0.U

  vs1WidenVec(0) := vs1(7, 0)
  vs1WidenVec(1) := Mux(sel8, vs1(15, 8), 0.U)
  vs1WidenVec(2) := Mux(sel32, 0.U, vs1(23, 16))
  vs1WidenVec(3) := Mux(sel8,  vs1(31, 24), 0.U)

  vs2WidenWire := vs2WidenVec.asUInt

  private val vwsllVs2Sel8  = Wire(Vec(4, UInt(16.W)))
  private val vwsllVs2Sel16 = Wire(Vec(2, UInt(32.W)))
  private val vwsllVs2Sel32 = Wire(UInt(64.W))

  for (i <- 0 until 4) {
    vwsllVs2Sel8(i) := Cat(vs2WidenWire(i * 16 + 15, i * 16).asBools)
  }
  for (i <- 0 until 2) {
    vwsllVs2Sel16(i) := Cat(vs2WidenWire(i * 32 + 31, i * 32).asBools)
  }
  vwsllVs2Sel32 := Cat(vs2WidenWire.asBools)

  private val vwsllResultSel8 = Wire(Vec(4, UInt(16.W)))
  private val vwsllResultSel16 = Wire(Vec(2, UInt(32.W)))
  private val vwsllResultSel32 = Wire(UInt(64.W))

  for (i <- 0 until 4) {
    vwsllResultSel8(i) := Cat(shiftOneElement(vwsllVs2Sel8(i), vs1WidenVec(i), 16)._1.asBools)
  }
  for (i <- 0 until 2) {
    vwsllResultSel16(i) := Cat(shiftOneElement(vwsllVs2Sel16(i), vs1WidenVec(i * 2), 32)._1.asBools)
  }
  vwsllResultSel32 := Cat(shiftOneElement(vwsllVs2Sel32, vs1WidenVec(0), 64)._1.asBools)

  private val vwsllResult = Wire(UInt(xlen.W))
  vwsllResult := Mux1H(Seq(
    sel8  -> vwsllResultSel8.asUInt,
    sel16 -> vwsllResultSel16.asUInt,
    sel32 -> vwsllResultSel32,
  ))

  io.out.vd := MuxCase(bitLogicResult, Seq(
    isExt -> extResult,
    isShift -> Mux(isScalVro,
                  Mux(isNotVro, scalResult, vroShiftResult),
                  Mux(widen, vwsllResult, shiftResult)),
    isZvbbOthers -> Mux(isCountZero, leadZeroResult, revResult),
  ))
  io.out.narrowVd := Mux(isNClip, nClipResult, narrowResult)
  io.out.vxsat := Mux(isNClip, nClipSat, 0.U)


  dontTouch(vs2WidenVec)
  dontTouch(vs1WidenVec)
  dontTouch(vwsllVs2Sel8)
  dontTouch(vwsllVs2Sel16)
  dontTouch(vwsllVs2Sel32)
  dontTouch(vwsllResultSel8)
  dontTouch(vwsllResultSel16)
  dontTouch(vwsllResultSel32)
  dontTouch(vwsllResult)
  dontTouch(vroShift8)
  dontTouch(vroShift16)
  dontTouch(vroShift32)
  dontTouch(vroShift64)
  dontTouch(vroShiftResult)
  dontTouch(isNClip)
  dontTouch(nClipResult)
  dontTouch(nClipResultSel8)
  dontTouch(nClipResultSel16)
  dontTouch(nClipResultSel32)
  dontTouch(nClipSatSel8)
  dontTouch(nClipSatSel16)
  dontTouch(nClipSatSel32)
  dontTouch(nClipSat)
  dontTouch(isNotVro)
  dontTouch(isScalVro)
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
