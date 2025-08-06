package yunsuan.vector.VectorALU

import chisel3._
import chisel3.util._
import yunsuan.encoding.Opcode.VIAluOpcode
import yunsuan.vector.SewOH

class VIAluAdderInput(xlen: Int) extends Bundle {
  val opcode = new VIAluOpcode
  val vs2 = UInt(xlen.W)
  val vs1 = UInt(xlen.W)
  val vsew = UInt(2.W)
  val vm = Bool()
  val widenVs2 = Bool()
  val widen = Bool()
  val isSigned = Bool()
  val mask = UInt(8.W)
  val isAddCarry = Bool()
}

class VIAluAdderOutput(xlen: Int) extends Bundle {
  val vd = UInt(xlen.W)
  val addCarryCmpMask = UInt(8.W)
  val vxsat = UInt(8.W)
}

class VIAluAdder(xlen: Int = 64) extends Module {
  val io = IO(new Bundle {
    val in = Input(new VIAluAdderInput(xlen))
    val out = Output(new VIAluAdderOutput(xlen))
  })

  private val opcode = io.in.opcode
  private val vsew = io.in.vsew
  private val vm = io.in.vm
  private val vs2 = io.in.vs2
  private val vs1 = io.in.vs1
  private val widenVs2 = io.in.widenVs2
  private val widen = io.in.widen
  private val isSigned = io.in.isSigned
  private val mask = io.in.mask
  private val isAddCarry = io.in.isAddCarry

  private val sel8 = SewOH(vsew).is8
  private val sel16 = SewOH(vsew).is16
  private val sel32 = SewOH(vsew).is32
  private val sel64 = SewOH(vsew).is64

  private val isSub = opcode.isSub
  private val isVmsbc = opcode.isVmsbc

  private val isCmpEq = opcode.isCmpEq
  private val isCmpLt = opcode.isCmpLt
  private val isCmpNe = opcode.isCmpNe
  private val isCmpLe = opcode.isCmpLe
  private val isCmpGt = opcode.isCmpGt

  private val isMaxMin = opcode.isMaxMinLogic
  private val isMax = opcode.isMax

  private val isSat = opcode.isSatLogic

  private val vs2_0 = Wire(UInt(8.W))
  private val vs2_1 = Wire(UInt(8.W))
  private val vs2_2 = Wire(UInt(8.W))
  private val vs2_3 = Wire(UInt(8.W))
  private val vs2_4 = Wire(UInt(8.W))
  private val vs2_5 = Wire(UInt(8.W))
  private val vs2_6 = Wire(UInt(8.W))
  private val vs2_7 = Wire(UInt(8.W))

  private val vs1_0 = Wire(UInt(8.W))
  private val vs1_1 = Wire(UInt(8.W))
  private val vs1_2 = Wire(UInt(8.W))
  private val vs1_3 = Wire(UInt(8.W))
  private val vs1_4 = Wire(UInt(8.W))
  private val vs1_5 = Wire(UInt(8.W))
  private val vs1_6 = Wire(UInt(8.W))
  private val vs1_7 = Wire(UInt(8.W))

  vs2_0 := vs2(7, 0)
  vs2_1 := Mux(widenVs2, Mux1H(Seq(
    sel8  -> Fill(8, isSigned & vs2(7)),
    sel16 -> vs2(15, 8),
    sel32 -> vs2(15, 8),
  )), vs2(15, 8))
  vs2_2 := Mux(widenVs2, Mux1H(Seq(
    sel8  -> vs2(15, 8),
    sel16 -> Fill(8, isSigned & vs2(15)),
    sel32 -> vs2(23, 16),
  )), vs2(23, 16))
  vs2_3 := Mux(widenVs2, Mux1H(Seq(
    sel8  -> Fill(8, isSigned & vs2(15)),
    sel16 -> Fill(8, isSigned & vs2(15)),
    sel32 -> vs2(31, 24),
  )), vs2(31, 24))
  vs2_4 := Mux(widenVs2, Mux1H(Seq(
    sel8  -> vs2(23, 16),
    sel16 -> vs2(23, 16),
    sel32 -> Fill(8, isSigned & vs2(31)),
  )), vs2(39, 32))
  vs2_5 := Mux(widenVs2, Mux1H(Seq(
    sel8  -> Fill(8, isSigned & vs2(23)),
    sel16 -> vs2(31, 24),
    sel32 -> Fill(8, isSigned & vs2(31)),
  )), vs2(47, 40))
  vs2_6 := Mux(widenVs2, Mux1H(Seq(
    sel8  -> vs2(31, 24),
    sel16 -> Fill(8, isSigned & vs2(31)),
    sel32 -> Fill(8, isSigned & vs2(31)),
  )), vs2(55, 48))
  vs2_7 := Mux(widenVs2, Mux1H(Seq(
    sel8  -> Fill(8, isSigned & vs2(31)),
    sel16 -> Fill(8, isSigned & vs2(31)),
    sel32 -> Fill(8, isSigned & vs2(31)),
  )), vs2(63, 56))

  vs1_0 := vs1(7, 0)
  vs1_1 := Mux(widen, Mux1H(Seq(
    sel8  -> Fill(8, isSigned & vs1(7)),
    sel16 -> vs1(15, 8),
    sel32 -> vs1(15, 8),
  )), vs1(15,  8))
  vs1_2 := Mux(widen, Mux1H(Seq(
    sel8  -> vs1(15, 8),
    sel16 -> Fill(8, isSigned & vs1(15)),
    sel32 -> vs1(23, 16),
  )), vs1(23, 16))
  vs1_3 := Mux(widen, Mux1H(Seq(
    sel8  -> Fill(8, isSigned & vs1(15)),
    sel16 -> Fill(8, isSigned & vs1(15)),
    sel32 -> vs1(31, 24),
  )), vs1(31, 24))
  vs1_4 := Mux(widen, Mux1H(Seq(
    sel8  -> vs1(23, 16),
    sel16 -> vs1(23, 16),
    sel32 -> Fill(8, isSigned & vs1(31)),
  )), vs1(39, 32))
  vs1_5 := Mux(widen, Mux1H(Seq(
    sel8 -> Fill(8, isSigned & vs1(23)),
    sel16 -> vs1(31, 24),
    sel32 -> Fill(8, isSigned & vs1(31)),
  )), vs1(47, 40))
  vs1_6 := Mux(widen, Mux1H(Seq(
    sel8 -> vs1(31, 24),
    sel16 -> Fill(8, isSigned & vs1(31)),
    sel32 -> Fill(8, isSigned & vs1(31)),
  )), vs1(55, 48))
  vs1_7 := Mux(widen, Mux1H(Seq(
    sel8 -> Fill(8, isSigned & vs1(31)),
    sel16 -> Fill(8, isSigned & vs1(31)),
    sel32 -> Fill(8, isSigned & vs1(31)),
  )), vs1(63, 56))

  private val srcbInv = Wire(Vec(8, UInt(8.W)))
  srcbInv(0) := Mux(isSub, ~vs1_0, vs1_0)
  srcbInv(1) := Mux(isSub, ~vs1_1, vs1_1)
  srcbInv(2) := Mux(isSub, ~vs1_2, vs1_2)
  srcbInv(3) := Mux(isSub, ~vs1_3, vs1_3)
  srcbInv(4) := Mux(isSub, ~vs1_4, vs1_4)
  srcbInv(5) := Mux(isSub, ~vs1_5, vs1_5)
  srcbInv(6) := Mux(isSub, ~vs1_6, vs1_6)
  srcbInv(7) := Mux(isSub, ~vs1_7, vs1_7)

  private val cIn = Wire(Vec(8, Bool()))
  cIn(0) := Mux(isAddCarry, Mux(vm, isSub, isSub ^ mask(0)), isSub)
  cIn(1) := Mux(isAddCarry,
    Mux1H(Seq(
      sel8  -> Mux(vm, isSub, isSub ^ mask(1)),
      sel16 -> 0.U,
      sel32 -> 0.U,
      sel64 -> 0.U,
    )),
    isSub & Mux(widenVs2, Mux1H(Seq(
      sel8  -> 0.U,
      sel16 -> 0.U,
      sel32 -> 0.U,
    )), sel8)
  )
  cIn(2) := Mux(isAddCarry,
    Mux1H(Seq(
      sel8  -> Mux(vm, isSub, isSub ^ mask(2)),
      sel16 -> Mux(vm, isSub, isSub ^ mask(1)),
      sel32 -> 0.U,
      sel32 -> 0.U,
    )),
    isSub & Mux(widenVs2, Mux1H(Seq(
      sel8  -> 1.U,
      sel16 -> 0.U,
      sel32 -> 0.U,
    )), sel8 | sel16)
  )
  cIn(3) := Mux(isAddCarry,
    Mux1H(Seq(
      sel8  -> Mux(vm, isSub, isSub ^ mask(3)),
      sel16 -> 0.U,
      sel32 -> 0.U,
      sel32 -> 0.U,
    )),
    isSub & Mux(widenVs2, Mux1H(Seq(
      sel8  -> 0.U,
      sel16 -> 0.U,
      sel32 -> 0.U,
    )), sel8)
  )
  cIn(4) := Mux(isAddCarry,
    Mux1H(Seq(
      sel8  -> Mux(vm, isSub, isSub ^ mask(4)),
      sel16 -> Mux(vm, isSub, isSub ^ mask(2)),
      sel32 -> Mux(vm, isSub, isSub ^ mask(1)),
      sel32 -> 0.U,
    )),
    isSub & Mux(widenVs2, Mux1H(Seq(
      sel8  -> 1.U,
      sel16 -> 1.U,
      sel32 -> 0.U,
    )), ~sel64)
  )
  cIn(5) := Mux(isAddCarry,
    Mux1H(Seq(
      sel8  -> Mux(vm, isSub, isSub ^ mask(5)),
      sel16 -> 0.U,
      sel32 -> 0.U,
      sel32 -> 0.U,
    )),
    isSub & Mux(widenVs2, Mux1H(Seq(
      sel8  -> 0.U,
      sel16 -> 0.U,
      sel32 -> 0.U,
    )), sel8)
  )
  cIn(6) := Mux(isAddCarry,
    Mux1H(Seq(
      sel8  -> Mux(vm, isSub, isSub ^ mask(6)),
      sel16 -> Mux(vm, isSub, isSub ^ mask(3)),
      sel32 -> 0.U,
      sel32 -> 0.U,
    )),
    isSub & Mux(widenVs2, Mux1H(Seq(
      sel8  -> 1.U,
      sel16 -> 0.U,
      sel32 -> 0.U,
    )), sel8 | sel16)
  )
  cIn(7) := Mux(isAddCarry,
    Mux1H(Seq(
      sel8  -> Mux(vm, isSub, isSub ^ mask(7)),
      sel16 -> 0.U,
      sel32 -> 0.U,
      sel32 -> 0.U,
    )),
    isSub & Mux(widenVs2, Mux1H(Seq(
      sel8  -> 0.U,
      sel16 -> 0.U,
      sel32 -> 0.U,
    )), sel8)
  )

  private val srca = Wire(Vec(8, UInt(8.W)))
  private val srcb = Wire(Vec(8, UInt(8.W)))
  private val srcaPre = Wire(Vec(8, Bool()))
  private val srcbPre = WireInit(VecInit(Seq.fill(8)(false.B)))
  private val addSrc1 = Wire(UInt(72.W))
  private val addSrc2 = Wire(UInt(72.W))
  private val addSrc3 = Wire(UInt(72.W))

  private val addResult = Wire(UInt(72.W))

  srca(0) := vs2_0
  srca(1) := vs2_1
  srca(2) := vs2_2
  srca(3) := vs2_3
  srca(4) := vs2_4
  srca(5) := vs2_5
  srca(6) := vs2_6
  srca(7) := vs2_7

  srcb(0) := srcbInv(0)
  srcb(1) := srcbInv(1)
  srcb(2) := srcbInv(2)
  srcb(3) := srcbInv(3)
  srcb(4) := srcbInv(4)
  srcb(5) := srcbInv(5)
  srcb(6) := srcbInv(6)
  srcb(7) := srcbInv(7)

  srcaPre(0) := Mux(widenVs2,
    Mux1H(Seq(
      sel8  -> 1.U,
      sel16 -> 1.U,
      sel32 -> 1.U,
    )),
    Mux1H(Seq(
      sel8  -> 0.U,
      sel16 -> 1.U,
      sel32 -> 1.U,
      sel64 -> 1.U,
    ))
  )
  srcaPre(1) := Mux(widenVs2,
    Mux1H(Seq(
      sel8  -> 0.U,
      sel16 -> 1.U,
      sel32 -> 1.U,
    )),
    Mux1H(Seq(
      sel8  -> 0.U,
      sel16 -> 0.U,
      sel32 -> 1.U,
      sel64 -> 1.U,
    ))
  )
  srcaPre(2) := Mux(widenVs2,
    Mux1H(Seq(
      sel8  -> 1.U,
      sel16 -> 1.U,
      sel32 -> 1.U,
    )),
    Mux1H(Seq(
      sel8  -> 0.U,
      sel16 -> 1.U,
      sel32 -> 1.U,
      sel64 -> 1.U,
    ))
  )
  srcaPre(3) := Mux(widenVs2,
    Mux1H(Seq(
      sel8  -> 0.U,
      sel16 -> 0.U,
      sel32 -> 1.U,
    )),
    Mux1H(Seq(
      sel8  -> 0.U,
      sel16 -> 0.U,
      sel32 -> 0.U,
      sel64 -> 1.U,
    ))
  )
  srcaPre(4) := Mux(widenVs2,
    Mux1H(Seq(
      sel8  -> 1.U,
      sel16 -> 1.U,
      sel32 -> 1.U,
    )),
    Mux1H(Seq(
      sel8  -> 0.U,
      sel16 -> 1.U,
      sel32 -> 1.U,
      sel64 -> 1.U,
    ))
  )
  srcaPre(5) := Mux(widenVs2,
    Mux1H(Seq(
      sel8  -> 0.U,
      sel16 -> 1.U,
      sel32 -> 1.U,
    )),
    Mux1H(Seq(
      sel8  -> 0.U,
      sel16 -> 0.U,
      sel32 -> 1.U,
      sel64 -> 1.U,
    ))
  )
  srcaPre(6) := Mux(widenVs2,
    Mux1H(Seq(
      sel8  -> 1.U,
      sel16 -> 1.U,
      sel32 -> 1.U,
    )),
    Mux1H(Seq(
      sel8  -> 0.U,
      sel16 -> 1.U,
      sel32 -> 1.U,
      sel64 -> 1.U,
    ))
  )
  srcaPre(7) := 0.U

  addSrc1 := Cat(VecInit(srcaPre.zip(srca).map { case (pre, a) =>
    Cat(pre, a)
  }).reverse)

  addSrc2 := Cat(VecInit(srcbPre.zip(srcb).map { case (pre, b) =>
    Cat(pre, b)
  }).reverse)

  addSrc3 := Cat(VecInit(cIn.map(in =>
    Cat(0.U(8.W), in)
  )).reverse)

  private val l0S = Wire(UInt(72.W))  // [71:0]
  private val l0C = Wire(UInt(72.W))  // [72:1]

  l0S := addSrc1 ^ addSrc2 ^ addSrc3
  l0C := addSrc1 & addSrc2 | addSrc2 & addSrc3 | addSrc1 & addSrc3

  addResult := l0S + Cat(l0C.tail(1), 0.U)

  private val originResult = Wire(Vec(8, UInt(8.W)))
  originResult(0) := addResult( 7,  0)
  originResult(1) := addResult(16,  9)
  originResult(2) := addResult(25, 18)
  originResult(3) := addResult(34, 27)
  originResult(4) := addResult(43, 36)
  originResult(5) := addResult(52, 45)
  originResult(6) := addResult(61, 54)
  originResult(7) := addResult(70, 63)

  private val originAddResult = Wire(UInt(xlen.W))
  originAddResult := originResult.asUInt

  private val cout = Wire(Vec(8, Bool()))
  for (i <- 0 until 8) {
    cout(i) := addResult(i * 9 + 8)
  }
  private val coutResult = Wire(UInt(8.W))
  coutResult := cout.asUInt

  // compare
  private val ltVec = Wire(Vec(8, Bool()))
  private val eqVec = Wire(Vec(8, Bool()))
  for (i <- 0 until 8) {
    ltVec(i) := Mux(isSigned, srca(i)(7) ^ srcb(i)(7) ^ cout(i), ~cout(i))
    eqVec(i) := vs2(i * 8 + 7, i * 8) === vs1(i * 8 + 7, i * 8)
  }

  private val eqResult = Wire(UInt(8.W))
  eqResult := Mux1H(Seq(
    sel8 -> eqVec.asUInt,
    sel16 -> Cat(Fill(2, eqVec(7) & eqVec(6)), Fill(2, eqVec(5) & eqVec(4)),
      Fill(2, eqVec(3) & eqVec(2)), Fill(2, eqVec(1) & eqVec(0))),
    sel32 -> Cat(Fill(4, eqVec(7) & eqVec(6) & eqVec(5) & eqVec(4)),
      Fill(4, eqVec(3) & eqVec(2) & eqVec(1) & eqVec(0))),
    sel64 -> Fill(8, eqVec(7) & eqVec(6) & eqVec(5) & eqVec(4) & eqVec(3) & eqVec(2) & eqVec(1) & eqVec(0))
  ))

  private val ltResult = Wire(UInt(8.W))
  ltResult := ltVec.asUInt

  private val cmpResult = Wire(UInt(8.W))
  cmpResult := Mux1H(Seq(
    isCmpEq -> eqResult,
    isCmpNe -> ~eqResult,
    isCmpLt -> ltResult,
    isCmpLe -> (eqResult | ltResult),
    isCmpGt -> ~(eqResult | ltResult),
  ))

  private val carryCoutCmp = Wire(UInt(8.W))
  carryCoutCmp := Mux(isAddCarry, Mux(isVmsbc, (~coutResult).asUInt, coutResult), cmpResult)

  private val addCarryCmpMask = Wire(UInt(8.W))
  addCarryCmpMask := Mux1H(Seq(
    sel8  -> carryCoutCmp,
    sel16 -> Cat(0.U(4.W), carryCoutCmp(7), carryCoutCmp(5), carryCoutCmp(3), carryCoutCmp(1)),
    sel32 -> Cat(0.U(6.W), carryCoutCmp(7), carryCoutCmp(3)),
    sel64 -> Cat(0.U(7.W), carryCoutCmp(7)),
  ))

  // max/min
  def selectPos(in: UInt, i: Int): Bool = {
    Mux1H(Seq(
      sel8  -> in(i),
      sel16 -> in((i / 2) * 2 + 1),
      sel32 -> in((i / 4) * 4 + 3),
      sel64 -> in(7),
    ))
  }

  private val maxMinVec = Wire(Vec(8, UInt(8.W)))
  for (i <- 0 until 8) {
    val select = selectPos(ltResult, i)
    maxMinVec(i) := Mux(select ^ isMax, vs2(i * 8 + 7, i * 8), vs1(i * 8 + 7, i * 8))
  }
  private val maxMinResult = Wire(UInt(xlen.W))
  maxMinResult := maxMinVec.asUInt

  // saturating add/sub
  // can move to the 2 cycle
  private val vs2Head = Wire(Vec(8, Bool()))
  private val vs1Head = Wire(Vec(8, Bool()))
  private val vdHead  = Wire(Vec(8, Bool()))
  private val coutSat = Wire(Vec(8, Bool()))
  private val satVec  = Wire(Vec(8, UInt(8.W)))

  private val overflowSign = Wire(UInt(8.W))
  overflowSign := Mux1H(Seq(
    sel8  -> "hff".U,
    sel16 -> "haa".U,
    sel32 -> "h88".U,
    sel64 -> "h80".U,
  ))

  for (i <- 0 until 8) {
    vs2Head(i) := vs2(i * 8 + 7)
    vs1Head(i) := vs1(i * 8 + 7)
    vdHead(i) := originResult(i)(7)
    coutSat(i) := Mux(isSigned,
      Mux(isSub, !vs2Head(i) & vs1Head(i) & vdHead(i) | vs2Head(i) & !vs1Head(i) & !vdHead(i),
        vs2Head(i) & vs1Head(i) & !vdHead(i) | !vs2Head(i) & !vs1Head(i) & vdHead(i)),
      cout(i) ^ isSub)
  }

  private val sat = Wire(UInt(8.W))
  sat := Mux1H(Seq(
    sel8 -> coutSat.asUInt,
    sel16 -> Cat(Fill(2, coutSat(7)), Fill(2, coutSat(5)), Fill(2, coutSat(3)), Fill(2, coutSat(1))),
    sel32 -> Cat(Fill(4, coutSat(7)), Fill(4, coutSat(3))),
    sel64 -> Fill(8, coutSat(7)),
  ))

  for (i <- 0 until 8) {
    val upOverflowUnSign = selectPos(coutResult, i)
    val downOverflowSign = selectPos(vs2Head.asUInt, i)

    satVec(i) := Mux(sat(i),
      Mux(isSigned,
        Mux(downOverflowSign, Cat(overflowSign(i), 0.U(7.W)), Cat(~overflowSign(i), "h7f".U(7.W))),
        Mux(upOverflowUnSign, "hff".U, "h00".U)),
      originResult(i))
  }

  private val satResult = Wire(UInt(xlen.W))
  satResult := satVec.asUInt

  io.out.vd := MuxCase(originAddResult, Seq(
    isSat -> satResult,
    isMaxMin -> maxMinResult,
  ))
  io.out.addCarryCmpMask := addCarryCmpMask
  io.out.vxsat := Mux(isSat, sat, 0.U)


  dontTouch(vs2Head)
  dontTouch(vs1Head)
  dontTouch(vdHead)
  dontTouch(coutSat)
  dontTouch(satVec)
  dontTouch(originAddResult)

  dontTouch(sel8)
  dontTouch(sel16)
  dontTouch(sel32)
  dontTouch(sel64)
  dontTouch(isSub)
  dontTouch(srcbInv)
  dontTouch(cIn)
  dontTouch(srca)
  dontTouch(srcb)
  dontTouch(addSrc1)
  dontTouch(addSrc2)
  dontTouch(addSrc3)
  dontTouch(addResult)
  dontTouch(vsew)
  dontTouch(l0S)
  dontTouch(l0C)
  dontTouch(cout)
  dontTouch(addCarryCmpMask)
  dontTouch(srcaPre)
  dontTouch(ltVec)
  dontTouch(isCmpEq)
  dontTouch(isCmpNe)
  dontTouch(isCmpLt)
  dontTouch(isCmpLe)
  dontTouch(isCmpGt)
  dontTouch(eqResult)
  dontTouch(ltResult)
  dontTouch(cmpResult)
}
