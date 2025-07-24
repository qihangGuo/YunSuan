package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util._
import yunsuan.vector.Common._
import _root_.circt.stage.FirtoolOption

class GatherArray(vlen: Int, dlen: Int, xlen: Int) extends Module {
  require(isPow2(dlen) && dlen >= 64)
  require(isPow2(vlen) && vlen >= 64)

  override def desiredName: String = super.desiredName + s"_vlen${vlen}b_dlen${dlen}b"

  val maxTableNum = vlen / dlen * 8
  val vlWidth = log2Ceil(vlen + 1)
  val vlenb = vlen / 8
  val dlenb = dlen / 8
  val MinDataWidth = 8
  val NumElem = dlen / MinDataWidth
  val IndexWidth = log2Ceil(dlen)
  val ResWidth = MinDataWidth
  val TableIdxWidth = log2Ceil(maxTableNum)

  val flushVec = IO(Input(Vec(maxTableNum, Bool())))

  val in = IO(Flipped(ValidIO(new Bundle {
    val info = new GatherInfoBundle

    val uopIdx = UInt(TableIdxWidth.W)

    // used as vs1 of vrgather
    val vs1 = UInt(dlen.W)
    val vs2 = UInt(dlen.W)
    val rs1 = UInt(xlen.W)
    val oldVd = UInt(dlen.W)
    val srcMask = UInt(vlen.W)
    val vl = UInt(vlWidth.W)
  })))

  val out = IO(DecoupledIO(new Bundle {
    val dest = UInt(dlen.W)
  }))

  private val inTable = in.bits.vs2.splitToVecN(NumElem)
  private val info = in.bits.info
  private val vs1 = in.bits.vs1
  private val vs2 = in.bits.vs2
  private val rs1 = in.bits.rs1


  private val gatherUnits = Seq.fill(maxTableNum)(Module(new GatherUnit(vlen = vlen, dlen = dlen, maxTableNum = maxTableNum)))
  private val compressIndexGen = Module(new CompressIndexGen(vlen = vlen, dlen = dlen))
  private val gatherIndexGen = Module(new GatherIndexGen(vlen = vlen, dlen = dlen, MinDataWidth = MinDataWidth))
  private val slideUpIndexGen = Module(new SlideUpIndexGen(vlen = vlen, dlen = dlen))
  private val slideDownIndexGen = Module(new SlideDownIndexGen(vlen = vlen, dlen = dlen))

  for (i <- gatherUnits.indices) {
    gatherUnits(i).flush := flushVec(i)
    gatherUnits(i).in.ctrl.valid := in.valid && !in.bits.info.compress && in.bits.uopIdx === i.U
    gatherUnits(i).in.ctrl.bits.isEntryUnit := (i == 0).B // Todo: support more entry unit
    gatherUnits(i).in.ctrl.bits.info := in.bits.info
    gatherUnits(i).in.ctrl.bits.half := false.B
    gatherUnits(i).in.ctrl.bits.vl := in.bits.vl

    gatherUnits(i).in.compress.valid := compressIndexGen.out.validVec(i)
    gatherUnits(i).in.compress.bits := compressIndexGen.out.index

    gatherUnits(i).in.gather := gatherIndexGen.out
    gatherUnits(i).in.slideUp := slideUpIndexGen.out
    gatherUnits(i).in.slideDown := slideDownIndexGen.out


    gatherUnits(i).out.ready := out.ready
  }

  private val gatherModTable = Mux(
    compressIndexGen.out.validVec.asUInt.orR,
    compressIndexGen.out.data,
    inTable,
  )

  private val gatherModUopIdx = Mux(
    compressIndexGen.out.validVec.asUInt.orR,
    compressIndexGen.out.uopIdx,
    in.bits.uopIdx,
  )

  gatherUnits.head.in.table.valid := in.valid && !in.bits.info.compress || compressIndexGen.out.validVec.asUInt.orR
  gatherUnits.head.in.table.bits.dataVec := gatherModTable
  gatherUnits.head.in.table.bits.idx := gatherModUopIdx

  for (i <- gatherUnits.indices.drop(1)) {
    gatherUnits(i).in.table := gatherUnits(i - 1).toNextUnit
  }

  compressIndexGen.in.valid := in.valid && in.bits.info.compress
  compressIndexGen.in.uopIdx := in.bits.uopIdx
  compressIndexGen.in.byteMask := in.bits.srcMask
  compressIndexGen.in.data := inTable

  gatherIndexGen.in.valid := in.valid && (in.bits.info.vrgather_v || in.bits.info.vrgatherei16)
  gatherIndexGen.in.bits.sew := info.sew
  gatherIndexGen.in.bits.ei.e64 := info.vrgather_v && info.sew.e64
  gatherIndexGen.in.bits.ei.e32 := info.vrgather_v && info.sew.e32
  gatherIndexGen.in.bits.ei.e16 := info.vrgather_v && info.sew.e16 || info.vrgatherei16
  gatherIndexGen.in.bits.ei.e8  := info.vrgather_v && info.sew.e8
  gatherIndexGen.in.bits.vs1 := in.bits.vs1

  slideUpIndexGen.in.valid := in.valid && info.slideup
  slideUpIndexGen.in.bits.sew := info.sew
  slideUpIndexGen.in.bits.offsetOverflow := in.bits.rs1.drop(IndexWidth) =/= 0.U
  slideUpIndexGen.in.bits.offset := in.bits.rs1.take(IndexWidth)
  slideUpIndexGen.in.bits.vl := in.bits.vl

  slideDownIndexGen.in.valid := in.valid && info.slidedown
  slideDownIndexGen.in.bits.lmul := info.lmul
  slideDownIndexGen.in.bits.sew := info.sew
  slideDownIndexGen.in.bits.offsetOverflow := in.bits.rs1.drop(IndexWidth) =/= 0.U
  slideDownIndexGen.in.bits.offset := in.bits.rs1.take(IndexWidth)

  out.valid := Cat(gatherUnits.map(_.out.valid)).orR
  out.bits.dest := Mux1H(gatherUnits.map(x => x.out.valid -> x.out.bits.dest))
}

object GatherArrayMain extends App {
  println("Generating the GatherArray hardware")

  val firtoolOpts = Array(
    "--target", "systemverilog",
    "-O=release",
    "--disable-annotation-unknown",
    "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
  )
  val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

  (new chisel3.stage.ChiselStage).execute(
    Array("--target-dir", "build/vector") ++ args,
    chisel3.stage.ChiselGeneratorAnnotation(() => new GatherArray(vlen = 128, dlen = 128, xlen = 64)) +: firtoolAnno
  )

  println("done")
}
