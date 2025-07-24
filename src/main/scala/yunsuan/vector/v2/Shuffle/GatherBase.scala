package yunsuan.vector.v2.Shuffle

import _root_.circt.stage.FirtoolOption
import chisel3._
import chisel3.util._
import yunsuan.util.ModuleWrapper.{ModuleMux1H, ModuleVec}
import yunsuan.vector.Common.caseToUIntUtil

class GatherBase(dlen: Int, maxTableNum: Int) extends Module {
  require(isPow2(dlen) && dlen >= 64)
  require(isPow2(maxTableNum))

  override def desiredName: String = super.desiredName + s"_dlen${dlen}b_table${maxTableNum}"

  val dlenb = dlen / 8
  val MinDataWidth = 8
  val NumElem = dlen / MinDataWidth
  val IndexWidth = log2Ceil(dlen)
  val LookUpIndexWidth = log2Ceil(NumElem)

  val in = IO(Input(new Bundle {
    val tableIdx = UInt(log2Up(maxTableNum).W)
    val table = Vec(NumElem, UInt(MinDataWidth.W))
    val indexValid = Vec(NumElem, Bool())
    val index = Vec(NumElem, UInt(IndexWidth.W))
    val indexGeVlmax = Vec(NumElem, Bool())
    val lmul = new GatherLmulBundle
  }))

  val out = IO(Output(new Bundle {
    // found or overflow
    val resultValid = Vec(NumElem, Bool())
    val result = Vec(NumElem, UInt(MinDataWidth.W))
    val inactive = Vec(NumElem, Bool())
    val notfound = Vec(NumElem, Bool())
  }))

  val geRealVlmax = Wire(Vec(NumElem, Bool()))

  for (i <- 0 until NumElem) {
    geRealVlmax(i) := ModuleMux1H(Seq(
      in.lmul.mf8 -> (in.index(i).head(6) =/= 0.U),
      in.lmul.mf4 -> (in.index(i).head(5) =/= 0.U),
      in.lmul.mf2 -> (in.index(i).head(4) =/= 0.U),
      in.lmul.m1  -> (in.index(i).head(3) =/= 0.U),
      in.lmul.m2  -> (in.index(i).head(2) =/= 0.U),
      in.lmul.m4  -> (in.index(i).head(1) =/= 0.U),
      // in m8 configuration, index will never greater than or equal to vlmax
    ))
  }

  val found    = Wire(Vec(NumElem, Bool()))
  val notfound = Wire(Vec(NumElem, Bool()))
  val overflow = Wire(Vec(NumElem, Bool()))
  val inactive = Wire(Vec(NumElem, Bool()))

  val indices = VecInit((overflow zip in.index).map { case (msb, idx) => Cat(msb, idx.take(LookUpIndexWidth)) })

  for (i <- 0 until NumElem) {
    found(i)    := in.indexValid(i) && !in.indexGeVlmax(i) && !geRealVlmax(i) && in.index(i).drop(LookUpIndexWidth) === in.tableIdx
    notfound(i) := in.indexValid(i) && !in.indexGeVlmax(i) && !geRealVlmax(i) && in.index(i).drop(LookUpIndexWidth) =/= in.tableIdx
    overflow(i) := in.indexValid(i) && (in.indexGeVlmax(i) || geRealVlmax(i))
    inactive(i) := !in.indexValid(i)

    out.resultValid(i) := found(i) || overflow(i)
    out.result(i) := ModuleVec(in.table)(indices(i))
  }

  out.notfound := notfound
  out.inactive := inactive
}

class GatherIndexBundle(NumElem: Int, IndexWidth: Int) extends Bundle {
  // if 1, this index need be looked up in GatherBase
  // may fill idx, zero, res
  val indexValid = Vec(NumElem, Bool())
  val index = Vec(NumElem, UInt(IndexWidth.W))
  val indexGeVlmax = Vec(NumElem, Bool())
}

object GatherBaseMain extends App {
  println("Generating the GatherBase hardware")

  val firtoolOpts = Array(
    "--target=systemverilog",
    "-O=release",
    "--disable-annotation-unknown",
    "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
  )
  val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

  (new chisel3.stage.ChiselStage).execute(
    Array("--target-dir", "build/vector") ++ args,
    chisel3.stage.ChiselGeneratorAnnotation(() => new GatherBase(128, 8)) +: firtoolAnno
  )

  println("done")
}
