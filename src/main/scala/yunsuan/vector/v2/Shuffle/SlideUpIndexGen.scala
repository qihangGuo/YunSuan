package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util._
import yunsuan.vector.Common._
import _root_.circt.stage.FirtoolOption

/**
 * This module is used to generate the indices for slideup instruction.
 * The unchanged part is handled in this module, but tail part is not handled.
 */
class SlideUpIndexGen(vlen: Int, dlen: Int) extends Module {
  require(isPow2(dlen) && dlen >= 64)

  override def desiredName: String = super.desiredName + s"_${dlen}b"

  val MinDataWidth = 8
  val NumElem = dlen / MinDataWidth
  val IndexWidth = log2Ceil(dlen)
  val VlWidth = log2Ceil(vlen + 1)

  val in = IO(Input(ValidIO(new Bundle {
    val sew = new GatherSewBundle
    // if slideNum is greater than or equal to dlen
    val offsetOverflow = Bool()
    val offset = UInt(IndexWidth.W)
    val vl = UInt(VlWidth.W)
  })))

  val out = IO(Output(ValidIO(new GatherIndexBundle(NumElem, IndexWidth))))

  val sew = in.bits.sew
  val vl = in.bits.vl
  val offset = in.bits.offset
  val offsetOverflow = in.bits.offsetOverflow

  private val e8Vl = Mux1H(Seq(
    sew.e8  -> vl,
    sew.e16 -> Cat(vl.tail(1), 0.U(1.W)),
    sew.e32 -> Cat(vl.tail(2), 0.U(2.W)),
    sew.e64 -> Cat(vl.tail(3), 0.U(3.W)),
  ))

  private val e8Off = Mux1H(Seq(
    sew.e8  -> offset,
    sew.e16 -> Cat(offset.tail(1), 0.U(1.W)),
    sew.e32 -> Cat(offset.tail(2), 0.U(2.W)),
    sew.e64 -> Cat(offset.tail(3), 0.U(3.W)),
  ))

  out.valid := in.valid
  // i < min(vl, max(vstart, offset))
  // Since vstart is always 0, we get
  // i < min(vl, offset) <=>
  // i < vl && i < offset
  for (i <- out.bits.index.indices) {
    out.bits.index(i) := Mux(
      offsetOverflow || i.U < e8Vl && i.U < e8Off,
      i.U,
      i.U - e8Off,
    )
  }


  out.bits.indexGeVlmax.foreach(_ := false.B)
}

object SlideIndexGenMain extends App {
  println("Generating the SlideIndexGen hardware")

  val firtoolOpts = Array(
    "--target", "systemverilog",
    "-O=release",
    "--disable-annotation-unknown",
    "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
  )
  val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

  (new chisel3.stage.ChiselStage).execute(
    Array("--target-dir", "build/vector") ++ args,
    chisel3.stage.ChiselGeneratorAnnotation(() => new SlideUpIndexGen(dlen = 128, vlen = 128)) +: firtoolAnno
  )

  println("done")
}