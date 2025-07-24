package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util._
import yunsuan.vector.Common._
import _root_.circt.stage.FirtoolOption

class SlideDownIndexGen(vlen: Int, dlen: Int) extends Module {
  require(isPow2(dlen) && dlen >= 64)

  override def desiredName: String = super.desiredName + s"_${dlen}b"

  val MinDataWidth = 8
  val NumElem = dlen / MinDataWidth
  val IndexWidth = log2Ceil(dlen)
  val VlWidth = log2Ceil(vlen) + 1

  val in = IO(Input(ValidIO(new Bundle {
    val lmul = new GatherLmulBundle
    val sew = new GatherSewBundle
    // if slideNum is greater than or equal to dlen
    val offsetOverflow = Bool()
    val offset = UInt(IndexWidth.W)
  })))

  val out = IO(Output(ValidIO(new GatherIndexBundle(NumElem, IndexWidth))))

  val lmul = in.bits.lmul
  val sew = in.bits.sew
  val offset = in.bits.offset
  val offsetOverflow = in.bits.offsetOverflow

  private val e8Off = Mux1H(Seq(
    sew.e8  -> offset,
    sew.e16 -> Cat(offset.tail(1), 0.U(1.W)),
    sew.e32 -> Cat(offset.tail(2), 0.U(2.W)),
    sew.e64 -> Cat(offset.tail(3), 0.U(3.W)),
  )).ensuring(_.getWidth == IndexWidth)

  out.valid := in.valid
  // 0     <= i + offset < vlmax --> src[i] = vs2[i + offset]
  // vlmax <= i + offset         --> src[i] = 0
  for (i <- out.bits.index.indices) {
    val iPlusOffset = i.U +& e8Off
    out.bits.index(i) := iPlusOffset.tail(1) // drop carry bit
    out.bits.indexGeVlmax(i) := Mux1H(Seq(
      lmul.mf8 -> (iPlusOffset.head(7) =/= 0.U),
      lmul.mf4 -> (iPlusOffset.head(6) =/= 0.U),
      lmul.mf2 -> (iPlusOffset.head(5) =/= 0.U),
      lmul.m1  -> (iPlusOffset.head(4) =/= 0.U),
      lmul.m2  -> (iPlusOffset.head(3) =/= 0.U),
      lmul.m4  -> (iPlusOffset.head(2) =/= 0.U),
      lmul.m8  -> (iPlusOffset.head(1) =/= 0.U),
    ))
  }
}

object SlideDownIndexGenMain extends App {
  println("Generating the SlideDownIndexGen hardware")

  val firtoolOpts = Array(
    "--target=systemverilog",
    "-O=release",
    "--disable-annotation-unknown",
    "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
  )
  val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

  (new chisel3.stage.ChiselStage).execute(
    Array("--target-dir", "build/vector") ++ args,
    chisel3.stage.ChiselGeneratorAnnotation(() => new SlideDownIndexGen(128, 128)) +: firtoolAnno
  )

  println("done")
}