package yunsuan.vector.v2.Shuffle

import _root_.circt.stage.FirtoolOption
import chisel3._
import chisel3.util._
import yunsuan.vector.Common._

class GatherUnit(vlen: Int, dlen: Int, maxTableNum: Int, xlen: Int = 64) extends Module {
  require(isPow2(dlen) && dlen >= 64)
  require(isPow2(vlen) && vlen >= 64)
  require(isPow2(maxTableNum))

  override def desiredName: String = super.desiredName + s"_vlen${vlen}b_dlen${dlen}b_table${maxTableNum}"

  val vlWidth = log2Ceil(vlen + 1)
  val dlenb = dlen / 8
  val MinDataWidth = 8
  val NumElem = dlen / MinDataWidth
  val IndexWidth = log2Ceil(dlen)
  val ResWidth = MinDataWidth
  val TableIdxWidth = log2Ceil(maxTableNum)
  val SplittedParts = 8 * vlen / dlen

  val flush = IO(Input(Bool()))

  val in = IO(new Bundle {
    val ctrl = Flipped(ValidIO(new Bundle {
      val isEntryUnit = Bool()

      val info = new GatherInfoBundle

      // used by vrgatherei16 e8
      // only lower half table will be used
      val half = Bool()

      val vl = UInt(vlWidth.W)
      // 1 bit per min width data
      val elemMask = UInt(NumElem.W)
      // 1 -> ma, 0 -> mu
      val ma = Bool()
      // 1 -> ta, 0 -> tu
      val ta = Bool()
    }))

    val table = Input(ValidIO(new Bundle {
      val dataVec = Vec(NumElem, UInt(MinDataWidth.W))
      val idx = UInt(TableIdxWidth.W)
    }))

    val oldTable = Input(new Bundle {
      val dataVec = Vec(NumElem, UInt(MinDataWidth.W))
    })

    val scalaTable = Input(UInt(xlen.W))

    val compress    = Input(ValidIO(new GatherIndexBundle(NumElem, IndexWidth)))
    val gather      = Input(ValidIO(new GatherIndexBundle(NumElem, IndexWidth)))
    val slideUp     = Input(ValidIO(new GatherIndexBundle(NumElem, IndexWidth)))
    val slideDown   = Input(ValidIO(new GatherIndexBundle(NumElem, IndexWidth)))
    val slide1Up    = Input(ValidIO(new GatherIndexBundle(NumElem, IndexWidth)))
    val slide1Down  = Input(ValidIO(new GatherIndexBundle(NumElem, IndexWidth)))
  })

  // out.valid means this uop can writeback
  // out.ready means this uop's writeback is handled
  // valid will not rely on ready
  val out = IO(DecoupledIO(new Bundle {
    val dest = UInt(dlen.W)
  }))

  val toNextUnit = IO(Output(ValidIO(new Bundle {
    val dataVec = Vec(NumElem, UInt(MinDataWidth.W))
    val idx = UInt(TableIdxWidth.W)
  })))

  private val ma = in.ctrl.bits.ma
  private val ta = in.ctrl.bits.ta
  private val info = in.ctrl.bits.info
  private val inTableIdx = in.table.bits.idx
  private val inTable: Vec[UInt] = in.table.bits.dataVec
  private val inScalaTable: Vec[UInt] = in.scalaTable.splitToVecByWidth(MinDataWidth)

  private val isGatherOp = in.gather.valid
  private val isCompressOp = in.compress.valid
  private val isSlideUpOp = in.slideUp.valid
  private val isSlideDownOp = in.slideDown.valid
  private val isSlide1UpOp = in.slide1Up.valid
  private val isSlide1DownOp = in.slide1Down.valid

  private val table = Reg(Vec(NumElem, UInt(MinDataWidth.W)))
  private val indexOrRes = Reg(Vec(NumElem, UInt(IndexWidth.max(ResWidth).W)))
  private val indexView = WireInit(VecInit(indexOrRes.map(_.take(IndexWidth))))
  private val resView = WireInit(VecInit(indexOrRes.map(_.take(ResWidth))))

  private val active = RegInit(false.B)
  private val counter = RegInit(0.U(log2Up(maxTableNum).W))
  private val indexDone = Reg(Vec(NumElem, Bool()))
  private val tableIdx = Reg(UInt(TableIdxWidth.W))
  private val multiUop = RegInit(false.B)

  private val gatherMod = Module(new GatherBase(dlen = dlen, maxTableNum))

  private val acceptFirstUop = !active && in.ctrl.valid
  private val acceptNonFirstUop = active && in.ctrl.valid

  when (flush) {
    active := false.B
  }.elsewhen (acceptFirstUop) {
    active := true.B
    counter := Mux1H(Seq(
      info.lmul.m8  -> (1.max(maxTableNum /  1) - 1).U,
      info.lmul.m4  -> (1.max(maxTableNum /  2) - 1).U,
      info.lmul.m2  -> (1.max(maxTableNum /  4) - 1).U,
      info.lmul.m1  -> (1.max(maxTableNum /  8) - 1).U,
      info.lmul.mf2 -> (1.max(maxTableNum / 16) - 1).U,
      info.lmul.mf4 -> (1.max(maxTableNum / 32) - 1).U,
      info.lmul.mf8 -> (1.max(maxTableNum / 64) - 1).U,
    ))
  }.elsewhen (acceptNonFirstUop) {
    counter := counter - 1.U
  }.elsewhen (active && counter === 0.U && out.ready) {
    active := false.B
  }.otherwise {
    // keep
  }

  private val iIsLtVl = VlCompareBitVecModule(vlen, dlen, MinDataWidth)(
    in.ctrl.bits.vl,
    info.sew,
    in.table.bits.idx,
    (i, vl) => i < vl,
    "LessThanVl",
  )
  private val iIsZero = VlCompareBitVecModule(vlen, dlen, MinDataWidth)(
    0.U,
    info.sew,
    in.table.bits.idx,
    (i, vl) => i === vl,
    "EqualZero",
  )
  private val iIsVlM1 = VlCompareBitVecModule(vlen, dlen, MinDataWidth)(
    in.ctrl.bits.vl,
    info.sew,
    in.table.bits.idx,
    (i, vl) => Mux(vl === 0.U, false.B, i === (vl - 1.U)),
    "EqualVlM1",
  )

  private val gatherModIndexValid = Wire(Vec(NumElem, Bool()))
  for (i <- gatherModIndexValid.indices) {
    gatherModIndexValid(i) :=
      Mux(
        acceptFirstUop,
        Mux1H(Seq(
          in.gather.valid    -> true.B,
          in.slideUp.valid   -> true.B,
          in.slideDown.valid -> true.B,
          in.compress.valid  -> true.B,
        )),
        !indexDone(i),
      )
  }

  private val gatherModIndex: GatherIndexBundle = Mux1H(Seq(
    in.gather.valid     -> in.gather.bits,
    in.slideUp.valid    -> in.slideUp.bits,
    in.slideDown.valid  -> in.slideDown.bits,
    in.compress.valid   -> in.compress.bits,
  ))

  gatherMod.in.tableIdx := inTableIdx
  gatherMod.in.table := inTable

  for (i <- gatherMod.in.indexValid.indices) {
    gatherMod.in.indexValid(i) := Mux(
      acceptFirstUop,
      gatherModIndex.indexValid(i),
      !indexDone(i),
    )
    gatherMod.in.index(i) := Mux(
      acceptFirstUop,
      gatherModIndex.index(i),
      indexView(i),
    )
    gatherMod.in.indexGeVlmax(i) := Mux(
      acceptFirstUop,
      gatherModIndex.indexGeVlmax(i),
      false.B
    )
  }

  gatherMod.in.lmul := info.lmul

  when (in.ctrl.valid) {
    multiUop := ((vlen / dlen) match {
      case 1 => info.lmul match { case lmul => !(lmul.mf8 || lmul.mf4 || lmul.mf2 || lmul.m1) }
      case 2 => info.lmul match { case lmul => !(lmul.mf8 || lmul.mf4 || lmul.mf2) }
      case 4 => info.lmul match { case lmul => !(lmul.mf8 || lmul.mf4) }
      case 8 => info.lmul match { case lmul => !(lmul.mf8) }
      case _ => false.B
    })
  }

  when (in.table.valid) {
    tableIdx := in.table.bits.idx
    table    := in.table.bits.dataVec
  }

  private val elemActive = Wire(Vec(NumElem, Bool()))
  private val elemInactive = Wire(Vec(NumElem, Bool()))
  private val elemTail = Wire(Vec(NumElem, Bool()))

  for (i <- 0 until NumElem) {
    elemActive(i)   := iIsLtVl(i) && in.ctrl.bits.elemMask(i)
    elemInactive(i) := iIsLtVl(i) && !in.ctrl.bits.elemMask(i)
    elemTail(i)     := !iIsLtVl(i)
  }

  private val fill1s = Wire(Vec(NumElem, Bool()))
  private val fillVs3 = Wire(Vec(NumElem, Bool()))
  private val fillIdx = Wire(Vec(NumElem, Bool()))
  private val fillRes = Wire(Vec(NumElem, Bool()))
  private val fillScala = Wire(Vec(NumElem, Bool()))

  private val scalaTable = Wire(inScalaTable.cloneType)
  for (i <- scalaTable.indices) {
    scalaTable(i) := Mux1H(Seq(
      info.sew.e8  -> inScalaTable(i % (8  / MinDataWidth)),
      info.sew.e16 -> inScalaTable(i % (16 / MinDataWidth)),
      info.sew.e32 -> inScalaTable(i % (32 / MinDataWidth)),
      info.sew.e64 -> inScalaTable(i % (64 / MinDataWidth)),
    ))
  }

  for (i <- 0 until NumElem) {
    fill1s(i)    := acceptFirstUop && (elemInactive(i) &&  ma || elemTail(i) &&  ta)
    fillVs3(i)   := acceptFirstUop && (elemInactive(i) && !ma || elemTail(i) && !ta)
    fillIdx(i)   := acceptFirstUop && gatherMod.out.notfound(i)
    fillRes(i)   :=                   elemActive(i) && gatherMod.out.resultValid(i) && (isSlide1UpOp && !iIsZero(i) || isSlide1DownOp && !iIsVlM1(i))
    fillScala(i) := acceptFirstUop && elemActive(i) &&                                 (isSlide1UpOp &&  iIsZero(i) || isSlide1DownOp &&  iIsVlM1(i))

    when (fillRes(i) || acceptFirstUop) {
      indexOrRes(i) := Mux1H(Seq(
        fillRes(i)   -> gatherMod.out.result(i),
        fillIdx(i)   -> gatherModIndex.index(i),
        fillVs3(i)   -> in.oldTable.dataVec(i),
        fill1s(i)    -> Fill(MinDataWidth, 1.U(1.W)),
        fillScala(i) -> scalaTable(i % (xlen / MinDataWidth))
      ))
    }

    indexDone(i) := gatherMod.out.resultValid(i)
  }

  // don't rely on out.ready
  out.valid     := active && counter === 0.U
  out.bits.dest := resView.asUInt

  // Need to transport table for non-first uop and
  toNextUnit.valid := in.table.valid && acceptNonFirstUop && multiUop
  toNextUnit.bits.dataVec := table
  toNextUnit.bits.idx := tableIdx
}

/**
 * This module is used to produce bit vector that is compared to vl.
 * This module is full configurable that support different vlen, dlen and MinDataWidth
 * @param vlen
 * @param dlen
 * @param MinDataWidth the min width of data. if we support int4, MinDataWidth should be 4
 */
class VlCompareBitVecModule(
  vlen        : Int,
  dlen        : Int,
  MinDataWidth: Int,
  compFunc    : (UInt, UInt) => Bool,
  postFixName : String = "",
) extends Module {
  override def desiredName: String = super.desiredName + postFixName

  private val vlWidth = log2Ceil(vlen + 1)
  private val SplittedParts = 8 * vlen / dlen
  private val NumElem = dlen / MinDataWidth

  val in = IO(Input(new Bundle {
    val vl = UInt(vlWidth.W)
    val sew = new GatherSewBundle
    val uopIdx = UInt(log2Ceil(SplittedParts).W)
  }))
  val out = IO(Output(new Bundle {
    val lessThanVl = Vec(NumElem, Bool())
  }))

  val e8Bias  = log2Ceil(8  / MinDataWidth)
  val e16Bias = log2Ceil(16 / MinDataWidth)
  val e32Bias = log2Ceil(32 / MinDataWidth)
  val e64Bias = log2Ceil(64 / MinDataWidth)

  val minDataVl = Mux1H(Seq(
    in.sew.e8  -> in.vl.tail( e8Bias) ## 0.U( e8Bias.W),
    in.sew.e16 -> in.vl.tail(e16Bias) ## 0.U(e16Bias.W),
    in.sew.e32 -> in.vl.tail(e32Bias) ## 0.U(e32Bias.W),
    in.sew.e64 -> in.vl.tail(e64Bias) ## 0.U(e64Bias.W),
  ))

  val indices = out.lessThanVl.indices.map(i => in.uopIdx ## i.U(log2Ceil(NumElem).W))

  for (i <- out.lessThanVl.indices) {
    out.lessThanVl(i) := compFunc(indices(i), minDataVl)
  }
}

object VlCompareBitVecModule {
  def apply(
    vlen        : Int,
    dlen        : Int,
    MinDataWidth: Int,
  )(
    vl      : UInt,
    sew     : GatherSewBundle,
    uopIdx  : UInt,
    compFunc: (UInt, UInt) => Bool,
    postFixName: String = "",
  ): Vec[Bool] = {
    val mod = Module(new VlCompareBitVecModule(vlen, dlen, MinDataWidth, compFunc, postFixName))
    mod.in.vl := vl
    mod.in.sew := sew
    mod.in.uopIdx := uopIdx
    mod.out.lessThanVl
  }
}

class GatherInfoBundle extends Bundle {
  val lmul = new GatherLmulBundle
  val sew = new GatherSewBundle
  val vrgather_v = Bool()
  val vrgatherei16 = Bool()
  val slideup = Bool()
  val slidedown = Bool()
  val compress = Bool()
}

class GatherLmulBundle extends Bundle {
  val mf8 = Bool()
  val mf4 = Bool()
  val mf2 = Bool()
  val m1  = Bool()
  val m2  = Bool()
  val m4  = Bool()
  val m8  = Bool()
}

class GatherSewBundle extends Bundle {
  val e64 = Bool()
  val e32 = Bool()
  val e16 = Bool()
  val e8 = Bool()

  def toOH: UInt = Cat(e64, e32, e16, e8)
}

object VlCompareBitVecModuleMain extends App {
  println("Generating the VlCompareBitVecModule hardware")

  val firtoolOpts = Array(
    "--target", "systemverilog",
    "-O=release",
    "--disable-annotation-unknown",
    "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
  )
  val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

  (new chisel3.stage.ChiselStage).execute(
    Array("--target-dir", "build/vector") ++ args,
    chisel3.stage.ChiselGeneratorAnnotation(
      () => new VlCompareBitVecModule(vlen = 128, dlen = 128, MinDataWidth = 8, (i, vl) => i < vl, "LessThanVl")
    ) +: firtoolAnno
  )

  (new chisel3.stage.ChiselStage).execute(
    Array("--target-dir", "build/vector") ++ args,
    chisel3.stage.ChiselGeneratorAnnotation(
      () => new VlCompareBitVecModule(vlen = 128, dlen = 128, MinDataWidth = 8, (i, vl) => i === (vl - 1.U), "EqualVlM1")
    ) +: firtoolAnno
  )

  println("done")
}

object GatherUnitMain extends App {
  println("Generating the GatherUnit hardware")

  val firtoolOpts = Array(
    "--target", "systemverilog",
    "-O=release",
    "--disable-annotation-unknown",
    "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
  )
  val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

  (new chisel3.stage.ChiselStage).execute(
    Array("--target-dir", "build/vector") ++ args,
    chisel3.stage.ChiselGeneratorAnnotation(() => new GatherUnit(vlen = 128, dlen = 128, maxTableNum = 8)) +: firtoolAnno
  )

  println("done")
}