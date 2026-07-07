package yunsuan.vector

import chisel3._
import chisel3.util._
import yunsuan.VfexpType
import yunsuan.util._
import yunsuan.vector.VectorConvert.RoundingModle._
import yunsuan.vector.VectorConvert.util.ShiftRightJam

case class VfExp2Format(
    expWidth: Int,
    fracWidth: Int,
    bias: Int,
    tFracBits: Int,
    coeffFracBits: Int,
    coeffs: Seq[(Int, Int, Int)]
) {
  def width: Int = 1 + expWidth + fracWidth
  def maxExpField: BigInt = (BigInt(1) << expWidth) - 2
  def inf: BigInt = ((BigInt(1) << expWidth) - 1) << fracWidth
  def maxFinite: BigInt =
    (maxExpField << fracWidth) | ((BigInt(1) << fracWidth) - 1)
  def canonicalNaN: BigInt = inf | (BigInt(1) << (fracWidth - 1))
  def sigFracBits: Int = coeffFracBits + 2 * tFracBits
  def segmentBits: Int = log2Ceil(coeffs.length)
}

object VfExp2Tables {
  val fp16 = VfExp2Format(
    expWidth = 5,
    fracWidth = 10,
    bias = 15,
    tFracBits = 13,
    coeffFracBits = 16,
    coeffs = Seq(
      (16088, 45417, 65536),
      (16800, 47428, 68438),
      (17544, 49528, 71468),
      (18321, 51721, 74632),
      (19132, 54011, 77936),
      (19979, 56402, 81386),
      (20864, 58899, 84990),
      (21787, 61507, 88753),
      (22752, 64230, 92682),
      (23759, 67074, 96785),
      (24811, 70043, 101070),
      (25910, 73144, 105545),
      (27057, 76383, 110218),
      (28255, 79765, 115098),
      (29506, 83296, 120194),
      (30812, 86984, 125515)
    )
  )

  val bf16 = VfExp2Format(
    expWidth = 8,
    fracWidth = 7,
    bias = 127,
    tFracBits = 12,
    coeffFracBits = 16,
    coeffs = Seq(
      (16088, 45417, 65536),
      (16800, 47428, 68438),
      (17544, 49528, 71468),
      (18321, 51721, 74632),
      (19132, 54011, 77936),
      (19979, 56402, 81386),
      (20864, 58899, 84990),
      (21787, 61507, 88753),
      (22752, 64230, 92682),
      (23759, 67074, 96785),
      (24811, 70043, 101070),
      (25910, 73144, 105545),
      (27057, 76383, 110218),
      (28255, 79765, 115098),
      (29506, 83296, 120194),
      (30812, 86984, 125515)
    )
  )

  val fp32 = VfExp2Format(
    expWidth = 8,
    fracWidth = 23,
    bias = 127,
    tFracBits = 20,
    coeffFracBits = 20,
    coeffs = Seq(
      (253918, 726788, 1048576),
      (255314, 734723, 1059994),
      (258345, 742726, 1071537),
      (260902, 750811, 1083205),
      (265195, 758964, 1095001),
      (266620, 767251, 1106924),
      (271042, 775582, 1118978),
      (272447, 784052, 1131163),
      (275443, 792589, 1143480),
      (278414, 801221, 1155932),
      (281478, 809945, 1168519),
      (284530, 818765, 1181244),
      (289248, 827655, 1194107),
      (292364, 836668, 1207110),
      (295528, 845780, 1220254),
      (297110, 855015, 1233542),
      (300360, 864325, 1246974),
      (303625, 873737, 1260553),
      (308580, 883227, 1274279),
      (310260, 892870, 1288155),
      (313648, 902592, 1302182),
      (317061, 912421, 1316362),
      (322283, 922329, 1330697),
      (324020, 932400, 1345187),
      (329329, 942526, 1359835),
      (331107, 952817, 1374643),
      (334679, 963194, 1389611),
      (340209, 973653, 1404743),
      (342039, 984284, 1420040),
      (347645, 994973, 1435503),
      (349480, 1005839, 1451135),
      (353341, 1016790, 1466937),
      (357188, 1027862, 1482910),
      (363087, 1039023, 1499058),
      (364994, 1050369, 1515382),
      (368970, 1061807, 1531883),
      (372998, 1073369, 1548565),
      (379156, 1085025, 1565427),
      (381161, 1096873, 1582474),
      (385201, 1108822, 1599706),
      (389520, 1120891, 1617125),
      (395852, 1133065, 1634735),
      (399130, 1145438, 1652536),
      (402372, 1157909, 1670531),
      (406741, 1170518, 1688722),
      (413462, 1183228, 1707111),
      (415644, 1196149, 1725700),
      (422568, 1209136, 1744492),
      (424768, 1222341, 1763488),
      (429393, 1235651, 1782691),
      (434060, 1249107, 1802103),
      (441172, 1262672, 1821727),
      (443566, 1276458, 1841564),
      (448394, 1290358, 1861617),
      (455814, 1304369, 1881889),
      (458234, 1318613, 1902381),
      (465750, 1332933, 1923097),
      (470914, 1347444, 1944038),
      (473375, 1362160, 1965207),
      (478487, 1376994, 1986607),
      (486396, 1391946, 2008240),
      (488973, 1407146, 2030108),
      (494314, 1422468, 2052214),
      (499560, 1437962, 2074562)
    )
  )
}

object VfExp2Pipe {
  val latency = 6
}

class VfExp2Pipe(fmt: VfExp2Format, laneCount: Int) extends Module {
  import VfExp2Pipe._

  val io = IO(new Bundle {
    val fire = Input(Bool())
    val src = Input(Vec(laneCount, UInt(fmt.width.W)))
    val rm = Input(UInt(3.W))
    val result = Output(Vec(laneCount, UInt(fmt.width.W)))
    val fflags = Output(Vec(laneCount, UInt(5.W)))
  })

  private val kWidth = 16
  private val coeffWidth = fmt.coeffFracBits + 2
  // localT high segBits are zero (removed by segIdx subtraction); only low bits carry info
  private val localTKeep = fmt.tFracBits - fmt.segmentBits
  private val product1Width = coeffWidth + localTKeep
  // horner1 = product1 + (B << tFracBits); B term dominates upper bits
  private val horner1Width = coeffWidth + fmt.tFracBits + 1

  // stage4 horner1 truncation: keep coeffWidth MSB bits to match stage2 multiplier
  private val stage4Trunc = true
  private val horner1Keep = if (stage4Trunc) coeffWidth else horner1Width
  private val product2TruncWidth = horner1Keep + localTKeep
  private val horner1Shift = horner1Width - horner1Keep

  // product2 aligned back to full precision
  private val product2Width = product2TruncWidth + horner1Shift
  private val cAlignedWidth = coeffWidth + 2 * fmt.tFracBits
  private val sigScaledWidth = math.max(product2Width, cAlignedWidth) + 1

  private def extendUInt(x: UInt, targetWidth: Int): UInt =
    if (x.getWidth >= targetWidth) x(targetWidth - 1, 0)
    else Cat(0.U((targetWidth - x.getWidth).W), x)

  private def shiftLeftTrunc(x: UInt, shamt: UInt, outWidth: Int): UInt = {
    require(outWidth > 0)
    val padded = extendUInt(x, outWidth)
    if (outWidth == 1) {
      Mux(shamt.orR, 0.U(1.W), padded)
    } else {
      val shiftWidth = log2Ceil(outWidth)
      val trimmedShamt = extendUInt(shamt, shiftWidth)
      val tooLarge =
        if (shamt.getWidth > shiftWidth)
          shamt(shamt.getWidth - 1, shiftWidth).orR
        else false.B
      Mux(
        tooLarge || trimmedShamt >= outWidth.U,
        0.U(outWidth.W),
        doShiftLeft(padded, trimmedShamt).asUInt
      )
    }
  }

  private def overflowResult(fmt: VfExp2Format, rm: UInt): UInt = {
    val toMaxFinite = rm === RTZ || rm === RDN
    Mux(toMaxFinite, fmt.maxFinite.U(fmt.width.W), fmt.inf.U(fmt.width.W))
  }

  private def roundShiftRightPositive(
      x: UInt,
      shamt: UInt,
      rm: UInt
  ): (UInt, Bool) = {
    val (shifted, _) = ShiftRightJam(x, shamt)
    val shamtM1 = Mux(shamt === 0.U, 0.U, shamt - 1.U)
    val (shiftedM1, stickyLow) = ShiftRightJam(x, shamtM1)
    val guard = Mux(shamt === 0.U, false.B, shiftedM1(0))
    val sticky = Mux(shamt <= 1.U, false.B, stickyLow)
    val inexact = guard || sticky
    val roundUp = MuxLookup(
      rm,
      guard && (sticky || shifted(0))
    )(
      Seq(
        RTZ -> false.B,
        RDN -> false.B,
        RUP -> inexact,
        RMM -> guard,
        RTO -> false.B
      )
    )
    (shifted + roundUp, inexact)
  }

  private def roundAndPackPositive(
      sigScaled: UInt,
      fmt: VfExp2Format,
      k: SInt,
      rm: UInt,
      hasFracInput: Bool
  ): (UInt, UInt) = {
    val sigFracBits = fmt.sigFracBits
    val normalShift = sigFracBits - fmt.fracWidth
    val overflowSig = (BigInt(1) << (fmt.fracWidth + 1)).U
    val normalThreshold = (BigInt(1) << fmt.fracWidth).U

    val normalizedSig = Wire(UInt((sigScaled.getWidth + 1).W))
    val normalizedK = Wire(SInt(kWidth.W))
    normalizedSig := sigScaled
    normalizedK := k
    when(sigScaled >= (BigInt(2) << sigFracBits).U) {
      normalizedSig := (sigScaled + 1.U) >> 1
      normalizedK := k + 1.S
    }

    val expCalcWidth = kWidth + 3
    val normalExp = normalizedK.pad(expCalcWidth) + fmt.bias.S(expCalcWidth.W)
    val (normalRoundedRaw, normalRoundInexact) =
      roundShiftRightPositive(normalizedSig, normalShift.U, rm)
    val normalCarry = normalRoundedRaw >= overflowSig
    val normalRounded =
      Mux(normalCarry, normalRoundedRaw >> 1, normalRoundedRaw)
    val normalExpAdj =
      normalExp + Mux(normalCarry, 1.S(expCalcWidth.W), 0.S(expCalcWidth.W))
    val normalOverflow = normalExpAdj > fmt.maxExpField.S
    val normalResult = Cat(
      normalExpAdj.asUInt(fmt.expWidth - 1, 0),
      normalRounded(fmt.fracWidth - 1, 0)
    )

    val subShift =
      sigFracBits.S(expCalcWidth.W) - (normalizedK.pad(
        expCalcWidth
      ) + (fmt.bias - 1 + fmt.fracWidth).S(expCalcWidth.W))
    val subWidth = sigScaled.getWidth + fmt.fracWidth + 4
    val subSigWide = Cat(0.U((subWidth - sigScaled.getWidth).W), normalizedSig)
    val subRoundedRaw = Wire(UInt((subWidth + 1).W))
    val subRoundInexact = Wire(Bool())
    when(subShift <= 0.S) {
      subRoundedRaw := shiftLeftTrunc(
        subSigWide,
        (-subShift).asUInt,
        subWidth + 1
      )
      subRoundInexact := false.B
    }.otherwise {
      val (rounded, inexact) =
        roundShiftRightPositive(subSigWide, subShift.asUInt, rm)
      subRoundedRaw := rounded
      subRoundInexact := inexact
    }
    val subCarry = subRoundedRaw >= normalThreshold
    val subZero = subRoundedRaw === 0.U
    val subResult = Mux(
      subCarry,
      (1.U(fmt.expWidth.W) << fmt.fracWidth).asUInt,
      Cat(0.U(fmt.expWidth.W), subRoundedRaw(fmt.fracWidth - 1, 0))
    )
    val subUnderflow = !subCarry && !subZero && subRoundInexact
    val zeroUnderflow = subZero && subRoundInexact

    val result = Wire(UInt(fmt.width.W))
    val of = Wire(Bool())
    val uf = Wire(Bool())
    val nxRound = Wire(Bool())
    when(normalExp > 0.S) {
      result := Mux(normalOverflow, overflowResult(fmt, rm), normalResult)
      of := normalOverflow
      uf := false.B
      nxRound := normalRoundInexact || normalOverflow
    }.otherwise {
      result := subResult
      of := false.B
      uf := subUnderflow || zeroUnderflow
      nxRound := subRoundInexact
    }

    val nx = hasFracInput || nxRound
    val rtoResult =
      Mux(rm === RTO && nx && !of, result | 1.U(fmt.width.W), result)
    (rtoResult, Cat(false.B, false.B, of, uf, nx))
  }

  val fireS1 = GatedValidRegNext(io.fire)
  val fireS2 = GatedValidRegNext(fireS1)
  val fireS3 = GatedValidRegNext(fireS2)
  val fireS4 = GatedValidRegNext(fireS3)
  val fireS5 = GatedValidRegNext(fireS4)
  val rmS1 = RegEnable(io.rm, 0.U(3.W), io.fire)
  val rmS2 = RegEnable(rmS1, 0.U(3.W), fireS1)
  val rmS3 = RegEnable(rmS2, 0.U(3.W), fireS2)
  val rmS4 = RegEnable(rmS3, 0.U(3.W), fireS3)
  val rmS5 = RegEnable(rmS4, 0.U(3.W), fireS4)

  val coeffA = VecInit(fmt.coeffs.map(_._1.U(coeffWidth.W)))
  val coeffB = VecInit(fmt.coeffs.map(_._2.U(coeffWidth.W)))
  val coeffC = VecInit(fmt.coeffs.map(_._3.U(coeffWidth.W)))

  val stage1SpecialNext = Wire(Vec(laneCount, Bool()))
  val stage1SpecialResultNext = Wire(Vec(laneCount, UInt(fmt.width.W)))
  val stage1SpecialFlagsNext = Wire(Vec(laneCount, UInt(5.W)))
  val stage1HasFracNext = Wire(Vec(laneCount, Bool()))
  val stage1KNext = Wire(Vec(laneCount, SInt(kWidth.W)))
  val stage1FracQNext = Wire(Vec(laneCount, UInt(fmt.tFracBits.W)))

  for (lane <- 0 until laneCount) {
    val src = io.src(lane)
    val exp = src(fmt.fracWidth + fmt.expWidth - 1, fmt.fracWidth)
    val frac = src(fmt.fracWidth - 1, 0)
    val sign = src(fmt.width - 1)

    val expIsZero = exp === 0.U
    val expIsOnes = exp.andR
    val fracNotZero = frac.orR
    val isSubnormal = expIsZero && fracNotZero
    val isInf = expIsOnes && !fracNotZero
    val isNaN = expIsOnes && fracNotZero
    val isSNaN = isNaN && !frac(fmt.fracWidth - 1)

    val sig = Mux(expIsZero, 0.U(1.W) ## frac, 1.U(1.W) ## frac)
    val p = Mux(
      expIsZero,
      (1 - fmt.bias - fmt.fracWidth).S,
      exp.zext - (fmt.bias + fmt.fracWidth).S
    )

    val intMag = Wire(UInt(kWidth.W))
    val rem = Wire(UInt((fmt.fracWidth + 1).W))
    val rshift = (-p).asUInt
    val sigShiftedRight = sig >> rshift
    val sigReconstructed = shiftLeftTrunc(sigShiftedRight, rshift, sig.getWidth)
    val pBig = p >= (kWidth - 1).S
    when(pBig) {
      intMag := ((BigInt(1) << (kWidth - 1)) - 1).U
      rem := 0.U
    }.elsewhen(p >= 0.S) {
      val shifted = shiftLeftTrunc(sig, p.asUInt, kWidth)
      val overflow = (sig >> ((kWidth - 1).U - p.asUInt)).orR
      intMag := Mux(
        overflow || shifted > ((BigInt(1) << (kWidth - 1)) - 1).U,
        ((BigInt(1) << (kWidth - 1)) - 1).U,
        ZeroExt(shifted, kWidth)
      )
      rem := 0.U
    }.otherwise {
      intMag := Mux(
        sigShiftedRight > ((BigInt(1) << (kWidth - 1)) - 1).U,
        ((BigInt(1) << (kWidth - 1)) - 1).U,
        ZeroExt(sigShiftedRight, kWidth)
      )
      rem := sig - sigReconstructed(fmt.fracWidth, 0)
    }

    val hasFracExact = p < 0.S && rem.orR
    val fracQPos = Wire(UInt(fmt.tFracBits.W))
    val fracQNeg = Wire(UInt(fmt.tFracBits.W))
    val qZero = 0.U(fmt.tFracBits.W)
    val qMax = ((BigInt(1) << fmt.tFracBits) - 1).U(fmt.tFracBits.W)
    fracQPos := qZero
    fracQNeg := qZero
    when(hasFracExact) {
      when(rshift <= fmt.tFracBits.U) {
        val leftShift = fmt.tFracBits.U - rshift
        fracQPos := shiftLeftTrunc(rem, leftShift, fmt.tFracBits)
        val complementBase = shiftLeftTrunc(1.U, rshift, fmt.tFracBits + 1)
        val complement = (complementBase - rem)(fmt.tFracBits - 1, 0)
        fracQNeg := shiftLeftTrunc(complement, leftShift, fmt.tFracBits)
      }.otherwise {
        val shift = rshift - fmt.tFracBits.U
        val (posRounded, _) = roundShiftRightPositive(rem, shift, RMM)
        val posQuantized = ZeroExt(posRounded, fmt.tFracBits)
        fracQPos := posQuantized
        fracQNeg := Mux(
          posQuantized === 0.U,
          qMax,
          ((1.U << fmt.tFracBits) - posQuantized)(fmt.tFracBits - 1, 0)
        )
      }
    }

    val k = Wire(SInt(kWidth.W))
    val fracQ = Wire(UInt(fmt.tFracBits.W))
    when(!sign.asBool) {
      k := intMag.asSInt
      fracQ := fracQPos
    }.otherwise {
      k := Mux(hasFracExact, -(intMag + 1.U).asSInt, -intMag.asSInt)
      fracQ := fracQNeg
      when(hasFracExact && !sigShiftedRight.orR) {
        k := -1.S
      }
    }

    val segIdx = Mux(
      fracQ === 0.U,
      0.U(fmt.segmentBits.W),
      fracQ(fmt.tFracBits - 1, fmt.tFracBits - fmt.segmentBits)
    )
    val localT = fracQ - (segIdx << (fmt.tFracBits - fmt.segmentBits))

    stage1SpecialNext(lane) := isNaN || isInf || isSubnormal
    stage1SpecialResultNext(lane) := Mux1H(
      Seq(
        isSNaN,
        isNaN && !isSNaN,
        isSubnormal,
        isInf && !sign.asBool,
        isInf && sign.asBool
      ),
      Seq(
        fmt.canonicalNaN.U(fmt.width.W),
        fmt.canonicalNaN.U(fmt.width.W),
        (fmt.bias.U(fmt.expWidth.W) << fmt.fracWidth).asUInt,
        fmt.inf.U(fmt.width.W),
        0.U(fmt.width.W)
      )
    )
    stage1SpecialFlagsNext(lane) := Mux(
      isSNaN,
      "b10000".U(5.W),
      Mux(isSubnormal, "b00001".U(5.W), 0.U(5.W))
    )
    stage1HasFracNext(lane) := hasFracExact
    stage1KNext(lane) := k
    stage1FracQNext(lane) := fracQ
  }

  val stage1Special =
    RegEnable(stage1SpecialNext, VecInit(Seq.fill(laneCount)(false.B)), io.fire)
  val stage1SpecialResult =
    RegEnable(
      stage1SpecialResultNext,
      VecInit(Seq.fill(laneCount)(0.U(fmt.width.W))),
      io.fire
    )
  val stage1SpecialFlags = RegEnable(
    stage1SpecialFlagsNext,
    VecInit(Seq.fill(laneCount)(0.U(5.W))),
    io.fire
  )
  val stage1HasFrac =
    RegEnable(stage1HasFracNext, VecInit(Seq.fill(laneCount)(false.B)), io.fire)
  val stage1K =
    RegEnable(stage1KNext, VecInit(Seq.fill(laneCount)(0.S(kWidth.W))), io.fire)
  val stage1FracQ = RegEnable(
    stage1FracQNext,
    VecInit(Seq.fill(laneCount)(0.U(fmt.tFracBits.W))),
    io.fire
  )

  val stage2SpecialNext = Wire(Vec(laneCount, Bool()))
  val stage2SpecialResultNext = Wire(Vec(laneCount, UInt(fmt.width.W)))
  val stage2SpecialFlagsNext = Wire(Vec(laneCount, UInt(5.W)))
  val stage2HasFracNext = Wire(Vec(laneCount, Bool()))
  val stage2KNext = Wire(Vec(laneCount, SInt(kWidth.W)))
  val stage2LocalTNext = Wire(Vec(laneCount, UInt(fmt.tFracBits.W)))
  val stage2CoeffBNext = Wire(Vec(laneCount, UInt(coeffWidth.W)))
  val stage2CoeffCNext = Wire(Vec(laneCount, UInt(coeffWidth.W)))
  val stage2ProductNext = Wire(Vec(laneCount, UInt(product1Width.W)))

  for (lane <- 0 until laneCount) {
    val fq = stage1FracQ(lane)
    val seg = Mux(
      fq === 0.U,
      0.U(fmt.segmentBits.W),
      fq(fmt.tFracBits - 1, fmt.tFracBits - fmt.segmentBits)
    )
    val locT = fq - (seg << (fmt.tFracBits - fmt.segmentBits))
    stage2SpecialNext(lane) := stage1Special(lane)
    stage2SpecialResultNext(lane) := stage1SpecialResult(lane)
    stage2SpecialFlagsNext(lane) := stage1SpecialFlags(lane)
    stage2HasFracNext(lane) := stage1HasFrac(lane)
    stage2KNext(lane) := stage1K(lane)
    stage2LocalTNext(lane) := locT
    stage2CoeffBNext(lane) := coeffB(seg)
    stage2CoeffCNext(lane) := coeffC(seg)
    stage2ProductNext(lane) := coeffA(seg) * locT(localTKeep - 1, 0)
  }

  val stage2Special =
    RegEnable(stage2SpecialNext, VecInit(Seq.fill(laneCount)(false.B)), fireS1)
  val stage2SpecialResult =
    RegEnable(
      stage2SpecialResultNext,
      VecInit(Seq.fill(laneCount)(0.U(fmt.width.W))),
      fireS1
    )
  val stage2SpecialFlags = RegEnable(
    stage2SpecialFlagsNext,
    VecInit(Seq.fill(laneCount)(0.U(5.W))),
    fireS1
  )
  val stage2HasFrac =
    RegEnable(stage2HasFracNext, VecInit(Seq.fill(laneCount)(false.B)), fireS1)
  val stage2K =
    RegEnable(stage2KNext, VecInit(Seq.fill(laneCount)(0.S(kWidth.W))), fireS1)
  val stage2LocalT = RegEnable(
    stage2LocalTNext,
    VecInit(Seq.fill(laneCount)(0.U(fmt.tFracBits.W))),
    fireS1
  )
  val stage2CoeffB = RegEnable(
    stage2CoeffBNext,
    VecInit(Seq.fill(laneCount)(0.U(coeffWidth.W))),
    fireS1
  )
  val stage2CoeffC = RegEnable(
    stage2CoeffCNext,
    VecInit(Seq.fill(laneCount)(0.U(coeffWidth.W))),
    fireS1
  )
  val stage2Product = RegEnable(
    stage2ProductNext,
    VecInit(Seq.fill(laneCount)(0.U(product1Width.W))),
    fireS1
  )

  val stage3SpecialNext = Wire(Vec(laneCount, Bool()))
  val stage3SpecialResultNext = Wire(Vec(laneCount, UInt(fmt.width.W)))
  val stage3SpecialFlagsNext = Wire(Vec(laneCount, UInt(5.W)))
  val stage3HasFracNext = Wire(Vec(laneCount, Bool()))
  val stage3KNext = Wire(Vec(laneCount, SInt(kWidth.W)))
  val stage3LocalTNext = Wire(Vec(laneCount, UInt(fmt.tFracBits.W)))
  val stage3CoeffCNext = Wire(Vec(laneCount, UInt(coeffWidth.W)))
  val stage3HornerNext = Wire(Vec(laneCount, UInt(horner1Width.W)))

  for (lane <- 0 until laneCount) {
    stage3SpecialNext(lane) := stage2Special(lane)
    stage3SpecialResultNext(lane) := stage2SpecialResult(lane)
    stage3SpecialFlagsNext(lane) := stage2SpecialFlags(lane)
    stage3HasFracNext(lane) := stage2HasFrac(lane)
    stage3KNext(lane) := stage2K(lane)
    stage3LocalTNext(lane) := stage2LocalT(lane)
    stage3CoeffCNext(lane) := stage2CoeffC(lane)
    stage3HornerNext(lane) := stage2Product(lane) +& (stage2CoeffB(
      lane
    ) << fmt.tFracBits)
  }

  val stage3Special =
    RegEnable(stage3SpecialNext, VecInit(Seq.fill(laneCount)(false.B)), fireS2)
  val stage3SpecialResult =
    RegEnable(
      stage3SpecialResultNext,
      VecInit(Seq.fill(laneCount)(0.U(fmt.width.W))),
      fireS2
    )
  val stage3SpecialFlags = RegEnable(
    stage3SpecialFlagsNext,
    VecInit(Seq.fill(laneCount)(0.U(5.W))),
    fireS2
  )
  val stage3HasFrac =
    RegEnable(stage3HasFracNext, VecInit(Seq.fill(laneCount)(false.B)), fireS2)
  val stage3K =
    RegEnable(stage3KNext, VecInit(Seq.fill(laneCount)(0.S(kWidth.W))), fireS2)
  val stage3LocalT = RegEnable(
    stage3LocalTNext,
    VecInit(Seq.fill(laneCount)(0.U(fmt.tFracBits.W))),
    fireS2
  )
  val stage3CoeffC = RegEnable(
    stage3CoeffCNext,
    VecInit(Seq.fill(laneCount)(0.U(coeffWidth.W))),
    fireS2
  )
  val stage3Horner = RegEnable(
    stage3HornerNext,
    VecInit(Seq.fill(laneCount)(0.U(horner1Width.W))),
    fireS2
  )

  val stage4SpecialNext = Wire(Vec(laneCount, Bool()))
  val stage4SpecialResultNext = Wire(Vec(laneCount, UInt(fmt.width.W)))
  val stage4SpecialFlagsNext = Wire(Vec(laneCount, UInt(5.W)))
  val stage4HasFracNext = Wire(Vec(laneCount, Bool()))
  val stage4KNext = Wire(Vec(laneCount, SInt(kWidth.W)))
  val stage4CoeffCNext = Wire(Vec(laneCount, UInt(coeffWidth.W)))
  val stage4ProductNext = Wire(Vec(laneCount, UInt(product2Width.W)))

  for (lane <- 0 until laneCount) {
    stage4SpecialNext(lane) := stage3Special(lane)
    stage4SpecialResultNext(lane) := stage3SpecialResult(lane)
    stage4SpecialFlagsNext(lane) := stage3SpecialFlags(lane)
    stage4HasFracNext(lane) := stage3HasFrac(lane)
    stage4KNext(lane) := stage3K(lane)
    stage4CoeffCNext(lane) := stage3CoeffC(lane)
    val horner1T =
      stage3Horner(lane)(horner1Width - 1, horner1Width - horner1Keep)
    val localTT = stage3LocalT(lane)(localTKeep - 1, 0)
    val productTrunc = horner1T * localTT
    stage4ProductNext(lane) := productTrunc << horner1Shift
  }

  val stage4Special =
    RegEnable(stage4SpecialNext, VecInit(Seq.fill(laneCount)(false.B)), fireS3)
  val stage4SpecialResult =
    RegEnable(
      stage4SpecialResultNext,
      VecInit(Seq.fill(laneCount)(0.U(fmt.width.W))),
      fireS3
    )
  val stage4SpecialFlags = RegEnable(
    stage4SpecialFlagsNext,
    VecInit(Seq.fill(laneCount)(0.U(5.W))),
    fireS3
  )
  val stage4HasFrac =
    RegEnable(stage4HasFracNext, VecInit(Seq.fill(laneCount)(false.B)), fireS3)
  val stage4K =
    RegEnable(stage4KNext, VecInit(Seq.fill(laneCount)(0.S(kWidth.W))), fireS3)
  val stage4CoeffC = RegEnable(
    stage4CoeffCNext,
    VecInit(Seq.fill(laneCount)(0.U(coeffWidth.W))),
    fireS3
  )
  val stage4Product = RegEnable(
    stage4ProductNext,
    VecInit(Seq.fill(laneCount)(0.U(product2Width.W))),
    fireS3
  )

  val stage5SpecialNext = Wire(Vec(laneCount, Bool()))
  val stage5SpecialResultNext = Wire(Vec(laneCount, UInt(fmt.width.W)))
  val stage5SpecialFlagsNext = Wire(Vec(laneCount, UInt(5.W)))
  val stage5HasFracNext = Wire(Vec(laneCount, Bool()))
  val stage5KNext = Wire(Vec(laneCount, SInt(kWidth.W)))
  val stage5SigScaledNext = Wire(Vec(laneCount, UInt(sigScaledWidth.W)))

  for (lane <- 0 until laneCount) {
    stage5SpecialNext(lane) := stage4Special(lane)
    stage5SpecialResultNext(lane) := stage4SpecialResult(lane)
    stage5SpecialFlagsNext(lane) := stage4SpecialFlags(lane)
    stage5HasFracNext(lane) := stage4HasFrac(lane)
    stage5KNext(lane) := stage4K(lane)
    stage5SigScaledNext(lane) := stage4Product(lane) +& (stage4CoeffC(
      lane
    ) << (2 * fmt.tFracBits))
  }

  val stage5Special =
    RegEnable(stage5SpecialNext, VecInit(Seq.fill(laneCount)(false.B)), fireS4)
  val stage5SpecialResult =
    RegEnable(
      stage5SpecialResultNext,
      VecInit(Seq.fill(laneCount)(0.U(fmt.width.W))),
      fireS4
    )
  val stage5SpecialFlags = RegEnable(
    stage5SpecialFlagsNext,
    VecInit(Seq.fill(laneCount)(0.U(5.W))),
    fireS4
  )
  val stage5HasFrac =
    RegEnable(stage5HasFracNext, VecInit(Seq.fill(laneCount)(false.B)), fireS4)
  val stage5K =
    RegEnable(stage5KNext, VecInit(Seq.fill(laneCount)(0.S(kWidth.W))), fireS4)
  val stage5SigScaled = RegEnable(
    stage5SigScaledNext,
    VecInit(Seq.fill(laneCount)(0.U(sigScaledWidth.W))),
    fireS4
  )

  val resultNext = Wire(Vec(laneCount, UInt(fmt.width.W)))
  val flagsNext = Wire(Vec(laneCount, UInt(5.W)))

  for (lane <- 0 until laneCount) {
    val (finiteResult, finiteFlags) =
      roundAndPackPositive(
        stage5SigScaled(lane),
        fmt,
        stage5K(lane),
        rmS5,
        stage5HasFrac(lane)
      )
    resultNext(lane) := Mux(
      stage5Special(lane),
      stage5SpecialResult(lane),
      finiteResult
    )
    flagsNext(lane) := Mux(
      stage5Special(lane),
      stage5SpecialFlags(lane),
      finiteFlags
    )
  }

  io.result := RegEnable(
    resultNext,
    VecInit(Seq.fill(laneCount)(0.U(fmt.width.W))),
    fireS5
  )
  io.fflags := RegEnable(
    flagsNext,
    VecInit(Seq.fill(laneCount)(0.U(5.W))),
    fireS5
  )
}

class VfExp2(width: Int = 64) extends Module {
  require(width == 64)

  val io = IO(new Bundle {
    val fire = Input(Bool())
    val src = Input(UInt(width.W))
    val opType = Input(UInt(8.W))
    val sew = Input(UInt(2.W))
    val rm = Input(UInt(3.W))
    val result = Output(UInt(width.W))
    val fflags = Output(UInt(20.W))
    val valid = Output(Bool())
  })

  private val isFpVfexp2 = io.opType === VfexpType.vfexp2
  private val isBf16Vfexp2 = io.opType === VfexpType.vfexp2bf16
  private val isVfexp2 = isFpVfexp2 || isBf16Vfexp2
  private val vfexp2Fire = io.fire && isVfexp2

  private val lanes16 = io.src.asTypeOf(Vec(4, UInt(16.W)))
  private val lanes32 = io.src.asTypeOf(Vec(2, UInt(32.W)))

  private val bf16Pipe = Module(new VfExp2Pipe(VfExp2Tables.bf16, 4))
  private val fp16Pipe = Module(new VfExp2Pipe(VfExp2Tables.fp16, 4))
  private val fp32Pipe = Module(new VfExp2Pipe(VfExp2Tables.fp32, 2))

  bf16Pipe.io.fire := vfexp2Fire
  bf16Pipe.io.src := lanes16
  bf16Pipe.io.rm := io.rm

  fp16Pipe.io.fire := vfexp2Fire
  fp16Pipe.io.src := lanes16
  fp16Pipe.io.rm := io.rm

  fp32Pipe.io.fire := vfexp2Fire
  fp32Pipe.io.src := lanes32
  fp32Pipe.io.rm := io.rm

  val fireS1 = GatedValidRegNext(vfexp2Fire)
  val fireS2 = GatedValidRegNext(fireS1)
  val fireS3 = GatedValidRegNext(fireS2)
  val fireS4 = GatedValidRegNext(fireS3)
  val fireS5 = GatedValidRegNext(fireS4)
  val isBf16S1 = RegEnable(isBf16Vfexp2, false.B, vfexp2Fire)
  val isBf16S2 = RegEnable(isBf16S1, false.B, fireS1)
  val isBf16S3 = RegEnable(isBf16S2, false.B, fireS2)
  val isBf16S4 = RegEnable(isBf16S3, false.B, fireS3)
  val isBf16S5 = RegEnable(isBf16S4, false.B, fireS4)
  val isBf16S6 = RegEnable(isBf16S5, false.B, fireS5)

  io.valid := fireS5

  val sewS1 = RegEnable(io.sew, 0.U(2.W), vfexp2Fire)
  val sewS2 = RegEnable(sewS1, 0.U(2.W), fireS1)
  val sewS3 = RegEnable(sewS2, 0.U(2.W), fireS2)
  val sewS4 = RegEnable(sewS3, 0.U(2.W), fireS3)
  val sewS5 = RegEnable(sewS4, 0.U(2.W), fireS4)
  val sewS6 = RegEnable(sewS5, 0.U(2.W), fireS5)

  private val bf16Result =
    bf16Pipe.io.result(3) ## bf16Pipe.io.result(2) ## bf16Pipe.io.result(
      1
    ) ## bf16Pipe.io.result(0)
  private val fp16Result =
    fp16Pipe.io.result(3) ## fp16Pipe.io.result(2) ## fp16Pipe.io.result(
      1
    ) ## fp16Pipe.io.result(0)
  private val fp32Result = fp32Pipe.io.result(1) ## fp32Pipe.io.result(0)

  private val bf16Flags =
    bf16Pipe.io.fflags(3) ## bf16Pipe.io.fflags(2) ## bf16Pipe.io.fflags(
      1
    ) ## bf16Pipe.io.fflags(0)
  private val fp16Flags =
    fp16Pipe.io.fflags(3) ## fp16Pipe.io.fflags(2) ## fp16Pipe.io.fflags(
      1
    ) ## fp16Pipe.io.fflags(0)
  private val fp32Flags =
    0.U(10.W) ## fp32Pipe.io.fflags(1) ## fp32Pipe.io.fflags(0)

  private val isFp32S6 = sewS6 === 2.U
  io.result := Mux(isBf16S6, bf16Result, Mux(isFp32S6, fp32Result, fp16Result))
  io.fflags := Mux(isBf16S6, bf16Flags, Mux(isFp32S6, fp32Flags, fp16Flags))
}
