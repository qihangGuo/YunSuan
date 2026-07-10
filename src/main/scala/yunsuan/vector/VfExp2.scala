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
    tFracBits = 17,
    coeffFracBits = 20,
    coeffs = Seq(
      (252950, 726809, 1048576),
      (253926, 730761, 1054270),
      (254980, 734730, 1059994),
      (256412, 738718, 1065750),
      (257726, 742731, 1071537),
      (259293, 746764, 1077355),
      (260584, 750818, 1083205),
      (262669, 754890, 1089087),
      (263402, 758994, 1095000),
      (265682, 763108, 1100946),
      (266896, 767255, 1106924),
      (268249, 771422, 1112935),
      (269856, 775609, 1118978),
      (270942, 779826, 1125054),
      (272731, 784057, 1131163),
      (274108, 788315, 1137305),
      (275254, 792597, 1143480),
      (276908, 796903, 1149689),
      (278100, 801228, 1155932),
      (279529, 805580, 1162209),
      (281304, 809954, 1168519),
      (283099, 814350, 1174864),
      (284983, 818766, 1181244),
      (285690, 823219, 1187658),
      (288096, 827682, 1194106),
      (289646, 832176, 1200590),
      (290339, 836702, 1207109),
      (291996, 841244, 1213664),
      (293601, 845812, 1220254),
      (296120, 850396, 1226880),
      (296819, 855026, 1233542),
      (298510, 859667, 1240240),
      (300888, 864326, 1246974),
      (302509, 869019, 1253745),
      (303538, 873746, 1260553),
      (304834, 878491, 1267397),
      (307404, 883253, 1274279),
      (308248, 888055, 1281198),
      (310203, 892879, 1288155),
      (311596, 897726, 1295150),
      (313215, 902601, 1302182),
      (314924, 907502, 1309253),
      (316670, 912429, 1316362),
      (318433, 917383, 1323510),
      (320550, 922365, 1330696),
      (321870, 927373, 1337922),
      (323636, 932409, 1345187),
      (325744, 937471, 1352491),
      (327100, 942563, 1359835),
      (328840, 947681, 1367219),
      (330701, 952826, 1374642),
      (332423, 958001, 1382107),
      (334962, 963200, 1389611),
      (336053, 968433, 1397157),
      (338326, 973692, 1404743),
      (340673, 978970, 1412371),
      (341642, 984293, 1420040),
      (344340, 989631, 1427751),
      (345351, 995011, 1435503),
      (347160, 1000415, 1443298),
      (349023, 1005847, 1451135),
      (350939, 1011309, 1459014),
      (353892, 1016791, 1466937),
      (355748, 1022313, 1474902),
      (356749, 1027871, 1482910),
      (359515, 1033447, 1490962),
      (360727, 1039068, 1499058),
      (362578, 1044706, 1507198),
      (364836, 1050381, 1515382),
      (367230, 1056081, 1523610),
      (368444, 1061818, 1531883),
      (370456, 1067583, 1540201),
      (372502, 1073380, 1548564),
      (375841, 1079196, 1556973),
      (376701, 1085066, 1565427),
      (379767, 1090950, 1573927),
      (380714, 1096883, 1582474),
      (382795, 1102839, 1591066),
      (384788, 1108828, 1599706),
      (387315, 1114849, 1608392),
      (389808, 1120899, 1617125),
      (392156, 1126981, 1625906),
      (393258, 1133108, 1634735),
      (395339, 1139261, 1643611),
      (397555, 1145446, 1652536),
      (399642, 1151667, 1661509),
      (401887, 1157919, 1670531),
      (404018, 1164208, 1679601),
      (406254, 1170528, 1688722),
      (409472, 1176877, 1697891),
      (410593, 1183276, 1707110),
      (412826, 1189701, 1716380),
      (416312, 1196150, 1725700),
      (418211, 1202651, 1735070),
      (420790, 1209176, 1744491),
      (423106, 1215742, 1753964),
      (424270, 1222351, 1763488),
      (427901, 1228977, 1773063),
      (428797, 1235663, 1782691),
      (431131, 1242373, 1792371),
      (433866, 1249120, 1802103),
      (437235, 1255889, 1811888),
      (438159, 1262721, 1821727),
      (440612, 1269577, 1831618),
      (443011, 1276470, 1841564),
      (445900, 1283405, 1851563),
      (449388, 1290356, 1861617),
      (450194, 1297378, 1871726),
      (452657, 1304422, 1881889),
      (455145, 1311505, 1892107),
      (458950, 1318615, 1902381),
      (460053, 1325787, 1912711),
      (462621, 1332984, 1923097),
      (465107, 1340223, 1933539),
      (467296, 1347507, 1944038),
      (470130, 1354817, 1954594),
      (472668, 1362174, 1965207),
      (475262, 1369571, 1975878),
      (479458, 1376992, 1986607),
      (481464, 1384481, 1997394),
      (483083, 1392001, 2008240),
      (485649, 1399560, 2019144),
      (488276, 1407160, 2030108),
      (492271, 1414790, 2041131),
      (493665, 1422482, 2052214),
      (497718, 1430195, 2063358),
      (498990, 1437972, 2074562),
      (503247, 1445767, 2085826)
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
    tFracBits = 24,
    coeffFracBits = 24,
    coeffs = Seq(
      (4047204, 11628951, 16777216),
      (4062809, 11692177, 16868315),
      (4079680, 11755674, 16959908),
      (4102585, 11819495, 17051999),
      (4123608, 11883694, 17144589),
      (4148689, 11948223, 17237683),
      (4169347, 12013085, 17331282),
      (4202709, 12078237, 17425389),
      (4214439, 12143904, 17520007),
      (4250909, 12209723, 17615139),
      (4270344, 12276077, 17710787),
      (4291982, 12342758, 17806955),
      (4317697, 12409741, 17903645),
      (4335079, 12477208, 18000860),
      (4363696, 12544918, 18098603),
      (4385726, 12613047, 18196877),
      (4404063, 12681558, 18295684),
      (4430535, 12750450, 18395027),
      (4449597, 12819643, 18494911),
      (4472467, 12889272, 18595336),
      (4500867, 12959263, 18696307),
      (4529585, 13029593, 18797826),
      (4559735, 13100256, 18899897),
      (4571043, 13171500, 19002521),
      (4609537, 13242907, 19105703),
      (4634340, 13314820, 19209445),
      (4645418, 13387234, 19313750),
      (4671944, 13459907, 19418622),
      (4697618, 13532989, 19524063),
      (4737925, 13606342, 19630078),
      (4749109, 13680421, 19736666),
      (4776157, 13754666, 19843835),
      (4814208, 13829212, 19951585),
      (4840142, 13904306, 20059920),
      (4856607, 13979941, 20168843),
      (4877342, 14055851, 20278358),
      (4918459, 14132049, 20388468),
      (4931967, 14208885, 20499175),
      (4963254, 14286058, 20610483),
      (4985539, 14363612, 20722396),
      (5011442, 14441621, 20834917),
      (5038784, 14520036, 20948048),
      (5066727, 14598870, 21061794),
      (5094927, 14678131, 21176158),
      (5128801, 14757841, 21291142),
      (5149921, 14837972, 21406751),
      (5178170, 14918537, 21522988),
      (5211897, 14999535, 21639855),
      (5233595, 15081003, 21757357),
      (5261447, 15162901, 21875498),
      (5291211, 15245215, 21994280),
      (5318771, 15328012, 22113706),
      (5359392, 15411207, 22233781),
      (5376855, 15494921, 22354509),
      (5413219, 15579065, 22475891),
      (5450772, 15663527, 22597934),
      (5466271, 15748682, 22720638),
      (5509437, 15834103, 22844009),
      (5525617, 15920177, 22968050),
      (5554562, 16006638, 23092764),
      (5584373, 16093558, 23218155),
      (5615020, 16180938, 23344227),
      (5662269, 16268660, 23470985),
      (5691969, 16357015, 23598430),
      (5707980, 16445940, 23726567),
      (5752240, 16535156, 23855400),
      (5771625, 16625093, 23984932),
      (5801245, 16715303, 24115168),
      (5837368, 16806089, 24246111),
      (5875684, 16897300, 24377765),
      (5895111, 16989086, 24510133),
      (5927302, 17081332, 24643221),
      (5960024, 17174074, 24777031),
      (6013453, 17267129, 24911569),
      (6027222, 17361058, 25046836),
      (6076274, 17455197, 25182838),
      (6091424, 17550124, 25319578),
      (6124719, 17645416, 25457061),
      (6156616, 17741249, 25595290),
      (6197043, 17837586, 25734270),
      (6236930, 17934381, 25874004),
      (6274498, 18031690, 26014498),
      (6292133, 18129722, 26155754),
      (6325425, 18228176, 26297777),
      (6360881, 18327138, 26440571),
      (6394267, 18426669, 26584141),
      (6430198, 18526707, 26728490),
      (6464293, 18627332, 26873623),
      (6500056, 18728451, 27019544),
      (6551551, 18830034, 27166258),
      (6569486, 18932411, 27313768),
      (6605223, 19035211, 27462079),
      (6660998, 19138405, 27611196),
      (6691377, 19242412, 27761121),
      (6732640, 19346820, 27911862),
      (6769693, 19451865, 28063421),
      (6788319, 19557624, 28215802),
      (6846411, 19663635, 28369011),
      (6860750, 19770614, 28523052),
      (6898096, 19877965, 28677929),
      (6941859, 19985922, 28833647),
      (6995754, 20094222, 28990212),
      (7010538, 20203541, 29147625),
      (7049791, 20313227, 29305894),
      (7088172, 20423525, 29465022),
      (7134395, 20534488, 29625014),
      (7190208, 20645690, 29785876),
      (7203108, 20758050, 29947609),
      (7242516, 20870753, 30110222),
      (7282316, 20984073, 30273718),
      (7343200, 21097839, 30438101),
      (7360855, 21212585, 30603377),
      (7401943, 21327751, 30769550),
      (7441713, 21443565, 30936626),
      (7476731, 21560108, 31104608),
      (7522077, 21677080, 31273503),
      (7562687, 21794788, 31443315),
      (7604195, 21913135, 31614049),
      (7671323, 22031874, 31785711),
      (7703424, 22151701, 31958304),
      (7729330, 22272016, 32131834),
      (7770385, 22392964, 32306307),
      (7812421, 22514559, 32481727),
      (7876339, 22636645, 32658100),
      (7898645, 22759709, 32835430),
      (7963482, 22883120, 33013723),
      (7983832, 23007558, 33192984),
      (8051957, 23132274, 33373219)
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
  val stage1LocalTNext = Wire(Vec(laneCount, UInt(fmt.tFracBits.W)))
  val stage1CoeffANext = Wire(Vec(laneCount, UInt(coeffWidth.W)))
  val stage1CoeffBNext = Wire(Vec(laneCount, UInt(coeffWidth.W)))
  val stage1CoeffCNext = Wire(Vec(laneCount, UInt(coeffWidth.W)))

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
    stage1LocalTNext(lane) := localT
    stage1CoeffANext(lane) := coeffA(segIdx)
    stage1CoeffBNext(lane) := coeffB(segIdx)
    stage1CoeffCNext(lane) := coeffC(segIdx)
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
  val stage1LocalT = RegEnable(
    stage1LocalTNext,
    VecInit(Seq.fill(laneCount)(0.U(fmt.tFracBits.W))),
    io.fire
  )
  val stage1CoeffA = RegEnable(
    stage1CoeffANext,
    VecInit(Seq.fill(laneCount)(0.U(coeffWidth.W))),
    io.fire
  )
  val stage1CoeffB = RegEnable(
    stage1CoeffBNext,
    VecInit(Seq.fill(laneCount)(0.U(coeffWidth.W))),
    io.fire
  )
  val stage1CoeffC = RegEnable(
    stage1CoeffCNext,
    VecInit(Seq.fill(laneCount)(0.U(coeffWidth.W))),
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
    val locT = stage1LocalT(lane)
    stage2SpecialNext(lane) := stage1Special(lane)
    stage2SpecialResultNext(lane) := stage1SpecialResult(lane)
    stage2SpecialFlagsNext(lane) := stage1SpecialFlags(lane)
    stage2HasFracNext(lane) := stage1HasFrac(lane)
    stage2KNext(lane) := stage1K(lane)
    stage2LocalTNext(lane) := locT
    stage2CoeffBNext(lane) := stage1CoeffB(lane)
    stage2CoeffCNext(lane) := stage1CoeffC(lane)
    stage2ProductNext(lane) := stage1CoeffA(lane) * locT(localTKeep - 1, 0)
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
