package yunsuan.vector


import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage._
import chiseltest._
import chiseltest.ChiselScalatestTester
import chiseltest.VerilatorBackendAnnotation
import chiseltest.simulator.{VerilatorFlags, VerilatorCFlags}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.vector.v2.MergeUnit

import scala.math.BigInt
import scala.util.Random

object GenTest extends App {
  val path = """./generated/MergeUnit"""
  (new ChiselStage).execute(
    Array(
      "--target", "systemverilog",
      "--target-dir", path
    ),
    Seq(
      ChiselGeneratorAnnotation(() => MergeUnit(128)),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--strip-debug-info")
    )
  )
}

trait HasTestAnnos {
  var testAnnos: firrtl.AnnotationSeq = Seq()
}

// trait UseVerilatorBackend { this: HasTestAnnos =>
//   testAnnos = testAnnos ++ Seq(VerilatorBackendAnnotation)
// 
// }

class YunSuanTester  extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasTestAnnos {
  behavior of "YunSuan Test"
}

class VFloatAdderTest extends YunSuanTester {

  behavior of "YunSuan VectorFloatAdder"
  it should "pass the syntax" in {
    test(new VectorFloatAdder()).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq()),
      // WriteVcdAnnotation,
      // TargetDirAnnotation("./build"),
    )) { dut =>
      dut.clock.step(10)
    }
  }
}

class VFloatDividerTest extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasTestAnnos {

  behavior of "YunSuan VectorFloatDivider"
  it should "pass the syntax" in {
    test(new VectorFloatDivider()).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq()),
      // WriteVcdAnnotation,
      // TargetDirAnnotation("./build"),
    )) { dut =>
      dut.clock.step(10)
    }
  }
}

class VFloatFMATest extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasTestAnnos {

  behavior of "YunSuan VectorFloatFMA"
  it should "pass the syntax" in {
    test(new VectorFloatFMA()).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq()),
      // WriteVcdAnnotation,
      // TargetDirAnnotation("./build"),
    )) { dut =>
      dut.clock.step(10)
    }
  }
}

class VIntAdderTest extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasTestAnnos {

  behavior of "YunSuan VectorIntAdder"
  it should "pass the syntax" in {
    test(new VectorIntAdder()).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq()),
      // WriteVcdAnnotation,
      // TargetDirAnnotation("./build"),
    )) { dut =>
      dut.clock.step(10)
    }
  }
}

class VSlideUpLookupTest extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasTestAnnos {

  behavior of "YunSuan VectorSlideUpLookup"
  it should "pass the syntax" in {
    test(new SlideUpLookupModule()).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq()),
      // WriteVcdAnnotation,
      // TargetDirAnnotation("./build"),
    )) { dut =>
      dut.clock.step(10)
    }
  }
}

class VSlide1UpTest extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasTestAnnos {

  behavior of "YunSuan VectorSlide1Up"
  it should "pass the syntax" in {
    test(new Slide1UpModule()).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq()),
      // WriteVcdAnnotation,
      // TargetDirAnnotation("./build"),
    )) { dut =>
      dut.clock.step(10)
    }
  }
}

class VSlideDownLookupTest extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasTestAnnos {

  behavior of "YunSuan VectorSlideDownLookup"
  it should "pass the syntax" in {
    test(new SlideDownLookupModule()).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq()),
      // WriteVcdAnnotation,
      // TargetDirAnnotation("./build"),
    )) { dut =>
      dut.clock.step(10)
    }
  }
}

class VSlide1DownTest extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasTestAnnos {

  behavior of "YunSuan VectorSlide1Down"
  it should "pass the syntax" in {
    test(new Slide1DownModule()).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq()),
      // WriteVcdAnnotation,
      // TargetDirAnnotation("./build"),
    )) { dut =>
      dut.clock.step(10)
    }
  }
}

class VRGatherLookupTest extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasTestAnnos {

  behavior of "YunSuan VectorRegGatherLookup"
  it should "pass the syntax" in {
    test(new VRGatherLookupModule()).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq()),
      // WriteVcdAnnotation,
      // TargetDirAnnotation("./build"),
    )) { dut =>
      dut.clock.step(10)
    }
  }
}

class VCompressTest extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasTestAnnos {

  behavior of "YunSuan VectorCompress"
  it should "pass the syntax" in {
    test(new CompressModule()).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq()),
      // WriteVcdAnnotation,
      // TargetDirAnnotation("./build"),
    )) { dut =>
      dut.clock.step(10)
    }
  }
}
class VIntDividerTest extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasTestAnnos {

  behavior of "YunSuan VectorIntDivider"
  it should "pass the syntax" in {
    test(new VectorIdiv()).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq()),
      // WriteVcdAnnotation,
      // TargetDirAnnotation("./build"),
    )) { dut =>
      dut.clock.step(10)
    }
  }
}

class MergeUnitTest extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasTestAnnos {

  def refModel(
    vlenb: Int,
    mask: BigInt,
    begin: Int,
    end: Int,
    vma: Boolean,
    vta: Boolean
  ): (BigInt, BigInt) = {
    val allOnes = (BigInt(1) << vlenb) - BigInt(1)
    val width = end - begin

    val body = (((BigInt(1) << width) - BigInt(1)) << begin) & allOnes
    val tail = (~((BigInt(1) << end) - BigInt(1))) & allOnes

    val activeEn = body & mask
    val agnosticEn =
      (if (vma) body & ~mask else BigInt(0)) |
      (if (vta) tail else BigInt(0))
    
    (activeEn, agnosticEn)
  }

  behavior of "MergeUnit"
  it should "compute activeEn, agnosticEn, res correctly" in {
    test(MergeUnit(128)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>

      val vlenb = 128 / 8
      val rand = new Random()
      val num = 2000

      // reset
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)

      for (i <- 0 until num) {
        // random inputs
        val begin = rand.nextInt(vlenb)
        val end   = rand.nextInt(vlenb)
        val (b, e) = if (begin <= end) (begin, end) else (end, begin)

        val mask = BigInt(vlenb, rand)
        val vma = rand.nextBoolean()
        val vta = rand.nextBoolean()

        val vd = Seq.fill(vlenb)(rand.nextInt(256))
        val oldVd = Seq.fill(vlenb)(rand.nextInt(256))

        // poke
        dut.in.ctrl.vma.poke(vma.B)
        dut.in.ctrl.vta.poke(vta.B)

        dut.in.data.mask.poke(mask.U)
        dut.in.data.begin.poke(b.U)
        dut.in.data.end.poke(e.U)

        vd.zipWithIndex.foreach { case (v, i) =>
          dut.in.data.vd(i).poke(v.U)
        }
        oldVd.zipWithIndex.foreach { case (v, i) =>
          dut.in.data.oldVd(i).poke(v.U)
        }

        dut.clock.step(1)

        // reference
        val (expectedActiveEn, expectedAgnosticEn) = refModel(vlenb, mask, b, e, vma, vta)

        dut.out.activeEn.expect(expectedActiveEn.U)
        dut.out.agnosticEn.expect(expectedAgnosticEn.U)

        val bytes1s = ((BigInt(1) << 8) - BigInt(1)).toInt

        val expectedRes = (0 until vlenb).map { i => 
          val a = (expectedActiveEn >> i) & BigInt(1)
          val b = (expectedAgnosticEn >> i) & BigInt(1)
          if (a == BigInt(1)) vd(i)
          else if (b == BigInt(1)) bytes1s
          else oldVd(i)
        }

        dut.out.res.zip(expectedRes).foreach { case (r, v) =>
          r.expect(v.U)
        }
      }

      dut.clock.step(5)
    }
  }
}
