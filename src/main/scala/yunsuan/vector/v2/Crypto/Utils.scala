package yunsuan.vector.v2.Crypto

import _root_.circt.stage.FirtoolOption
import chisel3._
import chisel3.util._
import yunsuan.vector.Common._

import scala.language.implicitConversions

package object Utils {
  object Zvknhb {
    private[Zvknhb] class sum1chModule(EEW: Int) extends Module {
      val h, g, f, e, kw = IO(Input(UInt(EEW.W)))
      val t1 = IO(Output(UInt(EEW.W)))

      t1 := h + sum1(EEW)(e) + ch(e, f, g) + kw
    }

    private[Zvknhb] object sum1chModule {
      def apply(
        EEW: Int,
      )(
        h: UInt, g: UInt, f: UInt, e: UInt, kw: UInt,
      ): UInt = {
        val sum1chMod = Module(new sum1chModule(EEW))
        sum1chMod.h := h
        sum1chMod.g := g
        sum1chMod.f := f
        sum1chMod.e := e
        sum1chMod.kw := kw
        sum1chMod.t1
      }
    }

    private[Zvknhb] class sum0majModule(EEW: Int) extends Module {
      val c, b, a = IO(Input(UInt(EEW.W)))
      val t2 = IO(Output(UInt(EEW.W)))

      t2 := sum0(EEW)(a) + maj(a, b, c)
    }

    private[Zvknhb] object sum0majModule {
      def apply(
        EEW: Int,
      )(
        c: UInt, b: UInt, a: UInt,
      ): UInt = {
        val sum0majMod = Module(new sum0majModule(EEW))
        sum0majMod.c := c
        sum0majMod.b := b
        sum0majMod.a := a
        sum0majMod.t2
      }
    }

    object SHA512 {
      val EEW = 64

      def sum0(x: UInt): UInt = x.rotateRight(28) ^ x.rotateRight(34) ^ x.rotateRight(39)

      def sum1(x: UInt): UInt = x.rotateRight(14) ^ x.rotateRight(18) ^ x.rotateRight(41)

      def sum0maj: (UInt, UInt, UInt) => UInt = sum0majModule(EEW)

      def sum1ch: (UInt, UInt, UInt, UInt, UInt) => UInt = sum1chModule(EEW)

      def sig0(x: UInt): UInt = x.rotateRight(1) ^ x.rotateRight(8) ^ x.>>(7).asUInt

      def sig1(x: UInt): UInt = x.rotateRight(19) ^ x.rotateRight(61) ^ x.>>(6).asUInt
    }

    object SHA256 {
      val EEW = 32

      def sum0(x: UInt): UInt = x.rotateRight(2) ^ x.rotateRight(13) ^ x.rotateRight(22)

      def sum1(x: UInt): UInt = x.rotateRight(6) ^ x.rotateRight(11) ^ x.rotateRight(25)

      def sum0maj: (UInt, UInt, UInt) => UInt = sum0majModule(EEW)

      def sum1ch: (UInt, UInt, UInt, UInt, UInt) => UInt = sum1chModule(EEW)

      def sig0(x: UInt): UInt = x.rotateRight(7) ^ x.rotateRight(18) ^ x.>>(3).asUInt

      def sig1(x: UInt): UInt = x.rotateRight(17) ^ x.rotateRight(19) ^ x.>>(10).asUInt
    }

    def ch(x: UInt, y: UInt, z: UInt): UInt = (x & y) ^ ((~x).asUInt & z)

    def maj(x: UInt, y: UInt, z: UInt): UInt = (x & y) ^ (x & z) ^ (y & z)

    def sum0(EEW: Int)(x: UInt): UInt = EEW match {
      case 32 => SHA256.sum0(x)
      case 64 => SHA512.sum0(x)
    }

    def sum1(EEW: Int)(x: UInt): UInt = EEW match {
      case 32 => SHA256.sum1(x)
      case 64 => SHA512.sum1(x)
    }
  }
}
