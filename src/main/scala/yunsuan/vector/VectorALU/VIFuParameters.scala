package yunsuan.vector

import chisel3.util.log2Ceil

object VIFuParam {
  val VLEN = 256
  val XLEN = 64
  val VLENB = VLEN / 8
  val maxUop = 8
  val wVL = log2Ceil(VLEN + 1)
  val wVSTART = log2Ceil(VLEN)
  val wUOPIDX = 6
  val wOPCODE = 6
}