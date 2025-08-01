/**
  * Integer and fixed-point (except mult and div)
  *   
  * Perform below instructions:
  *     11.1  vadd, ...
  *     11.2  vwadd, ...
  *     11.3  vzext, ...
  *     11.4  vadc, vmadc, ...
  *     11.5  vand, ...
  *     11.6  vsll, ...
  *     11.7  vnsrl, ...
  *     11.8  vmseq, vmsltu, ...
  *     11.9  vminu, ...
  *     11.15 vmerge
  *     11.16 vmv.v.
  *     12.1  vsadd, ...
  *     12.2  vaadd, ...
  *     12.4  vssrl, ...
  *     12.5  vnclip, ...
  */

package yunsuan.vector.alu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import yunsuan.vector._
import yunsuan.vector.alu.VAluOpcode._

class VIntFixpDecode extends Bundle {
  val sub = Bool()
  val misc = Bool()
  val cmp = Bool()
}

class VIntFixpAlu64b extends Module {
  val io = IO(new Bundle {
    val fire = Input(Bool())
    val opcode = Input(new VAluOpcode)
    val info = Input(new VIFuInfo)
    val srcType = Input(Vec(2, UInt(4.W)))
    val vdType  = Input(UInt(4.W))
    val vs1 = Input(UInt(64.W))
    val vs2_adder = Input(UInt(64.W))
    val vs2_misc = Input(UInt(64.W))
    val vmask = Input(UInt(8.W))
    val oldVd = Input(UInt(8.W))
    val narrow = Input(UInt(8.W))
    val isSub = Input(Bool())  // subtract
    val isMisc = Input(Bool())
    val isFixp = Input(Bool())
    val widen = Input(Bool())
    val widen_vs2 = Input(Bool())

    val vd = Output(UInt(64.W))
    val narrowVd = Output(UInt(32.W))
    val cmpOut = Output(UInt(8.W))
    val vxsat = Output(UInt(8.W))
  })

  val fire = io.fire
  val srcTypeVs2 = io.srcType(0)

  val vIntAdder64b = Module(new VIntAdder64b)
  vIntAdder64b.io.opcode := io.opcode
  vIntAdder64b.io.info := io.info
  vIntAdder64b.io.srcType := io.srcType
  vIntAdder64b.io.vdType := io.vdType
  vIntAdder64b.io.vs1 := io.vs1
  vIntAdder64b.io.vs2 := io.vs2_adder
  vIntAdder64b.io.vmask := io.vmask
  vIntAdder64b.io.oldVd := io.oldVd
  vIntAdder64b.io.isSub := io.isSub
  vIntAdder64b.io.widen := io.widen
  vIntAdder64b.io.widen_vs2 := io.widen_vs2

  val vIntMisc64b = Module(new VIntMisc64b)
  vIntMisc64b.io.opcode := io.opcode
  vIntMisc64b.io.info := io.info
  vIntMisc64b.io.srcType := io.srcType
  vIntMisc64b.io.vdType := io.vdType
  vIntMisc64b.io.vs1 := io.vs1
  vIntMisc64b.io.vs2 := io.vs2_misc
  vIntMisc64b.io.vmask := io.vmask
  vIntMisc64b.io.narrow := io.narrow

  val vdAdderS1 = RegEnable(vIntAdder64b.io.vd, fire)
  val vdMiscS1 = RegEnable(vIntMisc64b.io.vd, fire)
  val isMiscS1 = RegEnable(io.isMisc, fire)
  val narrowVdMiscS1 = RegEnable(vIntMisc64b.io.narrowVd, fire)
  val cmpOutS1 = RegEnable(vIntAdder64b.io.cmpOut, fire)
  val isFixpS1 = RegEnable(io.isFixp, fire)

  val vFixPoint64b = Module(new VFixPoint64b)
  vFixPoint64b.io.opcode := RegEnable(io.opcode, fire)
  vFixPoint64b.io.info := RegEnable(io.info, fire)
  vFixPoint64b.io.sew := RegEnable(SewOH(io.vdType(1, 0)), fire)
  vFixPoint64b.io.isSub := RegEnable(io.isSub, fire)
  vFixPoint64b.io.isSigned := RegEnable(srcTypeVs2(3, 2) === 1.U, fire)
  vFixPoint64b.io.isNClip := RegEnable(io.opcode.isScalingShift && io.vdType(1,0) =/= srcTypeVs2(1,0), fire)
  vFixPoint64b.io.fromAdder := RegEnable(vIntAdder64b.io.toFixP, fire)
  vFixPoint64b.io.fromMisc := RegEnable(vIntMisc64b.io.toFixP, fire)

  io.vd := Mux(isFixpS1, vFixPoint64b.io.vd, Mux(isMiscS1, vdMiscS1, vdAdderS1))
  io.narrowVd := Mux(isFixpS1, vFixPoint64b.io.narrowVd, narrowVdMiscS1)
  io.cmpOut := cmpOutS1
  io.vxsat := vFixPoint64b.io.vxsat
}
