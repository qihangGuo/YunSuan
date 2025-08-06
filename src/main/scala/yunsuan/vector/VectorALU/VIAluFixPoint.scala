package yunsuan.vector.VectorALU

import chisel3._
import chisel3.util._
import yunsuan.encoding.Opcode.VIAluOpcode
import yunsuan.util.GatedValidRegNext

class VIAluInfo extends Bundle {
  val vsew = UInt(2.W)
  val vm = Bool()
}

class ExtInfo extends Bundle {
  val isVf2 = Bool()
  val isVf4 = Bool()
  val isVf8 = Bool()
}

class VIAluFixPointInput(xlen: Int) extends Bundle {
  val valid = Bool()
  val opcode = new VIAluOpcode
  val info = new VIAluInfo
  val vs2 = UInt(xlen.W)
  val vs1 = UInt(xlen.W)
  val widenVs2 = Bool()
  val widen = Bool()
  val isSigned = Bool()
  val isExt = ValidIO(new ExtInfo)
  val isMisc = Bool()
  val mask = UInt(8.W)
  val isAddCarry = Bool()
  val isNarrow = Bool()
}

class VIAluFixPointOutput(xlen: Int) extends Bundle {
  val vd = UInt(xlen.W)
  val narrowVd = UInt((xlen/2).W)
  val addCarryCmpMask = UInt(8.W)
  val vxsat = UInt(8.W)
}

class VIAluFixPoint(xlen: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(new VIAluFixPointInput(xlen))
    val out = Output(new VIAluFixPointOutput(xlen))
  })
  private val valid = io.in.valid
  private val opcode = io.in.opcode
  private val vsew = io.in.info.vsew
  private val vm = io.in.info.vm
  private val vs2 = io.in.vs2
  private val vs1 = io.in.vs1
  private val widenVs2 = io.in.widenVs2
  private val widen = io.in.widen
  private val isSigned = io.in.isSigned
  private val isExt = io.in.isExt
  private val isMisc = io.in.isMisc
  private val mask = io.in.mask
  private val isAddCarry = io.in.isAddCarry
  private val isNarrow = io.in.isNarrow

  val vIAluAdder = Module(new VIAluAdder(xlen))
  vIAluAdder.io.in.opcode := opcode
  vIAluAdder.io.in.vsew := vsew
  vIAluAdder.io.in.vm := vm
  vIAluAdder.io.in.vs2 := vs2
  vIAluAdder.io.in.vs1 := vs1
  vIAluAdder.io.in.widenVs2 := widenVs2
  vIAluAdder.io.in.widen := widen
  vIAluAdder.io.in.isSigned := isSigned
  vIAluAdder.io.in.mask := mask
  vIAluAdder.io.in.isAddCarry := isAddCarry


  val vIAluMisc = Module(new VIAluMisc(xlen))
  vIAluMisc.io.in.opcode := opcode
  vIAluMisc.io.in.vsew := vsew
  vIAluMisc.io.in.vs2 := vs2
  vIAluMisc.io.in.vs1 := vs1
  vIAluMisc.io.in.isSigned := isSigned
  vIAluMisc.io.in.isExt := isExt
  vIAluMisc.io.in.isNarrow := isNarrow

  private val vdAdderS1 = Wire(UInt(xlen.W))
  private val vdMiscS1 = Wire(UInt(xlen.W))
  private val isMiscS1 = Wire(Bool())
  private val addCarryCmpMaskS1 = Wire(UInt(8.W))
  private val narrowVdS1 = Wire(UInt((xlen/2).W))
  private val satS1 = Wire(UInt(8.W))

  vdAdderS1 := RegEnable(vIAluAdder.io.out.vd, valid)
  vdMiscS1 := RegEnable(vIAluMisc.io.out.vd, valid)
  isMiscS1 := GatedValidRegNext(isMisc)
  addCarryCmpMaskS1 := RegEnable(vIAluAdder.io.out.addCarryCmpMask, valid)
  narrowVdS1 := RegEnable(vIAluMisc.io.out.narrowVd, valid)
  satS1 := RegEnable(vIAluAdder.io.out.vxsat, valid)

  io.out.vd := Mux(isMiscS1, vdMiscS1, vdAdderS1)
  io.out.narrowVd := narrowVdS1
  io.out.addCarryCmpMask := addCarryCmpMaskS1
  io.out.vxsat := satS1
}
