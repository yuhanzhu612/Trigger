import chisel3._
import chisel3.util._
import Constant._


class BUS_R extends Bundle {
  val pc        = UInt(32.W)
  val inst      = UInt(32.W)
  val wen       = Bool()
  val wdest     = UInt(5.W)
  val wdata     = UInt(64.W)
  val op1       = UInt(64.W)
  val op2       = UInt(64.W)
  val typew     = Bool()
  val wmem      = UInt(64.W)

  val opcode   = UInt(TYPE_X.length.W)
  val aluop    = UInt(ALU_X.length.W)
  val loadop   = UInt(LOAD_X.length.W)
  val storeop  = UInt(STORE_X.length.W)
  val sysop    = UInt(SYS_X.length.W)

  val bp_taken  = Bool()
  val bp_targer = UInt(32.W)

  def flush() : Unit = {
    pc        := 0.U
    inst      := 0.U
    wen       := 0.U
    wdest     := 0.U
    wdata     := 0.U
    op1       := 0.U
    op2       := 0.U
    typew     := 0.U
    wmem      := 0.U
    opcode    := 0.U
    aluop     := 0.U
    loadop    := 0.U
    storeop   := 0.U
    sysop     := 0.U
    bp_taken  := 0.U
    bp_targer := 0.U
  }
}

class PipelineReg extends Module {
  val io = IO(new Bundle {
    val in      = Input(new BUS_R)
    val out     = Output(new BUS_R)
    val flush   = Input(Bool())
    val stall   = Input(Bool())
  })

  val reg = Reg(new BUS_R)

  when (io.flush) {
    reg.flush()
  } .elsewhen (!io.stall) {
    reg := io.in
  }

  io.out := reg
}