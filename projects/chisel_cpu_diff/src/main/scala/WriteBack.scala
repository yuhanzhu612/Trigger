import chisel3._
import chisel3.util._
import Constant._

class WriteBack extends Module {
  val io  = IO(new Bundle {
  val in  = Input(new BUS_R)

  val pc    = Output(UInt(32.W))
  val inst  = Output(UInt(32.W))
  val op1   = Output(UInt(64.W))
  val sysop = Output(UInt(SYS_X.length.W))

  val wen   = Output(Bool())
  val wdest = Output(UInt(5.W))
  val wdata = Output(UInt(64.W))

  val wb_wdest  = Output(UInt(5.W))
  val wb_result = Output(UInt(64.W))

  val csr_flush = Output(Bool())

  val ready_cmt = Output(Bool())
  })

  // val wb_valid  = RegInit(false.B)
  // when (io.in.valid) {
  //   wb_valid := true.B
  // }.otherwise {
  //   wb_valid := false.B
  // }
  val wb_valid = io.in.valid
  val wb_pc    = io.in.pc
  val wb_inst  = io.in.inst
  val wb_wen   = io.in.wen
  val wb_wdest = io.in.wdest
  val wb_wdata = io.in.wdata
  val wb_op1   = io.in.op1
  val wb_op2   = io.in.op2
  val wb_sysop = io.in.sysop

  // io.intr := wb_sysop === s"b$SYS_INT".U && wb_valid
  // io.intr_no := 7.U

  io.pc         := wb_pc
  io.inst       := wb_inst
  io.op1        := wb_op1
  io.sysop      := wb_sysop

  io.wen        := wb_wen && wb_valid
  io.wdest      := wb_wdest
  io.wdata      := wb_wdata
  io.ready_cmt  := wb_inst =/= 0.U && wb_valid

  io.csr_flush  := (wb_sysop === s"b$SYS_ECALL".U || wb_sysop === s"b$SYS_MRET".U || wb_sysop === s"b$SYS_INT".U)

  io.wb_wdest  := Mux(wb_valid, io.wdest, 0.U)
  io.wb_result := io.wdata

}