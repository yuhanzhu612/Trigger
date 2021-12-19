import chisel3._
import chisel3.util._
import Constant._

class InstFetch extends Module {
  val io = IO(new Bundle {
    val imem = new CoreInst

    val stall = Input(Bool())
    val out  = Output(new BUS_R)
  })

  val if_pc = RegInit("h80000000".U(32.W))
  val if_inst = RegInit(0.U(32.W))

  // val bp = Module(new BrPredictor)
  // bp.io.pc := if_pc
  // bp.io.inst := if_inst
  // bp.io.is_br := (inst === Instructions.JAL) || (inst === Instructions.JALR) ||
  //                (inst === Instructions.BEQ) || (inst === Instructions.BNE)  ||
  //                (inst === Instructions.BLT) || (inst === Instructions.BLTU) ||
  //                (inst === Instructions.BGE) || (inst === Instructions.BGEU)
  // bp.io.jmp_packet <> io.jmp_packet

  if_pc   := Mux(io.stall, if_pc, if_pc + 4.U)
  if_inst := io.imem.inst_read

  io.imem.inst_valid := true.B
  io.imem.inst_req   := false.B
  io.imem.inst_addr  := if_pc
  io.imem.inst_size  := SIZE_W

  io.out.pc       := if_pc
  io.out.inst     := if_inst
  io.out.wen      := false.B
  io.out.wdest    := 0.U
  io.out.wdata    := 0.U
  io.out.op1      := 0.U
  io.out.op2      := 0.U
  io.out.typew    := false.B
  io.out.opcode   := 0.U
  io.out.aluop    := 0.U
  io.out.loadop   := 0.U
  io.out.storeop  := 0.U
  io.out.sysop    := 0.U

}
