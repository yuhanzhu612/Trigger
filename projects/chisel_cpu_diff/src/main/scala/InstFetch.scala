import chisel3._
import chisel3.util._
import Constant._

class InstFetch extends Module {
  val io = IO(new Bundle {
    val imem = new CoreInst

    val stall = Input(Bool())
    val out  = Output(new BUS_R)
  })

  val pc_init = "h7ffffffc".U(32.W)
  val pc = RegInit(pc_init)
  val inst = RegInit(0.U(32.W))

  // val bp = Module(new BrPredictor)
  val bp_pred_pc = pc + 4.U
  // bp.io.pc := if_pc
  // bp.io.inst := if_inst
  // bp.io.is_br := (inst === Instructions.JAL) || (inst === Instructions.JALR) ||
  //                (inst === Instructions.BEQ) || (inst === Instructions.BNE)  ||
  //                (inst === Instructions.BLT) || (inst === Instructions.BLTU) ||
  //                (inst === Instructions.BGE) || (inst === Instructions.BGEU)
  // bp.io.jmp_packet <> io.jmp_packet

  val s_reset :: s_init :: s_idle :: s_wait :: s_stall :: Nil = Enum(5)
  val state = RegInit(s_reset)

  when (state === s_reset) {
    state := s_init
  } .elsewhen (state === s_init || state === s_idle) {
    pc := bp_pred_pc
    state := s_wait
  } .elsewhen (state === s_wait) {
    when (io.imem.inst_ready) {
      inst := io.imem.inst_read
      state   := Mux(io.stall, s_stall, s_idle)
    }
  } .otherwise {  // s_stall
    state := Mux(io.stall, s_stall, s_idle)
  }

  val if_pc   = Mux(state === s_idle, pc, 0.U)
  val if_inst = Mux(state === s_idle, inst, 0.U)

  io.imem.inst_valid := (state === s_idle) || (state === s_init) || (state === s_wait)
  io.imem.inst_req   := false.B
  io.imem.inst_addr  := pc
  io.imem.inst_size  := SIZE_W

  io.out.pc       := if_pc
  io.out.inst     := if_inst
  io.out.wen      := false.B
  io.out.wdest    := 0.U
  io.out.wdata    := 0.U
  io.out.op1      := 0.U
  io.out.op2      := 0.U
  io.out.typew    := false.B
  io.out.wmem     := 0.U
  io.out.opcode   := 0.U
  io.out.aluop    := 0.U
  io.out.loadop   := 0.U
  io.out.storeop  := 0.U
  io.out.sysop    := 0.U

}
