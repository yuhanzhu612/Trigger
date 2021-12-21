import chisel3._
import chisel3.util._
import Constant._

class InstFetch extends Module {
  val io = IO(new Bundle {
    val imem = new CoreInst

    val jmp_packet = Input(new JmpPacket)

    val stall = Input(Bool())
    val out  = Output(new BUS_R)
  })

  val s_init :: s_idle :: s_req :: s_wait :: Nil = Enum(4)
  val state = RegInit(s_init)

  val stall = io.stall

  val pc_init = "h80000000".U(32.W)
  val pc = RegInit(pc_init)
  val inst = RegInit(0.U(32.W))
  val bp = Module(new BrPredictor)
  val bp_pred_pc = bp.io.pred_pc

  io.imem.inst_valid := (state === s_req ||state === s_wait) && !stall && !io.jmp_packet.mis
  io.imem.inst_req   := false.B
  io.imem.inst_addr  := pc.asUInt()
  io.imem.inst_size  := SIZE_W

  val resp_success = io.imem.inst_ready
  val mis_count = RegInit(0.U(5.W))
  def mis_increment() : Unit = { mis_count := Cat(mis_count(3, 0), 1.U)}
  def mis_decrement() : Unit = { mis_count := Cat(0.U, mis_count(4, 1))}

  switch (state) {
    is (s_init) {
      state := s_req
    }
    is (s_idle) {
      pc := Mux(stall, pc, bp_pred_pc)
      state := Mux(stall, s_idle, s_req)
    }
    is (s_req) {
      when (io.jmp_packet.mis) {
        pc := bp_pred_pc
      } .elsewhen (!stall) {
        state := s_wait
      }
    }
    is (s_wait) {
      when (io.jmp_packet.mis) {
        pc := bp_pred_pc
        mis_increment()
        state := s_req
      } .elsewhen (resp_success) {
        when (mis_count === 0.U) {
          inst := io.imem.inst_read
          state := s_idle
        } .otherwise {
          mis_decrement()
        }
      }
    }
  }

  //branch prediction
  bp.io.pc := pc
  bp.io.inst := inst
  bp.io.is_br := (inst === Instructions.JAL) || (inst === Instructions.JALR) ||
                 (inst === Instructions.BEQ) || (inst === Instructions.BNE ) ||
                 (inst === Instructions.BLT) || (inst === Instructions.BLTU) ||
                 (inst === Instructions.BGE) || (inst === Instructions.BGEU);
  bp.io.jmp_packet <> io.jmp_packet

  val if_pc   = Mux(state === s_idle, pc, 0.U)
  val if_inst = Mux(state === s_idle, inst, 0.U)

  io.out.pc         := if_pc
  io.out.inst       := if_inst
  io.out.wen        := false.B
  io.out.wdest      := 0.U
  io.out.wdata      := 0.U
  io.out.op1        := 0.U
  io.out.op2        := 0.U
  io.out.typew      := false.B
  io.out.wmem       := 0.U
  io.out.opcode     := 0.U
  io.out.aluop      := 0.U
  io.out.loadop     := 0.U
  io.out.storeop    := 0.U
  io.out.sysop      := 0.U
  io.out.bp_taken   := bp.io.pred_br
  io.out.bp_targer  := bp.io.pred_pc

}
