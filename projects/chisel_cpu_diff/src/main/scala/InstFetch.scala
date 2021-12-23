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

  val stall = io.stall

  val pc_init = "h7ffffffc".U(32.W)
  val pc = RegInit(pc_init)
  val inst = RegInit(0.U(32.W))
  val bp = Module(new BrPredictor)
  val bp_pred_pc = bp.io.pred_pc

  val mis = io.jmp_packet.mis                     // branch mis-predict
  val reg_mis = RegInit(false.B)
  val mis_pc  = RegInit(0.U(32.W))
  when (mis) {
    reg_mis := true.B
    mis_pc  := Mux(io.jmp_packet.jmp, io.jmp_packet.jmp_pc, io.jmp_packet.inst_pc + 4.U)
  } .elsewhen (io.imem.inst_ready && !mis) {
    reg_mis := false.B
    mis_pc  := 0.U
  }

  val npc = Mux(reg_mis, mis_pc, bp_pred_pc)

  io.imem.inst_valid := !stall
  io.imem.inst_req   := false.B
  io.imem.inst_addr  := npc.asUInt()
  io.imem.inst_size  := SIZE_W

  //branch prediction
  bp.io.pc := pc
  bp.io.inst := inst
  bp.io.is_br := (inst === Instructions.JAL) || (inst === Instructions.JALR) ||
                 (inst === Instructions.BEQ) || (inst === Instructions.BNE ) ||
                 (inst === Instructions.BLT) || (inst === Instructions.BLTU) ||
                 (inst === Instructions.BGE) || (inst === Instructions.BGEU);
  bp.io.jmp_packet <> io.jmp_packet

  val pc_update  = io.imem.inst_ready
  when (pc_update && !stall) {
    pc    := npc
    inst  := io.imem.inst_read
  }


  val if_stall = RegInit(false.B)
  val if_valid = RegInit(false.B)
  when (if_valid && stall) {
    if_stall := true.B
  }.elsewhen (!stall) {
    if_stall := false.B
  }
  when (pc_update && !stall) {
    if_valid := true.B
  }.otherwise {
    if_valid := false.B
  }

  val if_pc   = Mux(mis || reg_mis, 0.U, pc)
  val if_inst = Mux(mis || reg_mis, 0.U, inst)

  io.out.valid      := if_valid || if_stall
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
