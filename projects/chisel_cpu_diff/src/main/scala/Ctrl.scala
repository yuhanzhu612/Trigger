import chisel3._
import chisel3.util._
import Constant._

class Ctrl extends Module {
  val io = IO(new Bundle {
    val redirectop  = Input(UInt(REDIRECT_X.length.W))
    val pc          = Input(UInt(32.W))
    val imm_i       = Input(UInt(32.W))
    val imm_j       = Input(UInt(32.W))
    val imm_b       = Input(UInt(32.W))
    val rs1_value   = Input(UInt(64.W))
    val rs2_value   = Input(UInt(64.W))
    val mtvec       = Input(UInt(64.W))
    val mepc        = Input(UInt(64.W))

    val jmp         = Output(Bool())
    val target      = Output(UInt(64.W))
  })

  val redirectop  = io.redirectop
  val pc          = io.pc
  val imm_i       = io.imm_i
  val imm_j       = io.imm_j
  val imm_b       = io.imm_b
  val rs1_value   = io.rs1_value
  val rs2_value   = io.rs2_value
  val mtvec       = io.mtvec
  val mepc        = io.mepc

  io.jmp   :=   (redirectop === s"b$REDIRECT_ECALL".U && true.B) | 
                (redirectop === s"b$REDIRECT_MRET".U  && true.B) |
                (redirectop === s"b$REDIRECT_JAL".U   && true.B) | 
                (redirectop === s"b$REDIRECT_JALR".U  && true.B) |  
                (redirectop === s"b$REDIRECT_BEQ".U   && rs1_value === rs2_value) |  
                (redirectop === s"b$REDIRECT_BNE".U   && rs1_value =/= rs2_value) |
                (redirectop === s"b$REDIRECT_BLT".U   && rs1_value.asSInt() <  rs2_value.asSInt()) |
                (redirectop === s"b$REDIRECT_BGE".U   && rs1_value.asSInt() >= rs2_value.asSInt()) |
                (redirectop === s"b$REDIRECT_BLTU".U  && rs1_value.asUInt() <  rs2_value.asUInt()) |
                (redirectop === s"b$REDIRECT_BGEU".U  && rs1_value.asUInt() >= rs2_value.asUInt())

  io.target :=   MuxLookup(redirectop, 0.U, Array(
                  s"b$REDIRECT_ECALL".U -> Cat(mtvec(31, 2), Fill(2, 0.U)),
                  s"b$REDIRECT_MRET".U  -> mepc(31, 0),
                  s"b$REDIRECT_JAL".U   -> (pc + imm_j).asUInt(), 
                  s"b$REDIRECT_JALR".U  -> (rs1_value + imm_i).asUInt(),
                  s"b$REDIRECT_BEQ".U   -> (pc + imm_b).asUInt(),
                  s"b$REDIRECT_BNE".U   -> (pc + imm_b).asUInt(),
                  s"b$REDIRECT_BLT".U   -> (pc + imm_b).asUInt(),
                  s"b$REDIRECT_BGE".U   -> (pc + imm_b).asUInt(),
                  s"b$REDIRECT_BLTU".U  -> (pc + imm_b).asUInt(),
                  s"b$REDIRECT_BGEU".U  -> (pc + imm_b).asUInt(),
                ))

}