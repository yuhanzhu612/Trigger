import chisel3._
import chisel3.util._

trait Constant {
  val ALU_X     = "??????????"
  val ALU_ADD   = "0000000001"
  val ALU_SUB   = "0000000010"
  val ALU_SLT   = "0000000100"
  val ALU_SLTU  = "0000001000"
  val ALU_XOR   = "0000010000"
  val ALU_OR    = "0000100000"
  val ALU_AND   = "0001000000"
  val ALU_SLL   = "0010000000"
  val ALU_SRL   = "0100000000"
  val ALU_SRA   = "1000000000"

  val SIZE_B  = "b00".U
  val SIZE_H  = "b01".U
  val SIZE_W  = "b10".U
  val SIZE_D  = "b11".U

  val SYS_X      = "????????"
  val SYS_CSRRW  = "00000001"
  val SYS_CSRRS  = "00000010"
  val SYS_CSRRC  = "00000100"
  val SYS_CSRRSI = "00001000"
  val SYS_CSRRCI = "00010000"
  val SYS_ECALL  = "00100000"
  val SYS_MRET   = "01000000"
  val SYS_FENCE  = "10000000"

  val LOAD_X   = "???????"
  val LOAD_LB  = "0000001"
  val LOAD_LBU = "0000010"
  val LOAD_LH  = "0000100"
  val LOAD_LHU = "0001000"
  val LOAD_LW  = "0010000"
  val LOAD_LWU = "0100000"
  val LOAD_LD  = "1000000"

  val STORE_X   = "????"
  val STORE_SB  = "0001"
  val STORE_SH  = "0010"
  val STORE_SW  = "0100"
  val STORE_SD  = "1000"

  val TYPE_X    = "??????"
  val TYPE_U    = "100000"
  val TYPE_I    = "010000"
  val TYPE_R    = "001000"
  val TYPE_S    = "000100"
  val TYPE_J    = "000010"
  val TYPE_B    = "000001"

  val REDIRECT_X      = "??????????"
  val REDIRECT_BEQ    = "1000000000"
  val REDIRECT_BNE    = "0100000000"
  val REDIRECT_BLT    = "0010000000"
  val REDIRECT_BGE    = "0001000000"
  val REDIRECT_BLTU   = "0000100000"
  val REDIRECT_BGEU   = "0000010000"
  val REDIRECT_JAL    = "0000001000"
  val REDIRECT_JALR   = "0000000100"
  val REDIRECT_ECALL  = "0000000010"
  val REDIRECT_MRET   = "0000000001"

  val REQ_READ  = 0.U
  val REQ_WRITE = 1.U

  val RW_DATA_WIDTH = 128

  val CLINT_MTIMECMP  = "h0000000002004000".U
  val CLINT_MTIME     = "h000000000200bff8".U

  val EnableDifftest  = true.B
  val EnableCSR       = true.B
  val EnableIcache    = true.B
  val EnableDcache    = true.B
}

object Csrs {
  val mhartid  = "hf14".U
  val mstatus  = "h300".U
  val mie      = "h304".U
  val mtvec    = "h305".U
  val mscratch = "h340".U
  val mepc     = "h341".U
  val mcause   = "h342".U
  val mip      = "h344".U
  val mcycle   = "hb00".U
  val minstret = "hb02".U
}

object Constant extends Constant { }
