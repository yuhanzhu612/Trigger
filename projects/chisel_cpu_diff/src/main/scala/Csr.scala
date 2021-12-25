import chisel3._
import chisel3.util._
import chisel3.util.experimental._
import difftest._
import Constant._

class Csr extends Module {
  val io = IO(new Bundle {
    val in1       = Input(UInt(64.W))
    val pc        = Input(UInt(32.W))
    val inst      = Input(UInt(32.W))
    val raddr     = Input(UInt(12.W))
    val sysop     = Input(UInt(SYS_X.length.W))
    val exc       = Input(Bool())

    val csr_rdata = Output(UInt(64.W))
    val mstatus   = Output(UInt(64.W))
    val mie       = Output(UInt(64.W))
    val mtvec     = Output(UInt(64.W))
    val mepc      = Output(UInt(64.W))

  })

  val in1 = io.in1
  val in2 = Cat(Fill(59, 0.U), io.inst(19, 15))
  val sysop = io.sysop
  val csr_rw = (sysop === s"b$SYS_CSRRW".U)  ||
               (sysop === s"b$SYS_CSRRS".U)  ||
               (sysop === s"b$SYS_CSRRW".U)  ||
               (sysop === s"b$SYS_CSRRSI".U) ||
               (sysop === s"b$SYS_CSRRCI".U)

  val mhartid   = RegInit(UInt(64.W), 0.U)
  val mstatus   = RegInit(UInt(64.W), "h00001800".U)
  val mie       = RegInit(UInt(64.W), 0.U)
  val mip       = RegInit(UInt(64.W), 0.U)
  val mtvec     = RegInit(UInt(64.W), 0.U)
  val mscratch  = RegInit(UInt(64.W), 0.U)
  val mepc      = RegInit(UInt(64.W), 0.U)
  val mcause    = RegInit(UInt(64.W), 0.U)
  val mcycle    = RegInit(UInt(64.W), 0.U)
  val minstret  = RegInit(UInt(64.W), 0.U)

  // ECALL
  when (sysop === s"b$SYS_ECALL".U && io.exc) {
    mepc := io.pc
    mcause := 11.U
    mstatus := Cat(mstatus(63,13), Fill(2, 1.U), mstatus(10,8), mstatus(3), mstatus(6, 4), 0.U, mstatus(2, 0))
  }

  // MRET
  when (sysop === s"b$SYS_MRET".U && io.exc) {
    mstatus := Cat(mstatus(63,13), Fill(2, 0.U), mstatus(10,8), 1.U, mstatus(6, 4), mstatus(7), mstatus(2, 0))
  }

  //INTERRUPT
  when (sysop === s"b$SYS_INT".U && io.exc) {
    mepc := io.pc
    mcause := "h8000000000000007".U
    mstatus := Cat(mstatus(63,13), Fill(2, 1.U), mstatus(10,8), mstatus(3), mstatus(6, 4), 0.U, mstatus(2, 0))
  }

  // CSR register read/write
  val waddr = io.inst(31, 20)
  val raddr = io.raddr
  val wen   = csr_rw
  val op1   = MuxLookup(waddr, 0.U, Array(
                Csrs.mstatus  -> mstatus,
                Csrs.mcause   -> mcause,
                Csrs.mie      -> mie,
                Csrs.mtvec    -> mtvec,
                Csrs.mscratch -> mscratch,
                Csrs.mepc     -> mepc,
                Csrs.mip      -> mip,
                Csrs.mcycle   -> mcycle,
                Csrs.minstret -> minstret,
              ))

  val rdata = MuxLookup(raddr, 0.U, Array(
                Csrs.mstatus  -> mstatus,
                Csrs.mcause   -> mcause,
                Csrs.mie      -> mie,
                Csrs.mtvec    -> mtvec,
                Csrs.mscratch -> mscratch,
                Csrs.mepc     -> mepc,
                Csrs.mip      -> mip,
                Csrs.mcycle   -> mcycle,
                Csrs.minstret -> minstret,
              ))

  val wdata = MuxLookup(io.sysop, 0.U, Array(
                s"b$SYS_CSRRW".U  -> in1,
                s"b$SYS_CSRRS".U  -> (op1 | in1),
                s"b$SYS_CSRRC".U  -> (op1 & ~in1),
                s"b$SYS_CSRRSI".U -> (op1 | in2),
                s"b$SYS_CSRRCI".U -> (op1 & ~in2),
              ))

  //mcycle (not csr)
  mcycle := mcycle + 1.U

  when(wen) {
    when(waddr === Csrs.mcycle) {
      mcycle := wdata 
    }
    when(waddr === Csrs.mtvec) {
      mtvec := wdata 
    }
    when(waddr === Csrs.mepc) {
      mepc := wdata 
    }
    when(waddr === Csrs.mcause) {
      mcause := wdata 
    }
    when(waddr === Csrs.mstatus) {
      mstatus := Cat((wdata(16) & wdata(15)) | (wdata(14) && wdata(13)), wdata(62, 0))
    }
    when(waddr === Csrs.mie) {
      mie := wdata 
    }
    when(waddr === Csrs.mscratch) {
      mscratch := wdata 
    }
  }

  io.csr_rdata  := rdata
  io.mstatus    := mstatus
  io.mie        := mie
  io.mtvec      := mtvec
  io.mepc       := mepc

  // difftest for CSR state
  val dt_cs = Module(new DifftestCSRState)
  dt_cs.io.clock          := clock
  dt_cs.io.coreid         := 0.U
  dt_cs.io.priviledgeMode := 3.U        // machine mode
  dt_cs.io.mstatus        := mstatus
  dt_cs.io.sstatus        := mstatus & "h80000003000de122".U
  dt_cs.io.mepc           := mepc
  dt_cs.io.sepc           := 0.U
  dt_cs.io.mtval          := 0.U
  dt_cs.io.stval          := 0.U
  dt_cs.io.mtvec          := mtvec
  dt_cs.io.stvec          := 0.U
  dt_cs.io.mcause         := mcause
  dt_cs.io.scause         := 0.U
  dt_cs.io.satp           := 0.U
  dt_cs.io.mip            := 0.U
  dt_cs.io.mie            := mie
  dt_cs.io.mscratch       := mscratch
  dt_cs.io.sscratch       := 0.U
  dt_cs.io.mideleg        := 0.U
  dt_cs.io.medeleg        := 0.U
}
