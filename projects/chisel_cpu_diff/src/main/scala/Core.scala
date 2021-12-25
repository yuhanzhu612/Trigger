import chisel3._
import chisel3.util.experimental._
import difftest._
import Instructions._
import Constant._

class Core extends Module {
  val io = IO(new Bundle {
    val imem = new CoreInst
    val dmem = new CoreData
  })

  val fetch = Module(new InstFetch)
  val reg_if_id = Module(new PipelineReg)
  val decode = Module(new Decode)
  val reg_id_ex = Module(new PipelineReg)
  val execution = Module(new Execution)
  val reg_ex_wb = Module(new PipelineReg)
  val writeback = Module(new WriteBack)
  val rf = Module(new RegFile)
  val csr = Module(new Csr)
  val clint = Module(new Clint)

  val flush = decode.io.jmp_packet.mis
  val stall = execution.io.busy

  fetch.io.imem           <> io.imem
  fetch.io.jmp_packet     <> decode.io.jmp_packet
  fetch.io.stall          := stall

  reg_if_id.io.in         <> fetch.io.out
  reg_if_id.io.stall      := stall
  reg_if_id.io.flush      := false.B

  decode.io.in            <> reg_if_id.io.out
  decode.io.rs1_data      := rf.io.rs1_data
  decode.io.rs2_data      := rf.io.rs2_data
  decode.io.stall         := stall
  decode.io.ex_wdest      := execution.io.ex_wdest
  decode.io.ex_result     := execution.io.ex_result
  decode.io.wb_wdest      := writeback.io.wb_wdest
  decode.io.wb_result     := writeback.io.wb_result
  decode.io.mtvec         := csr.io.mtvec
  decode.io.mepc          := csr.io.mepc
  decode.io.time_int      := clint.io.time_int

  reg_id_ex.io.in         <> decode.io.out
  reg_id_ex.io.stall      := stall
  reg_id_ex.io.flush      := false.B

  execution.io.dmem       <> io.dmem
  execution.io.in         := reg_id_ex.io.out
  execution.io.csr_rdata  := csr.io.csr_rdata
  execution.io.cmp_rdata  := clint.io.cmp_rdata

  reg_ex_wb.io.in         <> execution.io.out
  reg_ex_wb.io.stall      := stall
  reg_ex_wb.io.flush      := false.B

  writeback.io.in         := reg_ex_wb.io.out

  rf.io.rs1_addr          := decode.io.rs1_addr
  rf.io.rs2_addr          := decode.io.rs2_addr
  rf.io.wen               := writeback.io.wen
  rf.io.wdest             := writeback.io.wdest
  rf.io.wdata             := writeback.io.wdata

  csr.io.in1              := writeback.io.op1
  csr.io.sysop            := writeback.io.sysop
  csr.io.raddr            := execution.io.csr_raddr
  csr.io.inst             := writeback.io.inst
  csr.io.pc               := writeback.io.pc
  csr.io.exc              := writeback.io.exc
  csr.io.intr             := writeback.io.intr

  clint.io.mstatus        := csr.io.mstatus
  clint.io.mie            := csr.io.mie
  clint.io.exc            := writeback.io.exc
  clint.io.cmp_ren        := execution.io.cmp_ren
  clint.io.cmp_wen        := execution.io.cmp_wen
  clint.io.cmp_addr       := execution.io.cmp_addr
  clint.io.cmp_wdata      := execution.io.cmp_wdata


  /* ----- Difftest ------------------------------ */

  val valid   = writeback.io.ready_cmt && !stall
  val inst_my = writeback.io.inst === MY_INST
  val op1     = writeback.io.op1

  val req_clint = (writeback.io.mem_addr === CLINT_MTIMECMP || writeback.io.mem_addr === CLINT_MTIME) &&
                  (writeback.io.loadop =/= 0.U || writeback.io.storeop =/= 0.U)

  val skip    = inst_my || (writeback.io.inst(31, 20) === Csrs.mcycle && writeback.io.sysop =/= 0.U) || req_clint

  val print_valid = RegInit(false.B)
  val print       = RegInit(0.U(64.W))
  when (valid && inst_my) {
    print_valid := true.B
    print := op1
  } .otherwise {
    print_valid := false.B
  }
  when (print_valid) {
    printf("%c", print)
  }

  val intr = writeback.io.intr
  val intr_no = Mux(intr, writeback.io.intr_no, 0.U)
  val exceptionPC = Mux(intr, writeback.io.pc, 0.U)

  when (EnableDifftest) {
    val dt_ic = Module(new DifftestInstrCommit)
    dt_ic.io.clock    := clock
    dt_ic.io.coreid   := 0.U
    dt_ic.io.index    := 0.U
    dt_ic.io.valid    := RegNext(valid)
    dt_ic.io.pc       := RegNext(writeback.io.pc)
    dt_ic.io.instr    := RegNext(writeback.io.inst)
    dt_ic.io.special  := 0.U
    dt_ic.io.skip     := RegNext(skip)
    dt_ic.io.isRVC    := false.B
    dt_ic.io.scFailed := false.B
    dt_ic.io.wen      := RegNext(writeback.io.wen)
    dt_ic.io.wdata    := RegNext(writeback.io.wdata)
    dt_ic.io.wdest    := RegNext(writeback.io.wdest)

    val dt_ae = Module(new DifftestArchEvent)
    dt_ae.io.clock        := clock
    dt_ae.io.coreid       := 0.U
    dt_ae.io.intrNO       := RegNext(intr_no)
    dt_ae.io.cause        := 0.U
    dt_ae.io.exceptionPC  := RegNext(exceptionPC)

    val cycle_cnt = RegInit(0.U(64.W))
    val instr_cnt = RegInit(0.U(64.W))

    cycle_cnt := cycle_cnt + 1.U
    instr_cnt := instr_cnt + valid

    val rf_a0 = WireInit(0.U(64.W))
    BoringUtils.addSink(rf_a0, "rf_a0")

    val dt_te = Module(new DifftestTrapEvent)
    dt_te.io.clock    := clock
    dt_te.io.coreid   := 0.U
    dt_te.io.valid    := (writeback.io.inst === "h0000006b".U)
    dt_te.io.code     := rf_a0(2, 0)
    dt_te.io.pc       := writeback.io.pc
    dt_te.io.cycleCnt := cycle_cnt
    dt_te.io.instrCnt := instr_cnt

    // val dt_cs = Module(new DifftestCSRState)
    // dt_cs.io.clock          := clock
    // dt_cs.io.coreid         := 0.U
    // dt_cs.io.priviledgeMode := 3.U  // Machine mode
    // dt_cs.io.mstatus        := 0.U
    // dt_cs.io.sstatus        := 0.U
    // dt_cs.io.mepc           := 0.U
    // dt_cs.io.sepc           := 0.U
    // dt_cs.io.mtval          := 0.U
    // dt_cs.io.stval          := 0.U
    // dt_cs.io.mtvec          := 0.U
    // dt_cs.io.stvec          := 0.U
    // dt_cs.io.mcause         := 0.U
    // dt_cs.io.scause         := 0.U
    // dt_cs.io.satp           := 0.U
    // dt_cs.io.mip            := 0.U
    // dt_cs.io.mie            := 0.U
    // dt_cs.io.mscratch       := 0.U
    // dt_cs.io.sscratch       := 0.U
    // dt_cs.io.mideleg        := 0.U
    // dt_cs.io.medeleg        := 0.U
  }
}
