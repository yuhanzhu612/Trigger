import chisel3._
import chisel3.util._
import Instructions._
import Constant._

class Execution extends Module {
  val io = IO(new Bundle {
    val in    = Input(new BUS_R)
    val out   = Output(new BUS_R)
    val busy  = Output(Bool())

    val csr_raddr = Output(UInt(12.W))
    val csr_rdata = Input(UInt(64.W))
    
    val dmem  = new CoreData

    val cmp_ren    = Output(Bool())
    val cmp_wen    = Output(Bool())
    val cmp_addr   = Output(UInt(64.W))
    val cmp_wdata  = Output(UInt(64.W))
    val cmp_rdata  = Input(UInt(64.W))

    val ex_wdest  = Output(UInt(5.W))
    val ex_result = Output(UInt(64.W))

  })

  val ex_valid    = io.in.valid
  val ex_pc       = io.in.pc
  val ex_inst     = io.in.inst
  val ex_wen      = io.in.wen 
  val ex_wdest    = io.in.wdest
  val ex_op1      = io.in.op1
  val ex_op2      = io.in.op2
  val ex_typew    = io.in.typew
  val ex_wmem     = io.in.wmem
  val ex_opcode   = io.in.opcode
  val ex_aluop    = io.in.aluop
  val ex_loadop   = io.in.loadop
  val ex_storeop  = io.in.storeop
  val ex_sysop    = io.in.sysop

  //alu
  val in1 = Mux(ex_typew, Mux(ex_aluop === s"b$ALU_SRA".U, Cat(Fill(32, ex_op1(31)), ex_op1(31, 0)), Cat(Fill(32, 0.U), ex_op1(31, 0))), ex_op1)
  val in2 = ex_op2
  val shamt = Wire(UInt(6.W))
  shamt := Mux(ex_typew, in2(4, 0).asUInt(), in2(5, 0))
  val alu_result_0, alu_result = Wire(UInt(64.W))
  alu_result_0 := MuxLookup(ex_aluop, 0.U, Array(
    s"b$ALU_ADD".U  -> (in1 + in2).asUInt(),
    s"b$ALU_SUB".U  -> (in1 - in2).asUInt(),
    s"b$ALU_SLT".U  -> (in1.asSInt() < in2.asSInt()).asUInt(),
    s"b$ALU_SLTU".U -> (in1 < in2).asUInt(),
    s"b$ALU_XOR".U  -> (in1 ^ in2).asUInt(),
    s"b$ALU_OR".U   -> (in1 | in2).asUInt(),
    s"b$ALU_AND".U  -> (in1 & in2).asUInt(),
    s"b$ALU_SLL".U  -> ((in1 << shamt)(63, 0)).asUInt(),
    s"b$ALU_SRL".U  -> (in1 >> shamt).asUInt(),
    s"b$ALU_SRA".U  -> (in1.asSInt() >> shamt).asUInt()
  ))
  alu_result := Mux(ex_typew, Cat(Fill(32, alu_result_0(31)), alu_result_0(31, 0)), alu_result_0)

  //clint
  val cmp_ren   = ex_loadop =/= 0.U  && (io.dmem.data_addr === CLINT_MTIMECMP || io.dmem.data_addr === CLINT_MTIME)
  val cmp_wen   = ex_storeop =/= 0.U && (io.dmem.data_addr === CLINT_MTIMECMP || io.dmem.data_addr === CLINT_MTIME)
  val cmp_addr  = io.dmem.data_addr
  val cmp_wdata = io.dmem.data_write
  io.cmp_ren    := cmp_ren
  io.cmp_wen    := cmp_wen
  io.cmp_addr   := cmp_addr
  io.cmp_wdata  := cmp_wdata
  val cmp_rdata  = io.cmp_rdata

  //csr
  io.csr_raddr    := ex_inst(31, 20)

  //mem
  val load_en     = ex_loadop =/= 0.U
  val store_en    = ex_storeop =/= 0.U
  val data_valid  = (load_en || store_en) && (!cmp_ren && !cmp_wen)
  val data_req    = store_en && !cmp_wen
  val data_addr   = alu_result(31, 0)
  val data_read   = Mux(cmp_ren, cmp_rdata, io.dmem.data_read)
  val data_ready  = io.dmem.data_ready

  val mem_wdata_lb  = Cat(Fill(56,data_read( 7)), data_read( 7, 0))
  val mem_wdata_lh  = Cat(Fill(48,data_read(15)), data_read(15, 0))
  val mem_wdata_lw  = Cat(Fill(32,data_read(31)), data_read(31, 0))
  val mem_wdata_ld  = data_read
  val mem_wdata_lbu = Cat(Fill(56, 0.U), data_read( 7, 0))
  val mem_wdata_lhu = Cat(Fill(48, 0.U), data_read(15, 0))
  val mem_wdata_lwu = Cat(Fill(32, 0.U), data_read(31, 0))
  val mem_wdata     = (Fill(64, ex_inst === LB ) & mem_wdata_lb ) | 
                      (Fill(64, ex_inst === LH ) & mem_wdata_lh ) |  
                      (Fill(64, ex_inst === LW ) & mem_wdata_lw ) |  
                      (Fill(64, ex_inst === LD ) & mem_wdata_ld ) |  
                      (Fill(64, ex_inst === LBU) & mem_wdata_lbu) |  
                      (Fill(64, ex_inst === LHU) & mem_wdata_lhu) |  
                      (Fill(64, ex_inst === LWU) & mem_wdata_lwu)

  //access memory
  val data_write_sb = MuxLookup(alu_result(2,0), 0.U, Array(
    "b000".U -> Cat(Fill(56, 0.U), ex_wmem(7,0)               ),
    "b001".U -> Cat(Fill(48, 0.U), ex_wmem(7,0), Fill( 8, 0.U)),
    "b010".U -> Cat(Fill(40, 0.U), ex_wmem(7,0), Fill(16, 0.U)),
    "b011".U -> Cat(Fill(32, 0.U), ex_wmem(7,0), Fill(24, 0.U)),
    "b100".U -> Cat(Fill(24, 0.U), ex_wmem(7,0), Fill(32, 0.U)),
    "b101".U -> Cat(Fill(16, 0.U), ex_wmem(7,0), Fill(40, 0.U)),
    "b110".U -> Cat(Fill( 8, 0.U), ex_wmem(7,0), Fill(48, 0.U)),
    "b111".U -> Cat(               ex_wmem(7,0), Fill(56, 0.U)),
  ))
  val data_strb_sb = MuxLookup(alu_result(2,0), 0.U, Array(
    "b000".U -> "b0000_0001".U,
    "b001".U -> "b0000_0010".U,
    "b010".U -> "b0000_0100".U,
    "b011".U -> "b0000_1000".U,
    "b100".U -> "b0001_0000".U,
    "b101".U -> "b0010_0000".U,
    "b110".U -> "b0100_0000".U,
    "b111".U -> "b1000_0000".U,
  ))

  val data_write_sh = MuxLookup(alu_result(2,1), 0.U, Array(
    "b00".U -> Cat(Fill(48, 0.U), ex_wmem(15,0)               ),
    "b01".U -> Cat(Fill(32, 0.U), ex_wmem(15,0), Fill(16, 0.U)),
    "b10".U -> Cat(Fill(16, 0.U), ex_wmem(15,0), Fill(32, 0.U)),
    "b11".U -> Cat(               ex_wmem(15,0), Fill(48, 0.U)),
  ))
  val data_strb_sh = MuxLookup(alu_result(2,1), 0.U, Array(
    "b00".U -> "b0000_0011".U,
    "b01".U -> "b0000_1100".U,
    "b10".U -> "b0011_0000".U,
    "b11".U -> "b1100_0000".U,
  ))

  val data_write_sw = MuxLookup(alu_result(2), 0.U, Array(
    "b0".U -> Cat(Fill(32, 0.U), ex_wmem(32,0)),
    "b1".U -> Cat(ex_wmem(32,0), Fill(32, 0.U)),
  ))
  val data_strb_sw = MuxLookup(alu_result(2), 0.U, Array(
    "b0".U -> "b0000_1111".U,
    "b1".U -> "b1111_0000".U,
  ))
  val data_strb_sd  = "b1111_1111".U
  val data_write_sd = ex_wmem
  
  val data_write  = (Fill(64, ex_inst === SD) & data_write_sd) | 
                    (Fill(64, ex_inst === SW) & data_write_sw) |  
                    (Fill(64, ex_inst === SH) & data_write_sh) |  
                    (Fill(64, ex_inst === SB) & data_write_sb)
  val data_size   = (Fill( 2, ex_inst === SD || ex_inst === LD)                    & SIZE_D) | 
                    (Fill( 2, ex_inst === SW || ex_inst === LW || ex_inst === LWU) & SIZE_W) |  
                    (Fill( 2, ex_inst === SH || ex_inst === LH || ex_inst === LHU) & SIZE_H) |  
                    (Fill( 2, ex_inst === SB || ex_inst === LB || ex_inst === LBU) & SIZE_B)
  val data_strb   = (Fill( 8, ex_inst === SD) & data_strb_sd) | 
                    (Fill( 8, ex_inst === SW) & data_strb_sw) |  
                    (Fill( 8, ex_inst === SH) & data_strb_sh) |  
                    (Fill( 8, ex_inst === SB) & data_strb_sb)

  val ex_wdata    = Mux(load_en, mem_wdata, Mux(ex_sysop =/= 0.U, io.csr_rdata, alu_result))

  val is_mem    = data_valid
  val load_data = RegInit(UInt(64.W), 0.U)
  val resp_success = io.dmem.data_ready

  val s_idle :: s_wait :: s_complete :: Nil = Enum(3)
  val state = RegInit(s_idle)

  val reg_busR  = RegInit(0.U.asTypeOf(new BUS_R))

  switch (state) {
    is (s_idle) {
      when (is_mem && ex_valid) {
        state     := s_wait
      }
    }
    is (s_wait) {
      when (resp_success) {
        load_data := io.dmem.data_read
        state := s_complete
      }
    }
    is (s_complete) {
      state := s_idle
    }
  }

  io.dmem.data_valid  := state === s_idle && is_mem && ex_valid
  io.dmem.data_req    := data_req
  io.dmem.data_addr   := data_addr
  io.dmem.data_write  := data_write
  io.dmem.data_size   := data_size
  io.dmem.data_strb   := data_strb

  val busy = (((state === s_idle) && is_mem) || (state === s_wait)) && ex_valid
  io.busy := busy

  //Next
  io.out.valid    := ex_valid
  io.out.pc       := ex_pc
  io.out.inst     := ex_inst
  io.out.wen      := ex_wen
  io.out.wdest    := ex_wdest
  io.out.wdata    := ex_wdata
  io.out.op1      := ex_op1
  io.out.op2      := ex_op2
  io.out.typew    := ex_typew
  io.out.wmem     := ex_wmem
  io.out.mem_addr := io.dmem.data_addr
  io.out.opcode   := ex_opcode
  io.out.aluop    := ex_aluop
  io.out.loadop   := ex_loadop
  io.out.storeop  := ex_storeop
  io.out.sysop    := ex_sysop
  io.out.intr     := io.in.intr
  io.out.bp_taken   := 0.U
  io.out.bp_targer  := 0.U

  io.ex_wdest  := Mux(ex_valid, io.out.wdest, 0.U)
  io.ex_result := io.out.wdata

}
