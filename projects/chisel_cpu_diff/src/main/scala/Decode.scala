import chisel3._
import chisel3.util._
import Instructions._
import Constant._

class Decode extends Module {
  val io = IO(new Bundle {
    val rs1_addr  = Output(UInt(5.W))
    val rs2_addr  = Output(UInt(5.W))
    val rs1_data  = Input(UInt(64.W))
    val rs2_data  = Input(UInt(64.W))

    val in  = Input(new BUS_R)
    val out = Output(new BUS_R)

    val jmp_packet  = Output(new JmpPacket)

    val stall       = Input(Bool())

    val mtvec       = Input(UInt(64.W))
    val mepc        = Input(UInt(64.W))

    val time_int    = Input(Bool())

    val ex_wdest    = Input(UInt(5.W))
    val wb_wdest    = Input(UInt(5.W))
    val ex_result   = Input(UInt(64.W))
    val wb_result   = Input(UInt(64.W))

  })

  val id_valid  = io.in.valid
  val id_pc     = io.in.pc
  val id_inst   = io.in.inst
  
  val inst = id_inst

  //I-TYPE 27
  val addi    = inst === ADDI
  val andi    = inst === ANDI
  val xori    = inst === XORI
  val ori     = inst === ORI
  val slli    = inst === SLLI
  val srli    = inst === SRLI
  val srai    = inst === SRAI
  val slti    = inst === SLTI
  val sltiu   = inst === SLTIU
  val addiw   = inst === ADDIW
  val slliw   = inst === SLLIW
  val srliw   = inst === SRLIW
  val sraiw   = inst === SRAIW
  val jalr    = inst === JALR
  val lb      = inst === LB
  val lh      = inst === LH
  val lw      = inst === LW
  val ld      = inst === LD
  val lbu     = inst === LBU
  val lhu     = inst === LHU
  val lwu     = inst === LWU
  val csrrw   = inst === CSRRW
  val csrrs   = inst === CSRRS
  val ecall   = inst === ECALL
  val csrrc   = inst === CSRRC
  val csrrsi  = inst === CSRRSI
  val csrrci  = inst === CSRRCI
  val typeI   = addi   || andi   || xori   || ori   || slli  || srli  ||
                srai   || slti   || sltiu  || addiw || slliw || srliw ||
                sraiw  || lb     || lh     || lw    || ld    || lbu   || 
                lhu    || lwu    || csrrw  || csrrs || ecall || csrrc || 
                csrrsi || csrrci
  //U-TYPE 2
  val auipc   = inst === AUIPC
  val lui     = inst === LUI
  val typeU  = auipc || lui
  //J-TYPE 1
  val jal     = inst === JAL
  val typeJ   = jal || jalr
  //R-TYPE 16
  val add     = inst === ADD
  val sub     = inst === SUB
  val sll     = inst === SLL
  val slt     = inst === SLT
  val sltu    = inst === SLTU
  val xor     = inst === XOR
  val srl     = inst === SRL
  val sra     = inst === SRA
  val or      = inst === OR
  val and     = inst === AND
  val addw    = inst === ADDW
  val subw    = inst === SUBW
  val sllw    = inst === SLLW
  val srlw    = inst === SRLW
  val sraw    = inst === SRAW
  val mret    = inst === MRET
  val typeR   = add  || sub  || sll  || slt  || sltu || 
                xor  || srl  || sra  || or   || and  || 
                addw || subw || sllw || srlw || sraw || 
                mret
  //B-TYPE 6
  val beq     = inst === BEQ
  val bne     = inst === BNE
  val blt     = inst === BLT
  val bge     = inst === BGE
  val bltu    = inst === BLTU
  val bgeu    = inst === BGEU
  val typeB   = beq  || bne  || blt  ||
                bge  || bltu || bgeu
  //S-TYPE 4
  val sb      = inst === SB
  val sh      = inst === SH
  val sw      = inst === SW
  val sd      = inst === SD
  val typeS   = sb || sh ||
                sw || sd
  //MY-TYPE 1
  val my_inst = inst === MY_INST

  val imm_i = Cat(Fill(52, inst(31)), inst(31, 20))
  val imm_u = Cat(Fill(32, inst(31)), inst(31, 12), Fill(12, 0.U))
  val imm_j = Cat(Fill(43, inst(31)), inst(31), inst(19, 12), inst(20), inst(30, 21), 0.U)
  val imm_b = Cat(Fill(43, inst(31)), inst(31), inst(7), inst(30, 25), inst(11, 8), 0.U)
  val imm_s = Cat(Fill(52, inst(31)), inst(31, 25), inst(11, 7))

  val alu_add   = addi || addiw || jalr || lb  || lbu   || 
                  lh   || lhu   || lw   || lwu || ld    || 
                  sb   || sh    || sw   || sd  || auipc || 
                  lui  || jal   || add  || addw
  val alu_and   = andi || and 
  val alu_sub   = subw || sub
  val alu_slt   = slti || slt
  val alu_sltu  = sltu || sltiu
  val alu_xor   = xori || xor
  val alu_or    = ori  || or
  val alu_sll   = slli || slliw || sll || sllw
  val alu_srl   = srli || srliw || srl || srlw
  val alu_sra   = srai || sraiw || sra || sraw

  val rs1_addr  = Mux(my_inst, "b01010".U, inst(19, 15))
  val rs2_addr  = inst(24, 20)
  val rs1_en    = ~(ecall || auipc || lui || jal)
  val rs2_en    = typeR || typeB || typeS
  val rs1_data  = io.rs1_data
  val rs2_data  = io.rs2_data

  val ex_wdest    = io.ex_wdest
  val ex_result   = io.ex_result
  val wb_wdest    = io.wb_wdest
  val wb_result   = io.wb_result

  val t_int       = io.time_int && id_valid
  
  val rs1_forward = (rs1_addr =/= 0.U) && (rs1_addr === ex_wdest || rs1_addr === wb_wdest) && rs1_en
  val rs2_forward = (rs2_addr =/= 0.U) && (rs2_addr === ex_wdest || rs2_addr === wb_wdest) && rs2_en
  val rs1_value   = Mux(rs1_forward, Mux(rs1_addr === ex_wdest, ex_result, wb_result), rs1_data)
  val rs2_value   = Mux(rs2_forward, Mux(rs2_addr === ex_wdest, ex_result, wb_result), rs2_data)

  val id_wen    = ~(ecall || mret || my_inst || typeS || typeB)
  val id_wdest  = Mux(id_wen, inst(11, 7), 0.U)
  val id_wdata  = 0.U
  val id_opcode = (Fill(TYPE_X.length, typeI) & s"b$TYPE_I".U) | 
                  (Fill(TYPE_X.length, typeU) & s"b$TYPE_U".U) |  
                  (Fill(TYPE_X.length, typeJ) & s"b$TYPE_J".U) |  
                  (Fill(TYPE_X.length, typeR) & s"b$TYPE_R".U) |
                  (Fill(TYPE_X.length, typeB) & s"b$TYPE_B".U) |
                  (Fill(TYPE_X.length, typeS) & s"b$TYPE_S".U)
  val id_aluop  = (Fill(ALU_X.length, alu_add ) & s"b$ALU_ADD".U ) | 
                  (Fill(ALU_X.length, alu_and ) & s"b$ALU_AND".U ) |  
                  (Fill(ALU_X.length, alu_or  ) & s"b$ALU_OR".U  ) |  
                  (Fill(ALU_X.length, alu_sll ) & s"b$ALU_SLL".U ) |
                  (Fill(ALU_X.length, alu_slt ) & s"b$ALU_SLT".U ) |
                  (Fill(ALU_X.length, alu_sltu) & s"b$ALU_SLTU".U) |
                  (Fill(ALU_X.length, alu_sra ) & s"b$ALU_SRA".U ) |
                  (Fill(ALU_X.length, alu_srl ) & s"b$ALU_SRL".U ) |
                  (Fill(ALU_X.length, alu_sub ) & s"b$ALU_SUB".U ) |
                  (Fill(ALU_X.length, alu_xor ) & s"b$ALU_XOR".U )
  val id_loadop = (Fill(LOAD_X.length, lb ) & s"b$LOAD_LB".U ) | 
                  (Fill(LOAD_X.length, lh ) & s"b$LOAD_LH".U ) |  
                  (Fill(LOAD_X.length, lw ) & s"b$LOAD_LW".U ) |  
                  (Fill(LOAD_X.length, ld ) & s"b$LOAD_LD".U ) |
                  (Fill(LOAD_X.length, lbu) & s"b$LOAD_LBU".U) |
                  (Fill(LOAD_X.length, lhu) & s"b$LOAD_LHU".U) |
                  (Fill(LOAD_X.length, lwu) & s"b$LOAD_LWU".U)
  val id_storeop= (Fill(STORE_X.length, sb) & s"b$STORE_SB".U) |  
                  (Fill(STORE_X.length, sh) & s"b$STORE_SH".U) |
                  (Fill(STORE_X.length, sw) & s"b$STORE_SW".U) |
                  (Fill(STORE_X.length, sd) & s"b$STORE_SD".U)
  val id_sysop  = (Fill(SYS_X.length, csrrs ) & s"b$SYS_CSRRS".U ) |
                  (Fill(SYS_X.length, csrrsi) & s"b$SYS_CSRRSI".U) |
                  (Fill(SYS_X.length, csrrc ) & s"b$SYS_CSRRC".U ) |
                  (Fill(SYS_X.length, csrrci) & s"b$SYS_CSRRCI".U) |
                  (Fill(SYS_X.length, csrrw ) & s"b$SYS_CSRRW".U ) |
                  (Fill(SYS_X.length, ecall ) & s"b$SYS_ECALL".U ) |
                  (Fill(SYS_X.length, mret  ) & s"b$SYS_MRET".U  ) |
                  (Fill(SYS_X.length, t_int ) & s"b$SYS_INT".U   )
  val id_op1    = MuxLookup(id_opcode, 0.U, Array(
                    s"b$TYPE_I".U -> rs1_value,
                    s"b$TYPE_U".U -> Mux(id_inst === AUIPC, id_pc, 0.U),
                    s"b$TYPE_J".U -> id_pc,
                    s"b$TYPE_R".U -> rs1_value,
                    s"b$TYPE_B".U -> rs1_value,
                    s"b$TYPE_S".U -> rs1_value,
                  ))
  val id_op2    = MuxLookup(id_opcode, 0.U, Array(
                    s"b$TYPE_I".U -> imm_i,
                    s"b$TYPE_U".U -> imm_u,
                    s"b$TYPE_J".U -> 4.U,
                    s"b$TYPE_R".U -> rs2_value,
                    s"b$TYPE_B".U -> rs2_value,
                    s"b$TYPE_S".U -> imm_s,
                  ))
  val id_typew  = addiw || slliw || srliw || sraiw || addw ||
                  subw  || sllw  || srlw  || sraw

  val id_bp_taken   = io.in.bp_taken
  val id_bp_targer  = io.in.bp_targer                           

  val ctrl = Module(new Ctrl)
  val redirectop = (Fill(REDIRECT_X.length, jal  ) & s"b$REDIRECT_JAL".U  ) |
                   (Fill(REDIRECT_X.length, jalr ) & s"b$REDIRECT_JALR".U ) |
                   (Fill(REDIRECT_X.length, beq  ) & s"b$REDIRECT_BEQ".U  ) |
                   (Fill(REDIRECT_X.length, bne  ) & s"b$REDIRECT_BNE".U  ) |
                   (Fill(REDIRECT_X.length, blt  ) & s"b$REDIRECT_BLT".U  ) |
                   (Fill(REDIRECT_X.length, bge  ) & s"b$REDIRECT_BGE".U  ) |
                   (Fill(REDIRECT_X.length, bltu ) & s"b$REDIRECT_BLTU".U ) |
                   (Fill(REDIRECT_X.length, bgeu ) & s"b$REDIRECT_BGEU".U ) |
                   (Fill(REDIRECT_X.length, ecall) & s"b$REDIRECT_ECALL".U) |
                   (Fill(REDIRECT_X.length, mret ) & s"b$REDIRECT_MRET".U ) |
                   (Fill(REDIRECT_X.length, t_int) & s"b$REDIRECT_INT".U  )
  ctrl.io.redirectop := redirectop
  ctrl.io.pc         := id_pc
  ctrl.io.imm_i      := imm_i
  ctrl.io.imm_j      := imm_j
  ctrl.io.imm_b      := imm_b
  ctrl.io.rs1_value  := rs1_value
  ctrl.io.rs2_value  := rs2_value
  ctrl.io.mtvec      := io.mtvec
  ctrl.io.mepc       := io.mepc

  val br_stall    = io.stall && (rs1_forward || rs2_forward) //branch target not ready

  val jmp_valid   = ctrl.io.jmp
  val jmp_pc      = ctrl.io.target
  val mis_predict = Mux(jmp_valid, (id_bp_taken && (jmp_pc =/= id_bp_targer)) || !id_bp_taken, id_bp_taken) && !br_stall

  io.jmp_packet.valid   := redirectop =/= 0.U
  io.jmp_packet.inst_pc := id_pc
  io.jmp_packet.jmp     := jmp_valid
  io.jmp_packet.jmp_pc  := jmp_pc
  io.jmp_packet.mis     := io.jmp_packet.valid && mis_predict

  io.rs1_addr           := rs1_addr
  io.rs2_addr           := rs2_addr

  //Next
  io.out.valid      := id_valid
  io.out.pc         := id_pc
  io.out.inst       := id_inst
  io.out.wen        := id_wen
  io.out.wdest      := id_wdest
  io.out.wdata      := id_wdata
  io.out.op1        := Mux(my_inst, rs1_value, id_op1)
  io.out.op2        := id_op2
  io.out.typew      := id_typew
  io.out.wmem       := rs2_value
  io.out.mem_addr   := 0.U
  io.out.opcode     := id_opcode
  io.out.aluop      := id_aluop
  io.out.loadop     := id_loadop
  io.out.storeop    := id_storeop
  io.out.sysop      := id_sysop
  io.out.bp_taken   := 0.U
  io.out.bp_targer  := 0.U
  
}
