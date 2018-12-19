module CPU
  extend self
  
    @@reg : Array(UInt32) = [] of UInt32
    @@pc = 0x0000_u32

    def reg
      @@reg
    end
 
    def pc
      @@pc
    end

    # INTEGER IMMEDIATE INSTRUCTIONS

    def addi_i(rd, rs1, imm_val)
      bit_12 = 0b00000000000000000001000000000000 # 0x00001000
      if (imm_val & bit_12) == bit_12 
        @@reg[rd] = (imm_val & 0x1fff) - @@reg[rs1] 
        @@pc += 4_u32
      else
        @@reg[rd] = (imm_val & 0x1fff) + @@reg[rs1]
        @@pc += 4_u32
      end
    end
    
    def slti_i(rd, rs1, imm_val)
    end

    def sltu_i(rd, rs1, imm_val)
    end

    def andi_i(rd, rs1, imm_val)
    end

    def ori_i(rd, rs1, imm_val)
    end

    def xori_i(rd, rs1, imm_val)
    end

    def slli_i(rd, rs1, imm_val)
    end

    def srli_i(rd, rs1, imm_val)
    end

    def srai_i(rd, rs1, imm_val)
    end

    def lui_i(rd, imm_val)
    end

    def auipc_i(rd, imm_val)
    end

    # INTEGER REGISTER INSTRUCTIONS

    def add_r(rd, rs1, rs2)
    end

    def slt_r(rd, rs1, rs2)
    end

    def sltu_r(rd, rs1, rs2)
    end

    def and_r(rd, rs1, rs2)
    end

    def or_r(rd, rs1, rs2)
    end

    def sll_r(rd, rs1, rs2)
    end

    def srl_r(rd, rs1, rs2)
    end

    def sub_r(rd, rs1, rs2)
    end

    def sra_r(rd, rs1, rs2)
    end

    # Nop is the equivalent of addi with 0 as all parameters
    def nop()
      @@pc += 4_u32
    end

    # CONTROL TRANSFER INSTRUCTIONS

    def jal_c()
    end

    def jalr_c()
    end

    def beq_c()
    end

    def bne_c()
    end

    def blt_c()
    end

    def bltu_c()
    end

    def bge_c()
    end

    def bgeu_c()
    end

    # LOAD AND STORE

    def load()
    end

    def store()
    end

    # Will need to look more into memory model, control and status registers, and ecall and ebreak
  
end

def printpc
  puts CPU.pc
end

#instantiate regs
r = CPU.reg
printpc

# testing
CPU.addi_i(1, 1, 0x01_u32)
CPU.addi_i(1, 1, 0xffff_u32)

# display regs
puts r
# display program counter
printpc
