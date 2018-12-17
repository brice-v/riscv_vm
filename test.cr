module CPU
  extend self
  
    @@reg = [] of UInt32
    @@pc = 0x0000_u32

    def reg
      @@reg = [0x0000_u32] * 32
    end
 
    def pc
      @@pc
    end

    def addi(imm_val, rs1)
      bit_12 = 0b0000000000001000000000000
      if (imm_val & bit_12) == bit_12 
        @@reg[rs1] = (imm_val & 0x1fff) - @@reg[rs1]
        @@pc += 4_u32
      else
        @@reg[rs1] = (imm_val & 0x1fff) + @@reg[rs1]
        @@pc += 4_u32
      end
    end
  
end

def printpc
  puts CPU.pc
end

#instantiate regs
r = CPU.reg
printpc

# testing
CPU.addi(0x80_u32, 1)
CPU.addi(0x80_u32, 1)

# display regs
puts r
# display program counter
printpc
