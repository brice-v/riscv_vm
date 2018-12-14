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
      @@reg[rs1] = (imm_val & 0x1fff) + @@reg[rs1]
      @@pc += 4_u32
    end
  
end

def printpc
  puts CPU.pc
end


#instantiate regs
r = CPU.reg
printpc

# testing
CPU.addi(0xff80_u32, 1)
CPU.addi(0xff80_u32, 2)

# display regs
puts r
# display program counter
printpc
