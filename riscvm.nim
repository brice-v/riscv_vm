var reg: array[0..31, uint32]

var ram: array[0..1024, uint32]


################################################################
#
## ##### ##### ##### IMMEDIATE INSTRUCTIONS ##### ##### ##### ##
#
################################################################

proc addi(imm_val: uint16, rs1: uint8) =
    ##
    ## Adds the sign extended 12 bit immediate to register rs1.
    ## Arithmetic overflow is ignored and the result is the
    ## low 32 bits.
    ## 
    ## --ADDI rs, rs1, 0 is used to implement MV rd, rs1
    ##

proc slti(imm_val: uint16, rs1: uint8, rd: uint8) =
    ## 
    ## Set less than immediate places the value 1 in register rd
    ## if register rs1 is less than the sign extended immediate
    ## when both are treated as signed numbers, else 0 is written
    ## to rd.
    ## 

proc sltiu(imm_val: uint16, rs1: uint8, rd: uint8) =
    ##
    ## Set less than immediate unsigned compares the value as
    ## unsigned numbers.
    ##

proc andi(imm_val: uint16, rs1: uint8, rd: uint8) =
    ##
    ## Bitwise AND on the register rs1 and the 12 bit sign
    ## extended immediate value and place result in rd
    ##

proc ori(imm_val: uint16, rs1: uint8, rd: uint8) =
    ##
    ## Bitwise OR on the register rs1 and the 12 bit sign
    ## extended immediate value and place result in rd
    ##

proc xori(imm_val: uint16, rs1: uint8, rd: uint8) =
    ##
    ## Bitwise XORI on the register rs1 and the 12 bit sign
    ## extended immediate value and place result in rd
    ## 
    ## --XORI rd, rs1, -1 performs a bitwise logical inversion
    ## of register rs1 (NOT)
    ##

proc slli(imm_val: uint16, rs1: uint8, rd: uint8) =
    ##
    ## SLLI is a logical shift left (zeros are shifted into the
    ## lower bits)
    ##

proc srli(imm_val: uint16, rs1: uint8, rd: uint8) =
    ##
    ## SRLI is a logical right shift (zeros are shifted into the
    ## upper bits)
    ##

proc srai(imm_val: uint16, rs1: uint8, rd: uint8) =
    ##
    ## SRAI is an arithmetic right shift (the original sign bit
    ## is copied into the vacated upperbits)
    ##

proc lui(imm_val: uint16, rd: uint8):
    ##
    ## Load Upper Immediate is used to build 32 bit constants
    ## and uses the U-type format. LUI places the U-Immediate
    ## value in the top 20 bits of the destination register rd,
    ## filling in the lowest 12 bits with zeros.
    ##

proc auipc():
    ##
    ## Add Upper Immediate to pc is used to build pc relative
    ## address and uses the U-type format. AUIPC forms a 32 bit
    ## offset from the 20 bit U-immediate, filling in the lowest
    ## 12 bits with zeros, adds this offset to the pc, then places
    ## the result in rd
    ##


################################################################
#
## ##### ##### REGISTER TO REGISTER INSTRUCTIONS  ##### ##### ##
#
################################################################



proc add(rs2, rs1, rd) =
    ##
    ## ADD performs addition ----------------------------------
    ## Overflows are ignored and the low 32 bits of results are
    ## written to the destination
    ##


proc slt(rs2, rs1, rd) =
    ##
    ## Set rd to 1 if rs1 < rs2 0 otherwise
    ## Signed
    ##



proc sltu(rs2, rs1, rd) =
    ##
    ## Set rd to 1 if rs1 < rs2 0 otherwise
    ## Unsigned
    ## Note, SLTU rd, x0, rs2 sets rd to 1 if rs2 is not equal to 0
    ## otherwise sets rd to 0 (assembler pseudo-op SNEZ rd, rs)
    ##


proc reg_and(rs2, rs1, rd) =
    reg[rd] = reg[rs2] & reg[rs1]


proc reg_or(rs2, rs1, rd) =
    reg[rd] = reg[rs2] | reg[rs1]


proc reg_xor(rs2, rs1, rd) =
    reg[rd] = reg[rs2] ^ reg[rs1]


proc sll(rs2, rs1, rd) =
    # This is probably the wrong order of rs2 and rs1
    reg[rd] = reg[rs2] << reg[rs1]


proc srl(rs2, rs1, rd) =
    # This is probably the wrong order of rs2 and rs1
    reg[rd] = reg[rs2] >> reg[rs1]


proc sub(rs2, rs1, rd) =
    ##
    ## SUB performs Subtraction -------------------------------
    ## Overflows are ignored and the low 32 bits of results are
    ## written to the destination
    ##

proc sra(rs2, rs1, rd) =
    # reg[rd] = reg[rs2] >> reg[rs1]
    # Has to look something like this but preserve the sign bits^^^


proc nop() =
   ##
   ## No Operation: uses addi 0 with 0 to r0
   ##

################################################################
#
## ##### ##### UNCONDITIONAL JUMP INSTRUCTIONS  ##### ##### ####
#
################################################################

proc jal(offset: uint32, rd: uint8) = 
    ## 
    ## Plain unconditional jumps are encoded as a JAL with rd=x0
    ##
    ## Use a 20bit offset
    ##
    ## see page 16 of riscv-spec-v2.2.pdf


proc jalr(offset: uint32, rs1:uint8, rd:uint8) = 
    ## 
    ## The indirect jump instruction uses I encoding
    ## The target address is obtained by adding the 12 bit signed I 
    ## immediate to the register rs1, then setting the least
    ## significant bit of the result to 0.
    ##
    ## The address of the instruction following the jump pc+4
    ## is written to rd.  reg[x0] can be used as the destination if
    ## the result is not required
    ##
    ## see page 16 of riscv-spec-v2.2.pdf


## ##### ##### CONDITIONAL BRANCH INSTRUCTIONS  ##### ##### ####

##
## All branch instructions use the B-type format
## The 12 bit signed offset encodes in multiples of 2 and is added
## to the current pc to give the target address
##
## The conditional branch range is +- 4KiB
##

proc beq(offset: uint16, rs2: uint8, rs1: uint8) =
    ##
    ## BEQ takes the branch if rs1 and rs2 are equal
    ##

proc bne(offset: uint16, rs2: uint8, rs1: uint8) =
    ##
    ## BNE takes the branch if rs1 and rs2 are not equal
    ##

proc blt(offset: uint16, rs2: uint8, rs1: uint8) =
    ##
    ## BLT takes the branch if rs1 is less than rs2
    ##
    ## Signed comparison
    ##

proc bltu(offset: uint16, rs2: uint8, rs1: uint8) =
    ##
    ## BLTU takes the branch if rs1 is less than rs2
    ##
    ## Unsigned comparison
    ##

proc bge(offset: uint16, rs2: uint8, rs1: uint8) =
    ##
    ## BGE takes the branch if rs1 is greater than or equal
    ## to rs2 using signed comparison
    ##

proc bgeu(offset: uint16, rs2: uint8, rs1: uint8) =
    ##
    ## BGEU takes the branch if rs1 is greater than or equal
    ## to rs2 using unsigned comparison
    ##

##
## NOTE: |BGT, BGTU, BLE, BLEU| can be synthesized by reversing the 
## operands to |BLT, BLTU, BGE, BGEU| respectively
##


################################################################
#
##### ##### ##### LOAD AND STORE INSTRUCTIONS  ##### ##### #####
#
################################################################

## see section 2.6, page 18+ in riscv-spec-v2.2.pdf

## Loads are encoded in the I type format and stores are S type
## The Effective address is obtained by adding register rs1 to
## the sign extended 12 bit offset
##
## Loads; copy a value from memory to register rd
## Stores; copy the value in register rs2 to memory
##
##
## LW instruction loads a 32 bit value from memory into rd
## LH instruction loads a 16 bit value from memory but then
## zero extends to 32 bits before storing in rd.
##
## LB and LBU are defined analogously for 8 bit values.
##
## SW, SH, SB instructions store 32 bit, 16 bit, 8 bit values
## from the low bits of register rs2 to memory

proc load(offset: uint16, base: uint8, width: uint8, dest: uint8) =
    ## rs1 is base
    ## rd is dest
    ## offset is 12 bits

proc store(offset: uint16, src: uint8, base: uint8, width: uint8) =
    ## rs2 is src
    ## rs1 is base
    ## offset is 12 bits

