# BRICE VADNAIS 2019 MIT LICENSE


# Array of 32, 32bit registers
var reg: array[0..31, uint32]
# Array of 1024, 32bit memory locations
var ram: array[0..1024, uint32]
# Program Counter
var pc = 0

################################################################
#
## ##### ##### ##### IMMEDIATE INSTRUCTIONS ##### ##### ##### ##
#
################################################################

proc add_i(imm_val: uint16, rs1: uint8) =
    ##
    ## Adds the sign extended 12 bit immediate to register rs1.
    ## Arithmetic overflow is ignored and the result is the
    ## low 32 bits.
    ## 
    ## --ADDI rs, rs1, 0 is used to implement MV rd, rs1
    ##
    reg[rs1] += imm_val
    pc += 1

proc slt_i(imm_val: uint16, rs1: uint8, rd: uint8) =
    ## 
    ## Set less than immediate places the value 1 in register rd
    ## if register rs1 is less than the sign extended immediate
    ## when both are treated as signed numbers, else 0 is written
    ## to rd.
    ##
    if reg[rs1] < imm_val: reg[rd] = 1
    else: reg[rd] = 0
    pc += 1

proc sltu_i(imm_val: uint16, rs1: uint8, rd: uint8) =
    ##
    ## Set less than immediate unsigned compares the value as
    ## unsigned numbers.
    ##
    if reg[rs1] < imm_val: reg[rd] = 1
    else: reg[rd] = 0
    pc += 1

proc and_i(imm_val: uint16, rs1: uint8, rd: uint8) =
    ##
    ## Bitwise AND on the register rs1 and the 12 bit sign
    ## extended immediate value and place result in rd
    ##
    reg[rd] = imm_val & reg[rs1]
    pc += 1

proc or_i(imm_val: uint16, rs1: uint8, rd: uint8) =
    ##
    ## Bitwise OR on the register rs1 and the 12 bit sign
    ## extended immediate value and place result in rd
    ##
    reg[rd] = imm_val or reg[rs1]
    pc += 1

proc xor_i(imm_val: uint16, rs1: uint8, rd: uint8) =
    ##
    ## Bitwise XORI on the register rs1 and the 12 bit sign
    ## extended immediate value and place result in rd
    ## 
    ## --XORI rd, rs1, -1 performs a bitwise logical inversion
    ## of register rs1 (NOT)
    ##
    reg[rd] = imm_val xor reg[rs1]
    pc += 1

proc sll_i(imm_val: uint16, rs1: uint8, rd: uint8) =
    ##
    ## SLLI is a logical shift left (zeros are shifted into the
    ## lower bits)
    ##
    reg[rd] = reg[rs1] << imm_val
    pc += 1

proc srl_i(imm_val: uint16, rs1: uint8, rd: uint8) =
    ##
    ## SRLI is a logical right shift (zeros are shifted into the
    ## upper bits)
    ##
    reg[rd] = reg[rs1] >> imm_val
    pc += 1

proc sra_i(imm_val: uint16, rs1: uint8, rd: uint8) =
    ##
    ## SRAI is an arithmetic right shift (the original sign bit
    ## is copied into the vacated upperbits)
    ##
    reg[rd] = reg[rs1] >> imm_val
    pc += 1

proc lu_i(imm_val: uint16, rd: uint8) = 
    ##
    ## Load Upper Immediate is used to build 32 bit constants
    ## and uses the U-type format. LUI places the U-Immediate
    ## value in the top 20 bits of the destination register rd,
    ## filling in the lowest 12 bits with zeros.
    ##
    reg[rd] = imm_val << 12
    pc += 1

proc auipc(imm_val: uint16, rd: uint8) =
    ##
    ## Add Upper Immediate to pc is used to build pc relative
    ## address and uses the U-type format. AUIPC forms a 32 bit
    ## offset from the 20 bit U-immediate, filling in the lowest
    ## 12 bits with zeros, adds this offset to the pc, then places
    ## the result in rd
    ##
    reg[rd] = imm_val << 12
    pc += 1



################################################################
#
## ##### ##### REGISTER TO REGISTER INSTRUCTIONS  ##### ##### ##
#
################################################################



proc add_r(rs2: uint8, rs1 : uint8, rd: uint8) =
    ##
    ## ADD performs addition ----------------------------------
    ## Overflows are ignored and the low 32 bits of results are
    ## written to the destination
    ##
    reg[rd] = reg[rs1] + reg[rs2]
    pc += 1

proc slt_r(rs2: uint8, rs1: uint8, rd : uint8) =
    ##
    ## Set rd to 1 if rs1 < rs2 0 otherwise
    ## Signed
    ##
    if reg[rs1] < reg[rs2]: reg[rd] = 1
    else: reg[rd] = 0
    pc += 1

proc sltu_r(rs2: uint8, rs1: uint8, rd : uint8) =
    ##
    ## Set rd to 1 if rs1 < rs2 0 otherwise
    ## Unsigned
    ## Note, SLTU rd, x0, rs2 sets rd to 1 if rs2 is not equal to 0
    ## otherwise sets rd to 0 (assembler pseudo-op SNEZ rd, rs)
    ##
    if reg[rs1] < reg[rs2]: reg[rd] = 1
    else: reg[rd] = 0
    pc += 1

proc and_r(rs2: uint8, rs1: uint8, rd : uint8) =
    ##
    ## Bitwise AND 2 registers together
    ##
    reg[rd] = reg[rs2] & reg[rs1]
    pc += 1


proc or_r(rs2: uint8, rs1: uint8, rd : uint8) =
    ##
    ## Bitwise OR 2 registers together
    ##
    reg[rd] = reg[rs2] or reg[rs1]
    pc += 1

proc xor_r(rs2: uint8, rs1: uint8, rd : uint8) =
    ##
    ## Bitwise XOR 2 registers together
    ##
    reg[rd] = reg[rs2] xor reg[rs1]
    pc += 1

proc sll_r(rs2: uint8, rs1: uint8, rd : uint8) =
    ##
    ## Bitwise Shift Left the value in rs2 by the
    ## value in rs1
    ##
    # This is probably the wrong order of rs2 and rs1
    reg[rd] = reg[rs2] << reg[rs1]
    pc += 1

proc srl_r(rs2: uint8, rs1: uint8, rd : uint8) =
    ##
    ## Bitwise Shift right the value in rs2 by the
    ## value in rs1
    ##
    # This is probably the wrong order of rs2 and rs1
    reg[rd] = reg[rs2] >> reg[rs1]
    pc += 1

proc sub_r(rs2: uint8, rs1: uint8, rd: uint8) =
    ##
    ## SUB performs Subtraction -------------------------------
    ## Overflows are ignored and the low 32 bits of results are
    ## written to the destination
    ##
    reg[rd] = reg[rs2] - reg[rs1]
    pc += 1

proc sra_r(rs2: uint8, rs1: uint8, rd: uint8) =
    # reg[rd] = reg[rs2] >> reg[rs1]
    # Has to look something like this but preserve the sign bits^^^
    reg[rd] = reg[rs2] >> reg[rs1]
    pc += 1

proc nop() =
   ##
   ## No Operation: uses addi 0 with 0 to r0
   ##
   pc += 1

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


proc jalr(offset: uint32, rs1: uint8, rd: uint8) = 
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


################################################################
#
# #### ##### ##### MEMORY MODEL INSTRUCTIONS  ##### ##### #### #
#
################################################################

## see section 2.7 page 20+ 

## `hart` -> Hardware thread
##
## Each `hart` has its own register state and program counter,
## and executes an independent sequential instruction stream
##
## `hart`s can communicate with other harts via calls to the
## execution environment, or directly via the shared mem system
##
## `hart`s can also interact with I/O devices and indirectly with
## each other via loads and stores to portions of the address space
## assigned to the I/O

proc fence(predecessor: uint8, successor: uint8) =
    ##
    ## FENCE is used to order device I/O and memory accesses
    ## as viewed by other harts and external devices and coprocessors
    ## 
    ## KEY: I -> Input, O -> Ouput, R -> Read, W -> Write
    ##      P -> Predecessor,   S -> Successor
    ##
    ## Informally, no other hart can observe any operation in the
    ## successor set following a FENCE before any operation in the
    ## predecessor set preceeding the FENCE

proc fencei(imm_val: uint16, rs1: uint8, rd: uint8) =
    ##
    ## FENCE.I is used to synchronize the instruction and data streams
    ##
    ## RISC-V does not guarantee that stores to instruction memory will
    ## be made visible to the instruction fetches on the same hart
    ## until a FENCE.I instruction is executed
    ##
    ## FENCE.I only ensures that a subsequent instruction fetch on a hart
    ## will see any previous data stores already visible to the same hart
    ##
    ## To make a store to instruction memory visble to all harts the writing
    ## hart has to execute a data FENCE before requesting that all remote
    ## harts execute a FENCE.I
    ##
    ## NOTE: imm[11:0], rs1, rd are reserved for finer grained control
    ## just zero them for now


################################################################
#
###### ##### ##### ##### SYSTEM INSTRUCTIONS ##### ##### ##### #
#
################################################################

## see section 2.8 page 21+

## SYSTEM instructions are used to access system functionality
## that might require priveledged access
##
## 2 Main Classes: Those that atomically read-modify-write control
## and status registers (CSR) (and all other priveledged instructions)


## ##### ##### CSR INSTRUCTIONS ##### ##### ##

proc csrrw(imm_val: uint16, rs1: uint8, rd: uint8) =
    ##
    ## CSRRW (Atomic Read/Write CSR) atomically swaps
    ## values in the CSRs and integer registers
    ##
    ## CSRRW reads the old value of the CSR, 0 extends the value to 
    ## XLEN bits, then writes it to integer register rd
    ##
    ## The initial value in rs1 is written to CSR
    ##
    ## If rd=x0 then the instruction shall not read the CSR and shall
    ## not cause any of the side effects that might occur on a CSR read

proc csrrs(imm_val: uint16, rs1: uint8, rd: uint8) =
    ##
    ## CSRRS (Atomic Read and Set bits in CSR) reads the value of the
    ## CSR, 0 extends it to XLEN bits and writes it to rd
    ##
    ## The initial value in rs1 is treated as a bit mask that specifies
    ## bit positions to be set in the CSR

proc csrrc(imm_val: uint16, rs1: uint8, rd: uint8) =
    ##
    ## CSRRC (Atomic Read and Clear bits in CSR) reads the value of the 
    ## CSR, 0 extends it to XLEN bits and writes it to rd
    ##
    ## The initial value in rs1 is treated as a bit mask that specifies
    ## the bit positions to be cleared in the CSR

## FOR BOTH CSRRS and CSRRC: if rs1=x0 then the instruction will not write
## to the CSR at all and shall not cause any side effects that might otherwise
## on a CSR write

## CSRRWI, CSRRSI, and CSRRCI: similar to above except that they
## update the CSR using an XLEN bit value obtained by 0 extending
## a 5 bit unsigned immediate field encoded in the rs1 field instead
## of a value from an integer register
##
## FOR CSRRSI and CSRRCI if the uimm field is 0 then these instructions will
## not write to the CSR and shall not cause any of the side effects that might
## otherwise occurs

proc csrrwi(imm_val: uint16, rs1: uint8, rd: uint8) =

proc csrrsi(imm_val: uint16, rs1: uint8, rd: uint8) =

proc csrrci(imm_val: uint16, rs1: uint8, rd: uint8) =


## ##### ##### TIMER AND COUNTERS INSTRUCTIONS ##### ##### ##


## RV32I provides a number of 64-bit read only user level counters which
## are mapped into the 12-bit CSR address space and accessed in 32-bit
## using the CSRRS instructions

## RDCYCLE
## RDCYCLEH
## RDTIME
## RDTIMEH
## RDINSTRET
## RDINSTRETH


## IGNORING THESE FOR NOW ##


## ##### ### ENVIRONMENT CALL AND BREAK POINTS ### ##### ##

proc ecall() =
    ##
    ## ECALL is used to make a request to the supporting execution
    ## environment (usually an os)
    ## 
    ## The ABI for the system will define how parameters for the
    ## environment request and passed, but usually these will be in
    ## defined locations in the integer register file


proc ebreak() =
    ##
    ## EBREAK is used by debuggers to cause control to be transferred
    ## back to a debugging environment


###########################################################
#                                                         #
################# END OF RV32I INSTRUCTIONS ###############
#                                                         #
###########################################################




# mul     rd rs1 rs2 31..25=1 14..12=0 6..2=0x0C 1..0=3

# 1111111XXXXXXXXXX000XXXXX0110011
