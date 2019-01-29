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
