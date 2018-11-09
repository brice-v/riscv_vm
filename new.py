# Trying to replicate RV32G in python

# Creating 32 Registers with 0x0000 as their default value
reg = [0] * 32
ram = [0] * 1024
# print(r)

# ###############################
# Helper Functions
# ###############################


def init_r():
    for i in range(1, len(reg)):
        reg[i] = 67


clearmem = init_r()


def print_reg_to_hex(column_or_array):
    """
    Will pretty print the registers in either a column with
    1 given as param, or an array with 2 as a param
    """
    pretty_reg = [format(reg[i], '08X') for i in range(len(reg))]
    if column_or_array == 1:
        for i in range(10):
            print("reg%d:     " % (i), pretty_reg[i])
        for i in range(10, 31):
            print("reg%d:    " % (i), pretty_reg[i])
    elif column_or_array == 2:
        print(pretty_reg)
    else:
        print(pretty_reg)


##############################################################
#   Terminology
##############################################################
#   reg := This is the array of registers 32 total
#   rs1 := Register Source 1
#   rs2 := Register Source 2
#   rd := Register Destination
#   imm_val := Signed/Unsigned Immediate value
##############################################################

# Create Names for all 32 registers
# for i in range(0, len(r)):
#     """
#     Registers are now defined as r0,r1,r2...,r31
#     """
#     exec("r%d = %s" % (i, str(i)))
#     exec("r%d = format(r%d,\'05b\')" % (i, i))


# reg = {}
# for numbers in range(len(r)):
#     exec("reg[%d] = format(%d,\'02X\')" % (numbers, numbers))
#     exec("reg%daddr = reg[%d]" % (numbers, numbers))

###############################################################
#
# IMMEDIATE INSTRUCTIONS
#
###############################################################

def addi(imm_val, rs1):
    """
    Adds the sign extended 12 bit immediate to register rs1.
    Arithmetic overflow is ignored and the result is the
    low 32 bits.

    --ADDI rs, rs1, 0 is used to implement MV rd, rs1
    """
    reg[rs1] = imm_val + int(reg[rs1])


def test_addi():
    clearmem
    addi(100, 1)
    if reg[1] == 167:
        print("test_addi -- Passed")
    else:
        print("test_addi -- Failed")


def slti(imm_val, rs1, rd):
    """
    Set less than immediate places the value 1 in register rd
    if register rs1 is less than the sign extended immediate
    when both are treated as signed numbers, else 0 is written
    to rd.
    """
    if reg[rs1] < imm_val:
        reg[rd] = 1
    else:
        reg[rd] = 0


def test_slti():
    clearmem
    slti(1, 1, 3)
    if reg[3] == 1:
        print("test_slti -- Failed")
    else:
        print("test_slti -- Passed")


def sltiu(imm_val, rs1, rd):
    """
    Set less than immediate unsigned compares the value as
    unsigned numbers.
    """
    if reg[rs1] < imm_val:
        reg[rd] = 1
    else:
        reg[rd] = 0


def andi(imm_val, rs1, rd):
    """
    Bitwise AND on the register rs1 and the 12 bit sign
    extended immediate value and place result in rd
    """
    reg[rd] = imm_val & reg[rs1]


def ori(imm_val, rs1, rd):
    """
    Bitwise OR on the register rs1 and the 12 bit sign
    extended immediate value and place result in rd
    """
    reg[rd] = imm_val | reg[rs1]


def xori(imm_val, rs1, rd):
    """
    Bitwise XORI on the register rs1 and the 12 bit sign
    extended immediate value and place result in rd

    --XORI rd, rs1, -1 performs a bitwise logical inversion
    of register rs1 (NOT)
    """
    reg[rd] = imm_val ^ reg[rs1]


def slli(imm_val, rs1, rd):
    """
    SLLI is a logical shift left (zeros are shifted into the
    lower bits)
    """
    reg[rd] = rs1 << imm_val


def srli(imm_val, rs1, rd):
    """
    SRLI is a logical right shift (zeros are shifted into the
    upper bits)
    """
    reg[rd] = (imm_val % 0x100000000) >> rs1


def srai(imm_val, rs1, rd):
    """
    SRAI is an arithmetic right shift (the original sign bit
    is copied into the vacated upperbits)
    """
    reg[rd] = rs1 >> imm_val


def lui(imm_val, rd):
    """
    Load Upper Immediate is used to build 32 bit constants
    and uses the U-type format. LUI places the U-Immediate
    value in the top 20 bits of the destination register rd,
    filling in the lowest 12 bits with zeros.
    """
    if imm_val < (2 ^ 20):
        result_val = int(bin(imm_val), 2) << int(bin(12), 2)
        reg[rd] = result_val
    else:
        print("ERROR CANNOT USE IMM_VAL > 2^20")


def auipc():
    """
    Add Upper Immediate to pc is used to build pc relative
    address and uses the U-type format. AUIPC forms a 32 bit
    offset from the 20 bit U-immediate, filling in the lowest
    12 bits with zeros, adds this offset to the pc, then places
    the result in rd
    """
    pass

###############################################################
#
# REGISTER TO REGISTER INSTRUCTIONS
#
###############################################################


def add(rs2, rs1, rd):
    """
    ADD performs addition ----------------------------------
    Overflows are ignored and the low 32 bits of results are
    written to the destination
    """
    reg[rd] = reg[rs2] + reg[rs1]


def test_add():
    clearmem
    add(1, 2, 3)
    if reg[3] == 134:
        print("test_add -- Passed")
    else:
        print("test_add -- failed")


def slt(rs2, rs1, rd):
    """
    Set rd to 1 if rs1 < rs2 0 otherwise
    Signed
    """
    reg[rd] = 1 if rs1 < rs2 else 0


def sltu(rs2, rs1, rd):
    """
    Set rd to 1 if rs1 < rs2 0 otherwise
    Unsigned
    Note, SLTU rd, x0, rs2 sets rd to 1 if rs2 is not equal to 0
    otherwise sets rd to 0 (assembler pseudo-op SNEZ rd, rs)
    """
    pass


def reg_and(rs2, rs1, rd):
    pass


def reg_or(rs2, rs1, rd):
    pass


def reg_xor(rs2, rs1, rd):
    pass


def sll(rs2, rs1, rd):
    pass


def srl(rs2, rs1, rd):
    pass


def sub(rs2, rs1, rd):
    """
    SUB performs Subtraction -------------------------------
    Overflows are ignored and the low 32 bits of results are
    written to the destination
    """
    reg[rd] = reg[rs2] - reg[rs1]


def sra(rs2, rs1, rd):
    pass


def nop():
    """
    No Operation: uses addi 0 with 0 to r0
    """
    addi(0, 0)


def run_all_tests():
    test_add()
    test_addi()
    test_slti()
