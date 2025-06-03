from enum import Enum


class OpCode(Enum):
    HALT: int = 0
    ADD: int = 1
    ADC: int = 2
    SUB: int = 3
    MUL: int = 4
    DIV: int = 5
    RMD: int = 6
    AND: int = 7
    OR: int = 8
    XOR: int = 9
    NEG: int = 10
    NOT: int = 11
    SAL: int = 12
    SAR: int = 13
    CMP: int = 14
    MOV: int = 15
    STORE: int = 16
    CALL: int = 17
    RET: int = 18
    BE: int = 19
    BNE: int = 20 #TODO
    BGE: int = 21
    BL: int = 22
    BCS: int = 23
    BCC: int = 24
    BVS: int = 25
    BVC: int = 26
    BNS: int = 27
    BNC: int = 28
    JMP: int = 29
class OperandType(Enum):
    REG2REG: int = 0
    INDIRECT_RIGHT: int = 1
    INDIRECT_LEFT: int = 2
    IMMEDIATE: int = 3

