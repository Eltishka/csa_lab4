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
    CALL: int = 16
    RET: int = 17
    BZS: int = 18
    BZC: int = 19
    BG: int = 20
    BL: int = 21
    BCS: int = 22
    BCC: int = 23
    BVS: int = 24
    BNS: int = 25
    BNC: int = 26

class OperandType(Enum):
    REG2REG: int = 0
    INDIRECT: int = 1
    IMMEDIATE: int = 2

