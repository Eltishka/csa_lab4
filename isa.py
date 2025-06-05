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
    BE: int = 18
    BNE: int = 19
    BGE: int = 20
    BL: int = 21
    BLE: int = 22
    BG: int = 23
    BVS: int = 24
    BVC: int = 25
    BNS: int = 26
    BNC: int = 27
    JMP: int = 28
    STORE_IMM: int = 29
    STORE: int = 48

class OperandType(Enum):
    REG2REG: int = 0
    INDIRECT_RIGHT: int = 1
    PC_OFFSET: int = 2
    IMMEDIATE: int = 3
