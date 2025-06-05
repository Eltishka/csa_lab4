from enum import Enum


class OpCode(Enum):
    HALT: int = 0
    ADD: int = 1
    SUB: int = 2
    MUL: int = 3
    DIV: int = 4
    RMD: int = 5
    AND: int = 6
    OR: int = 7
    XOR: int = 8
    NOT: int = 9
    CMP: int = 10
    MOV: int = 11
    CALL: int = 12
    RET: int = 13
    BE: int = 14
    BNE: int = 15
    BGE: int = 17
    BL: int = 18
    BLE: int = 19
    BG: int = 20
    JMP: int = 21
    STORE_IMM: int = 22
    STORE: int = 48

class OperandType(Enum):
    REG2REG: int = 0
    INDIRECT_RIGHT: int = 1
    PC_OFFSET: int = 2
    IMMEDIATE: int = 3
