import logging
import sys
from enum import Enum

from isa import OpCode, OperandType


class Signal(Enum):
    TICK: int = 0

    SELECT_LEFT_ALU: int = 1
    SELECT_RIGHT_ALU: int = 2
    EXECUTE_ALU: int = 3
    LATCH_FLAGS: int = 15

    LATCH_REGISTER: int = 5

    CHECK_CONDITION: int = 16
    LATCH_PC: int = 6

    LATCH_mPC: int = 7

    LATCH_AR: int = 8
    LATCH_DR: int = 9

    WRITE: int = 11

    SELECT_REGS: int = 12
    LATCH_IR: int = 13
    HALT: int = 14


class Sel:
    class LeftALU(Enum):
        Register: int = 0
        ALU: int = 1
        ZERO: int = 3
        PC: int = 4

    class RightALU(Enum):
        REGISTER: int = 0
        DR: int = 1
        ZERO: int = 2

    class AR(Enum):
        REGISTER: int = 0
        ALU: int = 1
        PC: int = 2

    class RegisterIn(Enum):
        REGISTER: int = 0
        ALU: int = 1
        PC: int = 2

    class MicroPC(Enum):
        OPCODE: int = 0
        OP_FETCH: int = 1
        ZERO: int = 2
        PLUS_ONE: int = 3

    class PC(Enum):
        REGISTER: int = 0
        ALU: int = 1
        PLUS_TWO: int = 2
        PLUS_FOUR: int = 3
        BRANCH_CONDITION: int = 4
        BRANCH_CONDITION_INVERSE: int = 5

    class DataIn(Enum):
        REGISTER: int = 0
        ALU: int = 1

    class Condition(Enum):
        ZERO: int = 0
        GREATER: int = 1
        GREATER_EQUALS: int = 2
        NONE: int = 3

    class DR(Enum):
        LOW: int = 0
        FULL: int = 1


class RegisterBlock:
    class Register(Enum):
        R0: int = 0
        R1: int = 1
        R2: int = 2
        R3: int = 3
        R4: int = 4
        R5: int = 5
        R6: int = 6
        R7: int = 7
        R8: int = 8
        R9: int = 9
        R10: int = 10
        R11: int = 11
        R12: int = 12
        R13: int = 13
        R14: int = 14
        R15: int = 15

    def __init__(self):
        self.registers = {
            self.Register.R0: 0,
            self.Register.R1: 0,
            self.Register.R2: 0,
            self.Register.R3: 0,
            self.Register.R4: 0,
            self.Register.R5: 0,
            self.Register.R6: 0,
            self.Register.R7: 0,
            self.Register.R8: 0,
            self.Register.R9: 0,
            self.Register.R10: 0,
            self.Register.R11: 0,
            self.Register.R12: 0,
            self.Register.R13: 0,
            self.Register.R14: 0,
            self.Register.R15: 0
        }


class IO:
    def __init__(self, input_arr):
        self.input = input_arr
        self.output = []


class DataPath:
    def __init__(self, io: IO, memory_size: int, input_addr: int, output_addr: int):
        self.alu: ALU = ALU(self)
        self.control_unit: ControlUnit = None

        self.memory = [0] * memory_size
        self.data_register: int = 0
        self.address_register: int = 0

        self.reg_out1: RegisterBlock.Register = None
        self.reg_out2: RegisterBlock.Register = None

        self.register_block: RegisterBlock = RegisterBlock()

        self.input_addr = input_addr
        self.output_addr = output_addr
        self.io = io

    def select_reg_out1(self, register: RegisterBlock.Register):
        self.reg_out1 = register

    def select_reg_out2(self, register: RegisterBlock.Register):
        self.reg_out2 = register

    def select_reg_in(self, register: RegisterBlock.Register):
        self.reg_out1 = register

    def write(self, sel: Sel.DataIn):
        if self.address_register == self.output_addr:
            if sel == Sel.DataIn.REGISTER:
                self.io.output.append(self.register_block.registers[self.reg_out1])
            elif sel == Sel.DataIn.ALU:
                self.io.output.append(self.alu.result)
        else:
            if sel == Sel.DataIn.REGISTER:
                self.memory[self.address_register] = self.register_block.registers[self.reg_out1] >> 24 & 0xFF
                self.memory[self.address_register + 1] = self.register_block.registers[self.reg_out1] >> 16 & 0xFF
                self.memory[self.address_register + 2] = self.register_block.registers[self.reg_out1] >> 8 & 0xFF
                self.memory[self.address_register + 3] = self.register_block.registers[self.reg_out1] & 0xFF
            elif sel == Sel.DataIn.ALU:
                self.memory[self.address_register] = self.alu.result >> 24 & 0xFF
                self.memory[self.address_register + 1] = self.alu.result >> 16 & 0xFF
                self.memory[self.address_register + 2] = self.alu.result >> 8 & 0xFF
                self.memory[self.address_register + 3] = self.alu.result & 0xFF

    def latch_address_register(self, sel: Sel.AR):
        if sel == Sel.AR.PC:
            self.address_register = self.control_unit.program_counter
        if sel == Sel.AR.ALU:
            self.address_register = self.alu.result

    def latch_data_register(self, sel: Sel.DR):

        if self.address_register == self.input_addr:
            if len(self.io.input) < 1:
                raise EOFError

            self.data_register = self.io.input[0]
            self.io.input = self.io.input[1:]
        else:
            self.data_register = (self.memory[self.address_register] << 24) | \
                                 (self.memory[self.address_register + 1] << 16) | \
                                 (self.memory[self.address_register + 2] << 8) | \
                                 self.memory[self.address_register + 3]
        if sel == Sel.DR.LOW:
            mask = 0x0000FFFF
            self.data_register = self.data_register & mask
            if (self.data_register >> 15) == 1:
                self.data_register |= 0xFFFF0000

    def latch_register(self, sel: Sel.RegisterIn):
        if sel == Sel.RegisterIn.REGISTER:
            self.register_block.registers[RegisterBlock.Register(self.reg_out1)] = self.register_block.registers[
                RegisterBlock.Register(self.reg_out2)]
        if sel == Sel.RegisterIn.ALU:
            self.register_block.registers[RegisterBlock.Register(self.reg_out1)] = self.alu.result
        if sel == Sel.RegisterIn.PC:
            self.register_block.registers[RegisterBlock.Register(self.reg_out1)] = self.control_unit.program_counter
        # TODO something will not work


class ALU:
    class Flags(Enum):
        ZERO = 0
        CARRY = 1
        OVERFLOW = 2
        NEGATIVE = 3

    class Operations(Enum):
        ADD: int = 0
        SUB: int = 1
        MUL: int = 2
        DIV: int = 3
        RMD: int = 4
        AND: int = 5
        OR: int = 6
        XOR: int = 7
        NOT: int = 8

    def __init__(self, datapath: DataPath):
        self.datapath = datapath
        self.left: int = 0
        self.right: int = 0
        self.result: int = 0
        self.latched_flags = {
            self.Flags.ZERO: False,
            self.Flags.CARRY: False,
            self.Flags.OVERFLOW: False,
            self.Flags.NEGATIVE: False
        }
        self.temp_flags = {
            self.Flags.ZERO: False,
            self.Flags.CARRY: False,
            self.Flags.OVERFLOW: False,
            self.Flags.NEGATIVE: False
        }

        self.operations = {
            self.Operations.ADD: self.add,
            self.Operations.SUB: self.sub,
            self.Operations.MUL: self.mul,
            self.Operations.DIV: self.div,
            self.Operations.RMD: self.rmd,
            self.Operations.AND: self.and_,
            self.Operations.OR: self.or_,
            self.Operations.XOR: self.xor,
            self.Operations.NOT: self.not_,
        }

    def latch_right_alu(self, sel: Sel.RightALU):
        if sel == Sel.RightALU.REGISTER:
            self.right = self.datapath.register_block.registers[self.datapath.reg_out1]
        elif sel == Sel.RightALU.DR:
            self.right = self.datapath.data_register
        elif sel == Sel.RightALU.ZERO:
            self.right = 0

    def latch_left_alu(self, sel: Sel.LeftALU):
        if sel == Sel.LeftALU.PC:
            self.left = self.datapath.control_unit.program_counter
        if sel == Sel.LeftALU.Register:
            self.left = self.datapath.register_block.registers[RegisterBlock.Register(self.datapath.reg_out2)]
        if sel == Sel.LeftALU.ALU:
            self.left = self.result
        if sel == Sel.LeftALU.ZERO:
            self.left = 0

    def execute(self, operation: Operations):
        self.operations[operation]()

    def latch_flags(self):
        self.latched_flags = self.temp_flags

    def _update_flags(self, result: int, carry: bool = False, overflow: bool = False):
        mask_32 = 0xFFFFFFFF
        self.result = result & mask_32
        result_32 = self.result
        self.temp_flags = {
            self.Flags.ZERO: result_32 == 0,
            self.Flags.CARRY: carry,
            self.Flags.OVERFLOW: overflow,
            self.Flags.NEGATIVE: bool(result_32 & (1 << 31))
        }

    def add(self):
        mask_32 = 0xFFFFFFFF
        result = self.left + self.right
        carry = result > mask_32
        overflow = (self.left > 0 and self.right > 0 and result < 0) or (
                self.left < 0 and self.right < 0 and result > 0)
        self._update_flags(result, carry, overflow)

    def sub(self):
        result = self.right - self.left
        carry = result < 0
        overflow = (self.left > 0 and self.right < 0 and result < 0) or (
                self.left < 0 and self.right > 0 and result > 0)
        self._update_flags(result, carry, overflow)

    def mul(self):
        result = self.left * self.right
        carry = result != (result & 0xFFFFFFFF)
        self._update_flags(result, carry, carry)

    def div(self):
        result = self.right // self.left
        self._update_flags(result)

    def rmd(self):
        result = self.right % self.left
        self._update_flags(result)

    def and_(self):
        result = self.left & self.right
        self._update_flags(result)

    def or_(self):
        result = self.left | self.right
        self._update_flags(result)

    def xor(self):
        result = self.left ^ self.right
        self._update_flags(result)

    def not_(self):
        result = ~self.right
        self._update_flags(result)


class ControlUnit:
    def __init__(self, datapath: DataPath):
        self.datapath = datapath
        self.datapath.control_unit = self
        self.instruction_register = [None] * 4
        self.program_counter: int = 0
        self.mPC: int = 0
        self.tick: int = 0
        self.branch_condition: bool = False
        self.signals = {
            Signal.SELECT_LEFT_ALU: self.datapath.alu.latch_left_alu,
            Signal.SELECT_RIGHT_ALU: self.datapath.alu.latch_right_alu,
            Signal.EXECUTE_ALU: self.datapath.alu.execute,
            Signal.LATCH_REGISTER: self.datapath.latch_register,
            Signal.LATCH_PC: self.latch_program_counter,
            Signal.LATCH_mPC: self.latch_mpc,
            Signal.LATCH_AR: self.datapath.latch_address_register,
            Signal.LATCH_DR: self.datapath.latch_data_register,
            Signal.LATCH_IR: self.latch_instruction_register,
            Signal.SELECT_REGS: self.select_regs,
            Signal.HALT: self.halt,
            Signal.LATCH_FLAGS: self.datapath.alu.latch_flags,
            Signal.CHECK_CONDITION: self.check_condition,
            Signal.WRITE: self.datapath.write
        }
        self.type_to_mPC = {
            OperandType.REG2REG: 3,
            OperandType.INDIRECT_RIGHT: 4,
            OperandType.PC_OFFSET: 8,
            OperandType.IMMEDIATE: 10
        }

        self.opcode_to_mPC = {
            OpCode.HALT: 14,
            OpCode.ADD: 15,
            OpCode.SUB: 17,
            OpCode.MUL: 19,
            OpCode.DIV: 21,
            OpCode.RMD: 23,
            OpCode.AND: 25,
            OpCode.OR: 27,
            OpCode.XOR: 29,
            OpCode.NOT: 31,
            OpCode.CMP: 33,
            OpCode.MOV: 34,
            OpCode.STORE: 35,
            OpCode.STORE_IMM: 36,
            OpCode.CALL: 38,
            OpCode.RET: 39,
            OpCode.BE: 40,
            OpCode.BNE: 41,
            OpCode.BGE: 42,
            OpCode.BL: 43,
            OpCode.BLE: 44,
            OpCode.BG: 45,
            OpCode.JMP: 46
        }
        self.mProgram = [
            # INSTRUCTION FETCH 0
            [
                (Signal.LATCH_AR, Sel.AR.PC),
                (Signal.LATCH_mPC, Sel.MicroPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_DR, Sel.DR.LOW),
                (Signal.LATCH_IR),
                (Signal.LATCH_mPC, Sel.MicroPC.PLUS_ONE)
            ],
            [
                (Signal.SELECT_REGS),
                (Signal.LATCH_mPC, Sel.MicroPC.OP_FETCH)
            ],
            # OPERANDS FETCH
            # REG REG 3
            [
                (Signal.SELECT_LEFT_ALU, Sel.LeftALU.Register),
                (Signal.SELECT_RIGHT_ALU, Sel.RightALU.ZERO),
                (Signal.EXECUTE_ALU, ALU.Operations.ADD),
                (Signal.LATCH_PC, Sel.PC.PLUS_TWO),
                (Signal.LATCH_mPC, Sel.MicroPC.OPCODE)
            ],

            [  # REG [REG + n]  # 4
                (Signal.SELECT_RIGHT_ALU, Sel.RightALU.DR),
                (Signal.SELECT_LEFT_ALU, Sel.LeftALU.Register),
                (Signal.EXECUTE_ALU, ALU.Operations.ADD),
                (Signal.LATCH_mPC, Sel.MicroPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_AR, Sel.AR.ALU),
                (Signal.LATCH_mPC, Sel.MicroPC.PLUS_ONE),
            ],
            [
                (Signal.LATCH_DR, Sel.DR.FULL),
                (Signal.LATCH_mPC, Sel.MicroPC.PLUS_ONE),
            ],
            [
                (Signal.SELECT_RIGHT_ALU, Sel.RightALU.DR),
                (Signal.SELECT_LEFT_ALU, Sel.LeftALU.ZERO),
                (Signal.EXECUTE_ALU, ALU.Operations.ADD),
                (Signal.LATCH_PC, Sel.PC.PLUS_FOUR),
                (Signal.LATCH_mPC, Sel.MicroPC.OPCODE),
            ],
            # PC_OFFSET 8
            [
                (Signal.SELECT_RIGHT_ALU, Sel.RightALU.DR),
                (Signal.SELECT_LEFT_ALU, Sel.LeftALU.PC),
                (Signal.EXECUTE_ALU, ALU.Operations.ADD),
                (Signal.LATCH_mPC, Sel.MicroPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_PC, Sel.PC.PLUS_FOUR),
                (Signal.LATCH_mPC, Sel.MicroPC.OPCODE)
            ],
            # REG IMM 10
            [
                (Signal.LATCH_PC, Sel.PC.PLUS_TWO),
                (Signal.LATCH_mPC, Sel.MicroPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_AR, Sel.AR.PC),
                (Signal.LATCH_mPC, Sel.MicroPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_DR, Sel.DR.FULL),
                (Signal.LATCH_mPC, Sel.MicroPC.PLUS_ONE)
            ],
            [
                (Signal.SELECT_RIGHT_ALU, Sel.RightALU.DR),
                (Signal.SELECT_LEFT_ALU, Sel.LeftALU.ZERO),
                (Signal.EXECUTE_ALU, ALU.Operations.ADD),
                (Signal.LATCH_mPC, Sel.MicroPC.OPCODE),
                (Signal.LATCH_PC, Sel.PC.PLUS_FOUR)
            ],
            # HALT 14
            [
                (Signal.HALT)
            ],
            # ADD 15
            [
                (Signal.SELECT_LEFT_ALU, Sel.LeftALU.ALU),
                (Signal.SELECT_RIGHT_ALU, Sel.RightALU.REGISTER),
                (Signal.EXECUTE_ALU, ALU.Operations.ADD),
                (Signal.LATCH_FLAGS),
                (Signal.LATCH_mPC, Sel.MicroPC.PLUS_ONE),
            ],
            [
                (Signal.LATCH_REGISTER, Sel.RegisterIn.ALU),
                (Signal.LATCH_mPC, Sel.MicroPC.ZERO)
            ],
            # SUB 17
            [
                (Signal.SELECT_LEFT_ALU, Sel.LeftALU.ALU),
                (Signal.SELECT_RIGHT_ALU, Sel.RightALU.REGISTER),
                (Signal.EXECUTE_ALU, ALU.Operations.SUB),
                (Signal.LATCH_FLAGS),
                (Signal.LATCH_mPC, Sel.MicroPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_REGISTER, Sel.RegisterIn.ALU),
                (Signal.LATCH_mPC, Sel.MicroPC.ZERO),
            ],
            # MUL 19
            [
                (Signal.SELECT_LEFT_ALU, Sel.LeftALU.ALU),
                (Signal.SELECT_RIGHT_ALU, Sel.RightALU.REGISTER),
                (Signal.EXECUTE_ALU, ALU.Operations.MUL),
                (Signal.LATCH_FLAGS),
                (Signal.LATCH_mPC, Sel.MicroPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_REGISTER, Sel.RegisterIn.ALU),
                (Signal.LATCH_mPC, Sel.MicroPC.ZERO),
            ],
            # DIV 21
            [
                (Signal.SELECT_LEFT_ALU, Sel.LeftALU.ALU),
                (Signal.SELECT_RIGHT_ALU, Sel.RightALU.REGISTER),
                (Signal.EXECUTE_ALU, ALU.Operations.DIV),
                (Signal.LATCH_FLAGS),
                (Signal.LATCH_mPC, Sel.MicroPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_REGISTER, Sel.RegisterIn.ALU),
                (Signal.LATCH_mPC, Sel.MicroPC.ZERO),
            ],
            # RMD 23
            [
                (Signal.SELECT_LEFT_ALU, Sel.LeftALU.ALU),
                (Signal.SELECT_RIGHT_ALU, Sel.RightALU.REGISTER),
                (Signal.EXECUTE_ALU, ALU.Operations.RMD),
                (Signal.LATCH_FLAGS),
                (Signal.LATCH_mPC, Sel.MicroPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_REGISTER, Sel.RegisterIn.ALU),
                (Signal.LATCH_mPC, Sel.MicroPC.ZERO),
            ],
            # AND 25
            [
                (Signal.SELECT_LEFT_ALU, Sel.LeftALU.ALU),
                (Signal.SELECT_RIGHT_ALU, Sel.RightALU.REGISTER),
                (Signal.EXECUTE_ALU, ALU.Operations.AND),
                (Signal.LATCH_FLAGS),
                (Signal.LATCH_mPC, Sel.MicroPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_REGISTER, Sel.RegisterIn.ALU),
                (Signal.LATCH_mPC, Sel.MicroPC.ZERO),
            ],
            # OR 27
            [
                (Signal.SELECT_LEFT_ALU, Sel.LeftALU.ALU),
                (Signal.SELECT_RIGHT_ALU, Sel.RightALU.REGISTER),
                (Signal.EXECUTE_ALU, ALU.Operations.OR),
                (Signal.LATCH_FLAGS),
                (Signal.LATCH_mPC, Sel.MicroPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_REGISTER, Sel.RegisterIn.ALU),
                (Signal.LATCH_mPC, Sel.MicroPC.ZERO),
            ],
            # XOR 29
            [
                (Signal.SELECT_LEFT_ALU, Sel.LeftALU.ALU),
                (Signal.SELECT_RIGHT_ALU, Sel.RightALU.REGISTER),
                (Signal.EXECUTE_ALU, ALU.Operations.XOR),
                (Signal.LATCH_FLAGS),
                (Signal.LATCH_mPC, Sel.MicroPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_REGISTER, Sel.RegisterIn.ALU),
                (Signal.LATCH_mPC, Sel.MicroPC.ZERO),
            ],
            # NOT 31
            [
                (Signal.SELECT_LEFT_ALU, Sel.LeftALU.ALU),
                (Signal.SELECT_RIGHT_ALU, Sel.RightALU.REGISTER),
                (Signal.EXECUTE_ALU, ALU.Operations.NOT),
                (Signal.LATCH_FLAGS),
                (Signal.LATCH_mPC, Sel.MicroPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_REGISTER, Sel.RegisterIn.ALU),
                (Signal.LATCH_mPC, Sel.MicroPC.ZERO),
            ],
            # CMP 33
            [
                (Signal.SELECT_LEFT_ALU, Sel.LeftALU.ALU),
                (Signal.SELECT_RIGHT_ALU, Sel.RightALU.REGISTER),
                (Signal.EXECUTE_ALU, ALU.Operations.SUB),
                (Signal.LATCH_FLAGS),
                (Signal.LATCH_mPC, Sel.MicroPC.ZERO),
            ],

            # MOV 34
            [
                (Signal.LATCH_REGISTER, Sel.RegisterIn.ALU),
                (Signal.LATCH_mPC, Sel.MicroPC.ZERO),
            ],
            #  [REG + n], REG for STORE 35
            [
                (Signal.SELECT_RIGHT_ALU, Sel.RightALU.DR),
                (Signal.SELECT_LEFT_ALU, Sel.LeftALU.Register),
                (Signal.EXECUTE_ALU, ALU.Operations.ADD),
                (Signal.LATCH_PC, Sel.PC.PLUS_FOUR),
                (Signal.LATCH_mPC, Sel.MicroPC.PLUS_ONE),
            ],
            # STORE 36
            [
                (Signal.LATCH_AR, Sel.AR.ALU),
                (Signal.LATCH_mPC, Sel.MicroPC.PLUS_ONE),
            ],
            [
                (Signal.WRITE, Sel.DataIn.REGISTER),
                (Signal.LATCH_mPC, Sel.MicroPC.ZERO),
            ],
            # CALL 38
            [
                (Signal.LATCH_REGISTER, Sel.RegisterIn.PC),
                (Signal.LATCH_PC, Sel.PC.ALU),
                (Signal.LATCH_mPC, Sel.MicroPC.ZERO),
            ],
            # RET 39
            [
                (Signal.LATCH_PC, Sel.PC.ALU),
                (Signal.LATCH_mPC, Sel.MicroPC.ZERO),
            ],
            # BE 40
            [
                (Signal.CHECK_CONDITION, Sel.Condition.ZERO),
                (Signal.LATCH_PC, Sel.PC.BRANCH_CONDITION),
                (Signal.LATCH_mPC, Sel.MicroPC.ZERO),
            ],
            # BNE 41
            [
                (Signal.CHECK_CONDITION, Sel.Condition.ZERO),
                (Signal.LATCH_PC, Sel.PC.BRANCH_CONDITION_INVERSE),
                (Signal.LATCH_mPC, Sel.MicroPC.ZERO),
            ],
            # BGE 42
            [
                (Signal.CHECK_CONDITION, Sel.Condition.GREATER_EQUALS),
                (Signal.LATCH_PC, Sel.PC.BRANCH_CONDITION),
                (Signal.LATCH_mPC, Sel.MicroPC.ZERO),
            ],
            # BL 43
            [
                (Signal.CHECK_CONDITION, Sel.Condition.GREATER_EQUALS),
                (Signal.LATCH_PC, Sel.PC.BRANCH_CONDITION_INVERSE),
                (Signal.LATCH_mPC, Sel.MicroPC.ZERO),
            ],
            # BLE 44
            [
                (Signal.CHECK_CONDITION, Sel.Condition.GREATER),
                (Signal.LATCH_PC, Sel.PC.BRANCH_CONDITION_INVERSE),
                (Signal.LATCH_mPC, Sel.MicroPC.ZERO),
            ],
            # BG 45
            [
                (Signal.CHECK_CONDITION, Sel.Condition.GREATER),
                (Signal.LATCH_PC, Sel.PC.BRANCH_CONDITION),
                (Signal.LATCH_mPC, Sel.MicroPC.ZERO),
            ],
            # JMP 46
            [
                (Signal.CHECK_CONDITION, Sel.Condition.NONE),
                (Signal.LATCH_PC, Sel.PC.BRANCH_CONDITION),
                (Signal.LATCH_mPC, Sel.MicroPC.ZERO)
            ]
        ]

    def halt(self):
        raise StopIteration()

    def select_regs(self):
        self.datapath.reg_out1 = self.instruction_register[2]
        self.datapath.reg_out2 = self.instruction_register[3]

    def latch_instruction_register(self):
        self.instruction_register[0] = OpCode(
            self.datapath.memory[self.datapath.address_register] >> 2)  # opcode (6 бит)
        self.instruction_register[1] = OperandType(
            self.datapath.memory[self.datapath.address_register] & 0b11)  # тип (2 бита)
        self.instruction_register[2] = RegisterBlock.Register(
            self.datapath.memory[self.datapath.address_register + 1] >> 4)  # reg1 (4 бита)
        self.instruction_register[3] = RegisterBlock.Register(
            self.datapath.memory[self.datapath.address_register + 1] & 0b00001111)  # reg2 (4 бита)

    def latch_program_counter(self, sel: Sel.PC):
        if sel == Sel.PC.BRANCH_CONDITION and self.branch_condition:
            self.program_counter = self.datapath.alu.result
        elif sel == Sel.PC.BRANCH_CONDITION_INVERSE and not self.branch_condition:
            self.program_counter = self.datapath.alu.result
        elif sel == Sel.PC.ALU:
            self.program_counter = self.datapath.alu.result
        elif sel == Sel.PC.PLUS_TWO:
            self.program_counter += 2
        elif sel == Sel.PC.PLUS_FOUR:
            self.program_counter += 4

    def latch_mpc(self, sel: Sel.MicroPC):
        if sel == Sel.MicroPC.ZERO:
            self.mPC = 0
        elif sel == Sel.MicroPC.PLUS_ONE:
            self.mPC += 1
        elif sel == Sel.MicroPC.OP_FETCH and (self.instruction_register[0].value >> 5) == 0:
            self.mPC = self.type_to_mPC[self.instruction_register[1]]
        elif sel == Sel.MicroPC.OPCODE or (self.instruction_register[0].value >> 5) == 1:
            self.mPC = self.opcode_to_mPC[self.instruction_register[0]]

    def check_condition(self, sel: Sel.Condition):
        self.branch_condition = True
        if sel == Sel.Condition.ZERO:
            self.branch_condition = self.datapath.alu.latched_flags[ALU.Flags.ZERO]
        elif sel == Sel.Condition.GREATER_EQUALS:
            self.branch_condition = self.datapath.alu.latched_flags[ALU.Flags.OVERFLOW] == \
                                    self.datapath.alu.latched_flags[ALU.Flags.NEGATIVE]
        elif sel == Sel.Condition.GREATER:
            self.branch_condition = self.datapath.alu.latched_flags[ALU.Flags.OVERFLOW] == \
                                    self.datapath.alu.latched_flags[ALU.Flags.NEGATIVE] and \
                                    self.datapath.alu.latched_flags[ALU.Flags.ZERO]

    def process_next_tick(self):
        for action in self.mProgram[self.mPC]:
            if isinstance(action, tuple):
                signal, arg = action
                self.signals[signal](arg)
            else:
                self.signals[action]()
        self.tick += 1

    def __repr__(self):
        """Вернуть строковое представление состояния процессора."""
        if self.datapath.reg_out1 is None:
            return " ".join([
                f"TICK:{self.tick:4}",
                f"PC:{self.program_counter:4}",
                f"REG1:{str(self.datapath.reg_out1)[9:]:5}",
                f"REG2:{str(self.datapath.reg_out2)[9:]:5}",
                f" R15_VAL:{self.datapath.register_block.registers[RegisterBlock.Register.R15]:5}"
                f" R14_VAL:{self.datapath.register_block.registers[RegisterBlock.Register.R14]:5}"
                f" AR:{self.datapath.address_register:5}"
                f" DR:{self.datapath.data_register:6}"])

        instr = self.instruction_register
        intstr_mnem = str(instr[0])[7:] + " "
        if instr[1] == OperandType.REG2REG:
            intstr_mnem += str(instr[2])[9:] + ", " + str(instr[3])[9:]
        elif instr[1] == OperandType.IMMEDIATE:
            intstr_mnem += str(instr[2])[9:] + ", VAL"
        elif instr[1] == OperandType.INDIRECT_RIGHT:
            intstr_mnem += str(instr[2])[9:] + ", [" + str(instr[3])[9:] + " + " + str(self.datapath.data_register) +"]"
        else:
            intstr_mnem += "PC + " + str(self.datapath.data_register)
        components = [
            f"TICK:{self.tick:4}",
            f" PC:{self.program_counter:4}",
            f" REG1:{str(self.datapath.reg_out1)[9:]:5}",
            f" REG1_VAL:{self.datapath.register_block.registers[self.datapath.reg_out1]:5}",
            f" REG2:{str(self.datapath.reg_out2)[9:]:5}",
            f" REG2_VAL:{self.datapath.register_block.registers[self.datapath.reg_out2]:5}",
            f" R15_VAL:{self.datapath.register_block.registers[RegisterBlock.Register.R15]:6}"
            f" R14_VAL:{self.datapath.register_block.registers[RegisterBlock.Register.R14]:5}"
            f" AR:{self.datapath.address_register:5}"
            f" DR:{self.datapath.data_register:6}"
            f" {intstr_mnem}"
        ]
        return " ".join(components)


def simulation(memory_init, input_tokens, data_memory_size, limit):
    io = IO(input_tokens)
    dp = DataPath(io, data_memory_size, 0x9996, 0x10000)

    idx = 0
    for i in memory_init:
        dp.memory[idx] = i
        idx += 1

    control_unit = ControlUnit(dp)

    logging.debug("%s", control_unit, extra={"skip_filename": True})
    try:
        while control_unit.tick < limit:
            control_unit.process_next_tick()
            if control_unit.tick <= 100:
                logging.debug("%s", control_unit, extra={"skip_filename": True})

    except EOFError:
        logging.warning("Input buffer is empty!")
    except StopIteration:
        pass

    if control_unit.tick >= limit:
        logging.warning("Limit exceeded!")
    logging.info("output_buffer: %s", repr("".join(str(dp.io.output))))
    return dp.io.output, control_unit.tick


def main(code_file, input_file, char_io=True):
    with open(code_file, "rb") as file:
        binary_code = file.read()
    code = bytearray(binary_code)
    with open(input_file, encoding="utf-8") as file:
        input_text = file.read()
        input_token = []
        for char in input_text:
            input_token.append(char)

    if char_io:
        input_token = [ord(ch) for ch in input_token]

    output, ticks = simulation(
        code,
        input_tokens=input_token,
        data_memory_size=100000,
        limit=6000000,
    )
    if char_io:
        output = [chr(num) for num in output]
    else:
        output = [str(num) for num in output]
    print("".join(output))
    print("ticks:", ticks)


if __name__ == "__main__":
    logging.basicConfig(
        level=logging.DEBUG,
        format="%(levelname)s   machine:simulation    %(message)s",
    )

    assert len(sys.argv) >= 3, "Wrong arguments: machine.py <code_file> <input_file>"
    _, code_file, input_file, char_io = sys.argv
    main(code_file, input_file, char_io)
