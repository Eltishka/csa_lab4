from enum import Enum
from isa import OperandType, OpCode


class Signal(Enum):
    TICK: int = 0

    LATCH_LEFT_ALU: int = 1
    LATCH_RIGHT_ALU: int = 2
    EXECUTE_ALU: int = 3

    DECODE_INSTRUCTION: int = 4

    LATCH_REGISTER: int = 5

    LATCH_PC: int = 6

    LATCH_mPC: int = 7

    LATCH_AR: int = 8
    LATCH_DR: int = 9

    READ: int = 10
    WRITE: int = 11

    SELECT_REGS: int = 12
    LATCH_IR: int = 13
    HALT: int = 14


class Sel:
    class LeftALU(Enum):
        Register: int = 0
        ALU: int = 1
        ZERO: int = 3

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

    class mPC(Enum):
        OPCODE: int = 0
        OP_FETCH: int = 1
        ZERO: int = 2
        PLUS_ONE: int = 3

    class PC(Enum):
        REGISTER: int = 0
        DR: int = 1
        PLUS_ONE: int = 2
        PLUS_TWO: int = 3
        PLUS_THREE: int = 4
        PLUS_FOUR: int = 5

    class DataIn(Enum):
        REGISTER: int = 0
        ALU: int = 0


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


class DataPath:
    def __init__(self, memory_size: int):
        self.alu: ALU = ALU(self)
        self.control_unit: ControlUnit = ControlUnit(self)

        self.memory = [0] * memory_size
        self.data_register: int = 0
        self.address_register: int = 0

        self.reg_out1: RegisterBlock.Register = None
        self.reg_out2: RegisterBlock.Register = None
        self.reg_in: RegisterBlock.Register = None
        self.register_block: RegisterBlock = RegisterBlock()

    def select_reg_out1(self, register: RegisterBlock.Register):
        self.reg_out1 = register

    def select_reg_out2(self, register: RegisterBlock.Register):
        self.reg_out2 = register

    def select_reg_in(self, register: RegisterBlock.Register):
        self.reg_out1 = register

    def write(self, sel: Sel.DataIn):
        if sel == Sel.DataIn.REGISTER:
            self.memory[self.address_register] = self.register_block.registers[self.reg_out1]
        if sel == Sel.DataIn.ALU:
            self.memory[self.address_register] = self.alu.result

    def latch_address_register(self, sel: Sel.AR):
        if sel == Sel.AR.PC:
            self.address_register = self.control_unit.program_counter
        if sel == Sel.AR.ALU:
            self.address_register = self.alu.result

    def latch_data_register(self):
        self.data_register = (self.memory[self.address_register] << 24) | \
                             (self.memory[self.address_register + 1] << 16) | \
                             (self.memory[self.address_register + 2] << 8) | \
                             self.memory[self.address_register + 3]

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
        ADC: int = 1
        SUB: int = 2
        MUL: int = 3
        DIV: int = 4
        RMD: int = 5
        SAR: int = 6
        SAL: int = 7
        AND: int = 8
        OR: int = 9
        XOR: int = 10
        NOT: int = 11
        NEG: int = 12

    def __init__(self, datapath: DataPath):
        self.datapath = datapath
        self.left: int = 0
        self.right: int = 0
        self.result: int = 0
        self.flags = {
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
            self.Operations.ADC: self.adc,
            self.Operations.SAR: self.sar,
            self.Operations.SAL: self.sal,
            self.Operations.AND: self.and_,
            self.Operations.OR: self.or_,
            self.Operations.XOR: self.xor,
            self.Operations.NOT: self.not_,
            self.Operations.NEG: self.neg,
        }

    def latch_right_alu(self, sel: Sel.RightALU):
        if sel == Sel.RightALU.REGISTER:
            self.right = self.datapath.register_block.registers[self.datapath.reg_out1]
        elif sel == Sel.RightALU.DR:
            self.right = self.datapath.data_register
        elif sel == Sel.RightALU.ZERO:
            self.right = 0

    def latch_left_alu(self, sel: Sel.LeftALU):
        if sel == Sel.LeftALU.Register:
            self.left = self.datapath.register_block.registers[RegisterBlock.Register(self.datapath.reg_out2)]
        if sel == Sel.LeftALU.ALU:
            self.left = self.result
        if sel == Sel.LeftALU.ZERO:
            self.left = 0

    def execute(self, operation: Operations):
        self.operations[operation]()

    def _update_flags(self, result: int, carry: bool = False, overflow: bool = False):
        mask_32 = 0xFFFFFFFF
        self.result = result & mask_32
        result_32 = self.result
        print(self.left, self.right, self.result)
        self.flags = {
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
        if self.right == 0:
            raise ZeroDivisionError("Division by zero")
        result = self.right // self.left
        self._update_flags(result)

    def rmd(self):
        if self.right == 0:
            raise ZeroDivisionError("Division by zero")
        result = self.right % self.left
        self._update_flags(result)

    def adc(self):
        result = self.left + self.right + self.flags[self.Flags.CARRY]
        carry = result > 0xFFFFFFFF
        overflow = (self.left > 0 and self.right > 0 and result < 0) or (self.left < 0 and b < 0 and result > 0)
        self._update_flags(result, carry, overflow)

    def and_(self):
        result = self.left & self.right
        self._update_flags(result)

    def or_(self):
        result = self.left | self.right
        self._update_flags(result)

    def xor(self):
        result = self.left ^ self.right
        self._update_flags(result)

    def neg(self):
        result = -self.right
        overflow = self.right == (1 << 31)
        self._update_flags(result, overflow=overflow)

    def not_(self):
        result = ~self.right
        self._update_flags(result)

    def sal(self):
        if self.left < 0:
            raise ValueError("Shift amount must be non-negative")
        if self.left >= 32:
            result = 0
            carry = False
        else:
            result = self.right << self.left
            carry = (result >> 32) & 1 == 1
            overflow = ((self.right << (self.left - 1)) >> 31) != (result >> 31)

        self._update_flags(result, carry=carry, overflow=overflow)

    def sar(self):
        if self.left < 0:
            raise ValueError("Shift amount must be non-negative")
        if self.left >= 32:
            result = 0xFFFFFFFF if (self.right & (1 << 31)) else 0
            carry = (self.right & (1 << 31)) != 0
        else:
            result = (self.right >> self.left) | (
                        (self.right & (1 << 31)) * ((1 << self.left) - 1) << (32 - self.left)) if (
                    self.right & (1 << 31)) else self.right >> self.left
            carry = (self.right >> (self.left - 1)) & 1 == 1 if self.left > 0 else False

        self._update_flags(result, carry=carry)


class ControlUnit:
    def __init__(self, datapath: DataPath):
        self.datapath = datapath
        self.instruction_register = [0] * 4
        self.program_counter: int = 0
        self.mPC: int = 0
        self.tick = 0
        self.signals = {
            Signal.LATCH_LEFT_ALU: self.datapath.alu.latch_left_alu,
            Signal.LATCH_RIGHT_ALU: self.datapath.alu.latch_right_alu,
            Signal.EXECUTE_ALU: self.datapath.alu.execute,
            Signal.DECODE_INSTRUCTION: self.decode,
            Signal.LATCH_REGISTER: self.datapath.latch_register,
            Signal.LATCH_PC: self.latch_program_counter,
            Signal.LATCH_mPC: self.latch_mPC,
            Signal.LATCH_AR: self.datapath.latch_address_register,
            Signal.LATCH_DR: self.datapath.latch_data_register,
            Signal.LATCH_IR: self.latch_instruction_register,
            Signal.SELECT_REGS: self.select_regs,
            Signal.HALT: self.halt
        }
        self.type_to_mPC = {
            OperandType.REG2REG: 1,
            OperandType.INDIRECT: 2,
            OperandType.IMMEDIATE: 7
        }

        self.opcode_to_mPC = {
            OpCode.HALT: 10,
            OpCode.ADD: 11,
            OpCode.ADC: 13,
            OpCode.SUB: 15,
            OpCode.MUL: 17,
            OpCode.DIV: 19,
            OpCode.RMD: 21,
            OpCode.AND: 23,
            OpCode.OR: 25,
            OpCode.XOR: 27,
            OpCode.NEG: 29,
            OpCode.NOT: 31,
            OpCode.SAL: 33,
            OpCode.SAR: 35,
            OpCode.CMP: 37,
            OpCode.MOV: 38,
            OpCode.CALL: 17,
            OpCode.RET: 18,
            OpCode.BZS: 19,
            OpCode.BZC: 20,
            OpCode.BG: 21,
            OpCode.BL: 22,
            OpCode.BCS: 23,
            OpCode.BCC: 24,
            OpCode.BVS: 25,
            OpCode.BNS: 26,
            OpCode.BNC: 27,
        }
        self.mProgram = [
            # INSTRUCTION FETCH 0
            [
                (Signal.LATCH_AR, Sel.AR.PC),
                (Signal.LATCH_DR),
                (Signal.LATCH_IR),
                (Signal.SELECT_REGS),
                (Signal.LATCH_mPC, Sel.mPC.OP_FETCH)
            ],
            # OPERANDS FETCH

            # REG REG 1
            [
                (Signal.LATCH_LEFT_ALU, Sel.LeftALU.Register),
                (Signal.LATCH_RIGHT_ALU, Sel.RightALU.ZERO),
                (Signal.EXECUTE_ALU, ALU.Operations.ADD),
                (Signal.LATCH_PC, Sel.PC.PLUS_TWO),
                (Signal.LATCH_mPC, Sel.mPC.OPCODE)
            ],
            # REG [REG + 4] / [REG + n] REG 2
            [
                (Signal.LATCH_PC, Sel.PC.PLUS_TWO),
                (Signal.LATCH_mPC, Sel.mPC.PLUS_ONE),
            ],
            [
                (Signal.LATCH_AR, Sel.AR.PC),
                (Signal.LATCH_DR),
                (Signal.LATCH_mPC, Sel.mPC.PLUS_ONE),
            ],
            [
                (Signal.LATCH_RIGHT_ALU, Sel.RightALU.DR),
                (Signal.LATCH_LEFT_ALU, Sel.LeftALU.Register),
                (Signal.EXECUTE_ALU, ALU.Operations.ADD),
                (Signal.LATCH_mPC, Sel.mPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_AR, Sel.AR.ALU),
                (Signal.LATCH_DR),
                (Signal.LATCH_mPC, Sel.mPC.PLUS_ONE),
            ],
            [
                (Signal.LATCH_RIGHT_ALU, Sel.RightALU.DR),
                (Signal.LATCH_LEFT_ALU, Sel.LeftALU.ZERO),
                (Signal.EXECUTE_ALU, ALU.Operations.ADD),
                (Signal.LATCH_mPC, Sel.mPC.OPCODE),
                (Signal.LATCH_PC, Sel.PC.PLUS_FOUR)
            ],
            # REG IMM 7
            [
                (Signal.LATCH_PC, Sel.PC.PLUS_TWO),
                (Signal.LATCH_mPC, Sel.mPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_AR, Sel.AR.PC),
                (Signal.LATCH_DR),
                (Signal.LATCH_mPC, Sel.mPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_RIGHT_ALU, Sel.RightALU.DR),
                (Signal.LATCH_LEFT_ALU, Sel.LeftALU.ZERO),
                (Signal.EXECUTE_ALU, ALU.Operations.ADD),
                (Signal.LATCH_mPC, Sel.mPC.OPCODE),
                (Signal.LATCH_PC, Sel.PC.PLUS_FOUR)
            ],
            # HALT 10
            [
                (Signal.HALT)
            ],
            # ADD 11
            [
                (Signal.LATCH_LEFT_ALU, Sel.LeftALU.ALU),
                (Signal.LATCH_RIGHT_ALU, Sel.RightALU.REGISTER),
                (Signal.EXECUTE_ALU, ALU.Operations.ADD),
                (Signal.LATCH_mPC, Sel.mPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_REGISTER, Sel.RegisterIn.ALU),
                (Signal.LATCH_mPC, Sel.mPC.ZERO)
            ],
            # ADC 13
            [
                (Signal.LATCH_LEFT_ALU, Sel.LeftALU.ALU),
                (Signal.LATCH_RIGHT_ALU, Sel.RightALU.REGISTER),
                (Signal.EXECUTE_ALU, ALU.Operations.ADC),
                (Signal.LATCH_mPC, Sel.mPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_REGISTER, Sel.RegisterIn.ALU),
                (Signal.LATCH_mPC, Sel.mPC.ZERO),
            ],
            # SUB 15
            [
                (Signal.LATCH_LEFT_ALU, Sel.LeftALU.ALU),
                (Signal.LATCH_RIGHT_ALU, Sel.RightALU.REGISTER),
                (Signal.EXECUTE_ALU, ALU.Operations.SUB),
                (Signal.LATCH_mPC, Sel.mPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_REGISTER, Sel.RegisterIn.ALU),
                (Signal.LATCH_mPC, Sel.mPC.ZERO),
            ],
            # MUL 17
            [
                (Signal.LATCH_LEFT_ALU, Sel.LeftALU.ALU),
                (Signal.LATCH_RIGHT_ALU, Sel.RightALU.REGISTER),
                (Signal.EXECUTE_ALU, ALU.Operations.MUL),
                (Signal.LATCH_mPC, Sel.mPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_REGISTER, Sel.RegisterIn.ALU),
                (Signal.LATCH_mPC, Sel.mPC.ZERO),
            ],
            # DIV 19
            [
                (Signal.LATCH_LEFT_ALU, Sel.LeftALU.ALU),
                (Signal.LATCH_RIGHT_ALU, Sel.RightALU.REGISTER),
                (Signal.EXECUTE_ALU, ALU.Operations.DIV),
                (Signal.LATCH_mPC, Sel.mPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_REGISTER, Sel.RegisterIn.ALU),
                (Signal.LATCH_mPC, Sel.mPC.ZERO),
            ],
            # RMD 21
            [
                (Signal.LATCH_LEFT_ALU, Sel.LeftALU.ALU),
                (Signal.LATCH_RIGHT_ALU, Sel.RightALU.REGISTER),
                (Signal.EXECUTE_ALU, ALU.Operations.RMD),
                (Signal.LATCH_mPC, Sel.mPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_REGISTER, Sel.RegisterIn.ALU),
                (Signal.LATCH_mPC, Sel.mPC.ZERO),
            ],
            # AND 23
            [
                (Signal.LATCH_LEFT_ALU, Sel.LeftALU.ALU),
                (Signal.LATCH_RIGHT_ALU, Sel.RightALU.REGISTER),
                (Signal.EXECUTE_ALU, ALU.Operations.AND),
                (Signal.LATCH_mPC, Sel.mPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_REGISTER, Sel.RegisterIn.ALU),
                (Signal.LATCH_mPC, Sel.mPC.ZERO),
            ],
            # OR 25
            [
                (Signal.LATCH_LEFT_ALU, Sel.LeftALU.ALU),
                (Signal.LATCH_RIGHT_ALU, Sel.RightALU.REGISTER),
                (Signal.EXECUTE_ALU, ALU.Operations.OR),
                (Signal.LATCH_mPC, Sel.mPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_REGISTER, Sel.RegisterIn.ALU),
                (Signal.LATCH_mPC, Sel.mPC.ZERO),
            ],
            # XOR 27
            [
                (Signal.LATCH_LEFT_ALU, Sel.LeftALU.ALU),
                (Signal.LATCH_RIGHT_ALU, Sel.RightALU.REGISTER),
                (Signal.EXECUTE_ALU, ALU.Operations.XOR),
                (Signal.LATCH_mPC, Sel.mPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_REGISTER, Sel.RegisterIn.ALU),
                (Signal.LATCH_mPC, Sel.mPC.ZERO),
            ],
            # NEG 29
            [
                (Signal.LATCH_LEFT_ALU, Sel.LeftALU.ALU),
                (Signal.LATCH_RIGHT_ALU, Sel.RightALU.REGISTER),
                (Signal.EXECUTE_ALU, ALU.Operations.NEG),
                (Signal.LATCH_mPC, Sel.mPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_REGISTER, Sel.RegisterIn.ALU),
                (Signal.LATCH_mPC, Sel.mPC.ZERO),
            ],
            # NOT 31
            [
                (Signal.LATCH_LEFT_ALU, Sel.LeftALU.ALU),
                (Signal.LATCH_RIGHT_ALU, Sel.RightALU.REGISTER),
                (Signal.EXECUTE_ALU, ALU.Operations.NOT),
                (Signal.LATCH_mPC, Sel.mPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_REGISTER, Sel.RegisterIn.ALU),
                (Signal.LATCH_mPC, Sel.mPC.ZERO),
            ],
            # SAL 33
            [
                (Signal.LATCH_LEFT_ALU, Sel.LeftALU.ALU),
                (Signal.LATCH_RIGHT_ALU, Sel.RightALU.REGISTER),
                (Signal.EXECUTE_ALU, ALU.Operations.SAL),
                (Signal.LATCH_mPC, Sel.mPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_REGISTER, Sel.RegisterIn.ALU),
                (Signal.LATCH_mPC, Sel.mPC.ZERO),
            ],
            # SAR 35
            [
                (Signal.LATCH_LEFT_ALU, Sel.LeftALU.ALU),
                (Signal.LATCH_RIGHT_ALU, Sel.RightALU.REGISTER),
                (Signal.EXECUTE_ALU, ALU.Operations.SAR),
                (Signal.LATCH_mPC, Sel.mPC.PLUS_ONE)
            ],
            [
                (Signal.LATCH_REGISTER, Sel.RegisterIn.ALU),
                (Signal.LATCH_mPC, Sel.mPC.ZERO),
            ],
            # CMP 37
            [
                (Signal.LATCH_LEFT_ALU, Sel.LeftALU.ALU),
                (Signal.LATCH_RIGHT_ALU, Sel.RightALU.REGISTER),
                (Signal.EXECUTE_ALU, ALU.Operations.SUB),
                (Signal.LATCH_mPC, Sel.mPC.ZERO),
            ],
            # MOV DEFAULT 38
            [
                (Signal.LATCH_REGISTER, Sel.RegisterIn.ALU),
                (Signal.LATCH_mPC, Sel.mPC.ZERO),
            ],
            # MOV [REG + n], REG 11
            [
                (Signal.LATCH_AR, Sel.AR.ALU),
                (Signal.WRITE, Sel.DataIn.REGISTER),
                (Signal.LATCH_mPC, Sel.mPC.ZERO),
            ],
        ]

    def halt(self):
        raise "HALTED"

    def select_regs(self):
        self.datapath.reg_out1 = self.instruction_register[2]
        self.datapath.reg_out2 = self.instruction_register[3]

    def latch_instruction_register(self):
        self.instruction_register[0] = OpCode((self.datapath.data_register >> 26) & 0b111111)  # opcode (6 бит)
        self.instruction_register[1] = OperandType((self.datapath.data_register >> 24) & 0b11)  # тип (2 бита)
        self.instruction_register[2] = RegisterBlock.Register((self.datapath.data_register >> 20) & 0b1111)  # reg1 (4 бита)
        self.instruction_register[3] = RegisterBlock.Register((self.datapath.data_register >> 16) & 0b1111)

    def latch_program_counter(self, sel: Sel.PC):
        if sel == Sel.PC.REGISTER:
            self.program_counter = self.datapath.register_block.registers[self.datapath.reg_out1]
        if sel == Sel.PC.DR:
            self.program_counter = self.datapath.data_register
        if sel == Sel.PC.PLUS_ONE:
            self.program_counter += 1
        if sel == Sel.PC.PLUS_TWO:
            self.program_counter += 2
        if sel == Sel.PC.PLUS_THREE:
            self.program_counter += 3
        if sel == Sel.PC.PLUS_FOUR:
            self.program_counter += 4

    def latch_mPC(self, sel: Sel.mPC):
        if sel == Sel.mPC.ZERO:
            self.mPC = 0
        if sel == Sel.mPC.PLUS_ONE:
            self.mPC += 1
        if sel == Sel.mPC.OP_FETCH:
            self.mPC = self.type_to_mPC[self.instruction_register[1]]
        if sel == Sel.mPC.OPCODE:
            self.mPC = self.opcode_to_mPC[self.instruction_register[0]]
        # TODO opcode

    def decode(self, micro_instruction):
        for action in micro_instruction:
            if isinstance(action, tuple):
                print(action)
                signal, arg = action
                self.signals[signal](arg)
            else:
                print(action)
                self.signals[action]()

    def simulate(self):
        for i in range(200):
            print(self.mPC)
            self.decode(self.mProgram[self.mPC])
            if self.mPC == self.opcode_to_mPC[OpCode.HALT]:
                print(self.mPC)
                break


if __name__ == "__main__":
    dp = DataPath(100)
    dp.memory[0] = 0b01000110  # mov r0, 4
    dp.memory[1] = 0b00000000
    dp.memory[2] = 0b00000000
    dp.memory[3] = 0b00000000
    dp.memory[4] = 0b00000000
    dp.memory[5] = 0b00000100

    dp.memory[6] = 0b01000110  # mov r1, 5
    dp.memory[7] = 0b00010000
    dp.memory[8] = 0b00000000
    dp.memory[9] = 0b00000000
    dp.memory[10] = 0b00000000
    dp.memory[11] = 0b00000101

    dp.memory[12] = 0b00111000  # ADD r1, r0
    dp.memory[13] = 0b00000001
    dp.memory[14] = 0b00000000

    cu = dp.control_unit
    cu.simulate()
    print(dp.register_block.registers)
    print(dp.alu.flags)
