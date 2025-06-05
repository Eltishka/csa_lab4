import os
import sys

from astparser import Parser
from isa import OpCode, OperandType
from machine import RegisterBlock


class LispCompiler:

    def __init__(self, code):
        self.text = bytearray()
        self.data = bytearray()
        self.heap_pointer_initial = 3000
        self.heap_pointer = RegisterBlock.Register.R13
        self.static_data_addresses = {}
        self.asm = []
        self.pc = 0
        self.output_addr = 0x10000
        self.input_addr = 0x9996
        self.labels = {}
        self.labels_to_replace = []
        self.stack_base = 9000
        self.sp = RegisterBlock.Register.R15
        self.return_register = RegisterBlock.Register.R14
        self.current_block_idx = 0
        self.binop2opcode = {
            "+": OpCode.ADD,
            "-": OpCode.SUB,
            "*": OpCode.MUL,
            "/": OpCode.DIV,
            "mod": OpCode.RMD,
            "and": OpCode.AND,
            "or": OpCode.OR,
        }
        self.compare2opcode_inverse = {
            "<": OpCode.BGE,
            ">=": OpCode.BL,
            "=": OpCode.BNE,
            "!=": OpCode.BE,
            ">": OpCode.BLE,
            "<=": OpCode.BGE,
        }
        self.default_exprs_compilers = {
            "if": self.compile_if,
            "let": self.compile_let,
            "setq": self.compile_setq,
            "loop": self.compile_loop,
            "return": self.compile_return,
            "defun": self.compile_defun,
            "not": self.compile_not,
        }
        parser = Parser()
        self.ast = parser.parse(code)

    def to_bytes(self):
        return self.text + self.data

    def to_hex(self):
        result = []
        address = 0
        for line in self.asm:
            word = self.emit_instr(*line)
            hex_word = word.hex().upper()

            mnemonic = str(line[0])[7:] + " "
            if line[1] == OperandType.REG2REG:
                mnemonic += str(line[2])[9:] + ", " + str(line[3])[9:]
            elif line[1] == OperandType.IMMEDIATE:
                mnemonic += str(line[2])[9:] + ", " + str(line[4])
            elif line[1] == OperandType.INDIRECT_RIGHT:
                mnemonic += str(line[2])[9:] + ", [" + str(line[3])[9:] + " + " + str(line[4]) + "]"
            else:
                mnemonic += "PC + " + str(line[4])
            line = f"{address:<5} - {hex_word:<12} - {mnemonic:<15}".strip()
            address += len(word)
            result.append(line)

        for line in self.static_data_addresses:
            address = self.static_data_addresses[line]
            mnemonic = line
            word = []
            for ch in line[1:-1]:
                word += bytearray([0, 0, 0, ord(ch)])
            word += bytearray([0, 0, 0, 0])
            hex_word = bytearray(word).hex().upper()
            line = f"{address} - {hex_word} - {mnemonic}".strip()
            result.append(line)
        return "\n".join(result)

    def compile(self):
        self.translate_to_instr(OpCode.MOV, OperandType.IMMEDIATE, self.sp, self.sp, self.stack_base)
        self.translate_to_instr(OpCode.MOV, OperandType.IMMEDIATE, self.heap_pointer, self.heap_pointer,
                                self.heap_pointer_initial)
        for node in self.ast:
            if node[0] != "defun":
                self.compile_expr(node, RegisterBlock.Register.R0)
        self.translate_to_instr(OpCode.HALT, OperandType.REG2REG, RegisterBlock.Register.R0, RegisterBlock.Register.R0)
        self.compile_print()
        self.compile_concat()
        self.compile_list()
        self.compile_car()
        self.compile_cdr()
        self.compile_null()
        self.compile_push()
        self.compile_nreverse()
        self.compile_read()
        self.compile_input_string()
        for node in self.ast:
            if node[0] == "defun":
                self.compile_expr(node, RegisterBlock.Register.R0)

        self.replace_labels()
        for line in self.asm:
            self.text += bytearray(self.emit_instr(*line))

    def emit_instr(self, opcode, optype, r1, r2, val=None):
        instr = (opcode.value << 26) | (optype.value << 24) | (r1.value << 20) | (
                r2.value << 16)
        result = [
            (instr >> 24) & 0xFF,
            (instr >> 16) & 0xFF,

        ]
        if optype == OperandType.IMMEDIATE:
            result += [(val >> 24) & 0xFF, (val >> 16) & 0xFF, (val >> 8) & 0xFF, val & 0xFF]
        elif optype in (OperandType.PC_OFFSET, OperandType.INDIRECT_RIGHT):
            result += [(val >> 8) & 0xFF, val & 0xFF]
        return bytearray(result)

    def translate_to_instr(self, opcode, optype, r1, r2, imm=None):
        self.asm.append((opcode, optype, r1, r2, imm))
        if optype is OperandType.IMMEDIATE:
            self.pc += 6
        elif optype in (OperandType.PC_OFFSET, OperandType.INDIRECT_RIGHT):
            self.pc += 4
        else:
            self.pc += 2
        return self.pc

    def compile_expr(self, expr, return_reg, local_vars={}, current_block_end=None):
        if isinstance(expr, int):
            self.compile_int_expr(expr, return_reg)
        elif isinstance(expr, list):
            self.compile_list_expr(expr, return_reg, local_vars, current_block_end)
        elif isinstance(expr, str):
            if expr in local_vars:
                self.translate_to_instr(OpCode.MOV, OperandType.INDIRECT_RIGHT, return_reg, self.sp, local_vars[expr])
            elif expr[0][0] == '"' and expr[0][-1] == '"':
                self.compile_string(expr, return_reg)

    def compile_int_expr(self, expr, return_reg):
        self.translate_to_instr(OpCode.MOV, OperandType.IMMEDIATE, return_reg, return_reg, expr)
        self.translate_to_instr(OpCode.MOV, OperandType.IMMEDIATE, RegisterBlock.Register.R1,
                                RegisterBlock.Register.R1, 0)

    def compile_list_expr(self, expr, return_reg, local_vars={}, current_block_end=None):
        if len(expr) == 0:
            self.compile_empty_list(return_reg)
        elif isinstance(expr[0], list):
            for ex in expr:
                self.compile_expr(ex, return_reg, local_vars, current_block_end)
        elif expr[0] in self.default_exprs_compilers:
            self.default_exprs_compilers[expr[0]](expr, return_reg, local_vars, current_block_end)
        elif expr[0] in self.binop2opcode:
            self.compile_binop(expr, return_reg, local_vars)
        elif expr[0] in self.compare2opcode_inverse:
            self.compile_comparing(expr, return_reg, local_vars)
        else:
            self.compile_call(expr, return_reg, local_vars)

    def compile_string_expr(self, expr, return_reg, local_vars={}):
        if expr in local_vars:
            self.translate_to_instr(OpCode.MOV, OperandType.INDIRECT_RIGHT, return_reg, self.sp, local_vars[expr])
        elif expr[0][0] == '"' and expr[0][-1] == '"':
            self.compile_string(expr, return_reg)

    def push_reg(self, reg, local_vars={}):
        self.translate_to_instr(OpCode.SUB, OperandType.IMMEDIATE, self.sp, self.sp, 4)
        self.translate_to_instr(OpCode.STORE, OperandType.INDIRECT_RIGHT, reg, self.sp, 0)

        for var in local_vars:
            local_vars[var] += 4

    def pop_reg(self, reg, local_vars={}):
        self.translate_to_instr(OpCode.MOV, OperandType.INDIRECT_RIGHT, reg, self.sp, 0)
        self.translate_to_instr(OpCode.ADD, OperandType.IMMEDIATE, self.sp, self.sp, 4)

        for var in local_vars:
            local_vars[var] -= 4

    def compile_binop(self, binop_node, return_reg, local_vars={}, current_block_end=None):
        binop = binop_node[0]
        result_reg = RegisterBlock.Register.R7

        self.push_reg(result_reg, local_vars)
        self.compile_expr(binop_node[1], RegisterBlock.Register.R3, local_vars)
        self.translate_to_instr(OpCode.MOV, OperandType.REG2REG, result_reg, RegisterBlock.Register.R3)

        for operand in binop_node[2:]:
            self.compile_expr(operand, RegisterBlock.Register.R2, local_vars)
            self.translate_to_instr(self.binop2opcode[binop], OperandType.REG2REG, result_reg,
                                    RegisterBlock.Register.R2)

        self.translate_to_instr(OpCode.MOV, OperandType.REG2REG, return_reg,
                                result_reg)

        self.pop_reg(result_reg, local_vars)

    def compile_comparing(self, comparing_node, return_reg, local_vars={}, current_block_end=None):
        result_reg = RegisterBlock.Register.R7

        self.push_reg(result_reg, local_vars)

        self.compile_expr(comparing_node[1], RegisterBlock.Register.R2, local_vars)
        self.translate_to_instr(OpCode.MOV, OperandType.REG2REG, result_reg, RegisterBlock.Register.R2)
        self.current_block_idx += 1
        label_false = str(self.current_block_idx) + "_false"
        for operand in comparing_node[2:]:
            self.compile_expr(operand, RegisterBlock.Register.R2, local_vars)
            self.translate_to_instr(OpCode.CMP, OperandType.REG2REG, result_reg,
                                    RegisterBlock.Register.R2)
            self.labels[label_false] = self.pc
            self.translate_to_instr(self.compare2opcode_inverse[comparing_node[0]], OperandType.PC_OFFSET, result_reg,
                                    result_reg, label_false)
            self.labels_to_replace.append(len(self.asm) - 1)

        self.translate_to_instr(OpCode.MOV, OperandType.IMMEDIATE, return_reg, return_reg,
                                1)
        self.translate_to_instr(OpCode.JMP, OperandType.PC_OFFSET, return_reg, return_reg, 10)
        self.labels[label_false] = self.pc - self.labels[label_false]
        self.translate_to_instr(OpCode.MOV, OperandType.IMMEDIATE, return_reg, return_reg,
                                0)

        self.pop_reg(result_reg, local_vars)

    def compile_not(self, not_node, return_reg, local_vars={}, current_block_end=None):
        self.compile_expr(not_node[1], return_reg, local_vars)
        self.translate_to_instr(OpCode.NOT, OperandType.REG2REG, return_reg, return_reg)
        self.translate_to_instr(OpCode.AND, OperandType.IMMEDIATE, return_reg, return_reg, 1)

    def compile_if(self, if_node, return_reg, local_vars={}, current_block_end=None):
        self.compile_expr(if_node[1], return_reg, local_vars, current_block_end)
        self.translate_to_instr(OpCode.CMP, OperandType.IMMEDIATE, return_reg, return_reg, 0)
        else_label = self.pc
        self.translate_to_instr(OpCode.BE, OperandType.PC_OFFSET, return_reg, return_reg)
        jmp_to_else = len(self.asm) - 1
        self.compile_expr(if_node[2], return_reg, local_vars, current_block_end)
        end_label = self.pc
        self.translate_to_instr(OpCode.JMP, OperandType.PC_OFFSET, return_reg, return_reg)
        jmp_end_label = len(self.asm) - 1
        self.asm[jmp_to_else] = (
            OpCode.BE, OperandType.PC_OFFSET, return_reg, return_reg, self.pc - else_label)

        if len(if_node) > 3:
            self.compile_expr(if_node[3], return_reg, local_vars, current_block_end)
        self.asm[jmp_end_label] = (
            OpCode.JMP, OperandType.PC_OFFSET, return_reg, return_reg, self.pc - end_label)

    def compile_let(self, let_node, register, local_vars={}, current_block_end=None):
        let_local_vars = local_vars.copy()

        self.translate_to_instr(OpCode.SUB, OperandType.IMMEDIATE, self.sp, self.sp, 4 * len(let_node[1]))
        for key in let_local_vars:
            let_local_vars[key] += 4 * len(let_node[1])
        offset = 0
        for var in let_node[1]:
            var_name = var[0]
            var_value = var[1]
            self.compile_expr(var_value, register, let_local_vars, current_block_end)
            self.translate_to_instr(OpCode.STORE, OperandType.INDIRECT_RIGHT, register, self.sp, offset)
            let_local_vars[var_name] = offset
            offset += 4

        for expr in let_node[2:]:
            self.compile_expr(expr, register, let_local_vars, current_block_end)

        self.translate_to_instr(OpCode.ADD, OperandType.IMMEDIATE, self.sp, self.sp, len(let_node[1]) * 4)

    def compile_setq(self, setq_node, register, local_vars={}, current_block_end=None):
        self.compile_expr(setq_node[2], register, local_vars, current_block_end)
        self.translate_to_instr(OpCode.STORE, OperandType.INDIRECT_RIGHT, register,
                                self.sp, local_vars[setq_node[1]])

    def compile_loop(self, loop_node, register, local_vars={}, current_block_end=None):
        loop_label = self.pc
        self.current_block_idx += 1
        while_name = "while" + str(self.current_block_idx)
        loop_name = "loop" + str(self.current_block_idx)
        if loop_node[1] == "while":
            self.compile_expr(loop_node[2], register, local_vars, while_name)
            self.translate_to_instr(OpCode.CMP, OperandType.IMMEDIATE, register, register, 0)
            self.labels[while_name] = self.pc
            self.translate_to_instr(OpCode.BE, OperandType.PC_OFFSET, register, register, while_name)
            self.labels_to_replace.append(len(self.asm) - 1)
            loop_node = loop_node[3:]
        for expr in loop_node[1:]:
            self.compile_expr(expr, register, local_vars, loop_name)
        self.translate_to_instr(OpCode.JMP, OperandType.PC_OFFSET, register,
                                register, loop_label - self.pc)
        if while_name in self.labels:
            self.labels[while_name] = self.pc - self.labels[while_name]
        self.labels[loop_name] = self.pc

    def replace_labels(self):
        for expr in self.static_data_addresses:
            self.static_data_addresses[expr] = self.pc + len(self.data)
            for ch in expr[1:-1]:
                self.data += bytearray([0, 0, 0, ord(ch)])
            self.data += bytearray([0, 0, 0, 0])
        for idx in self.labels_to_replace:
            if self.asm[idx][-1] in self.labels:
                self.asm[idx] = (
                    self.asm[idx][0], self.asm[idx][1], self.asm[idx][2], self.asm[idx][3],
                    self.labels[self.asm[idx][-1]])
            elif self.asm[idx][-1] in self.static_data_addresses:
                self.asm[idx] = (
                    self.asm[idx][0], self.asm[idx][1], self.asm[idx][2], self.asm[idx][3],
                    self.static_data_addresses[self.asm[idx][-1]])

    def compile_return(self, return_node, return_register, local_vars={}, current_block_end=None):
        self.compile_expr(return_node[1], return_register, local_vars, current_block_end)
        self.translate_to_instr(OpCode.JMP, OperandType.IMMEDIATE, return_register,
                                return_register, current_block_end)
        self.labels_to_replace.append(len(self.asm) - 1)

    def compile_defun(self, defun_node, return_reg, local_vars={}, current_block_end=None):
        name = defun_node[1]
        self.labels[name] = self.pc
        arguments = defun_node[2]
        body = defun_node[3]

        fun_local_vars = local_vars.copy()
        if len(arguments) > 0:
            offset = 0
            for arg in arguments:
                fun_local_vars[arg] = offset
                offset += 4

        self.compile_expr(body, RegisterBlock.Register.R0, fun_local_vars, name + "_end")
        self.labels[name + "_end"] = self.pc
        self.translate_to_instr(OpCode.RET, OperandType.REG2REG, self.return_register, self.return_register)

    def compile_call(self, call_node, return_reg, local_vars={}, current_block_end=None):
        func_name = call_node[0]
        args = call_node[1:]
        num_args = len(args)

        self.push_reg(self.return_register, local_vars)
        for i in range(num_args - 1, -1, -1):
            self.compile_expr(args[i], RegisterBlock.Register.R3, local_vars)
            self.translate_to_instr(OpCode.SUB, OperandType.IMMEDIATE, self.sp, self.sp, 4)
            for var in local_vars:
                local_vars[var] += 4
            self.translate_to_instr(OpCode.STORE, OperandType.INDIRECT_RIGHT, RegisterBlock.Register.R3, self.sp, 0)

        self.translate_to_instr(OpCode.MOV, OperandType.IMMEDIATE, RegisterBlock.Register.R2, RegisterBlock.Register.R2,
                                num_args)

        self.translate_to_instr(OpCode.CALL, OperandType.IMMEDIATE, self.return_register,
                                self.return_register, func_name)
        self.labels_to_replace.append(len(self.asm) - 1)

        if num_args > 0:
            self.translate_to_instr(OpCode.ADD, OperandType.IMMEDIATE, self.sp, self.sp, 4 * num_args)

        for var in local_vars:
            local_vars[var] -= 4 * num_args

        self.pop_reg(self.return_register, local_vars)

        if return_reg != RegisterBlock.Register.R0:
            self.translate_to_instr(OpCode.MOV, OperandType.REG2REG, return_reg, RegisterBlock.Register.R0)

    def compile_print(self):
        self.labels["print"] = self.pc
        result_reg = RegisterBlock.Register.R3
        self.translate_to_instr(OpCode.MOV, OperandType.INDIRECT_RIGHT, result_reg, self.sp, 0)

        self.translate_to_instr(OpCode.CMP, OperandType.IMMEDIATE, RegisterBlock.Register.R1, RegisterBlock.Register.R1,
                                0)
        self.translate_to_instr(OpCode.BNE, OperandType.PC_OFFSET, result_reg, result_reg,
                                14)
        self.translate_to_instr(OpCode.STORE_IMM, OperandType.IMMEDIATE, result_reg, result_reg, self.output_addr)
        self.translate_to_instr(OpCode.JMP, OperandType.PC_OFFSET, result_reg, result_reg, 34)

        self.translate_to_instr(OpCode.MOV, OperandType.INDIRECT_RIGHT, RegisterBlock.Register.R2, result_reg, 0)
        self.translate_to_instr(OpCode.CMP, OperandType.IMMEDIATE, RegisterBlock.Register.R2, RegisterBlock.Register.R2,
                                0)
        self.translate_to_instr(OpCode.BE, OperandType.PC_OFFSET, result_reg, result_reg, 20)

        self.translate_to_instr(OpCode.STORE_IMM, OperandType.IMMEDIATE, RegisterBlock.Register.R2,
                                RegisterBlock.Register.R2, self.output_addr)
        self.translate_to_instr(OpCode.ADD, OperandType.IMMEDIATE, result_reg, result_reg, 4)
        self.translate_to_instr(OpCode.JMP, OperandType.PC_OFFSET, result_reg, result_reg, -26)

        self.translate_to_instr(OpCode.RET, OperandType.REG2REG, self.return_register, self.return_register)

    def compile_string(self, expr, return_reg):
        if expr not in self.static_data_addresses:
            self.static_data_addresses[expr] = 0

        self.translate_to_instr(OpCode.MOV, OperandType.IMMEDIATE, return_reg, return_reg,
                                expr)
        self.labels_to_replace.append(len(self.asm) - 1)
        self.translate_to_instr(OpCode.MOV, OperandType.IMMEDIATE, RegisterBlock.Register.R1, RegisterBlock.Register.R1,
                                1)

    def compile_concat(self):
        self.labels["concat"] = self.pc
        self.translate_to_instr(OpCode.MOV, OperandType.REG2REG, RegisterBlock.Register.R0, self.heap_pointer)
        result_reg = RegisterBlock.Register.R3

        for offset in [0, 4]:
            self.translate_to_instr(OpCode.MOV, OperandType.INDIRECT_RIGHT, result_reg, self.sp, offset)
            self.translate_to_instr(OpCode.MOV, OperandType.INDIRECT_RIGHT, RegisterBlock.Register.R2, result_reg, 0)
            self.translate_to_instr(OpCode.CMP, OperandType.IMMEDIATE, RegisterBlock.Register.R2,
                                    RegisterBlock.Register.R2,
                                    0)
            self.translate_to_instr(OpCode.BE, OperandType.PC_OFFSET, result_reg, result_reg, 24)

            self.translate_to_instr(OpCode.STORE, OperandType.INDIRECT_RIGHT, RegisterBlock.Register.R2,
                                    self.heap_pointer, 0)
            self.translate_to_instr(OpCode.ADD, OperandType.IMMEDIATE, result_reg, result_reg, 4)
            self.translate_to_instr(OpCode.ADD, OperandType.IMMEDIATE, self.heap_pointer, self.heap_pointer, 4)
            self.translate_to_instr(OpCode.JMP, OperandType.PC_OFFSET, result_reg, result_reg, -30)

        self.translate_to_instr(OpCode.STORE, OperandType.INDIRECT_RIGHT, RegisterBlock.Register.R2,
                                self.heap_pointer, 0)
        self.translate_to_instr(OpCode.ADD, OperandType.IMMEDIATE, self.heap_pointer, self.heap_pointer,
                                4)
        self.translate_to_instr(OpCode.RET, OperandType.REG2REG, self.return_register, self.return_register)

    def compile_list(self):
        self.labels["list"] = self.pc
        self.current_block_idx += 1
        self.translate_to_instr(OpCode.MOV, OperandType.REG2REG, RegisterBlock.Register.R0, self.heap_pointer)
        cur_el_address = RegisterBlock.Register.R3
        el = RegisterBlock.Register.R5
        loop_counter = RegisterBlock.Register.R4

        self.translate_to_instr(OpCode.MOV, OperandType.REG2REG, loop_counter, RegisterBlock.Register.R2)
        self.translate_to_instr(OpCode.MOV, OperandType.REG2REG, cur_el_address, self.sp)

        cycle_pointer = self.pc
        self.translate_to_instr(OpCode.CMP, OperandType.IMMEDIATE, loop_counter, loop_counter, 0)
        self.labels["list_end" + str(self.current_block_idx)] = self.pc
        self.translate_to_instr(OpCode.BE, OperandType.PC_OFFSET, loop_counter, loop_counter,
                                "list_end" + str(self.current_block_idx))
        self.labels_to_replace.append(len(self.asm) - 1)

        self.translate_to_instr(OpCode.MOV, OperandType.INDIRECT_RIGHT, el, cur_el_address, 0)
        self.translate_to_instr(OpCode.STORE, OperandType.INDIRECT_RIGHT, el,
                                self.heap_pointer, 0)
        self.translate_to_instr(OpCode.ADD, OperandType.IMMEDIATE, self.heap_pointer, self.heap_pointer, 8)
        self.translate_to_instr(OpCode.STORE, OperandType.INDIRECT_RIGHT, self.heap_pointer,
                                self.heap_pointer, -4)
        self.translate_to_instr(OpCode.ADD, OperandType.IMMEDIATE, cur_el_address, cur_el_address, 4)

        self.translate_to_instr(OpCode.SUB, OperandType.IMMEDIATE, loop_counter, loop_counter, 1)
        self.translate_to_instr(OpCode.JMP, OperandType.PC_OFFSET, loop_counter, loop_counter, cycle_pointer - self.pc)

        self.labels["list_end" + str(self.current_block_idx)] = self.pc - self.labels[
            "list_end" + str(self.current_block_idx)]
        self.translate_to_instr(OpCode.MOV, OperandType.IMMEDIATE, el, el, 0)
        self.translate_to_instr(OpCode.STORE, OperandType.INDIRECT_RIGHT, el,
                                self.heap_pointer, -4)
        self.translate_to_instr(OpCode.RET, OperandType.REG2REG, self.return_register, self.return_register)

    def compile_empty_list(self, return_reg):
        self.translate_to_instr(OpCode.MOV, OperandType.IMMEDIATE, return_reg, return_reg, 0)

    def compile_car(self):
        self.labels["car"] = self.pc
        self.translate_to_instr(OpCode.MOV, OperandType.INDIRECT_RIGHT,
                                RegisterBlock.Register.R0, self.sp, 0)
        self.translate_to_instr(OpCode.MOV, OperandType.INDIRECT_RIGHT,
                                RegisterBlock.Register.R0, RegisterBlock.Register.R0, 0)
        self.translate_to_instr(OpCode.MOV, OperandType.IMMEDIATE,
                                RegisterBlock.Register.R1, RegisterBlock.Register.R1, 0)
        self.translate_to_instr(OpCode.RET, OperandType.REG2REG,
                                self.return_register, self.return_register)

    def compile_cdr(self):
        self.labels["cdr"] = self.pc
        self.translate_to_instr(OpCode.MOV, OperandType.INDIRECT_RIGHT,
                                RegisterBlock.Register.R0, self.sp, 0)

        self.translate_to_instr(OpCode.MOV, OperandType.INDIRECT_RIGHT,
                                RegisterBlock.Register.R0, RegisterBlock.Register.R0, 4)
        self.translate_to_instr(OpCode.RET, OperandType.REG2REG,
                                self.return_register, self.return_register)

    def compile_null(self):
        self.labels["null"] = self.pc
        self.translate_to_instr(OpCode.MOV, OperandType.INDIRECT_RIGHT,
                                RegisterBlock.Register.R0, self.sp, 0)
        self.translate_to_instr(OpCode.CMP, OperandType.IMMEDIATE,
                                RegisterBlock.Register.R0, RegisterBlock.Register.R0, 0)
        self.translate_to_instr(OpCode.MOV, OperandType.IMMEDIATE,
                                RegisterBlock.Register.R0, RegisterBlock.Register.R0, 0)
        self.translate_to_instr(OpCode.BNE, OperandType.PC_OFFSET,
                                RegisterBlock.Register.R0, RegisterBlock.Register.R0, 10)
        self.translate_to_instr(OpCode.MOV, OperandType.IMMEDIATE,
                                RegisterBlock.Register.R0, RegisterBlock.Register.R0, 1)
        self.translate_to_instr(OpCode.RET, OperandType.REG2REG,
                                self.return_register, self.return_register)

    def compile_push(self):
        self.labels["push"] = self.pc

        self.translate_to_instr(OpCode.MOV, OperandType.INDIRECT_RIGHT,
                                RegisterBlock.Register.R2, self.sp, 0)
        self.translate_to_instr(OpCode.MOV, OperandType.INDIRECT_RIGHT,
                                RegisterBlock.Register.R3, self.sp, 4)

        self.translate_to_instr(OpCode.STORE, OperandType.INDIRECT_RIGHT,
                                RegisterBlock.Register.R2, self.heap_pointer, 0)
        self.translate_to_instr(OpCode.STORE, OperandType.INDIRECT_RIGHT,
                                RegisterBlock.Register.R3, self.heap_pointer, 4)

        self.translate_to_instr(OpCode.MOV, OperandType.REG2REG,
                                RegisterBlock.Register.R0, self.heap_pointer)

        self.translate_to_instr(OpCode.ADD, OperandType.IMMEDIATE,
                                self.heap_pointer, self.heap_pointer, 8)

        self.translate_to_instr(OpCode.RET, OperandType.REG2REG,
                                self.return_register, self.return_register)

    def compile_nreverse(self):

        self.labels["nreverse"] = self.pc
        self.translate_to_instr(OpCode.MOV, OperandType.IMMEDIATE,
                                RegisterBlock.Register.R1, RegisterBlock.Register.R1, 0)
        self.translate_to_instr(OpCode.MOV, OperandType.INDIRECT_RIGHT,
                                RegisterBlock.Register.R2, self.sp, 0)

        loop_start = self.pc

        self.translate_to_instr(OpCode.CMP, OperandType.IMMEDIATE,
                                RegisterBlock.Register.R2, RegisterBlock.Register.R2, 0)
        self.translate_to_instr(OpCode.BE, OperandType.PC_OFFSET,
                                RegisterBlock.Register.R0, RegisterBlock.Register.R0, 20)

        self.translate_to_instr(OpCode.MOV, OperandType.INDIRECT_RIGHT,
                                RegisterBlock.Register.R3, RegisterBlock.Register.R2, 4)

        self.translate_to_instr(OpCode.STORE, OperandType.INDIRECT_RIGHT,
                                RegisterBlock.Register.R1, RegisterBlock.Register.R2, 4)

        self.translate_to_instr(OpCode.MOV, OperandType.REG2REG,
                                RegisterBlock.Register.R1, RegisterBlock.Register.R2)

        self.translate_to_instr(OpCode.MOV, OperandType.REG2REG,
                                RegisterBlock.Register.R2, RegisterBlock.Register.R3)

        self.translate_to_instr(OpCode.JMP, OperandType.PC_OFFSET,
                                RegisterBlock.Register.R0, RegisterBlock.Register.R0, loop_start - self.pc)

        self.translate_to_instr(OpCode.MOV, OperandType.REG2REG,
                                RegisterBlock.Register.R0, RegisterBlock.Register.R1)
        self.translate_to_instr(OpCode.MOV, OperandType.IMMEDIATE,
                                RegisterBlock.Register.R1, RegisterBlock.Register.R1, 0)
        self.translate_to_instr(OpCode.RET, OperandType.REG2REG,
                                self.return_register, self.return_register)

    def compile_read(self):
        self.labels["read"] = self.pc
        return_reg = RegisterBlock.Register.R0
        self.translate_to_instr(OpCode.MOV, OperandType.IMMEDIATE, RegisterBlock.Register.R2, RegisterBlock.Register.R2,
                                self.input_addr)
        self.translate_to_instr(OpCode.MOV, OperandType.INDIRECT_RIGHT, return_reg, RegisterBlock.Register.R2, 0)
        self.translate_to_instr(OpCode.MOV, OperandType.IMMEDIATE, RegisterBlock.Register.R1, RegisterBlock.Register.R1,
                                0)
        self.translate_to_instr(OpCode.RET, OperandType.REG2REG, self.return_register, self.return_register)

    def compile_input_string(self):
        self.labels["input-string"] = self.pc
        return_reg = RegisterBlock.Register.R3
        self.translate_to_instr(OpCode.MOV, OperandType.REG2REG, RegisterBlock.Register.R0, self.heap_pointer)
        self.translate_to_instr(OpCode.MOV, OperandType.IMMEDIATE, RegisterBlock.Register.R2, RegisterBlock.Register.R2,
                                self.input_addr)

        cycle_start = self.pc
        self.translate_to_instr(OpCode.MOV, OperandType.INDIRECT_RIGHT, return_reg, RegisterBlock.Register.R2, 0)
        self.translate_to_instr(OpCode.STORE, OperandType.INDIRECT_RIGHT, return_reg, self.heap_pointer, 0)
        self.translate_to_instr(OpCode.ADD, OperandType.IMMEDIATE, self.heap_pointer, self.heap_pointer, 4)
        self.translate_to_instr(OpCode.CMP, OperandType.IMMEDIATE, return_reg, return_reg, 0)
        self.translate_to_instr(OpCode.BE, OperandType.PC_OFFSET, return_reg, return_reg, 8)
        self.translate_to_instr(OpCode.JMP, OperandType.PC_OFFSET, return_reg, return_reg, cycle_start - self.pc)

        self.translate_to_instr(OpCode.MOV, OperandType.IMMEDIATE, RegisterBlock.Register.R1, RegisterBlock.Register.R1,
                                1)

        self.translate_to_instr(OpCode.RET, OperandType.REG2REG, self.return_register, self.return_register)


def main(source, target):
    with open(source, encoding="utf-8") as f:
        source = f.read()

    compiler = LispCompiler(source)
    compiler.compile()
    binary_code = compiler.to_bytes()
    hex_code = compiler.to_hex()

    os.makedirs(os.path.dirname(os.path.abspath(target)) or ".", exist_ok=True)

    if target.endswith(".bin"):
        with open(target, "wb") as f:
            f.write(binary_code)
        with open(target + ".hex", "w") as f:
            f.write(hex_code)
    print("instructions: " + str(len(compiler.asm)))


if __name__ == "__main__":
    assert len(sys.argv) == 3, "Wrong arguments: translator.py <input_file> <target_file>"
    _, source, target = sys.argv
    main(source, target)
