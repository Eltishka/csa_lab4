import re
from collections import namedtuple
from enum import Enum


class TokenType(Enum):
    LPAREN = 1
    RPAREN = 2
    SYMBOL = 3
    NUMBER = 4
    STRING = 5


Token = namedtuple("Token", ["type", "value"])


class Parser:
    def __init__(self):
        self.tokens = []
        self.current = 0
        self.ast = []

    def tokenize(self, code):
        token_specs = [
            (r"\(", self.handle_lparen),
            (r"\)", self.handle_rparen),
            (r'"([^"]*)"', self.handle_string),
            (r"-?\d+", self.handle_number),
            (r'[^\s()",]+', self.handle_symbol),
            (r"\s+", lambda x: None),
            (r";[^\n]*", lambda x: None)
        ]

        tokens = []
        pos = 0
        length = len(code)

        while pos < length:
            match = None
            for pattern, handler in token_specs:
                regex = re.compile(pattern)
                match = regex.match(code, pos)
                if match:
                    token = handler(match)
                    if token:
                        tokens.append(token)
                    pos = match.end()
                    break
            if not match:
                pos += 1

        return tokens

    def handle_lparen(self, match):
        return Token(TokenType.LPAREN, "(")

    def handle_rparen(self, match):
        return Token(TokenType.RPAREN, ")")

    def handle_string(self, match):
        return Token(TokenType.STRING, match.group(1))

    def handle_number(self, match):
        return Token(TokenType.NUMBER, int(match.group()))

    def handle_symbol(self, match):
        return Token(TokenType.SYMBOL, match.group())

    def parse(self, code):
        self.tokens = self.tokenize(code)
        self.current = 0
        self.ast = []

        while self.current < len(self.tokens):
            self.ast.append(self.parse_expression())

        return self.ast

    def parse_expression(self):
        token = self.tokens[self.current]
        self.current += 1
        if token.type == TokenType.LPAREN:
            return self.parse_list()
        if token.type == TokenType.STRING:
            return '"' + token.value + '"'

        return token.value

    def parse_list(self):
        elements = []
        while self.current < len(self.tokens) and self.tokens[self.current].type != TokenType.RPAREN:
            elements.append(self.parse_expression())

        if self.current < len(self.tokens) and self.tokens[self.current].type == TokenType.RPAREN:
            self.current += 1

        return elements

