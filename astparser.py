import re
from enum import Enum
from collections import namedtuple


class TokenType(Enum):
    LPAREN = 1
    RPAREN = 2
    SYMBOL = 3
    NUMBER = 4
    STRING = 5
    BOOLEAN = 6
    QUOTE = 7


Token = namedtuple('Token', ['type', 'value'])


class Parser:
    def __init__(self):
        self.tokens = []
        self.current = 0
        self.ast = []

    def tokenize(self, code):
        token_specs = [
            (r'\(', self.handle_lparen),
            (r'\)', self.handle_rparen),
            (r'"([^"]*)"', self.handle_string),
            (r'-?\d+', self.handle_number),
            (r'T|Nil', self.handle_boolean),
            (r"'", self.handle_quote),
            (r'[^\s()",]+', self.handle_symbol),
            (r'\s+', self.handle_whitespace),
            (r';[^\n]*', self.handle_comment)
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
                pos += 1  # Пропустить нераспознанный символ

        return tokens

    def handle_lparen(self, match):
        return Token(TokenType.LPAREN, '(')

    def handle_rparen(self, match):
        return Token(TokenType.RPAREN, ')')

    def handle_string(self, match):
        return Token(TokenType.STRING, match.group(1))

    def handle_number(self, match):
        return Token(TokenType.NUMBER, int(match.group()))

    def handle_boolean(self, match):
        return Token(TokenType.BOOLEAN, match.group() == 'T')

    def handle_quote(self, match):
        return Token(TokenType.QUOTE, "'")

    def handle_symbol(self, match):
        return Token(TokenType.SYMBOL, match.group())

    def handle_whitespace(self, match):
        return None

    def handle_comment(self, match):
        return None

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
        elif token.type == TokenType.QUOTE:
            return ['quote', self.parse_expression()]
        elif token.type == TokenType.NUMBER:
            return token.value
        elif token.type == TokenType.STRING:
            return token.value
        elif token.type == TokenType.BOOLEAN:
            return token.value
        elif token.type == TokenType.SYMBOL:
            return token.value
        else:
            raise SyntaxError(f"Unexpected token {token.type}")

    def parse_list(self):
        elements = []
        while self.current < len(self.tokens) and self.tokens[self.current].type != TokenType.RPAREN:
            elements.append(self.parse_expression())

        if self.current < len(self.tokens) and self.tokens[self.current].type == TokenType.RPAREN:
            self.current += 1
        else:
            raise SyntaxError("Expected ')'")

        return elements


if __name__ == "__main__":
    lisp_code = """

(defun is-palindrom (num)
    (let ((reversed-num 0)
         (original num))
        (loop while (> num 0)
              do (setq reversed-num (+ (* reversed-num 10) (mod num 10)))
                  (setq num (floor (/ num 10))))
         (if (= reversed-num original) T Nil)))

(defun is-palindrom-str (num)
    (let ((s (write-to-string num)))
         (string= s (reverse s))))
(let ((max-palindrom 0)) 
     (loop for x from 100 to 1000
      do (loop for y from 100 to 1000
               when (is-palindrom-str (* x y))
                 do (if (> (* x y) max-palindrom) 
                        (setq max-palindrom (* x y) ))
               ))
     (print max-palindrom))

(print (is-palindrom-str 313))


(defun mult-until-zero ()
    (let ((input 1)
          (mul 1))
         (loop 
             (setq input (parse-integer (read-line)))
             (when (= input 0) (return mul))
             (setq mul (* mul input)))))

(defun find-min(lst)
    (let ((min 1000000000000000))
         (loop
             (when (null lst) (return min))
             (if (> min (car lst)) (setq min (car lst)))
             (setq lst (cdr lst)))))


(defun merge_(lst1 lst2)
    (let ((res ()))
         (loop while (not (or (null lst1) (null lst2)))
               do (cond 
                      ((< (car lst1) (car lst2)) (setq res (append res (list (car lst1)))) (setq lst1 (cdr lst1)))
                      (t (setq res (append res (list (car lst2)))) (setq lst2 (cdr lst2))) 
                      ))
         (loop while (not (null lst1))
               do (setq res (append res (list (car lst1)))) (setq lst1 (cdr lst1)))
         (loop while (not (null lst2))
               do (setq res (append res (list (car lst2)))) (setq lst2 (cdr lst2)))
         res))

(defun merge-sort (lst)
    (cond
        ((null lst) lst)
        ((= 1 (list-length lst)) lst)
        (t (merge_ (merge-sort (subseq lst 0 (ceiling (/ (list-length lst) 2)))) (merge-sort (subseq lst (ceiling (/ (list-length lst) 2))))))))
             

(print (mult-until-zero))


(defun cat ()
  (loop for line = (read-line *standard-input* nil nil)
        while line
        do (write-line line *standard-output*)))


(print "hello world")

(print "What is your name?")
(let ((name (read-line)))
  (print (concatenate 'string "Hello, " name "!")))     
    """

    parser = Parser()
    ast = parser.parse(lisp_code)
    import pprint

    pp = pprint.PrettyPrinter(indent=2)
    pp.pprint(ast)
