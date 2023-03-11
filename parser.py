from lexer import tokenize, TokenType, Token

class NumberExpression:
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return f"[Number {self.value}]"

class VariableExpression:
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return f"[Variable {self.name}]"

class BinaryExpression:
    def __init__(self, left, operator, right):
        self.left = left
        self.operator = operator
        self.right = right

    def __repr__(self):
        return f"[Binary({self.operator}) {self.left} {self.right}]"

class CallExpression:
    def __init__(self, callee, args):
        self.callee = callee
        self.args = args

    def __repr__(self):
        args_str = ""
        for arg in self.args:
            args_str += f"{arg} "
        args_str = args_str[:-1]
        return f"[Call({self.callee}) {args_str}]"

class UnaryExpression:
    def __init__(self, operator, operand):
        self.operator = operator
        self.operand = operand

    def __repr__(self):
        return f"[Unary({self.operator}) {self.operand}]"

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.index = 0
        self.current_token = self.tokens[0]
    
    def advance(self):
        self.index += 1
        if self.index < len(self.tokens):
            self.current_token = self.tokens[self.index]
    
    def parse(self):
        top_level_exprs = []
        while self.current_token.type != TokenType.EOF:
            top_level_exprs.append(self.parse_expression())
            if self.current_token.type != TokenType.SEMICOLON:
               raise Exception(f"Expected ; but got {self.current_token}")
            self.advance()
        return top_level_exprs
    
    def parse_expression(self):
        return self.parse_term()
    
    def parse_term(self):
        lhs = self.parse_factor()
        while self.current_token.type == TokenType.OPERATOR and self.current_token["operator"] in ["+", "-"]:
            operator = self.current_token["operator"]
            self.advance()
            rhs = self.parse_factor()
            lhs = BinaryExpression(lhs, operator, rhs)
        return lhs
    
    def parse_factor(self):
        lhs = self.parse_unary()
        while self.current_token.type == TokenType.OPERATOR and self.current_token["operator"] in ["*"]:
            operator = self.current_token["operator"]
            self.advance()
            rhs = self.parse_unary()
            lhs = BinaryExpression(lhs, operator, rhs)
        return lhs
    
    def parse_unary(self):
        token = self.current_token
        if token.type == TokenType.OPERATOR and token["operator"] in ["+", "-"]:
            self.advance()
            return UnaryExpression(token["operator"], self.parse_unary())
        return self.parse_primary()
    
    def parse_primary(self):
        token = self.current_token
        if token.type == TokenType.NUMBER:
            self.advance()
            return NumberExpression(token["value"])
        if token.type == TokenType.IDENTIFIER:
            self.advance()
            if self.current_token.type != TokenType.LPAR:
                return VariableExpression(token["name"])
            #Function call
            self.advance()
            args = []
            while self.current_token.type != TokenType.RPAR:
                args.append(self.parse_expression())
                if self.current_token.type == TokenType.COMMA:
                    self.advance()
            self.advance()
            return CallExpression(token["name"], args)
        if token.type == TokenType.LPAR:
            self.advance()
            expr = self.parse_expression()
            if self.current_token.type != TokenType.RPAR:
                raise Exception("Expected )")
            self.advance()
            return expr

code = input("Ready > ")

tokens = tokenize(code)
ast = Parser(tokens).parse()

ast_str = "[Program "
for expr in ast:
    ast_str += str(expr) + " "
ast_str = ast_str[:-1] + "]"


print(ast_str)
