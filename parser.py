from lexer import tokenize, TokenType, Token

#############################################
# Classes that are building blocks of the AST
#############################################

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

#############################################
# Parser
#############################################

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
            # Top level expressions are separated by semicolons
            if self.current_token.type != TokenType.SEMICOLON:
               raise Exception(f"Expected ; but got {self.current_token}")
            self.advance() # Advance past the semicolon
        return top_level_exprs
    
    def parse_expression(self):
        return self.parse_term()
    
    def parse_term(self):
        lhs = self.parse_factor() #Get the first factor

        # If there is no operator then this term is just a factor otherwise we need to parse the rest of the term
        while self.current_token.type == TokenType.OPERATOR and self.current_token["operator"] in ["+", "-"]:
            operator = self.current_token["operator"]
            self.advance()
            rhs = self.parse_factor()
            lhs = BinaryExpression(lhs, operator, rhs)
            #If the loop continues beyond 1 iteration then the next factor 
            # will be added/subtracted with the previous result: ((x + x) + x) + x
        return lhs
    
    # TODO: Fix code duplication between parse_term and parse_factor
    def parse_factor(self):
        lhs = self.parse_unary() # Get the first unary

        # If there is no operator then this factor is just a unary otherwise we need to parse the rest of the factor
        while self.current_token.type == TokenType.OPERATOR and self.current_token["operator"] in ["*"]:
            operator = self.current_token["operator"]
            self.advance()
            rhs = self.parse_unary()
            lhs = BinaryExpression(lhs, operator, rhs)
            #If the loop continues beyond 1 iteration then the next unary
            # will be multiplied by the previous result: ((x * x) * x) * x
        return lhs
    
    def parse_unary(self):
        token = self.current_token # Get the current token which should be either a valid unary operator or a primary
        if token.type == TokenType.OPERATOR and token["operator"] in ["+", "-"]:
            self.advance()
            #If it was a unary operator then parse the next unary which is the operand 
            return UnaryExpression(token["operator"], self.parse_unary())
        return self.parse_primary() #If it wasn't a unary operator then it must be a primary
    
    def parse_primary(self):
        token = self.current_token
        if token.type == TokenType.NUMBER:
            self.advance()
            return NumberExpression(token["value"])
        if token.type == TokenType.IDENTIFIER:
            self.advance()
            #If the next token is not a ( then it is a variable
            if self.current_token.type != TokenType.LPAR:
                return VariableExpression(token["name"])
            #If it is a ( then it is a function call: "foo(" and we need to parse the arguments
            self.advance()
            args = []
            while self.current_token.type != TokenType.RPAR:
                #Arguments can be full expressions
                args.append(self.parse_expression())
                if self.current_token.type == TokenType.COMMA:
                    #If there is a comma then we need to advance past it, if the arguments are separated by spaces
                    #then we don't want to advance. To remove the ability to separate arguments by spaces we can 
                    #Report an error if the next token is not a comma instead
                    self.advance()
            self.advance() #Advance past the )
            return CallExpression(token["name"], args)
        if token.type == TokenType.LPAR:
            # Parenthesized expression take precedence and are therefore primary
            self.advance() #Advance past the (
            expr = self.parse_expression()
            if self.current_token.type != TokenType.RPAR:
                raise Exception("Expected )")
            self.advance() #Advance past the )
            return expr

code = input("Ready > ")

tokens = tokenize(code)
ast = Parser(tokens).parse()

ast_str = "[Program "
for expr in ast:
    ast_str += str(expr) + " "
ast_str = ast_str[:-1] + "]"


print(ast_str)
