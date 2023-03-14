from lexer import tokenize, TokenType, Token

#############################################
# Classes that are building blocks of the AST
#############################################

class NumberNode:
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        node_dict = {
            "type": "Number",
            "value": self.value
        }
        return str(node_dict)

class VariableNode: #Variable reference
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        node_dict = {
            "type": "Variable",
            "name": self.name
        }
        return str(node_dict)

class BinaryNode:
    def __init__(self, left, operator, right):
        self.left = left
        self.operator = operator
        self.right = right

    def __repr__(self):
        node_dict = {
            "type": "Binary",
            "operator": self.operator,
            "left": self.left,
            "right": self.right
        }
        return str(node_dict)

class CallNode:
    def __init__(self, callee, args):
        self.callee = callee
        self.args = args

    def __repr__(self):
        node_dict = {
            "type": "Call",
            "callee": self.callee,
            "args": self.args
        }
        return str(node_dict)

class UnaryNode:
    def __init__(self, operator, operand):
        self.operator = operator
        self.operand = operand

    def __repr__(self):
        node_dict = {
            "type": "Unary",
            "operator": self.operator,
            "operand": self.operand
        }
        return str(node_dict)

class PrototypeNode:
    def __init__(self, name, args, ret_type):
        self.name = name
        self.args = args
        self.ret_type = ret_type
    
    def __repr__(self):
        node_dict = {
            "type": "Prototype",
            "name": self.name,
            "args": self.args,
            "ret_type": self.ret_type
        }
        return str(node_dict)
        args_str = ""
        for arg in self.args:
            args_str += f"{arg} "
        args_str = args_str[:-1]

class FunctionNode:
    def __init__(self, prototype, body):
        self.prototype = prototype
        self.body = body

    def __repr__(self):
        node_dict = {
            "type": "Function",
            "prototype": self.prototype,
            "body": self.body
        }
        return str(node_dict)

class ReturnNode:
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        node_dict = {
            "type": "Return",
            "value": self.value
        }
        return str(node_dict)

class LetNode:
    def __init__(self, name, value):
        self.name = name
        self.value = value

    def __repr__(self):
        node_dict = {
            "type": "Let",
            "name": self.name,
            "value": self.value
        }
        return str(node_dict)

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
    
    def peek(self, n=1):
        if self.index + n < len(self.tokens):
            return self.tokens[self.index + n]
        return None

    def parse(self):
        #The term statement is used loosely here. It can be a function declaration, function definition, or expression
        statements = []
        while self.current_token.type != TokenType.EOF:
            if self.current_token.type == TokenType.DEC:
                statements.append(self.parse_function_declaration())
            elif self.current_token.type == TokenType.FN or self.current_token.type == TokenType.ACTN:
                statements.append(self.parse_function_definition())
            else:
                raise Exception(f"Top level expressions are not allowed")
                #statements.append(self.parse_expression())
            # Statements are separated by semicolons
            if self.current_token.type != TokenType.SEMICOLON:
               raise Exception(f"Expected ; but got {self.current_token}")
            self.advance() # Advance past the semicolon
        return statements

    def parse_expression(self):
        return self.parse_comparison()
    
    def parse_comparison(self):
        return self.parse_binary_expression(self.parse_term, ["<", ">", "="])

    def parse_term(self):
        return self.parse_binary_expression(self.parse_factor, ["+", "-"])
    
    def parse_factor(self):
        return self.parse_binary_expression(self.parse_unary, ["*"])
    
    def parse_binary_expression(self, operand_parse_fn, operators):
        lhs = operand_parse_fn() # Get the first operand

        # If there is no operator then this factor is just a unary otherwise we need to parse the rest of the factor
        while self.current_token.type == TokenType.OPERATOR and self.current_token["operator"] in operators:
            operator = self.current_token["operator"]
            self.advance()
            rhs = operand_parse_fn()
            lhs = BinaryNode(lhs, operator, rhs)
            #If the loop continues beyond 1 iteration then the next unary
            # will be multiplied by the previous result: ((x * x) * x) * x
        return lhs
    
    def parse_unary(self):
        token = self.current_token # Get the current token which should be either a valid unary operator or a primary
        if token.type == TokenType.OPERATOR and token["operator"] in ["+", "-"]:
            self.advance()
            #If it was a unary operator then parse the next unary which is the operand 
            return UnaryNode(token["operator"], self.parse_unary())
        return self.parse_primary() #If it wasn't a unary operator then it must be a primary
    
    def parse_primary(self):
        token = self.current_token
        if token.type == TokenType.NUMBER:
            self.advance()
            return NumberNode(token["value"])
        if token.type == TokenType.IDENTIFIER:
            self.advance() #TODO: DO A PEEK HERE INSTEAD AND DONT SEND TOKEN TO FUNCTION CALL
            #If the next token is not a ( then it is a variable
            if self.current_token.type != TokenType.LPAR:
                return VariableNode(token["name"])
            #If it is a ( then it is a function call: eg. "foo(" and we need to handle it as such
            return self.parse_function_call(token)
        if token.type == TokenType.LPAR:
            # Parenthesized expression take precedence and are therefore primary
            self.advance() #Advance past the (
            expr = self.parse_expression()
            if self.current_token.type != TokenType.RPAR:
                raise Exception("Expected )")
            self.advance() #Advance past the )
            return expr
        
        raise Exception(f"Expected primary but got {token}")
    
    def parse_function_call(self, identifier_tok):
        self.advance() #Advance past the (
        args = []
        while self.current_token.type != TokenType.RPAR:
            #Arguments can be full expressions
            args.append(self.parse_expression())
            #Arguments are separated by commas, if the next token is not a comma then it must be a )
            #if it is something else then it is an error
            if self.current_token.type != TokenType.COMMA and self.current_token.type != TokenType.RPAR:
                raise Exception(f"Expected ',' but got {self.current_token}")

            #We don't want to advance past the ) so we only advance if the next token is a comma
            #The loop will end when the next token is a ) and advance past it 
            if self.current_token.type == TokenType.COMMA:
                self.advance()
        self.advance() #Advance past the )
        return CallNode(identifier_tok["name"], args)
    
    def parse_function_declaration(self):
        non_returning = False
        if(self.current_token.type == TokenType.ACTN):
            non_returning = True
        self.advance() #Advance past the dec/fn/act keyword
        if(self.current_token.type == TokenType.ACTN): #This second check is for the case "dec act"
            non_returning = True
            self.advance() #Advance past the act keyword
        if self.current_token.type != TokenType.IDENTIFIER:
            raise Exception("Expected identifier after dec/fn/act")
        fn_name = self.current_token["name"]
        self.advance() #Advance past the function name
        if self.current_token.type != TokenType.LPAR:
            raise Exception("Expected ( after function name")
        self.advance() #Advance past the (
        args = []
        while self.current_token.type != TokenType.RPAR:
            if self.current_token.type != TokenType.IDENTIFIER:
                raise Exception("Expected identifier after (")
            args.append(self.current_token["name"])
            self.advance() #Advance past the argument name
            if self.current_token.type != TokenType.COMMA and self.current_token.type != TokenType.RPAR:
                raise Exception(f"Expected ',' but got {self.current_token}")
            
            #Advancing past the ')' is handled after the loop so we only advance if the token is a comma
            if self.current_token.type == TokenType.COMMA:
                self.advance()
        self.advance() #Advance past the )
        return PrototypeNode(fn_name, args, "void" if non_returning else "double")
    
    def parse_function_definition(self):
        fn_prototype = self.parse_function_declaration()
        if self.current_token.type != TokenType.LBRACE:
            raise Exception("Expected { after function prototype")
        return FunctionNode(fn_prototype, self.parse_expression_block())
    
    def parse_assignment(self):
        variable = self.parse_primary() #The left side of the assignment is a variable which is a primary (identfier)
        if not isinstance(variable, VariableNode):
            raise Exception("Left side of assignment must be a variable")
        if self.current_token.type != TokenType.OPERATOR or self.current_token["operator"] != ":=":
            raise Exception(f"Expected := after variable name in assignment but got {self.current_token}")
        self.advance() #Advance past the #
        return BinaryNode(variable, ":=", self.parse_expression())

    def parse_let(self):
        self.advance() #Advance past the let keyword
        if self.current_token.type != TokenType.IDENTIFIER:
            raise Exception("Expected identifier after let")
        variable = self.current_token["name"]
        self.advance() #Advance past the variable name
        if self.current_token.type != TokenType.OPERATOR or self.current_token["operator"] != ":=":
            raise Exception(f"Expected := after variable name in assignment but got {self.current_token}")
        self.advance() #Advance past the #
        return LetNode(variable, self.parse_expression())

    def parse_expression_block(self):
        self.advance() #Advance past the {
        expressions = []
        while self.current_token.type != TokenType.RBRACE:
            if self.current_token.type == TokenType.RET:
                expressions.append(self.parse_return())
            elif self.current_token.type == TokenType.IDENTIFIER and self.peek().type == TokenType.OPERATOR and self.peek()["operator"] == ":=":
                expressions.append(self.parse_assignment())
            elif self.current_token.type == TokenType.LET:
                expressions.append(self.parse_let())
            else:
                expressions.append(self.parse_expression())
            if self.current_token.type != TokenType.SEMICOLON:
                raise Exception(f"Expected ; after expression but got {self.current_token}")
            self.advance() #Advance past the ;
        self.advance() #Advance past the }
        return expressions

    def parse_return(self):
        self.advance() # Advance past the ret keyword
        return ReturnNode(self.parse_expression())

import sys

if(len(sys.argv) != 2):
    print("Usage: python3 parser.py <filename>")
    exit()

code = open(sys.argv[1], "r").read()

tokens = tokenize(code)
ast = Parser(tokens).parse()

# Generate an AST string for the codegen
ast_str = "["
for expr in ast:
    ast_str += str(expr) + ",\n"
ast_str = ast_str[:-2] + "]"

f = open("ast", "w")
f.write(ast_str.replace("\'", "\""))
f.close()
