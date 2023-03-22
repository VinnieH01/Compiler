from lexer import Lexer, TokenType, Token
import json

#############################################
# Classes that are building blocks of the AST
#############################################

class LiteralNode:
    def __init__(self, value, data_type):
        self.value = value
        self.data_type = data_type

    def __repr__(self):
        node_dict = {
            "type": "Literal",
            "data_type": self.data_type,
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
    def __init__(self, name, arg_types, args, ret_type):
        self.name = name
        self.arg_types = arg_types
        self.args = args
        self.ret_type = ret_type
    
    def __repr__(self):
        node_dict = {
            "type": "Prototype",
            "name": self.name,
            "arg_types": self.arg_types,
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
    def __init__(self, name, data_type, value):
        self.name = name
        self.data_type = data_type
        self.value = value

    def __repr__(self):
        node_dict = {
            "type": "Let",
            "name": self.name,
            "data_type": self.data_type,
            "value": self.value
        }
        return str(node_dict)

class IfNode:
    def __init__(self, condition, then, else_):
        self.condition = condition
        self.then = then
        self.else_ = else_

    def __repr__(self):
        node_dict = {
            "type": "If",
            "condition": self.condition,
            "then": self.then,
            "else": self.else_
        }
        return str(node_dict)

class LoopNode:
    def __init__(self, body):
        self.body = body

    def __repr__(self):
        node_dict = {
            "type": "Loop",
            "body": self.body
        }
        return str(node_dict)

class LoopTerminationNode:
    def __init__(self, _break):
        self._break = _break

    def __repr__(self):
        node_dict = {
            "type": "LoopTermination",
            "break": self._break
        }
        #This is so the bool is written as a correct json bool, should probably do this for all classes
        return str(json.dumps(node_dict))

class CastNode:
    def __init__(self, value, data_type):
        self.value = value
        self.data_type = data_type

    def __repr__(self):
        node_dict = {
            "type": "Cast",
            "value": self.value,
            "data_type": self.data_type
        }
        return str(node_dict)
    
class DereferenceNode:
    def __init__(self, variable, data_type):
        self.variable = variable
        self.data_type = data_type

    def __repr__(self):
        node_dict = {
            "type": "Dereference",
            "variable": self.variable,
            "data_type": self.data_type
        }
        return str(node_dict)

class StructInstanceNode:
    def __init__(self, members):
        self.members = members

    def __repr__(self):
        node_dict = {
            "type": "StructInstance",
            "members": self.members
        }
        return str(node_dict)

class StructDefinitionNode:
    def __init__(self, name, member_types):
        self.name = name
        self.member_types = member_types

    def __repr__(self):
        node_dict = {
            "type": "StructDefinition",
            "name": self.name,
            "member_types": self.member_types
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
        statements = []
        while self.current_token.type != TokenType.EOF:
            if self.current_token.type == TokenType.DEC:
                statements.append(self.parse_function_declaration())
            elif self.current_token.type == TokenType.FN or self.current_token.type == TokenType.ACTN:
                statements.append(self.parse_function_definition())
            elif self.current_token.type == TokenType.LET:
                let_statement = self.parse_let()
                if not isinstance(let_statement.value, LiteralNode):
                    raise Exception("Global let statements must be initialized with a literal")
                statements.append(let_statement)
            elif self.current_token.type == TokenType.STRUCT:
                statements.append(self.parse_struct_definition())
            else:   
                raise Exception(f"Top level expressions are not allowed")
                #statements.append(self.parse_expression())
            # Statements are separated by semicolons
            if self.current_token.type != TokenType.SEMICOLON:
               raise Exception(f"Expected ; but got {self.current_token}")
            self.advance() # Advance past the semicolon
        return statements

    def parse_expression(self):
        return self.parse_pointee_assignment()
    
    def parse_pointee_assignment(self):
        return self.parse_binary_expression(self.parse_logic, [":="])

    #Binary boolean logic &, | etc.
    def parse_logic(self):
        return self.parse_binary_expression(self.parse_comparison, ["&", "|"])
    
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
        if token.type == TokenType.OPERATOR and token["operator"] in ["+", "-", "!", "&"]:
            self.advance()
            #If it was a unary operator then parse the next unary which is the operand 
            return UnaryNode(token["operator"], self.parse_unary())
        return self.parse_primary() #If it wasn't a unary operator then it must be a primary
    
    def parse_dereference(self):
        data_type = self.current_token["data_type"]
        self.advance() #Advance past the type
        if self.current_token.type != TokenType.OPERATOR or self.current_token["operator"] != "<-":
            raise Exception("Expected <- operator after type")
        self.advance() #Advance past the <- operator
        variable = self.parse_primary()
        return DereferenceNode(variable, data_type)

    def parse_primary(self):
        node = None
        if self.current_token.type == TokenType.LITERAL:
            node = LiteralNode(self.current_token["value"], self.current_token["data_type"])
            self.advance() #Advance past the literal
        elif self.current_token.type == TokenType.TYPE: 
            if self.peek().type == TokenType.COLON: #Explicitly typed literals
                node = self.parse_explicit_literal()
            elif self.peek().type == TokenType.OPERATOR and self.peek()["operator"] == "<-": #Dereference
                node = self.parse_dereference()
            else:
                raise Exception("Expected a literal or dereference after type")
        elif self.current_token.type == TokenType.IDENTIFIER:
            #If the next token is not a ( it is a function call
            if self.peek().type == TokenType.LPAR:
                node = self.parse_function_call()
            elif self.peek().type == TokenType().LBRACKET: #This is an index of some sort
                node = self.parse_index()
            else: #If its not followed by anything of relevance then we treat it as a variable reference
                node = VariableNode(self.current_token["name"])
                self.advance() #Advance past the identifier
        elif self.current_token.type == TokenType.LPAR:
            # Parenthesized expression take precedence and are therefore primary
            self.advance() #Advance past the (
            expr = self.parse_expression()
            if self.current_token.type != TokenType.RPAR:
                raise Exception("Expected )")
            self.advance() #Advance past the )
            node = expr
        elif self.current_token.type == TokenType.LBRACE:
            node = self.parse_struct_instance()
        
        #After we have the node we need to check if its being cast into a type
        if self.current_token.type == TokenType.AS:
            self.advance() #Advance past the AS
            if self.current_token.type != TokenType.TYPE:
                raise Exception("Expected type after 'as'")
            data_type = self.current_token["data_type"]
            self.advance() #Advance past the type
            node = CastNode(node, data_type)      
        if node is not None:
            return node
        
        raise Exception(f"Expected primary but got {self.current_token}")
    
    def parse_struct_definition(self):
        self.advance() #Advance past the struct keyword
        if self.current_token.type != TokenType.IDENTIFIER:
            raise Exception("Expected identifier after struct")
        struct_name = self.current_token["name"]
        self.advance() #Advance past the identifier
        if self.current_token.type != TokenType.LBRACE:
            raise Exception("Expected { after struct name")
        self.advance() #Advance past the {
        members = []
        while self.current_token.type != TokenType.RBRACE:
            if self.current_token.type != TokenType.TYPE:
                raise Exception("Expected type in struct definition")
            data_type = self.current_token["data_type"]
            self.advance() #Advance past the type
            if self.current_token.type != TokenType.COLON:
                raise Exception("Expected : in struct definition")
            self.advance() #Advance past the :
            if self.current_token.type != TokenType.IDENTIFIER:
                raise Exception("Expected identifier in struct definition")
            member_name = self.current_token["name"]
            self.advance() #Advance past the identifier
            members.append(data_type) #TODO: Add support name of member
            if self.current_token.type != TokenType.COMMA and self.current_token.type != TokenType.RBRACE:
                raise Exception("Expected , or in struct definition")
            if self.current_token.type == TokenType.COMMA:
                self.advance() 
        self.advance() #Advance past the }
        return StructDefinitionNode(struct_name, members)

    def parse_index(self):
        variable = VariableNode(self.current_token["name"])
        self.advance() #Advance past the identifier
        if self.current_token.type != TokenType.LBRACKET:
            raise Exception("Expected [ in index")
        self.advance() #Advance past the [
        index = self.parse_expression()
        if self.current_token.type != TokenType.RBRACKET:
            raise Exception("Expected ] in index")
        self.advance() #Advance past the ]
        #Indexes are just binary operators between the variable and the index
        return BinaryNode(variable, "[]", index)

    def parse_struct_instance(self):
        self.advance() #Advance past the {
        members = []
        while self.current_token.type != TokenType.RBRACE:
            members.append(self.parse_expression())
            if self.current_token.type != TokenType.COMMA and self.current_token.type != TokenType.RBRACE:
                raise Exception(f"Expected , in struct but got {self.current_token}")
            if self.current_token.type == TokenType.COMMA:
                self.advance()
        self.advance() #Advance past the }
        return StructInstanceNode(members)

    def parse_explicit_literal(self):
        data_type = self.current_token["data_type"]
        self.advance() #Advance past the type
        if self.current_token.type != TokenType.COLON:
            raise Exception(f"Expected : but got {self.current_token}")
        self.advance() #Advance past the :
        if self.current_token.type != TokenType.LITERAL:
            raise Exception(f"Expected literal but got {self.current_token}")
        #TODO: Check that the literal is of the correct type (i.e if literal datatype is "float" then explicit type cant be i32 etc.)
        literal = LiteralNode(self.current_token["value"], data_type)
        self.advance() #Advance past the literal
        return literal

    def parse_function_call(self):
        fn_name = self.current_token["name"]
        self.advance() #Advance past the identifier
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
        return CallNode(fn_name, args)
    
    def parse_function_declaration(self):
        return_type = ""
        if(self.current_token.type == TokenType.ACTN):
            return_type = "void"
        self.advance() #Advance past the dec/fn/actn keyword
        if(self.current_token.type == TokenType.ACTN): #This second check is for the case "dec actn"
            return_type = "void"
            self.advance() #Advance past the actn keyword
        if self.current_token.type != TokenType.IDENTIFIER:
            raise Exception("Expected identifier after dec/fn/act")
        fn_name = self.current_token["name"]
        self.advance() #Advance past the function name
        if self.current_token.type != TokenType.LPAR:
            raise Exception("Expected ( after function name")
        self.advance() #Advance past the (
        args = []
        arg_types = []
        while self.current_token.type != TokenType.RPAR:
            if self.current_token.type == TokenType.TYPE:
                arg_types.append(self.current_token["data_type"])
            elif self.current_token.type == TokenType.IDENTIFIER:
                arg_types.append(self.current_token["name"])
            else:
                raise Exception("Expected type or identifier before argument name")
            self.advance() #Advance past the type
            if self.current_token.type != TokenType.COLON:
                raise Exception("Expected : after argument type")
            self.advance() #Advance past the :
            if self.current_token.type != TokenType.IDENTIFIER:
                raise Exception("Expected identifier after type in argument list")
            args.append(self.current_token["name"])
            self.advance() #Advance past the argument name
            if self.current_token.type != TokenType.COMMA and self.current_token.type != TokenType.RPAR:
                raise Exception(f"Expected ',' but got {self.current_token}")
            
            #Advancing past the ')' is handled after the loop so we only advance if the token is a comma
            if self.current_token.type == TokenType.COMMA:
                self.advance()
        self.advance() #Advance past the )
        if return_type != "void":
            if self.current_token.type != TokenType.OPERATOR or self.current_token["operator"] != "->":
                raise Exception("Expected -> after function name")
            self.advance() #Advance past the ->
            if self.current_token.type != TokenType.TYPE:
                raise Exception("Expected return type after function name")
            return_type = self.current_token["data_type"]
            self.advance() #Advance past the return type
        return PrototypeNode(fn_name, arg_types, args, return_type)
    
    def parse_function_definition(self):
        fn_prototype = self.parse_function_declaration()
        if self.current_token.type != TokenType.LBRACE:
            raise Exception("Expected { after function prototype")
        return FunctionNode(fn_prototype, self.parse_statement_block())
    
    def parse_assignment(self):
        variable = self.parse_primary() #Left hand side of the assignment is a variable which is primary
        if not isinstance(variable, VariableNode):
            raise Exception(f"Expected variable on left side of assignment but got {lhs}")
        if self.current_token.type != TokenType.OPERATOR or (self.current_token["operator"] != "<-"):
            raise Exception(f"Expected <- after variable name in assignment but got {self.current_token}")
        op = self.current_token["operator"]
        self.advance() #Advance past the <- or :=
        return BinaryNode(variable, op, self.parse_expression())
        

    def parse_let(self):
        self.advance() #Advance past the let keyword
        if self.current_token.type == TokenType.TYPE:
            data_type = self.current_token["data_type"]
        elif self.current_token.type == TokenType.IDENTIFIER:
            data_type = self.current_token["name"]
        else:
            raise Exception("Expected type or identifier after let")
        self.advance() #Advance past the type
        if self.current_token.type != TokenType.COLON:
            raise Exception("Expected : after type in let")
        self.advance() #Advance past the :
        if self.current_token.type != TokenType.IDENTIFIER:
            raise Exception("Expected identifier after type in let")
        variable = self.current_token["name"]
        self.advance() #Advance past the variable name
        if self.current_token.type != TokenType.OPERATOR or self.current_token["operator"] != "<-":
            raise Exception(f"Expected <- after variable name in let assignment but got {self.current_token}")
        self.advance() #Advance past the <- operator
        return LetNode(variable, data_type, self.parse_expression())

    def parse_if_statement(self):
        self.advance() #Advance past the if keyword
        condition = self.parse_expression()
        if self.current_token.type != TokenType.LBRACE:
            raise Exception("Expected { after if condition")
        then = self.parse_statement_block()
        if self.current_token.type == TokenType.SEMICOLON:
            #There should be no else node if there is a semicolon.
            return IfNode(condition, then, [])
            # Dont want to advance past the semicolon because it is used to separate statements 
            # and not tecnically part of the statement therefore it is the responisibility of the calling function
        if self.current_token.type != TokenType.ELSE:
            raise Exception(f"Expected else or ';' after if block but got {self.current_token}")
        self.advance() #Advance past the else keyword
        if self.current_token.type == TokenType.IF:
            # We have an else if statement
            return IfNode(condition, then, [self.parse_if_statement()])
        if self.current_token.type != TokenType.LBRACE:
            raise Exception("Expected { after else")
        else_ = self.parse_statement_block()
        return IfNode(condition, then, else_)
        # Again, dont want to advance past semicolon because it is the responsibility of the calling function
    
    def parse_loop(self):
        self.advance() #Advance past the loop keyword
        if self.current_token.type != TokenType.LBRACE:
            raise Exception("Expected { after loop keyword")
        return LoopNode(self.parse_statement_block())
    
    def parse_while(self):
        self.advance() #Advance past the while keyword
        condition = self.parse_expression()
        if self.current_token.type != TokenType.LBRACE:
            raise Exception("Expected { after while condition")
        # We add everyting to an "if condition then statements else break"
        statements = [IfNode(condition, self.parse_statement_block(), [LoopTerminationNode(True)])]
        return LoopNode(statements)

    def parse_statement_block(self):
        self.advance() #Advance past the {
        statements = []
        while self.current_token.type != TokenType.RBRACE:
            if self.current_token.type == TokenType.RET:
                statements.append(self.parse_return())
            elif self.current_token.type == TokenType.IDENTIFIER and self.peek().type == TokenType.OPERATOR and \
            (self.peek()["operator"] == "<-"):
                statements.append(self.parse_assignment())
            elif self.current_token.type == TokenType.LET:
                statements.append(self.parse_let())
            elif self.current_token.type == TokenType.IF:
                statements.append(self.parse_if_statement())
            elif self.current_token.type == TokenType.LOOP:
                statements.append(self.parse_loop())
            elif self.current_token.type == TokenType.WHILE:
                statements.append(self.parse_while())
            elif self.current_token.type == TokenType.BREAK or self.current_token.type == TokenType.CONTINUE:
                statements.append(LoopTerminationNode(self.current_token.type == TokenType.BREAK))
                self.advance() #Advance past the break/continue keyword
            else:
                statements.append(self.parse_expression())
                
            if self.current_token.type != TokenType.SEMICOLON:
                raise Exception(f"Expected ; after expression but got {self.current_token}")
            self.advance() #Advance past the ;
        self.advance() #Advance past the }
        return statements

    def parse_return(self):
        self.advance() # Advance past the ret keyword
        return ReturnNode(self.parse_expression())

import sys

if(len(sys.argv) != 2):
    print("Usage: python3 parser.py <filename>")
    exit()

code = open(sys.argv[1], "r").read()

tokens = Lexer(code).tokenize()
ast = Parser(tokens).parse()

# Generate an AST string for the codegen
ast_str = "["
for expr in ast:
    ast_str += str(expr) + ",\n"
ast_str = ast_str[:-2] + "]"

f = open("ast", "w")
f.write(ast_str.replace("\'", "\""))
f.close()
