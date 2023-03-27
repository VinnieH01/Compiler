class TokenType:
    DEC = "DEC"
    FN = "FN"
    ACTN = "ACTN"
    SEMICOLON = "SEMICOLON"
    IDENTIFIER = "IDENTIFIER"
    LITERAL = "LITERAL"
    OPERATOR = "OPERATOR"
    LPAR = "LPAR"
    RPAR = "RPAR"
    COMMA = "COMMA"
    EOF = "EOF"
    LBRACE = "LBRACE"
    RBRACE = "RBRACE"
    LBRACKET = "LBRACKET"
    RBRACKET = "RBRACKET"
    RET = "RET"
    LET = "LET"
    IF = "IF"
    ELSE = "ELSE"
    BREAK = "BREAK"
    CONTINUE = "CONTINUE"
    LOOP = "LOOP"
    WHILE = "WHILE"
    TYPE = "TYPE"
    COLON = "COLON"
    AS = "AS"
    STRUCT = "STRUCT"
    EXTERN = "EXTERN"

    PREPROCESSOR = "PREPROCESSOR"

class Token:
    def __init__(self, type, meta):
        self.type = type
        self.meta = meta

    def __repr__(self):
        if self.meta == {}:
            return f"{self.type}"
        return f"{self.type}{self.meta}"
        pass
    
    # [] operator
    def __getitem__(self, key):
        return self.meta[key]

class Lexer():
    def __init__(self, text):
        self.text = text
        self.index = 0
        self.current_char = self.text[0]
        self.keywords = {
            "dec": TokenType.DEC,
            "fn": TokenType.FN,
            "actn": TokenType.ACTN,
            ";": TokenType.SEMICOLON,
            "(": TokenType.LPAR,
            ")": TokenType.RPAR,
            ",": TokenType.COMMA,
            "{": TokenType.LBRACE,
            "}": TokenType.RBRACE,
            "[": TokenType.LBRACKET,
            "]": TokenType.RBRACKET,
            "ret": TokenType.RET,
            "let": TokenType.LET,
            "if": TokenType.IF,
            "else": TokenType.ELSE,
            "break": TokenType.BREAK,
            "loop": TokenType.LOOP,
            "continue": TokenType.CONTINUE,
            ":": TokenType.COLON,
            "as": TokenType.AS,
            "while": TokenType.WHILE,
            "struct": TokenType.STRUCT,
            "extern": TokenType.EXTERN,
        }
        self.operator_constituents = ["+", "-", "*", "(", ")", ";", ",", "{", "}", "=", ">", "<", ":", "!", "&", "|", "[", "]", "."] 
        self.operators = ["+", "-", "*", "(", ")", ";", ",", "{", "}", "=", ">", "<", ":", "!", "&", "|", "<-", "->", ":=", "[", "]", "."] 
        self.types = {
            "bool": "bool",
            "i8": "i8",
            "i16": "i16",
            "i32": "i32",
            "i64": "i64",
            "i128": "i128",
            "f32": "f32",
            "f64": "f64",
            "ptr": "ptr",
        }
        self.literals = {
            "true": Token(TokenType.LITERAL, {"data_type": "bool", "value": 1}),
            "false": Token(TokenType.LITERAL, {"data_type": "bool", "value": 0}),
            "null": Token(TokenType.LITERAL, {"data_type": "ptr", "value": 0}),
        }

    def advance(self):
        self.index += 1
        self.current_char = self.text[self.index] if self.index < len(self.text) else None
    
    def peek(self, n=1):
        if self.index + n < len(self.text):
            return self.text[self.index + n]
        return None

    def tokenize(self):
        tokens = []
        while self.current_char != None:
            if self.current_char.isspace():
                self.advance() # Skip whitespace between tokens
            elif self.current_char.isalpha() or self.current_char == '_':
                tokens.append(self.tokenize_identifier())
            elif self.current_char in self.operator_constituents:
                tokens.append(self.tokenize_operator())
            elif self.current_char.isdigit():
                tokens.append(self.tokenize_number())
            elif self.current_char == '"':
                tokens.append(self.tokenize_string())
            elif self.current_char == "#":
                tokens.append(self.tokenize_preprocessor_directive())
            else:
                raise Exception(f"Unknown char: {self.current_char}")
        tokens.append(Token(TokenType.EOF, {}))
        return tokens

    def tokenize_identifier(self):
        result = ""
        while self.current_char != None and self.current_char.isalnum() or self.current_char == '_':
            result += self.current_char
            self.advance()
        if result in self.keywords:
            return Token(self.keywords[result], {})
        if result in self.types:
            return Token(TokenType.TYPE, {"data_type": result})
        if result in self.literals:
            return self.literals[result]
        return Token(TokenType.IDENTIFIER, {"name": result})
    
    def tokenize_operator(self):
        result = self.current_char
        self.advance()
        double_op = result + self.current_char if self.current_char != None else ""
        if double_op in self.operators: # This is to handle operators like <- and :=
            self.advance()
            return Token(TokenType.OPERATOR, {"operator": double_op})
        if result in self.keywords:
            return Token(self.keywords[result], {})
        if result in self.operators:
            return Token(TokenType.OPERATOR, {"operator": result})
        raise Exception(f"Unknown token: {result}")
    
    def tokenize_number(self):
        result = ""
        while self.current_char != None and self.current_char.isdigit():
            result += self.current_char
            self.advance()
        if self.current_char == ".": # Float
            result += self.current_char
            self.advance()
            while self.current_char != None and self.current_char.isdigit():
                result += self.current_char
                self.advance()
            return Token(TokenType.LITERAL, {"data_type": "float", "value": float(result)})
        return Token(TokenType.LITERAL, {"data_type": "integer", "value": int(result)})
    
    def tokenize_string(self):
        result = ""
        self.advance() # Skip the first "
        while self.current_char != None and self.current_char != '"':
            result += self.current_char
            self.advance()
        self.advance() # Skip the last "
        return Token(TokenType.LITERAL, {"data_type": "string", "value": result})
    
    def tokenize_preprocessor_directive(self):
        result = ""
        self.advance() # Skip the #
        while self.current_char != None and self.current_char.isspace() == False:
            result += self.current_char
            self.advance()
        return Token(TokenType.PREPROCESSOR, {"directive": result})

class Preprocessor:
    def __init__(self, file):
        self.file = file
        self.tokens = []
    
    def preprocess(self):
        #read the file
        with open(self.file, "r") as f:
            text = f.read()
        #tokenize the file
        lexer = Lexer(text)
        self.tokens = lexer.tokenize()
        
        new_tokens = []
        i = 0
        while i < len(self.tokens):
            if self.tokens[i].type == TokenType.PREPROCESSOR:
                if self.tokens[i]["directive"] == "include":
                    if self.tokens[i+1].type == TokenType.LITERAL:
                        included_file = self.tokens[i+1]["value"]
                        pp = Preprocessor(included_file)
                        new_tokens += pp.preprocess()[:-1] # Skip the EOF token
                        i += 2 # Skip the include and the string literal
                        continue
            new_tokens.append(self.tokens[i])
            i += 1

        return new_tokens




    