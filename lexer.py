import re

class TokenType:
    DEC = "DEC"
    FN = "FN"
    ACTN = "ACTN"
    SEMICOLON = "SEMICOLON"
    IDENTIFIER = "IDENTIFIER"
    NUMBER = "NUMBER"
    OPERATOR = "OPERATOR"
    LPAR = "LPAR"
    RPAR = "RPAR"
    COMMA = "COMMA"
    EOF = "EOF"
    LBRACE = "LBRACE"
    RBRACE = "RBRACE"

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

def tokenize(code):
    raw_tokens = code.split()

    # Tokens are only split based on whitespace so x+y or func(x) 
    # are all one token while they should be multiple eg. func, (, x, and )

    # This splits them further into multiple tokens

    # The following regex selects empty chars around ";" and splits on them so from x;y you get [x, ;, y]
    # "(?<=;)|(?=;)" We use this to generate a regex which splits on all operators while also keeping them as tokens

    # This only supports single char operators
    operators = ["+", "-", "*", "(", ")", ";", ",", "{", "}", "=", ">", "<"]

    op_regex = ""
    for op in operators:
        op_regex += f"(?<=\{op})|(?=\{op})|"
    op_regex = op_regex[:-1] #Remove final |

    new_raw_tokens = []

    for raw_token in raw_tokens:
        new_raw_tokens += re.split(op_regex, raw_token)

    #Remove empty strings
    raw_tokens = list(filter(None, new_raw_tokens))

    keywords = {
        "dec": TokenType.DEC,
        "fn": TokenType.FN,
        "actn": TokenType.ACTN,
        ";": TokenType.SEMICOLON,
        "(": TokenType.LPAR,
        ")": TokenType.RPAR,
        ",": TokenType.COMMA,
        "{": TokenType.LBRACE,
        "}": TokenType.RBRACE
    }

    tokens = []
    for raw_token in raw_tokens:
        type = ""
        meta = {}
        if raw_token in keywords:
            type = keywords[raw_token]
        elif raw_token[0].isalpha() and raw_token.isalnum():
            type = TokenType.IDENTIFIER
            meta["name"] = raw_token
        elif raw_token.isnumeric(): #TODO: Add support for inputting floats (12.23 is currently invalid)
            type = TokenType.NUMBER
            meta["value"] = float(raw_token)
        elif re.search(op_regex, raw_token) != None:
            type = TokenType.OPERATOR
            meta["operator"] = raw_token
        else:
            raise Exception(f"Invalid token: {raw_token}")

        tokens.append(Token(type, meta))
    
    tokens.append(Token(TokenType.EOF, {}))
    return tokens



    