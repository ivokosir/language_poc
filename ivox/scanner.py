from typing import List, NoReturn, Optional, Tuple
import re
import sys

from error import report
from token import Token

OPERATORS = {
    '(': Token.Type.LEFT_PAREN,
    ')': Token.Type.RIGHT_PAREN,
    '{': Token.Type.LEFT_BRACE,
    '}': Token.Type.RIGHT_BRACE,
    ',': Token.Type.COMMA,
    '.': Token.Type.DOT,
    '-': Token.Type.MINUS,
    '+': Token.Type.PLUS,
    ';': Token.Type.SEMICOLON,
    '/': Token.Type.SLASH,
    '*': Token.Type.STAR,
    '!=': Token.Type.BANG_EQUAL,
    '!': Token.Type.BANG,
    '==': Token.Type.EQUAL_EQUAL,
    '=': Token.Type.EQUAL,
    '>=': Token.Type.GREATER_EQUAL,
    '>': Token.Type.GREATER,
    '<=': Token.Type.LESS_EQUAL,
    '<': Token.Type.LESS,
}

KEYWORDS = {
    'and': Token.Type.AND,
    'class': Token.Type.CLASS,
    'else': Token.Type.ELSE,
    'false': Token.Type.FALSE,
    'fun': Token.Type.FUN,
    'for': Token.Type.FOR,
    'if': Token.Type.IF,
    'nil': Token.Type.NIL,
    'or': Token.Type.OR,
    'print': Token.Type.PRINT,
    'return': Token.Type.RETURN,
    'super': Token.Type.SUPER,
    'this': Token.Type.THIS,
    'true': Token.Type.TRUE,
    'var': Token.Type.VAR,
    'while': Token.Type.WHILE,
    'object': Token.Type.OBJECT,
}


def error(char: str, line: int) -> NoReturn:
    report(line, '', f"Unexpected {char}.")
    sys.exit(1)


def scan_regex(regex: str, chars: str) -> Tuple[Optional[str], str]:
    m = re.match(regex, chars)

    if m:
        return (m.group(0), chars[m.end():])
    else:
        return (None, chars)


def scan_(chars: str, line: int) -> List[Token]:
    if not chars:
        return [Token(Token.Type.EOF, '', line)]

    if chars[0] == '\n':
        return scan_(chars[1:], line + 1)

    empty, chars = scan_regex(r'\s', chars)
    if empty:
        return scan_(chars, line)

    for operator, type in OPERATORS.items():
        if chars.startswith(operator):
            return [Token(type, operator, line)] + scan_(chars[len(operator):], line)

    number, chars = scan_regex(r'-?\d+', chars)
    if number:
        return [Token(Token.Type.NUMBER, number, line, int(number))] + scan_(chars, line)

    identifier, chars = scan_regex(r'[^\W\d]\w*', chars)
    if identifier:
        for keyword, type in KEYWORDS.items():
            if keyword == identifier:
                return [Token(type, keyword, line)] + scan_(chars, line)
        return [Token(Token.Type.IDENTIFIER, identifier, line)] + scan_(chars, line)

    string, chars = scan_regex(r'"[^"]*"', chars)
    if string:
        value = string[1:-1]
        return [Token(Token.Type.STRING, string, line, value)] + scan_(chars, line)

    error(chars[0], line)


def scan(source: str) -> List[Token]:
    return scan_(source, 1)
