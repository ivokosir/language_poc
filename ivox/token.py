from enum import Enum

from literal import Literal
from typing import Optional, Union

class Token:
    Type = Enum('TokenType', [
        'LEFT_PAREN', 'RIGHT_PAREN', 'LEFT_BRACE', 'RIGHT_BRACE', 'COMMA', 'DOT', 'MINUS', 'PLUS', 'SEMICOLON', 'SLASH',
        'STAR', 'BANG', 'BANG_EQUAL', 'EQUAL', 'EQUAL_EQUAL', 'GREATER', 'GREATER_EQUAL', 'LESS', 'LESS_EQUAL',

        'IDENTIFIER', 'STRING', 'NUMBER',

        'AND', 'CLASS', 'ELSE', 'FALSE', 'FUN', 'FOR', 'IF', 'NIL', 'OR', 'PRINT', 'RETURN', 'SUPER', 'THIS', 'TRUE',
        'VAR', 'WHILE', 'OBJECT',

        'EOF',
    ])

    def __init__(self, type: Type, lexeme: str, line: int, literal: Optional[Literal] = None) -> None:
        self.type = type
        self.lexeme = lexeme
        self.line = line
        self.literal = literal

    def __repr__(self) -> str:
        if self.literal:
            return f'{self.type.name}: {str(self.lexeme)}'
        else:
            return f'{self.lexeme}'
