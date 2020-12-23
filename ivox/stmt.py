from typing import List, Optional, Union

from expr import Expr
from token import Token


Stmt = Union['Expression', 'Print', 'Var', 'Block', 'If', 'While', 'Function', 'Return']


class Expression:
    def __init__(self, expr: Expr):
        self.expr = expr

    def __repr__(self) -> str:
        return f"Expression {self.expr}"


class Print:
    def __init__(self, expr: Expr):
        self.expr = expr

    def __repr__(self) -> str:
        return f"Print {self.expr}"


class Var:
    def __init__(self, name: Token, initializer: Expr):
        self.name = name
        self.initializer = initializer

    def __repr__(self) -> str:
        return f"Var {self.name} = {self.initializer}"


class Block:
    def __init__(self, statements: List[Stmt]):
        self.statements = statements

    def __repr__(self) -> str:
        statement_strings = [repr(statement) for statement in self.statements]
        return f"{{ {' ; '.join(statement_strings)} }}"


class If:
    def __init__(self, condition: Expr, then_branch: Stmt, else_branch: Optional[Stmt]):
        self.condition = condition
        self.then_branch = then_branch
        self.else_branch = else_branch

    def __repr__(self) -> str:
        return f"if {self.condition} then {self.then_branch} else {self.else_branch}"


class While:
    def __init__(self, condition: Expr, body: Stmt):
        self.condition = condition
        self.body = body

    def __repr__(self) -> str:
        return f"while {self.condition} do {self.body}"


class Function:
    def __init__(self, name: Token, params: List[Token], body: List[Stmt]):
        self.name = name
        self.params = params
        self.body = body

    def __repr__(self) -> str:
        param_strings = [repr(param) for param in self.params]
        body_strings = [repr(statement) for statement in self.body]
        return f"fun {self.name} ( {' , '.join(param_strings)} ) {{ {' ; '.join(body_strings)} }}"


class Return:
    def __init__(self, keyword: Token, value: Expr):
        self.keyword = keyword
        self.value = value

    def __repr__(self) -> str:
        return f"return {self.value}"
