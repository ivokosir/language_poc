from token import Token
from typing import Dict, List, Union
import literal


Expr = Union['Binary', 'Unary', 'Grouping', 'Logical', 'Literal', 'Variable', 'Assign', 'Call', 'Object', 'Get', 'Set']


class Binary:
    def __init__(self, left: Expr, operator: Token, right: Expr):
        self.left = left
        self.operator = operator
        self.right = right

    def __repr__(self) -> str:
        return f"({self.left} {self.operator} {self.right})"


class Unary:
    def __init__(self, operator: Token, right: Expr):
        self.operator = operator
        self.right = right

    def __repr__(self) -> str:
        return f"({self.operator} {self.right})"


class Grouping:
    def __init__(self, expression: Expr):
        self.expression = expression

    def __repr__(self) -> str:
        return f"(group {self.expression})"


class Logical:
    def __init__(self, left: Expr, operator: Token, right: Expr):
        self.left = left
        self.operator = operator
        self.right = right

    def __repr__(self) -> str:
        return f"({self.left} {self.operator} {self.right})"


class Literal:
    def __init__(self, value: literal.Literal):
        self.value = value

    def __repr__(self) -> str:
        return repr(self.value)


class Variable:
    def __init__(self, name: Token):
        self.name = name

    def __repr__(self) -> str:
        return str(self.name)


class Assign:
    def __init__(self, name: Token, value: Expr):
        self.name = name
        self.value = value

    def __repr__(self) -> str:
        return f"({self.name} = {self.value})"


class Call:
    def __init__(self, callee: Expr, paren: Token, arguments: List[Expr]):
        self.callee = callee
        self.paren = paren
        self.arguments = arguments

    def __repr__(self) -> str:
        argument_strings = [repr(argument) for argument in self.arguments]
        return f"call {self.callee} ( {' , '.join(argument_strings)} )"


class Object:
    def __init__(self, fields: Dict[Token, Expr]):
        self.fields = fields

    def __repr__(self) -> str:
        field_strings = [f"{name} = {e}" for (name, e) in self.fields.items()]
        return f"object {{ {' , '.join(field_strings)} }}"


class Get:
    def __init__(self, object: Expr, name: Token):
        self.object = object
        self.name = name

    def __repr__(self) -> str:
        return f"{self.object} . {self.name}"


class Set:
    def __init__(self, object: Expr, name: Token, value: Expr):
        self.object = object
        self.name = name
        self.value = value

    def __repr__(self) -> str:
        return f"{self.object} set {self.name} = {self.value}"
