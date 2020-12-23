from typing import Dict, Optional

from literal import Literal
from token import Token


class Environment:
    def __init__(self, enclosing: Optional['Environment'] = None) -> None:
        self.enclosing = enclosing
        self.values: Dict[str, Literal] = dict()

    def define(self, name: str, value: Literal) -> None:
        self.values[name] = value

    def get(self, name: Token) -> Literal:
        if name.lexeme in self.values:
            return self.values[name.lexeme]

        if self.enclosing:
            return self.enclosing.get(name)

        raise ValueError(f"Undefined variable '{name.lexeme}'")

    def assign(self, name: Token, value: Literal) -> None:
        if name.lexeme in self.values:
            self.values[name.lexeme] = value
        elif self.enclosing:
            self.enclosing.assign(name, value)
        else:
            raise ValueError(f"Undefined variable '{name.lexeme}'")

    def getAt(self, distance: int, name: str) -> Literal:
        return self.ancestor(distance).values[name]

    def assignAt(self, distance: int, name: Token, value: Literal) -> None:
        self.ancestor(distance).values[name.lexeme] = value

    def ancestor(self, distance: int) -> 'Environment':
        environment = self
        for i in range(distance):
            if environment.enclosing:
                environment = environment.enclosing

        return environment
