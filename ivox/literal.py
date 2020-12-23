from typing import Callable, Dict, List, Union

Literal = Union[int, str, None, 'Function', 'Object']

class Function:
    def __init__(self, call: Callable[[List[Literal]], Literal], arity: int) -> None:
        self.call = call
        self.arity = arity

    def __repr__(self) -> str:
        return f"function({self.arity})"

class Object:
    def __init__(self, fields: Dict[str, Literal]):
        self.fields = fields

    def __repr__(self) -> str:
        field_strings = [f"{name} = {e}" for (name, e) in self.fields.items()]
        return f"object {{ {' , '.join(field_strings)} }}"
