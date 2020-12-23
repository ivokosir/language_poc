from interpreter import Interpreter
from parser import Parser
from resolver import Resolver
import error
import scanner


def compile(source: str) -> None:
    tokens = scanner.scan(source)
    parser = Parser(tokens)
    statements = parser.parse()
    interpreter = Interpreter()
    resolver = Resolver(interpreter)
    print(statements)
    resolver.resolve(statements)

    if not error.hadError:
        interpreter.interpret(statements)

with open('tests/int.ivox') as f:
    source = f.read()
    compile(source)
