from typing import Dict, List, NoReturn
import time

from environment import Environment
from literal import Literal, Function, Object
from token import Token
import expr
import stmt


class Return(Exception):
    def __init__(self, value: Literal):
        self.value = value


class Interpreter:
    def __init__(self) -> None:
        self.environment = Environment()
        self.locals: Dict[expr.Expr, int] = dict()
        self.globals = self.environment

        def clock(args: List[Literal]) -> Literal:
            return round(time.time())
        self.environment.define('clock', Function(clock, 0))

    def interpret(self, statements: List[stmt.Stmt]) -> None:
        for stmt in statements:
            self.execute(stmt)

    def executeExpressionStmt(self, stmt: stmt.Expression) -> None:
        self.evaluate(stmt.expr)

    def executePrintStmt(self, stmt: stmt.Print) -> None:
        value = self.evaluate(stmt.expr)
        print(value)

    def executeReturnStmt(self, stmt: stmt.Return) -> NoReturn:
        value = self.evaluate(stmt.value)

        raise Return(value)

    def executeBlockStmt(self, stmt: stmt.Block) -> None:
        previous = self.environment

        self.environment = Environment(previous)

        for statement in stmt.statements:
            self.execute(statement)

        self.environment = previous

    def executeVarStmt(self, stmt: stmt.Var) -> None:
        value = self.evaluate(stmt.initializer)

        self.environment.define(stmt.name.lexeme, value)

    def executeFunctionStmt(self, stmt: stmt.Function) -> None:
        parent = self.environment

        def call(args: List[Literal]) -> Literal:
            previous = self.environment

            self.environment = parent

            for param, arg in zip(stmt.params, args):
                self.environment.define(param.lexeme, arg)

            value: Literal = None
            try:
                for statement in stmt.body:
                    self.execute(statement)
            except Return as r:
                value = r.value

            self.environment = previous

            return value

        function = Function(call, len(stmt.params))

        self.environment.define(stmt.name.lexeme, function)

    def executeIfStmt(self, stmt: stmt.If) -> None:
        if self.isTruthy(self.evaluate(stmt.condition)):
            self.execute(stmt.then_branch);
        elif stmt.else_branch:
            self.execute(stmt.else_branch)

    def executeWhileStmt(self, stmt: stmt.While) -> None:
        while self.isTruthy(self.evaluate(stmt.condition)):
            self.execute(stmt.body);

    def isEqual(self, left: Literal, right: Literal) -> bool:
        return left == right

    def isTruthy(self, literal: Literal) -> bool:
        if literal == None or literal == False:
            return False
        else:
            return True

    def evaluateBinary(self, expr: expr.Binary) -> Literal:
        left = self.evaluate(expr.left)
        right = self.evaluate(expr.right)

        if expr.operator.type == Token.Type.MINUS:
            if isinstance(left, int) and isinstance(right, int):
                return left - right
        elif expr.operator.type == Token.Type.SLASH:
            if isinstance(left, int) and isinstance(right, int):
                return left // right
        elif expr.operator.type == Token.Type.STAR:
            if isinstance(left, int) and isinstance(right, int):
                return left * right
        elif expr.operator.type == Token.Type.PLUS:
            if isinstance(left, int) and isinstance(right, int):
                return left + right
            if isinstance(left, str) and isinstance(right, str):
                return left + right
        elif expr.operator.type == Token.Type.GREATER:
            if isinstance(left, int) and isinstance(right, int):
                return left > right
        elif expr.operator.type == Token.Type.GREATER_EQUAL:
            if isinstance(left, int) and isinstance(right, int):
                return left >= right
        elif expr.operator.type == Token.Type.LESS:
            if isinstance(left, int) and isinstance(right, int):
                return left < right
        elif expr.operator.type == Token.Type.LESS_EQUAL:
            if isinstance(left, int) and isinstance(right, int):
                return left <= right
        elif expr.operator.type == Token.Type.EQUAL_EQUAL:
            return self.isEqual(left, right)
        elif expr.operator.type == Token.Type.BANG_EQUAL:
            return not self.isEqual(left, right)

        raise ValueError("Unexpected types")

    def evaluateLogical(self, expr: expr.Logical) -> Literal:
        left = self.evaluate(expr.left)

        if expr.operator.type == Token.Type.OR:
            if self.isTruthy(left):
                return left
        else:
            if not self.isTruthy(left):
                return left

        return self.evaluate(expr.right)

    def evaluateUnary(self, expr: expr.Unary) -> Literal:
        right = self.evaluate(expr.right)

        if expr.operator.type == Token.Type.MINUS:
            if isinstance(right, int):
                return -right
        elif expr.operator.type == Token.Type.BANG:
            return not self.isTruthy(right)

        raise ValueError("Unexpected type")

    def evaluateGrouping(self, expr: expr.Grouping) -> Literal:
        return self.evaluate(expr.expression)

    def evaluateLiteral(self, expr: expr.Literal) -> Literal:
        return expr.value

    def evaluateVariable(self, expr: expr.Variable) -> Literal:
        return self.lookUpVariable(expr.name, expr)

    def evaluateAssign(self, expr: expr.Assign) -> Literal:
        value = self.evaluate(expr.value)

        distance = self.locals.get(expr)
        if distance:
            self.environment.assignAt(distance, expr.name, value)
        else:
            self.globals.assign(expr.name, value)
        return value

    def evaluateCall(self, expr: expr.Call) -> Literal:
        callee = self.evaluate(expr.callee)

        arguments = [self.evaluate(argument) for argument in expr.arguments]

        if isinstance(callee, Function):
            if len(arguments) == callee.arity:
                return callee.call(arguments)
            else:
                raise ValueError(f"Expected {callee.arity} arguments, but got {len(arguments)}.")
        else:
            raise ValueError("Can only call functions.")

    def evaluateObject(self, expr: expr.Object) -> Literal:
        fields = {name.lexeme: self.evaluate(expression) for name, expression in expr.fields.items()}

        return Object(fields)

    def evaluateGet(self, expr: expr.Get) -> Literal:
        object = self.evaluate(expr.object)
        name = expr.name.lexeme

        if isinstance(object, Object):
            if name in object.fields:
                return object.fields[name]
            else:
                raise ValueError(f"Object does not have property {name}.")
        else:
            raise ValueError(f"Cannot get property of an non object value.")

    def evaluateSet(self, expr: expr.Set) -> Literal:
        object = self.evaluate(expr.object)
        name = expr.name.lexeme

        if isinstance(object, Object):
            value = self.evaluate(expr.value)
            object.fields[name] = value

            return value
        else:
            raise ValueError(f"Cannot set property of an non object value.")

    def evaluate(self, expression: expr.Expr) -> Literal:
        if isinstance(expression, expr.Binary):
            return self.evaluateBinary(expression)
        if isinstance(expression, expr.Logical):
            return self.evaluateLogical(expression)
        if isinstance(expression, expr.Grouping):
            return self.evaluateGrouping(expression)
        if isinstance(expression, expr.Literal):
            return self.evaluateLiteral(expression)
        if isinstance(expression, expr.Unary):
            return self.evaluateUnary(expression)
        if isinstance(expression, expr.Variable):
            return self.evaluateVariable(expression)
        if isinstance(expression, expr.Assign):
            return self.evaluateAssign(expression)
        if isinstance(expression, expr.Call):
            return self.evaluateCall(expression)
        if isinstance(expression, expr.Object):
            return self.evaluateObject(expression)
        if isinstance(expression, expr.Get):
            return self.evaluateGet(expression)
        if isinstance(expression, expr.Set):
            return self.evaluateSet(expression)

    def execute(self, statement: stmt.Stmt) -> None:
        if isinstance(statement, stmt.Expression):
            self.executeExpressionStmt(statement)
        if isinstance(statement, stmt.Print):
            self.executePrintStmt(statement)
        if isinstance(statement, stmt.Return):
            self.executeReturnStmt(statement)
        if isinstance(statement, stmt.Block):
            self.executeBlockStmt(statement)
        if isinstance(statement, stmt.Var):
            self.executeVarStmt(statement)
        if isinstance(statement, stmt.Function):
            self.executeFunctionStmt(statement)
        if isinstance(statement, stmt.If):
            self.executeIfStmt(statement)
        if isinstance(statement, stmt.While):
            self.executeWhileStmt(statement)

    def resolve(self, expression: expr.Expr, depth: int) -> None:
        self.locals[expression] = depth

    def lookUpVariable(self, name: Token, expression: expr.Expr) -> Literal:
        if expression in self.locals:
            distance = self.locals[expression]
            return self.environment.getAt(distance, name.lexeme)
        else:
            return self.globals.get(name)
