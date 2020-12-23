from typing import Dict, List

from error import error
from interpreter import Interpreter
from token import Token
import expr
import stmt


class Resolver:
    def __init__(self, interpreter: Interpreter) -> None:
        self.scopes: List[Dict[str, bool]] = []
        self.in_function = False
        self.interpreter = interpreter


    def resolve(self, statements: List[stmt.Stmt]) -> None:
        for statement in statements:
            self.resolve_stmt(statement)

    def resolve_stmt(self, statement: stmt.Stmt) -> None:
        if isinstance(statement, stmt.Expression):
            self.resolve_expression(statement)
        if isinstance(statement, stmt.Print):
            self.resolve_print(statement)
        if isinstance(statement, stmt.Var):
            self.resolve_var(statement)
        if isinstance(statement, stmt.Block):
            self.resolve_block(statement)
        if isinstance(statement, stmt.If):
            self.resolve_if(statement)
        if isinstance(statement, stmt.While):
            self.resolve_while(statement)
        if isinstance(statement, stmt.Function):
            self.resolve_function(statement)
        if isinstance(statement, stmt.Return):
            self.resolve_return(statement)

    def resolve_expression(self, statement: stmt.Expression) -> None:
        self.resolve_expr(statement.expr)

    def resolve_print(self, statement: stmt.Print) -> None:
        self.resolve_expr(statement.expr)

    def resolve_var(self, statement: stmt.Var) -> None:
        self.declare(statement.name)
        self.resolve_expr(statement.initializer)
        self.define(statement.name)

    def resolve_block(self, statement: stmt.Block) -> None:
        self.begin_scope()
        self.resolve(statement.statements)
        self.end_scope()

    def resolve_if(self, statement: stmt.If) -> None:
        self.resolve_expr(statement.condition)
        self.resolve_stmt(statement.then_branch)
        if statement.else_branch:
            self.resolve_stmt(statement.else_branch)

    def resolve_while(self, statement: stmt.While) -> None:
        self.resolve_expr(statement.condition)
        self.resolve_stmt(statement.body)

    def resolve_function(self, statement: stmt.Function) -> None:
        was_in_function = self.in_function
        self.in_function = True

        self.declare(statement.name)
        self.define(statement.name)

        self.begin_scope()

        for param in statement.params:
            self.declare(param)
            self.define(param)

        self.resolve(statement.body)

        self.end_scope()

        self.in_function = was_in_function


    def resolve_return(self, statement: stmt.Return) -> None:
        if self.in_function:
            self.resolve_expr(statement.value)
        else:
            error(statement.keyword, "Cannot return from top-level code.")

    def resolve_expr(self, expression: expr.Expr) -> None:
        if isinstance(expression, expr.Binary):
            self.resolve_binary(expression)
        if isinstance(expression, expr.Unary):
            self.resolve_unary(expression)
        if isinstance(expression, expr.Grouping):
            self.resolve_grouping(expression)
        if isinstance(expression, expr.Logical):
            self.resolve_logical(expression)
        if isinstance(expression, expr.Literal):
            self.resolve_literal(expression)
        if isinstance(expression, expr.Variable):
            self.resolve_variable(expression)
        if isinstance(expression, expr.Assign):
            self.resolve_assign(expression)
        if isinstance(expression, expr.Call):
            self.resolve_call(expression)
        if isinstance(expression, expr.Object):
            self.resolve_object(expression)
        if isinstance(expression, expr.Get):
            self.resolve_get(expression)
        if isinstance(expression, expr.Set):
            self.resolve_set(expression)

    def resolve_binary(self, expression: expr.Binary) -> None:
        self.resolve_expr(expression.left)
        self.resolve_expr(expression.right)

    def resolve_unary(self, expression: expr.Unary) -> None:
        self.resolve_expr(expression.right)

    def resolve_grouping(self, expression: expr.Grouping) -> None:
        self.resolve_expr(expression.expression)

    def resolve_logical(self, expression: expr.Logical) -> None:
        self.resolve_expr(expression.left)
        self.resolve_expr(expression.right)

    def resolve_literal(self, expression: expr.Literal) -> None:
        pass

    def resolve_variable(self, expression: expr.Variable) -> None:
        if self.scopes and self.scopes[-1].get(expression.name.lexeme) == False:
            error(expression.name, "Cannot read local variable in its own initializer.")

        self.resolve_local(expression, expression.name)

    def resolve_assign(self, expression: expr.Assign) -> None:
        self.resolve_expr(expression.value)
        self.resolve_local(expression, expression.name)

    def resolve_call(self, expression: expr.Call) -> None:
        self.resolve_expr(expression.callee)

        for argument in expression.arguments:
            self.resolve_expr(argument)

    def resolve_object(self, expression: expr.Object) -> None:
        used_names: List[str] = []

        for name, field in expression.fields.items():
            if name.lexeme in used_names:
                error(name, "Field with this name is already in object.")
            used_names.append(name.lexeme)
            self.resolve_expr(field)

    def resolve_get(self, expression: expr.Get) -> None:
        self.resolve_expr(expression.object)

    def resolve_set(self, expression: expr.Set) -> None:
        self.resolve_expr(expression.object)
        self.resolve_expr(expression.value)

    def begin_scope(self) -> None:
        self.scopes.append(dict())

    def end_scope(self) -> None:
        self.scopes.pop()

    def declare(self, name: Token) -> None:
        if self.scopes:
            scope = self.scopes[-1]

            if name.lexeme in scope:
                error(name, "Variable with this name already declared in this scope.")

            scope[name.lexeme] = False

    def define(self, name: Token) -> None:
        if self.scopes:
            scope = self.scopes[-1]
            scope[name.lexeme] = True

    def resolve_local(self, expression: expr.Expr, name: Token) -> None:
        for i, scope in reversed(list(enumerate(self.scopes))):
            if name.lexeme in scope:
                self.interpreter.resolve(expression, i)
