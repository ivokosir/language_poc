"""
program        → declaration* EOF ;

declaration    → funDecl
               | varDecl
               | statement ;

funDecl        → "fun" function ;
function       → IDENTIFIER "(" parameters? ")" block ;
parameters     → IDENTIFIER ( "," IDENTIFIER )* ;

statement      → exprStmt
               | ifStmt
               | returnStmt
               | printStmt
               | whileStmt
               | forStmt
               | block ;

varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;

exprStmt       → expression ";" ;
ifStmt         → "if" "(" expression ")" statement ( "else" statement )? ;
returnStmt     → "return" expression? ";" ;
printStmt      → "print" expression ";" ;
whileStmt      → "while" "(" expression ")" statement ;
forStmt        → "for" "(" ( varDecl | exprStmt | ";" )
                           expression? ";"
                           expression? ")" statement ;
block          → "{" declaration* "}" ;

expression     → assignment ;
assignment     → ( call "." )? IDENTIFIER "=" assignment
               | logic_or ;
logic_or       → logic_and ( "or" logic_and )* ;
logic_and      → equality ( "and" equality )* ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
addition       → multiplication ( ( "-" | "+" ) multiplication )* ;
multiplication → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary | call ;

call           → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
arguments      → expression ( "," expression )* ;

primary        → "true" | "false" | "nil"
               | NUMBER | STRING
               | "(" expression ")"
               | "object" "{" fields? "}"
               | IDENTIFIER ;

fields         → IDENTIFIER "=" expression ( "," IDENTIFIER "=" expression )* ;
"""

from typing import Dict, List, NoReturn
import sys

from token import Token
from literal import Literal
import expr
import stmt
import error

class Parser:
    def __init__(self, tokens: List[Token]) -> None:
        self.tokens = tokens
        self.current = 0

    def parse(self) -> List[stmt.Stmt]:
        statements: List[stmt.Stmt] = []

        while not self.is_at_end():
            statements.append(self.declaration())

        return statements

    def declaration(self) -> stmt.Stmt:
        if self.match(Token.Type.FUN):
            return self.function()
        if self.match(Token.Type.VAR):
            return self.var_declaration()
        else:
            return self.statement()

    def function(self) -> stmt.Stmt:
        name = self.consume(Token.Type.IDENTIFIER, "Expect function name.")

        self.consume(Token.Type.LEFT_PAREN, "Expect '(' after function name.")

        parameters: List[Token] = []
        if not self.check(Token.Type.RIGHT_PAREN):
            while True:
                if len(parameters) >= 255:
                    self.error(self.peek(), "Cannot have more than 255 parameters.")
                parameter = self.consume(Token.Type.IDENTIFIER, "Expect parameter name.")
                parameters.append(parameter)
                if not self.match(Token.Type.COMMA):
                    break

        self.consume(Token.Type.RIGHT_PAREN, "Expect ')' after parameters.")

        self.consume(Token.Type.LEFT_BRACE, "Expect '{' before function body.");

        body = self.block().statements

        return stmt.Function(name, parameters, body)

    def var_declaration(self) -> stmt.Stmt:
        name = self.consume(Token.Type.IDENTIFIER, "Expect variable name.")

        if self.match(Token.Type.EQUAL):
            initializer = self.expression()
        else:
            initializer = expr.Literal(None)

        self.consume(Token.Type.SEMICOLON, "Expect ';' after variable declaration.")
        return stmt.Var(name, initializer)

    def statement(self) -> stmt.Stmt:
        if self.match(Token.Type.IF):
            return self.if_statement()
        if self.match(Token.Type.WHILE):
            return self.while_statement()
        if self.match(Token.Type.FOR):
            return self.for_statement()
        if self.match(Token.Type.RETURN):
            return self.return_statement()
        if self.match(Token.Type.PRINT):
            return self.print_statement()
        if self.match(Token.Type.LEFT_BRACE):
            return self.block()
        else:
            return self.expression_statement()

    def if_statement(self) -> stmt.Stmt:
        self.consume(Token.Type.LEFT_PAREN, "Expect '(' after if statement.")
        condition = self.expression()
        self.consume(Token.Type.RIGHT_PAREN, "Expect ')' after if condition.")

        then_branch = self.statement()

        else_branch = None
        if self.match(Token.Type.ELSE):
            else_branch = self.statement()

        return stmt.If(condition, then_branch, else_branch)

    def while_statement(self) -> stmt.Stmt:
        self.consume(Token.Type.LEFT_PAREN, "Expect '(' after while statement.")
        condition = self.expression()
        self.consume(Token.Type.RIGHT_PAREN, "Expect ')' after while condition.")

        body = self.statement()

        return stmt.While(condition, body)


    def for_statement(self) -> stmt.Stmt:
        self.consume(Token.Type.LEFT_PAREN, "Expect '(' after for statement.")

        if self.match(Token.Type.SEMICOLON):
            initializer = None
        elif self.match(Token.Type.VAR):
            initializer = self.var_declaration()
        else:
            initializer = self.expression_statement()

        if self.check(Token.Type.SEMICOLON):
            condition = None
        else:
            condition = self.expression()

        self.consume(Token.Type.SEMICOLON, "Expect ';' after loop condition.")

        if self.check(Token.Type.RIGHT_PAREN):
            increment = None
        else:
            increment = self.expression()

        self.consume(Token.Type.RIGHT_PAREN, "Expect ')' after for clauses.")

        body = self.statement()

        if increment:
            body = stmt.Block([body, stmt.Expression(increment)])

        if not condition:
            condition = expr.Literal(True)

        body = stmt.While(condition, body)

        if initializer:
            body = stmt.Block([initializer, body])

        return body

    def return_statement(self) -> stmt.Stmt:
        keyword = self.previous()

        value: expr.Expr = expr.Literal(None)
        if not self.check(Token.Type.SEMICOLON):
            value = self.expression()

        self.consume(Token.Type.SEMICOLON, "Expect ';' after return statement.")
        return stmt.Return(keyword, value)

    def print_statement(self) -> stmt.Stmt:
        expression = self.expression()
        self.consume(Token.Type.SEMICOLON, "Expect ';' after print statement.")
        return stmt.Print(expression)

    def block(self) -> stmt.Block:
        statements: List[stmt.Stmt] = []

        while not self.check(Token.Type.RIGHT_BRACE) and not self.is_at_end():
            statements.append(self.declaration())

        self.consume(Token.Type.RIGHT_BRACE, "Expect '}' after block.")

        return stmt.Block(statements)

    def expression_statement(self) -> stmt.Stmt:
        expression = self.expression()
        self.consume(Token.Type.SEMICOLON, "Expect ';' after expression.")
        return stmt.Expression(expression)

    def error(self, token: Token, message: str) -> NoReturn:
        error.error(token, message)
        sys.exit(1)

    def consume(self, type: Token.Type, message: str) -> Token:
        if self.check(type):
            return self.advance()

        self.error(self.peek(), message)

    def is_at_end(self) -> bool:
        return self.peek().type == Token.Type.EOF

    def peek(self) -> Token:
        return self.tokens[self.current]

    def previous(self) -> Token:
        return self.tokens[self.current - 1]

    def advance(self) -> Token:
        if not self.is_at_end():
            self.current += 1
        return self.previous()

    def check(self, type: Token.Type) -> bool:
        if self.is_at_end():
            return False
        return self.peek().type == type

    def match(self, *types: Token.Type) -> bool:
        for type in types:
            if self.check(type):
                self.advance()
                return True

        return False

    def object(self) -> expr.Expr:
        self.consume(Token.Type.LEFT_BRACE, "Expect '{' after object keyword.")

        fields: Dict[Token, expr.Expr] = dict()

        if not self.check(Token.Type.RIGHT_BRACE):
            while True:
                name = self.consume(Token.Type.IDENTIFIER, "Expect field name.")
                self.consume(Token.Type.EQUAL, "Expect '=' after field name.")
                expression = self.expression()
                fields[name] = expression
                if not self.match(Token.Type.COMMA):
                    break

        self.consume(Token.Type.RIGHT_BRACE, "Expect '}' after object fields.")

        return expr.Object(fields)


    def primary(self) -> expr.Expr:
        if self.match(Token.Type.FALSE): return expr.Literal(False)
        if self.match(Token.Type.TRUE): return expr.Literal(True)
        if self.match(Token.Type.NIL): return expr.Literal(None)

        if self.match(Token.Type.NUMBER, Token.Type.STRING):
            return expr.Literal(self.previous().literal)

        if self.match(Token.Type.LEFT_PAREN):
            expression = self.expression()
            self.consume(Token.Type.RIGHT_PAREN, "Expect ')' after expression.")
            return expr.Grouping(expression)

        if self.match(Token.Type.IDENTIFIER):
            return expr.Variable(self.previous())

        if self.match(Token.Type.OBJECT):
            return self.object()

        self.error(self.peek(), "Expect expression.")

    def finish_call(self, callee: expr.Expr) -> expr.Expr:
        arguments: List[expr.Expr] = []

        if not self.check(Token.Type.RIGHT_PAREN):
            while True:
                if len(arguments) >= 255:
                    self.error(self.peek(), "Cannot have more than 255 arguments.")
                arguments.append(self.expression())
                if not self.match(Token.Type.COMMA):
                    break

        paren = self.consume(Token.Type.RIGHT_PAREN, "Expect ')' after arguments.")

        return expr.Call(callee, paren, arguments)

    def call(self) -> expr.Expr:
        expression = self.primary()

        while True:
            if self.match(Token.Type.LEFT_PAREN):
                expression = self.finish_call(expression)
            elif self.match(Token.Type.DOT):
                name = self.consume(Token.Type.IDENTIFIER, "Expect property name after '.'.")
                expression = expr.Get(expression, name)
            else:
                break;

        return expression

    def unary(self) -> expr.Expr:
        if self.match(Token.Type.BANG, Token.Type.MINUS):
            operator = self.previous()
            right = self.unary()
            return expr.Unary(operator, right)

        return self.call()

    def multiplication(self) -> expr.Expr:
        expression = self.unary()

        while self.match(Token.Type.SLASH, Token.Type.STAR):
            operator = self.previous()
            right = self.unary()
            expression = expr.Binary(expression, operator, right)

        return expression

    def addition(self) -> expr.Expr:
        expression = self.multiplication()

        while self.match(Token.Type.MINUS, Token.Type.PLUS):
            operator = self.previous()
            right = self.multiplication()
            expression = expr.Binary(expression, operator, right)

        return expression

    def comparison(self) -> expr.Expr:
        expression = self.addition()

        while self.match(Token.Type.GREATER, Token.Type.GREATER_EQUAL, Token.Type.LESS, Token.Type.LESS_EQUAL):
            operator = self.previous()
            right = self.addition()
            expression = expr.Binary(expression, operator, right)

        return expression

    def equality(self) -> expr.Expr:
        expression = self.comparison()

        while self.match(Token.Type.BANG_EQUAL, Token.Type.EQUAL_EQUAL):
            operator = self.previous()
            right = self.comparison()
            expression = expr.Binary(expression, operator, right)

        return expression

    def and_(self) -> expr.Expr:
        expression = self.equality()

        while self.match(Token.Type.AND):
            operator = self.previous()
            right = self.equality()
            expression = expr.Logical(expression, operator, right)

        return expression

    def or_(self) -> expr.Expr:
        expression = self.and_()

        while self.match(Token.Type.OR):
            operator = self.previous()
            right = self.and_()
            expression = expr.Logical(expression, operator, right)

        return expression

    def assignment(self) -> expr.Expr:
        expression = self.or_()

        if self.match(Token.Type.EQUAL):
            equals = self.previous()
            value = self.assignment()

            if isinstance(expression, expr.Variable):
                name = expression.name
                return expr.Assign(name, value)
            if isinstance(expression, expr.Get):
                return expr.Set(expression.object, expression.name, value)
            else:
                self.error(equals, "Invalid assignment target.")

        return expression

    def expression(self) -> expr.Expr:
        return self.assignment()
