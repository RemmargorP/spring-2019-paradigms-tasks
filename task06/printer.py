#!/usr/bin/env python3

from model import *


class PrettyPrinter(ASTNodeVisitor):
    SPACES_PER_INDENT = 4

    class __Indent:
        def __init__(self, pretty_printer):
            self.pretty_printer = pretty_printer

        def __enter__(self):
            self.pretty_printer.indent_level += 1

        def __exit__(self, type, value, traceback):
            self.pretty_printer.indent_level -= 1

    class __IndentDisabled:
        def __init__(self, pretty_printer):
            self.pretty_printer = pretty_printer

        def __enter__(self):
            self.pretty_printer.indent_disable_count += 1

        def __exit__(self, type, value, traceback):
            self.pretty_printer.indent_disable_count -= 1

    def __init__(self):
        self.indent_level = 0
        self.indent_disable_count = 0

    def statementify(block):
        return block + ';' if block and block[-1] != '}' else block

    def pretty_print(self, program):
        self.indent_level = 0
        self.indent_disable_count = 0
        return PrettyPrinter.statementify(program.accept(self))

    def indent(self):
        return ' ' * (PrettyPrinter.SPACES_PER_INDENT *
                      self.indent_level) if self.indent_disable_count == 0 \
            else ''

    def visit_number(self, number):
        return self.indent() + str(number.value)

    def visit_function(self, function):
        raise TypeError('PrettyPrinter shouldn\'t visit function object')

    def visit_function_definition(self, func_def):
        result = self.indent() + 'def ' + func_def.name + \
            '(' + ', '.join(func_def.function.args) + ') {\n'
        with PrettyPrinter.__Indent(self):
            for statement in func_def.function.body or []:
                result += PrettyPrinter.statementify(
                    statement.accept(self)) + '\n'
        result += self.indent() + '}'
        return result

    def visit_function_call(self, func_call):
        with PrettyPrinter.__IndentDisabled(self):
            args_result = []
            for arg in func_call.args:
                args_result.append(arg.accept(self))
        return func_call.fun_expr.accept(
            self) + '(' + ', '.join(args_result) + ')'

    def visit_conditional(self, conditional):
        with PrettyPrinter.__IndentDisabled(self):
            condition_result = conditional.condition.accept(self)

        result = self.indent() + 'if (' + condition_result + ') {\n'

        with PrettyPrinter.__Indent(self):
            for statement in conditional.if_true or []:
                result += PrettyPrinter.statementify(
                    statement.accept(self)) + '\n'

        result += self.indent() + '}'

        if conditional.if_false:
            result += ' else {\n'

            with PrettyPrinter.__Indent(self):
                for statement in conditional.if_false:
                    result += PrettyPrinter.statementify(
                        statement.accept(self)) + '\n'

            result += self.indent() + '}'

        return result

    def visit_print(self, print_):
        with PrettyPrinter.__IndentDisabled(self):
            expr_result = print_.expr.accept(self)

        return self.indent() + f'print {expr_result}'

    def visit_read(self, read):
        return self.indent() + f'read {read.name}'

    def visit_reference(self, reference):
        return self.indent() + reference.name

    def visit_binary_operation(self, binary_operation):
        with PrettyPrinter.__IndentDisabled(self):
            lhs_result = binary_operation.lhs.accept(self)
            rhs_result = binary_operation.rhs.accept(self)

        return self.indent() + \
            f'({lhs_result}) {binary_operation.op} ({rhs_result})'

    def visit_unary_operation(self, unary_operation):
        self.indent_disable_count += 1
        expr_result = unary_operation.expr.accept(self)
        self.indent_disable_count -= 1
        return self.indent() + f'{unary_operation.op}({expr_result})'


def pretty_print(program):
    print(PrettyPrinter().pretty_print(program))
