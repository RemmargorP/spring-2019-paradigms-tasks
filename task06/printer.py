#!/usr/bin/env python3

from model import *


class PrettyPrinter(ASTNodeVisitor):
    SPACES_PER_INDENT = 4

    def __init__(self):
        self.indent_level = 0
        self.indent_disable_count = 0

    def pretty_print(self, program):
        self.indent_level = 0
        self.indent_disable_count = 0
        return program.accept(self) + ';'

    def indent(self):
        return ' ' * (PrettyPrinter.SPACES_PER_INDENT *
                      self.indent_level) if self.indent_disable_count == 0 \
            else ''

    def visit_number(self, number):
        return self.indent() + str(number.value)

    def visit_function(self, function):
        raise RuntimeError('PrettyPrinter shouldn\'t visit function object')

    def visit_function_definition(self, func_def):
        result = self.indent() + 'def ' + func_def.name + \
            '(' + ', '.join(func_def.function.args) + ') {\n'
        self.indent_level += 1
        for statement in func_def.function.body or []:
            result += statement.accept(self) + ';\n'
        self.indent_level -= 1
        result += '}'
        return result

    def visit_function_call(self, func_call):
        self.indent_disable_count += 1
        args_result = []
        for arg in func_call.args:
            args_result.append(arg.accept(self))
        self.indent_disable_count -= 1
        return func_call.fun_expr.accept(
            self) + '(' + ', '.join(args_result) + ')'

    def visit_conditional(self, conditional):
        self.indent_disable_count += 1
        condition_result = conditional.condition.accept(self)
        self.indent_disable_count -= 1

        result = self.indent() + 'if (' + condition_result + ') {\n'

        self.indent_level += 1
        for statement in conditional.if_true or []:
            result += statement.accept(self) + ';\n'
        self.indent_level -= 1

        result += self.indent() + '}'

        if conditional.if_false:
            result += ' else {\n'

            self.indent_level += 1
            for statement in conditional.if_false:
                result += statement.accept(self) + ';\n'
            self.indent_level -= 1

            result += self.indent() + '}'

        return result

    def visit_print(self, print_):
        self.indent_disable_count += 1
        expr_result = print_.expr.accept(self)
        self.indent_disable_count -= 1
        return self.indent() + f'print {expr_result}'

    def visit_read(self, read):
        return self.indent() + f'read {read.name}'

    def visit_reference(self, reference):
        return self.indent() + reference.name

    def visit_binary_operation(self, binary_operation):
        self.indent_disable_count += 1
        lhs_result = binary_operation.lhs.accept(self)
        rhs_result = binary_operation.rhs.accept(self)
        self.indent_disable_count -= 1
        return self.indent() + \
            f'({lhs_result}) {binary_operation.op} ({rhs_result})'

    def visit_unary_operation(self, unary_operation):
        self.indent_disable_count += 1
        expr_result = unary_operation.expr.accept(self)
        self.indent_disable_count -= 1
        return self.indent() + f'{unary_operation.op}({expr_result})'


def pretty_print(program):
    print(PrettyPrinter().pretty_print(program))
