from model import *


class PrettyPrinter(ASTNodeVisitor):
    SPACES_PER_INDENT = 4

    def __init__(self):
        self.indent_level = 0
        self.indent_disable_count = 0

    @staticmethod
    def statementify(block):
        return block + ';' if block and block[-1] != '}' else block

    def pretty_print(self, program):
        self.indent_level = 0
        self.indent_disable_count = 0
        return PrettyPrinter.statementify(program.accept(self))

    def indent(self):
        if self.indent_disable_count:
            return ''
        return ' ' * (PrettyPrinter.SPACES_PER_INDENT * self.indent_level)

    def visit_with_indent(self, body):
        self.indent_level += 1
        result = ''.join([self.statementify(statement.accept(self)) + '\n'
                          for statement in body or []])
        self.indent_level -= 1
        return result

    def visit_no_indent(self, expr):
        self.indent_disable_count += 1
        result = expr.accept(self)
        self.indent_disable_count -= 1
        return result

    def visit_number(self, number):
        return self.indent() + str(number.value)

    def visit_function(self, function):
        raise TypeError("PrettyPrinter shouldn't visit function object")

    def visit_function_definition(self, func_def):
        result = (self.indent() + 'def ' + func_def.name +
                  '(' + ', '.join(func_def.function.args) + ') {\n')
        result += self.visit_with_indent(func_def.function.body)
        result += self.indent() + '}'
        return result

    def visit_function_call(self, func_call):
        args_result = [self.visit_no_indent(arg) for arg in func_call.args]

        return (func_call.fun_expr.accept(self) +
                '(' + ', '.join(args_result) + ')')

    def visit_conditional(self, conditional):
        condition_result = self.visit_no_indent(conditional.condition)

        result = self.indent() + 'if (' + condition_result + ') {\n'
        result += self.visit_with_indent(conditional.if_true)
        result += self.indent() + '}'

        if conditional.if_false:
            result += ' else {\n'
            result += self.visit_with_indent(conditional.if_false)
            result += self.indent() + '}'

        return result

    def visit_print(self, print_):
        return self.indent() + f'print {self.visit_no_indent(print_.expr)}'

    def visit_read(self, read):
        return self.indent() + f'read {read.name}'

    def visit_reference(self, reference):
        return self.indent() + reference.name

    def visit_binary_operation(self, binary_operation):
        lhs_result = self.visit_no_indent(binary_operation.lhs)
        rhs_result = self.visit_no_indent(binary_operation.rhs)

        return (self.indent() +
                f'({lhs_result}) {binary_operation.op} ({rhs_result})')

    def visit_unary_operation(self, unary_operation):
        expr_result = self.visit_no_indent(unary_operation.expr)

        return self.indent() + f'{unary_operation.op}({expr_result})'


def pretty_print(program):
    print(PrettyPrinter().pretty_print(program))
