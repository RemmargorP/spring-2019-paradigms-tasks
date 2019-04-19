from model import *


class PrettyPrinter(ASTNodeVisitor):
    SPACES_PER_INDENT = 4

    class Indent:
        def __init__(self, pretty_printer):
            self.pretty_printer = pretty_printer

        def __enter__(self):
            self.pretty_printer.indent_level += 1

        def __exit__(self, type, value, traceback):
            self.pretty_printer.indent_level -= 1

    class IndentDisabled:
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

    def visit(self, program):
        self.indent_level = 0
        self.indent_disable_count = 0
        return PrettyPrinter.statementify(program.accept(self))

    def indent(self):
        if self.indent_disable_count != 0:
            return ''
        return ' ' * (PrettyPrinter.SPACES_PER_INDENT * self.indent_level)

    def visit_number(self, number):
        return self.indent() + str(number.value)

    def visit_function(self, function):
        raise TypeError("PrettyPrinter shouldn't visit function object")

    def visit_function_definition(self, func_def):
        result = (self.indent() + 'def ' + func_def.name +
                  '(' + ', '.join(func_def.function.args) + ') {\n')
        with PrettyPrinter.Indent(self):
            for statement in func_def.function.body or []:
                result += PrettyPrinter.statementify(
                    statement.accept(self)) + '\n'
        result += self.indent() + '}'
        return result

    def visit_function_call(self, func_call):
        with PrettyPrinter.IndentDisabled(self):
            args_result = [arg.accept(self) for arg in func_call.args]

        return func_call.fun_expr.accept(
            self) + '(' + ', '.join(args_result) + ')'

    def visit_conditional(self, conditional):
        with PrettyPrinter.IndentDisabled(self):
            condition_result = conditional.condition.accept(self)

        result = self.indent() + 'if (' + condition_result + ') {\n'

        with PrettyPrinter.Indent(self):
            for statement in conditional.if_true or []:
                result += PrettyPrinter.statementify(
                    statement.accept(self)) + '\n'

        result += self.indent() + '}'

        if conditional.if_false:
            result += ' else {\n'

            with PrettyPrinter.Indent(self):
                for statement in conditional.if_false:
                    result += PrettyPrinter.statementify(
                        statement.accept(self)) + '\n'

            result += self.indent() + '}'

        return result

    def visit_print(self, print_):
        with PrettyPrinter.IndentDisabled(self):
            expr_result = print_.expr.accept(self)

        return self.indent() + f'print {expr_result}'

    def visit_read(self, read):
        return self.indent() + f'read {read.name}'

    def visit_reference(self, reference):
        return self.indent() + reference.name

    def visit_binary_operation(self, binary_operation):
        with PrettyPrinter.IndentDisabled(self):
            lhs_result = binary_operation.lhs.accept(self)
            rhs_result = binary_operation.rhs.accept(self)

        return (self.indent() +
                f'({lhs_result}) {binary_operation.op} ({rhs_result})')

    def visit_unary_operation(self, unary_operation):
        with PrettyPrinter.IndentDisabled(self):
            expr_result = unary_operation.expr.accept(self)

        return self.indent() + f'{unary_operation.op}({expr_result})'


def pretty_print(program):
    print(PrettyPrinter().visit(program))
