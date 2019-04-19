from model import *


class ConstantFolder(ASTNodeVisitor):
    def visit(self, program):
        return program.accept(self)

    def visit_number(self, number):
        return Number(number.value)

    def visit_function(self, function):
        body_result = [statement.accept(self)
                       for statement in function.body or []]
        return Function(function.args.copy(), body_result)

    def visit_function_definition(self, func_def):
        return FunctionDefinition(
            func_def.name,
            func_def.function.accept(self)
        )

    def visit_function_call(self, func_call):
        args_result = [arg.accept(self) for arg in func_call.args or []]
        return FunctionCall(func_call.fun_expr.accept(self), args_result)

    def visit_conditional(self, conditional):
        condition_result = conditional.condition.accept(self)

        if_true_result = [statement.accept(self)
                          for statement in conditional.if_true or []]
        if_false_result = [statement.accept(
            self) for statement in conditional.if_false or []]

        return Conditional(condition_result, if_true_result, if_false_result)

    def visit_print(self, print_):
        return Print(print_.expr.accept(self))

    def visit_read(self, read):
        return Read(read.name)

    def visit_reference(self, reference):
        return Reference(reference.name)

    def visit_binary_operation(self, binary_operation):
        op = binary_operation.op
        lhs = binary_operation.lhs.accept(self)
        rhs = binary_operation.rhs.accept(self)

        result = BinaryOperation(lhs, op, rhs)

        if isinstance(lhs, Number) and isinstance(rhs, Number):
            result = result.evaluate(Scope())
        elif ((lhs == 0 and op == '*' and isinstance(rhs, Reference)) or
              (rhs == 0 and op == '*' and isinstance(lhs, Reference))):
            result = Number(0)
        elif (isinstance(lhs, Reference) and isinstance(rhs, Reference) and
                op == '-' and lhs.name == rhs.name):
            result = Number(0)

        return result

    def visit_unary_operation(self, unary_operation):
        expr = unary_operation.expr.accept(self)
        result = UnaryOperation(unary_operation.op, expr)
        if isinstance(expr, Number):
            result = result.evaluate(Scope())
        return result


def fold_constants(program):
    return ConstantFolder().visit(program)
