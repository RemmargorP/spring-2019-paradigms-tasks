#!/usr/bin/env python3
import abc


class Scope:
    def __init__(self, parent=None):
        self.variables = {}
        self.parent = parent

    def __getitem__(self, key):
        if key in self.variables:
            return self.variables[key]
        if self.parent is not None:
            return self.parent[key]
        raise KeyError(key)

    def __setitem__(self, key, value):
        self.variables[key] = value


class ASTNode(metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def evaluate(self, scope):
        """
        Запускает вычисление текущего узла синтаксического дерева
        в заданной области видимости и возвращает результат вычисления.
        """

    @abc.abstractmethod
    def accept(self, visitor):
        pass

    @abc.abstractmethod
    def __eq__(self, node):
        pass


class ASTNodeVisitor(metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def visit_number(self, number):
        pass

    @abc.abstractmethod
    def visit_function(self, function):
        pass

    @abc.abstractmethod
    def visit_function_definition(self, func_definition):
        pass

    @abc.abstractmethod
    def visit_function_call(self, func_call):
        pass

    @abc.abstractmethod
    def visit_conditional(self, conditional):
        pass

    @abc.abstractmethod
    def visit_print(self, print_):
        pass

    @abc.abstractmethod
    def visit_read(self, read):
        pass

    @abc.abstractmethod
    def visit_reference(self, reference):
        pass

    @abc.abstractmethod
    def visit_binary_operation(self, binary_operation):
        pass

    @abc.abstractmethod
    def visit_unary_operation(self, unary_operation):
        pass


class Number(ASTNode):
    """
    Представляет собой константу или значение типа "целое число".

    Метод evaluate() всегда возвращает self.

    Number должен содержать поле value, которое будет хранить число,
    переданное в конструкторе.

    Также Number должен корректно работать с операторами ==, != и его должно
    быть можно положить в словарь в качестве ключа (см. специальные методы
    __eq__, __ne__, __hash__ — требуется реализовать две из них).
    """

    def __init__(self, value):
        assert isinstance(value, int)
        self.value = value

    def evaluate(self, scope):
        return self

    def accept(self, visitor):
        return visitor.visit_number(self)

    def __eq__(self, other):
        if isinstance(other, int):
            return self.value == other
        if isinstance(other, Number):
            return self.value == other.value
        raise TypeError

    def __hash__(self):
        return self.value

    def __bool__(self):
        return bool(self.value)

    def __str__(self):
        return str(self.value)


class Function(ASTNode):
    """
    Представляет собой константу или значение типа "функция".

    Функция состоит из тела и списка имен аргументов.
    Тело функции — это список выражений, т. е. у каждого объекта в списке
    можно вызвать evaluate.
    Список имен аргументов - список имен формальных параметров функции.

    Аналогично Number, метод evaluate должен возвращать self.
    """

    def __init__(self, args, body):
        self.args = args
        self.body = body

    def evaluate(self, scope):
        return self

    def accept(self, visitor):
        return visitor.visit_function(self)

    def __eq__(self, function):
        return isinstance(function, Function) and\
            self.args == function.args and self.body == function.body


class FunctionDefinition(ASTNode):
    """
    Представляет собой определение функции, т. е. связывает некоторое
    имя с объектом типа Function.

    Результатом вычисления FunctionDefinition является побочный эффект -
    обновление текущего Scope,  т.е. в него добавляется новое значение типа
    Function под заданным именем, а возвращать evaluate должен саму функцию.
    """

    def __init__(self, name, function):
        self.name = name
        self.function = function

    def evaluate(self, scope):
        scope[self.name] = self.function
        return self.function

    def accept(self, visitor):
        return visitor.visit_function_definition(self)

    def __eq__(self, func_def):
        return isinstance(func_def, FunctionDefinition) and\
            self.name == func_def.name and self.function == func_def.function


class FunctionCall(ASTNode):
    """
    Представляет вызов функции в программе.

    В результате вызова функции должен создаваться новый объект Scope,
    являющийся дочерним для текущего Scope (т.е. текущий Scope должен стать
    для него родителем). Новый Scope станет текущим Scope-ом при вычислении
    тела функции.

    Метод evaluate должен вычислить fun_expr и результатом этого вычисления
    будет объект типа Function (назовем его function). Кроме того, он должен
    вычислить все объекты в списке args слева направо,  результаты этих
    вычислений будут позиционными аргументами при вызове функции.

    Затем метод должен создать новый Scope (назовем его call_scope), родителем
    которого является scope. В call_scope должны быть добавлены результаты
    вычисления args, под именами, указанными в объекте Function, в
    соответствующем порядке.

    После этого нужно вычислить все выражения в теле function с использованием
    call_scope, результат вычисления последнего выражения будет результатом
    метода evaluate. Если результат вычисления последнего выражения
    неопределён, то возвращаемое значение остаётся на ваше усмотрение.
    """

    def __init__(self, fun_expr, args):
        self.fun_expr = fun_expr
        self.args = args

    def evaluate(self, scope):
        function = self.fun_expr.evaluate(scope)
        call_scope = Scope(scope)

        for (arg_name, arg) in zip(function.args, self.args):
            call_scope[arg_name] = arg.evaluate(scope)

        last_result = Number(0)

        for op in function.body:
            last_result = op.evaluate(call_scope)

        return last_result

    def accept(self, visitor):
        return visitor.visit_function_call(self)

    def __eq__(self, func_call):
        return isinstance(func_call, FunctionCall) and\
            self.fun_expr == func_call.fun_expr and self.args == func_call.args


class Conditional(ASTNode):
    """
    Представляет ветвление в программе, т. е. if.

    condition - это некоторое выражение, результат вычисления которого
        обязательно является объектом типа Number.
    if_true и if_false - списки (возможно, пустые или равные None) выражений.

    Если результат вычисления condition - это объект Number, содержащий 0, то
    вычисляется if_false список, иначе if_true.

    Результатом вычисления всего Conditional является результат вычисления
    последнего элемента в соответствующем (if_true или if_false) списке.

    Если соответствующий список пуст или равен None, то возвращаемое значение
    остается на ваше усмотрение.
    """

    def __init__(self, condition, if_true, if_false=None):
        self.condition = condition
        self.if_true = if_true
        self.if_false = if_false

    def evaluate(self, scope):
        branch = self.if_true if bool(self.condition.evaluate(
            scope)) else self.if_false

        last_result = Number(0)

        for op in branch or []:
            last_result = op.evaluate(scope)

        return last_result

    def accept(self, visitor):
        return visitor.visit_conditional(self)

    def __eq__(self, conditional):
        return isinstance(
            conditional,
            Conditional) and self.condition == conditional.condition and (
            self.if_true or []) == (
            conditional.if_true or []) and (
                self.if_false or []) == (
                    conditional.if_false or [])


class Print(ASTNode):
    """
    Печатает значение выражения на отдельной строке.

    В методе evaluate вычисляется значение выражения expr, после чего
    на экран выводится число, хранящееся внутри результата вычисления (это
    гарантированно Number).

    Вывод завершается переходом на следующую строку (никаких дополнительных
    символов, лишнего форматирования, научных форматов, отступов и пр).

    Возвращаемое значение метода evаluate - объект типа Number, который был
    выведен.
    """

    def __init__(self, expr):
        self.expr = expr

    def evaluate(self, scope):
        result = self.expr.evaluate(scope)
        print(result)
        return result

    def accept(self, visitor):
        return visitor.visit_print(self)

    def __eq__(self, print):
        return isinstance(print, Print) and self.expr == print.expr


class Read(ASTNode):
    """
    Читает число из стандартного потока ввода и обновляет текущий Scope.

    Метод evaluate читает со стандартного потока ввода число (на отдельной
    строке) и добавляет в scope это число под именем name.

    evaluate должен возвращать объект типа Number, представляющий прочитанное
    число.

    Каждое входное число располагается на отдельной строке (никаких пустых
    строк и лишних символов не будет).
    """

    def __init__(self, name):
        self.name = name

    def evaluate(self, scope):
        value = Number(int(input()))
        scope[self.name] = value
        return value

    def accept(self, visitor):
        return visitor.visit_read(self)

    def __eq__(self, read):
        return isinstance(read, Read) and self.name == read.name


class Reference(ASTNode):
    """
    Представляет получение объекта (функции или переменной) по его имени.
    Метод evaluate должен найти в scope объект с именем name и вернуть его
    (см. подробнее про класс Scope).
    """

    def __init__(self, name):
        self.name = name

    def evaluate(self, scope):
        return scope[self.name]

    def accept(self, visitor):
        return visitor.visit_reference(self)

    def __eq__(self, reference):
        return isinstance(reference, Reference) and self.name == reference.name


class BinaryOperation(ASTNode):
    """
    Представляет бинарную операция над двумя выражениями.
    Результатом вычисления бинарной операции является объект Number.
    Поддерживаемые операции: '+', '-', '*', '/', '%', '==', '!=',
    '<', '>', '<=', '>=', '&&', '||'.

    lhs и rhs - левое и правое выражения соответственно.
    op - строка с обозначением оператора (все допустимые строки приведены
    выше).

    Метод evaluate должен вычислить значение lhs и rhs, и вернуть Number,
    хранящий значение соответствующей бинарной операции над результатами
    вычисления lhs и rhs.

    Для логических операций и операций сравнения считаем, что Number,
    хранящий 0, соответствует False, а остальные значения соответствуют True.
    Гарантируется, что lhs и rhs при вычислении дадут объект типа Number,
    т.е. не может получиться так, что вам придется сравнивать две функции.
    """

    __OPERATORS = {
        '+': lambda x, y: Number(x.value + y.value),
        '-': lambda x, y: Number(x.value - y.value),
        '*': lambda x, y: Number(x.value * y.value),
        '/': lambda x, y: Number(x.value // y.value),
        '%': lambda x, y: Number(x.value % y.value),
        '==': lambda x, y: Number(int(x.value == y.value)),
        '!=': lambda x, y: Number(int(x.value != y.value)),
        '<': lambda x, y: Number(int(x.value < y.value)),
        '>': lambda x, y: Number(int(x.value > y.value)),
        '<=': lambda x, y: Number(int(x.value <= y.value)),
        '>=': lambda x, y: Number(int(x.value >= y.value)),
        '&&': lambda x, y: Number(int(bool(x) and bool(y))),
        '||': lambda x, y: Number(int(bool(x) or bool(y))),
    }

    def __init__(self, lhs, op, rhs):
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

        if op not in BinaryOperation.__OPERATORS:
            raise SyntaxError(f'operator {op} is not supported')

    def evaluate(self, scope):
        left = self.lhs.evaluate(scope)
        right = self.rhs.evaluate(scope)

        if not isinstance(left, Number):
            raise TypeError('Left operand is not a Number')
        if not isinstance(right, Number):
            raise TypeError('Right operand is not a Number')

        return BinaryOperation.__OPERATORS[self.op](left, right)

    def accept(self, visitor):
        return visitor.visit_binary_operation(self)

    def __eq__(self, bin_op):
        return isinstance(bin_op, BinaryOperation) and\
            self.op == bin_op.op and self.lhs == bin_op.lhs and\
            self.rhs == bin_op.rhs


class UnaryOperation(ASTNode):
    """
    Представляет унарную операцию над выражением.
    Результатом вычисления унарной операции является объект Number.
    Поддерживаемые операции: '-', '!' (логическое отрицание, а не факториал).

    Метод evaluate должен вычислить expr, и вернуть Number, хранящий значение
    соответствующей унарной операции над результатом вычисления expr.
    Как и для BinaryOperation, Number, хранящий 0, считаем за False, а все
    остальные за True.
    """

    def __init__(self, op, expr):
        self.op = op
        self.expr = expr

    def evaluate(self, scope):
        result = self.expr.evaluate(scope)

        if self.op == '-':
            return Number(-result.value)
        if self.op == '!':
            return Number(int(not bool(result)))

        return SyntaxError(f'unary operator {op} is not supported')

    def accept(self, visitor):
        return visitor.visit_unary_operation(self)

    def __eq__(self, un_op):
        return isinstance(un_op, UnaryOperation) and\
            self.op == un_op.op and self.expr == un_op.expr
