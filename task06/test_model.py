#!/usr/bin/env python3
import pytest

from model import *


def test_construction():
    FunctionDefinition('fac', Function(['n'], [
        Conditional(
            BinaryOperation(Reference('n'), '==', Number(0)),
            [Number(1)],
            [
                BinaryOperation(
                    Reference('n'),
                    '*',
                    FunctionCall(Reference('fac'), [
                        BinaryOperation(
                            Reference('n'),
                            '-',
                            Number(1)
                        )
                    ])
                )
            ]
        )
    ]))
    Read('n')
    Print(
        UnaryOperation('-', FunctionCall(Reference('fac'), [Reference('n')]))
    )


def test_scope_ok():
    scope = Scope()

    b = [1, 2, 3]

    scope['a'] = 3
    scope['b'] = b

    assert scope['a'] is 3
    assert scope['b'] is b


def test_scope_exception():
    scope = Scope()

    scope['a'] = 3
    scope['123'] = 'a'

    with pytest.raises(KeyError):
        scope['hello']


def test_scope_parent():
    d = {123: 456, 'top': 'bottom'}

    parent = Scope()
    parent['a'] = d
    parent['b'] = 4

    scope = Scope(parent)
    scope['a'] = 5

    assert scope['a'] is 5
    assert scope['b'] is 4
    assert parent['a'] is d

    with pytest.raises(KeyError):
        scope['hello']


def test_scope_grand_parent():
    grand = Scope()
    parent = Scope(grand)
    scope = Scope(parent)

    var_a = '123'
    var_b = 32

    grand['a'] = var_a
    grand['b'] = var_b

    parent['a'] = 239

    scope['c'] = 123

    assert scope['a'] is 239
    assert scope['b'] is var_b
    assert scope['c'] is 123


def test_number():
    with pytest.raises(TypeError):
        n = Number()

    a = Number(3)
    b = Number(4)

    assert a != b
    assert a == Number(3)
    assert bool(Number(1)) is True
    assert bool(Number(0)) is False
    assert a.evaluate(Scope()) is a


def test_function():
    f = Function(['a', 'b'], [])
    assert f.evaluate(Scope()) is f


def test_function_definition():
    scope = Scope()
    func = Function(['a', 'b'], [])
    FunctionDefinition('func', func).evaluate(scope)

    assert scope['func'] is func


def test_function_call():
    scope = Scope()
    assert FunctionCall(Function([], [Number(42)]), []
                        ).evaluate(scope) == Number(42)


def test_conditional():
    assert Conditional(Number(0), [Number(239)], [
                       Number(30)]).evaluate(Scope()) == 30
    assert Conditional(Number(1), [Number(239)], [
                       Number(30)]).evaluate(Scope()) == 239
    assert Conditional(Number(1), None, []).evaluate(Scope()) == 0
    assert Conditional(Number(0), None, []).evaluate(Scope()) == 0


def test_print(capsys):
    Print(Number(239**2)).evaluate(Scope())

    assert int(capsys.readouterr().out) == 239**2


def test_read(monkeypatch):
    monkeypatch.setattr('builtins.input', lambda: '42')
    scope = Scope()
    Read('a').evaluate(scope)
    assert scope['a'] == 42


def test_reference():
    scope = Scope()

    a = Number(37)
    scope['a'] = a

    assert Reference('a').evaluate(scope) is a


def test_binary_operator():
    arr = [0, 30, 45, 239, 1337, 228, 1997]
    brr = [2, 52, 34, 1, 9, 88, 728]
    s = Scope()

    for a in arr:
        for b in brr:
            assert BinaryOperation(
                Number(a), '+', Number(b)).evaluate(s) == a + b
            assert BinaryOperation(
                Number(a), '-', Number(b)).evaluate(s) == a - b
            assert BinaryOperation(
                Number(a), '*', Number(b)).evaluate(s) == a * b
            assert BinaryOperation(
                Number(a), '/', Number(b)).evaluate(s) == a // b
            assert BinaryOperation(
                Number(a), '%', Number(b)).evaluate(s) == a % b
            assert BinaryOperation(
                Number(a), '==', Number(b)).evaluate(s) == (a == b)
            assert BinaryOperation(
                Number(a), '!=', Number(b)).evaluate(s) == (a != b)
            assert BinaryOperation(
                Number(a), '<', Number(b)).evaluate(s) == (a < b)
            assert BinaryOperation(
                Number(a), '>', Number(b)).evaluate(s) == (a > b)
            assert BinaryOperation(
                Number(a), '<=', Number(b)).evaluate(s) == (a <= b)
            assert BinaryOperation(
                Number(a), '>=', Number(b)).evaluate(s) == (a >= b)
            assert BinaryOperation(Number(a), '&&', Number(
                b)).evaluate(s) == (bool(a) and bool(b))
            assert BinaryOperation(Number(a), '||', Number(
                b)).evaluate(s) == (bool(a) or bool(b))


def test_unary_operator():
    assert UnaryOperation('-', Number(3)).evaluate(Scope()) == -3
    assert UnaryOperation('-', Number(-2000)).evaluate(Scope()) == 2000
    assert bool(UnaryOperation('!', Number(0)).evaluate(Scope())) is True
    assert bool(UnaryOperation('!', Number(23)).evaluate(Scope())) is False


def test_factorial():
    scope = Scope()

    FunctionDefinition('factorial', Function(['n'], [
        Conditional(BinaryOperation(Reference('n'), '!=', Number(1)),
                    [BinaryOperation(Reference('n'), '*',
                                     FunctionCall(Reference('factorial'),
                                                  [BinaryOperation(
                                                      Reference('n'),
                                                      '-',
                                                      Number(1))
                                                   ]))],
                    [Number(1)])
    ])).evaluate(scope)

    assert FunctionCall(Reference('factorial'), [
                        Number(1)]).evaluate(scope) == 1
    assert FunctionCall(Reference('factorial'), [
                        Number(2)]).evaluate(scope) == 2
    assert FunctionCall(Reference('factorial'), [
                        Number(3)]).evaluate(scope) == 6
    assert FunctionCall(Reference('factorial'), [
                        Number(4)]).evaluate(scope) == 24
    assert FunctionCall(Reference('factorial'), [
                        Number(5)]).evaluate(scope) == 120
    assert FunctionCall(Reference('factorial'), [
                        Number(10)]).evaluate(scope) == 3628800


def test_eq_number():
    assert Number(0) == Number(0)
    assert Number(1) == Number(1)


def test_eq_function():
    assert Function(['a', 'b'], [Number(0)]) == Function(
        ['a', 'b'], [Number(0)])
    assert FunctionDefinition(
        'plus', Function(
            ['a, b'], [
                BinaryOperation(
                    Reference('a'),
                    '+',
                    Reference('b'))])) == FunctionDefinition(
        'plus', Function(
            ['a, b'], [
                BinaryOperation(
                    Reference('a'), '+', Reference('b'))]))
    assert FunctionCall('plus', [Number(1), Number(2)]) == FunctionCall(
        'plus', [Number(1), Number(2)])


def test_eq_conditional():
    assert Conditional(Number(0), [], None) == Conditional(Number(0), None, [])


def test_eq_print():
    assert Print(Number(239)) == Print(Number(239))


def test_eq_read():
    assert Read('abc') == Read('abc')


def test_eq_reference():
    assert Reference('x') == Reference('x')


def test_eq_binary_op():
    assert BinaryOperation(Number(0), '+', Number(1)
                           ) == BinaryOperation(Number(0), '+', Number(1))


def test_eq_unary_op():
    assert UnaryOperation('-', Number(1)) == UnaryOperation('-', Number(1))


if __name__ == '__main__':
    pytest.main()
