#!/usr/bin/env python3
import pytest
from textwrap import dedent

from printer import *


def test_pretty_conditional():
    assert PrettyPrinter().pretty_print(Conditional(
        Number(42), [], [])) == 'if (42) {\n}'


def test_pretty_function_definition():
    assert PrettyPrinter().pretty_print(FunctionDefinition(
        "foo", Function([], []))) == 'def foo() {\n}'


def test_pretty_print():
    assert PrettyPrinter().pretty_print(Print(Number(42))) == 'print 42;'


def test_pretty_read():
    assert PrettyPrinter().pretty_print(Read('x')) == 'read x;'


def test_pretty_number():
    assert PrettyPrinter().pretty_print(Number(10)) == '10;'


def test_pretty_reference():
    assert PrettyPrinter().pretty_print(Reference('x')) == 'x;'


def test_pretty_binary_operation():
    add = BinaryOperation(Number(2), '+', Number(3))
    mul = BinaryOperation(Number(0), '*', add)
    assert PrettyPrinter().pretty_print(mul) == '(0) * ((2) + (3));'


def test_pretty_unary_operation():
    assert (PrettyPrinter().pretty_print(UnaryOperation(
        '-', Number(42))) == '-(42);')


def test_pretty_function_call():
    assert PrettyPrinter().pretty_print(FunctionCall(Reference('foo'), [
        Number(1), Number(2), Number(3)])) == 'foo(1, 2, 3);'


def test_complex(capsys):
    pretty_print(FunctionDefinition('main', Function(['arg1'], [
        Read('x'),
        Print(Reference('x')),
        Conditional(
            BinaryOperation(Number(2), '==', Number(3)),
            [
                Conditional(Number(1), [], [])
            ],
            [
                FunctionCall(Reference('exit'), [
                    UnaryOperation('-', Reference('arg1'))
                ])
            ],
        ),
    ])))
    out = capsys.readouterr().out
    assert out == dedent(
        '''
        def main(arg1) {
            read x;
            print x;
            if ((2) == (3)) {
                if (1) {
                }
            } else {
                exit(-(arg1));
            }
        }
        ''').lstrip()


def test_nested(capsys):
    pretty_print(FunctionDefinition('main', Function([], [
        FunctionDefinition('plus', Function(['a', 'b'], [
            BinaryOperation(Reference('a'), '+', Reference('b'))
        ])),
        FunctionCall(Reference('plus'), [Number(200), Number(39)])
    ])))
    out = capsys.readouterr().out
    assert out == dedent(
        '''
        def main() {
            def plus(a, b) {
                (a) + (b);
            }
            plus(200, 39);
        }
        ''').lstrip()


if __name__ == '__main__':
    pytest.main()
