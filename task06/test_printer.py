#!/usr/bin/env python3
import pytest
from textwrap import dedent

from printer import *


def test_pretty_conditional():
    assert PrettyPrinter().visit(Conditional(
        Number(42), [], [])).rstrip() == 'if (42) {\n}'


def test_pretty_function_definition():
    assert PrettyPrinter().visit(FunctionDefinition(
        "foo", Function([], []))).rstrip() == 'def foo() {\n}'


def test_pretty_print():
    assert PrettyPrinter().visit(Print(Number(42))).rstrip() == 'print 42;'


def test_pretty_read():
    assert PrettyPrinter().visit(Read('x')).rstrip() == 'read x;'


def test_pretty_number():
    assert PrettyPrinter().visit(Number(10)).rstrip() == '10;'


def test_pretty_reference():
    assert PrettyPrinter().visit(Reference('x')).rstrip() == 'x;'


def test_pretty_binary_operation():
    add = BinaryOperation(Number(2), '+', Number(3))
    mul = BinaryOperation(Number(0), '*', add)
    assert PrettyPrinter().visit(mul).rstrip() == '(0) * ((2) + (3));'


def test_pretty_unary_operation():
    assert (PrettyPrinter().visit(UnaryOperation(
        '-', Number(42))).rstrip() == '-(42);')


def test_pretty_function_call():
    assert PrettyPrinter().visit(FunctionCall(Reference('foo'), [
        Number(1), Number(2), Number(3)])).rstrip() == 'foo(1, 2, 3);'


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
    assert out.rstrip() == dedent(
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
        ''').strip()


def test_nested(capsys):
    pretty_print(FunctionDefinition('main', Function([], [
        FunctionDefinition('plus', Function(['a', 'b'], [
            BinaryOperation(Reference('a'), '+', Reference('b'))
        ])),
        FunctionCall(Reference('plus'), [Number(200), Number(39)])
    ])))
    out = capsys.readouterr().out
    assert out.rstrip() == dedent(
        '''
        def main() {
            def plus(a, b) {
                (a) + (b);
            }
            plus(200, 39);
        }
        ''').strip()


if __name__ == '__main__':
    pytest.main()
