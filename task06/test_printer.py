#!/usr/bin/env python3
import pytest

from printer import *


def test_pretty_conditional(capsys):
    pretty_print(Conditional(Number(42), [], []))
    out = capsys.readouterr().out
    assert out.rstrip() == 'if (42) {\n};'


def test_pretty_function_definition(capsys):
    pretty_print(FunctionDefinition("foo", Function([], [])))
    out = capsys.readouterr().out
    assert out.rstrip() == 'def foo() {\n};'


def test_pretty_print(capsys):
    pretty_print(Print(Number(42)))
    out = capsys.readouterr().out
    assert out.rstrip() == 'print 42;'


def test_pretty_read(capsys):
    pretty_print(Read('x'))
    out = capsys.readouterr().out
    assert out.rstrip() == 'read x;'


def test_pretty_number(capsys):
    pretty_print(Number(10))
    out = capsys.readouterr().out
    assert out.rstrip() == '10;'


def test_pretty_reference(capsys):
    pretty_print(Reference('x'))
    out = capsys.readouterr().out
    assert out.rstrip() == 'x;'


def test_pretty_binary_operation(capsys):
    add = BinaryOperation(Number(2), '+', Number(3))
    mul = BinaryOperation(Number(0), '*', add)
    pretty_print(mul)
    out = capsys.readouterr().out
    assert out.rstrip() == '(0) * ((2) + (3));'


def test_pretty_unary_operation(capsys):
    pretty_print(UnaryOperation('-', Number(42)))
    out = capsys.readouterr().out
    assert out.rstrip() == '-(42);'


def test_pretty_function_call(capsys):
    pretty_print(FunctionCall(Reference('foo'), [
                 Number(1), Number(2), Number(3)]))
    out = capsys.readouterr().out
    assert out.rstrip() == 'foo(1, 2, 3);'


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
    assert out.rstrip().split('\n') == [
        'def main(arg1) {',
        '    read x;',
        '    print x;',
        '    if ((2) == (3)) {',
        '        if (1) {',
        '        };',
        '    } else {',
        '        exit(-(arg1));',
        '    };',
        '};']


if __name__ == "__main__":
    pytest.main()
