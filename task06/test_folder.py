#!/usr/bin/env python3
import pytest

from folder import *


def test_fold_binary_number_number():
    assert fold_constants(BinaryOperation(
        Number(1), '+', Number(2))) == Number(3)
    assert fold_constants(BinaryOperation(
        Number(1), '*', Number(2))) == Number(2)
    assert fold_constants(BinaryOperation(
        Number(1), '==', Number(2))) == Number(0)
    assert fold_constants(BinaryOperation(
        Number(1), '/', Number(2))) == Number(0)


def test_fold_zero_reference():
    assert fold_constants(BinaryOperation(
        Number(0), '*', Reference('x'))) == Number(0)


def test_fold_reference_zero():
    assert fold_constants(BinaryOperation(
        Reference('x'), '*', Number(0))) == Number(0)


def test_fold_reference_subtraction():
    assert fold_constants(BinaryOperation(
        Reference('x'), '-', Reference('x'))) == Number(0)


def test_fold_unary():
    assert fold_constants(UnaryOperation('!', Number(1))) == Number(0)
    assert fold_constants(UnaryOperation('!', Number(0))) == Number(1)
    assert fold_constants(UnaryOperation('-', Number(1))) == Number(-1)


def test_fold_complex():
    assert fold_constants(
        BinaryOperation(
            Number(10),
            '-',
            UnaryOperation(
                '-',
                BinaryOperation(
                    Number(3),
                    '+',
                    BinaryOperation(
                        Reference('x'),
                        '-',
                        Reference('x')
                    )
                )
            )
        )
    ) == Number(13)
