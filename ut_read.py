from input_stream import InputStream
from standard_read_definition import read
from symbol import Symbol
from read_macro import Llist, Quote, Backquote, Comma

def test_read_symbol():
    input_stream = InputStream(' hello ')
    result = read(input_stream)
    assert result == Symbol('hello')

def test_read_s_expression():
    input_stream = InputStream(' (+ abc def) ')
    result = read(input_stream)
    assert result == Llist([Symbol('+'), Symbol('abc'), Symbol('def')])

def test_read_quote():
    input_stream = InputStream("'a")
    result = read(input_stream)
    assert result == Quote(Symbol('a'))

def test_read_backquote():
    input_stream = InputStream(' ` (+ abc def) ')
    result = read(input_stream)
    assert result == Backquote(Llist([Symbol('+'), Symbol('abc'), Symbol('def')]))

def test_read_comma():
    input_stream = InputStream('`(+ a ,b))')
    result = read(input_stream)
    assert result == Backquote(Llist([Symbol('+'), Symbol('a'), Comma('comma', Symbol('b'))]))

def test_read_splice():
    input_stream = InputStream('`(+ a ,@(b c))')
    result = read(input_stream)
    assert result == Backquote(Llist([Symbol('+'), Symbol('a'), Comma('splice', Llist([Symbol('b'), Symbol('c')]))]))

def run_tests():
    test_read_symbol()
    test_read_s_expression()
    test_read_quote()
    test_read_backquote()
    test_read_comma()
    test_read_splice()
