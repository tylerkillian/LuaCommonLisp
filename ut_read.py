from input_stream import InputStream
from read import read

def test_read_symbol():
    input_stream = InputStream(' hello ')
    result = read(input_stream)
    assert result == 'hello'

def test_read_s_expression():
    input_stream = InputStream(' (+ abc def) ')
    result = read(input_stream)
    assert result == ['+', 'abc', 'def']

def test_read_backquote():
    input_stream = InputStream(' ` (+ abc def) ')
    result = read(input_stream)
    assert result == {
        'form': 'backquote',
        'arg': ['+', 'abc', 'def']
    }

def run_tests():
    test_read_symbol()
    test_read_s_expression()
    test_read_backquote()
