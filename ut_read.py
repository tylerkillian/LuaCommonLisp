from input_stream import InputStream
from standard_read_definition import read

def test_read_symbol():
    input_stream = InputStream(' hello ')
    result = read(input_stream)
    assert result == 'hello'

def test_read_s_expression():
    input_stream = InputStream(' (+ abc def) ')
    result = read(input_stream)
    assert result == {'form': 'list', 'elements': ['+', 'abc', 'def']}

def test_read_quote():
    input_stream = InputStream("'a")
    result = read(input_stream)
    assert result == {
        'form': 'quote',
        'arg': 'a'
    }

def test_read_backquote():
    input_stream = InputStream(' ` (+ abc def) ')
    result = read(input_stream)
    assert result == {
        'form': 'backquote',
        'arg': {'form': 'list', 'elements': ['+', 'abc', 'def']}
    }

def test_read_comma():
    input_stream = InputStream('`(+ a ,b))')
    result = read(input_stream)
    assert result == {
        'form': 'backquote',
        'arg': {
            'form': 'list', 
            'elements': [
                '+',
                'a',
                {
                    'form': 'comma',
                'arg': 'b'
            }
        ]}
    }

def test_read_splice():
    input_stream = InputStream('`(+ a ,@(b c))')
    result = read(input_stream)
    assert result == {
        'form': 'backquote',
        'arg': [
            '+',
            'a',
            {
                'form': 'splice',
                'arg': ['b', 'c']
            }
        ]
    }

def run_tests():
    test_read_symbol()
    test_read_s_expression()
    test_read_quote()
    test_read_backquote()
    test_read_comma()
    test_read_splice()
