from input_stream import InputStream
from read import read

def test_read_symbol():
    input_stream = InputStream(' hello ')
    symbol = read(input_stream)
    assert symbol == 'hello'

def run_tests():
    test_read_symbol()
