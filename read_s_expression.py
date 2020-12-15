from read_common import _is_whitespace
from read import read

def read_s_expression(x, input_stream):
    result = []
    next_token = None
    while not input_stream.at_eof():
        x = input_stream.get_next_character()
        if x == ')':
            return result
        elif _is_whitespace(x):
            continue
        else:
            read(input_stream)
