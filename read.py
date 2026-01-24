import read_s_expression
import read_token
import Stream
from character import is_whitespace, is_macro, is_constituent

def get_reader_macro_function(x):
    def _read_s_expression(stream, x):
        return read_s_expression.read_s_expression(read, stream, x)
    if x == "(":
        return _read_s_expression

def read(environment, stream):
    x = Stream.get_next_character(stream)
    while x:
        if is_whitespace(x):
            continue
        elif is_macro(x):
            reader_macro_function = get_reader_macro_function(x)
            reader_macro_result = reader_macro_function(stream, x)
            if not reader_macro_result is None:
                return reader_macro_result
            else:
                continue
        elif is_constituent(x):
            token = x + read_token.read_token(stream)
            return token
        x = Stream.get_next_character(stream)
