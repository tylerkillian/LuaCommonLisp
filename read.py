import streams
from character import \
    is_constituent, \
    is_macro, \
    is_terminating_macro, \
    is_whitespace

def read_token(stream):
    result = ""
    while True:
        if streams.at_end_of_file(stream):
            return result

        y = streams.get_next_character(stream)
        if is_constituent(y):
            result += y
        elif is_whitespace(y):
            return result
        elif is_terminating_macro(y):
            streams.prepend(stream, y)
            return result
        else:
            assert False

def read_string(stream, x):
    assert x == '"'
    result = ""
    while True:
        next_character = streams.get_next_character(stream)
        if next_character == '"':
            break
        result += next_character
    return result


def skip_whitespace(stream):
    x = streams.see_next_character(stream)
    while is_whitespace(x):
        streams.get_next_character(stream)
        x = streams.see_next_character(stream)

def read_s_expression(read, stream, x):
    assert x == "("
    result = []
    skip_whitespace(stream)
    while streams.see_next_character(stream) != ")":
        result.append(read(None, stream))
        skip_whitespace(stream)
    return result

def get_reader_macro_function(x):
    def _read_s_expression(stream, x):
        return read_s_expression(read, stream, x)
    if x == "(":
        return _read_s_expression
    elif x == "(":
        return read_string

def read(environment, stream):
    x = streams.get_next_character(stream)
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
            token = x + read_token(stream)
            return token
        x = streams.get_next_character(stream)
