from character import is_whitespace, is_macro
import streams

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

