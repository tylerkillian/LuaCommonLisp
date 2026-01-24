from character import is_whitespace, is_macro
import Stream
import sys

def skip_whitespace(stream):
    x = Stream.see_next_character(stream)
    while is_whitespace(x):
        Stream.get_next_character(stream)
        x = Stream.see_next_character(stream)

def read_s_expression(read, stream, x):
    result = []
    skip_whitespace(stream)
    while Stream.see_next_character(stream) != ")":
        result.append(read(None, stream))
        skip_whitespace(stream)
    return result

